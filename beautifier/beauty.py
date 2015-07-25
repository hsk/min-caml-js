import re
import sys

sys.setrecursionlimit(1000*1000)


class Parser:
    def __or__(self, that):
        return Or(self, that)

    def __add__(self, that):
        return seq(self, that)

    def __rshift__(self, that):
        return seq(self, that) ^ (lambda a: a[1])

    def __lshift__(self, that):
        return seq(self, that) ^ (lambda a: a[0])

    def __xor__(self, that):
        return Action(self, that)

    def __neg__(self):
        return rep(self)

    def __pos__(self):
        return rep1(self)

    def __invert__(self):
        return opt(self)


class nreg(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        m = re.search(self.param, i)
        return None if m is None else [m.group(0), i[len(m.group(0)):]]


skip = nreg('''^(\s|\(\*.*\*\))*''')


class Or(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        e = self.thiz(i)
        return self.that(i) if e is None else e


class nstr(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        return None if not i.startswith(self.param) else [self.param, i[len(self.param):]]


def st(s):
    return skip + nstr(s)


class seq(Parser):
    def __init__(self, *params):
        self.params = map(lambda v: st(v) if isinstance(v, basestring) else v, params)

    def __call__(self, i):
        rs = []
        for v in self.params:
            r = v(i)
            if r is None:
                return None
            rs.append(r[0])
            i = r[1]
        return [rs, i]
p = seq


class Action(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        r = self.thiz(i)
        return r if r is None else [self.that(r[0]), r[1]]


class opt(Parser):
    def __init__(self, thiz):
        self.thiz = st(thiz) if isinstance(thiz, basestring) else thiz

    def __call__(self, i):
        r = self.thiz(i)
        return [[], i] if r is None else r


class rep(Parser):
    def __init__(self, thiz):
        self.thiz = st(thiz) if isinstance(thiz, basestring) else thiz

    def __call__(self, i):
        rs = []
        while(True):
            r = self.thiz(i)
            if r is None:
                return [rs, i]
            rs.append(r[0])
            i = r[1]


def rep1(thiz):
    return rep(thiz) ^ (lambda p: None if len(p) < 1 else p)


class notp(Parser):
    def __init__(self, thiz):
        self.thiz = st(thiz) if isinstance(thiz, basestring) else thiz

    def __call__(self, i):
        return [[], i] if self.thiz(i) is None else None

def reg(s):
    return skip + nreg(s)


class Nest:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name
NestM = Nest("NestM")
NestP = Nest("NestP")


def n(*a):
    return seq(*a) ^ (lambda a: [NestP, a, NestM])


def flat(a):
    def f(a, rc):
        if a is None:
            pass
        elif isinstance(a, list):
            for i in a:
                rc = f(i, rc)
        else:
            rc.append(a)
        return rc
    return f(a, [])


def cnv(e):
    reg2 = r'(^.*\n.*$)'
    whiteSpace = r'(^\s*$)'
    nest = 0
    e2 = []
    for s in e:
        if isinstance(s, basestring):
            m = re.search(r'(^.*\n)(\s+$)', s)
            if not(m is None):
                s = m.group(1)
        e2.append(s)

    e3 = []
    i = 0
    while(i < len(e2)):
        s = [e2[i]]
        if isinstance(s[0], basestring):
            m = re.search(reg2, s[0])
            if not(m is None):
                s = []
                while(i < len(e2)):
                    s2 = e2[i]
                    if s2 is NestM:
                        e3.append(s2)
                    elif isinstance(s2, basestring) and not(re.search(whiteSpace, s2) is None):
                        s.append(s2)
                    else:
                        s.append(s2)
                        break
                    i += 1
        i += 1
        e3.extend(s)

    e4 = []
    for s in e3:
        if s is NestP:
            nest += 1
        elif s is NestM:
            nest -= 1
        else:
            m = re.search(reg2, s)
            if m is not None:
                s = re.sub(r'\n', "\n"+("  " * nest), m.group(0))
            e4.append(s)
    return "".join(e4)


def parse(s):
    s = re.compile(r"^[\s]+", re.M).sub("", s)

    keywords = reg(r"^(let|in|if|else|then|rec|begin|end|match|with|type)\b")

    id = notp(keywords) >> reg(r"^[_a-zA-Z0-9]+") \
        | reg(r'^[+\-*\/.<>:@=][+\-*\/.<>:@=]*') \
        | reg(r'^[,!]') \
        | reg(r'^("(\\.|[^"])*")')

    exp = p(lambda i: (exps + -p(notp(";;") >> p(";"), exps))(i))

    exp1 = p("begin", n(exp), "end") \
        | p("(", n(~exp), ")") \
        | p("{", n(~exp), "}") \
        | p("[", n(~exp), "]") \
        | p("if", n(exp), "then", n(exp), "else", exp) \
        | p("let", n(~p("rec"), exp), "in", exp) \
        | p(p("match") | p("try"), n(exp), "with", ~p("|"), n(exp), -p("|", n(exp))) \
        | p("function", ~p("|"), n(exp), -p("|", n(exp))) \
        | p("type", n(id, "=", ~p("|"), n(exp), -p("|", n(exp))), ~p(";;")) \
        | p("type", n(id, "=", exp), ~p(";;")) \
        | p("open", n(id, -p(".", id)), ~p(";;")) \
        | id

    exps = +exp1

    e = flat(exp(s))
    #print("parse e="),
    #print(e)

    return cnv(e)
