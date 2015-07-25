import re
import sys

sys.setrecursionlimit(1000*1000)


class Parser:
    def __rshift__(self, that):
        return p(self, that) ^ (lambda a: a[1])

    def __lshift__(self, that):
        return p(self, that) ^ (lambda a: a[0])

    def __xor__(self, that):
        return Action(self, that)


class nreg(Parser):
    def __init__(self, param):
        self.param = re.compile(param)

    def __call__(self, i):
        m = self.param.search(i)
        return None if m is None else [m.group(0), i[len(m.group(0)):]]


skip = nreg(r'^(\s|\(\*.*\*\))*')


class Or(Parser):
    def __init__(self, *params):
        self.params = map(lambda v: st(v) if isinstance(v, basestring) else v, params)

    def __call__(self, i):
        for v in self.params:
            r = v(i)
            if r is not None:
                return r
        return None


class nstr(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        return None if not i.startswith(self.param) else [self.param, i[len(self.param):]]


def st(s):
    return p(skip, nstr(s))


class p(Parser):
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


class Action(Parser):
    def __init__(self, thiz, action):
        self.thiz = thiz
        self.action = action

    def __call__(self, i):
        r = self.thiz(i)
        if r is None:
            return None
        else:
            r2 = self.action(r[0])
            return None if r2 is None else [r2, r[1]]


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
    return p(skip, nreg(s))


class Nest:
    def __init__(self, name, n):
        self.name = name
        self.n = n

    def __repr__(self):
        return self.name
NestP = Nest("NestP", 1)
NestM = Nest("NestM", -1)


def n(*a):
    return p(*a) ^ (lambda a: [NestP, a, NestM])


reg2 = re.compile(r'(^.*\n.*$)')
whiteSpace = re.compile(r'(^\s*$)')
reg1 = re.compile(r'\n')


def flat(a, rc):
    if isinstance(a, list):
        for i in a:
            rc = flat(i, rc)
    else:
        rc.append(a)
    return rc


def cnv(e):
    e = flat(e, [])
    e3 = []
    i = 0
    while(i < len(e)):
        s = [e[i]]
        if isinstance(s[0], basestring) and reg2.search(s[0]) is not None:
            s = []
            while(i < len(e)):
                s2 = e[i]
                if s2 is NestM:
                    e3.append(s2)
                elif whiteSpace.search(s2) is not None:
                    s.append(s2)
                else:
                    s.append(s2)
                    break
                i += 1
        i += 1
        e3.extend(s)

    nest = 0
    e4 = []
    for s in e3:
        if isinstance(s, Nest):
            nest += s.n
        else:
            m = reg2.search(s)
            if m is not None:
                s = reg1.sub("\n"+("  " * nest), m.group(0))
            e4.append(s)
    return "".join(e4)

keywords = reg(r"^(let|in|if|else|then|rec|begin|end|match|with|type)\b")

id = Or(
    notp(keywords) >> reg(r"^[_a-zA-Z0-9]+"),
    reg(r'^[+\-*\/.<>:@=][+\-*\/.<>:@=]*'),
    reg(r'^[,!]'),
    reg(r'^("(\\.|[^"])*")')
)

exp = p(lambda i: p(exps, rep(p(notp(";;") >> p(";"), exps)))(i))

exp1 = Or(
    p("begin", n(exp), "end"),
    p("(", n(opt(exp)), ")"),
    p("{", n(opt(exp)), "}"),
    p("[", n(opt(exp)), "]"),
    p("if", n(exp), "then", n(exp), "else", exp),
    p("let", n(opt("rec"), exp), "in", exp),
    p(Or("match", "try"), n(exp), "with", opt("|"), n(exp), rep(p("|", n(exp)))),
    p("function", opt("|"), n(exp), rep(p("|", n(exp)))),
    p("type", n(id, "=", opt("|"), n(exp), rep(p("|", n(exp)))), opt(";;")),
    p("type", n(id, "=", exp), opt(";;")),
    p("open", n(id, rep(p(".", id))), opt(";;")),
    id
)

exps = rep1(exp1)

regparse = re.compile(r"^[\s]+", re.M)


def parse(s):
    s = regparse.sub("", s)
    return cnv(exp(s))
