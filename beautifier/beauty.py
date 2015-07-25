import re
import sys

sys.setrecursionlimit(1000*1000)


class Parser(object):
    def __rshift__(self, that):
        return p(self, that) ^ (lambda a: a[1])

    def __lshift__(self, that):
        return p(self, that) ^ (lambda a: a[0])

    def __xor__(self, that):
        return Action(self, that)

    def __neg__(self):
        return Action(self, lambda a: [NestP, a, NestM])

    class static(type):
        def __getitem__(self, i):
            return self(*i) if isinstance(i, tuple) else self(i)
    __metaclass__ = static


class nreg(Parser):
    def __init__(self, param):
        self.param = re.compile(param)

    def __call__(self, i):
        m = self.param.search(i)
        return None if m is None else [m.group(0), i[len(m.group(0)):]]


skip = nreg(r'^(\s|\(\*.*\*\))*')


class orp(Parser):
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
    def __init__(self, *thiz):
        self.thiz = p(*thiz)

    def __call__(self, i):
        r = self.thiz(i)
        return [[], i] if r is None else r


class rep(Parser):
    def __init__(self, *thiz):
        self.thiz = p(*thiz)

    def __call__(self, i):
        rs = []
        while(True):
            r = self.thiz(i)
            if r is None:
                return [rs, i]
            rs.append(r[0])
            i = r[1]


def rep1(*thiz):
    return rep(*thiz) ^ (lambda p: None if len(p) < 1 else p)


class notp(Parser):
    def __init__(self, *thiz):
        self.thiz = orp(*thiz)

    def __call__(self, i):
        return [[], i] if self.thiz(i) is None else None


def reg(s):
    return p(skip, nreg(s))


class orhash(Parser):
    def __init__(self, reg, dict):
        self.reg = reg
        self.dict = dict

    def __call__(self, i):

        m = self.reg(i)
        if m is None:
            return None
        if not (m[0][1] in self.dict):
            return None
        p = self.dict[m[0][1]]
        p2 = p(m[1])
        if p2 is None:
            return None
        return [[m[0], p2[0]], p2[1]]


class Nest:
    def __init__(self, name, n):
        self.name = name
        self.n = n

    def __repr__(self):
        return self.name
NestP = Nest("NestP", 1)
NestM = Nest("NestM", -1)


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
                else:
                    s.append(s2)
                    if whiteSpace.search(s2) is None:
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

id = orp(
    notp(keywords) >> reg(r"^[_a-zA-Z0-9]+"),
    reg(r'^[+\-*\/.<>:@=][+\-*\/.<>:@=]*'),
    reg(r'^[,!]'),
    reg(r'^("(\\.|[^"])*")')
)

exp = p(lambda i: p(exps, rep[notp(";;") >> p(";"), exps])(i))

exp1 = orp(
    orhash(
        reg(r"^[_a-zA-Z0-9]+|^[(\[{]"),
        {
            "begin": p(-exp, "end"),
            "(": p(-opt(exp), ")"),
            "{": p(-opt(exp), "}"),
            "[": p(-opt(exp), "]"),
            "if": p(-exp, "then", -exp, "else", exp),
            "let": p(-p[opt("rec"), exp], "in", exp),
            "match": p(-exp, "with", opt("|"), -exp, rep["|", -exp]),
            "try": p(-exp, "with", opt("|"), -exp, rep["|", -exp]),
            "function": p(opt("|"), -exp, rep("|", -exp)),
            "type": p(-p[id, "=", opt("|"), exp, rep("|", exp)], opt(";;")),
            "open": p(-p[id, rep(".", id)], opt(";;"))
        }
    ),
    id
)

exps = rep1(exp1)

regparse = re.compile(r"^[\s]+", re.M)


def parse(s):
    return cnv(exp(regparse.sub("", s)))
