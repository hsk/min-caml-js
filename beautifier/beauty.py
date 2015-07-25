import re
import sys

sys.setrecursionlimit(1000*1000)

def startsWith(i, param):
    ilen = len(i)
    plen = len(param)
    if plen == 0 and ilen == 0:
        return True
    if plen > ilen:
        return False
    else:
        return i[0: plen] == param

class Parser:
    def __or__(self, that):
        return Or(self, that)

    def __add__(self, that):
        return Seq(self, that)

    def __rshift__(self, that):
        return Next(self, that)

    def __lshift__(self, that):
        return Prev(self, that)

    def __xor__(self, that):
        return Action(self, that)

class Or(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        e = self.thiz(i)
        if e is None:
            return self.that(i)
        return e

class any_char_obj(Parser):
    def __call__(self, i):
        if len(i) > 0:
            return [i[0], i[1:]]
        else:
            return None

any_char = any_char_obj()

class nstr(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        if not startsWith(i, self.param):
            return None
        return [self.param, i[len(self.param):]]

class nreg(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        #print(self.param)
        m = re.search(self.param, i)
        if m is None:
            #print("None")
            return None
        #print(m.group(0))
        return [m.group(0), i[len(m.group(0)):]]

class Seq(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        r1 = self.thiz(i)
        if r1 is None:
            return None
        r2 = self.that(r1[1])
        if r2 is None:
            return None
        return [[r1[0], r2[0]], r2[1]]

skip = nreg('''^(\s|\(\*.*\*\))*''')

def st(s):
    return skip + nstr(s)

#print (any_char("abc"))

#print (st("ab") | any_char)("  abc")

class seq(Parser):
    def __init__(self, *params):
        args = []
        for v in params:
            if isinstance(v, basestring):
                args.append(st(v))
            elif v is None:
                args.append(st(""))
            else:
                args.append(v)

        self.params = args

    def __call__(self, i):
        rs = []
        for v in self.params:
            r = v(i)
            if r is None:
                return None
            rs.append(r[0])
            i = r[1]
        #print([rs, i])
        return [rs, i]

class Next(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        r = (self.thiz + self.that)(i)
        if r is None:
            return r
        return [r[0][1], r[1]]

class Prev(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        r = (self.thiz + self.that)(i)
        if r is None:
            return r
        return [r[0][0], r[1]]

class Action(Parser):
    def __init__(self, thiz, that):
        self.thiz = thiz
        self.that = that

    def __call__(self, i):
        r = self.thiz(i)
        #print("action "),
        #print(r)
        if r is None:
            return r
        return [self.that(r[0]), r[1]]

#print (st("a") + st("b") + st("c") ^ (lambda x: ["<"]+x+[">"])) ("a b c")

class opt(Parser):
    def __init__(self, thiz):
        if isinstance(thiz, basestring):
            thiz = st(thiz)
        self.thiz = thiz

    def __call__(self, i):
        r = self.thiz(i)
        if r is None:
            return [None, i]
        return r

class rep(Parser):
    def __init__(self, thiz):
        if isinstance(thiz, basestring):
            thiz = st(thiz)
        self.thiz = thiz

    def __call__(self, i):
        rs = []
        while(True):
            r = self.thiz(i)
            if r is None:
                return [rs, i]
            rs.append(r[0])
            i = r[1]

def rep1(thiz):
    if isinstance(thiz, basestring):
        thiz = st(thiz)
    return rep(thiz) ^ (lambda p: None if len(p) < 1 else p)

class notp(Parser):
    def __init__(self, thiz):
        if isinstance(thiz, basestring):
            thiz = st(thiz)
        self.thiz = thiz

    def __call__(self, i):
        r = self.thiz(i)
        if r is None:
            return ["", i]
        return None

class Range(Parser):
    def __init__(self, thiz):
        if isinstance(thiz, basestring):
            thiz = st(thiz)
        self.thiz = thiz

    def __call__(self, i):
        if len(i) <= 0:
            return None
        c = ord(i[0])
        if ord(self.thiz[0]) <= c and c <= ord(self.that[0]):
            return [i[0], i[1:]]
        return None

def reg(s):
    return skip + nreg(s)

class Nest(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

NestM = Nest("NestM")
NestP = Nest("NestP")

def printf(text):
    print text

#print([NestP, "a", NestM])

def n(a, b, c):

    def nf(a):
        return [a[0], NestP, a[1], NestM, a[2]]

    return seq(a, b, c) ^ nf

def flat(a):
#    print("flat")
#    print(a)
    if a is None:
        return []
    if isinstance(a, list):
        rc = []
        for i in a:
            rc.extend(flat(i))
        return rc
    return [a]

def cnv(e):
    #print(e)
    reg2 = '''(^.*\\n.*$)'''
    whiteSpace = '''(^\\s*$)'''
    nest = 0
    e2 = []
    for s in e:
        if isinstance(s, basestring):
            m = re.search('''(^.*\\n)(\\s+$)''', s)
            if not(m is None):
                s = m.group(1)
        e2.append(s)
    #print(e2)

    e3 = []
    i = 0
    while(i < len(e2)):
        s = e2[i]
        if isinstance(s, basestring):
            m = re.search(reg2, s)
            if not(m is None):
                e = []
                while(i < len(e2)):
                    s2 = e2[i]
                    if s2 is NestM:
                        e3.append(s2)
                        i += 1
                        continue
                    if s2 is NestP:
                        e.append(s2)
                        break

                    if not(re.search(whiteSpace, s2) is None):
                        e.append(s2)
                        i += 1
                        continue

                    e.append(s2)
                    break

                e3.extend(e)
                i += 1
                continue
        i += 1
        e3.append(s)
    #print(e3)

    def makeString(s, n):
        rs = ""
        for i in range(0, n):
            rs = rs + s
        return rs

    e2 = []
    for s in e3:
        if s is NestP:
            nest += 1
            continue
        if s is NestM:
            nest -= 1
            continue
        m = re.search(reg2, s)
        if m is not None:
            #print("add nest "+nest)
            e2.append(re.sub('\n', "\n"+makeString("  ", nest), m.group(0)))
            continue
        e2.append(s)
    #print(e2)
    return "".join(e2)

def parse(s):
    s = re.compile("^[\\s]+", re.M).sub("", s)
    #s = re.sub("^\s+", "", s)

    keywords = reg('''^(let|in|if|else|then|rec|begin|end|match|with|type)\\b''')

    id = notp(keywords) >> reg('''^[_a-zA-Z0-9]+''') \
        | reg('''^[+\\-*\\/.<>:@=][+\\-*\\/.<>:@=]*''') \
        | reg('''^[,!]''') \
        | reg('''^("(\\.|[^"])*")''')

    def exp(i):
        return (exps + rep(seq(notp(st(";;")) >> st(";"), exps)))(i)

    exp1 = n("begin", exp, "end") \
        | n("(", opt(exp), ")") \
        | n("{", opt(exp), "}") \
        | n("[", opt(exp), "]") \
        | seq(n("if", exp, ""), n("then", exp, "else"), exp) \
        | seq(n("let", seq(opt("rec"), exp), "in"), exp) \
        | seq(n((st("match") | st("try")), exp, "with"), opt("|"), n("", exp, ""), rep(seq("|", n("", exp, "")))) \
        | seq("function", opt("|"), n("", exp, ""), rep(seq("|", n("", exp, "")))) \
        | n("type", seq(id, "=", n(opt("|"), exp, ""), rep(n("|", exp, ""))), opt(";;")) \
        | n("type", seq(id, "=", exp), opt(";;")) \
        | n("open", id + rep(seq(".", id)), opt(";;")) \
        | id

    exps = rep1(exp1)

    e = flat(exp(s))
    #print("parse e="),
    #print(e)

    return cnv(e)
