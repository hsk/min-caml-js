from beauty import *

#print(p("(", n(rep("a")), ")")("()"))

print(parse("()"))

print(parse("1"))


print(parse("begin\n  1 + 2 \nend"))


print(parse(
    "if a then b else c\n" +
    ""))
print(parse(
    "let a = 1 in\n" +
    "if a then b else c\n" +
    ""))

print(parse(
    "let a = 1 in\n" +
    "if a\n" +
    "then b\n" +
    "else c\n" +
    ""))

print(parse(
    "let a = 1 in\n" +
    "if a\n" +
    "then\n" +
    "b\n" +
    "else\n" +
    "c\n" +
    ""))

print(parse("""
    let a =
        let a = 1 in
        a
    in
    a"""))

print(parse(
    "if a > 10 then\n" +
    "b\n" +
    "else if a > 20 then\n" +
    "c\n" +
    "else\n" +
    "d\n" +
    ""))

print(parse(
    "if a > 10 then b else\n" +
    "if a > 20 then c else\n" +
    "d\n" +
    ""))


tests = [
    "print", "sum-tail", "gcd", "sum", "fib", "ack", "even-odd",
    "adder", "funcomp", "cls-rec", "cls-bug", "cls-bug2",
    "shuffle", "spill", "spill2", "spill3", "join-stack", "join-stack2", "join-stack3",
    "join-reg", "join-reg2", "non-tail-if", "non-tail-if2",
    "inprod", "inprod-rec", "inprod-loop", "matmul", "matmul-flat",
    "ref", "record", "string", "as", "list1", "match", "begin",
    "variant", "when", "list"
]

for f in tests:
    path = "../test/" + f + ".ml"
    print("test " + path)
    print(parse(open(path).read()))

print(parse(
    "type e=\n" +
    "   | Add of int\n" +
    "   | Mul of int\n" +
    "   ;;  \n" +
    "   match a with\n" +
    "       | Add(a) -> a\n" +
    "       | Mul(a) -> a\n" +
    ""))
print(parse(
    "type e=\n" +
    "   Add of int\n" +
    "   ;;  \n" +
    "   1\n" +
    ""))


print(parse(
    "  open a;;\n" +
    "  let a = 1\n" +
    ""))


print(parse(
    '''
    try
    a;
    with
    e
    '''))

print(parse(
    r'''
    open Printf
    let a = 1
    let b = 2
    let _ =
        let c = a + b in
        printf("%d\n", c)
    '''))

print(parse(
    r'''
    module a = struct
        let a = 1
        let b = 2
    end
    module a =
        Set.Make(String)
    module a =
        Set.Make(struct type t = s end)
    module F (X : X_type) = struct

    end
    '''))

print(parse(
    r'''
        let b a = match a; with
            | a when a = 1 ->
                let a = a in
                a
            | _ ->
                let a = a in a
    '''))
