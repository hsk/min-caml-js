from beauty import *

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

print(parse(
    "let a =\n" +
    "let a = 1 in\n" +
    "if a > 10 \n" +
    "then\n" +
    "b\n" +
    "else\n" +
    "c\n" +
    "in\n" +
    "a + b\n" +
    ""))

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
    "   ;;  \n" +
    "   match a with\n" +
    "       | Add(a) -> a\n" +
    "       | Add(a) -> a\n" +
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
        open a;;
          let b =
      try
    a
      with
     e
     in
     a
    '''))


print(seq(opt("|"), "a")("a"))
