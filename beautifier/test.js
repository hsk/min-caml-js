
var beauty = require('./beauty.js');
var parse=beauty.parse;


console.log(parse("begin\n1 + 2\nend"));


console.log(parse(
  "let a = 1 in\n"+
  "if a then b else c\n"+
  ""));

console.log(parse(
  "let a = 1 in\n"+
  "if a\n"+
  "then b\n"+
  "else c\n"+
  ""));

console.log(parse(
  "let a = 1 in\n"+
  "if a\n"+
  "then\n"+
  "b\n"+
  "else\n"+
  "c\n"+
  ""));

console.log(parse(
  "let a =\n"+
    "let a = 1 in\n"+
    "a\n"+
  "in\n"+
  "a\n"+
  ""));

console.log(parse(
  "if a > 10 then\n"+
  "b\n"+
  "else if a > 20 then\n"+
  "c\n"+
  "else\n"+
  "d\n"+
  ""));

console.log(parse(
  "if a > 10 then b else\n"+
  "if a > 20 then c else\n"+
  "d\n"+
  ""));

var fs = require('fs');

var tests = [
    "print", "sum-tail", "gcd", "sum", "fib", "ack", "even-odd",
    "adder", "funcomp", "cls-rec", "cls-bug", "cls-bug2",
    "shuffle", "spill", "spill2", "spill3", "join-stack", "join-stack2", "join-stack3",
    "join-reg", "join-reg2", "non-tail-if", "non-tail-if2",
    "inprod", "inprod-rec", "inprod-loop", "matmul", "matmul-flat",
    "ref", "record", "string", "as", "list1", "match", "begin",
    "variant", "when", "list"
];

for(var i in tests) {
  var f = tests[i];
  var path = "../test/" + f + ".ml";
  console.log(parse(fs.readFileSync(path,"utf-8")));
}

