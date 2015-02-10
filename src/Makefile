RESULT = min-caml
NCSUFFIX = .opt
CC = gcc
CFLAGS = -g -O2 -Wall

default: debug-code top $(RESULT) do_test

$(RESULT): debug-code top

clean:: nobackup

SOURCES = syntax.ml parser.mly lexer.mll to_if.ml emit.ml main.ml

TESTS = string as list1 match begin print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat variant when list

do_test: $(TESTS:%=../test/%.cmp)

.PRECIOUS: ../test/%.res ../test/%.ans ../test/%.cmp ../test/%.js
TRASH = $(TESTS:%=../test/%.s) $(TESTS:%=../test/%) $(TESTS:%=../test/%.res) $(TESTS:%=../test/%.ans) $(TESTS:%=../test/%.cmp) $(TESTS:%=../test/%.js)

../test/%.js: $(RESULT) ../test/%.ml
	./$(RESULT) ../test/$*
../test/%.res: ../test/%.js
	node $< > $@
../test/%.ans: ../test/%.ml
	ocaml $< > $@
../test/%.cmp: ../test/%.res ../test/%.ans
	diff $^ > $@

include OCamlMakefile
