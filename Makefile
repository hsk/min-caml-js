RESULT = min-caml
NCSUFFIX = .opt
CC = gcc
CFLAGS = -g -O2 -Wall

default: debug-code top $(RESULT) do_test

$(RESULT): debug-code top

clean:: nobackup

SOURCES = type.ml id.ml \
syntax.ml parser.mly lexer.mll emit.ml \
main.ml

TESTS = match begin print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat variant

do_test: $(TESTS:%=test/%.cmp)

.PRECIOUS: test/%.s test/% test/%.res test/%.ans test/%.cmp test/%.js
TRASH = $(TESTS:%=test/%.s) $(TESTS:%=test/%) $(TESTS:%=test/%.res) $(TESTS:%=test/%.ans) $(TESTS:%=test/%.cmp) $(TESTS:%=test/%.js)

test/%.js: $(RESULT) test/%.ml
	./$(RESULT) test/$*
test/%: test/%.js
	echo "node test/$*.js" > test/$*
	chmod 777 test/$*
test/%.res: test/%
	$< > $@
test/%.ans: test/%.ml
	ocaml $< > $@
test/%.cmp: test/%.res test/%.ans
	diff $^ > $@

include OCamlMakefile
