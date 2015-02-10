.PHONY: clean all default
RESULT = mincaml
TESTS = ref record string as list1 match begin print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat variant when list
SOURCES = syntax.ml peg.ml parser.ml to_if.ml alpha.ml javascript.ml emit.ml main.ml
TRASH1 = $(TESTS:%=../test/%.res) $(TESTS:%=../test/%.ans) $(TESTS:%=../test/%.cmp) $(TESTS:%=../test/%.js)
.PRECIOUS: ../test/%.res ../test/%.ans ../test/%.cmp ../test/%.js
default: mincaml test
mincaml: $(SOURCES)
	ocamlc $(SOURCES) -o $(RESULT)
syntax.ml: ../src/syntax.ml
	cp ../src2/peg.ml .
	cp ../src2/parser.ml .
	cp ../src2/main.ml .
	cp ../src/syntax.ml .
to_if.ml: ../src/to_if.ml
	cp ../src/to_if.ml .
test: $(TESTS:%=../test/%.cmp)
../test/%.js: ./$(RESULT) ../test/%.ml
	./$(RESULT) ../test/$*
../test/%.res: ../test/%.js
	node $< > $@
../test/%.ans: ../test/%.ml
	ocaml $< > $@
../test/%.cmp: ../test/%.res ../test/%.ans
	diff $^ > $@
clean:
	rm -rf *.cm* mincaml $(TRASH1) syntax.ml to_if.ml peg.ml parser.ml main.ml

