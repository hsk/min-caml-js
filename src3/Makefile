.PHONY: clean all default
RESULT = mincaml
TESTS = as list1 match begin print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat variant when list
SOURCES =  syntax.ml peg.ml parser.ml emit.ml main.ml
TRASH1 = $(TESTS:%=../test/%.res) $(TESTS:%=../test/%.ans) $(TESTS:%=../test/%.cmp) $(TESTS:%=../test/%.js)
.PRECIOUS: ../test/%.res ../test/%.ans ../test/%.cmp ../test/%.js
default: mincaml test
mincaml: $(SOURCES)
	cp ../src2/*.ml .
	ocamlc $(SOURCES) -o $(RESULT)
syntax.ml: ../src/syntax.ml
	cp ../src/syntax.ml .
	cp ../src2/*.ml .
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
	rm -rf *.cm* mincaml $(TRASH1) syntax.ml peg.ml parser.ml main.ml
