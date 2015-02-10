RESULT = mincaml
TESTS = string as list1 match begin print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat variant when list

default: mincaml do_test
mincaml:
	cp ../src/syntax.ml .
	cp ../src/to_if.ml .
	cp ../src/emit.ml .
	ocamlc syntax.ml peg.ml parser.ml to_if.ml emit.ml main.ml -o $(RESULT)
	rm -rf *.cm*

.PRECIOUS: ../test/%.res ../test/%.ans ../test/%.cmp ../test/%.js
do_test: $(TESTS:%=../test/%.cmp)

TRASH = $(TESTS:%=../test/%) $(TESTS:%=../test/%.res) $(TESTS:%=../test/%.ans) $(TESTS:%=../test/%.cmp) $(TESTS:%=../test/%.js)

../test/%.js: $(RESULT) ../test/%.ml
	./$(RESULT) ../test/$*
../test/%.res: ../test/%.js
	node $< > $@
../test/%.ans: ../test/%.ml
	ocaml $< > $@
../test/%.cmp: ../test/%.res ../test/%.ans
	diff $^ > $@

clean:
	rm -rf *.cm* mincaml $(TRASH) to_if.ml emit.ml syntax.ml 
