all:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc syntax.ml parser.ml lexer.ml main.ml

clean:
	rm -f *.cm* parser.ml lexer.ml a.out
