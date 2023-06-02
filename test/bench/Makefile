OCAMLC=ocamlopt
OCAMLFLAGS=-O3
MENHIRFLAGS=-O 0

.PHONY: bench
bench: main.yacc.exe main.menhir.exe main.cpspg.exe
	@echo -n 'ocamlyacc: '; ./main.yacc.exe
	@echo -n 'menhir:    '; ./main.menhir.exe
	@echo -n 'cpspg:     '; ./main.cpspg.exe

.PHONY: clean
clean:
	rm -f *.cmi *.cmx *.o
	rm -f Lexer.ml Parser.*.ml main.*.exe

Lexer.ml: Lexer.mll
	ocamllex $^

Parser.yacc.ml: Parser.mly
	ocamlyacc -b $(basename $@) $^
	rm $(basename $@).mli

Parser.menhir.ml: Parser.mly
	menhir $(MENHIRFLAGS) -b $(basename $@) $^
	rm $(basename $@).mli

Parser.cpspg.ml: Parser.mly
	dune exec -- cpspg --readable --no-locations $^ -o $@

main.%.exe: Parser.%.ml Lexer.ml main.ml
	cp Parser.$(*F).ml Parser.ml
	$(OCAMLC) $(OCAMLFLAGS) -I +unix unix.cmxa Parser.ml Lexer.ml main.ml -o $@

