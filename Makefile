build: 
	ocamlbuild -use-ocamlfind Assignment2.d.byte
clean: 
	ocamlbuild -clean
.PHONY: 
	build clean
