
OCAMLBUILD := ocamlbuild -j 4 -use-ocamlfind
OCAMLFIND  := ocamlfind

platon: platon.ml
	ocamlbuild -j 4 -use-ocamlfind -package llvm -package oUnit platon.native

clean:
	ocamlbuild -clean
