OCAMLBUILD := ocamlbuild -j 4 -use-ocamlfind
OCAMLFIND  := ocamlfind

platon: platon.ml
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package core -package tsdl -package core_extended -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit platon.native

clean:
	ocamlbuild -clean
