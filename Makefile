all: repl.native platon.native test.native

SOURCES:=pparser.ml plexer.ml platon.ml codegen.ml position.ml		\
parse_error.ml codegen_error.ml ast.ml circuit.ml repl.ml token.ml	\
unionfind.ml typeInference.ml
TESTS:=test/*.ml

test.native: $(TESTS)
	ocamlbuild -I test/ -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit test.native

repl.native: $(SOURCES)
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit repl.native

platon.native: $(SOURCES)
	ocamlbuild -I test/ -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit platon.native

clean:
	ocamlbuild -clean
