all: repl.native platon.native platon.byte repl.byte

repl.byte: repl.ml
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit repl.byte

repl.native: repl.ml
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit repl.native

platon.byte: platon.ml
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit platon.byte


platon.native: platon.ml
	ocamlbuild -j 4 -use-ocamlfind -tag thread -package gg,vg,vg.cairo -package core -package tsdl -package core_extended -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit platon.native

clean:
	ocamlbuild -clean
