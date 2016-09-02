all: repl.native platon.native test.native verilog.native
byte: repl.byte platon.byte test.byte

SOURCES:=platon/*.ml
TESTS:=test/*.ml


test: test.byte
	./test.byte
test.byte: $(TESTS) $(SOURCES)
	@ocamlbuild -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core -package core_extended,sexplib -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit test.byte
test.native: $(TESTS) $(SOURCES)
	@ocamlbuild -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit test.native
repl.native: $(SOURCES)
	@ocamlbuild -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib,sedlex,llvm,llvm.analysis,linenoise repl.native
platon.native: $(SOURCES)
	@ocamlbuild -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib,sedlex,sedlex.ppx,llvm,llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target platon.native

verilog.native: verilog/verilog.ml
	@ocamlbuild -I verilog -j 4 -use-ocamlfind -tag thread -package core,core_extended,sedlex,sedlex.ppx verilog.native

clean:
	@ocamlbuild -clean
