
all: repl.native platon.native test.native verilog.native
byte: repl.byte platon.byte test.byte

SOURCES:=platon/*.ml
TESTS:=test/*.ml

OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

llvm_library_path = /usr/local/opt/llvm/lib/

test: test.byte
	./test.byte

test.byte: $(TESTS) $(SOURCES)
	@ocamlbuild -lflags '-cclib -L$(llvm_library_path)' -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core -package core_extended,sexplib -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit test.byte

test.native: $(TESTS) $(SOURCES)
	@ocamlbuild  -lflags '-cclib -L$(llvm_library_path)' -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib -package sedlex -package sedlex.ppx -package llvm.scalar_opts -package llvm -package llvm.executionengine -package oUnit test.native

repl.native: $(SOURCES)
	@ocamlbuild -lflags '-cclib -L$(llvm_library_path)' -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib,sedlex,llvm,linenoise repl.native

platon.native: $(SOURCES)
	@ocamlbuild  -lflags '-cclib -L$(llvm_library_path)' -I test/ -I platon/ -j 4 -use-ocamlfind -tag thread -package core,core_extended,sexplib,sedlex,sedlex.ppx,llvm,llvm.analysis,llvm.executionengine,llvm.scalar_opts,llvm.target platon.native

verilog.native: verilog/*.ml
	@ocamlbuild -I verilog -I ext -j 4 -use-ocamlfind -tag thread -package core,core_extended,sedlex,sedlex.ppx,cmdliner verilog.native

clean:
	@ocamlbuild -clean
