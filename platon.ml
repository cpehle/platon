let () =
  let module Ast = Ast.L1 in
  let f = Ast.Function (Ast.Prototype ("square", [| "x" |]),
		   Ast.Term.Binary ("+", Ast.Type.Base Ast.Type.Double, Ast.Term.Variable "x", Ast.Term.Variable "x")) in
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "my cool jit" in
  let builder = Llvm.builder context in
  let l = Codegen.codegen_function {
	      Codegen.llvm_context = context;
	      Codegen.llvm_module = the_module;
	      Codegen.llvm_builder = builder;
	      Codegen.named_values = Hashtbl.create 10} f in
  begin
    Llvm.dump_value l;
    Llvm.dump_module the_module
  end

let id = Ast.L0.Term.Lambda ("x", Ast.L0.Term.Variable"x");;

let suite = OUnit2.test_list [Test_lexer.suite; Test_inference.suite]
let () = OUnit2.run_test_tt_main suite
