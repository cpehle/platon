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

let top_type_check : Ast.L0.Term.t -> Ast.L0.Type.t = fun exp ->
  let initial_state = { TypeInference.gensym_counter = 0;
                        TypeInference.current_level = 0;
                        TypeInference.to_be_level_adjusted = []} in
  TypeInference.reset_type_inference_state initial_state;
  TypeInference.reset_level_adjustment initial_state;
  let ty = TypeInference.typeof initial_state [] exp in
  TypeInference.cycle_free ty;
  ty

module Test_inference = struct
    open OUnit2
    open Ast.L0.Term
    open Ast.L0.Type

let test1 text_ctxt = assert_equal
    (TArrow
       (TVar
          {contents =
             Link
               (TArrow
                  (TVar
                     {contents =
                        Link
                          (TArrow (TVar {contents = Unbound ("d", 1)},
                                   TVar {contents = Unbound ("e", 1)},
                                   {level_old = 1; level_new = 1}))},
                   TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
        TArrow
          (TVar
             {contents =
                Link
                  (TArrow (TVar {contents = Unbound ("d", 1)},
                           TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}))},
           TArrow (TVar {contents = Unbound ("d", 1)},
                   TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}),
           {level_old = 1; level_new = 1}),
        {level_old = 1; level_new = 1}))
    (  top_type_check (Lambda ("x", Lambda ("y",Let ("x",Application (Variable"x",Variable"y"),
                                                     Lambda ("x",Application (Variable"y",Variable"x"))))))
    )
let test2 text_ctxt = assert_equal
    (TArrow (TVar {contents = Unbound ("a", 1)},
             TVar {contents = Unbound ("a", 1)}, {level_old = 1; level_new = 1}))
    (top_type_check (Lambda ("x", Let ("y",Variable"x", Variable"y"))))

let suite =
  "test_inference">:::
  ["test1">:: test1;
   "test2">:: test2]
  end

let suite = OUnit2.test_list [Test_lexer.suite; Test_inference.suite]
let () = OUnit2.run_test_tt_main suite
