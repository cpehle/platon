open Core.Std
open Core_extended.Color_print
open Core_extended.Readline

let () =
  let module Ast = Ast.L1 in
  let f = Ast.Term.Function (Ast.Term.Prototype ("square", [| "x" |]),
                        Ast.Term.Binary ("*", Ast.Type.Base Ast.Type.Double, Ast.Term.Variable "x", Ast.Term.Variable "x")) in
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "platon" in
  let builder = Llvm.builder context in
  let l = Codegen.codegen_function {
              Codegen.llcontext = context;
              Codegen.llmodule = the_module;
              Codegen.llbuilder = builder;
              Codegen.named_values = Hashtbl.create ~hashable:String.hashable ()}
                                   f in
  begin
    ignore(Llvm_executionengine.initialize ());
    let e = Llvm_executionengine.create the_module in
    let fpm = Llvm.PassManager.create_function the_module in
    Llvm_target.DataLayout.add_to_pass_manager fpm (Llvm_executionengine.data_layout e);
    Llvm_scalar_opts.add_instruction_combination fpm;
    Llvm_scalar_opts.add_reassociation fpm;
    Llvm_scalar_opts.add_gvn fpm;
    Llvm_scalar_opts.add_cfg_simplification fpm;
    ignore(Llvm.PassManager.initialize fpm);
    Llvm_analysis.assert_valid_module the_module;
    Llvm_analysis.assert_valid_function l;
  end

let id = Ast.L0.Term.Lambda ("x", Ast.L0.Term.Variable ("x"));;

let suite = OUnit2.test_list [Test_lexer.suite; Test_inference.suite; Test_parser.suite]
let () =
  let open Result.Monad_infix in
  let rec loop = fun () ->
   let res = input_line ~prompt:">>> " () in
   match res with
   | None -> ()
   | Some s ->
      let ps = Parser.make_parser_from_string s in
      match Parser.term ps with
      | Result.Ok tm ->
         printf "%s\n" (Ast.L0.Term.to_string tm);
         loop ()
      | Result.Error (err, (fn, l, c)) ->
         let error = match fn with
         | Some n -> sprintf "%s:%i %i %s\n" n l c (Parse_error.to_string err)
         | None -> sprintf "%s:%i %i %s\n" "<interactive>" l c (Parse_error.to_string err)
         in
         print_string error;
         loop ()
  in begin
      colorprintf ~color:`Orchid "Running tests...\n";
      print_string (Parse_error.mark_string "This is a test." 2 3);
      loop ();
      OUnit2.run_test_tt_main suite;
    end
