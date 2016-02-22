open Core.Std
open Core_extended.Color_print

let parse_file filename =
  In_channel.create filename
  |> Lexbuf.from_channel
  |> Pparser.from_lexbuf
  |> Pparser.term

let f =
  let module E = Ast.L1.Term in
  let module T = Ast.L1.Type in
  E.Function (E.Prototype ("square", [| "x" |]),
                     E.Binary ("*", T.Base T.Double, E.Variable "x", E.Variable "x"))

let g =
  let module E = Ast.L1.Term in
  let module T = Ast.L1.Type in
  E.Function (E.Prototype ("double", [| "x" |]),
                     E.Binary ("+", T.Base T.Double, E.Variable "x", E.Variable "x"))


let codegen_top =
  let open Result.Monad_infix in
  fun () ->


  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "platon" in
  let builder = Llvm.builder context in
  let cgcontext = {
      Codegen.llcontext = context;
      Codegen.llmodule = the_module;
      Codegen.llbuilder = builder;
      Codegen.named_values = Hashtbl.create ~hashable:String.hashable ()} in
  Codegen.codegen_function cgcontext f >>= fun l ->
  Codegen.codegen_function cgcontext g >>= fun l' ->
  begin
    ignore (Llvm_executionengine.initialize ());
    let e = Llvm_executionengine.create the_module in
    let fpm = Llvm.PassManager.create_function the_module in
    Llvm_target.DataLayout.add_to_pass_manager fpm (Llvm_executionengine.data_layout e);
    Llvm_scalar_opts.add_instruction_combination fpm;
    Llvm_scalar_opts.add_reassociation fpm;
    Llvm_scalar_opts.add_gvn fpm;
    Llvm_scalar_opts.add_cfg_simplification fpm;
    ignore (Llvm.PassManager.initialize fpm);
    Llvm_analysis.assert_valid_module the_module;
    Llvm_analysis.assert_valid_function l;
    printf "%s\n" (Ast.L1.Term.string_of_func f);
    Llvm.dump_module the_module;
    Result.Ok ()
  end

let () =
    match codegen_top () with
    | Result.Error err -> print_string (Codegen_error.to_string err);
    | Result.Ok res -> ()
