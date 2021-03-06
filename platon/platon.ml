open Core.Std
open Core_extended.Color_print


let parse_file filename =
  In_channel.create filename
  |> Lexbuf.from_channel
  |> Pparser.from_lexbuf
  |> Pparser.term


let f =
  let module E = Llang.Term in
  let module T = Llang.Type in
  E.Function (E.Prototype ("square", [| "x" |]),
                     E.Binary ("*", T.Base T.Double, E.Variable "x", E.Variable "x"))


let g =
  let module E = Llang.Term in
  let module T = Llang.Type in
  E.Function (E.Prototype ("double", [| "x" |]),
              E.Binary ("+", T.Base T.Double, E.Variable "x", E.Variable "x"))


let codegen_top =
  let open Result.Monad_infix in
  fun () ->
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "platon" in
  let builder = Llvm.builder context in
  let module Cg = Codegen in
  let cgcontext = {
      Cg.llcontext = context;
      Cg.llmodule = the_module;
      Cg.llbuilder = builder;
      Cg.named_values = Hashtbl.create ~hashable:String.hashable ()} in
  Cg.codegen_function cgcontext f >>= fun l ->
  Cg.codegen_function cgcontext g >>= fun l' ->
  begin
    let fpm = Llvm.PassManager.create_function the_module in
    Llvm_scalar_opts.add_instruction_combination fpm;
    Llvm_scalar_opts.add_reassociation fpm;
    Llvm_scalar_opts.add_gvn fpm;
    Llvm_scalar_opts.add_cfg_simplification fpm;
    ignore (Llvm.PassManager.initialize fpm);
    Llvm_analysis.assert_valid_module the_module;
    Llvm_analysis.assert_valid_function l;
    printf "%s\n" (Llang.Term.string_of_func f);
    Llvm.dump_module the_module;
    Result.Ok ()
  end

let () =
    match codegen_top () with
    | Result.Error err -> print_string (Codegen_error.to_string err);
    | Result.Ok res -> ()
