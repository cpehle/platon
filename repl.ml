open Core.Std
open Core_extended.Color_print
open Core_extended.Readline


let top_type_check : Ast.L0.Term.t -> Ast.L0.Type.t = fun exp ->
  let initial_state = { TypeInference.gensym_counter = 0;
                        TypeInference.current_level = 0;
                        TypeInference.to_be_level_adjusted = []} in
  TypeInference.reset_type_inference_state initial_state;
  TypeInference.reset_level_adjustment initial_state;
  let ty = TypeInference.typeof initial_state [] exp in
  TypeInference.cycle_free ty;
  ty

let () =
  let open Result.Monad_infix in
  let rec loop filename = fun () ->
    let prompt = color_sprintf ~color:`Red "> " in
    let res = input_line ~prompt () in
   match res with
   | None -> ()
   | Some s ->
      let ps = Pparser.from_string s in
      match Pparser.term ps with
      | Result.Ok tm ->
         printf "%s\n" (Ast.L0.Term.to_string tm);
         loop filename ()
      | Result.Error (err, l, l') ->
         print_string (Parse_error.mark_string s l.Position.columnoffset l'.Position.columnoffset);
         let error = sprintf "%s -- %s : %s\n" (Position.to_string l) (Position.to_string l') (Parse_error.to_string err)
         in
         print_string error;
         loop filename ()
  in
  let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string) in
  let command =
  Command.basic
    ~summary:"Read eval print loop"
    ~readme:(fun () -> "This is the read eval print loop of platon")
    spec
    (fun filename () -> loop filename ()) in
  Command.run command
