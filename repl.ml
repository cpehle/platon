open Core.Std
open Core_extended.Color_print
open Core_extended.Readline

let () =
  let open Result.Monad_infix in
  let rec loop = fun () ->
    let prompt = color_sprintf ~color:`Red "> " in
    let res = input_line ~prompt () in
   match res with
   | None -> ()
   | Some s ->
      let ps = Pparser.from_string s in
      match Pparser.term ps with
      | Result.Ok tm ->
         printf "%s\n" (Ast.L0.Term.to_string tm |> color_sprintf ~color:`Blue "%s");
         loop ()
      | Result.Error (err, l, l') ->
         print_string (Parse_error.mark_string s l.Position.columnoffset l'.Position.columnoffset);
         let error = sprintf "%s -- %s : %s\n" (Position.to_string l) (Position.to_string l') (Parse_error.to_string err)
         in
         print_string error;
         loop ()
  in
  let spec =
  let open Command.Spec in
  empty
  in
  let command =
  Command.basic
    ~summary:"Read eval print loop"
    ~readme:(fun () -> "This is the read eval print loop of platon")
    spec
    (fun () -> loop ()) in
  Command.run command
