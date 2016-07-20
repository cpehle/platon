open Core.Std

let rec input_line prompt cb =
  match (LNoise.linenoise prompt) with
  | None -> ()
  | Some v ->
    cb v;
    input_line prompt cb

let () =
  let open Result.Monad_infix in
  let prompt = "> " in
  let loop = input_line prompt (fun s ->
      let ps = Pparser.from_string s in
      match Pparser.term ps with
      | Result.Ok tm ->
         (Plang.Term.to_string tm) |> Printf.sprintf "%s"  |> print_endline
      | Result.Error (err, l, l') ->
         print_endline (Parse_error.mark_string s l.Position.columnoffset l'.Position.columnoffset);
         let error = sprintf "%s -- %s : %s" (Position.to_string l) (Position.to_string l') (Parse_error.to_string err)
         in
         print_endline error;
                    )
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
    (fun () -> loop) in
  Command.run command
