open Core.Std
open OUnit2
open Token

let test_cases =
  let lexerror = Result.Error (Parse_error.LexError "Unexpected character", Position.default_position, Position.default_position) in
  let ok x = Result.Ok x in
  [
    ("", ok []);
    ("  \t\n\n\t\r\n\r", ok []);
    ("let fn in", ok [LET; FUN; IN]);
    (";",  lexerror);
    ("~", ok [ATOM "~"]);
    ("12.1", ok [FLOAT 12.1]);
    ("[12.1 12 23.23 1.233]", ok [LBRACKET; FLOAT 12.1; INT (Int.to_int64 12); FLOAT 23.23; FLOAT 1.233; RBRACKET]);
    ("שדגשדג", Result.Ok [IDENT "שדגשדג"]);
    ("Δ", ok [IDENT "Δ"]);
    ("forall", Result.Ok [FORALL]);
    ("module", Result.Ok [MODULE]);
    ("→", Result.Ok [RIGHTARROW]);
    ("0b100", Result.Ok [INT (Int64.of_int 4)]);
    ("0x16", Result.Ok [INT (Int64.of_int (16+6))]);
    ("12312313 12.32 \"string\"", Result.Ok [INT (Int64.of_int 12312313); FLOAT 12.32; STRING "string"])
  ]

let string_of_result = function
  | Result.Error (s,l,l') -> Parse_error.to_string s
  | Result.Ok tokens -> "OK (" ^ String.concat ~sep:", " (List.map ~f:Token.to_string  tokens) ^ ")"

let parse_all code =
  let open Result.Monad_infix in
  let lexbuf = Lexbuf.from_string code in
  let rec f acc =
    Plexer.token lexbuf >>= fun tok ->
    match tok with
    | EOF -> Result.return acc
    | _ -> f (tok :: acc)
  in f [] >>| List.rev

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
                          let res = parse_all code in
                          assert_equal ~printer:string_of_result res expected_result

let suite =
  "test_lexer" >::: List.map ~f:make_single_test_case test_cases
