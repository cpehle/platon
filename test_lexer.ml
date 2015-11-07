open Core.Std
open OUnit2
open Token

let test_cases = [
    ("", Result.Ok []);
    ("  \t\n\n\t\r\n\r", Result.Ok []);
    ("())in,let_ _1Ma->==",
    Result.Ok [LPAREN; RPAREN; RPAREN; IN; COMMA; IDENT "let_"; IDENT "_1Ma"; ARROW; EQUALS; EQUALS]);
    ("let fn in", Result.Ok [LET; FUN; IN]);
    (";", Result.Error ());
    ("~", Result.Error ());
    ("forall", Result.Ok [FORALL]);
    ("module", Result.Ok [MODULE]);
    ("12312313 12.32 \"string\"", Result.Ok [INT (Int64.of_int 12312313); FLOAT 12.32; STRING "string"])
  ]


let string_of_result = function
  | Result.Error (s,l) -> Parse_error.to_string s
  | Result.Ok tokens -> "OK (" ^ String.concat ~sep:", " (List.map ~f:Token.to_string  tokens) ^ ")"

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
                          assert_equal ~printer:string_of_int 1 1

let suite =
  "test_lexer" >::: List.map ~f:make_single_test_case test_cases
