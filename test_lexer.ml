open OUnit2

type result =
  | Fail
  | Ok of Token.t list

open Token

let test_cases = [
    ("", Ok []);
    ("  \t\n\n\t\r\n\r", Ok []);
    ("())in,let_ _1Ma->==",
     Ok [LPAREN; RPAREN; RPAREN; IN; COMMA; IDENT "let_"; IDENT "_1Ma"; ARROW; EQUALS; EQUALS]);
    ("let fn in", Ok [LET; FUN; IN]);
    (";", Fail);
    ("~", Fail);
    ("forall", Ok [FORALL]);
    ("module", Ok [MODULE]);
  ]

let parse_all code =
  let lexbuf = Lexing.from_string code in
  let rec f acc =
    match Lexer.token lexbuf with
    | EOF -> acc
    | token -> f (token :: acc)
  in
  List.rev (f [])

let string_of_result = function
  | Fail -> "Fail"
  | Ok tokens -> "OK (" ^ String.concat ", " (List.map Lexer.string_of_token tokens) ^ ")"

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
		          let result =
			    try
			      Ok (parse_all code)
			    with Lexer.Error ->
			      Fail
		          in
			  assert_equal ~printer:string_of_result expected_result result

let suite =
  "test_lexer" >::: List.map make_single_test_case test_cases
