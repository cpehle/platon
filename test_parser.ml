open Core.Std
open OUnit2
open Parser

type result =
  | Fail
  | Ok of Ast.L0.Term.t list

let test_cases = [
    ("test.platon", Ok [(Ast.L0.Term.Let ("x", Ast.L0.Term.Variable ("y"), Ast.L0.Term.Variable ("z")));
                        (Ast.L0.Term.Let ("z", Ast.L0.Term.Variable ("x"), Ast.L0.Term.Variable ("p")));
                        (Ast.L0.Term.Let ("g", Ast.L0.Term.Variable ("d"), Ast.L0.Term.Variable ("q")))]);
    ("test_fun.platon", Ok [(Ast.L0.Term.Let ("f", Ast.L0.Term.Lambda ("x", Ast.L0.Term.Variable "y"), Ast.L0.Term.Variable "z")); (Ast.L0.Term.Let ("f", Ast.L0.Term.Lambda ("x", Ast.L0.Term.Lambda ("y", Ast.L0.Term.Variable "t")), Ast.L0.Term.Variable "z"))])]

let parse_all file =
  let ps = make_parser file in
  parse_term_list ps [] Token.EOF

let string_of_result = function
  | Fail -> "Fail"
  | Ok ast -> String.concat ~sep:"\n" (List.map ast ~f:Ast.L0.PPrint.string_of_term)

let make_single_test_case (file, expected_result) =
  String.escaped file >:: fun _ ->
                          let result =
                            try
                              Ok (parse_all file)
                            with Lexer.Error ->
                              Fail
                          in assert_equal ~printer:string_of_result expected_result result
let suite =
  "test_parser" >::: List.map ~f:make_single_test_case test_cases
