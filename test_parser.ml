open Core.Std
open OUnit2
open Parser

let test_cases = [
    ("test.platon", Result.Ok [(Ast.L0.Term.Let ("x", Ast.L0.Term.Variable ("y"), Ast.L0.Term.Variable ("z")));
                        (Ast.L0.Term.Let ("z", Ast.L0.Term.Variable ("x"), Ast.L0.Term.Variable ("p")));
                        (Ast.L0.Term.Let ("g", Ast.L0.Term.Variable ("d"), Ast.L0.Term.Variable ("q")))]);
    ("test_fun.platon", Result.Ok [(Ast.L0.Term.Let ("f", Ast.L0.Term.Lambda ("x", Ast.L0.Term.Variable "y"), Ast.L0.Term.Variable "z")); (Ast.L0.Term.Let ("f", Ast.L0.Term.Lambda ("x", Ast.L0.Term.Lambda ("y", Ast.L0.Term.Variable "t")), Ast.L0.Term.Variable "z"))]);
    ("test_module.platon", Result.Ok [])
  ]

let parse_all file =
  let ps = make_parser file in
  term_list ps Token.EOF []

let string_of_result = function
  | Result.Error (a,l) -> Parse_error.to_string a
  | Result.Ok ast -> String.concat ~sep:"\n" (List.map ast ~f:Ast.L0.Term.to_string)

let make_single_test_case (file, expected_result) =
  String.escaped file >:: fun _ ->
  let result = parse_all file in assert_equal ~printer:string_of_result expected_result result

let suite =
  "test_parser" >::: List.map ~f:make_single_test_case test_cases
