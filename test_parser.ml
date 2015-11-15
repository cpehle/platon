open Core.Std
open OUnit2
open Pparser

open Ast.L0.Term


let test_cases = [
    ("let x = x in x", Result.Ok (let_ "x" (var "x") (var "x")));
  ]


let parse_all str =
  let ps = make_parser_from_string str in
  term ps

let string_of_result = function
  | Result.Error (a,l,l') -> Parse_error.to_string a
  | Result.Ok ast -> to_string ast

let make_single_test_case (str, expected_result) =
  String.escaped str >:: fun _ ->
  let result = parse_all str in
  assert_equal ~printer:string_of_result expected_result result

let suite =
  "test_parser" >::: List.map ~f:make_single_test_case test_cases
