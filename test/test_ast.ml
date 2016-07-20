open Core.Std
open OUnit2
open Ast.L0


let test_cases =
  [
    ("", Result.Ok "");
  ]

let string_of_result = function
  | Result.Error s -> s
  | Result.Ok s-> s

let make_single_test_case (code, expected_result) =
  String.escaped code >:: fun _ ->
                          let res = Result.Ok "" in
                          assert_equal ~printer:string_of_result res expected_result

let suite =
  "test_ast" >::: List.map ~f:make_single_test_case test_cases
