open Core.Std
open OUnit2
open Pparser

open Plang.Term

let test_cases = [
    ("let x = x in x", Result.Ok (let_ "x" (var "x") (var "x")));
    ("let x = 12.12 in y", Result.Ok (let_ "x" (float 12.12) (var "y")));
    ("x", Result.Ok (var "x"));
    ("let f = fn x . x in [f 12.0]", Result.Ok (let_ "f" (fn "x" (var "x")) (Prod [var "f"; Literal (Literal.Float 12.0)])));
    ("fn x . x", Result.Ok (fn "x" (var "x")));
    ("(x y z)", Result.Ok (comp [var "x"; var "y"; var "z"]));
    ("([+ x] y)", Result.Ok (comp [prod [Atom "+"; var "x"]; var "y"]));
    ("123.12", Result.Ok (Literal (Literal.Float 123.12)));
    ("123", Result.Ok (Literal (Literal.Int (Int64.of_int 123))));
    ("(Δ [Δ Δ] [+ +] ·)", Result.Ok (comp [Atom "Δ"; prod [Atom "Δ"; Atom "Δ"]; prod [Atom "+"; Atom "+"]; Atom "·"]));
  ]

let parse_all str =
  let ps = from_string str in
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
