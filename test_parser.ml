open OUnit2
(* open Parser *)

type result =
  | Fail
  | Ok of Ast.L0.Term.t

let test_cases = [
    ("test.platon", Ok (Ast.L0.Term.Variable "x"))
  ]

let parse_all file =
  (* let ps = make_parser file in *)
  Ast.L0.Term.Variable "x"

let string_of_result = function
  | Fail -> "Fail"
  | Ok tokens -> "OK (" ^ ")"

let make_single_test_case (file, expected_result) =
  String.escaped file >:: fun _ ->
                  let result =
                try
                  Ok (parse_all file)
                with Lexer.Error ->
                  Fail
                  in
              assert_equal ~printer:string_of_result expected_result result

let suite =
  "test_parser" >::: List.map make_single_test_case test_cases
