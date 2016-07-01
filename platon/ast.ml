open Core.Std

type ('a, 'b) expr =
  | Let of var * 'a * 'a
  | Var of var
  with sexp
