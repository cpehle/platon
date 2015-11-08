open Core.Std

type t =
    | LexError of string
    | UnexpectedToken of string
    | UnexpectedTokenWithSuggestion of string * string
    | UnexpectedTokenWithExpectation of string * string
    | UnexpectedReserverd
    | UnexpectedIdentifier
    | UnexpectedTokenExpected of Token.t * Token.t
    | InternalError
let to_string = function
  | LexError s -> s
  | UnexpectedToken s -> "Unexpected token `" ^ s ^"`"
  | UnexpectedTokenWithSuggestion (token, suggestion) ->
     Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?"
                    token
                    suggestion
  | UnexpectedTokenWithExpectation (token, expectation) ->
     Printf.sprintf "Unexpected token `%s`. Expected %s."
                    token
                    expectation
  | UnexpectedIdentifier -> "Unexpected identifier"
  | UnexpectedReserverd -> "Unexpected reserved"
  | UnexpectedTokenExpected  (t,t') ->  Printf.sprintf "Unexpected token `%s`, expected `%s`"
                                                       (Token.to_string t) (Token.to_string t')
  | InternalError -> "Internal error"

let mark_string s from until =
  let strlen = String.length s in
  let mark = String.make strlen ' ' in
  if (0 <= from) && (from < until) && (until < strlen)
  then
  for i = from to until do
    String.set mark i '~';
  done;
  String.set mark from '^';
  String.set mark until '^';
  (String.concat ~sep:"\n" [s;mark]) ^ "\n"
