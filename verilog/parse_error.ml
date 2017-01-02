open Core.Std
open Core_extended.Color_print

type t =
  | LexError of string
  | UnexpectedToken of string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedTokenWithExpectation of string * string
  | UnexpectedReserverd
  | UnexpectedIdentifier
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
  | InternalError -> "Internal error"

let mark_string s from until =
  let strlen = String.length s in
  let mark = String.make strlen ' ' in
  if (0 <= from) && (from < until) && (until < strlen) then begin
      for i = from to until do
        String.set mark i '~';
      done;
    end else ();
  if (0 <= from) && (until < strlen) then begin
      String.set mark from '^';
      String.set mark until '^';
    end else ();
  let mark =  color ~color:`Red mark in
  (String.concat ~sep:"\n" [s;mark]) ^ "\n"
