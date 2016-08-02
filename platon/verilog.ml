open Core.Std

type var = string

(* verilog types *)
type typ =
  | Logic of int * int
  | Bit of int * int

type ('a, 'b) declaration_form =
  | Register of string * typ
  | Wire of string * typ
and ('a,'b) module_form = {
  name : string;
  input : (var * typ) list;
  output : (var * typ) list;
  body : (('a,'b) declaration_form)
}

type ('a,'b) statement_form  =
  | Always of 'a * 'b
  | Case of 'a * 'b
  | Instance of 'a * 'b

type ('a, 'b) expr_form =
  | Var
  | Binop
  | Concat
and ('a, 'b) elt_form =
  | Bit of int * int

(* let atom = { atom_style = Some "atom" } *)
(* let label = { label with label_style = Some "label" } *)
(* let begin_style = ({ label with ident_after_label = 0 }, *)
(*                    ("begin", ";", "end", {list with stick_to_label = false})) *)

(* let format_module_definition (body_label, body_param) name param body = *)
(*   Easy_format.Label ((Easy_format.Label (()), body_label), Easy_format.List (body_param, List.map (fun s -> Easy_format.Atom (s, atom)) body)) *)

let parameters = { Easy_format.list with
                   Easy_format.opening_style = Some "punct";
                   Easy_format.separator_style = Some "punct";
                   Easy_format.closing_style = Some "punct"
                 }

let pp_module m =
  print_string (Easy_format.Pretty.to_string (format_module m) ^ "\n")

let _ = pp_module ()
