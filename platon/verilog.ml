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

let _module = { Easy_format.atom_style = Some "module" }

let format_module m =
  Easy_format.Atom ("module", _module)

let parameters = { Easy_format.list with
                   Easy_format.opening_style = Some "punct";
                   Easy_format.separator_style = Some "punct";
                   Easy_format.closing_style = Some "punct"
                 }

let pp_module m =
  print_string (Easy_format.Pretty.to_string (format_module m) ^ "\n")
