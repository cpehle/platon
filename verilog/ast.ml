type direction = IN | OUT | INOUT

type port = {
    name : string;
    dir : direction;
}

type statement = I

type vmodule = {
    name : string;
    ports : port list;
    param : (string * int) list;
    body : statement list
}


type var = string

(* verilog types *)
type typ = | Logic of int * int

type 'a io = | Input of 'a | Output of 'a

type ('a, 'b) declaration_form =
  | Register of string * typ
  | Wire of string * typ
 and ('a,'b) module_form = {
  name : string;
  input : (var * typ) list;
  output : (var * typ) list;
  declaration : (('a,'b) declaration_form) list
}




type 'a case_clause = {
    case : 'a;
    body : string;
  }

type ('a,'b) statement_form  =
  | Always of 'a * 'b
  | Always_comb of 'a * 'b
  | Always_ff of 'a * 'b
  | Case of 'a * 'b (** case expr
                           A:
                           B:
                           default:
                        endcase
                      *)
  | Assign of 'a * 'b (** assign x = expr *)

type ('a, 'b) expr_form =
  | Var
  | Binop
  | Concat
and ('a, 'b) elt_form =
  | Bit of int * int
