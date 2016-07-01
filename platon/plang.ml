open Core.Std

module Term = struct
  type varname = string [@@deriving sexp]

  module Literal = struct
    type t =
      | Float of float
      | Int of Int64.t
      | String of string
      [@@deriving sexp]

    let to_string = function
      | Float f -> Float.to_string f
      | Int i -> Int64.to_string i
      | String s -> "\"" ^ s ^ "\""
  end

  type t =
    | Variable of varname (** variable "x" *)
    | Atom of varname
    | Literal of Literal.t  (** literal "3.14" or "234235" *)
    | Prod of t list (** [e_0 e_1 ... e_n] *)
    | Comp of t list (** (e_0 e_1 ... e_n) *)
    | Rel of t list  (** {e_0 e_1 ... e_n} *)
    | Application of t * t
    | Lambda of varname * t
    | Let of varname * t * t
    [@@deriving sexp]

  let rec to_string = function
    | Variable (varname) -> varname
    | Literal l -> (Literal.to_string l)
    | Atom a  -> a
    | Rel  tl -> "{" ^ String.concat ~sep:" " (List.map tl ~f:to_string) ^ "}"
    | Comp tl -> "(" ^ String.concat ~sep:" " (List.map tl ~f:to_string) ^ ")"
    | Prod tl -> "[" ^ String.concat ~sep:" " (List.map tl ~f:to_string) ^ "]"
    | Application (e,e') -> (to_string e) ^ " " ^ (to_string e')
    | Lambda (v,e) -> "fn " ^  v ^ " . " ^ (to_string e)
    | Let (v,e,e') -> "let " ^ v ^ " = " ^ (to_string e) ^ " in " ^ (to_string e')

  let var x = Variable x
  let app t t' = Application (t, t')
  let prod tl  = Prod tl
  let comp tl = Comp tl
  let fn v t = Lambda (v,t)
  let let_ v t t' = Let (v,t,t')
  let float f = Literal (Literal.Float f)
  let int i = Literal (Literal.Int i)

  let lookup v e = ()

  let rec eval e tm = match tm with
    | Variable v -> lookup v e
    | Literal l -> ()
    | Atom a -> ()
    | Rel r -> ()
    | Prod tl -> ()
    | Comp tl -> ()
    | Application (e, e') -> ()
    | Lambda (v,e) -> ()
    | Let (v,e,e') -> ()
  and apply e tm = ()
end


module Type = struct
  type qname = string [@@deriving sexp]
  type level = int [@@deriving sexp]
  let generic_level =  100000000
  let marked_level = -1

  type t =
    | TVar of tv ref
    | QVar of qname
    | TArrow of t * t * levels
   and tv = Unbound of string * level | Link of t
   and levels = { mutable level_old : level; mutable level_new : level } [@@deriving sexp]
  let default_levels = { level_old = generic_level;  level_new = generic_level}


  let rec to_string : t -> string = function
    | TVar v -> assert false
    | QVar v -> v
    | TArrow (t,t',l) -> (to_string t) ^ "->" ^ (to_string t')
end

module Module = struct
  type t =
    | Module of Term.t * Term.t list [@@deriving sexp]
end
