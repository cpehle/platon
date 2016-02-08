open Core.Std

module L0 = struct
  module Term = struct
    type varname = string
    module Literal = struct
      type t =
      | Float of float
      | Int of Int64.t
      | String of string
      let to_string = function
        | Float f -> Float.to_string f
        | Int i -> Int64.to_string i
        | String s -> "\"" ^ s ^ "\""
    end
    type t =
      | Variable of varname
      | Atom of varname
      | Literal of Literal.t
      | Prod of t list (** [e_0 e_1 ... e_n] *)
      | Comp of t list (** (e_0 e_1 ... e_n) *)
      | Rel of t list  (** {e_0 e_1 ... e_n} *)
      | Application of t * t
      | Lambda of varname * t
      | Let of varname * t * t
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
    type qname = string
    type level = int
    let generic_level =  100000000
    let marked_level = -1
    type t =
      | TVar of tv ref
      | QVar of qname
      | TArrow of t * t * levels
    and tv = Unbound of string * level | Link of t
    and levels = { mutable level_old : level; mutable level_new : level }
    let default_levels = { level_old = generic_level;  level_new = generic_level}


    let rec to_string : t -> string = function
      | TVar v -> assert false
      | QVar v -> v
      | TArrow (t,t',l) -> (to_string t) ^ "->" ^ (to_string t')
  end

  module Module = struct
    type t =
      | Module of Term.t * Term.t list
  end
end

module L1 = struct
  module Type = struct
    type arity = int
    type base =
      | Double
      | Integer
    type t =
      | Base of base
      | Arrow of t * t
  end

  module Term = struct
    type varname = string

    type literal =
      | Double of float
      | Integer of int
    type t =
      | Literal of literal
      | Variable of varname
      | Binary of string * Type.t * t * t
      | Call of string * t array
    type proto = Prototype of string * string array
    type func = Function of proto * t

    let rec to_string : t -> string = function
      | Variable v -> v
      | Binary (op,ty,e1,e2) -> "(" ^ to_string e1 ^ op ^ to_string e2 ^ ")"
      | Call (f,args) -> ""
      | Literal l -> string_of_literal l
    and string_of_literal : literal -> string = function
      | Double d -> Float.to_string d
      | Integer i -> string_of_int i
    let string_of_func : func -> string = function
      | Function (Prototype (s, ss), t) -> s ^ "[" ^ String.concat ~sep:" " (Array.to_list ss) ^ "]" ^ "->" ^ to_string t
  end
end
