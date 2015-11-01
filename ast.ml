open Core.Std

module type IDENT = sig
  type t
  val create: string -> t
  val name: t -> string
  val equal: t -> t -> bool
  type ’a tbl
  val emptytbl: ’a tbl
  val add: t -> ’a -> ’a tbl -> ’a tbl val find: t -> ’a tbl -> ’a
end

module Ident : IDENT = struct
  type t = {name: string; stamp: int} let currstamp = ref 0
  let create s =
    currstamp := !currstamp + 1; {name = s; stamp = !currstamp}
  let name id = id.name
  let equal id1 id2 = (id1.stamp = id2.stamp)
  type ’a tbl = (t * ’a) list
  let emptytbl = []
  let add id data tbl = (id, data) :: tbl let rec find id1 = function
    |  [] -> raise Not_found
    | (id2, data) :: rem ->
       if equal id1 id2 then data else find id1 rem
end

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
      | Literal of Literal.t
      | Application of t * t
      | Lambda of varname * t
      | Let of varname * t * t

    let rec to_string = function
      | Variable v -> v
      | Literal l -> (Literal.to_string l)
      | Application (e,e') -> (to_string e) ^ " " ^ (to_string e')
      | Lambda (v,e) -> "fn " ^ v ^ " . " ^ (to_string e)
      | Let (v,e,e') -> "let " ^ v ^ " = " ^ (to_string e) ^ " in " ^ (to_string e')
    let var x = Variable x
    let app t t' = Application (t, t')
    let lam v t = Lambda (v,t)
    let let_ v t t'  = Let (v,t,t')
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
      | Module of Term.t list
  end
  module Declaration = struct
    type t =
      | Module of Module.t
      | Type of string * Type.t
      | Term of string * Term.t
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
  end
  type proto = Prototype of string * string array
  type func = Function of proto * Term.t

  module PPrint = struct
      let rec string_of_term : Term.t -> string = function
        | Term.Variable v -> v
        | Term.Binary (op,ty,e1,e2) -> "(" ^ string_of_term e1 ^ op ^ string_of_term e2 ^ ")"
        | Term.Call (f,args) -> ""
        | Term.Literal l -> string_of_literal l
      and string_of_literal : Term.literal -> string = function
        | Term.Double d -> Float.to_string d
        | Term.Integer i -> string_of_int i
      let string_of_func : func -> string = function
        | Function (Prototype (s, ss), t) -> s ^ "[" ^ String.concat ~sep:" " (Array.to_list ss) ^ "]" ^ "->" ^ string_of_term t
    end
  end
