open Core.Std

module Type = struct
    type arity = int
    type base =
      | Double
      | Integer
      [@@deriving sexp]
    type t =
      | Base of base
      | Arrow of t * t
      [@deriving sexp]

    let rec to_string : t -> string = function
      | Base b -> (match b with
                  | Double -> "Double"
                  | Integer -> "Integer")
      | Arrow (t, t') -> "(" ^ to_string t ^ "->" ^ to_string t' ^ ")"
end

module Term = struct
    type varname = string [@@deriving sexp]
    type literal =
      | Double of float
      | Integer of int
      [@@deriving sexp]
    type t =
      | Literal of literal
      | Variable of varname
      | Let of t * Type.t * t * t
      | Binary of string * Type.t * t * t
      | Call of string * t array
      [@@deriving sexp]

    type proto = Prototype of string * string array [@@deriving sexp]
    type func = Function of proto * t [@@deriving sexp]


    let rec to_string : t -> string = function
      | Variable v -> v
      | Binary (op,ty,e1,e2) -> "(" ^ to_string e1 ^ op ^ to_string e2 ^ ")"
      | Call (f,args) -> ""
      | Let (v, typ, t, t') -> "let " ^ to_string v ^ " : " ^ Type.to_string typ ^ "=" ^ to_string t ^ " in " ^ to_string t'
      | Literal l -> string_of_literal l
    and string_of_literal : literal -> string = function
      | Double d -> Float.to_string d
      | Integer i -> string_of_int i
    let string_of_func : func -> string = function
      | Function (Prototype (s, ss), t) -> s ^ "[" ^ String.concat ~sep:" " (Array.to_list ss) ^ "]" ^ "->" ^ to_string t
end
