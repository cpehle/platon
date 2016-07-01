open Core.Std

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
