module L0 = struct
  module Term = struct
    type varname = string
    type t =
      | Variable of varname
      | Application of t * t
      | Lambda of varname * t
      | Let of varname * t * t
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
  end

  module PPrint = struct
    let rec string_of_term : Term.t -> string = function
      | Term.Variable v -> v
      | Term.Application (e,e') -> (string_of_term e) ^ " " ^ (string_of_term e')
      | Term.Lambda (v,e) -> "fn " ^ v ^ " . " ^ (string_of_term e)
      | Term.Let (v,e,e') -> "let " ^ v ^ " = " ^ (string_of_term e) ^ " . " ^ (string_of_term e')
    let rec string_of_type : Type.t -> string = function
      | Type.TVar v -> assert false
      | Type.QVar v -> assert false
      | Type.TArrow (t,t',l) -> (string_of_type t) ^ "->" ^ (string_of_type t')
  end
end
module L1 = struct
  module Type = struct
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
end
