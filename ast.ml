open Core.Std
(*
module type IDENT = sig
  type t
  val create: string -> t
  val name: t -> string
  val equal: t -> t -> bool
  type 'a tbl
  val emptytbl: 'a tbl
  val add: t -> 'a -> 'a tbl -> 'a tbl
  val find: t -> 'a tbl -> 'a
end

module Ident : IDENT = struct
  type t = {name: string; stamp: int}
  let currstamp = ref 0
  let create s =
    currstamp := !currstamp + 1; {name = s; stamp = !currstamp}
  let name id = id.name
  let equal id1 id2 = (id1.stamp = id2.stamp)
  type 'a tbl = (t * 'a) list
  let emptytbl = []
  let add id data tbl = (id, data) :: tbl
  let rec find id1 = function
    |  [] -> raise Not_found
    | (id2, data) :: rem ->
       if equal id1 id2 then data else find id1 rem
end



module type SUBST = sig
  type t
  val identity: t
  val add: Ident.t -> path -> t -> t val path: path -> t -> path
end *)

module type ID = sig
  type t
  val create : string -> t
  val name : t -> string
end

module Id : ID = struct
  type t = { id : int; name : string}

  let current_id = ref 0

  let name i = i.name

  let create s =
    incr current_id;
    { name = s; id = !current_id}

  let rename i =
    incr current_id;
    { i with id = !current_id}

  let same i1 i2 = i1 = i2
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
      | Prod of t list
      | Application of t * t
      | Lambda of varname * t
      | Let of varname * t * t
    let rec to_string = function
      | Variable (varname) -> varname
      | Literal l -> (Literal.to_string l)
      | Prod tl -> String.concat ~sep:" " (List.map tl ~f:to_string)
      | Application (e,e') -> (to_string e) ^ " " ^ (to_string e')
      | Lambda (v,e) -> "fn " ^  v ^ " . " ^ (to_string e)
      | Let (v,e,e') -> "let " ^ v ^ " = " ^ (to_string e) ^ " in " ^ (to_string e')
    let var x = Variable x
    let app t t' = Application (t, t')
    let prod tl  = Prod tl
    let fn v t = Lambda (v,t)
    let let_ v t t' = Let (v,t,t')
    let float f = Literal (Literal.Float f)
    let int i = Literal (Literal.Int i)
  end

  (* module Relation = struct *)
  (*   type constant = string *)
  (*   type variable = { name : Core.Std.String.t; level : Core.Std.Int.t} *)
  (*   type t = *)
  (*     | Var of variable *)
  (*     | Const of constant *)
  (*     | App of constant * t Core.Std.List.t *)
  (*   type atom = constant * t Core.Std.List.t *)
  (*   type clause = atom Core.Std.List.t *)
  (*   type assertion = atom * clause *)

  (*   let compare_var { name = vn; level = vl } { name = vn'; level = vl' } = *)
  (*     let cs = (String.compare vn vn') in if (cs = 0) then Int.compare vl vl' else cs *)

  (*   type database = assertion Core.Std.List.t *)
  (*   let rec subst_relation env = function *)
  (*     | Var x as e -> *)
  (*        (let e' = Core.Std.String.Map.find env x in *)
  (*         if e = e' then e' else subst_relation env e') *)
  (*     | Const _ as e -> e *)
  (*     | App (c, ls) -> App (c, List.map ~f:(subst_relation env)) *)
  (* end *)

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
      | Trace of (string List.t) * t
      | Call of string * t array
    type proto = Prototype of string * string array
    type func = Function of proto * t

    let rec to_string : t -> string = function
      | Variable v -> v
      | Trace (names, tm) -> "tr(" ^ String.concat ~sep:" " names ^ ") {" ^ to_string tm  ^ "}"
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
