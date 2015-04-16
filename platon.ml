module Lexer = struct
  end
module Parser = struct
  end
module Ast = struct
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
  end

module TypeInference = struct
    open Ast.L0

    type type_inference_state = {
        mutable gensym_counter : int;
        mutable current_level  : int;
        mutable to_be_level_adjusted : Type.t list;
      }
    let reset_gensym : type_inference_state -> unit =
      fun s -> s.gensym_counter <- 0
    let reset_level : type_inference_state -> unit = fun s -> s.current_level <- 1
    let reset_level_adjustment : type_inference_state -> unit =
      fun s -> s.to_be_level_adjusted <- []
    let reset_type_inference_state : type_inference_state -> unit =
      fun s -> reset_level s; reset_gensym s
    let enter_level : type_inference_state -> unit =
      fun s -> s.current_level <- s.current_level + 1
    let leave_level : type_inference_state -> unit =
      fun s -> s.current_level <- s.current_level - 1
    let gensym : type_inference_state -> string = fun s ->
      let n = s.gensym_counter in
      s.gensym_counter <- s.gensym_counter + 1;
      if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
      else "t" ^ string_of_int n
    let new_var : type_inference_state -> Type.t =
      fun s -> Type.TVar (ref (Type.Unbound (gensym s, s.current_level)))
    let new_arrow : type_inference_state -> Type.t -> Type.t -> Type.t =
      fun s ty1 ty2 -> Type.TArrow (ty1, ty2, {Type.level_new = s.current_level; Type.level_old = s.current_level})

(* Chase the links of bound variables, returning either
   a free variable or a constructed type.
   OCaml's typing/btype.ml has the same function with the same name.
   Unlike OCaml, we do path compression.
     *)
    let rec repr : Type.t -> Type.t = function
      | Type.TVar ({contents = Type.Link t} as tvr) ->
	       let t = repr t in
	       tvr := Type.Link t; t
      | t -> t

    let get_level : Type.t -> Type.level = function
      | Type.TVar {contents = Type.Unbound (_,l)} -> l
      | Type.TArrow (_,_,ls) -> ls.Type.level_new
      | _ -> assert false

    let update_level : type_inference_state -> Type.level -> Type.t -> unit = fun s l -> function
      |Type.TVar ({contents = Type.Unbound (n,l')} as tvr) ->
        assert (not (l' = Type.generic_level));
        if l < l' then
          tvr := Type.Unbound (n,l)
      | Type.TArrow (_,_,ls) as ty ->
         assert (not (ls.Type.level_new = Type.generic_level));
         if ls.Type.level_new = Type.marked_level then failwith "occurs check";
         if l < ls.Type.level_new then begin
             if ls.Type.level_new = ls.Type.level_old then
               s.to_be_level_adjusted <- ty :: s.to_be_level_adjusted;
             ls.Type.level_new <- l
           end
      | _ -> assert false

    let rec unify : type_inference_state -> Type.t -> Type.t -> unit =
      fun s t t' ->
      if t == t' then ()
      else match (repr t, repr t') with
	         | (Type.TVar ({contents = Type.Unbound (_,l1)} as tv1) as t1,
	            (Type.TVar ({contents = Type.Unbound (_,l2)} as tv2) as t2)) ->
              if l1 > l2 then tv1 := Type.Link t2 else tv2 := Type.Link t1
	         | (Type.TVar ({contents = Type.Unbound (_,l)} as tv),t')
	         | (t', Type.TVar ({contents = Type.Unbound (_,l)} as tv)) ->
		          update_level s l t';
              tv := Type.Link t'
	         | (Type.TArrow (tyl1, tyl2,ll), Type.TArrow (tyr1, tyr2, lr)) ->
	            if ll.Type.level_new = Type.marked_level || lr.Type.level_new = Type.marked_level then
		            failwith "cycle: occurs check.";
	            let min_level = min ll.Type.level_new lr.Type.level_new in
	            ll.Type.level_new <- Type.marked_level;
              lr.Type.level_new <- Type.marked_level;
	            unify_lev s min_level tyl1 tyr1;
	            unify_lev s min_level tyl2 tyr2;
	            ll.Type.level_new <- min_level; lr.Type.level_new <- min_level
           | _ -> assert false
    and unify_lev s l ty1 ty2 =
	    let ty1 = repr ty1 in
	    update_level s l ty1;
	    unify s ty1 ty2


    let force_delayed_adjustments : type_inference_state -> unit = fun s ->
      let rec loop acc level ty =
        match repr ty with
        | Type.TVar ({contents = Type.Unbound (name,l)} as tvr) when l > level -> tvr := Unbound (name,level); acc
        | Type.TArrow (_,_,ls) when ls.Type.level_new = Type.marked_level ->
           failwith "occurs check"
        | Type.TArrow (_,_,ls) as ty ->
           if ls.Type.level_new > level then ls.Type.level_new <- level;
           adjust_one acc ty
        | _ -> acc
      and adjust_one acc = function
        | Type.TArrow (_,_,ls) as ty when ls.Type.level_old <= s.current_level -> ty::acc
        | Type.TArrow (_,_,ls) when ls.Type.level_old = ls.Type.level_new -> acc
        | Type.TArrow (ty1,ty2,ls) ->
           let level = ls.Type.level_new in
           ls.Type.level_new <- Type.marked_level;
           let acc = loop acc level ty1 in
           let acc = loop acc level ty2 in
           ls.Type.level_new <- level;
           ls.Type.level_old <- level;
           acc
        | _ -> assert false
      in s.to_be_level_adjusted <- List.fold_left adjust_one [] s.to_be_level_adjusted

    let gen : type_inference_state -> Type.t -> unit = fun s ty ->
      force_delayed_adjustments s;
      let rec loop ty =
        match repr ty with
        | Type.TVar ({contents = Type.Unbound (name,l)} as tvr)
             when l > s.current_level -> tvr := Type.Unbound (name, Type.generic_level)
        | Type.TArrow (ty1, ty2, ls) when ls.Type.level_new > s.current_level ->
           let ty1 = repr ty1 and ty2 = repr ty2 in
               loop ty1; loop ty2;
               let l = max (get_level ty1) (get_level ty2) in
               ls.Type.level_old <- l; ls.Type.level_new <- l
        | _ -> ()
      in loop ty

    let inst : type_inference_state -> Type.t -> Type.t =
      let rec loop subst = function
        | Type.TVar {contents = Unbound (name,l)} when
               l = Type.generic_level
                     begin
                       try (List.assoc name subst, subst)
                       with Not_found ->
                         let tv = new_var s in
                         (tv, (name,tv)::subst)
                     end
        | Type.TVar {contents = Type.Link ty} -> loop subst ty
        | Type.TArrow (ty1,ty2,ls) when ls.Type.level_new = Type.generic_level ->
           let (ty1, subst) = loop subst ty1 in
           let (ty2, subst) = loop subst ty2 in
           (new_arrow s ty1 ty2, subst)
        | ty -> (ty,subst)
      in fun ty -> fst (loop [] ty)
 end


exception Error of string

module Codegen = struct
    module Ast = Ast.L1
    type codegen_state = {
	llvm_context : Llvm.llcontext;
	llvm_module  : Llvm.llmodule;
	llvm_builder : Llvm.llbuilder;
	named_values : (string, Llvm.llvalue) Hashtbl.t;
      }

    let codegen_literal ctx = function
      | Ast.Term.Double d -> Llvm.const_float (Llvm.double_type ctx.llvm_context) d
      | Ast.Term.Integer i -> Llvm.const_int (Llvm.integer_type ctx.llvm_context 32) i

    let rec codegen_term ctx = function
      | Ast.Term.Literal l -> codegen_literal ctx l
      | Ast.Term.Variable name ->
	 (try Hashtbl.find ctx.named_values name with
          | Not_found -> raise (Error "unknown variable name"))
      | Ast.Term.Binary (op,t,x,y) ->
	 let x' = codegen_term ctx x in
	 let y' = codegen_term ctx y in
	 begin
	   match op with
	   | "+" -> Llvm.build_fadd x' y' "addtmp" ctx.llvm_builder
	   | "-" -> Llvm.build_fsub x' y' "subtmp" ctx.llvm_builder
	   | "*" -> Llvm.build_fmul x' y' "multmp" ctx.llvm_builder
	   | _ -> raise (Error "invalid binary operation")
	 end
      | Ast.Term.Call (callee, args) ->
	 let callee =
	   match Llvm.lookup_function callee ctx.llvm_module with
	   | Some callee -> callee
	   | None -> raise (Error "unknown function reference")
	 in
	 let params = Llvm.params callee in
	 if Array.length params == Array.length args then () else
	   raise (Error "incorrect # arguments passed");
	 let args = Array.map (codegen_term ctx) args in
	 Llvm.build_call callee args "calltmp" ctx.llvm_builder

    let codegen_proto ctx = function
      | Ast.Prototype (name,args) ->
	 let doubles = Array.make (Array.length args) (Llvm.double_type ctx.llvm_context) in
	 let ft = Llvm.function_type (Llvm.double_type ctx.llvm_context) doubles in
	 let f =
	   match Llvm.lookup_function name ctx.llvm_module with
	   | None -> Llvm.declare_function name ft ctx.llvm_module
	   | Some f ->
	      if Llvm.block_begin f <> Llvm.At_end f then
		raise (Error "redefinition of function");
	      if Llvm.element_type (Llvm.type_of f) <> ft then
		raise (Error "redefinition of function with different # args");
	      f
	 in
	 Array.iteri (fun i a ->
		      let n = args.(i) in
		      Llvm.set_value_name n a;
		      Hashtbl.add ctx.named_values n a;
		     ) (Llvm.params f);
	 f
    let codegen_function ctx = function
      | Ast.Function (proto, body) ->
	 Hashtbl.clear ctx.named_values;
	 let the_function = codegen_proto ctx proto in
	 let bb = Llvm.append_block ctx.llvm_context "entry" the_function in
	 Llvm.position_at_end bb ctx.llvm_builder;
	 try
	   let ret_val = codegen_term ctx body in
	   let _ = Llvm.build_ret ret_val ctx.llvm_builder in
	   the_function
	 with e ->
	   Llvm.delete_function the_function;
	   raise e
  end



let () =
  let module Ast = Ast.L1 in
  let f = Ast.Function (Ast.Prototype ("square", [| "x" |]),
		   Ast.Term.Binary ("+", Ast.Type.Base Ast.Type.Double, Ast.Term.Variable "x", Ast.Term.Variable "x")) in
  let context = Llvm.global_context () in
  let the_module = Llvm.create_module context "my cool jit" in
  let builder = Llvm.builder context in
  let l = Codegen.codegen_function {
	      Codegen.llvm_context = context;
	      Codegen.llvm_module = the_module;
	      Codegen.llvm_builder = builder;
	      Codegen.named_values = Hashtbl.create 10} f in
  begin
    Llvm.dump_value l;
    Llvm.dump_module the_module
  end
