open Plang

type varname = string
type env = (varname * Type.t) list

type error =
  | InternalError of string
  | OccursCheckFailed
  | CycleDetected

type type_inference_state = {
  mutable gensym_counter : int;
  mutable current_level  : int;
  mutable to_be_level_adjusted : Type.t list; }

let reset_gensym (s : type_inference_state) = s.gensym_counter <- 0
let reset_level (s : type_inference_state) = s.current_level <- 1
let reset_level_adjustment s = s.to_be_level_adjusted <- []
let reset_type_inference_state s = reset_level s; reset_gensym s
let enter_level s = s.current_level <- s.current_level + 1
let leave_level s = s.current_level <- s.current_level - 1
let gensym s =
  let n = s.gensym_counter in
  s.gensym_counter <- s.gensym_counter + 1;
  if n < 26 then
    String.make 1 (Char.chr (Char.code 'a' + n))
  else
    "t" ^ string_of_int n

let new_var s = Type.TVar (ref (Type.Unbound (gensym s, s.current_level)))
let new_arrow s t1 t2 = Type.TArrow (t1, t2, {Type.level_new = s.current_level; Type.level_old = s.current_level})

let rec repr : Type.t -> Type.t = function
  | Type.TVar ({contents = Type.Link t} as tvr) ->
    let t = repr t in
    tvr := Type.Link t; t
  | t -> t

let get_level : Type.t -> Type.level = function
  | Type.TVar {contents = Type.Unbound (_,l)} -> l
  | Type.TArrow (_,_,ls) -> ls.Type.level_new
  | _ -> assert false

let rec cycle_free : Type.t -> unit = function
  | Type.TVar {contents = Type.Unbound _} -> ()
  | Type.TVar {contents = Type.Link ty}   -> cycle_free ty
  | Type.TArrow (_,_,ls) when ls.Type.level_new = Type.marked_level -> failwith "occurs check"
  | Type.TArrow (t1,t2,ls) ->
    let level = ls.Type.level_new in
    ls.Type.level_new <- Type.marked_level;
    cycle_free t1;
    cycle_free t2;
    ls.Type.level_new <- level
  | _ -> assert false

let update_level : type_inference_state -> Type.level -> Type.t -> unit =
  fun s l -> function
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
        if l1 > l2
        then tv1 := Type.Link t2
        else tv2 := Type.Link t1
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
         unify_level s min_level tyl1 tyr1;
         unify_level s min_level tyl2 tyr2;
         ll.Type.level_new <- min_level;
         lr.Type.level_new <- min_level
      | _ -> assert false
and unify_level s l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level s l ty1;
  unify s ty1 ty2


let force_delayed_adjustments : type_inference_state -> unit = fun s ->
  let rec loop acc level ty =
    match repr ty with
    | Type.TVar ({contents = Type.Unbound (name,l)} as tvr) when l > level -> tvr := Type.Unbound (name,level); acc
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

let instantiate : type_inference_state -> Type.t -> Type.t =
  let rec loop s subst = function
    | Type.TVar {contents = Type.Unbound (name,l)} when l = Type.generic_level ->
      begin
        try (List.assoc name subst, subst)
        with Not_found ->
          let tv = new_var s in
          (tv, (name,tv)::subst)
      end
    | Type.TVar {contents = Type.Link ty} -> loop s subst ty
    | Type.TArrow (ty1,ty2,ls) when ls.Type.level_new = Type.generic_level ->
      let (ty1, subst) = loop s subst ty1 in
      let (ty2, subst) = loop s subst ty2 in
      (new_arrow s ty1 ty2, subst)
    | ty -> (ty,subst)
  in fun s ty -> fst (loop s [] ty)

let rec typeof : type_inference_state -> env -> Term.t -> Type.t =
  fun s env -> function
    | Term.Variable x -> instantiate s (List.assoc x env)
    | Term.Lambda (x,e) ->
      let ty_x = new_var s in
      let ty_e = typeof s ((x,ty_x)::env) e in
      new_arrow s ty_x ty_e
    | Term.Application (e1, e2) ->
      let ty_fun = typeof s env e1 in
      let ty_arg = typeof s env e2 in
      let ty_res = new_var s in
      unify s ty_fun (new_arrow s ty_arg ty_res);
      ty_res
    | Term.Comp ts ->
       failwith "Not implemented"
    | Term.Rel ts ->
       failwith "Not implemented"
    | Term.Atom ts ->
       failwith "Not implemented"
    | Term.Literal l ->
       failwith "Not implemented"
    | Term.Prod l ->
       let typs = List.map (typeof s env) l in
       Type.Prod typs
    | Term.Let (x,e,e2) ->
      enter_level s;
      let ty_e = typeof s env e in
      leave_level s;
      gen s ty_e;
      typeof s ((x, ty_e) :: env) e2
