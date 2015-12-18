include Core.Std

module Source = Ast.L1

type codegen_state = {
    llcontext : Llvm.llcontext;
    llmodule  : Llvm.llmodule;
    llbuilder : Llvm.llbuilder;
    named_values : (string, Llvm.llvalue) Hashtbl.t;
  }

let (>>) f g x = g (f x)
let (|>) x f   = f x

type 'a func = { func : Llvm.llvalue; context : codegen_state }

let make_function context name returns args f =
    let fn_type = Llvm.function_type returns args in
    let fn = Llvm.declare_function name fn_type context.llmodule in
    let bb = Llvm.append_block context.llcontext "entry" fn in
    Llvm.position_at_end bb context.llbuilder;
    f {func = fn; context }

let rec find_type_representation : codegen_state -> Source.Type.t -> Llvm.lltype =
    fun ctx -> function
        | Source.Type.Base bt ->
           begin match bt with
                 | Source.Type.Double ->
                    (Llvm.double_type ctx.llcontext)
                 | Source.Type.Integer -> (Llvm.integer_type ctx.llcontext 32) end
        | Source.Type.Arrow (t,t') -> (Llvm.integer_type ctx.llcontext 32)

let codegen_struct context array =
    Llvm.struct_type context array

let codegen_literal = fun context -> function
    | Source.Term.Double d -> Result.Ok (Llvm.const_float (Llvm.double_type context.llcontext) d)
    | Source.Term.Integer i -> Result.Ok (Llvm.const_int (Llvm.integer_type context.llcontext 32) i)

let codegen_select context res = ()

type operator = Add | Mul | Sub
type base_type = Float | Int
type operand = {
    base_type : base_type;
    value : Llvm.llvalue;
  }

let codegen_binop context op o o' = let builder, tmp = match op with
  | Add -> if o.base_type = Float then Llvm.build_fadd, "faddtmp" else  Llvm.build_add, "addtmp"
  | Mul -> if o.base_type = Float then Llvm.build_fmul, "fmultmp" else  Llvm.build_fadd, "multmp"
  | Sub -> if o.base_type = Float then Llvm.build_fsub, "fmultmp" else  Llvm.build_fsub, "subtmp"
 in builder o.value o'.value tmp context.llbuilder

let rec codegen_term : codegen_state -> Ast.L1.Term.t -> (Llvm.llvalue, Codegen_error.t) Result.t =
  let open Result.Monad_infix in
  fun context ->
  function
  | Source.Term.Trace _ -> Result.Error (Codegen_error.Error ("Not implemented: Trace"))
  | Source.Term.Literal l -> codegen_literal context l
  | Source.Term.Variable name -> begin
      match Hashtbl.find context.named_values name with
      | Some r -> Result.Ok r
      | None -> Result.Error (Codegen_error.Error ("Unbound variable: " ^ name))
    end
  | Source.Term.Binary (op,t,x,y) ->
    codegen_term context x >>= fun x' ->
    codegen_term context y >>= fun y' ->
    let llvm_ty = find_type_representation context t in
    begin
      match op with
      | "+" -> Result.Ok (Llvm.build_fadd x' y' "addtmp" context.llbuilder)
      | "-" -> Result.Ok (Llvm.build_fsub x' y' "subtmp" context.llbuilder)
      | "*" -> Result.Ok (Llvm.build_fmul x' y' "multmp" context.llbuilder)
      | _ -> Result.Error (Codegen_error.Error "invalid binary operation")
    end
  | Source.Term.Call (callee, args) ->
    let callee =
      match Llvm.lookup_function callee context.llmodule with
      | Some callee -> Result.Ok callee
      | None -> Result.Error (Codegen_error.Error "unknown function reference")
    in
    callee >>= fun callee ->
    let params = Llvm.params callee in
    if phys_equal (Array.length params) (Array.length args) then
      Result.Error (Codegen_error.Error "incorrect # arguments passed")
    else
      Result.all (Array.to_list (Array.map args ~f:(codegen_term context))) >>| List.to_array >>= fun args ->
      Result.Ok (Llvm.build_call callee args "calltmp" context.llbuilder)


let codegen_proto : codegen_state -> Ast.L1.Term.proto -> (Llvm.llvalue, Codegen_error.t) Result.t =
  let open Result.Monad_infix in
  fun context -> function
  | Source.Term.Prototype (name,args) ->
      let doubles = Array.create ~len:(Array.length args) (Llvm.double_type context.llcontext) in
      let ft = Llvm.function_type (Llvm.double_type context.llcontext) doubles in
      (match Llvm.lookup_function name context.llmodule with
       | Some f when Llvm.block_begin f <> Llvm.At_end f -> Result.Error (Codegen_error.Error "redefinition of function")
       | Some f when Llvm.element_type (Llvm.type_of f) <> ft -> Result.Error (Codegen_error.Error  "redefinition of function with different # args")
       | None -> Result.Ok (Llvm.declare_function name ft context.llmodule)
       | Some f -> Result.Ok f)
      >>= fun f -> Array.iteri
                     ~f:(fun i a ->
                         let n = args.(i) in
                         Llvm.set_value_name n a;
                         match Hashtbl.add context.named_values n a with
                         | `Ok
                         | `Duplicate -> ()
                        )
                     (Llvm.params f);
                   Result.Ok f

let codegen_function context =
  let open Result.Monad_infix in function
  | Source.Term.Function (proto, body) ->
      Hashtbl.clear context.named_values;
      codegen_proto context proto >>= fun the_function ->
      let bb = Llvm.append_block context.llcontext "entry" the_function in
      Llvm.position_at_end bb context.llbuilder;
      codegen_term context body >>= fun ret_val ->
      let _ = Llvm.build_ret ret_val context.llbuilder in
      Llvm_analysis.assert_valid_function the_function;
      Result.return the_function




(* let compile_register context load store signal = *)
(*   let r = match signal with *)
(*     | Signal_Register (_, r) -> r *)
(*     | _ -> assert false *)
(*   in *)
(*   let name n = name n signal in *)
(*   let width = width signal in *)

(*   let clr, clr_level, clr_value = *)
(*     if (r.reg_clear) <> empty then *)
(*       load r.reg_clear, load r.reg_clear_level, load r.reg_clear_value *)
(*     else *)
(*       Llvm.const_int, const_int 1 1, const_int width 0 *)
(*   let clr = Llvm.build_icmp Llvm.Icmp.Eq clr clr_level *)
