open OUnit2
open Plang.Term
open Plang.Type

let top_type_check : Plang.Term.t -> Plang.Type.t = fun exp ->
  let initial_state = { TypeInference.gensym_counter = 0;
                        TypeInference.current_level = 0;
                        TypeInference.to_be_level_adjusted = []} in
  TypeInference.reset_type_inference_state initial_state;
  TypeInference.reset_level_adjustment initial_state;
  let ty = TypeInference.typeof initial_state [] exp in
  TypeInference.cycle_free ty;
  ty

let test1 text_ctxt = assert_equal
    (TArrow
       (TVar
          {contents =
             Link
               (TArrow
                  (TVar
                     {contents =
                        Link
                          (TArrow (TVar {contents = Unbound ("d", 1)},
                                   TVar {contents = Unbound ("e", 1)},
                                   {level_old = 1; level_new = 1}))},
                   TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
        TArrow
          (TVar
             {contents =
                Link
                  (TArrow (TVar {contents = Unbound ("d", 1)},
                           TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}))},
           TArrow (TVar {contents = Unbound ("d", 1)},
                   TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}),
           {level_old = 1; level_new = 1}),
        {level_old = 1; level_new = 1}))
    (  top_type_check (fn "x" (fn "y" (let_ "x" (app (var "x") (var "y"))
                                            (fn "x" (app (var "y") (var "x")))))))


let test2 text_ctxt = assert_equal
    (TArrow (TVar {contents = Unbound ("a", 1)},
             TVar {contents = Unbound ("a", 1)}, {level_old = 1; level_new = 1}))
    (top_type_check (fn "x" (let_ "y" (var "x") (var "y"))))

let suite =
  "test_inference">:::
  ["test1">:: test1;
   "test2">:: test2]
