open Core.Std


(*
  consider operations

  (f ∘ g ∘ (h ⊗ i ⊗ j) ∘ k) ⊗ (a ∘ b ∘ (c ⊗ d ⊗ e) ∘ f)

  use distributive law to arange it as

  (f ⊗ a) ∘ (g ⊗ b) ∘ (h ⊗ i ⊗ j ⊗ c ⊗ d ⊗ e) ∘ (k ⊗ f)





 *)







type state = {
    reg_tag : tag Array.t;
    reg_int : Array.Int.t;
    reg_float : Array.Float.t;
  }

type op =
  | Add of int * int
  | Sub of int * int
  | Mul of int * int
  | Load of int * tag * int
  | Store of int * int
