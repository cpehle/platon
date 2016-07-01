open Core.Std
type t = {
    id : int array;
    sz : int array;
  }

let make n =
  let id = Array.init n ~f:(fun i -> i) in
  let sz = Array.init n ~f:(fun i -> 1) in
  { id = id; sz = sz }

let root (state:t) (i:int) : int =
  let rec go state i =
    if (state.id.(i) = i)
    then i
    else begin
        state.id.(i) <- state.id.(state.id.(i));
        go state state.id.(i)
      end in
  go state i

let find state p q =
  let i = root state p in
  let j = root state q in
  i = j

let unite state p q =
  let i = root state p in
  let j = root state q in
  if state.sz.(i) < state.sz.(j) then begin
    state.id.(i) <- j;
    state.sz.(j) <- state.sz.(j) + state.sz.(i);
    end
  else begin
    state.id.(j) <- i;
    state.sz.(i) <- state.sz.(i) + state.sz.(j);
    end
