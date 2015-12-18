open Core.Std
open Gg
open Vg

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100.
let view = Box2.v P2.o (Size2.v aspect 1.)
let triangle =
  let triangle_path = P.empty >> P.sub (P2.v 0.0 0.0) >> P.line ~rel:true (P2.v 0.5 0.5) >> P.line ~rel:true (P2.v 0.5 (-0.5)) >> P.line ~rel:true (P2.v 0.5 (-0.5)) in
  I.const (Color.gray 0.5) >> I.cut triangle_path
let fork =
  let fork_path a = P.empty >> P.sub (P2.v 0.5 0.0) >> P.line ~rel:true (P2.v 0.0 0.5)
                  >> P.sub (P2.v 0.5 0.5) >> P.line ~rel:true (P2.v (0.0 -. a) 0.5)
                  >> P.sub (P2.v 0.5 0.5) >> P.line ~rel:true (P2.v a 0.5 )in
  let area = `O { P.o with P.width = 0.01; P.join = `Round} in
  I.const Color.black >> I.cut ~area (fork_path 0.2)
let braid =
  let d0 = P.empty >> P.sub (P2.v 0.0 0.0) >> P.line ~rel:true (P2.v 0.5 1.0) >>  P.line ~rel:true (P2.v 0.0 1.0) in
  let d1 = P.empty >> P.sub (P2.v 0.5 0.0) >> P.line ~rel:true (P2.v (-0.5) 1.0) >>  P.line ~rel:true (P2.v 0.0 1.0) in
  let area1 = `O { P.o with P.width = 0.04; P.join = `Round} in
  let area2 = `O { P.o with P.width = 0.06; P.join = `Round } in
  ((I.const Color.white  >> I.cut ~area:area2 d0) >> I.blend (I.const Color.green  >> I.cut ~area:area1 d0)) >> I.blend ((I.const Color.white  >> I.cut ~area:area2 d1) >> I.blend (I.const Color.red  >> I.cut ~area:area1 d1)) >> I.scale (P2.v 0.5 0.5) >> I.move (P2.v 0.25 0.0)
let image =
  let beside l = List.fold_left l ~init:I.void ~f:(fun acc i -> acc >> I.move (P2.v 0.2 0.0) >> I.blend i) in
  let a = Float.pi_div_2 in
  let da = Float.two_pi /. 3. in
  let dotp = P.empty >> P.circle P2.o 0.08 in
  let font = { Font.name = "Fira"; size = 0.7; weight = `W800; slant = `Normal} in
  let glyphs = [ 53; 72; 89; 82; 79; 87; 4 ] in
  let text = "Revolt!" in
  let dot c da = I.const c >> I.cut dotp >> I.move (V2.polar 0.05 (a +. da)) in
  let colors = [Color.v_srgb 0.608 0.067 0.118 ~a:0.75; Color.v_srgb 0.608 0.067 0.118 ~a:0.75; Color.v_srgb 0.608 0.067 0.118 ~a:0.75] in
  (List.fold_left colors ~init:(I.const Color.white) ~f:(fun acc c -> acc >> I.move (P2.v 0.2 0.0) >> I.blend (dot c da))) >> I.move (P2.v 1.0 1.0) >> I.blend triangle >> I.blend braid >> I.blend fork
  >> I.blend ( I.const Color.black >> I.cut_glyphs ~text font glyphs >> I.move (V2.v 0.23 0.25))


let () =
         (* This is a test of graphics output. The eventual goal is to develop
      interactively in the interpreter and get a live view of the
      computational graph in an output window *)
      Out_channel.with_file "test.png" ~f:(fun oc ->
      let res = 300. /. 0.0254 (* 300dpi in dots per meters *) in
      let fmt = `Png (Size2.v res res) in
      let warn w = Vgr.pp_warning Format.err_formatter w in
      let r = Vgr.create ~warn (Vgr_cairo.stored_target fmt) (`Channel oc) in
      ignore (Vgr.render r (`Image (size, view, image)));
      ignore (Vgr.render r `End););
