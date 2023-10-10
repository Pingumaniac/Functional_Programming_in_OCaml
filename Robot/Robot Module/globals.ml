(* types *)
type vector = float * float * float
type basis = vector * vector * vector * vector
type pose = float * float * float * float * float
type mark = float
type board = mark list
type config = pose * board

(* global variables *)
(*vectors*)
let gv_o = (0.,0.,0.)
let gv_x = (1.,0.,0.)
let gv_y = (0.,1.,0.)
let gv_z = (0.,0.,1.)

(*basis*)
let gb_basis = (gv_o, gv_x, gv_y, gv_z)

(* helper functions *)
let pi = acos (- 1.) (*3.141592*)
let pi2 = pi /. 2.
let rad2deg a = a *. 180. /. pi
let deg2rad a = a /. 180. *. pi

let abs x = if x < 0. then -. x else x
let equ x y = abs (x -. y) < 1e-10
