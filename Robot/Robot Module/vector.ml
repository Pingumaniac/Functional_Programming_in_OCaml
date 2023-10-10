(*-------------------------------------
  Vector 3D
-------------------------------------*)
open Globals

module VectImpl: Ivector.IVect = struct

    (*vector addition*)
    let add (x1, y1, z1) (x2, y2, z2) =
        (x1 +. x2, y1 +. y2, z1 +. z2)

    (*vector subtraction*)
    let sub (x1, y1, z1) (x2, y2, z2) =
        (x1 -. x2, y1 -. y2, z1 -. z2)

    (*scalar multiplication*)
    let smul s (x, y, z) =
        (s *. x, s *. y, s *. z)

    (*inner product*)
    let prod (x1, y1, z1) (x2, y2, z2) =
        x1 *. x2 +. y1 *. y2 +. z1 *. z2

    (*length of a vector*)
    let len (x, y, z) =
        sqrt (x *. x +. y *. y +. z *. z)

    (*vector rotation in 2D*)
    let rot2d ang (x, y) =
        let r = deg2rad ang in
        (cos r *. x -. sin r *. y, sin r *. x +. cos r *. y)

    (*rotation around x-axis*)
    let rotx ang (x, y, z) =
        rot2d ang (y, z) |> fun (u, v) -> (x, u, v)

    (*rotation around y-axis*)
    let roty ang (x, y, z) =
        rot2d ang (z, x) |> fun (u, v) -> (v, y, u)

    (*rotation around z-axis*)
    let rotz ang (x, y, z) =
        rot2d ang (x, y) |> fun (u, v) -> (u, v, z)

end
