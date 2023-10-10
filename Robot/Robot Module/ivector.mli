open Globals

module type IVect = sig
  type vector = Globals.vector
  val add : vector -> vector -> vector
  val sub : vector -> vector -> vector
  val smul : float -> vector -> vector
  val prod : vector -> vector -> float
  val len : vector -> float
  val rotx : float -> vector -> vector
  val roty : float -> vector -> vector
  val rotz : float -> vector -> vector
end

module Vect : IVect = struct
  type vector = Globals.vector

  let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)

  let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)

  let smul s (x, y, z) = (s *. x, s *. y, s *. z)

  let prod (x1, y1, z1) (x2, y2, z2) = x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let len (x, y, z) = sqrt (x *. x +. y *. y +. z *. z)

  let rotx theta (x, y, z) =
    let y' = cos theta *. y -. sin theta *. z in
    let z' = sin theta *. y +. cos theta *. z in
    (x, y', z')

  let roty theta (x, y, z) =
    let x' = cos theta *. x +. sin theta *. z in
    let z' = -. sin theta *. x +. cos theta *. z in
    (x', y, z')

  let rotz theta (x, y, z) =
    let x' = cos theta *. x -. sin theta *. y in
    let y' = sin theta *. x +. cos theta *. y in
    (x', y', z)
end
