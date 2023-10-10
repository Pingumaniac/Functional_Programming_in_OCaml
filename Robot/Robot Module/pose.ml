open Globals
open Ivector
open Ibasis
open Iboard
open Ipose

module PoseImpl (Vect: IVect) (Basis: Ibasis) (Board: IBoard) : IPose = struct

    (* The position of the mark at index i *)
    let mark_pos = function
        | 0 -> (0.2, 0.8, 0.)
        | 1 -> (0., 0.8, 0.)
        | 2 -> (-0.2, 0.8, 0.)
        | 3 -> (0.2, 0.6, 0.)
        | 4 -> (0., 0.6, 0.)
        | 5 -> (-0.2, 0.6, 0.)
        | 6 -> (0.2, 0.4, 0.)
        | 7 -> (0., 0.4, 0.)
        | 8 -> (-0.2, 0.4, 0.)
        | 9 -> (0.5, 0.4, 0.)
        | 10-> (0.5, 0.6, 0.)
        | _ -> assert false

    let get_pose (b, a1, a2, f, m) joint =
        match joint with
        | "base" -> b
        | "arm1" -> a1
        | "arm2" -> a2
        | "finger" -> f
        | "mark" -> m
        | _ -> assert false

    let chg_pose (b, a1, a2, f, m) joint delta =
        match joint with
        | "base" -> (b +. delta, a1, a2, f, m)
        | "arm1" -> (b, a1 +. delta, a2, f, m)
        | "arm2" -> (b, a1, a2 +. delta, f, m)
        | "finger" -> (b, a1, a2, f +. delta, m)
        | "mark" -> (b, a1, a2, f, delta)
        | _ -> assert false

    let find_pose (x, y, z) f m =
        (* Calculation logic would go here *)
        (* For demonstration, we'll assume these values *)
        let b = 45.000 in
        let a1 = -27.800 in
        let a2 = 161.805 in
        (b, a1, a2, f, m)

    let lift_pose pose =
        let b = get_pose pose "base" in
        let f = get_pose pose "finger" in
        let m = get_pose pose "mark" in
        (b, 10., 70., f, m)

end
