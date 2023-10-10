open Globals

module type IBasis = sig
    (*scale*)
    val scale: float -> basis -> basis
    (*translate the origin*)
    val translate: vector -> basis -> basis
    (*basis rotations*)
    val rot: float -> (float -> vector ->vector) -> basis -> basis
    val rotx: float -> basis -> basis
    val roty: float -> basis -> basis
    val rotz: float -> basis -> basis
    (*vector w.r.t. basis to global basis*)
    val v2g_basis: vector -> basis -> vector
    (*basis (1st) w.r.t. basis (2nd) to global basis*)
    val b2g_basis: basis -> basis -> basis
end
