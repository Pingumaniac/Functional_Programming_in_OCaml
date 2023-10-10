open Globals

module type IPose = sig
    val mark_pos: int -> vector
    val get_pose: pose -> string -> float
    val chg_pose: pose -> string -> float -> pose
    val find_pose: vector -> float -> mark -> pose
    val lift_pose: pose -> pose
end
