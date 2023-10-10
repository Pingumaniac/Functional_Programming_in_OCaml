open Globals

module type IDrawer = sig
    (*open a graphics window*)
    val open_graph: unit -> unit
    (*close the graphics window*)
    val close_graph: unit -> unit
    (*delay*)
    val delay: float -> unit
    (*draw robot and board*)
    val draw: basis -> pose -> board -> unit
end
