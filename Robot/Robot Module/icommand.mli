open Globals

module type ICommand = sig
    (*mark on the board*)
    val mark: basis -> config -> mark -> int -> config
end
