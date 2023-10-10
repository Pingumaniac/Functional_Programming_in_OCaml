open Globals

module type IGame = sig
    (*run the game*)
    val game: basis -> config -> mark
    (*print the game result*)
    val print_result: mark -> unit
end

