open Globals

module type IPlayer = sig
    (*mark on its turn*)
    val play: mark -> board -> int
end
