open Globals

module type IBoard = sig
    (*the marks are floats to match with other pose elements*)
    val mark_n: mark
    val mark_o: mark
    val mark_x: mark
    (*get the i-th mark of the board*)
    val get_mark: board -> int -> mark
    (*the board whose i-th mark is switched to m*)
    val chg_mark: board -> int -> mark -> board
    (*print the board for debugging*)
    val print_board: board -> unit
    (*winner of the board if any; otherwise mark_n*)
    val winner: board -> mark
    (*find empty location*)
    val empty_pos: board -> int
    (*game over or not*)
    val game_over: board -> bool
end
