open Globals
open Iboard
open Idrawer
open Icommand
open Igame
open Iplayer

module GameImpl (Board : IBoard) (Drawer : IDrawer) (Command : ICommand) (PlayerO : Iplayer) (PlayerX : Iplayer) : IGame = struct

    (* Helper function: Mark a position on the board and return updated game state *)
    let mark b_camera (pose, board) mark_func i =
        let new_board = mark_func i board in
        Drawer.draw b_camera pose new_board;
        (pose, new_board)

    (* Run the game *)
    let rec game b_camera (pose, board) =
        let open Command in
        Drawer.delay 0.05; (* to make drawer get correct window sizes *)
        Drawer.draw b_camera pose board;
        let i = PlayerO.play Board.mark_o board in
        let (pose, board) =
            if i = 9 then (pose, Board.mark_n board)
            else mark b_camera (pose, board) Board.mark_o i in
        if Board.game_over board then Board.winner board
        else
            let i = PlayerX.play Board.mark_x board in
            let (pose, board) =
            if i = 9 then (pose, Board.mark_n board)
            else mark b_camera (pose, board) Board.mark_x i in
            if Board.game_over board then Board.winner board
            else
            game b_camera (pose, board)

    (* Print the game result *)
    let print_result m =
        if      m = Board.mark_o then Printf.printf "O win! :)\n"
        else if m = Board.mark_x then Printf.printf "X win! :)\n"
        else                          Printf.printf "No winner\n"
    end
