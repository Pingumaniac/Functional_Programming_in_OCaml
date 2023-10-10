(*-------------------------------------
  Unit Test for Game
-------------------------------------*)
open Globals

module TestGame = struct
    module Board = Board.BoardImpl
    module MockDrawer = Drawer.DrawerImpl (Vect) (Basis) (Board) (Pose)

    module MockCommand: Icommand.ICommand = struct
        let mark basis (pose, board) mrk i =
            Board.chg_mark board i mrk |> fun b ->
                Board.print_board b;
                (pose, b)
    end

    module Player = Player_com.PlayerCom (Board)
    module Game   = Game.GameImpl (Board) (MockDrawer) (MockCommand) (Player)

    (*unit test*)
    let test () =
        let open Board in
        let board = [   mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_o (*9*); mark_x (*10*)] in
        let ipose = (0., 0., 0., 0., mark_n) in
        Printf.printf("----------------------------------------\n");
        Printf.printf("test game...\n");
        (ipose, board)
            |> Game.game gb_basis
            |> fun w -> assert(w = Board.mark_o);
        Printf.printf("test game done\n")
end

let _ = TestGame.test ()
