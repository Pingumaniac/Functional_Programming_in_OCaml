(*-------------------------------------
  App Computer VS Computer
-------------------------------------*)
open Globals

module App = struct
    module Vect      = Vector.VectImpl
    module Basis     = Basis.BasisImpl (Vect)
    module Board     = Board.BoardImpl
    module Pose      = Pose.PoseImpl (Vect) (Basis) (Board)
    module Drawer    = Drawer.DrawerImpl (Vect) (Basis) (Board) (Pose)
    module Command   = Command.CommandImpl (Pose) (Drawer) (Board)
    module PlayerCom = Player_com.PlayerCom (Board)
    module PlayerKey = Player_key.PlayerKey (Board)

    module Game      = Game.GameImpl (Vect) (Basis) (Board) (Pose) (Drawer) (Command) (PlayerCom) (PlayerKey)

    let main () =
        let open Board in
        (*enable print stack trace*)
        let _ = Printexc.record_backtrace true in

        (*camera basis*)
        let b_camera = gb_basis |> Basis.rotz (-210.) |> Basis.rotx (-60.) in

        (*initial pose*)
        let ipose = (90., 30., 60., 0., mark_n) in

        (*initial board*)
        let iboard = [  mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_o (*9*); mark_x (*10*)] in

        Drawer.open_graph ();
        Game.game b_camera (ipose, iboard) |> Game.print_result;
end

let _ = App.main ()

