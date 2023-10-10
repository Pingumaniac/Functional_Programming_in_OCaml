open Globals

module TestCommand = struct
    module Vect  = Vector.VectImpl
    module Basis = Basis.BasisImpl (Vect)
    module Board = Board.BoardImpl
    module Pose  = Pose.PoseImpl (Vect) (Basis) (Board)
    module MockDrawer = Drawer.DrawerImpl (Vect) (Basis) (Board) (Pose)
    module Command = Command.CommandImpl (Pose) (Drawer) (Board)

    (*unit test*)
    let test () =
        let open Board in
        let board = [   mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_o (*9*); mark_x (*10*)] in
        let ipose = (0., 0., 0., 0., mark_n) in
        Printf.printf("----------------------------------------\n");
        Printf.printf("test command...\n");
        (ipose, board)  |> fun (p, b) ->  Command.mark gb_basis (p, b) mark_x 0
                        |> fun (p, b) ->  Command.mark gb_basis (p, b) mark_o 2
                        |> fun (p, b) ->  Command.mark gb_basis (p, b) mark_x 1
                        |> fun (p, b) ->
                            assert(mark_x = get_mark b 0);
                            assert(mark_o = get_mark b 2);
                            assert(mark_x = get_mark b 1);
                            assert(mark_n = get_mark b 3);
                            assert(equ 90.    (Pose.get_pose p "base"));
                            assert(equ 10.    (Pose.get_pose p "arm1"));
                            assert(equ 70.    (Pose.get_pose p "arm2"));
                            assert(equ 10.    (Pose.get_pose p "finger"));
                            assert(equ mark_n (Pose.get_pose p "mark"));
        Printf.printf("test command done\n")
end
let _ = TestCommand.test ()
