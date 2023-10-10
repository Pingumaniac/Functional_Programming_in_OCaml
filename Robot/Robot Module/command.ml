open Globals
open Ipose
open Iboard
open Idrawer
open Icommand

module CommandImpl(Pose: IPose)(Drawer: IDrawer)(Board: IBoard) : ICommand = struct

    let moveto_pose b_camera (pose, board) target_pose =
        let db  = (Pose.get_pose target_pose "base")   -. (Pose.get_pose pose "base") in
        let da1 = (Pose.get_pose target_pose "arm1")   -. (Pose.get_pose pose "arm1") in
        let da2 = (Pose.get_pose target_pose "arm2")   -. (Pose.get_pose pose "arm2") in
        let df  = (Pose.get_pose target_pose "finger") -. (Pose.get_pose pose "finger") in

        let rec rot_joint pose joint ang step =
            if step <= 0 then pose else begin
            let pose = Pose.rotate_joint pose joint (ang /. (float_of_int step)) in
            Drawer.draw b_camera pose board;
            Drawer.delay 0.05;
            rot_joint pose joint ang (step - 1)
            end
        in

        let p = pose
            |> fun p -> rot_joint p "base" db  5
            |> fun p -> rot_joint p "arm1" da1 5
            |> fun p -> rot_joint p "arm2" da2 5
            |> fun p -> rot_joint p "finger" df 3
        in
        (p, board)

    let pick (pose, board) i =
        let f = Pose.get_pose pose "finger" in
        let m = Board.get_mark board i in
        let new_pose = Pose.set_pose pose "finger" 0. in
        let new_pose = Pose.set_pose new_pose "mark" m in
        let new_board = Board.set_mark board i Board.mark_n in
        (new_pose, new_board)

    let drop (pose, board) i =
        let f = Pose.get_pose pose "finger" in
        let m = Pose.get_pose pose "mark" in
        let j = if m = Board.mark_o then 9 else 10 in
        let new_pose = Pose.set_pose pose "finger" 10. in
        let new_pose = Pose.set_pose new_pose "mark" Board.mark_n in
        let new_board = Board.set_mark board i m in
        let new_board = Board.set_mark new_board j m in
        (new_pose, new_board)

    let mark b_camera (pose, board) mrk dst =
        let src = if mrk = Board.mark_o then 9 else 10 in
        let dst_pose = Pose.find_pose board dst in
        let src_pose = Pose.find_pose board src in
        let lift_pose = Pose.set_pose pose "finger" 10. in
        let mvp = moveto_pose b_camera in
        let pose, board = mvp (pose, board) src_pose in
        let pose, board = pick (pose, board) src in
        let pose, board = mvp (pose, board) lift_pose in
        let pose, board = mvp (pose, board) dst_pose in
        let pose, board = drop (pose, board) dst in
        let pose, board = mvp (pose, board) lift_pose in
        (pose, board)
    end
