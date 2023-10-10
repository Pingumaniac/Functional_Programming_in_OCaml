open Globals

module TestPose = struct
    module Vect  = Vector.VectImpl
    module Basis = Basis.BasisImpl (Vect)
    module Board = Board.BoardImpl
    module Pose  = Pose.PoseImpl (Vect) (Basis) (Board)

    (*unit equ_pose*)
    let equ_pose () =
        let open Board in
        let pose = (90., 30., 60., 0., mark_n) in
        let equ_pose pose (b, a1, a2, f, m) =
            assert (equ b  (Pose.get_pose pose "base"));
            assert (equ a1 (Pose.get_pose pose "arm1"));
            assert (equ a2 (Pose.get_pose pose "arm2"));
            assert (equ f  (Pose.get_pose pose "finger"));
            assert (equ m  (Pose.get_pose pose "mark")) in

        Printf.printf("----------------------------------------\n");
        Printf.printf("equ_pose robot pose...\n");
        equ_pose pose (90., 30., 60., 0., mark_n);

        equ_pose (Pose.chg_pose pose "base" 5.)     (95., 30., 60., 0., mark_n);
        equ_pose (Pose.chg_pose pose "arm1" 5.)     (90., 35., 60., 0., mark_n);
        equ_pose (Pose.chg_pose pose "arm2" 5.)     (90., 30., 65., 0., mark_n);
        equ_pose (Pose.chg_pose pose "finger" 5.)   (90., 30., 60., 5., mark_n);
        equ_pose (Pose.chg_pose pose "mark" mark_o) (90., 30., 60., 0., mark_o);

        let p = (Pose.find_pose (Pose.mark_pos 0)) 3. Board.mark_o in
        let a = (75.963756532073532, 49.548507296007486, 76.595860623084960, 3., mark_o) in
        equ_pose p a;
        Printf.printf("equ_pose robot pose\n")
end

let _ = TestPose.equ_pose ()
