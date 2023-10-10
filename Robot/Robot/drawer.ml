let draw_box w d h clr =
    let open Graphics in
    let scr_coord (x, y, z) =
        let s = 600. in
        let p = (2. +. z) /. 3. in (*perspective*)
        (truncate (s *. x *. p) + 500,
         truncate (s *. y *. p) + 400) in
    let p0 = (-.w /.2., -.d /.2., 0.) in
    let p1 = (  w /.2., -.d /.2., 0.) in
    let p2 = (  w /.2.,   d /.2., 0.) in
    let p3 = (-.w /.2.,   d /.2., 0.) in
    let p4 = (-.w /.2., -.d /.2., h ) in
    let p5 = (  w /.2., -.d /.2., h ) in
    let p6 = (  w /.2.,   d /.2., h ) in
    let p7 = (-.w /.2.,   d /.2., h ) in

    let draw_axes basis =
        let s = 2. *. min w (min d h) in
        let (xo, yo) = scr_coord (v2g_basis (0., 0., 0.) basis) in
        let (xx, yx) = scr_coord (v2g_basis (s,  0., 0.) basis) in
        let (xy, yy) = scr_coord (v2g_basis (0., s,  0.) basis) in
        let (xz, yz) = scr_coord (v2g_basis (0., 0., s ) basis) in
        Graphics.set_color Graphics.red;
        moveto xo yo; lineto xx yx;
        Graphics.set_color Graphics.green;
        moveto xo yo; lineto xy yy;
        Graphics.set_color Graphics.blue;
        moveto xo yo; lineto xz yz in

    fun basis ->
        let (x0, y0) = scr_coord (v2g_basis p0 basis) in
        let (x1, y1) = scr_coord (v2g_basis p1 basis) in
        let (x2, y2) = scr_coord (v2g_basis p2 basis) in
        let (x3, y3) = scr_coord (v2g_basis p3 basis) in
        let (x4, y4) = scr_coord (v2g_basis p4 basis) in
        let (x5, y5) = scr_coord (v2g_basis p5 basis) in
        let (x6, y6) = scr_coord (v2g_basis p6 basis) in
        let (x7, y7) = scr_coord (v2g_basis p7 basis) in
        Graphics.set_color clr;
        moveto x3 y3; lineto x0 y0; lineto x1 y1; lineto x2 y2; lineto x3 y3;
        moveto x7 y7; lineto x4 y4; lineto x5 y5; lineto x6 y6; lineto x7 y7;
        moveto x0 y0; lineto x4 y4;
        moveto x1 y1; lineto x5 y5;
        moveto x2 y2; lineto x6 y6;
        moveto x3 y3; lineto x7 y7
        (*draw_axes basis*)

(*mark to color map*)
let mark_clr m =
    if      m = mark_n then Graphics.black
    else if m = mark_o then Graphics.red
    else if m = mark_x then Graphics.blue
    else assert false

(* draw board *)
let draw_board board =
    let draw_mark  = draw_box 0.05 0.05 0.05 in
    let draw_plate = draw_box 0.18 0.18 0.05 in
    let v_tmk      = (0.0, 0.0, 0.05) in (* height of plates *)
    fun basis ->
        let rec iter i =
            let b_mk    = b_translate (mark_pos i) gb_basis
                         |> b_translate v_tmk
                         |> b2g_basis basis in

            let b_pl    = b_translate (mark_pos i) gb_basis
                         |> b2g_basis basis in

            let m = get_mark board i in
            if m != mark_n then draw_mark (mark_clr m) b_mk;
            draw_plate Graphics.black b_pl;

            if i < 10 then iter (i+1) else () in
        iter 0

let draw_mark pose =
    let s = 0.9 *. 0.5 *. 0.1 in
    fun basis ->
        let m = get_pose pose "mark" in
        if m = mark_n
        then    ()
        else    draw_box (0.05/.s) (0.05/.s) (0.05/.s) (mark_clr m) basis

let draw_finger pose =
    let s = 0.9 *. 0.5 *. 0.2 in
    fun basis ->
        draw_box (0.02/.s) (0.02/.s) (0.1/.s) Graphics.black basis

let draw_arm2 pose =
    let s     = 0.9 *. 0.5 in
    let v_tf1 = (0.0, -0.08, 0.5/.s) in
    let v_tf2 = (0.0,  0.08, 0.5/.s) in
    let v_tmk = (0.0, 0.0, 0.5/.s +. 0.1) in
    fun basis ->
        let finger_angle = get_pose pose "finger" in
        let b_f1 = b_rot_x finger_angle gb_basis
                   |> b_scale 0.2
                   |> b_translate v_tf1
                   |> b2g_basis basis in

        let b_f2 = b_rot_x (-.finger_angle) gb_basis
                   |> b_scale 0.2
                   |> b_translate v_tf2
                   |> b2g_basis basis in

        let b_mk = b_scale 0.1 gb_basis
                   |> b_translate v_tmk
                   |> b2g_basis basis in

        draw_mark   pose b_mk;
        draw_finger pose b_f1;
        draw_finger pose b_f2;
        draw_box (0.1/.s) (0.1/.s) (0.5/.s) Graphics.black basis

let draw_arm1 pose =
    let s     = 0.9 in
    let v_ta2 = (0.0, 0.0, 0.56) in
    fun basis ->
        let arm2_angle = get_pose pose "arm2" in
        let b_a2 = b_rot_y arm2_angle gb_basis
                   |> b_scale 0.5
                   |> b_translate v_ta2
                   |> b2g_basis basis in

        draw_arm2 pose b_a2;
        draw_box (0.12/.s) (0.12/.s) (0.5/.s) Graphics.black basis

let draw_base pose =
    let v_ta1 = (0.0, 0.0, 0.1) in
    fun basis ->
        let arm1_angle = get_pose pose "arm1" in
        let b_a1 = b_rot_y arm1_angle gb_basis
                   |> b_scale 0.9
                   |> b_translate v_ta1
                   |> b2g_basis basis in

        draw_arm1 pose b_a1;
        draw_box 0.3 0.3 0.1 Graphics.black basis

let draw_robot pose =
    fun basis ->
        let base_angle = get_pose pose "base" in
        let b_bs = b_rot_z base_angle gb_basis
                   |> b2g_basis basis in

        draw_base pose b_bs

let draw_robot pose =
    fun basis ->
        let b_bs    = b_rotz (get_pose pose "base") gb_basis
                     |> b2g_basis basis in
        draw_base pose b_bs

(*draw robot and board*)
let draw b_camera pose board =
    Graphics.clear_graph ();
    (*draw_robot b_camera robot pose;*)
    draw_robot pose  b_camera;
    draw_board board b_camera;
    Graphics.synchronize ()
