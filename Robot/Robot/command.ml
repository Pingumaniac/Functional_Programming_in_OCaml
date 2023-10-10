let rotate_joint pose joint step_angle =
    chg_pose pose joint step_angle

let chg_pose (b, a1, a2, f, m) joint delta =
    match joint with
    | "base" -> (b +. delta, a1, a2, f, m)
    | "arm1" -> (b, a1 +. delta, a2, f, m)
    | "arm2" -> (b, a1, a2 +. delta, f, m)
    | "finger" -> (b, a1, a2, f +. delta, m)
    | "mark" -> (b, a1, a2, f, delta) (* Assuming marks are also represented as floats *)
    | _ -> failwith "Invalid joint name"

(*move from pose to target_pose*)
let moveto_pose b_camera (pose, board) target_pose =
    let db  = (get_pose target_pose "base")   -. (get_pose pose "base") in
    let da1 = (get_pose target_pose "arm1")   -. (get_pose pose "arm1") in
    let da2 = (get_pose target_pose "arm2")   -. (get_pose pose "arm2") in
    let df  = (get_pose target_pose "finger") -. (get_pose pose "finger") in

    (*move the joint <ang> angle in <step> steps
      e.g. rotate arm1 30 deg in 5 steps
           => rotate arm1 5 times 6 deg each
    *)
    let rot_joint pose joint ang step =
        let step_angle = ang /. float_of_int step in
        let rec aux n curr_pose =
            if n = 0 then curr_pose
            else
                let new_pose = rotate_joint curr_pose joint step_angle in
                Thread.delay 0.05;
                aux (n-1) new_pose
        in
        aux step pose
    in
    (*move the joints in base, arm1, arm2, and finger order*)
    let p   = pose
            |> fun p -> rot_joint p "base" db  5
            |> fun p -> rot_joint p "arm1" da1 5
            |> fun p -> rot_joint p "arm2" da2 5
            |> fun p -> rot_joint p "finger" df 3 in
    (p, board)


let pick (pose, board) i =
    let m = get_mark board i in
    let new_pose = chg_pose pose "finger" 0. in
    let new_pose = chg_pose new_pose "mark" m in
    let new_board = chg_mark board i mark_n in
    (new_pose, new_board)

let drop (pose, board) i =
    let m = get_pose pose "mark" in
    let j = if m = mark_o then 9 else 10 in
    let new_pose = chg_pose pose "finger" 10. in
    let new_pose = chg_pose new_pose "mark" mark_n in
    let new_board = chg_mark (chg_mark board i m) j m in
    (new_pose, new_board)


let mark b_camera (pose, board) mrk dst =
    let src = if mrk = mark_o then 9 else 10 in
    let f = get_pose pose "finger" in
    let m = get_pose pose "mark" in
    let (b_dst, a1_dst, a2_dst) = find_pose mark_pos dst in
    let (b_src, a1_src, a2_src) = find_pose mark_pos src in
    let dst_pose = (b_dst, a1_dst, a2_dst, f, mrk) in
    let src_pose = (b_src, a1_src, a2_src, 0., m) in
    let mvp = moveto_pose b_camera in

    let (p1, b1) = mvp (pose, board) src_pose in
    let (p2, b2) = pick (p1, b1) src in
    let (p3, b3) = mvp (p2, b2) (lift_pose p2) in
    let (p4, b4) = mvp (p3, b3) dst_pose in
    let (p5, b5) = drop (p4, b4) dst in
    let (p6, b6) = mvp (p5, b5) (lift_pose p5) in
    (p6, b6)
