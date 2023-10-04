(* Define pi *)
let pi = 3.14159265358979323846

(*the position of the mark at index i*)
let mark_pos i =
    match i with
    | 0 -> ( 0.2, 0.8, 0.)
    | 1 -> (  0., 0.8, 0.)
    | 2 -> (-0.2, 0.8, 0.)
    | 3 -> ( 0.2, 0.6, 0.)
    | 4 -> (  0., 0.6, 0.)
    | 5 -> (-0.2, 0.6, 0.)
    | 6 -> ( 0.2, 0.4, 0.)
    | 7 -> (  0., 0.4, 0.)
    | 8 -> (-0.2, 0.4, 0.)
    | 9 -> ( 0.5, 0.4, 0.)
    | 10-> ( 0.5, 0.6, 0.)
    | _ -> assert false

(*the specified angle of the pose*)
let get_pose (b, a1, a2, f, m) = function
    | "base"   -> b
    | "arm1"   -> a1
    | "arm2"   -> a2
    | "finger" -> f
    | "mark"   -> m
    | _        -> assert false


(*the pose whose joint is changed by delta*)
let chg_pose (b, a1, a2, f, m) joint delta =
    match joint with
    | "base"   -> (b +. delta, a1, a2, f, m)
    | "arm1"   -> (b, a1 +. delta, a2, f, m)
    | "arm2"   -> (b, a1, a2 +. delta, f, m)
    | "finger" -> (b, a1, a2, f +. delta, m)
    | "mark"   -> (b, a1, a2, f, delta)
    | _        -> assert false

(*find the angle joints to get to x y z*)
let find_pose (x, y, z) =
    fun f m ->
        let d1 = 0.5 in  (*length of arm1*)
        let d2 = 0.55 in (*length of arm2 + length of finger*)
        let b  = atan2 y x *. 180. /. pi in
        let d  = sqrt (x *. x +. y *. y) in
        let a1 = acos (z /. (d1 +. d2)) *. 180. /. pi in
        let a2 = acos ((d1 *. d1 +. d2 *. d2 -. d *. d) /. (2. *. d1 *. d2)) *. 180. /. pi in
        (b, a1, a2, f, m)


(*the pose between movements*)
let lift_pose pose =
    let b = get_pose pose "base"   in
    let f = get_pose pose "finger" in
    let m = get_pose pose "mark"   in
    (b, 10., 70., f, m)

(*unit test*)
let test_robot_pose () =
    let pose = (90., 30., 60., 0., mark_n) in
    let equ_pose pose (b, a1, a2, f, m) =
        assert (equ b  (get_pose pose "base"));
        assert (equ a1 (get_pose pose "arm1"));
        assert (equ a2 (get_pose pose "arm2"));
        assert (equ f  (get_pose pose "finger"));
        assert (equ m  (get_pose pose "mark")) in

    Printf.printf("----------------------------------------\n");
    Printf.printf("test robot pose...\n");
    equ_pose pose (90., 30., 60., 0., mark_n);

    equ_pose (chg_pose pose "base" 5.)     (95., 30., 60., 0., mark_n);
    equ_pose (chg_pose pose "arm1" 5.)     (90., 35., 60., 0., mark_n);
    equ_pose (chg_pose pose "arm2" 5.)     (90., 30., 65., 0., mark_n);
    equ_pose (chg_pose pose "finger" 5.)   (90., 30., 60., 5., mark_n);
    equ_pose (chg_pose pose "mark" mark_o) (90., 30., 60., 0., mark_o);

    let p = (find_pose (mark_pos 0)) 3. mark_o in
    let a = (75.963756532073532, 49.548507296007486, 76.595860623084960, 3., mark_o) in
    equ_pose p a;
    Printf.printf("test robot pose\n")

let _ = test_robot_pose ()
