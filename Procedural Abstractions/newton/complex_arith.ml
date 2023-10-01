#use "complex.ml"

let rec complex_arith opr a b =
    let add a b =
        let real_part = a "real" +. b "real" in
        let imag_part = a "imag" +. b "imag" in
        complex real_part imag_part
    in

    let sub a b =
        let real_part = a "real" -. b "real" in
        let imag_part = a "imag" -. b "imag" in
        complex real_part imag_part
    in

    let mul a b =
        let real_part_a, imag_part_a = a "real", a "imag" in
        let real_part_b, imag_part_b = b "real", b "imag" in
        let real_part = (real_part_a *. real_part_b) -. (imag_part_a *. imag_part_b) in
        let imag_part = (real_part_a *. imag_part_b) +. (imag_part_a *. real_part_b) in
        complex real_part imag_part
    in

    let div a b =
        let real_part_a, imag_part_a = a "real", a "imag" in
        let real_part_b, imag_part_b = b "real", b "imag" in
        let denom = (real_part_b ** 2.) +. (imag_part_b ** 2.) in
        let real_part = ((real_part_a *. real_part_b) +. (imag_part_a *. imag_part_b)) /. denom in
        let imag_part = ((imag_part_a *. real_part_b) -. (real_part_a *. imag_part_b)) /. denom in
        complex real_part imag_part
    in

    match opr with
        | "add" -> add a b
        | "sub" -> sub a b
        | "mul" -> mul a b
        | "div" -> div a b
        | _ -> failwith "Invalid operation"

(* Test *)
let equ a b =
    let eps = 1e-10 in
    let abs x = if x < 0. then -.x else x in
    abs (a -. b) < eps

let test_arith a b =
    Printf.printf "testing arith...\n";
    let ( + ) = complex_arith "add" in
    let ( - ) = complex_arith "sub" in
    let ( * ) = complex_arith "mul" in
    let ( / ) = complex_arith "div" in
    let ( == ) = equ in

    a + b |> fun c -> assert (c "real" == 3. && c "imag" == 4.);
    a - b |> fun c -> assert (c "real" == 1. && c "imag" == 2.);
    a * b |> fun c -> assert (c "real" == -1. && c "imag" == 5.);
    a / b |> fun c -> assert (c "real" == 2.5 && c "imag" == 0.5);
    Printf.printf "success.\n"

let test_arith_complex () =
    let a = complex 2. 3. in
    let b = complex 1. 1. in
    test_arith a b

let test_arith_polar () =
    let a = complex 2. 3. in
    let b = complex 1. 1. in
    let c = polar (a "mag") (a "ang") in
    let d = polar (b "mag") (b "ang") in
    test_arith c d

(*
let _ = test_arith_complex ()
let _ = test_arith_polar ()
*)
