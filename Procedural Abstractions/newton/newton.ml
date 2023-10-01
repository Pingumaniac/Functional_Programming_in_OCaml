#use "complex_arith.ml"

(*TODO: implement newton's method*)
let newton f x0 =
    let ( + ) = complex_arith "add" in
    let ( - ) = complex_arith "sub" in
    let ( / ) = complex_arith "div" in
    let eps   = 1e-8 in  (*epsilon: a small number*)
    let delta = complex eps eps in

    (*difference*)
    let diff a b =
        (a - b) "mag" in

    (*the derivative of f: (f(x + delta) - f(x)) / delta*)
    let derivative f x =
        let x_plus_delta = x + delta in
        let fx_plus_delta = f x_plus_delta in
        let fx = f x in
        (fx_plus_delta - fx) / delta
    in

    (*return a function that finds the next guess from the current guess*)
    let next f x =
        let dfdx = derivative f x in (*f'(x)*)
        x - (f x / dfdx)
    in

    (*fixed point of f is x such that x = f(x) *)
    (*TODO: recursively apply f(x) to f until the difference between x and f(x) is less than eps*)
    let rec fixed_point f x =
        let next_x = next f x in
        if diff next_x x < eps then
            x
        else
            fixed_point f next_x
    in

    (*return the solution*)
    (*TODO: find a fixed point of next f starting from x0*)
    fixed_point f x0

let complex_sqrt x =
    let ini = complex 1. 1. in
    let ( - ) = complex_arith "sub" in
    let ( * ) = complex_arith "mul" in
    newton (fun y -> y * y - x) ini

(*test*)
let equ a b =
    let eps = 1e-8 in
    let abs x = if x < 0. then -. x else x in
    abs (a -. b) < eps

let test_sqrt () =
    Printf.printf "testing newton (sqrt -2)...\n";
    let a = complex (-2.) 0. in
    let b = polar 2. 2. in
    let ( == ) = equ in

    let sa = complex_sqrt a in
    let sb = complex_sqrt b in
    Printf.printf "sa: %f + i %f\n" (sa "real") (sa "imag");
    Printf.printf "sb: %f \\_ %f\n" (sb "mag") (sb "ang");
    assert(sa "real" == 0. && sa "imag" == sqrt 2.);
    assert(sb "mag" == sqrt 2. && sb "ang" == 1.);
    Printf.printf "success.\n"

let test_poly () =
    Printf.printf "testing newton (solve x^2 + 1)...\n";
    let ( == ) = equ in
    let solve_poly () =
        let one = complex 1. 0. in
        let ini = polar 1. 0. in
        let ( + ) = complex_arith "add" in
        let ( * ) = complex_arith "mul" in
        newton (fun x -> x * x + one) ini
    in

    let ans = solve_poly () in
    Printf.printf "ans: %f + i %f\n" (ans "real") (ans "imag");
    assert(ans "real" == 0. && ans "imag" == 1.);
    Printf.printf "success.\n"

(*
let _ = test_sqrt ()
let _ = test_poly ()
*)
