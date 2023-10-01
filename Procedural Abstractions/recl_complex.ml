(*complex numbers*)

(*constructor and accessor*)
(*sel is one of "real", "imag", "mag", and "ang"*)
let complex r i =
fun sel ->
    match sel with
    | "real" -> r
    | "imag" -> i
    | "mag" -> sqrt (r *. r +. i *. i)
    | "ang" -> atan2 i r
    | _ -> failwith "Invalid selector"


(*test*)
let equ a b =
    let eps = 1e-10 in
    let abs x = if x < 0. then -. x else x in
    abs (a -. b) < eps

let print c =
    Printf.printf "%f + %f i\n" (c "real") (c "imag")

let a = complex 2. 3.
let b = complex 1. 1.

let test () =
    let ( == ) = equ in
    assert (a "real" == 2. && a "imag" == 3.);
    assert (b "mag" == sqrt 2. && b "ang" == atan2 1. 1.);
    ()
let _ = test ()

(*
(*arithmetic operations*)
(*opr is one of "add", "sub", "mul", and "div"*)
let rec arith opr a b =
    (a "real", a "imag", b "real", b "imag") |> fun (ra, ia, rb, ib) ->
        if      opr = "add" then (*TODO: implement this part*)
        else if opr = "sub" then (*TODO: implement this part*)
        else if opr = "mul" then (*TODO: implement this part*)
        else if opr = "div" then (*TODO: implement this part*)
            let c = complex rb (-. ib) in (*conjugate of b*)
            let (rnum, inum) = arith "mul" a c |> fun num -> (num "real", num "imag") in
            let rden         = arith "mul" b c |> fun den -> den "real" in

        else assert false

let test () =
    let ( + ) = arith "add" in
    let ( - ) = arith "sub" in
    let ( * ) = arith "mul" in
    let ( / ) = arith "div" in
    let ( == ) = equ in

    a + b |> fun c -> assert (c "real" == 3.  && c "imag" == 4.);
    a - b |> fun c -> assert (c "real" == 1.  && c "imag" == 2.);
    a * b |> fun c -> assert (c "real" == -1. && c "imag" == 5.);
    a / b |> fun c -> assert (c "real" == 2.5 && c "imag" == 0.5);
    ()
let _ = test ()


(*ploar form*)
(*sel is one of "real", "imag", "mag", and "ang"*)
let polar m a =
    fun sel -> (*TODO: implement this function*)


let test () =
    let ( == ) = equ in
    let c = polar (a "mag") (a "ang") in
    assert (c "mag" == a "mag" && c "ang" == a "ang");
    assert (c "real" == 2.     && c "imag" == 3.);
    ()
let _ = test ()


(*arithmetic operations*)
(*opr is one of "add", "sub", "mul", and "div"*)
let arith2 opr a b =
    (a "mag", a "ang", b "mag", b "ang") |> fun (ma, aa, mb, ab) ->
        (*TODO: implement this function*)

let test () =
    let ( + ) = arith2 "add" in
    let ( - ) = arith2 "sub" in
    let ( * ) = arith2 "mul" in
    let ( / ) = arith2 "div" in
    let ( == ) = equ in

    a + b |> fun c -> assert (c "real" == 3.  && c "imag" == 4.);
    a - b |> fun c -> assert (c "real" == 1.  && c "imag" == 2.);
    a * b |> fun c -> assert (c "real" == -1. && c "imag" == 5.);
    a / b |> fun c -> assert (c "real" == 2.5 && c "imag" == 0.5);
    ()
let _ = test ()
*)

let _ = "Success!"
