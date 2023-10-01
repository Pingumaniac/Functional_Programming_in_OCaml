(* Complex number in rectangular form *)
let complex r i =
    fun sel ->
        match sel with
        | "real" -> r
        | "imag" -> i
        | "mag" -> sqrt (r *. r +. i *. i)
        | "ang" -> atan2 i r
        | _ -> failwith "Invalid selector"

(* Complex number in polar form *)
let polar m a =
    fun sel ->
        match sel with
        | "real" -> m *. cos a
        | "imag" -> m *. sin a
        | "mag" -> m
        | "ang" -> a
        | _ -> failwith "Invalid selector"

(* Test function *)
let equ a b =
    let eps = 1e-10 in
    let abs x = if x < 0. then -.x else x in
    abs (a -. b) < eps

let test_complex () =
    Printf.printf "testing complex...\n";
    let ( == ) = equ in
    let a = complex 2. 3. in
    let b = complex 1. 1. in
    assert (a "real" == 2. && a "imag" == 3.);
    assert (b "mag" == sqrt 2. && b "ang" == atan2 1. 1.);
    Printf.printf "success.\n"

let test_polar () =
    Printf.printf "testing polar...\n";
    let ( == ) = equ in
    let a = complex 2. 3. in
    let b = complex 1. 1. in
    let c = polar (a "mag") (a "ang") in
    let d = polar (b "mag") (b "ang") in
    assert (c "mag" == a "mag" && c "ang" == a "ang");
    assert (c "real" == a "real" && c "imag" == a "imag");
    assert (d "mag" == b "mag" && d "ang" == b "ang");
    assert (d "real" == b "real" && d "imag" == b "imag");
    Printf.printf "success.\n"

  (*
  let _ = test_complex ()
  let _ = test_polar ()
  *)
