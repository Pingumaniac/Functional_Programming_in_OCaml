(*Stream*)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

let cons = fun h thunk ->
    Cons (h, thunk)

(* car and cdr implementation *)
let car = function
  | Nil -> failwith "empty stream"
  | Cons (h, _) -> h

let cdr = function
  | Nil -> failwith "empty stream"
  | Cons (_, t) -> t ()

(* const implementation *)
let rec const c = Cons (c, fun () -> const c)

(* sum implementation *)
let rec sum a_strm b_strm =
  match a_strm, b_strm with
  | Cons (ha, ta), Cons (hb, tb) ->
      Cons (ha +. hb, fun () -> sum (ta ()) (tb ()))
  | _, _ -> Nil

(* fibs implementation *)
let rec fibs () =
  let rec aux a b =
    Cons (a, fun () -> aux b (a +. b))
  in
  aux 0. 1.

(* psum implementation *)
let psum strm =
  let rec iter acc strm =
    match strm with
    | Cons (h, t) -> Cons (acc +. h, fun () -> iter (acc +. h) (t ()))
    | Nil -> Nil
  in
  iter 0. strm

(* diff implementation *)
let diff strm =
  let rec iter prev strm =
    match strm with
    | Cons (h, t) -> Cons (h -. prev, fun () -> iter h (t ()))
    | Nil -> Nil
  in
  match strm with
  | Cons (h, t) -> iter h (t ())
  | Nil -> Nil

(* sys implementation *)
(* sys implementation *)
let sys strm =
    let rec iter y1 y2 u1 u2 strm =
      match strm with
      | Cons (u, t) ->
        let y = 0.9 *. y1 -. 0.1 *. y2 +. 0.5 *. u -. 0.3 *. u1 +. 0.1 *. u2 in
        Cons (y, fun () -> iter y y1 u u1 (t ()))
      | Nil -> Nil
    in
    iter 0. 0. 0. 0. strm


(* make_rand_stream implementation *)
let rand_update x = (x * 16807) mod 0x7fffffff
let rec make_rand_stream seed =
  let r = rand_update seed in
  let x = (float r) /. (float 0x7fffffff) in
  Cons (x, fun () -> make_rand_stream r)

(*estimate pi*)
let pi_stream =
    let inside x y =
        if x *. x +. y *. y < 1. then 1 else 0 in

    let rec iter cnt_inside cnt_total r_strm =
        match r_strm with
        | Cons (rx, rt1) ->
            (match rt1 () with
            | Cons (ry, rt2) ->
                let new_inside = inside rx ry in
                let new_cnt_inside = cnt_inside + new_inside in
                let new_cnt_total = cnt_total + 1 in
                let estm = 4. *. (float_of_int new_cnt_inside) /. (float_of_int new_cnt_total) in
                let thunk = fun () -> iter new_cnt_inside new_cnt_total (rt2 ()) in
                Cons(estm, thunk)
            | Nil -> Nil
            )
        | Nil -> Nil
    in

    iter 0 0 (make_rand_stream 1)


let rec stream_ref n strm =
    if n = 0
    then car strm
    else stream_ref (n-1) (cdr strm)

let _ =
    pi_stream
    |> stream_ref 1000000
    |> Printf.printf "pi ~ %f\n"
