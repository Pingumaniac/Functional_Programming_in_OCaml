(*sum from 0 to n*)
(*
let rec sum n =
  if n = 0
  then n
  else n + sum (n-1)
*)

(*Rewrite sum in CPS*)
let sum n =
  let rec iter n k =
    if n = 0 then k 0
    else iter (n - 1) (fun m -> k (n + m))
  in
  iter n (fun x -> x)


let _ = sum 1000000 (*500000500000*)


(**********************************************************
 n-th Fibonacci number
**********************************************************)
(*
let rec fib n =
  if n <= 1
  then n
  else fib (n-1) + fib (n-2)
*)

(*Rewrite fib in CPS*)
let fib n =
  let rec iter n k =
    if n <= 1 then k n
    else iter (n - 1) (fun m -> iter (n - 2) (fun o -> k (m + o)))
  in
  iter n (fun x -> x)

let _ = fib 8 (*21*)

(*symbolic derivative*)
type expr = N of int            (*number*)
        | X | Y | Z           (*variable*)
        | Add of expr * expr  (*add expr*)
        | Mul of expr * expr  (*mul expr*)

let add a b = (*add two exprs*)
  match (a, b) with
  | (N 0, y) -> y
  | (x, N 0) -> x
  | (N x, N y) -> N (x + y)
  | _ -> Add (a, b)

let mul a b = (*multiply two exprs*)
  match (a, b) with
  | (N 0, y) -> N 0
  | (N 1, y) -> y
  | (x, N 0) -> N 0
  | (x, N 1) -> x
  | (N x, N y) -> N (x * y)
  | _ -> Mul (a, b)

(*derivative of expr w.r.t. var*)
(*
let rec deriv expr var =
  match expr with
  | N x -> N 0
  | X | Y | Z  -> if expr = var then N 1 else N 0
  | Add (a, b) -> add (deriv a var) (deriv b var)
  | Mul (a, b) -> add (mul a (deriv b var)) (mul (deriv a var) b)
*)

(*Rewrite deriv in CPS*)
let rec deriv expr var k =
  match expr with
  | N _ -> k (N 0)
  | X | Y | Z -> k (if expr = var then N 1 else N 0)
  | Add (a, b) ->
    deriv a var (fun da -> deriv b var (fun db -> k (add da db)))
  | Mul (a, b) ->
    deriv a var (fun da -> deriv b var (fun db -> k (add (mul a db) (mul da b))))

(*convert expr to string*)
(*
let rec to_str expr =
  let open Printf in
  match expr with
  | N a -> sprintf "%d" a
  | X -> "x"
  | Y -> "y"
  | Z -> "z"
  | Add (a, b) -> sprintf "(%s + %s)" (to_str a) (to_str b)
  | Mul (a, b) -> sprintf "%s * %s"   (to_str a) (to_str b)
*)

(*Rewrite to_str in CPS*)
let rec to_str expr k =
  match expr with
  | N a -> k (Printf.sprintf "%d" a)
  | X -> k "x"
  | Y -> k "y"
  | Z -> k "z"
  | Add (a, b) ->
    to_str a (fun sa -> to_str b (fun sb -> k (Printf.sprintf "(%s + %s)" sa sb)))
  | Mul (a, b) ->
    to_str a (fun sa -> to_str b (fun sb -> k (Printf.sprintf "%s * %s" sa sb)))

(*test cases*)
let test () =
  let ( + ) = add in
  let ( * ) = mul in

  let a = X + N 3 in
  let b = N 2 * X + N 3 * Y in
  let c = X * Y * (X + N 3) in
  let d = X * X + N 2 * X + N 1 in

  [a; b; c; d] |> List.iter (fun exp ->
          let prn = Printf.printf "%s\n" in
          to_str exp prn;
          deriv exp X (fun e -> to_str e prn);
          deriv exp Y (fun e -> to_str e prn))

let _ = test ()
