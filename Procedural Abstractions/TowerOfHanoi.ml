let rec move n src dst aux =
  if n = 1 then
    Printf.printf "move from %s to %s\n" src dst
  else begin
    move (n - 1) src aux dst;
    Printf.printf "move from %s to %s\n" src dst;
    move (n - 1) aux dst src;
  end

let main () =
    move 3 "A" "B" "C"

let () = main ()

(*
Output:
val move : int -> string -> string -> string -> unit = <fun>
val main : unit -> unit = <fun>
move from A to B
move from A to C
move from B to C
move from A to B
move from C to A
move from C to B
move from A to B
- : unit = ()
*)
