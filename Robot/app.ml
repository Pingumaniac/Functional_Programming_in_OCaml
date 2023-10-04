(*https://caml.inria.fr/pub/docs/manual-caml-light/node16.html*)
(*https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora050.html*)

#load "graphics.cma"
#load "unix.cma"
#directory "+threads"
#load "threads.cma"

#use "globals.ml"
#use "vector.ml"
#use "basis.ml"
#use "board.ml"
#use "pose.ml"
#use "drawer.ml"
#use "command.ml"
#use "game.ml"


let app () =
    (*enable print stack trace*)
    let _ = Printexc.record_backtrace true in

    (*camera basis*)
    let b_camera = (b_rotx (-60.) (b_rotz (-210.) gb_basis)) in

    (*initial pose*)
    let ipose = (90., 30., 60., 0., mark_n) in

    (*initial board*)
    let iboard = [ mark_n; mark_n; mark_n;
                mark_n; mark_n; mark_n;
                mark_n; mark_n; mark_n;
                mark_o (*9*); mark_x (*10*)] in

    Graphics.open_graph " 800x800";
    Graphics.auto_synchronize false;
    game b_camera (ipose, iboard) |> print_result;
    Graphics.auto_synchronize true

let _ = app ()

