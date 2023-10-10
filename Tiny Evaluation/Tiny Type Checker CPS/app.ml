#use "globals.ml"
#use "type.ml"
#use "parser.ml"

(*--Helpers----------------------*)
(*identity continuation*)
let id = fun x -> x

(*type_str: get the type of string expression in env*)
let type_str str_expr env k =
    parse str_expr
    |> fun expr -> kind expr env k

(*define: extends env with the value of string expression
*)
let define name str_expr env =
    type_str str_expr env (fun knd ->
    (name, knd)::env)

let str_max =
    "(lambda (x:num y:num)
            (if (>= x y) x y))"

let str_inside =
    "(lambda (lb:num ub:num x:num)
        (and (>= x lb) (>= ub x)))"

let str_outside =
    "(lambda (in:(num -> (num -> (num -> bool)))
              lb:num
              ub:num
              x:num)
        (in lb ub x))"

let str_cons =
    "(lambda (x:num
              y:num
              z:bool)
        (if z x y))"

let str_car =
    "(lambda (x:(bool -> num))
        (x true))"

let str_cdr =
    "(lambda (x:(bool -> num))
        (x false))"

let str_lst =
    "(  (lambda (pair  :(num -> (num -> (bool -> num)))
                 first :((bool -> num) -> num)
                 second:((bool -> num) -> num))
            (first (pair 1 2)))
        cons car cdr)"

let env = []
    |> define "max"     str_max
    |> define "inside"  str_inside
    |> define "outside" str_outside
    |> define "cons"    str_cons
    |> define "car"     str_car
    |> define "cdr"     str_cdr

let _ =
    Printf.printf "-- Function Defs -------------\n";
    type_str "max"     env print_kind;
    type_str "inside"  env print_kind;
    type_str "outside" env print_kind;
    type_str "cons"    env print_kind;
    type_str "car"     env print_kind;
    type_str "cdr"     env print_kind

let _ =
    Printf.printf "-- Function Apps -------------\n";
    type_str "(max 1 2)"              env print_kind;
    type_str "(inside 1 2 3)"         env print_kind;
    type_str "(outside inside 1 2 3)" env print_kind;
    type_str str_lst                  env print_kind

(* expected results
-- Function Defs -------------
(num -> (num -> num))
(num -> (num -> (num -> bool)))
((num -> (num -> (num -> bool))) -> (num -> (num -> (num -> bool))))
(num -> (num -> (bool -> num)))
((bool -> num) -> num)
((bool -> num) -> num)
- : unit = ()
-- Function Apps -------------
num
bool
bool
num
- : unit = ()
*)
