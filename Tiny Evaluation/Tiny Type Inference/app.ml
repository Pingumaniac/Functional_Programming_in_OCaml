#use "globals.ml"
#use "infer.ml"

open StateMonad

(*--Helpers----------------------*)
(*type_infer: infer the type of str_expr
    str_expr: tiny expression in string
    env: name -> kind expr map
*)
let type_infer str_expr env =
    parse str_expr     |>  fun expr ->
    newvar ()          >>= fun kv ->
    constr kv expr env >>= fun clist ->
    unify clist        |>  fun unifier ->
    unifier kv         |>
    ret

(*define: extends env with the value of string expression
*)
let define name str_expr env =
    type_infer str_expr env >>= fun kexp ->
    (name, kexp)::env       |>
    ret

let str_id =
    "(lambda (x) x)"

let str_max =
    "(lambda (x y)
        (if (>= x y) x y))"

let str_cons =
    "(lambda (x y sel) (if sel x y))"

let str_car =
    "(lambda (x) (x true))"

let str_cdr =
    "(lambda (x) (x false))"

let mon_def = []
    |>  define "id"   str_id
    >>= define "max"  str_max
    >>= define "cons" str_cons
    >>= define "car"  str_car
    >>= define "cdr"  str_cdr

let mon_test =
    mon_def >>= fun env ->
    type_infer "id" env                >>= fun k -> print k;
    type_infer "(id 1)" env            >>= fun k -> print k;
    type_infer "(id true)" env         >>= fun k -> print k;
    type_infer "max" env               >>= fun k -> print k;
    type_infer "(max 1)" env           >>= fun k -> print k;
    type_infer "(max 1 2)" env         >>= fun k -> print k;
    type_infer "cons" env              >>= fun k -> print k;
    type_infer "(cons 1)" env          >>= fun k -> print k;
    type_infer "(cons 1 2)" env        >>= fun k -> print k;
    type_infer "(cons true)" env       >>= fun k -> print k;
    type_infer "(cons true false)" env >>= fun k -> print k;
    type_infer "car" env               >>= fun k -> print k;
    type_infer "(car (cons 1 2))" env  >>= fun k -> print k;
    type_infer "cdr" env               >>= fun k -> print k;
    type_infer "(cdr (cons 1 2))" env  >>= fun k -> print k;
    ret ()

let _ = mon_test 0 |> fun (v, s) -> s

(* expected results
(k1) -> (k1)
num
bool
(num) -> ((num) -> (num))
(num) -> (num)
num
(k14) -> ((k14) -> ((bool) -> (k14)))
(num) -> ((bool) -> (num))
(bool) -> (num)
(bool) -> ((bool) -> (bool))
(bool) -> (bool)
((bool) -> (k25)) -> (k25)
num
((bool) -> (k30)) -> (k30)
num
*)
