#use "globals.ml"
#use "eval.ml"

let eval_str str_expr env =
    parse str_expr
    |> fun expr -> eval expr env

let define name str_expr env =
    eval_str str_expr env
    |> fun v -> (name, v)::env

let str_rec =
    "(lambda (f)\
        (f f))"
let str_cons =
    "(lambda (x y z)\
        (if z x y))"
let str_car =
    "(lambda (x)\
        (x true))"
let str_cdr =
    "(lambda (x)\
        (x false))"

let env = []
        |> define "rec"  str_rec
        |> define "cons" str_cons
        |> define "car"  str_car
        |> define "cdr"  str_cdr

let str_max =
    "(lambda (x y) (if (> x y) x y))"

let str_gcd =
    "(lambda (a b)
        ((rec (lambda (f)
            (lambda (a b)
                (if (= b 0)
                    a
                    (f f (mod a b) b))))) a b))"

let str_index =
    "(lambda (s n)
        ((rec (lambda (f)
            (lambda (s n)
                (if (= n 0)
                    (car s)
                    (f f (cdr s) (- n 1))))) s n))"

let str_nat =
    "(rec (lambda (f)
        (lambda (n)
            (cons n (f f (+ n 1))))) 0)"

let _ = env
        |> define "max" str_max
        |> eval_str "(max 1 2)"
        |> print

let _ = env
        |> define "gcd" str_gcd
        |> eval_str "(gcd 30 42)"
        |> print

let _ = env
        |> define "index" str_index
        |> define "nat"   str_nat
        |> eval_str "(index nat 10)"
        |> print
