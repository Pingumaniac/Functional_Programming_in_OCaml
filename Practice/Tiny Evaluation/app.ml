#use "globals.ml"
#use "eval.ml"

let test () =
    let max = parse
                "( (lambda (x y)\
                        (if (>= x y) x y))\
                    1 2)" in

    let sum = parse
                "(  (lambda (f) (f f))\
                    (lambda (self x)\
                        (if (= x 0)\
                            0\
                            (+  x\
                                (self self (- x 1)))))\
                    10)" in

    let gcd = parse
                "(  (lambda (f) (f f))\
                    (lambda (self x y)\
                        (if (= x y)\
                            x\
                            (if (>= x y)\
                                (self self (- x y) y)\
                                (self self (- y x) x))))\
                    14 21)" in

    let lst = parse
                "(  (lambda (cons car cdr)\
                        (car (cdr\
                            (cons 1 (cons 2 (cons 3 false))))))\
                    (lambda (x y z) (if z x y))\
                    (lambda (x) (x true))\
                    (lambda (x) (x false)))" in

    eval max [] |> print;
    eval sum [] |> print;
    eval gcd [] |> print;
    eval lst [] |> print;
    ()

let _ = test ()
