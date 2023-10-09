#use "globals.ml"
#use "eval.ml"

(*--Helpers----------------------*)
(*eval_str: evaluates string expression in env*)
let eval_str str_expr env =
    parse str_expr
    |> fun expr -> eval expr env

(*define: extends env with the value of string expression*)
let define name str_expr env =
    eval_str str_expr env
    |> fun v -> (name, v)::env

let str_true  = "(lambda (x y) x)"
let str_false = "(lambda (x y) y)"

let env = []
        |> define "TRUE"  str_true
        |> define "FALSE" str_false

let _ = Printf.printf "--Test Boolean----------\n";

eval_str "(TRUE  true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "(FALSE true false)" env |> fun x ->
    print x;
    assert (x = BOOL false)

(* expected results
BOOL(true)
BOOL(false)
*)

(*--Logical operators----------------------*)
let str_and = "(lambda (x y) (x y FALSE))"
let str_or = "(lambda (x y) (x TRUE y))"
let str_not = "(lambda (x) (x FALSE TRUE))"

let env = env
        |> define "AND" str_and
        |> define "OR"  str_or
        |> define "NOT" str_not

let _ = Printf.printf "--Test Logical OP----------\n";

eval_str "((AND TRUE FALSE) true false)" env |> fun x ->
    print x;
    assert (x = BOOL false);

eval_str "((OR  TRUE FALSE) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((NOT TRUE) true false)" env |> fun x ->
    print x;
    assert (x = BOOL false)

(* expected results
BOOL(false)
BOOL(true)
BOOL(false)
*)

(*--List operators----------------------*)
let str_cons = "(lambda (x y) (lambda (f) (f x y)))"
let str_car = "(lambda (p) (p (lambda (x y) x)))"
let str_cdr = "(lambda (p) (p (lambda (x y) y)))"
let str_nil = "(lambda (x) FALSE)"
let str_is_nil = "(lambda (x) (x (lambda (a b) FALSE)))"


let env = env
        |> define "CONS"   str_cons
        |> define "CAR"    str_car
        |> define "CDR"    str_cdr
        |> define "NIL"    str_nil
        |> define "IS_NIL" str_is_nil

let _ = Printf.printf "--Test List OP----------\n";

eval_str "(CAR (CONS 2 3))" env |> fun x ->
    print x;
    assert (x = NUM 2);

eval_str "(CDR (CONS 2 3))" env |> fun x ->
    print x;
    assert (x = NUM 3);

eval_str "(CAR (CDR (CONS 2 (CONS 3 4))))" env |> fun x ->
    print x;
    assert (x = NUM 3);

eval_str "((NIL NIL) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((IS_NIL NIL) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((IS_NIL (CONS 2 3)) true false)" env |> fun x ->
    print x;
    assert (x = BOOL false)

(* expected results
NUM(2)
NUM(3)
NUM(3)
BOOL(true)
BOOL(true)
BOOL(false)
*)

(*--Numbers----------------------*)
let str_zero = "(lambda (f x) x)"
let str_succ = "(lambda (n f x) (f (n f x)))"
let str_is_zero = "(lambda (n) (n (lambda (x) FALSE) TRUE))"


let env = env
        |> define "ZERO"    str_zero
        |> define "SUCC"    str_succ
        |> define "IS_ZERO" str_is_zero

(*for testing*)
let env = env
        |> define "ONE"   "(SUCC ZERO)"
        |> define "TWO"   "(SUCC ONE)"
        |> define "THREE" "(SUCC TWO)"
        |> define "FOUR"  "(SUCC THREE)"
        |> define "FIVE"  "(SUCC FOUR)"
        |> define "SIX"   "(SUCC FIVE)"
        |> define "SEVEN" "(SUCC SIX)"
        |> define "EIGHT" "(SUCC SEVEN)"
        |> define "NINE"  "(SUCC EIGHT)"
        |> define "TEN"   "(SUCC NINE)"

let env = env
        |> define "INC"   "(lambda (x) (+ x 1))"

let _ = Printf.printf "--Test Numbers----------\n";

eval_str "((IS_ZERO ZERO) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((IS_ZERO ONE)  true false)" env |> fun x ->
    print x;
    assert (x = BOOL false);

eval_str "(ZERO INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 0);

eval_str "(TWO  INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 2)

(* expected results
BOOL(true)
BOOL(false)
NUM(0)
NUM(2)
*)


(*--Arithmetic operators----------------------*)
let str_add = "(lambda (m n f x) (m f (n f x)))"
let str_mul = "(lambda (m n f) (m (n f)))"
let str_sub = "(lambda (m n) (n PRED m))"


let str_pred =
    "(  (lambda (shift n)\
            (CAR (n shift (CONS ZERO ZERO))))\
        (lambda (pair)\
            (CONS (CDR pair) (SUCC (CDR pair)))))"

let env = env
        |> define "ADD"  str_add
        |> define "MUL"  str_mul
        |> define "PRED" str_pred
        |> define "SUB"  str_sub

let _ = Printf.printf "--Test Arithmetic OP----------\n";

eval_str "((ADD ONE TWO) INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 3);

eval_str "((MUL SIX TWO) INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 12);

eval_str "((SUB SIX TWO) INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 4)

(* expected results
NUM(3)
NUM(12)
NUM(4)
*)


(*--Comparison operators----------------------*)
let str_geq = "(lambda (m n) (IS_ZERO (SUB n m)))"
let str_equ = "(lambda (m n) (AND (GEQ m n) (GEQ n m)))"

let env = env
        |> define "GEQ" str_geq
        |> define "EQU" str_equ

let _ = Printf.printf "--Test Comparison OP----------\n";

eval_str "((GEQ TWO TWO) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((GEQ ONE TWO) true false)" env |> fun x ->
    print x;
    assert (x = BOOL false);

eval_str "((GEQ TWO ONE) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((EQU TWO TWO) true false)" env |> fun x ->
    print x;
    assert (x = BOOL true);

eval_str "((EQU SIX TWO) true false)" env |> fun x ->
    print x;
    assert (x = BOOL false)

(* expected results
BOOL(true)
BOOL(false)
BOOL(true)
BOOL(true)
BOOL(false)
*)


(*--Conditional operator----------------------*)
let str_if = "(lambda (cond a b) (cond a b))"

let env = env
        |> define "IF" str_if

let _ = Printf.printf "--Test If----------\n";

eval_str "((IF (GEQ ONE TWO) ONE TWO) INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 2);

eval_str "((IF (GEQ TWO ONE) ONE TWO) INC 0)" env |> fun x ->
    print x;
    assert (x = NUM 1)

(* expected results
NUM(2)
NUM(1)
*)


(*--Y-combinator----------------------*)
let str_y =
    "(lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))"

let env = env
        |> define "Y" str_y

(* max function *)
let str_max = "(lambda (m n) (IF (GEQ m n) m n))"

let _ = Printf.printf "--Test max----------\n";
define "max" str_max env
    |> eval_str "((max TWO TEN) INC 0)"
    |> fun x -> print x; x
    |> fun x -> assert (x = NUM 10)
(* expected results
NUM(10)
*)


(*sum function*)
let str_sum = "(Y (lambda (f n) (IF (IS_ZERO n) ZERO (ADD n (f (PRED n))))))"

let _ = Printf.printf "--Test sum----------\n";
define "sum" str_sum env
    |> eval_str "((sum TEN) INC 0)"
    |> fun x -> print x; x
    |> fun x -> assert (x = NUM 55)
(* expected results
NUM(55)
*)

(*gcd function*)
let str_gcd = "(Y (lambda (f a b) (IF (EQU b ZERO) a (f b (SUB a (MUL (DIV a b) b))))))"

let _ = Printf.printf "--Test gcd----------\n";
define "gcd" str_gcd env
    |> eval_str "((gcd EIGHT SIX) INC 0)"
    |> fun x -> print x; x
    |> fun x -> assert (x = NUM 2)
(* expected results
NUM(2)
*)


(*index and a stream of natural numbers*)
let str_index = "(Y (lambda (f i s) (IF (EQU i ZERO) (CAR s) (f (PRED i) (CDR s)))))"
let str_nat = "(Y (lambda (f n) (CONS n (f (SUCC n)))))"

let _ = Printf.printf "--Test nat----------\n";
env |> define "index" str_index
    |> define "nat"   str_nat
    |> eval_str "((index nat TEN) INC 0)"
    |> fun x -> print x; x
    |> fun x -> assert (x = NUM 10)
(* expected results
NUM(10)
*)
