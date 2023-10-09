(*app.ml*)
#use "complex.ml"
#use "complex_arith.ml"
#use "newton.ml"

(*run the test cases*)
let _ = test_complex ()
let _ = test_polar ()

let _ = test_arith_complex ()    
let _ = test_arith_polar ()    

let _ = test_sqrt ()
let _ = test_poly ()

(* expected results

testing complex...
success.
- : unit = ()
testing polar...
success.
- : unit = ()
testing arith...
success.
- : unit = ()
testing arith...
success.
- : unit = ()
testing newton (sqrt -2)...
sa: 0.000000 + i 1.414214
sb: 1.414214 \_ 1.000000
success.
- : unit = ()
testing newton (solve x^2 + 1)...
ans: 0.000000 + i 1.000000
success.
- : unit = ()
*)
