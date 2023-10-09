(*literals: true / false*)
let true_  = fun x y -> x
let false_ = fun x y -> y

let _ = assert(1 = true_  1 0)
let _ = assert(0 = false_ 1 0)

(*logical operators*)
let and_ = fun x y -> x y false_
let or_  = fun x y -> x true_ y
let not_ = fun z -> z false_ true_

let _ = assert(1 = (and_ true_  true_)   1 0)
let _ = assert(0 = (and_ true_  false_)  1 0)
let _ = assert(0 = (and_ false_ true_)   1 0)
let _ = assert(0 = (and_ false_ false_)  1 0)

let _ = assert(1 = (or_ true_  true_)    1 0)
let _ = assert(1 = (or_ true_  false_)   1 0)
let _ = assert(1 = (or_ false_ true_)    1 0)
let _ = assert(0 = (or_ false_ false_)   1 0)

let _ = assert(0 = (not_ true_)  1 0)
let _ = assert(1 = (not_ false_) 1 0)

(*conditional operator*)
let if_ = fun p t f u -> p t f u

(*comparators*)
let ge  = fun x y -> if x >= y then true_ else false_

let gt = fun x y -> and_ (ge x y) (not_ (ge y x))
let lt = fun x y -> and_ (not_ (ge x y)) (ge y x)
let eq = fun x y -> and_ (ge x y) (ge y x)
let ne = fun x y -> or_ (gt x y) (lt x y)

(*thunk: implement gcd using thunk*)
let rec gcd a b u =
  if_ (gt a b)
    (fun _ -> gcd (a - b) b ())
    (if_ (lt a b)
      (fun _ -> gcd b (a - b) ())
      (fun _ -> a))
    u

let _ = assert(4 = gcd 12 16 ())
