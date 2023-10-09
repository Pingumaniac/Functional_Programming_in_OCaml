type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

module type IStream = sig
    val cons: 'a -> (unit -> 'a stream) -> 'a stream
    val car:  'a stream -> 'a
    val cdr:  'a stream -> 'a stream
end

module Stream : IStream = struct
    let cons = fun h thunk ->
        Cons (h, thunk)

    let car = function
        | Cons (h, _) -> h
        | _ -> assert false

    let cdr = function
        | Cons (_, thunk) -> thunk ()
        | _ -> assert false
end


module type ISystem = sig
    val output: float stream -> float stream
end

module type ARMAParam = sig
    val a1: float
    val a2: float
    val a3: float
    val b0: float
    val b1: float
    val b2: float
    val b3: float
end

module ARMA (Param:ARMAParam) : ISystem = struct
    open Stream

    (*ARMA system model
        y(t) = a1*y(t-1) + a2*y(t-2) + a3*y(t-3)
             + b0*u(t)   + b1*u(t-1) + b2*u(t-2) + b3*u(t-3)
        assume that y(t) = 0 for t < 0 and u(t) = 0 for t < 0
    *)
    let a1 = Param.a1
    let a2 = Param.a2
    let a3 = Param.a3
    let b0 = Param.b0
    let b1 = Param.b1
    let b2 = Param.b2
    let b3 = Param.b3

    let output in_strm =
        let rec iter (y1, y2, y3) (u1, u2, u3) u =
            let u0 = car u in
            let y0 = (a1 *. y1) +. (a2 *. y2) +. (a3 *. y3) +. (b0 *. u0) +. (b1 *. u1) +. (b2 *. u2) +. (b3 *. u3) in
            Cons (y0, fun () -> iter (y0, y1, y2) (u0, u1, u2) (cdr u))
        in
        iter (0., 0., 0.) (0., 0., 0.) in_strm
end


module type IController = sig
    val control: float stream -> float stream
end

module type PIDParam = sig
    val p: float
    val i: float
    val d: float
end

module PID (Param:PIDParam) : IController = struct
    open Stream

    module IntegratorParam : ARMAParam = struct
        let a1 = 1.0
        let a2 = 0.0
        let a3 = 0.0
        let b0 = 1.0
        let b1 = 0.0
        let b2 = 0.0
        let b3 = 0.0
    end

    module Integrator = ARMA(IntegratorParam)

    module DifferentiatorParam : ARMAParam = struct
        let a1 = 0.0
        let a2 = 0.0
        let a3 = 0.0
        let b0 = 1.0
        let b1 = -1.0
        let b2 = 0.0
        let b3 = 0.0
    end

    module Differentiator = ARMA(DifferentiatorParam)

    let p_gain = Param.p
    let i_gain = Param.i
    let d_gain = Param.d

    let control err_strm =
        let i_strm = Integrator.output err_strm in
        let d_strm = Differentiator.output err_strm in
        let rec iter e_strm i_strm d_strm =
            match e_strm, i_strm, d_strm with
            | Cons(e, e_thunk), Cons(i, i_thunk), Cons(d, d_thunk) ->
                let u = p_gain *. e +. i_gain *. i +. d_gain *. d in
                Cons (u, fun () -> iter (e_thunk ()) (i_thunk ()) (d_thunk ()))
            | _ -> Nil
        in
        iter err_strm i_strm d_strm
end


module ClosedLoopSystem (Plant:ISystem) (Ctrl:IController) : ISystem = struct
    open Stream

    let rec difference a_strm b_strm =
        match a_strm, b_strm with
        | Cons(a, a_thunk), Cons(b, b_thunk) ->
            Cons (a -. b, fun () -> difference (a_thunk ()) (b_thunk ()))
        | _ -> Nil

    let output ref_strm =
        let rec fwd_flow o_strm =
            o_strm
            |> difference ref_strm
            |> Ctrl.control
            |> Plant.output
        in
        let rec y = lazy (cons 0. (fun () -> Lazy.force y) |> fwd_flow) in
        Lazy.force y
end


module TestSys (Sys: ISystem) = struct
    open Stream

    let rec const c = cons c (fun () -> const c)
    let ones        = const 1.
    let zeros       = const 0.
    let impulse     = cons 1. (fun() -> zeros)

    let rec print n strm =
        let open Printf in
        printf "%6.3f, " (car strm);
        if n > 0
        then print (n-1) (cdr strm)
        else printf "\n"; ()

    let test test_title =
        Printf.printf "%s\n" test_title;
        impulse |> Sys.output |> print 10;
        ones    |> Sys.output |> print 10;
        ()
end

module Test = struct
    module Integrator = ARMA (struct
        let a1 = 1.0
        let a2 = 0.0
        let a3 = 0.0
        let b0 = 1.0
        let b1 = 0.0
        let b2 = 0.0
        let b3 = 0.0
    end)

    module Differentiator = ARMA (struct
        let a1 = 1.0
        let a2 = -1.0
        let a3 = 0.0
        let b0 = 1.0
        let b1 = 0.0
        let b2 = 0.0
        let b3 = 0.0
    end)

    module Plant = ARMA (struct
        let a1 = 0.9
        let a2 = 0.0
        let a3 = 0.0
        let b0 = 0.5
        let b1 = 0.3
        let b2 = 0.0
        let b3 = 0.0
    end)

    module Ctrl  = PID (struct
        let p = 0.9
        let i = 0.1
        let d = 0.1
    end)

    module CLS   = ClosedLoopSystem (Plant) (Ctrl)

    module IntTest   = TestSys (Integrator)
    module DiffTest  = TestSys (Differentiator)
    module PlantTest = TestSys (Plant)
    module CLSTest   = TestSys (CLS)

    let test () =
        IntTest.test   "Integrator test";
        DiffTest.test  "Differentiator test";
        PlantTest.test "Plant test";
        CLSTest.test   "Closed Loop System test";
        ()
end

let _ = Test.test ()
