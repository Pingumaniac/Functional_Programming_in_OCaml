type expr
    = NUM  of int    (*number*)
    | BOOL of bool   (*Boolean*)
    | VAR  of string (*variable*)
    (*arithmetic exprs*)
    | ADD of expr * expr | SUB of expr * expr | MUL of expr * expr | DIV of expr * expr
    (*comparators*)
    | EQ of expr * expr  | GE of expr * expr
    (*logical exprs*)
    | AND of expr * expr | OR  of expr * expr | NOT of expr
    (*conditional expr*)
    | IF  of expr * expr * expr
    (*function definition: parameter, body*)
    | FUN of string * expr
    (*closure: parameter, body, environment*)
    | CLO of string * expr * (string * expr) list
    (*lazy eval, thunk: expr, env*)
    | TNK of expr * (string * expr) list
    (*function application: operator, operand*)
    | APP of expr * expr

(*state monad*)
module StateMonad = struct
    (*monad operations*)
    (*return: wraps values in a monad type*)
    let ret value =
        fun state -> (value, state)

    (*bind: composes functions that return a monad type*)
    let (>>=) mon f =
        fun state ->
            mon state |> fun (v, s) ->
            (f v) s

    (*other operations for state monad*)
    let get =
        fun state -> (state, state)

    let set state' =
        fun state -> ((), state')
end
