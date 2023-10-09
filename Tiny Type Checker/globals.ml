type kind
    = Boolean
    | Number
    | Function of kind * kind
    | Error

type expr
    = NUM  of int    (*number*)
    | BOOL of bool   (*Boolean*)
    | VAR  of string (*variable*)
    (*arithmetic exprs*)
    | ADD of expr * expr | SUB of expr * expr
    (*comparators*)
    | EQ of expr * expr  | GE of expr * expr
    (*logical exprs*)
    | AND of expr * expr | OR  of expr * expr | NOT of expr
    (*conditional expr*)
    | IF  of expr * expr * expr
    (*function definition: parameter, body*)
    | FUN of (string * kind) * expr
    (*function application: operator, operand*)
    | APP of expr * expr
