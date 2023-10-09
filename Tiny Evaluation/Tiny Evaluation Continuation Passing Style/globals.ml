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
    | FUN of string * expr
    (*closure: parameter, body, environment*)
    | CLO of string * expr * (string * expr) list
    (*function application: operator, operand*)
    | APP of expr * expr
