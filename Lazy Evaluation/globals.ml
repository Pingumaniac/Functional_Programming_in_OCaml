(*write the expr type*)
type expr
    = NUM  of int
    | BOOL of bool
    | VAR  of string
    (*arithmetic exprs*)
    | ADD of expr * expr
    | SUB of expr * expr
    (*comparators*)
    | EQ of  expr * expr
    | GE of  expr * expr
    (*logical exprs*)
    | AND of expr * expr
    | OR  of expr * expr
    | NOT of expr
    (*conditional expr*)
    | IF  of expr * expr * expr (* condition, true-expr, false-expr *)
    (*function definition: parameter, body*)
    | FUN of string * expr (* param name and body-expr *)
    (*closure: parameter, body, environment: list of (name, expr) tuples*)
    | CLO of string * expr * ((string * expr) list)
    (*function application: operator, operand*)
    | APP of expr * expr (*function and argument*)
