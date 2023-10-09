#use "parser.ml"

(*print expr*)
let print expr =
    let rec to_str e =
        let open Printf in
        match e with
        | NUM n -> sprintf "NUM(%d)" n
        | BOOL b -> if b then "BOOL(true)" else "BOOL(false)"
        | VAR v -> sprintf "VAR(%s)" v
        | ADD (a, b) -> sprintf "ADD(%s, %s)" (to_str a) (to_str b)
        | SUB (a, b) -> sprintf "SUB(%s, %s)" (to_str a) (to_str b)
        | EQ  (a, b) -> sprintf "EQ(%s, %s)"  (to_str a) (to_str b)
        | GE  (a, b) -> sprintf "GE(%s, %s)"  (to_str a) (to_str b)
        | AND (a, b) -> sprintf "AND(%s, %s)" (to_str a) (to_str b)
        | OR  (a, b) -> sprintf "OR(%s, %s)"  (to_str a) (to_str b)
        | NOT a      -> sprintf "NOT(%s)"     (to_str a)
        | IF  (c, t, f) -> sprintf "IF(%s, %s, %s)" (to_str c) (to_str t) (to_str f)
        | FUN (v, e) -> sprintf "FUN(%s, %s)"  v (to_str e)
        | CLO (v, e, ev) -> sprintf "CLO(%s, %s, env)" v (to_str e)
        | APP (a, b) -> sprintf "APP(%s, %s)" (to_str a) (to_str b) in

     to_str expr |> (Printf.printf "%s\n")


(*evaluate expr in env*)
let rec eval expr env =
    let dropNUM  = function NUM  n -> n | e -> print e; assert false in
    let dropBOOL = function BOOL b -> b | e -> print e; assert false in
    let dropCLO  = function CLO (v, e, ev) -> (v, e, ev) | e -> print e; assert false in

    (*look up the value of a variable from an environment*)
    (*implement lookup
        environment is a list of (name, expr) tuples
    *)
    let rec lookup name env =
        match env with
        | [] -> failwith "Variable not found"
        | (n, e)::xs -> if n = name then e else lookup name xs
        in

    match expr with
    (*evaluate the literal expressions*)
    | BOOL b -> BOOL b
    | NUM n  -> NUM n

    (*evaluate the variable expression*)
    | VAR v  -> lookup v env

    (*implement the primitive operators*)
    | ADD (a, b)   ->  NUM (dropNUM (eval a env) + dropNUM (eval b env))
    | SUB (a, b)   ->  NUM (dropNUM (eval a env) - dropNUM (eval b env))
    | EQ (a, b)    ->  BOOL (dropBOOL (eval a env) = dropBOOL (eval b env))
    | GE (a, b)    ->  BOOL (dropBOOL (eval a env) >= dropBOOL (eval b env))
    | AND (a, b)   ->  BOOL (dropBOOL (eval a env) && dropBOOL (eval b env))
    | OR (a, b)    ->  BOOL (dropBOOL (eval a env) || dropBOOL (eval b env))
    | NOT a        ->  BOOL (not (dropBOOL (eval a env)))

    (*implement the conditional expression*)
    | IF (c, t, f) -> if dropBOOL (eval c env) then eval t env else eval f env

    (*evaluate the function definition*)
    | FUN (v, e)   -> CLO (v, e, env)

    (*implement the function application*)
    | APP (f, a)  ->
        let closure = dropCLO (eval f env) in
        let (v, body, closure_env) = closure in
        let arg_value = eval a env in
        eval body ((v, arg_value)::closure_env)

    | _ -> assert false
