(*#use "globals.ml"*)
#use "scanner.ml"

let parse str =
    let car = function
        | h::t -> h
        | _ -> assert false in
    let cdr = function
        | h::t -> t
        | _ -> assert false in

    (*match*)
    let mat tok lst =
        if tok = (car lst)
        then cdr lst
        else assert false in
        (*match (tok, car lst) with
        | (T_NUM x, T_NUM y) -> cdr lst
        | (T_VAR x, T_VAR y) -> cdr lst
        | (x, y) -> if x = y then cdr lst
                    else assert false in*)

    (*parse an expression*)
    let rec parse_expr lst =
        let (h, t) = (car lst, cdr lst) in
        match h with
        | T_NUM x -> (NUM x, t)
        | T_TRUE  -> (BOOL true, t)
        | T_FALSE -> (BOOL false, t)
        | T_VAR x -> (VAR x, t)
        | T_OPR -> t
                    |> parse_app_expr |> fun (exp, t1) -> t1
                    |> mat T_CPR
                    |> fun t2 -> (exp, t2)
        | _ -> assert false

    (*parse an expression within parentheses*)
    and parse_app_expr lst =
        let (h, t) = (car lst, cdr lst) in
        match h with
        (*primitive operators*)
        | T_ADD -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (ADD (exp1, exp2), t2)
        | T_SUB -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (SUB (exp1, exp2), t2)
        | T_EQ -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (EQ (exp1, exp2), t2)
        | T_GE -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (GE (exp1, exp2), t2)
        | T_AND -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (AND (exp1, exp2), t2)
        | T_OR -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> (OR (exp1, exp2), t2)
        | T_NOT -> t
                    |> parse_expr |> fun (exp, t1) -> (NOT (exp), t1)

        (*conditional operator*)
        | T_IF -> t
                    |> parse_expr |> fun (exp1, t1) -> t1
                    |> parse_expr |> fun (exp2, t2) -> t2
                    |> parse_expr |> fun (exp3, t3) -> (IF (exp1, exp2, exp3), t3)

        (*function defintion*)
        | T_LAMBDA -> t
                    |> mat T_OPR
                    |> get_var_list |> fun (vars, t1) -> t1
                    |> mat T_CPR
                    |> parse_expr |> fun (exp, t2) -> (curry vars exp, t2)

        (*function application*)
        | _ -> lst
                    |> parse_expr_list |> fun (exp_lst, t1) ->
                        app_arg_list (car exp_lst) (cdr exp_lst)
                    |> fun exp -> (exp, t1)

    (*get kind*)
    and get_kind lst =
        let (h, t) = (car lst, cdr lst) in
        match h with
        | T_KNUM  -> (Number, t)
        | T_KBOOL -> (Boolean, t)
        | T_OPR   -> t
            |> get_kind  |> fun (k1, t1) -> t1
            |> mat T_ARR
            |> get_kind  |> fun (k2, t2) -> t2
            |> mat T_CPR |> fun t3 -> (Function (k1, k2), t3)
        | _ -> assert false

    (*formal param var:kind list -> (var, kind) list*)
    and get_var_list lst =
        let (h, t) = (car lst, cdr lst) in
        match h with
        | T_VAR v -> t
                    |> mat T_COL
                    |> get_kind |> fun (k, t1) -> t1
                    |> get_var_list |> fun (vs, t2) -> ((v, k)::vs, t2)
        | _ -> ([], lst)

    (*curry: multi-param function -> nested single param function*)
    and curry vars body =
        match vars with
        | h::t -> FUN (h, curry t body)
        | []   -> body

    (*actual param exprs -> expr list*)
    and parse_expr_list lst =
        let (h, t) = (car lst, cdr lst) in
        match h with
        | T_CPR -> ([], lst)  (*return without consuming h*)
        | T_EOS -> ([], lst)
        | _ -> lst
                |> parse_expr |> fun (exp, t1) -> t1
                |> parse_expr_list |> fun (exp_lst, t2) -> (exp :: exp_lst, t2)

    (*apply curried functions to expr list one by one*)
    and app_arg_list opr arg_lst =
        match arg_lst with
        | h::t -> app_arg_list (APP (opr, h)) t
        | []   -> opr in

    scan str
    |> parse_expr |> fun (exp, t) -> t
    |> mat T_EOS  |> fun _ -> exp
