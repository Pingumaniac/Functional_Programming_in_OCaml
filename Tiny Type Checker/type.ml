#use "parser.ml"

(*print expr*)
let rec kind_to_str knd =
    match knd with
    | Boolean -> "bool"
    | Number  -> "num"
    | Function (knd1, knd2) ->
        kind_to_str knd1 |> fun x ->
        kind_to_str knd2 |> fun y ->
        Printf.sprintf "(%s -> %s)" x y
    | Error   -> "error"


(*print kind*)
let print_kind knd =
    Printf.printf "%s\n" (kind_to_str knd)


(*kind of expr in env*)
let rec kind expr env =
    (*look up the kind of a variable from an environment*)
    let rec lookup name env =
        match env with
        | [] -> Error
        | (n, knd)::rest -> if name = n
                        then knd
                        else lookup name rest in

    match expr with
    | BOOL b -> Boolean
    | NUM n  -> Number
    | VAR v  -> lookup v env
    | ADD (a, b) | SUB (a, b)  ->
        kind a env |> fun ka ->
        kind b env |> fun kb ->
        if ka = Number && kb = Number
        then Number
        else Error
    | EQ (a, b) | GE (a, b) ->
        kind a env |> fun ka ->
        kind b env |> fun kb ->
        if ka = Number && kb = Number
        then Boolean
        else Error
    | AND (a, b) | OR (a, b) ->
        kind a env |> fun ka ->
        kind b env |> fun kb ->
        if ka = Boolean && kb = Boolean
        then Boolean
        else Error
    | NOT a ->
        kind a env |> fun ka ->
        if ka = Boolean
        then Boolean
        else Error

    | IF (cond, t_exp, f_exp) ->
        kind cond env  |> fun kc ->
        kind t_exp env |> fun kt ->
        kind f_exp env |> fun kf ->
        (*kind of t_exp and f_exp must match*)
        if kc = Boolean && kt = kf
        then kt
        else Error

    | FUN ((param, k_param), body) ->
        (*find the kind of body in the extended env*)
        kind body ((param, k_param)::env) |> fun k_body ->
        if k_body != Error 
        then Function (k_param, k_body)
        else Error

    | APP (f, a) ->
        kind f env |> fun kf ->
        kind a env |> fun ka ->
        (match kf with
        | Function (kf_param, kf_ret) ->
            (*parameter kind of f and kind of a must match*)
            if kf_param = ka && ka != Error
            then kf_ret 
            else Error
        | _ -> Error)
    (*| _ -> assert false*)

