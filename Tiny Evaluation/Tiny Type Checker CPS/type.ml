#use "parser.ml"

(* print_kind *)
let print_kind knd =
    let rec to_str knd =
        match knd with
        | Boolean -> "Boolean"
        | Number -> "Number"
        | Error -> "Error"
        | Function(k1, k2) ->
            (to_str k1) ^ " -> " ^ (to_str k2)
    in
    Printf.printf "%s\n" (to_str knd)

(*kind of expr in env*)
type env = (string * kind) list

(* lookup function in CPS *)
let rec lookup name env k =
    match env with
    | [] -> k Error  (* No such variable in the environment *)
    | (n, knd)::rest ->
        if name = n then k knd else lookup name rest k

(* kind function in CPS *)
let rec kind expr env k =
    match expr with
    | NUM _ -> k Number
    | BOOL _ -> k Boolean
    | VAR x ->
        lookup x env (fun knd ->
            k knd
        )
    | ADD (_, _) | SUB (_, _) -> k Number
    | EQ (_, _) | GE (_, _) -> k Boolean
    | AND (_, _) | OR (_, _) | NOT _ -> k Boolean
    | IF (_, _, _) -> k Boolean  (* Assuming the IF expression results in a boolean *)
    | FUN ((_, knd), _) -> k knd  (* Assuming the kind of the function is explicitly given *)
    | APP (e1, _) ->
        kind e1 env (fun knd1 ->
            match knd1 with
            | Function (k1, k2) -> k k2
            | _ -> k Error
        )
