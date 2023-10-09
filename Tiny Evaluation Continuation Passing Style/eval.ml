#use "parser.ml"

(* print expr *)
let print expr =
    let rec to_str e k =
        match e with
        | NUM n  -> k (string_of_int n)
        | BOOL b -> k (string_of_bool b)
        | VAR v  -> k v
        | ADD (e1, e2) ->
            to_str e1 (fun str1 ->
            to_str e2 (fun str2 ->
            k ("(" ^ str1 ^ " + " ^ str2 ^ ")")))
        (* Add other cases below *)
        | SUB (e1, e2) ->
            to_str e1 (fun str1 ->
            to_str e2 (fun str2 ->
            k ("(" ^ str1 ^ " - " ^ str2 ^ ")")))
        (* Implement other patterns similarly *)
        | _ -> k "NotImplemented"
    in
    to_str expr (Printf.printf "%s\n")

(* evaluate expr in env *)
let rec eval expr env k =
    let dropNUM  = function NUM  n -> n | e -> print e; assert false in
    let dropBOOL = function BOOL b -> b | e -> print e; assert false in
    let dropCLO  = function CLO (v, e, ev) -> (v, e, ev) | e -> print e; assert false in

    let rec lookup name env =
        match env with
        | [] -> print (VAR name); assert false
        | (n, e)::rest -> if name = n then e else lookup name rest
    in

    match expr with
    | NUM n  -> k (NUM n)
    | BOOL b -> k (BOOL b)
    | VAR v  -> k (lookup v env)
    | ADD (e1, e2) ->
        eval e1 env (fun v1 ->
        eval e2 env (fun v2 ->
        k (NUM (dropNUM v1 + dropNUM v2))))
    | SUB (e1, e2) ->
        eval e1 env (fun v1 ->
        eval e2 env (fun v2 ->
        k (NUM (dropNUM v1 - dropNUM v2))))
    (* Add more patterns like AND, OR, NOT, etc. *)
    | AND (e1, e2) ->
        eval e1 env (fun v1 ->
        eval e2 env (fun v2 ->
        k (BOOL (dropBOOL v1 && dropBOOL v2))))
    | OR (e1, e2) ->
        eval e1 env (fun v1 ->
        eval e2 env (fun v2 ->
        k (BOOL (dropBOOL v1 || dropBOOL v2))))
    | NOT e1 ->
        eval e1 env (fun v1 ->
        k (BOOL (not (dropBOOL v1))))
    | CLO (v, e, ev) ->
        let (v, e, captured_env) = dropCLO expr in
        (* Extend environment with closure *)
        let new_env = (v, e)::captured_env in
        (* Evaluate the body in the new environment *)
        eval e new_env k
    (* Add more patterns *)
    | _ -> k (BOOL false)
