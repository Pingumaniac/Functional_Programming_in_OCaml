#use "globals.ml"
#use "parser.ml"

(*kind (type) expression*)
type kexpr
    = KB
    | KN
    | KV of int (*kind variables like k0, k1, k2, ...*)
    | KF of kexpr * kexpr (*functions like ke1 -> ke2*)

(*print kexpr*)
let print kexp =
    let open Printf in
    let rec to_str ke =
        match ke with
        | KB -> "bool"
        | KN -> "num"
        | KV v -> sprintf "k%d" v
        | KF (ke1, ke2) ->
            sprintf "(%s) -> (%s)" (to_str ke1) (to_str ke2) in
    to_str kexp |> printf "%s\n"

(*fresh variable index*)
let newvar () =
    let open StateMonad in
    get       >>= fun i ->
    set (i+1) >>= fun () -> ret (KV i)

(*constr: type constraints for kvar = expr
    kvar: type variable for expr
    expr: expression (kvar = expr)
    env : variable name to kind variable map
    return: list of constraints (kexpr = kexpr)...
*)
let rec constr kvar kexpr env =
    let open StateMonad in

    (*look up the kind expr associated with name from an environment*)
    let rec lookup name env =
        match env with
        | [] -> Printf.printf "%s" name; assert false
        | (n, ke)::rest -> if name = n
                        then ke
                        else lookup name rest in    

    match kexpr with
    | BOOL b -> ret [(kvar, KB)]
    | NUM n  -> ret [(kvar, KN)]
    | VAR v  -> ret [(kvar, lookup v env)]
    | ADD (a, b) | SUB (a, b) ->
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        constr kv1 a env >>= fun c1 ->
        constr kv2 b env >>= fun c2 ->
        (kvar, KN)::(kv1, KN)::(kv2, KN)::c2@c1 |> ret
    | EQ (a, b) | GE (a, b) ->
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        constr kv1 a env >>= fun c1 ->
        constr kv2 b env >>= fun c2 ->
        (kvar, KB)::(kv1, KN)::(kv2, KN)::c2@c1 |> ret
    | AND (a, b) | OR (a, b) -> 
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        constr kv1 a env >>= fun c1 ->
        constr kv2 b env >>= fun c2 ->
        (kvar, KB)::(kv1, KB)::(kv2, KB)::c2@c1 |> ret
    | NOT a -> 
        newvar ()        >>= fun kv1 ->
        constr kv1 a env >>= fun c1 ->
        (kvar, KB)::(kv1, KB)::c1 |> ret
    | IF (c, t, f) -> 
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        newvar ()        >>= fun kv3 ->
        constr kv1 c env >>= fun c1 ->
        constr kv2 t env >>= fun c2 ->
        constr kv3 f env >>= fun c3 ->
        (kvar, kv2)::(kv1, KB)::(kv2, kv3)::c3@c2@c1 |> ret
    | FUN (v, e) -> 
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        (v, kv1)::env    |>
        constr kv2 e     >>= fun c1 ->
        (kvar, KF (kv1, kv2))::c1 |> ret
    | APP (f, a) -> 
        newvar ()        >>= fun kv1 ->
        newvar ()        >>= fun kv2 ->
        constr kv1 f env >>= fun c1 ->
        constr kv2 a env >>= fun c2 ->
        (kv1, KF (kv2, kvar))::c2@c1 |> ret
    | _ -> assert false

(*generate type constraints
*)        
let constraints kv_base expr env =
    let open StateMonad in
    let mon =
        newvar () >>= fun kv ->
        constr kv expr env in
    mon kv_base |> fun (_, cl) -> 
    cl

(*unification algorithm
    clist: constraint list
    return: unifier (a substitution)
*)
let unify clist =
    (*does kexp contain kvar?*)
    let rec contains kvar kexp =
        match kexp with
        | KV _ -> kvar = kexp
        | KF (ke1, ke2) -> contains kvar ke1 || contains kvar ke2
        | _ -> false in
    
    (*substitution that substitutes kvar in ke with kexp*)
    let substitution kvar kexp =
        let rec iter ke =
            match ke with 
            | KB -> KB
            | KN -> KN
            | KV _ -> if kvar = ke then kexp else ke
            | KF (ke1, ke2) -> KF (iter ke1, iter ke2) in
        iter in

    (*return a unifier (substitution) for the constraints in clist*)
    let rec unifier clist = 
        match clist with
        | [] -> fun x -> x (*id substitution: no substitution*)
        | hd::tl ->
            match hd with
            | (KV v, ke) | (ke, KV v) ->
                if contains (KV v) ke
                then assert false (*recursive def is not supported*)
                else
                    let sub_h = substitution (KV v) ke in
                    let sub_t = tl
                            |> List.map (fun (a, b) -> (sub_h a, sub_h b))
                            |> unifier in
                    (*unifier is the composed substitutions*)
                    fun e -> e |> sub_h |> sub_t 

            | (KF (a, b), KF (c, d)) -> (a, c)::(b, d)::tl |> unifier
            
            | (a, b) -> if a = b
                        then unifier tl      (*hd is already unified*)
                        else begin           (*cannot unify: type error*)
                            print a; print b; assert false
                        end in 
    unifier clist
