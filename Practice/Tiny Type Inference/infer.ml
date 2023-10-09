#use "globals.ml"
#use "parser.ml"

type kexpr
    = KB
    | KN
    | KV of int
    | KF of kexpr * kexpr

let print kexp =
    let open Printf in
    let rec to_str ke =
        match ke with
        | KB -> "bool"
        | KN -> "num"
        | KV v -> sprintf "k%d" v
        | KF (ke1, ke2) -> sprintf "(%s) -> (%s)" (to_str ke1) (to_str ke2)
    in
    to_str kexp |> printf "%s\n"

let newvar =
    let counter = ref 0 in
    fun () -> incr counter; KV !counter

let rec constr kvar expr env =
    let rec lookup name env =
        match env with
        | [] -> assert false
        | (n, ke)::rest -> if name = n then ke else lookup name rest
    in
    match expr with
    | BOOL _ -> [(kvar, KB)]
    | NUM  _ -> [(kvar, KN)]
    | VAR v  -> [(kvar, lookup v env)]
    | ADD (a, b) | SUB (a, b) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        (kvar, KN)::(kv1, KN)::(kv2, KN)::(constr kv1 a env)@(constr kv2 b env)
    | EQ (a, b) | GE (a, b) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        (kvar, KB)::(kv1, KN)::(kv2, KN)::(constr kv1 a env)@(constr kv2 b env)
    | AND (a, b) | OR (a, b) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        (kvar, KB)::(kv1, KB)::(kv2, KB)::(constr kv1 a env)@(constr kv2 b env)
    | NOT a ->
        let kv1 = newvar () in
        (kvar, KB)::(kv1, KB)::(constr kv1 a env)
    | IF (c, t, f) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        let kv3 = newvar () in
        (kvar, kv3)::(kv1, KB)::(kv2, kv3)::(kv3, kv3)::(constr kv1 c env)@(constr kv2 t env)@(constr kv3 f env)
    | FUN (v, e) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        (kvar, KF (kv1, kv2))::(constr kv2 e ((v, kv1)::env))
    | APP (f, a) ->
        let kv1 = newvar () in
        let kv2 = newvar () in
        (kvar, kv2)::(kv1, KF (kv2, kvar))::(constr kv1 f env)@(constr kv2 a env)
    | _ -> assert false

let unify clist =
    let rec contains kvar kexp =
        match kexp with
        | KV _ -> kvar = kexp
        | KF (ke1, ke2) -> contains kvar ke1 || contains kvar ke2
        | _ -> false
    in
    let substitution kvar kexp =
        let rec iter ke =
            match ke with
            | KB -> KB
            | KN -> KN
            | KV _ -> if kvar = ke then kexp else ke
            | KF (ke1, ke2) -> KF (iter ke1, iter ke2)
        in
        iter
    in
    let rec unifier clist =
        match clist with
        | [] -> fun x -> x
        | hd::tl ->
            match hd with
            | (KV v, ke) | (ke, KV v) ->
                if contains (KV v) ke then assert false
                else
                    let sub_h = substitution (KV v) ke in
                    let sub_t = List.map (fun (a, b) -> (sub_h a, sub_h b)) tl |> unifier in
                    fun e -> e |> sub_h |> sub_t
            | (KF (a, b), KF (c, d)) ->
                let sub = unifier [(a, c); (b, d)] in
                let sub_t = List.map (fun (x, y) -> (sub x, sub y)) tl |> unifier in
                fun e -> e |> sub |> sub_t
            | (a, b) -> if a = b then unifier tl else assert false
    in
    unifier clist
