#load "str.cma"

type tokn
    = T_EOS
    | T_NUM of int
    | T_VAR of string
    | T_IF  | T_LAMBDA | T_AND | T_OR | T_NOT | T_TRUE | T_FALSE
    | T_OPR | T_CPR
    | T_COL | T_ARR
    | T_ADD | T_SUB | T_MUL | T_DIV
    | T_EQ  | T_NE | T_GE | T_GT | T_LE | T_LT
    | T_KBOOL | T_KNUM

let scan str =
    let re_ws  = Str.regexp "[ \n\r\t]+" in
    let re_num = Str.regexp "[0-9]+" in
    let re_sym = Str.regexp "[_a-zA-Z][_a-zA-Z0-9]*" in
    let re_op  = Str.regexp "!=\\|>=\\|<=\\|->\\|." in

    let rec iter pos =
        (*end of string*)
        if pos >= String.length str then
            [T_EOS]

        (*ignore white spaces*)
        else if Str.string_partial_match re_ws str pos then
            Str.match_end () |> iter

        (*number*)
        else if Str.string_partial_match re_num str pos then
            (Str.matched_string str, Str.match_end ()) |> fun (lexeme, next) ->
                T_NUM (int_of_string lexeme) :: iter next

        (*keyword and variable*)
        else if Str.string_partial_match re_sym str pos then
            (Str.matched_string str, Str.match_end ()) |> fun (lexeme, next) ->
                match lexeme with
                | "if"     -> T_IF     :: iter next
                | "lambda" -> T_LAMBDA :: iter next
                | "and"    -> T_AND    :: iter next
                | "or"     -> T_OR     :: iter next
                | "not"    -> T_NOT    :: iter next
                | "true"   -> T_TRUE   :: iter next
                | "false"  -> T_FALSE  :: iter next
                | "num"    -> T_KNUM   :: iter next
                | "bool"   -> T_KBOOL  :: iter next
                | _        -> T_VAR lexeme :: iter next

        (*operator*)
        else if Str.string_partial_match re_op str pos then
            (Str.matched_string str, Str.match_end ()) |> fun (lexeme, next) ->
                match lexeme with
                | "("  -> T_OPR :: iter next
                | ")"  -> T_CPR :: iter next
                | "+"  -> T_ADD :: iter next
                | "-"  -> T_SUB :: iter next
                | "*"  -> T_MUL :: iter next
                | "/"  -> T_DIV :: iter next
                | "="  -> T_EQ  :: iter next
                | "!=" -> T_NE  :: iter next
                | ">=" -> T_GE  :: iter next
                | ">"  -> T_GT  :: iter next
                | "<=" -> T_LE  :: iter next
                | "<"  -> T_LT  :: iter next
                | ":"  -> T_COL :: iter next
                | "->" -> T_ARR :: iter next
                | e    -> Printf.printf "error: [%s]\n" e; assert false
        else
            assert false in
    iter 0
