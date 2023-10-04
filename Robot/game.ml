(*winner of the board if any; otherwise mark_n*)
let winner board =
    (*whether a = b = c*)
    let equ3 a b c = (a = b && b = c && a <> mark_n) in
    (*get_mark with board param provided*)
    let gm = get_mark board in
    (*cache for the 9 positions*)
    let (b00, b01, b02) = (gm 0, gm 1, gm 2) in
    let (b10, b11, b12) = (gm 3, gm 4, gm 5) in
    let (b20, b21, b22) = (gm 6, gm 7, gm 8) in

    (*return the winner if there is one; otherwise mark_n*)
    if      equ3 b00 b01 b02 then b00
    else if equ3 b10 b11 b12 then b10
    else if equ3 b20 b21 b22 then b20

    else if equ3 b00 b10 b20 then b00
    else if equ3 b01 b11 b21 then b01
    else if equ3 b02 b12 b22 then b02

    else if equ3 b00 b11 b22 then b00
    else if equ3 b02 b11 b20 then b02
    else mark_n

(*next mark index of m*)
let next_mark board m =
    (*iter: for each poisition i in [0..8], check if i is mark_n (candidate)
            and if putting m at i will make m the winner
       e.g.)
        Suppose that your board is as below
            | O | O |   |
            | X |   |   |
            | X |   |   |
        if m is O, then iter should return (2,true) as putting O at position 2
        will make O the winner.
        if m is X, then it can return any empty position like (2,false), (4,false), ... (8,false)
        as there are no winning position for X. However, the test case assumes that it will return
        the last empty position that is (8, false).

        If your board is as below
            | O | O | X |
            | X | O | O |
            | O | X | X |
        then, there is no empty position and iter should return (9, false).
    *)
    let rec iter m c i = (*c: (candidate) 9 or an unoccupied board index*)
        (*return (i, true)  if putting m at i makes m the winner
          return (c, false) if no winning position is found*)
        if i < 9 then
            if get_mark board i <> mark_n then
                iter m c (i+1) (*i is taken: check next*)
            else
                if winner (chg_mark board i m) <> mark_n
                then (i, true) (*marking i with m makes m the winner*)
                else iter m i (i+1) (*no winner, but found an empty spot*)
        else
            (c, false) in

    (*the other mark*)
    let n = if m = mark_o then mark_x else mark_o in
    (*get my winning chance, mi will be 9 if no empty position is left*)
    let (mi, mw) = iter m 9 0 in
    (*get other's winning chance, ni will be 9 if no empty position is left*)
    let (ni, nw) = iter n 9 0 in

    if mw      then mi (*if I can win, take the chance*)
    else if nw then ni (*if the other can win, block it*)
    else            mi (*take any empty spot*)

(*read a keyboard input*)
let rec get_key () =
    try
        let s = Graphics.wait_next_event [Graphics.Key_pressed] in
        if s.Graphics.keypressed then
            let k = s.Graphics.key in
            if ('1' <= k && k <= '9') || (k = 'q')
            then k
            else get_key ()
        else get_key ()
    with
        _ -> 'q'

(*play the tic-tac-toe game*)
let rec game b_camera (pose, board) =
    (*draw the board and the robot*)
    Thread.delay 0.05;
    draw b_camera pose board;

    (*mark at index i and let the other mark*)
    let play i =
        (*mark O at position i*)
        let (p, b) = mark b_camera (pose, board) mark_o i in

        (*check if this change makes me the winner*)
        let w = winner b in
        if w = mark_n then (*no winner yet*)
            (*get the computer's next move*)
            let j = next_mark b mark_x in
            if j = 9 then (*no empty place: game over*)
                mark_n
            else (*mark X at index j*)
                let (p', b') = mark b_camera (p, b) mark_x j in
                game b_camera (p', b')
        else (*w is the winner*)
            w in
    (*check if there is any winner*)
    let w = winner board in
    if w = mark_n then (*no winner yet*)
        let k = get_key () in
        if '1' <= k && k <= '9' then
            let i = (Char.code k) - (Char.code '0') - 1 in
            (*mark at position i if it is unoccupied*)
            if get_mark board i = mark_n then
                play i
            else (*position i is already taken*)
                game b_camera (pose, board)
        else if k = 'q' then
            mark_n
        else
            assert false
    else (*w is the winner*)
        w


(*print the game result*)
let print_result m =
    if      m = mark_o then Printf.printf "You win! :)\n"
    else if m = mark_x then Printf.printf "You lose :(\n"
    else                    Printf.printf "No winner\n"

(*unit test*)
let test_game () =
    let board = [ mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_n; mark_n; mark_n;
                  mark_o (*9*); mark_x (*10*)] in
    Printf.printf("----------------------------------------\n");
    Printf.printf("test game...\n");
    board |> fun b -> chg_mark b 0 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> assert(8 = i); chg_mark b i mark_x
          |> fun b -> chg_mark b 4 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> assert(7 = i); chg_mark b i mark_x
          |> fun b -> chg_mark b 6 mark_o
          |> fun b -> next_mark b mark_x
          |> fun i -> assert(2 = i); chg_mark b i mark_x
          |> fun b -> chg_mark b 3 mark_o
          |> fun b -> assert(mark_o = winner b);
    Printf.printf("test game done\n")
let _ = test_game ()
