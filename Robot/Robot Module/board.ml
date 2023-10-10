module BoardImpl : Iboard.IBoard = struct
    let mark_n = 0.
    let mark_o = 1.
    let mark_x = 2.

    let get_mark board i = List.nth board i

    let chg_mark board i m = List.mapi (fun idx mark -> if idx = i then m else mark) board

    let print_board board =
      let str m = if      m = mark_o then "o"
                  else if m = mark_x then "x"
                  else                    " " in
      let pm = fun i -> str (get_mark board i) in
      Printf.printf "%s. %s. %s.\n"   (pm 0) (pm 1) (pm 2);
      Printf.printf "%s. %s. %s.\n"   (pm 3) (pm 4) (pm 5);
      Printf.printf "%s. %s. %s.\n\n" (pm 6) (pm 7) (pm 8)

    let winner board =
      let equ3 a b c = (a = b && b = c && a <> mark_n) in
      let gm i = List.nth board i in
      let (b00, b01, b02) = (gm 0, gm 1, gm 2) in
      let (b10, b11, b12) = (gm 3, gm 4, gm 5) in
      let (b20, b21, b22) = (gm 6, gm 7, gm 8) in
      if      equ3 b00 b01 b02 then b00
      else if equ3 b10 b11 b12 then b10
      else if equ3 b20 b21 b22 then b20
      else if equ3 b00 b10 b20 then b00
      else if equ3 b01 b11 b21 then b01
      else if equ3 b02 b12 b22 then b02
      else if equ3 b00 b11 b22 then b00
      else if equ3 b02 b11 b20 then b02
      else mark_n

    let empty_pos board =
      let rec aux idx = function
        | [] -> 9
        | hd :: tl -> if hd = mark_n then idx else aux (idx + 1) tl
      in aux 0 board

    let game_over board =
      (winner board <> mark_n) || (empty_pos board = 9)
  end
