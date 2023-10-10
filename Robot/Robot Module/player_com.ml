open Globals

module PlayerCom (Board: Iboard.IBoard) : Iplayer.IPlayer = struct

    let play mrk board =
        let other m =
            if m = Board.mark_o then Board.mark_x else Board.mark_o
        in

        let can_win m i =
            let temp_board = Board.copy board in
            Board.set_mark temp_board i m;
            Board.check_winner temp_board = Some m
        in

        let rec iter m i =
            if i > 8 then 9
            else if Board.get_mark board i = Board.mark_n && can_win m i then i
            else iter m (i + 1)
        in

        let i = iter mrk 0 in
        if i <> 9 then i
        else
            let i = iter (other mrk) 0 in
            if i <> 9 then i
            else
            Board.empty_pos board
end
