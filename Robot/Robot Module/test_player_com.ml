open Globals

module TestPlayerCom = struct
    module Board  = Board.BoardImpl
    module Player = Player_com.PlayerCom (Board)

    (*unit test*)
    let test () =
        let open Board in
        let board = [   mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_o (*9*); mark_x (*10*)] in
        let play = Player.play Board.mark_x in
        Printf.printf("----------------------------------------\n");
        Printf.printf("test PlayerCom...\n");
        board |> fun b -> Board.chg_mark b 0 mark_o
              |> fun b -> play b
              |> fun i -> assert(1 = i); Board.chg_mark b i mark_x
              |> fun b -> Board.chg_mark b 4 mark_o
              |> fun b -> play b
              |> fun i -> assert(8 = i); Board.chg_mark b i mark_x
              |> fun b -> Board.chg_mark b 3 mark_o
              |> fun b -> play b
              |> fun i -> assert(5 = i); Board.chg_mark b i mark_x
              |> fun b -> Board.chg_mark b 6 mark_o
              |> fun b -> assert(mark_o = Board.winner b);
        Printf.printf("test PlayerCom done\n")
end

let _ = TestPlayerCom.test ()
