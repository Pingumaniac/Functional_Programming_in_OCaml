open Globals

module TestBoard = struct
    module Board = Board.BoardImpl

    (*unit test*)
    let test () =
        let open Board in
        let board = [   mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_n; mark_n; mark_n;
                        mark_o (*9*); mark_x (*10*)] in
        Printf.printf("----------------------------------------\n");
        Printf.printf("test board...\n");
        assert(mark_x = Board.get_mark board 10);
        assert(mark_n = Board.get_mark board 5);
        assert(mark_o = (board |> fun b -> Board.chg_mark b 5 mark_o
                               |> fun b -> Board.get_mark b 5));
        assert(mark_o = (board |> fun b -> Board.chg_mark b 0 mark_o
                               |> fun b -> Board.chg_mark b 4 mark_o
                               |> fun b -> Board.chg_mark b 8 mark_o
                               |> winner));
        assert(mark_n = (board |> fun b -> Board.chg_mark b 0 mark_o
                               |> fun b -> Board.chg_mark b 1 mark_o
                               |> winner));
        assert(2 =      (board |> fun b -> Board.chg_mark b 0 mark_o
                               |> fun b -> Board.chg_mark b 1 mark_o
                               |> empty_pos));
        assert(9 =      (board |> List.map (fun x -> mark_x)
                               |> empty_pos));
        Printf.printf("test board done\n")
end
let _ = TestBoard.test ()
