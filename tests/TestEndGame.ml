open OUnit2;;
open GameState;;
open Rank;;
open Color;;

let string_of_game_ended : game_ended_t -> string = function
    | Ongoing -> "Ongoing"
    | Checkmate White -> "Checkmate White"
    | Checkmate Black -> "Checkmate Black"
    | Stalemate -> "Stalemate"

let assert_endgame_state ~msg pieces turn expected =
    let board = Board.(pieces |> List.map occupied_space_of_shorthand |> create) in
    let full_msg = String.concat "" [
        msg; "\n";
        "Board: "; "\n";
        IO.string_of_board false board;
        "Turn: ";
        Color.to_string turn
    ] in
    assert_equal
        ~msg:full_msg
        ~printer:string_of_game_ended
        expected
        (game_ended {board;turn})

let test_checkmate _ =
    assert_endgame_state
        ~msg:"White victory is recognized"
        [(0,0,Black,King true); (7,0,White,Queen); (7,1,White,Queen)]
        Black
        (Checkmate White)
    ;

    assert_endgame_state
        ~msg:"Black victory is recognized"
        [(0,0,White,King true); (7,0,Black,Queen); (7,1,Black,Queen)]
        White
        (Checkmate Black)

let test_ongoing _ =
    assert_endgame_state
        ~msg:"Ongoing when no check"
        [(0,0,White,King true)]
        White
        Ongoing

let test_stalemate _ =
    assert_endgame_state
        ~msg:"Stalemate when no moves and no check"
        [(0,0,White,King true);(1,2,Black,Pawn false);(2,2,Black,Pawn false);(2,1,Black,Pawn false)]
        White
        Stalemate

let suite =
 [
     "test checkmate">:: test_checkmate;
     "test ongoing">:: test_ongoing;
     "test stalemate">:: test_stalemate;
 ]
;;
