open OUnit2;;
open TestUtil;;
open Color;;
open Rank;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"White can move forward by one"
        [(3,3,White,Pawn)] White (3,3) (4,3) [(4,3,White,Pawn)]
    ;

    assert_legal_move
        ~msg:"Black can move forward by one"
        [(3,3,Black,Pawn)] Black (3,3) (2,3) [(2,3,Black,Pawn)]
    ;

    assert_legal_move
        ~msg:"White can move forward by two"
        [(1,3,White,Pawn)] White (1,3) (3,3) [(3,3,White,Pawn)]
    ;

    assert_legal_move
        ~msg:"Black can move forward by two"
        [(6,3,Black,Pawn)] Black (6,3) (4,3) [(4,3,Black,Pawn)]
    ;

    let white_assert = assert_illegal_move [(3,3,White,Pawn)] White (3,3) in
    white_assert ~msg:"can't move laterally" (3,2);
    white_assert ~msg:"can't move backwards" (2,3);
    white_assert ~msg:"can't move two when not in starting position" (5,3);

    let black_assert = assert_illegal_move [(5,3,Black,Pawn)] Black (5,3) in
    black_assert ~msg:"can't move laterally" (5,2);
    black_assert ~msg:"can't move backwards" (6,3);
    black_assert ~msg:"can't move two when not in starting position" (3,3);

    assert_illegal_move
        ~msg:"can't move forward into a piece"
        [(3,3,White,Pawn);(4,3,Black,Knight)] White (3,3) (4,3)
    ;

    assert_illegal_move
        ~msg:"can't move two into a piece"
        [(5,3,Black,Pawn);(3,3,White,Knight)] Black (5,3) (3,3)

let test_capturing _ =
    assert_legal_move
        ~msg:"White can capture left"
        [(3,3,White,Pawn); (4,2,Black,Knight)]
        White (3,3) (4,2)
        [(4,2,White,Pawn)]
    ;

    assert_legal_move
        ~msg:"White can capture right"
        [(3,3,White,Pawn); (4,4,Black,Knight)]
        White (3,3) (4,4)
        [(4,4,White,Pawn)]
    ;

    assert_legal_move
        ~msg:"Black can capture toward White's left"
        [(5,3,Black,Pawn); (4,2,White,Knight)]
        Black (5,3) (4,2)
        [(4,2,Black,Pawn)]
    ;

    assert_legal_move
        ~msg:"Black can capture toward White's right"
        [(5,3,Black,Pawn); (4,4,White,Knight)]
        Black (5,3) (4,4)
        [(4,4,Black,Pawn)]

let suite =
"suite">:::
 [
     "test basic motion">:: test_basic_motion;
     "test capturing">:: test_capturing;
 ]
