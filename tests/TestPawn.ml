open OUnit2;;
open TestUtil;;
open Color;;
open Rank;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"White can move forward by one"
        [(3,3,White,Pawn false)] White (3,3) (4,3) [(4,3,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Black can move forward by one"
        [(3,3,Black,Pawn false)] Black (3,3) (2,3) [(2,3,Black,Pawn false)]
    ;

    assert_legal_move
        ~msg:"White can move forward by two"
        [(1,3,White,Pawn false)] White (1,3) (3,3) [(3,3,White,Pawn true)]
    ;

    assert_legal_move
        ~msg:"Black can move forward by two"
        [(6,3,Black,Pawn false)] Black (6,3) (4,3) [(4,3,Black,Pawn true)]
    ;

    let white_assert = assert_illegal_move [(3,3,White,Pawn false)] White (3,3) in
    white_assert ~msg:"can't move laterally" (3,2);
    white_assert ~msg:"can't move backwards" (2,3);
    white_assert ~msg:"can't move two when not in starting position" (5,3);

    let black_assert = assert_illegal_move [(5,3,Black,Pawn false)] Black (5,3) in
    black_assert ~msg:"can't move laterally" (5,2);
    black_assert ~msg:"can't move backwards" (6,3);
    black_assert ~msg:"can't move two when not in starting position" (3,3);

    assert_illegal_move
        ~msg:"can't move forward into a piece"
        [(3,3,White,Pawn false);(4,3,Black,Knight)] White (3,3) (4,3)
    ;

    assert_illegal_move
        ~msg:"can't move two into a piece"
        [(5,3,Black,Pawn false);(3,3,White,Knight)] Black (5,3) (3,3)

let test_capturing _ =
    assert_legal_move
        ~msg:"White can capture left"
        [(3,3,White,Pawn false); (4,2,Black,Knight)]
        White (3,3) (4,2)
        [(4,2,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"White can capture right"
        [(3,3,White,Pawn false); (4,4,Black,Knight)]
        White (3,3) (4,4)
        [(4,4,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Black can capture toward White's left"
        [(5,3,Black,Pawn false); (4,2,White,Knight)]
        Black (5,3) (4,2)
        [(4,2,Black,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Black can capture toward White's right"
        [(5,3,Black,Pawn false); (4,4,White,Knight)]
        Black (5,3) (4,4)
        [(4,4,Black,Pawn false)]

let test_en_passant _ =
    assert_legal_move
        ~msg:"White pawn marked eligible after a jump"
        [(1,1,White,Pawn false)]
        White (1,1) (3,1)
        [(3,1,White,Pawn true)]
    ;

    assert_legal_move
        ~msg:"Black pawn marked eligible after a jump"
        [(6,1,Black,Pawn false)]
        Black (6,1) (4,1)
        [(4,1,Black,Pawn true)]
    ;

    assert_legal_move
        ~msg:"Not marked eligible after one-space move"
        [(1,4,White,Pawn false)]
        White (1,4) (2,4)
        [(2,4,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Not marked eligible after capture"
        [(3,3,White,Pawn false) ; (4,4,Black,Pawn false)]
        White (3,3) (4,4)
        [(4,4,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Eligibility removed after one move"
        [(3,3,White,Pawn true) ; (4,5,Black,Pawn true) ; (7,1,Black,Knight)]
        Black (7,1) (6,3)
        [(3,3,White,Pawn false) ; (4,5,Black,Pawn false) ; (6,3,Black,Knight)]
    ;

    assert_illegal_move
        ~msg:"No en passant capture when target is ineligible"
        [(4,4,White,Pawn false); (4,3,Black,Pawn false)]
        White (4,4) (5,3)
    ;

    assert_legal_move
        ~msg:"White can capture en passant when the target is eligible"
        [(4,4,White,Pawn false); (4,3,Black,Pawn true)]
        White (4,4) (5,3)
        [(5,3,White,Pawn false)]
    ;

    assert_legal_move
        ~msg:"Black can capture en passant when the target is eligible"
        [(3,4,Black,Pawn false); (3,3,White,Pawn true)]
        Black (3,4) (2,3)
        [(2,3,Black,Pawn false)]

let suite =
 [
     "test basic motion">:: test_basic_motion;
     "test capturing">:: test_capturing;
     "test en passant capturing">:: test_en_passant;
 ]
