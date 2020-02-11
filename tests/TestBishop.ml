open OUnit2;;
open TestUtil;;
open Color;;
open Rank;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"Can move forward-left"
        [(3,3,White,Bishop)] White (3,3) (6,0) [(6,0,White,Bishop)]
    ;

    assert_legal_move
        ~msg:"Can move forward-right"
        [(3,3,White,Bishop)] White (3,3) (6,6) [(6,6,White,Bishop)]
    ;

    assert_legal_move
        ~msg:"Can move backward-left"
        [(3,3,White,Bishop)] White (3,3) (0,0) [(0,0,White,Bishop)]
    ;

    assert_legal_move
        ~msg:"Can move backward-right"
        [(3,3,White,Bishop)] White (3,3) (0,6) [(0,6,White,Bishop)]
    ;

    assert_illegal_move
        ~msg:"Cannot move laterally"
        [(3,3,White,Bishop)] White (3,3) (3,0)
    ;

    assert_illegal_move
        ~msg:"Cannot move vertically"
        [(3,3,White,Bishop)] White (3,3) (6,3)
    ;

    assert_legal_move
        ~msg:"Can capture"
        [(3,3,White,Bishop);(6,6,Black,Pawn false)] White (3,3) (6,6) [(6,6,White,Bishop)]
    ;

    assert_illegal_move
        ~msg:"Cannot move through a piece"
        [(3,3,White,Bishop);(6,6,Black,Pawn false);(7,7,Black,Knight)] White (3,3) (7,7)

let suite =
 [
     "test basic motion">:: test_basic_motion;
 ]
