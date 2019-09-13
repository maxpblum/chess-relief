open OUnit2;;
open TestUtil;;
open Color;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"Can move forward two and right one"
        [(3,3,White,Knight)] White (3,3) (5,4) [(5,4,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move forward two and left one"
        [(3,3,White,Knight)] White (3,3) (5,2) [(5,2,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move backward two and right one"
        [(3,3,White,Knight)] White (3,3) (1,4) [(1,4,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move backward two and left one"
        [(3,3,White,Knight)] White (3,3) (1,2) [(1,2,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move forward one and right two"
        [(3,3,White,Knight)] White (3,3) (4,5) [(4,5,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move forward one and left two"
        [(3,3,White,Knight)] White (3,3) (4,1) [(4,1,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move backward one and right two"
        [(3,3,White,Knight)] White (3,3) (2,5) [(2,5,White,Knight)]
    ;

    assert_legal_move
        ~msg:"Can move backward one and left two"
        [(3,3,White,Knight)] White (3,3) (2,1) [(2,1,White,Knight)]
    ;

    assert_illegal_move
        ~msg:"Cannot move 1x1"
        [(3,3,White,Knight)] White (3,3) (4,4)
    ;

    assert_illegal_move
        ~msg:"Cannot move 2x2"
        [(3,3,White,Knight)] White (3,3) (1,1)
    ;

    assert_legal_move
        ~msg:"Can capture"
        [(3,3,White,Knight);(5,4,Black,Pawn)] White (3,3) (5,4) [(5,4,White,Knight)]

let suite =
"suite">:::
 [
     "test basic motion">:: test_basic_motion;
 ]
