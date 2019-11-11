open OUnit2;;
open TestUtil;;
open Color;;
open Rank;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"Can move left"
        [(3,3,White,Rook true)] White (3,3) (3,0) [(3,0,White,Rook true)]
    ;

    assert_legal_move
        ~msg:"Can move right"
        [(3,3,White,Rook true)] White (3,3) (3,6) [(3,6,White,Rook true)]
    ;

    assert_legal_move
        ~msg:"Can move forward"
        [(3,3,White,Rook true)] White (3,3) (6,3) [(6,3,White,Rook true)]
    ;

    assert_legal_move
        ~msg:"Can move backward"
        [(3,3,White,Rook true)] White (3,3) (0,3) [(0,3,White,Rook true)]
    ;

    assert_illegal_move
        ~msg:"Cannot move diagonally"
        [(3,3,White,Rook true)] White (3,3) (6,6)
    ;

    assert_legal_move
        ~msg:"Can capture"
        [(3,3,White,Rook true);(6,3,Black,Pawn)] White (3,3) (6,3) [(6,3,White,Rook true)]
    ;

    assert_illegal_move
        ~msg:"Cannot move through a piece"
        [(3,3,White,Rook true);(6,3,Black,Pawn);(7,3,Black,Knight)] White (3,3) (7,3)
    ;

    assert_legal_move
        ~msg:"Moving a rook marks it as moved"
        [(0,0,White,Rook false)] White (0,0) (0,1) [(0,1,White,Rook true)]
    ;

    assert_legal_move
        ~msg:"Rook remains 'clean' when not moved"
        [(0,0,White,Rook false);(1,0,White,Pawn)]
        White (1,0) (2,0)
        [(0,0,White,Rook false);(2,0,White,Pawn)]


let suite =
"suite">:::
 [
     "test basic motion">:: test_basic_motion;
 ]
