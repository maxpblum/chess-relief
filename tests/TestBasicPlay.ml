open OUnit2;;
open TestUtil;;
open Color;;
open Rank;;

let test_moving_pieces _ =
    assert_legal_move
        ~msg:"Can move a white piece on white's turn"
        [(3,3,White,Pawn)] White (3,3) (4,3) [(4,3,White,Pawn)]
    ;
    assert_illegal_move
        ~msg:"Cannot move a black piece on white's turn"
        [(3,3,Black,Pawn)] White (3,3) (4,3)
    ;
    assert_legal_move
        ~msg:"Can move a black piece on black's turn"
        [(3,3,Black,Pawn)] Black (3,3) (2,3) [(2,3,Black,Pawn)]
    ;
    assert_illegal_move
        ~msg:"Cannot move a white piece on black's turn"
        [(3,3,White,Pawn)] Black (3,3) (4,3)
    ;
    assert_illegal_move
        ~msg:"Cannot move from an empty square"
        [] White (3,3) (4,3)
    ;
    assert_illegal_move
        ~msg:"Cannot move from a square off of the board"
        [(3,3,White,Pawn)] White (9,9) (4,3)
    ;
    assert_illegal_move
        ~msg:"Cannot move to a square off of the board"
        [(7,3,White,Bishop)] White (7,3) (8,4)
    ;
    assert_illegal_move
        ~msg:"Cannot capture own piece"
        [(3,3,White,Bishop);(4,4,White,Bishop)] White (3,3) (4,4)


let suite =
"suite">:::
 [
     "test moving pieces">:: test_moving_pieces;
 ]
