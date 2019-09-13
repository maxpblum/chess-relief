open OUnit2;;
open TestUtil;;
open Color;;

let test_basic_motion _ =
    assert_legal_move
        ~msg:"Can move left"
        [(3,3,White,King true)] White (3,3) (3,2) [(3,2,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move right"
        [(3,3,White,King true)] White (3,3) (3,4) [(3,4,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move forward"
        [(3,3,White,King true)] White (3,3) (4,3) [(4,3,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move backward"
        [(3,3,White,King true)] White (3,3) (2,3) [(2,3,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move forward-left"
        [(3,3,White,King true)] White (3,3) (4,2) [(4,2,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move forward-right"
        [(3,3,White,King true)] White (3,3) (4,4) [(4,4,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move backward-left"
        [(3,3,White,King true)] White (3,3) (2,2) [(2,2,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can move backward-right"
        [(3,3,White,King true)] White (3,3) (2,4) [(2,4,White,King true)]
    ;

    assert_legal_move
        ~msg:"Can capture"
        [(3,3,White,King true);(4,4,Black,Pawn)] White (3,3) (4,4) [(4,4,White,King true)]
    ;

    assert_illegal_move
        ~msg:"Cannot move multiple spaces"
        [(3,3,White,King true)] White (3,3) (5,3)
    ;

    assert_legal_move
        ~msg:"Moving a king marks it as moved"
        [(0,4,White,King false)] White (0,4) (0,5) [(0,5,White,King true)]
    ;

    assert_legal_move
        ~msg:"King remains 'clean' when not moved"
        [(0,4,White,King false);(1,0,White,Pawn)]
        White (1,0) (2,0)
        [(0,4,White,King false);(2,0,White,Pawn)]

let test_castling _ =
    assert_legal_move
        ~msg:"King-side castling works for White"
        [(0,4,White,King false);(0,7,White,Rook false)]
        White (0,4) (0,6)
        [(0,5,White,Rook true);(0,6,White,King true)]
    ;

    assert_legal_move
        ~msg:"King-side castling works for Black"
        [(7,4,Black,King false);(7,7,Black,Rook false)]
        Black (7,4) (7,6)
        [(7,5,Black,Rook true);(7,6,Black,King true)]
    ;

    assert_legal_move
        ~msg:"Queen-side castling works for White"
        [(0,4,White,King false);(0,0,White,Rook false)]
        White (0,4) (0,2)
        [(0,3,White,Rook true);(0,2,White,King true)]
    ;

    assert_legal_move
        ~msg:"Queen-side castling works for Black"
        [(7,4,Black,King false);(7,0,Black,Rook false)]
        Black (7,4) (7,2)
        [(7,3,Black,Rook true);(7,2,Black,King true)]
    ;

    assert_illegal_move
        ~msg:"Castling disallowed when Rook destination is occupied"
        [(0,4,White,King false);(0,0,White,Rook false);(0,3,Black,Knight)]
        White (0,4) (0,2)
    ;

    assert_illegal_move
        ~msg:"Castling disallowed when King destination is occupied"
        [(0,4,White,King false);(0,0,White,Rook false);(0,2,Black,Knight)]
        White (0,4) (0,2)
    ;

    assert_illegal_move
        ~msg:"Castling disallowed when a space in between is occupied"
        [(0,4,White,King false);(0,0,White,Rook false);(0,1,Black,Knight)]
        White (0,4) (0,2)
    ;

    assert_illegal_move
        ~msg:"Castling disallowed when King has previously moved"
        [(0,4,White,King true);(0,0,White,Rook false)]
        White (0,4) (0,2)
    ;

    assert_illegal_move
        ~msg:"Castling disallowed when Rook has previously moved"
        [(0,4,White,King false);(0,0,White,Rook true)]
        White (0,4) (0,2)
    ;

    assert_illegal_move
        ~msg:"Castling disallowed if King would cross through check"
        [(0,4,White,King false);(0,0,White,Rook true);(7,3,Black,Queen)]
        White (0,4) (0,2)

let test_check _ =
    assert_illegal_move
        ~msg:"Cannot move into check"
        [(4,3,White,King true);(7,7,Black,Queen)]
        White (4,3) (3,3)
    ;

    assert_illegal_move
        ~msg:"Must move out of check"
        [(3,3,White,King true);(7,7,Black,Queen);(2,2,White,Pawn)]
        White (2,2) (3,2)

let suite =
"suite">:::
 [
     "test basic motion">:: test_basic_motion;
     "test castling">:: test_castling;
     "test check">:: test_check;
 ]
