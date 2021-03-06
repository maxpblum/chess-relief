type t = {
    mechanics: MoveMechanics.t;
    delta : Board.delta_t;
    condition : Condition.t;
}

let apply_universal_conditions move =
    let open Condition in
    {move with condition=(AllOf [
        DestinationOnBoard;
        NotCapturingOwnPiece;
        move.condition;
    ])}

(* Convenient constructor for making a "normal" PotentialMove from shorthand. *)
let make_normal_move (rows,cols,condition) =
    let open Board in {
        mechanics = MoveMechanics.NormalMove;
        delta={rows;cols};
        condition;
    } |> apply_universal_conditions

let knight_moves = [1,2 ; 1,-2 ; -1,2 ; -1,-2 ; 2,1 ; 2,-1 ; -2,1 ; -2,-1]
    |> List.map (fun (a,b) -> (a,b,Condition.True))
    |> List.map make_normal_move

(* range 5 10 => [5;6;7;8;9] *)
let rec range start stop =
    if start = stop
    then []
    else start :: range (start+1) stop

type intermediate_distances_t = int list
type distance_set_t = int * intermediate_distances_t
type intermediates_t = (int * int) list
type delta_set_t = (int * int) * intermediates_t
type deltas_of_direction_t = delta_set_t list

(*
 * A list of partial-delta sets representing non-zero distances along a single
 * direction, each paired with all of the distances in between, to be turned
 * into full-delta sets in various ways to represent all straight and diagonal
 * moves.
 *)
let distances : distance_set_t list = [
    1 , [] ;
    2 , [ 1 ];
    3 , [ 1 ; 2 ];
    4 , [ 1 ; 2 ; 3 ];
    5 , [ 1 ; 2 ; 3 ; 4 ];
    6 , [ 1 ; 2 ; 3 ; 4 ; 5 ];
    7 , [ 1 ; 2 ; 3 ; 4 ; 5 ; 6 ];
]

let distance_as_rows = fun d -> d,0
let distance_as_both = fun d -> d,d
let distance_as_pos_row_neg_col = fun d -> d,-d

(*
 * Apply a simple delta-modifier to both the destination delta and the
 * between-space deltas of a delta set.
 *)
let modify_delta_set modifier (delta, pairs) =
    (modifier delta), (List.map modifier pairs)

(*
 * Apply a simple delta-modifier to the destinations and between-space deltas of
 * a list of delta sets.
 *)
let modify_delta_sets modifier = List.map (modify_delta_set modifier)

let reverse_row (a,b) = -a,b
let reverse_col (a,b) = a,-b
let switch_coords (a,b) = b,a

let reverse_rows = modify_delta_sets reverse_row
let reverse_cols = modify_delta_sets reverse_col
let switch_all_coords = modify_delta_sets switch_coords

(*
 * Various lists of "delta set"s, each containing a "ray" of delta sets in one
 * direction from the origin. Each delta set represents a possible move in that
 * direction.
 *)
let forward : delta_set_t list = distances |> modify_delta_sets distance_as_rows
let backward : delta_set_t list = forward |> reverse_rows
let right : delta_set_t list = forward |> switch_all_coords
let left : delta_set_t list = right |> reverse_cols

let forward_right : delta_set_t list = distances |> modify_delta_sets distance_as_both
let back_left : delta_set_t list = forward_right |> reverse_rows |> reverse_cols
let forward_left : delta_set_t list = distances |> modify_delta_sets distance_as_pos_row_neg_col
let back_right : delta_set_t list = forward_left |> reverse_rows |> reverse_cols

(*
 * Take a delta (representing a destination relative to the origin) and a list
 * of intermediate deltas (representing the spaces in between) and return a move
 * that requires all intermediate spaces to be empty.
 *)
let move_of_delta_set ((rows,cols),intermediates) =
    let open Board in
    let open Condition in
    let empty_intermediate_conditions =
        intermediates |>
        List.map (fun (rows,cols) -> SpaceEmpty {rows;cols})
    in {
        mechanics = MoveMechanics.NormalMove ;
        delta={rows;cols} ;
        condition = AllOf empty_intermediate_conditions ;
    } |> apply_universal_conditions

(*
 * Take a list of tuples each containing a delta (representing destination) and
 * a list of deltas (representing spaces in between) and return a list of
 * PotentialMoves, each of which represents moving to that destination and
 * requires the spaces in between to be empty.
 *)
let moves_of_delta_sets = List.map move_of_delta_set

let straight_moves : t list =
    [forward; backward; right; left] |>
    List.map moves_of_delta_sets |>
    List.concat

let diagonal_moves : t list =
    [forward_right; forward_left; back_right; back_left] |>
    List.map moves_of_delta_sets |>
    List.concat

let bishop_moves : t list = diagonal_moves
let queen_moves : t list = List.concat [diagonal_moves ; straight_moves]
let rook_moves : t list = straight_moves

let require_condition cond pm = Condition.{
    pm with condition = AllOf [ pm.condition ; cond ]
}

let make_pawn_moves direction starting_row : t list =
    let open Board in
    let open Condition in
    let next_square_empty = SpaceEmpty {rows=direction;cols=0} in
    let second_square_empty = SpaceEmpty {rows=2*direction ; cols=0} in
    let simple_moves = (
        [
            (direction,        0, next_square_empty) ;
            (direction,     (-1), SpaceOccupied {rows=direction;cols=(-1)}) ;
            (direction,        1, SpaceOccupied {rows=direction;cols=1}) ;
        ] |>
        List.map make_normal_move
    ) in
    let non_exchange_simple_moves =
        simple_moves |> List.map (require_condition NotMovingToEndRow) in
    let exchange_moves =
        simple_moves |>
        List.map (require_condition (AllOf [MovingToEndRow ; MoveSpecifiesReplacement])) |>
        List.map (fun pm -> {
            pm with mechanics = MoveMechanics.PawnExchange
        }) in
    let jump = {
        (make_normal_move (
            2 * direction, 0, AllOf [
                next_square_empty;
                second_square_empty;
                StartingRowIs starting_row;
        ])) with mechanics = MoveMechanics.PawnJump
    } in
    let make_en_passant col_direction =
        let diagonal_normal_move =
            make_normal_move (direction, col_direction, True) in
        {
            diagonal_normal_move
            with
            mechanics=MoveMechanics.EnPassant ;
            condition=AllOf [
                diagonal_normal_move.condition ;
                (* There must be a just-jumped pawn in the same row
                 * that the current pawn started in, in the column
                 * that the current pawn is moving to. *)
                EnPassantCapturable {rows=0 ; cols=col_direction}
            ]
        }
    in
    List.concat [non_exchange_simple_moves ; exchange_moves ; [jump ; make_en_passant 1 ; make_en_passant (-1)]]

let white_pawn_moves : t list = make_pawn_moves 1    1
let black_pawn_moves : t list = make_pawn_moves (-1) 6

let basic_king_moves = [
    (1,0) ;
    (0,1) ;
    ((-1),0) ;
    (0,(-1)) ;
    (1,1) ;
    (1,(-1)) ;
    ((-1),1) ;
    ((-1),(-1)) ;
] |> List.map (
    fun (rows,cols) ->
        make_normal_move (rows,cols,Condition.True)
)

let king_side_castle =
    let open Board in
    let open Condition in
    {(make_normal_move (0,2,AllOf [
        SpaceEmpty {rows=0;cols=1} ;
        SpaceEmpty {rows=0;cols=2} ;
        SpaceNotThreatened {rows=0;cols=1} ;
        PieceHasNotMoved {rows=0;cols=0} ;
        PieceHasNotMoved {rows=0;cols=3} ;
    ])) with mechanics=MoveMechanics.Castling}

let queen_side_castle =
    let open Board in
    let open Condition in
    {(make_normal_move (0,(-2),AllOf [
        SpaceEmpty {rows=0;cols=(-1)} ;
        SpaceEmpty {rows=0;cols=(-2)} ;
        SpaceEmpty {rows=0;cols=(-3)} ;
        SpaceNotThreatened {rows=0;cols=(-1)} ;
        PieceHasNotMoved {rows=0;cols=0} ;
        PieceHasNotMoved {rows=0;cols=(-4)} ;
    ])) with mechanics=MoveMechanics.Castling}

let king_moves = List.concat [basic_king_moves ; [king_side_castle ; queen_side_castle]]

let get_for_piece : Board.piece_t -> t list =
    let open Color in
    let open Board in
    let open Rank in function
    | {rank=Pawn _;color=White} -> white_pawn_moves
    | {rank=Pawn _;color=Black} -> black_pawn_moves
    | {rank=Bishop} -> bishop_moves
    | {rank=Queen} -> queen_moves
    | {rank=Rook _} -> rook_moves
    | {rank=Knight} -> knight_moves
    | {rank=King _} -> king_moves
