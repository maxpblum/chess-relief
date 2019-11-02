type special_move_t =
    | NormalMove
    | Castling
    | PawnExchange
    | EnPassant

type condition_t =
    | TrueCondition

    (* All of the subconditions must be met. *)
    | AllOf of condition_t list

    (* Any one of the subconditions must be met. *)
    | AnyOf of condition_t list

    (* The origin must contain a piece belonging to the player. *)
    | MovingOwnPiece

    (* The destination must not contain a piece of the player's color. *)
    | NotCapturingOwnPiece

    (* The destination must be on the board. *)
    | DestinationOnBoard

    (* A particular space must be empty, expressed relative to the origin. *)
    | SpaceEmpty of Board.delta_t

type t = {
    special_move_type : special_move_t;
    delta : Board.delta_t;
    condition : condition_t;
}

let apply_universal_conditions move =
    {move with condition=(AllOf [
        MovingOwnPiece;
        DestinationOnBoard;
        NotCapturingOwnPiece;
        move.condition;
    ])}

let make_normal_move (rows,cols,condition) = {
    special_move_type = NormalMove;
    delta={rows;cols};
    condition;
} |> apply_universal_conditions

let knight_moves = [1,2 ; 1,-2 ; -1,2 ; -1,-2 ; 2,1 ; 2,-1 ; -2,1 ; -2,-1]
    |> List.map (fun (a,b) -> (a,b,TrueCondition))
    |> List.map make_normal_move

let rec range start stop =
    if start = stop
    then []
    else start :: range (start+1) stop

type intermediates_t = (int * int) list
type delta_set_t = (int * int) * intermediates_t
type deltas_of_direction_t = delta_set_t list

let nonzero_distances = [
    1 , [ 0 ] ;
    2 , [ 0 ; 1 ];
    3 , [ 0 ; 1 ; 2 ];
    4 , [ 0 ; 1 ; 2 ; 3 ];
    5 , [ 0 ; 1 ; 2 ; 3 ; 4 ];
    6 , [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ];
    7 , [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ];
]

let distances = (0,[])::nonzero_distances

let distance_as_rows = fun d -> d,0
let distance_as_both = fun d -> d,d
let distance_as_pos_row_neg_col = fun d -> d,-d

let modify_delta_sets modifier delta_sets =
    delta_sets
    |> List.map (
        fun (delta, pairs) ->
            (modifier delta), (List.map modifier pairs)
    )

let reverse_row (a,b) = -a,b
let reverse_col (a,b) = a,-b
let switch_coords (a,b) = b,a

let reverse_rows = modify_delta_sets reverse_row
let reverse_cols = modify_delta_sets reverse_col
let switch_all_coords = modify_delta_sets switch_coords

let forward = distances |> modify_delta_sets distance_as_rows
let backward = forward |> reverse_rows
let right = forward |> switch_all_coords
let left = right |> reverse_cols

let forward_right = nonzero_distances |> modify_delta_sets distance_as_both
let back_left = forward_right |> reverse_rows |> reverse_cols
let forward_left = nonzero_distances |> modify_delta_sets distance_as_pos_row_neg_col
let back_right = forward_left |> reverse_rows |> reverse_cols

let moves_of_delta_sets = List.map (fun ((rows,cols),intermediates) ->
    let empty_intermediate_conditions =
        intermediates |>
        List.map (fun (rows,cols) -> SpaceEmpty {rows;cols})
    in {
        special_move_type = NormalMove ;
        delta={rows;cols} ;
        condition = AllOf empty_intermediate_conditions ;
    } |> apply_universal_conditions
)

let straight_moves =
    [forward; backward; right; left] |>
    List.map moves_of_delta_sets |>
    List.concat

let diagonal_moves =
    [forward_right; forward_left; back_right; back_left] |>
    List.map moves_of_delta_sets |>
    List.concat
