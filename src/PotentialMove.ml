type special_move_t =
    | NormalMove
    | Castling
    | PawnExchange
    | EnPassant

type condition_t =
    | True

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

    (* A particular space be empty, expressed relative to the origin. *)
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
    |> List.map (fun (a,b) -> (a,b,True))
    |> List.map make_normal_move
