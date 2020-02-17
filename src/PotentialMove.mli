type special_move_t =
    | NormalMove
    | Castling
    | PawnExchange
    | EnPassant
    | PawnJump

type t = {
    special_move_type : special_move_t;
    delta : Board.delta_t;
    condition : Condition.t;
}

val get_for_piece : Board.piece_t -> t list
