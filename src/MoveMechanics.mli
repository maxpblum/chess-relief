type t =
    | NormalMove
    | Castling
    | PawnExchange
    | EnPassant
    | PawnJump

val realize_move : Board.move_t -> Board.t -> t -> Board.t
