type t = {
    board : Board.t;
    turn : Color.t;
}
type possible_threat_t =
    | NonThreat of IllegalMoveReason.t
    | Threat of Board.t
type attempted_move_t =
    | Illegal of IllegalMoveReason.t
    | Legal   of t
type game_ended_t =
    | Ongoing
    | Checkmate of Color.t
    | Stalemate

val initial : t
val is_in_check : Color.t -> Board.t -> bool
val attempt_move : Board.move_t -> t -> attempted_move_t
val game_ended : t -> game_ended_t
val all_moves : t -> Board.move_t list
