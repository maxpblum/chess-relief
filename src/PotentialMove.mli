type t = {
    mechanics : MoveMechanics.t;
    delta : Board.delta_t;
    condition : Condition.t;
}

val get_for_piece : Board.piece_t -> t list
