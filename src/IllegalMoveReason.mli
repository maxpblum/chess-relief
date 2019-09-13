type t =
    | FromEmpty
    | WrongColor
    | CaptureOwn
    | OffBoard
    | IllegalForPiece
    | MovingIntoCheck
    | CastlingOutOfCheck
    | CastlingThroughCheck
    | FailedCondition of PotentialMove.condition_t

val to_string : t -> string
