type t =
    | FromEmpty
    | WrongColor
    | CaptureOwn
    | OffBoard
    | IllegalForPiece
    | MovingIntoCheck
    | CastlingOutOfCheck
    | CastlingThroughCheck
    | FailedCondition of Condition.t

val to_string : t -> string
