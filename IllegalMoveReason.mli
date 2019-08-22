type t =
    | FromEmpty
    | WrongColor
    | CaptureOwn
    | OffBoard
    | IllegalForPiece
    | MovingIntoCheck
    | CastlingOutOfCheck
    | CastlingThroughCheck

val to_string : t -> string
