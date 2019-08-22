type t =
    | FromEmpty
    | WrongColor
    | CaptureOwn
    | OffBoard
    | IllegalForPiece
    | MovingIntoCheck
    | CastlingOutOfCheck
    | CastlingThroughCheck

let to_string = function
    | FromEmpty -> "Trying to move from an empty square."
    | WrongColor -> "Trying to move a piece belonging to the wrong team."
    | CaptureOwn -> "Trying to capture own piece."
    | OffBoard -> "Trying to move to a spot not on the board."
    | IllegalForPiece -> "Not a legal move for the piece."
    | MovingIntoCheck -> "Cannot move into check."
    | CastlingOutOfCheck -> "Cannot castle out of check."
    | CastlingThroughCheck -> "Cannot castle through check."
