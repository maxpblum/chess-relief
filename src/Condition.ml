type t =
    | True

    (* All of the subconditions must be met. *)
    | AllOf of t list

    (* Any one of the subconditions must be met. *)
    | AnyOf of t list

    (* The destination must not contain a piece of the origin's color. *)
    | NotCapturingOwnPiece

    (* The destination must be on the board. *)
    | DestinationOnBoard

    (* A particular space must be empty, expressed relative to the origin. *)
    | SpaceEmpty of Board.delta_t

    (* A particular space must not be empty, expressed, relative to the origin. *)
    | SpaceOccupied of Board.delta_t

    (* The piece must be moving from a specific row. *)
    | StartingRowIs of int

    (* The space expressed relative to origin must contain a pawn that just jumped there. *)
    | EnPassantCapturable of Board.delta_t

    (* The destination is not in row 0 or 7. *)
    | NotMovingToEndRow

    (* The destination is in row 0 or 7. *)
    | MovingToEndRow

    (* The move object must specify a replacement rank (for pawn exchange). *)
    | MoveSpecifiesReplacement

    (* The space expressed relative to origin must not be threatened by the
     * other team (the team for which we are not currently evaluating a
     * potential move. *)
    | SpaceNotThreatened of Board.delta_t

    (* The space expressed relative to origin must contain a king or rook that
     * has not moved yet during this game. *)
    | PieceHasNotMoved of Board.delta_t

