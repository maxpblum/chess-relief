type t =
    | NormalMove
    | Castling
    | PawnExchange
    | EnPassant
    | PawnJump

let mark_rook_or_king_moved location board =
    let open Board in
    let open Rank in
    match get_value_at location board with
    | None -> board
    | Some piece -> match piece with
    | {color;rank=King false} ->
            set_location location (Some {color;rank=King true}) board
    | {color;rank=Rook false} ->
            set_location location (Some {color;rank=Rook true}) board
    | _ -> board

let realize_move move board =
    let open Board in
    let open Rank in
    let king_and_rook_marked_board = mark_rook_or_king_moved move.from board in
    let mark_false_if_pawn board (space:space_t) location = match space with
    | Some {rank=Pawn true;color} -> set_location location (Some {rank=Pawn false;color}) board
    | _ -> board
    in
    (* Use the board fold function to iterate through the spaces on the existing
     * board, building up a newly marked version of the board. We pass in the same
     * board argument twice since fold doesn't know we're using the board as our
     * accumulator. *)
    let pawn_marked_board =
            Board.fold
                mark_false_if_pawn
                king_and_rook_marked_board
                king_and_rook_marked_board
    in
    function
    | NormalMove -> make_move move pawn_marked_board
    | EnPassant -> (
        let captured_pawn_row =
            (* Capturing a black pawn that just jumped to row four. *)
            if move.destination.row >= 4 then 4
            (* Capturing a white pawn that just jumped to row three. *)
            else 3
        in
        let captured_pawn_spot = {
            row=captured_pawn_row ;
            col=move.destination.col ;
        } in
        pawn_marked_board |>
        make_move move |>
        set_location captured_pawn_spot None
    )
    | PawnJump -> (
        match (pawn_marked_board |> get_value_at move.from) with
        (* Should not be possible *)
        | None -> Board.initial
        | Some {color} ->
            make_move move pawn_marked_board |>
            set_location move.destination (Some {color;rank=Pawn true})
    )
    | Castling -> (
        let row = move.from.row in
        let rook_from_column = if move.destination.col < 3 then 0 else 7 in
        let rook_to_column = if rook_from_column < 3 then 3 else 5 in
        let rook_origin = {row;col=rook_from_column} in
        let rook_destination = {row;col=rook_to_column} in
        pawn_marked_board |>
        make_move move |>
        make_move {null_move with from=rook_origin ; destination=rook_destination} |>
        mark_rook_or_king_moved rook_destination
    )
    | PawnExchange -> (
        match (pawn_marked_board |> get_value_at move.from) with
        (* Should not be possible *)
        | None -> Board.initial
        | Some {color} ->
        match move.replacement with
        (* Should not be possible *)
        | None -> Board.initial
        | Some rank ->
            make_move move pawn_marked_board |>
            set_location move.destination (Some {color;rank})
    )
