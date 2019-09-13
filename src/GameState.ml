let spot_blocked spot board = match (Board.get_value_at spot board : Board.space_t) with
| None -> false
| Some _ -> true

let default_move board move = Board.{new_board=Board.make_move move board;move}

let legal_pawn_moves board color from : Board.legal_move_t list =
    let Board.{row; col} = from in
    let move_unit = Color.(if color = White then 1 else -1) in
    let one_ahead_spot=Board.{row=row+move_unit ; col} in
    let two_ahead_spot=Board.{row=row+(2*move_unit) ; col} in
    let one_ahead_blocked = spot_blocked one_ahead_spot board in
    let two_ahead_blocked = spot_blocked two_ahead_spot board in
    let forward_one_move = Board.{from ; destination=one_ahead_spot} in
    let maybe_forward_one =
        if one_ahead_blocked || not (Board.is_on_board one_ahead_spot)
        then []
        else [default_move board forward_one_move] in
    let is_unmoved = ((color=White && row=1) || (color=Black && row=6)) in
    let forward_two_move = Board.{from ; destination=two_ahead_spot} in
    let maybe_forward_two =
        if (
            Board.is_on_board two_ahead_spot &&
            is_unmoved &&
            (not one_ahead_blocked) &&
            (not two_ahead_blocked)
        )
        then [default_move board forward_two_move]
        else [] in
    let capture_left_spot = Board.{row=row+move_unit ; col=col-1} in
    let capture_right_spot = Board.{row=row+move_unit ; col=col+1} in
    let maybe_capture spot =
        if spot_blocked spot board 
        then
            let capture_move = Board.{from;destination=spot} in
            [default_move board capture_move]
        else [] in
    List.concat [
        maybe_forward_one;
        maybe_forward_two;
        maybe_capture capture_left_spot;
        maybe_capture capture_right_spot
    ]

let legal_knight_moves board _ from : Board.legal_move_t list =  (
    [(2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (1, -2); (-1, 2); (-1, -2)]
    |> List.map (fun (rows, cols) -> Board.{rows;cols})
    |> List.map Board.move_of_delta
    |> List.map (fun f -> f from)
    |> List.filter (fun Board.{destination} -> Board.is_on_board destination)
    |> List.map (default_move board)
)

let rec legal_line_moves_impl from direction prev board : Board.legal_move_t list =
    let destination = Board.{row=(prev.row+direction.rows);col=(prev.col+direction.cols)} in
    if not (Board.is_on_board destination) then [] else
    let new_legal_move = Board.{from;destination} |> default_move board in
    if spot_blocked destination board
    then [new_legal_move] else
    new_legal_move :: legal_line_moves_impl from direction destination board

(* Use the unit direction as the first spot to check *)
let legal_line_moves from direction = legal_line_moves_impl from direction from

let delta_of_tuple (rows, cols) = Board.{rows;cols}

let legal_line_moves_multi directions from board = (
    directions |>
    List.map delta_of_tuple |>
    List.map (fun delta -> legal_line_moves from delta board) |>
    List.concat
)

let diagonal_units = [(1, 1); (-1, 1); (1, -1); (-1, -1)]
let straight_units = [(1, 0); (0, 1); (-1, 0); (0, -1)]

let diagonal_moves = legal_line_moves_multi diagonal_units
let straight_moves = legal_line_moves_multi straight_units

let legal_bishop_moves board color from : Board.legal_move_t list =
    diagonal_moves from board

let legal_queen_moves board color from : Board.legal_move_t list =
    List.concat [diagonal_moves from board ; straight_moves from board]

let mark_moved location board =
    let open Board in (
        let new_piece = match get_value_at location board with
        | None -> None
        | Some {color;rank} -> (
            match rank with
            | Rook _ -> Some {color;rank=Rook true}
            | King _ -> Some {color;rank=King true}
            | r -> Some {color;rank=r}
        ) in
        set_location location new_piece board
    )

let legal_rook_moves board color from : Board.legal_move_t list =
    mark_moved from board |> straight_moves from

let basic_king_moves board color from : Board.legal_move_t list=
    let updated_board = mark_moved from board in
    List.concat [diagonal_units ; straight_units] |>
    List.map delta_of_tuple |>
    List.map (fun delta -> Board.move_of_delta delta from) |>
    List.filter (fun Board.{destination} -> Board.is_on_board destination) |>
    List.map (default_move updated_board)

type castling_data_t = {
    rook_col : int ;
    new_rook_col : int ;
    new_king_col : int ;
    between_cols : int list ;
}

let castling_left_data : castling_data_t = {
    rook_col = 0;
    new_rook_col = 3;
    new_king_col = 2;
    between_cols = [1; 2; 3];
}

let castling_right_data : castling_data_t = {
    rook_col = 7;
    new_rook_col = 5;
    new_king_col = 6;
    between_cols = [5; 6];
}

let castle_move board color king_row castling_data : Board.legal_move_t option =
    (* The king always starts in column 4 if unmoved, which it has to be *)
    let king_col = 4 in

    (* Unpack the relative move data for the specific castling variant *)
    let {rook_col;new_rook_col;new_king_col;between_cols} = castling_data in

    (* Check if the rook is clean *)
    let unmoved_rook = Some Board.{rank=Rank.(Rook false);color} in
    let rook_spot = Board.{row=king_row;col=rook_col} in
    let rook_okay = (Board.get_value_at rook_spot board = unmoved_rook) in
    if (not rook_okay) then None else

    (* Check if anything is in the in-between spots *)
    let between_spots = List.map (fun col -> Board.{row=king_row;col}) between_cols in
    let spots_blocked = List.map (fun spot -> spot_blocked spot board) between_spots in
    let spots_clear = (List.filter (fun x -> x) spots_blocked = []) in
    if (not spots_clear) then None else

    (* Calculate the new board state *)
    let open Board in
    let king_origin = {row=king_row;col=king_col} in
    let king_destination = {row=king_row;col=new_king_col} in
    let king_move = {from=king_origin;destination=king_destination} in
    let rook_origin = {row=king_row;col=rook_col} in
    let rook_destination = {row=king_row;col=new_rook_col} in
    let rook_move = {from=rook_origin;destination=rook_destination} in
    let new_board = (
        board |>
        mark_moved king_origin |> mark_moved rook_origin |>
        make_move king_move |> make_move rook_move) in
    Some {move=king_move;new_board}

let castle_moves board color from king_moved : Board.legal_move_t list =
    (* If the king has moved, castling is not allowed *)
    if king_moved then [] else
    let king_row = Board.(from.row) in
    let maybe_castle_left = (
        match castle_move board color king_row castling_left_data with
        | None -> []
        | Some legal_move -> [legal_move]
    ) in
    let maybe_castle_right = (
        match castle_move board color king_row castling_right_data with
        | None -> []
        | Some legal_move -> [legal_move]
    ) in
    List.concat [maybe_castle_left; maybe_castle_right]

let legal_king_moves board color from moved : Board.legal_move_t list =
    List.concat [
        basic_king_moves board color from ;
        castle_moves board color from moved ;
    ]

let legal_moves board ({color; rank} : Board.piece_t) from : Board.legal_move_t list = match rank with
    | Pawn -> legal_pawn_moves board color from
    | Knight -> legal_knight_moves board color from
    | Bishop -> legal_bishop_moves board color from
    | Rook _ -> legal_rook_moves board color from
    | Queen -> legal_queen_moves board color from
    | King moved -> legal_king_moves board color from moved

let new_board_from_move board piece move =
    let moves = legal_moves board piece Board.(move.from) in
    match List.find_opt (fun m -> Board.(m.move) = move) moves with
    | None -> None
    | Some m -> Some m.new_board

type t = {
    board : Board.t;
    turn : Color.t;
}

type complete_move_t = {
    move : Board.move_t;
    new_state : t;
}

type attempted_move_t =
    | Illegal of IllegalMoveReason.t
    | Legal   of complete_move_t

let initial = {board=Board.initial;turn=White}

let blocked_by_color board match_color spot =
    match Board.get_value_at spot board with
    | None -> false
    | Some Board.{color;rank} -> color = match_color

let legal_moves_for_color color (board : Board.t) : Board.legal_move_t list =
    Board.all_pieces_of_color color board |>
    List.map (fun Board.{piece;location=from} -> legal_moves board piece from) |>
    List.concat

let threats_to_spot_from_color color board spot =
    legal_moves_for_color color board |>
    List.filter (fun Board.{move={destination}} -> spot=destination)

let king_location (color : Color.t) board =
    let is_king Board.{piece={rank}} = match rank with
    | King _ -> true
    | _ -> false
    in
    match (Board.all_pieces_of_color color board |> List.filter is_king) with
    (* This shouldn't happen *)
    | [] -> None
    | {location} :: _ -> Some location

let is_threatened_by_color board spot color =
    match threats_to_spot_from_color color board spot with
    | [] -> false
    | _  -> true

let is_in_check color board =
    match king_location color board with
    (* This shouldn't happen, but let's say there's no check without a king *)
    | None -> false
    | Some spot -> is_threatened_by_color board spot Color.(opposite color)

let attempt_move move {board;turn} =
    match Board.get_value_at Board.(move.from) board with
    | None -> Illegal FromEmpty
    | Some piece ->
            let ({color=from_color;rank=from_rank} : Board.piece_t) = piece in
            if from_color != turn
            then Illegal WrongColor
            else if not (Board.is_on_board move.destination)
            then Illegal OffBoard
            else if blocked_by_color board from_color move.destination
            then Illegal CaptureOwn
            else
            let is_castling = (
                from_rank = Rank.(King false) &&
                abs(Board.(move.destination.col - move.from.col)) > 1
            ) in
            if is_castling && is_in_check from_color board
            then Illegal CastlingOutOfCheck
            else if is_castling && (
                let king_col = 4 in
                let {new_king_col} =
                    if Board.(move.destination.col) > 4
                    then castling_right_data
                    else castling_left_data in
                let between_col = (king_col + new_king_col) / 2 in
                let between_spot = Board.{row=move.from.row;col=between_col} in
                let other_color = Color.(opposite from_color) in
                is_threatened_by_color board between_spot other_color
            )
            then Illegal CastlingThroughCheck
            else match new_board_from_move board piece move with
            | None -> Illegal IllegalForPiece
            | Some board ->
                    if is_in_check turn board
                    then Illegal MovingIntoCheck
                    else Legal {move;new_state={board;turn=Color.opposite turn}}

type game_ended_t =
    | Ongoing
    | Checkmate of Color.t
    | Stalemate

let has_actual_legal_moves board turn =
    let possible_moves = legal_moves_for_color turn board in
    let move_actually_legal Board.{move} = match attempt_move move {board;turn} with
    | Legal _ -> true
    | _ -> false in
    List.length (List.map move_actually_legal possible_moves) > 0

let game_ended {board;turn} =
    if is_in_check turn board then Checkmate Color.(opposite turn)
    (* TODO: Implement repeated-moves stalemate *)
    else if has_actual_legal_moves board turn then Ongoing
    else Stalemate
