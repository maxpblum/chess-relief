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

let initial = {board=Board.initial;turn=Color.White}

let king_location (color : Color.t) board =
    let open Rank in
    let is_king Board.{piece={rank}} = match rank with
    | King _ -> true
    | _ -> false
    in
    let open Board in
    match (all_pieces_of_color color board |> List.filter is_king) with
    (* This shouldn't happen *)
    | [] -> None
    | {location} :: _ -> Some location

let delta_matches move potential_move =
    let open Board in
    let open PotentialMove in
    (* To match pawn exchange moves, pretend they are not pawn exchange moves,
     * since move_of_delta always outputs a non-pawn exchange. *)
    ({move with replacement=None} = move_of_delta potential_move.delta move.from)

let rec failed_condition move board cond =
    let open Rank in
    let open Board in
    let open Condition in
    let {from;destination} = move in
    let open PotentialMove in
    let all_true = function
        | first :: cs -> (
            match failed_condition move board first with
            | None -> failed_condition move board (AllOf cs)
            | Some failed -> Some first
        )
        | [] -> None
    in
    let any_true = function
        | first :: cs -> (
            match failed_condition move board first with
            | None -> None
            | Some failed ->
                match failed_condition move board (AnyOf cs) with
                | None -> None
                | Some AllOf fs -> Some (AllOf (failed::fs))
                | _ -> Some failed
        )
        | [] -> Some (AllOf [])
    in
    let wrap_bool b = if b then None else Some cond in
    match cond with
    | True -> None
    | AllOf subconditions -> all_true subconditions
    | AnyOf subconditions -> any_true subconditions
    | NotCapturingOwnPiece -> (
        match get_value_at from board with
        (* This condition is meaningless if there is no origin piece. *)
        | None -> wrap_bool true
        | Some origin_piece ->
        match get_value_at destination board with
        | None -> None
        | Some captured_piece -> wrap_bool (origin_piece.color != captured_piece.color)
    )
    | DestinationOnBoard -> wrap_bool (is_on_board destination)
    | SpaceEmpty delta -> (
        let {destination=space} = move_of_delta delta from in
        wrap_bool (get_value_at space board = None)
    )
    | SpaceOccupied delta -> (
        let {destination=space} = move_of_delta delta from in
        wrap_bool (get_value_at space board != None)
    )
    | StartingRowIs row -> wrap_bool (row = from.row)
    | EnPassantCapturable delta -> (
        let {destination=space} = move_of_delta delta from in (
            match get_value_at space board with
            | Some {rank=Pawn true} -> true
            | _ -> false
        )
        |> wrap_bool
    )
    | NotMovingToEndRow -> wrap_bool (destination.row != 0 && destination.row != 7)
    | MovingToEndRow -> wrap_bool (destination.row == 0 || destination.row == 7)
    | MoveSpecifiesReplacement ->
            wrap_bool (match move.replacement with None -> false | Some _ -> true)
    | SpaceNotThreatened delta -> wrap_bool (
        match get_value_at from board with
        (* Should not happen *)
        | None -> false
        | Some {color} ->
        let {destination=space} = move_of_delta delta from in
        not (is_threatened_by (Color.opposite color) board space)
    )
    | PieceHasNotMoved delta -> wrap_bool (
        let {destination=space} = move_of_delta delta from in (
            match get_value_at space board with
            | Some {rank=Rook false} -> true
            | Some {rank=King false} -> true
            | _ -> false
        )
    )

and attempt_potential_move board from replacement potential_move =
    let open PotentialMove in
    let {condition;mechanics;delta} = potential_move in
    let open Board in
    let move = { (move_of_delta delta from) with replacement=replacement } in
    let open IllegalMoveReason in
    match failed_condition move board condition with
    | Some cond -> NonThreat (FailedCondition cond)
    | None -> Threat (MoveMechanics.realize_move move board mechanics)

and try_potential_moves board from replacement last_illegal_reason
  : PotentialMove.t list -> possible_threat_t
  = function
    | [] -> NonThreat last_illegal_reason
    | pm :: pms -> match attempt_potential_move board from replacement pm with
        | NonThreat i -> try_potential_moves board from replacement i pms
        | threat -> threat

and find_matching_threat board piece move =
    let open IllegalMoveReason in
    let open Board in
    piece |>
    PotentialMove.get_for_piece |>
    List.filter (delta_matches move) |>
    try_potential_moves board move.from move.replacement IllegalForPiece

and piece_threatens_spot board spot Board.{piece;location} =
    let open Board in
    let open Rank in
    (* Ensure pawn exchanges are recognized as valid. *)
    let move = {from=location ; destination=spot ; replacement=Some Queen} in
    match find_matching_threat board piece move with
    | NonThreat _ -> false
    | Threat _ -> true

and is_threatened_by color board spot =
    Board.all_pieces_of_color color board |>
    List.find_opt (piece_threatens_spot board spot) |>
    function
        | None -> false
        | Some _ -> true

let is_in_check color board =
    match king_location color board with
    (* This shouldn't happen, but let's say there's no check without a king *)
    | None -> false
    | Some spot -> is_threatened_by (Color.opposite color) board spot

let attempt_move move state =
    let {board;turn} = state in
    let open IllegalMoveReason in
    let open Board in

    (* Fail if there is no piece at the origin *)
    match get_value_at move.from board with
    | None -> Illegal FromEmpty
    | Some piece ->

    if piece.color != state.turn
    then Illegal WrongColor
    else

    (* Search for a legal move that matches the input *)
    match find_matching_threat board piece move with
    | NonThreat i -> Illegal i
    | Threat new_board ->

    (* Fail if current player would be in check after this move *)
    if is_in_check turn new_board
    then Illegal MovingIntoCheck

    (* The move is legal, return the new state *)
    else Legal {turn=Color.opposite state.turn ; board=new_board}

type game_ended_t =
    | Ongoing
    | Checkmate of Color.t
    | Stalemate

let all_moves state =
    let {board;turn} = state in
    let open Board in
    let open Rank in
    let open PotentialMove in
    all_pieces_of_color turn board
    |> List.map
        (fun {piece;location} ->
            PotentialMove.get_for_piece piece
            |> List.map
                (fun {delta} ->
                    { (move_of_delta delta location)
                      (* Always specify the optional replacement
                       * so that potential pawn-exchange moves
                       * will be recognized as legal. *)
                       with replacement=Some Queen }
                )
        )
    |> List.concat
    |> List.filter
        (fun move ->
            match attempt_move move state with
            | Legal _ -> true
            | Illegal _ -> false
        )

let has_moves state = List.length (all_moves state) > 0

let game_ended state =
    let has_moves = has_moves state in
    let {board;turn} = state in
    if (is_in_check turn board) && (not has_moves) then Checkmate Color.(opposite turn)
    (* TODO: Implement repeated-moves stalemate *)
    else if has_moves then Ongoing
    else Stalemate
