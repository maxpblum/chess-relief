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

let rec failed_condition move state cond =
    let Board.{from;destination} = move in
    let {board;turn} = state in
    let open PotentialMove in
    let all_true = function
        | first :: cs -> (
            match failed_condition move state first with
            | None -> failed_condition move state (AllOf cs)
            | Some failed -> Some first
        )
        | [] -> None
    in
    let any_true = function
        | first :: cs -> (
            match failed_condition move state first with
            | None -> None
            | Some failed ->
                match failed_condition move state (AnyOf cs) with
                | None -> None
                | Some AllOf fs -> Some (AllOf (failed::fs))
                | _ -> Some failed
        )
        | [] -> Some (AllOf [])
    in
    let wrap_bool b = if b then None else Some cond in
    let open Board in
    match cond with
    | TrueCondition -> None
    | AllOf subconditions -> all_true subconditions
    | AnyOf subconditions -> any_true subconditions
    | MovingOwnPiece -> (match get_value_at from board with
        | None -> Some cond
        | Some {color} -> wrap_bool (turn=color)
    )
    | NotCapturingOwnPiece -> (match get_value_at destination board with
        | None -> None
        | Some {color} -> wrap_bool (turn!=color)
    )
    | DestinationOnBoard -> wrap_bool (is_on_board destination)
    | SpaceEmpty delta -> (
        let {destination=space} = move_of_delta delta from in
        wrap_bool (get_value_at space board = None)
    )

let realize_potential_move move board =
    let marked_board = (
        let open Board in
        let open Rank in
        match Board.get_value_at move.from board with
        | None -> board
        | Some piece -> match piece with
        | {color;rank=King false} ->
                Board.set_location move.from (Some {color;rank=King true}) board
        | {color;rank=Rook false} ->
                Board.set_location move.from (Some {color;rank=Rook true}) board
        | _ -> board
    )
    in
    let open PotentialMove in
    function
    | NormalMove -> Board.make_move move marked_board
    | _ -> Board.initial

let attempt_potential_move state from potential_move =
    let {turn;board} = state in
    let open PotentialMove in
    let {condition;special_move_type;delta} = potential_move in
    let open Board in
    let move = move_of_delta delta from in
    let open IllegalMoveReason in
    match failed_condition move state condition with
    | Some cond -> Illegal (FailedCondition cond)
    | None -> Legal {
        move;
        new_state={
            turn=Color.(opposite turn);
            board=realize_potential_move move board special_move_type;
        };
    }

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

let is_in_check color board =
    match king_location color board with
    (* This shouldn't happen, but let's say there's no check without a king *)
    | None -> false
    (* TODO: Fix check check *)
    | Some spot -> false

let find_one_potential_move_with_correct_delta _ _ _ = None

let attempt_move move state =
    let {board;turn} = state in
    let open Board in
    let open IllegalMoveReason in
    match get_value_at move.from board with
    | None -> Illegal FromEmpty
    | Some piece ->
            let ({color;rank} : Board.piece_t) = piece in
            let potential_moves = PotentialMove.get_for_rank rank in
            match find_one_potential_move_with_correct_delta potential_moves move.from move.destination with
            | None -> Illegal IllegalForPiece
            | Some potential_move -> attempt_potential_move state move.from potential_move

type game_ended_t =
    | Ongoing
    | Checkmate of Color.t
    | Stalemate

let game_ended {board;turn} =
    let has_moves = true in
    if (is_in_check turn board) && (not has_moves) then Checkmate Color.(opposite turn)
    (* TODO: Implement repeated-moves stalemate *)
    else if has_moves then Ongoing
    else Stalemate
