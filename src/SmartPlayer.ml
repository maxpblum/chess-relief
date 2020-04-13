let score_of_piece_on_board Board.{piece={rank}} =
    match rank with
    | Pawn _ -> 1.
    | Knight -> 2.9
    | Bishop -> 3.
    | Rook _ -> 5.
    | Queen _ -> 9.
    | King _ -> 1000.

let score_of_pieces_on_board pieces =
    pieces |> List.map score_of_piece_on_board |> List.fold_left (+.) 0.

(* Higher = better for team *)
let piece_score color board =
    let open Board in
    (all_pieces_of_color color board |> score_of_pieces_on_board) +.
    ((-1.) *. (all_pieces_of_color (Color.opposite color) board |> score_of_pieces_on_board))

let new_score_of_move state move =
    let open GameState in
    match attempt_move move state with
    (* Only call this function with legal moves *)
    | Illegal reason -> 0.
    | Legal {board} -> piece_score state.turn board

let better_move state a b =
    let a_score = new_score_of_move state a in
    let b_score = new_score_of_move state b in
    if a_score > b_score then a else b

let simple_best_move moves state =
    match moves with
    (* Only call this function with a non-empty list of valid moves *)
    | [] -> ({from={row=(-1);col=(-1)};destination={row=(-1);col=(-1)};replacement=None} : Board.move_t)
    | first_move :: mvs -> moves |> List.fold_left (better_move state) first_move

let get_move state =
    (GameState.all_moves state |> simple_best_move) state
