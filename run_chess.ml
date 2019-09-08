type parsed_move_t =
    | ParsedMove of Board.move_t
    | UnparseableMove

let lower = Char.lowercase_ascii

let parse_move move_str =
    let len = String.length move_str in
    if len < 4
    then UnparseableMove
    else ParsedMove (

    let from_col_char = String.get move_str 0       |> lower in
    let from_row_char = String.get move_str 1                in
    let   to_col_char = String.get move_str (len-2) |> lower in
    let   to_row_char = String.get move_str (len-1)          in

    let col_offset = int_of_char 'a'                          in
    let from_col   = (int_of_char from_col_char) - col_offset in
    let   to_col   = (int_of_char   to_col_char) - col_offset in

    let row_offset = int_of_char '1'                          in
    let from_row   = (int_of_char from_row_char) - row_offset in
    let   to_row   = (int_of_char   to_row_char) - row_offset in

    {from        = {row = from_row ; col = from_col} ;
     destination = {row =   to_row ; col =   to_col}}

    )

let rec play_game (state : GameState.t) =
    match GameState.game_ended state with
    | Checkmate winner -> print_endline (String.concat "" [(Color.to_string winner); " wins!"])
    | Stalemate -> print_endline "Stalemate!"
    | Ongoing ->
    let {board;turn} : GameState.t = state in
    let () = Board.print board in
    let () = String.concat "" [(Color.to_string turn); " to move"] |> print_endline in
    let () = print_string "Enter a move: " in
    let parsed_move = read_line () |> parse_move in
    match parsed_move with
    | UnparseableMove -> (
        let () = print_endline "Move not parseable." in
        play_game state)
    | ParsedMove move ->
        match (GameState.attempt_move move state) with
        | Illegal reason -> (
            let () = IllegalMoveReason.to_string reason |> print_endline in
            play_game state)
        | Legal {move;new_state} -> play_game new_state
;;

play_game GameState.initial
