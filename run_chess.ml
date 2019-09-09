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

let space_string invert row col =
    let remainder = if invert then 1 else 0 in
    let background =
        let open Color in
        if (row + col) mod 2 = remainder
        then Black
        else White
    in Space.to_string background

let row_of_strings invert idx = Array.mapi (space_string invert idx)
let row_strings invert = Array.mapi (row_of_strings invert)
let print_row_of_strings (row : string array) : unit =
    row |> Array.to_list |> String.concat "" |> print_endline

let reset_background = "\027[0m"

let print_board invert board =
    let () = board |> row_strings invert |> Util.iterate_array_backwards print_row_of_strings in
    print_string reset_background

let rec play_game (state : GameState.t) =
    match GameState.game_ended state with
    | Checkmate winner -> print_endline (String.concat "" [(Color.to_string winner); " wins!"])
    | Stalemate -> print_endline "Stalemate!"
    | Ongoing ->
    let {board;turn} : GameState.t = state in
    let () = print_board false (Board.to_matrix board) in
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
