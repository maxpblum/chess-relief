type parsed_move_t =
    | ParsedMove of Board.move_t
    | UnparseableMove

let lower = Char.lowercase_ascii

let rank_of_char =
    let open Rank in
    function
    | 'r' -> Some (Rook false)
    | 'k' -> Some Knight
    | 'b' -> Some Bishop
    | 'q' -> Some Queen
    | _ -> None

let parse_move move_str =
    let len = String.length move_str in
    if len < 4
    then UnparseableMove
    else ParsedMove (

    let exchange_rank = String.get move_str (len-1) |> lower |> rank_of_char in
    let (stripped_move_str,stripped_len) = match exchange_rank with
    | None -> (move_str,len)
    | Some rank ->
        let stripped =
            (move_str |>
            String.mapi (fun idx ch -> if idx < (len-1) then ch else ' ') |>
            String.trim)
        in (stripped, String.length stripped)
    in

    let from_col_char = String.get stripped_move_str 0       |> lower in
    let from_row_char = String.get stripped_move_str 1                in
    let   to_col_char = String.get stripped_move_str (stripped_len-2) |> lower in
    let   to_row_char = String.get stripped_move_str (stripped_len-1)          in

    let col_offset = int_of_char 'a'                          in
    let from_col   = (int_of_char from_col_char) - col_offset in
    let   to_col   = (int_of_char   to_col_char) - col_offset in

    let row_offset = int_of_char '1'                          in
    let from_row   = (int_of_char from_row_char) - row_offset in
    let   to_row   = (int_of_char   to_row_char) - row_offset in

    Board.
    {from        = {row = from_row ; col = from_col} ;
     destination = {row =   to_row ; col =   to_col} ;
     replacement = exchange_rank}

    )

let int_of_bool = function
    | true -> 1
    | false -> 0

let xor a b = (
        [a; b] |> List.map int_of_bool |> List.fold_left (+) 0
    ) mod 2 = 1

let string_of_space invert row col (space : Board.space_t) =
    let remainder = if invert then 1 else 0 in
    let background =
        let open Color in
        if (row + col) mod 2 = remainder
        then Black
        else White
    in
    let background_string = Color.set_background background in
    match space with
        | None -> String.concat "" [background_string; "   "]
        | Some Board.{color; rank} ->
                let stringifier = (
                    if xor (color = background) invert
                    then Rank.to_string_in_background_color
                    else Rank.to_string_in_non_background_color
                ) in
                String.concat "" [background_string; " "; stringifier rank; " "]

let rec one_row_strings_list_impl invert row col accum board =
    if col<0 then accum else
    let space = Board.(get_value_at {row;col} board) in
    let space_string = string_of_space invert row col space in
    one_row_strings_list_impl invert row (col-1) (space_string :: accum) board

let one_row_string invert row board =
    one_row_strings_list_impl invert row 7 [] board |> String.concat ""

let rec row_strings_list_impl invert row accum board =
    if row>7 then accum else
    let row_string = one_row_string invert row board in
    row_strings_list_impl invert (row+1) (row_string :: accum) board

let reset_background = "\027[0m"

let string_of_board invert board =
    row_strings_list_impl invert 0 [reset_background] board |>
    String.concat "\n"

let rec get_human_move state =
    let () = print_string "Enter a move: " in
    let parsed_move = read_line () |> parse_move in
    match parsed_move with
    | UnparseableMove ->
        let () = print_endline "Move not parseable." in
        get_human_move state
    | ParsedMove move -> move

let get_random_move state =
    let moves = GameState.all_moves state in
    List.nth moves (Random.int (List.length moves))

let rec play_game (state : GameState.t) =
    let open GameState in
    match game_ended state with
    | Checkmate winner -> print_endline (String.concat "" [(Color.to_string winner); " wins!"])
    | Stalemate -> print_endline "Stalemate!"
    | Ongoing ->
    let {board;turn} : GameState.t = state in
    let () = print_string (string_of_board false board) in
    let () = String.concat "" [(Color.to_string turn); " to move"] |> print_endline in
    let move_getter =
        if state.turn = Color.White
        then get_human_move
        else get_random_move
    in
    let move = move_getter state in
    match attempt_move move state with
    | Illegal reason -> (
        let () = IllegalMoveReason.to_string reason |> print_endline in
        play_game state)
    | Legal new_state -> play_game new_state
