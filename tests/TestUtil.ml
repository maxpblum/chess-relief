let string_of_board_option = function
    | None -> "no board"
    | Some board -> String.concat "" ["\n"; IO.string_of_board (* invert=*) false board]

let expand_shorthand = List.map Board.occupied_space_of_shorthand

let attempt_move_from_shorthand before turn (from_row,from_col) (to_row,to_col) =
    let before_board = Board.create (expand_shorthand before) in
    let before_state = GameState.{board=before_board;turn} in
    let move = Board.{
        from={row=from_row;col=from_col};
        destination={row=to_row;col=to_col};
    } in
    GameState.attempt_move move before_state

let assert_legal_move before turn from destination expected ~msg =
    let expected_board = Board.create (expand_shorthand expected) in

    let maybe_actual_board =
        let open GameState in
        match attempt_move_from_shorthand before turn from destination with
        | Illegal _ -> None
        | Legal GameState.{board} -> Some board in

    OUnit2.assert_equal
        ~msg:msg
        ~printer:string_of_board_option
        (Some expected_board)
        maybe_actual_board

let assert_illegal_move before turn from destination ~msg =
    let open GameState in
    let maybe_actual_board =
        match attempt_move_from_shorthand before turn from destination with
        | Illegal _ -> None
        | Legal GameState.{board} -> Some board in

    OUnit2.assert_equal
        ~msg:msg
        ~printer:string_of_board_option
        None
        maybe_actual_board
