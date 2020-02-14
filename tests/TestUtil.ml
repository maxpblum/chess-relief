let string_of_board_option = function
    | None -> "no board"
    | Some board -> String.concat "" ["\n"; IO.string_of_board (* invert=*) false board]

let string_of_attempted_move : GameState.attempted_move_t -> string =
    let open GameState in function
    | Illegal reason -> IllegalMoveReason.to_string reason
    | Legal GameState.{board} ->
            String.concat "" ["\n"; IO.string_of_board (* invert=*) false board]

let expand_shorthand = List.map Board.occupied_space_of_shorthand

let attempt_move_from_shorthand_state before turn move =
    let before_board = Board.create (expand_shorthand before) in
    let before_state = GameState.{board=before_board;turn} in
    GameState.attempt_move move before_state

let assert_legal_full_move before turn move expected ~msg =
    let open GameState in
    let expected_board = Board.create (expand_shorthand expected) in
    let result = attempt_move_from_shorthand_state before turn move in

    OUnit2.assert_equal
        ~msg:msg
        ~printer:string_of_attempted_move
        (Legal {turn=(Color.opposite turn) ; board=expected_board})
        result

let assert_legal_move before turn (from_row,from_col) (to_row,to_col) expected ~msg =
    let move = Board.{
        from={row=from_row;col=from_col};
        destination={row=to_row;col=to_col};
        replacement=None;
    } in
    assert_legal_full_move before turn move expected ~msg

let assert_illegal_full_move before turn move ~msg =
    let open GameState in
    let maybe_actual_board =
        match attempt_move_from_shorthand_state before turn move with
        | Illegal _ -> None
        | Legal GameState.{board} -> Some board in

    OUnit2.assert_equal
        ~msg:msg
        ~printer:string_of_board_option
        None
        maybe_actual_board

let assert_illegal_move before turn (from_row,from_col) (to_row,to_col) ~msg =
    let move = Board.{
        from={row=from_row;col=from_col};
        destination={row=to_row;col=to_col};
        replacement=None;
    } in
    assert_illegal_full_move before turn move ~msg
