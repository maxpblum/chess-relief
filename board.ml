type t = Space.t array array
type delta_t = {rows:int ; cols:int}
type location_t = {row:int ; col:int}
type move_t = {from:location_t ; destination:location_t}
type legal_move_t = {
    move : move_t ;
    new_board : t ;
}

let set_location location value board : t =
    let new_board = Array.copy board in
    let row_array = Array.get new_board location.row in
    let new_row_array = Array.copy row_array in
    (Array.set new_row_array location.col value;
     Array.set new_board location.row new_row_array;
     new_board)

let is_on_board {row; col} =
    0 <= row &&
    row <= 7 &&
    0 <= col &&
    col <= 7

let get_value_at location board : Space.t =
    if not (location |> is_on_board)
    then None
    else
    let row_array = Array.get board location.row in
    Array.get row_array location.col

let make_move move board : t =
    let piece = get_value_at move.from board in
    board |> set_location move.from None |> set_location move.destination piece

let move_of_delta {rows;cols} from =
    {from;destination={row=from.row+rows;col=from.col+cols}}

type piece_in_row_t = {piece : Piece.t ; idx : int}

let rec all_pieces_of_color_from_row_impl color row idx =
    if idx = 8 then [] else
    let rest_of_row = all_pieces_of_color_from_row_impl color row (idx+1) in
    match Array.get row idx with
    | None -> rest_of_row
    | Some piece ->
            let Piece.{color=piece_color} = piece in
            if piece_color = color
            then {piece;idx} :: rest_of_row
            else rest_of_row

let all_pieces_of_color_from_row color row = all_pieces_of_color_from_row_impl color row 0

type piece_on_board_t = {piece : Piece.t ; location : location_t}

let rec all_pieces_of_color_impl color board idx =
    if idx = 8 then [] else
    let rest_of_board = all_pieces_of_color_impl color board (idx+1) in
    let pieces_in_row = all_pieces_of_color_from_row color (Array.get board idx) in
    (List.map (fun {piece;idx=col} -> {piece;location={row=idx;col}}) pieces_in_row) :: rest_of_board

let all_pieces_of_color color board =
    all_pieces_of_color_impl color board 0 |> List.concat

let rec fill_row_array_from_board row_idx col_idx board (row_array : Space.t array) =
    if col_idx > 7
    then ()
    else
    let () = Array.set row_array col_idx (get_value_at {row=row_idx;col=col_idx} board) in
    fill_row_array_from_board row_idx (col_idx+1) board row_array

let rec fill_matrix_from_board row_idx board matrix =
    if row_idx > 7
    then ()
    else
    let new_row = Array.make 8 None in
    let () = fill_row_array_from_board row_idx 0 board new_row in
    let () = Array.set matrix row_idx new_row in
    fill_matrix_from_board (row_idx+1) board matrix

let to_matrix board =
    let matrix = Array.make 8 (Array.make 8 None) in
    let () = fill_matrix_from_board 0 board matrix in
    matrix

let empty : t = Array.make 8 (Array.make 8 None)

type occupied_space_t = {location:location_t;color:Color.t;rank:Rank.t}

let rec create_impl board = function
    | [] -> board
    | {location;color;rank} :: ps ->
            let new_board = board |>
                set_location location (Some (Piece.piece color rank))
            in create_impl new_board ps

let create = create_impl empty

type occupied_space_shorthand_t = int*int*Color.t*Rank.t

let occupied_space_of_shorthand (row,col,color,rank) = {location={row;col};color;rank}

let initial =
    Color.(Rank.[
        (0,0,White,Rook false);
        (0,1,White,Knight);
        (0,2,White,Bishop);
        (0,3,White,Queen);
        (0,4,White,King false);
        (0,5,White,Bishop);
        (0,6,White,Knight);
        (0,7,White,Rook false);
        (1,0,White,Pawn);
        (1,1,White,Pawn);
        (1,2,White,Pawn);
        (1,3,White,Pawn);
        (1,4,White,Pawn);
        (1,5,White,Pawn);
        (1,6,White,Pawn);
        (1,7,White,Pawn);
        (6,0,Black,Pawn);
        (6,1,Black,Pawn);
        (6,2,Black,Pawn);
        (6,3,Black,Pawn);
        (6,4,Black,Pawn);
        (6,5,Black,Pawn);
        (6,6,Black,Pawn);
        (6,7,Black,Pawn);
        (7,0,Black,Rook false);
        (7,1,Black,Knight);
        (7,2,Black,Bishop);
        (7,3,Black,Queen);
        (7,4,Black,King false);
        (7,5,Black,Bishop);
        (7,6,Black,Knight);
        (7,7,Black,Rook false);
    ]) |> List.map occupied_space_of_shorthand |> create
