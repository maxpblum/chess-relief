type piece_t = {
    color : Color.t ;
    rank  : Rank.t ;
}
type space_t = piece_t option
type t = space_t Octet.t Octet.t
type delta_t = {rows:int ; cols:int}
type location_t = {row:int ; col:int}
type move_t = {from:location_t ; destination:location_t}
type legal_move_t = {
    move : move_t ;
    new_board : t ;
}

let set_location location value board : t =
    match Octet.get location.row board with
    (* Shouldn't happen if location is valid *)
    | None -> board
    | Some old_row ->
            let new_row = Octet.set location.col value old_row
            in Octet.set location.row new_row board

let is_on_board {row; col} =
    0 <= row &&
    row <= 7 &&
    0 <= col &&
    col <= 7

let get_value_at location board : space_t =
    match Octet.get location.row board with
    | None -> None
    | Some row -> match Octet.get location.col row with
    | None -> None
    | Some space -> space

let make_move move board : t =
    let piece = get_value_at move.from board in
    board |> set_location move.from None |> set_location move.destination piece

let move_of_delta {rows;cols} from =
    {from;destination={row=from.row+rows;col=from.col+cols}}

let rec fold_impl combine accum board location =
    if location.row > 7 then accum
    else if location.col > 7 then fold_impl combine accum board {row=location.row+1;col=0}
    else
        let new_accum = combine accum (get_value_at location board) location in
        fold_impl combine new_accum board {location with col=location.col+1}

let fold combine initial board = fold_impl combine initial board {row=0;col=0}

type piece_on_board_t = {piece : piece_t ; location : location_t}

let all_pieces_of_color color =
    let combine pieces space location = match space with
    | None -> pieces
    | Some piece ->
            if (piece.color) = color
            then {piece;location} :: pieces
            else pieces
    in fold combine []

let rec fill_row_array_from_board row_idx col_idx board (row_array : space_t array) =
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

let empty = Octet.init (Octet.init None)

type occupied_space_t = {location:location_t;color:Color.t;rank:Rank.t}

let rec create_impl board = function
    | [] -> board
    | {location;color;rank} :: ps ->
            let new_board = board |>
                set_location location (Some {color;rank})
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
        (1,0,White,Pawn false);
        (1,1,White,Pawn false);
        (1,2,White,Pawn false);
        (1,3,White,Pawn false);
        (1,4,White,Pawn false);
        (1,5,White,Pawn false);
        (1,6,White,Pawn false);
        (1,7,White,Pawn false);
        (6,0,Black,Pawn false);
        (6,1,Black,Pawn false);
        (6,2,Black,Pawn false);
        (6,3,Black,Pawn false);
        (6,4,Black,Pawn false);
        (6,5,Black,Pawn false);
        (6,6,Black,Pawn false);
        (6,7,Black,Pawn false);
        (7,0,Black,Rook false);
        (7,1,Black,Knight);
        (7,2,Black,Bishop);
        (7,3,Black,Queen);
        (7,4,Black,King false);
        (7,5,Black,Bishop);
        (7,6,Black,Knight);
        (7,7,Black,Rook false);
    ]) |> List.map occupied_space_of_shorthand |> create
