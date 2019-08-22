type t = Space.t array array
type delta_t = {rows:int ; cols:int}
type location_t = {row:int ; col:int}
type move_t = {from:location_t ; destination:location_t}
type legal_move_t = {
    move : move_t ;
    new_board : t ;
}

let back_order = Rank.[Rook false; Knight; Bishop; Queen; King false; Bishop; Knight; Rook false]
let front_order = Util.repeat_times Rank.Pawn 8
let apply_color color = List.map (fun rank -> Some (Piece.piece color rank))
let rows : Space.t list list = List.concat [
    [apply_color White back_order; apply_color White front_order];
    Util.repeat_times (Util.repeat_times None 8) 4;
    [apply_color Black front_order; apply_color Black back_order]
]
let row_arrays : Space.t array list = List.map Array.of_list rows
let initial : t = Array.of_list row_arrays
let castling_test : t = Array.of_list [
    Array.of_list [None; None; None; None; Some Piece.{rank=Rank.(King false);color=Color.White}; None; None; Some {rank=Rank.(Rook false);color=Color.White};];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; None; None; None;];
    Array.of_list [None; None; None; None; None; Some Piece.{rank=Rank.Queen;color=Color.Black}; None; None;];
]

let space_string row col =
    let background =
        let open Color in
        if (row + col) mod 2 = 0
        then Black
        else White
    in Space.to_string background

let row_of_strings idx = Array.mapi (space_string idx)
let row_strings = Array.mapi row_of_strings
let print_row_of_strings (row : string array) : unit =
    row |> Array.to_list |> String.concat "" |> print_endline

let reset_background = "\027[0m"

let print (board : t) =
    let () = board |> row_strings |> Util.iterate_array_backwards print_row_of_strings in
    print_string reset_background

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
