type moved_t = bool

type t =
    | Pawn of bool
    | Rook of moved_t
    | Knight
    | Bishop
    | Queen
    | King of moved_t

(* Maps piece constructors to same-color string representation *)
let to_string_in_background_color : t -> string = function
    | King _ -> "♔"
    | Queen -> "♕"
    | Rook _ -> "♖"
    | Bishop -> "♗"
    | Knight -> "♘"
    | Pawn _ -> "♙"

(* Maps piece constructors to opposite-color string representation *)
let to_string_in_non_background_color : t -> string = function
    | King _ -> "♚"
    | Queen -> "♛"
    | Rook _ -> "♜"
    | Bishop -> "♝"
    | Knight -> "♞"
    | Pawn _ -> "♟"

let clean_rook = Rook false
let clean_king = King false
let queen = Queen
let bishop = Bishop
let knight = Knight
let pawn = Pawn false
