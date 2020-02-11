type moved_t = bool

type t =
    | Pawn of bool
    | Rook of moved_t
    | Knight
    | Bishop
    | Queen
    | King of moved_t

(* Maps piece constructors to same-color string representation *)
val to_string_in_background_color : t -> string
val to_string_in_non_background_color : t -> string
