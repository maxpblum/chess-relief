type piece_t = {
    color : Color.t ;
    rank  : Rank.t ;
}
type space_t = piece_t option
type t
type delta_t = {rows:int ; cols:int}
type location_t = {row:int ; col:int}
type move_t = {from:location_t ; destination:location_t ; replacement:Rank.t option}

val null_move : move_t

type legal_move_t = {
    move : move_t ;
    new_board : t ;
}

val get_value_at : location_t -> t -> space_t
val make_move : move_t -> t -> t
val set_location : location_t -> space_t -> t -> t
val is_on_board : location_t -> bool
val move_of_delta : delta_t -> location_t -> move_t

type occupied_space_t = {location:location_t;color:Color.t;rank:Rank.t}
val create : occupied_space_t list -> t
val initial : t

type occupied_space_shorthand_t = int*int*Color.t*Rank.t
val occupied_space_of_shorthand : occupied_space_shorthand_t -> occupied_space_t

val to_matrix : t -> space_t array array

type piece_on_board_t = {piece : piece_t ; location : location_t}
val all_pieces_of_color : Color.t -> t -> piece_on_board_t list

(* Iterate through all the spaces on a given board, combining into an
 * accumulator of any type. *)
val fold : ('a -> space_t -> location_t -> 'a) -> 'a -> t -> 'a
