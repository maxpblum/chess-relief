type t = Space.t array array
type delta_t = {rows:int ; cols:int}
type location_t = {row:int ; col:int}
type move_t = {from:location_t ; destination:location_t}
type legal_move_t = {
    move : move_t ;
    new_board : t ;
}

val initial : t
val castling_test : t
val print : t -> unit
val get_value_at : location_t -> t -> Space.t
val make_move : move_t -> t -> t
val set_location : location_t -> Space.t -> t -> t
val is_on_board : location_t -> bool
val move_of_delta : delta_t -> location_t -> move_t

type occupied_space_t = {location:location_t;color:Color.t;rank:Rank.t}
val create : occupied_space_t list -> t
