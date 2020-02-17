val iterate_array_backwards_impl : (int -> 'a -> 'b) -> 'a array -> int -> unit
val iterate_array_backwards_i : (int -> 'a -> 'b) -> 'a array -> unit
val iterate_array_backwards : ('a -> unit) -> 'a array -> unit

type ('a,'b) iterable_state_t = {
    current: 'a ;
    state: 'b ;
}

type ('a,'b) iterable_t =
    | Done
    | Ongoing of ('a,'b) iterable_state_t

type ('a,'b) iterate_func_t = 'b -> ('a,'b) iterable_t
