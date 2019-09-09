type 'a t
val set : int -> 'a -> 'a t -> 'a t
val get : int -> 'a t -> 'a option
val init : 'a -> 'a t
val to_list : 'a t -> 'a list
