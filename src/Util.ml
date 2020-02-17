let rec iterate_array_backwards_impl f arr idx =
    if idx < 0
    then ()
    else (
        (Array.get arr idx |> f idx);
        iterate_array_backwards_impl f arr (idx-1)
    )

let iterate_array_backwards_i f arr =
    iterate_array_backwards_impl f arr ((Array.length arr) - 1)

let iterate_array_backwards f = iterate_array_backwards_i (fun _ -> f)

type ('a,'b) iterable_state_t = {
    current: 'a ;
    state: 'b ;
}

type ('a,'b) iterable_t =
    | Done
    | Ongoing of ('a,'b) iterable_state_t

type ('a,'b) iterate_func_t = 'b -> ('a,'b) iterable_t

let rec find_bool (iterate_func : ('a,'b) iterate_func_t)
: ('a,'b) iterable_t -> bool =
    function
    | Done -> false
    | Ongoing {current;state} ->
        if current
        then true
        else let next_state = iterate_func state in
        find_bool iterate_func next_state
