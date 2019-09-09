let rec repeat_times x times =
    if times = 0
    then []
    else x :: repeat_times x (times-1)

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

let rec range start stop interval =
    if (interval >= 0 && start >= stop) || (interval < 0 && start <= stop)
    then []
    else start :: range (start + interval) stop interval
