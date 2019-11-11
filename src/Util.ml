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
