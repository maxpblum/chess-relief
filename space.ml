type t = Piece.t option
let to_string (background : Color.t) : t -> string =
    let background_string = Color.set_background background in
    function
        | None -> String.concat "" [background_string; "   "]
        | Some {color; rank} ->
                let stringifier = (
                    if color = background
                    then Rank.to_string_in_background_color
                    else Rank.to_string_in_non_background_color
                ) in
                String.concat "" [background_string; " "; stringifier rank; " "]
