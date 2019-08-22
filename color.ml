type t = Black | White

let opposite = function
    | Black -> White
    | White -> Black

let to_string = function
    | Black -> "Black"
    | White -> "White"

let set_background : t -> string = function
    | Black -> "\027[30m\027[00m"
    | White -> "\027[07m"
