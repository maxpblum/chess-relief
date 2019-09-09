type t = {
    color : Color.t ;
    rank  : Rank.t ;
}

let piece color rank : t = {
    color = color;
    rank  = rank;
}
