type 'a t = 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a

let set idx value (v0,v1,v2,v3,v4,v5,v6,v7) =
         if idx = 0 then (value,v1,v2,v3,v4,v5,v6,v7)
    else if idx = 1 then (v0,value,v2,v3,v4,v5,v6,v7)
    else if idx = 2 then (v0,v1,value,v3,v4,v5,v6,v7)
    else if idx = 3 then (v0,v1,v2,value,v4,v5,v6,v7)
    else if idx = 4 then (v0,v1,v2,v3,value,v5,v6,v7)
    else if idx = 5 then (v0,v1,v2,v3,v4,value,v6,v7)
    else if idx = 6 then (v0,v1,v2,v3,v4,v5,value,v7)
    else if idx = 7 then (v0,v1,v2,v3,v4,v5,v6,value)
    else (v0,v1,v2,v3,v4,v5,v6,v7)

let get idx (v0,v1,v2,v3,v4,v5,v6,v7) =
         if idx = 0 then Some v0
    else if idx = 1 then Some v1
    else if idx = 2 then Some v2
    else if idx = 3 then Some v3
    else if idx = 4 then Some v4
    else if idx = 5 then Some v5
    else if idx = 6 then Some v6
    else if idx = 7 then Some v7
    else None

let init value = value, value, value, value, value, value, value, value

let to_list (v0,v1,v2,v3,v4,v5,v6,v7) = [v0;v1;v2;v3;v4;v5;v6;v7]
