type 'a t = {
    length : int     ;
    front  : 'a list ;
    back   : 'a list ;
}

let enqueue (item : 'a) ({length=qlength;front=qfront;back=qback} : 'a t) =
    {
        length = qlength+1       ;
        front  = qfront          ;
        back   = (item :: qback) ;
    }

let rec dequeue {length=qlength;front=qfront;back=qback} = match (qfront, qback) with
    | ([], []) -> None
    | ([], xs) -> dequeue ({
        length = qlength       ;
        front  = (List.rev xs) ;
        back   = []            ;
    })
    | (x :: xs, _) -> Some {
        length = qlength - 1 ;
        front  = xs          ;
        back   = qback       ;
    }
