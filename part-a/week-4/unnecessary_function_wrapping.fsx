let rec nTimes (f,n,x) =
    if n=0
    then x
    else f (nTimes(f,n-1,x))

let nthTail(n,xs) = nTimes(List.tail,n,xs)

let rev = List.rev