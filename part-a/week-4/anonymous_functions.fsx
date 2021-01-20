let rec nTimes (f,n,x) =
    if n=0
    then x
    else f (nTimes(f,n-1,x))

let tripleNTimes (n,x) =
    nTimes((fun x -> 3*x), n, x)
    // F# was giving an error without the parantheses surrounding the
    // anonymouse function.