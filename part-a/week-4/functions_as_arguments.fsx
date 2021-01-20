let rec incrementNTimesLame (n,x) =
    if n=0
    then x
    else 1 + incrementNTimesLame(n-1,x)

let rec doubleNTimesLame (n,x) =
    if n=0
    then x
    else 2 * doubleNTimesLame(n-1,x)

let rec nthTailLame (n,xs) =
    if n=0
    then xs
    else List.tail (nthTailLame(n-1,xs))

let rec nTimes (f,n,x) =
    if n=0
    then x
    else f (nTimes(f,n-1,x))

let increment x = x + 1

let double x = x + x

let x1 = nTimes(double,4,7)

let x2 = nTimes(increment,4,7)

let x3 = nTimes(List.tail,2,[4;8;12;16])

let addition (n,x) = nTimes(increment,n,x)

let doubleNTimes (n,x) = nTimes(double,n,x)

let nthTail (n,x) = nTimes(List.tail,n,x)

let triple x = 3 * x

let triple_nTimes (n,x) = nTimes(triple,n,x)