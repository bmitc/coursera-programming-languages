let rec map (f,xs) =
    match xs with
        | [] -> []
        | x::xs' -> (f x)::map(f,xs')

let x1 = map((fun x -> x + 1), [4;8;12;16])

let x2 = map(List.head, [[1;2];[3;4];[5;6;7]])

let rec filter (f,xs) =
    match xs with
        | [] -> []
        | x::xs' -> if f x
                    then x::(filter (f,xs'))
                    else filter(f,xs')

let isEven v =
    (v % 2 = 0)

let allEven xs = filter(isEven, xs)

let allEvenSnd xs = filter((fun (_,v) -> isEven v), xs)