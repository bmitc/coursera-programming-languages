let rec countup (from : int, upto : int) =
    if from = upto
    then upto :: []
    else from :: countup(from+1, upto)

// A much easier definition using F#'s built-in ranger operator
let countup2 from upto = [from .. upto]

let rec countdown (from : int, down : int) =
    if from = down
    then down :: []
    else from :: countdown(from-1, down)

// Similarly, using a custom increment/decrement
let countdown2 from down = [from .. -1 .. down]

let rec badMax (xs : int list) =
    if List.isEmpty xs
    then 0
    else if List.isEmpty(List.tail xs)
    then List.head xs
    else if List.head xs > badMax(List.tail xs)
    then List.head xs
    else badMax(List.tail xs)

let rec goodMax (xs : int list) =
    if List.isEmpty xs
    then 0
    else if List.isEmpty (List.tail xs)
    then List.head xs
    else
        let tlAns = goodMax(List.tail xs)
        if List.head xs > tlAns
        then List.head xs
        else tlAns