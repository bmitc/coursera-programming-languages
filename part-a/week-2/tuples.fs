// F# does not have the syntax #1, #2, #3, ... for accessing
// arbitrary tuple elements. However, it does have fst and
// snd for accessing the first and second values of tuple pairs.
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/tuples

let swap (pr : int * bool) =
    (fst pr, snd pr)

let sumTwoPairs (pr1 : int * int, pr2 : int * int) =
    (fst pr1) + (snd pr1) + (fst pr2) + (snd pr2)

let divMod (x : int, y : int) =
    (x / y, x % y)
//https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators

let sortPair (pr : int * int) =
    if (fst pr) < (snd pr)
    then pr
    else (snd pr, fst pr)