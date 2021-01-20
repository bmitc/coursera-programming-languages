// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists
// Lists in F# are entered similarly to SML, but with semi-colons.
// So the list [1,2,3] in SML is [1;2;3] in F#.

// Also, recursive functions in F# must use the rec keyword.
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#recursive-functions

// List.isEmpty, List.head, List.tail are part of the List module.
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#module-functions

let rec sumList (xs : int list) =
    if List.isEmpty xs
    then 0
    else List.head xs + sumList(List.tail xs)

// Alternatively, since F# is an OOP language, we could use
// the properties: xs.IsEmpty, xs.Head, xs.Tail
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#properties

let rec listProduct (xs : int list) =
    if List.isEmpty xs
    then 1
    else List.head xs * listProduct(List.tail xs)

let rec countdown (x : int) =
    if x = 0
    then []
    else x :: countdown(x-1)

let rec append (xs : int list, ys : int list) =
    if List.isEmpty xs
    then ys
    else (List.head xs) :: append(List.tail xs, ys)

let rec sumPairList (xs : (int * int) list) =
    if List.isEmpty xs
    then 0
    else fst (List.head xs) + snd (List.head xs) + sumPairList(List.tail xs)

let rec firsts (xs : (int * int) list) =
    if List.isEmpty xs
    then []
    else (fst (List.head xs)) :: firsts(List.tail xs)

let rec seconds (xs : (int * int) list) =
    if List.isEmpty xs
    then []
    else (snd (List.head xs)) :: seconds(List.tail xs)

let sumPairList2 (xs : (int * int) list) =
    (sumList(firsts xs)) + (sumList(seconds xs))

let factorial (n : int) =
    listProduct(countdown n)