type MyType = TwoInts of int * int
            | Str of string
            | Pizza

(* mytype -> int *)
let f (x : MyType) =
    match x with
        | Pizza -> 3
        | Str s -> 8
        | TwoInts(i1,i2) -> i1 + i2