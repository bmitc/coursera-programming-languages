type MyType = TwoInts of int * int
            | Str of string
            | Pizza

let a = Str "hi"
let b = Str
let c = Pizza
let d = TwoInts(1+2,3+4)
let e = a