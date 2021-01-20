fun fold (f,acc,xs) =
    case xs of
          [] => acc
        | x::xs' => fold (f,f(acc,x),xs')

(* examples not using private data *)

(* sum list *)
fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)

(* are all list elements non-negative? *)
fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* examples using private data *)

(* counting the number of elements between lo and hi, inclusive *)
fun f3 (xs,lo,hi) =
    fold ((fn (x,y) => x + (if y >= lo andalso y <= hi
                            then 1
                            else 0))
          0, xs)

(* are all strings in the list of smaller size than s? *)
fun f4 (xs,s) =
    let
        val i = String.size s
    in
        fold ((fn (x,y) => x andalso String.size y < i), true, xs)
    end

(* do all elements of the list produce true when passed to g? *)
fun f5 (g,xs) = fold ((fn (x,y) => x andalso g y), true, xs)

fun f4again (xs,s) =
    let
        val i = String.size s
    in
        f5(fn y => String.size y < i, xs)
    end