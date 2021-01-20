fun sorted3 x y z = z >= y andalso y >= x

fun fold f acc xs = (* means fun fold f = fn acc => fn xs => *)
    case xs of
          []     => acc
        | x::xs' => fold f (f(acc,x)) xs'

(* If a curried function is applied to "too few" arguments, that
   returns, which is often useful. A power idiom (no new semantics) *)

val is_nonnegative = sorted3 0 0

val sum = fold (fn (x,y) => x+y) 0

(* inferior options compared to partial application *)

fun is_nonnegative_inferior x = sorted3 0 0 x

fun sum_inferior xs = fold (fn (x,y) => x+y) 0 xs

(* another example *)

(* range 3 6 = [3,4,5,6] *)
fun range i j = if i > j then [] else i :: range (i+1) j

(* countup 6 = [1,2,3,4,5,6] *)
val countup = range 1

fun countup_inferior x = range 1 x

(* Common style is to curry higher-order functions with
   function arguments first to enable conventient
   partial application. *)

fun exists predicate xs =
    case xs of
          [] => false
        | x::xs' => predicate x orelse exists predicate xs'

val no = exists (fn x => x=7) [4,11,23] (* false *)

val hasZero = exists (fn x => x=0) (* int list -> bool *)

val incrementAll = List.map (fn x => x + 1) (* int list -> int list *)

(* library functions foldl, List.filter, etc. also curried: *)

val removeZeros = List.filter (fn x => x <> 0)

(* But if you get a strange message about "value restriction",
   put back in the actually-necessary wrapping or an explicit
   non-polymorphic type. *)

(* Doesn't work for reasons we won't explain here (more later). 
   (Only an issue with polymorphic functions.) *)

(* 'a list -> ('a * int) list *)
(* val pairWithOne = List.map (fn x => (x,1)) *)

(* workarounds: *)
fun pairWithOne xs = List.map (fn x => (x,1)) xs

val pairWithOne : string list -> (string * int) list = 
    List.map (fn x => (x,1))

(* This function works fine because result is not polymorphic. *)
val incrementAndPairWithOne = List.map (fn x => (x+1,1))