fun hd xs =
    case xs of
          [] => raise List.Empty
        | x::_ => x

exception MyUndesirableCondition

exception MyOtherException of int * int

(* raise MyOtherException (3,4) *)

fun mydiv (x,y) =
    if y=0
    then raise MyUndesirableCondition
    else x div y

fun maxlist (xs,ex) =
    case xs of
          [] => raise ex
        | x::[] => x
        | x::xs' => Int.max(x,maxlist(xs',ex))

val w = maxlist ([3,4,5],MyUndesirableCondition)

val x = maxlist ([3,4,5],MyUndesirableCondition)
        handle MyUndesirableCondition => 42

(* val y = maxlist ([],MyUndesirableCondition) *)

val z = maxlist ([],MyUndesirableCondition)
        handle MyUndesirableCondition => 42