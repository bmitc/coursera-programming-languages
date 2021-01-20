let x = 7

(* works only if y >= 0 *)
let rec pow (x : int, y : int) =
    if y=0
    then 1
    else x * pow(x,y-1)

let cube (x : int) =
    pow(x,3)

let sixtyFour = cube(4)

let fortyTwo = pow(2,2+2) + pow(4,2) + cube(2) + 2