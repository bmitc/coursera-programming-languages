type MUPLExpression =
    | Variable of string
    | Integer of int
    | Add of MUPLExpression * MUPLExpression
    | IfGreater of MUPLExpression * MUPLExpression * MUPLExpression * MUPLExpression
    | Function of string * string * MUPLExpression
    | Call of MUPLExpression * MUPLExpression
    | Mlet of string * MUPLExpression * MUPLExpression
    | Apair of MUPLExpression * MUPLExpression
    | Fst of MUPLExpression
    | Snd of MUPLExpression
    | Aunit
    | Isaunit
    
let rec fsharpListToMuplList lst =
    if List.isEmpty lst
    then Aunit
    else Apair (List.head lst, fsharpListToMuplList (List.tail lst))

let rec MuplListToRacketList mlist =
    match mlist with
    | Aunit -> List.empty
    | Apair(car, cdr) -> car :: MuplListToRacketList (cdr)