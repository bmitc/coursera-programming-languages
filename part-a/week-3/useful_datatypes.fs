type Suit = Club | Diamond | Heart | Spade

type Rank = Jack | Queen | King | Ace | Num of int

type Id = StudentNum of int
        | Name of string
                  * (string option)
                  * string

type Exp = Constant of int
         | Negate   of Exp
         | Add      of Exp * Exp
         | Multiply of Exp * Exp

let rec eval e =
    match e with
          Constant i      -> i
        | Negate e2       -> - (eval e2)
        | Add(e1,e2)      -> (eval e1) + (eval e2)
        | Multiply(e1,e2) -> (eval e1) + (eval e2)

let rec numberOfAdds e =
    match e with
          Constant i      -> 0
        | Negate e2       -> numberOfAdds e2
        | Add(e1,e2)      -> 1 + numberOfAdds e1 + numberOfAdds e2
        | Multiply(e1,e2) -> numberOfAdds e1 + numberOfAdds e2

let exampleExp : Exp = Add (Constant 19, Negate (Constant 4))

let exampleAns : int = eval exampleExp