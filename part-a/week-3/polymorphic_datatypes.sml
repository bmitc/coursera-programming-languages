datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a,'b) tree =
      Node of 'a * ('a,'b) tree * ('a,'b) tree
    | Leaf of 'b

(* int list -> int *)
fun sum_list xs =
    case xs of
          [] => 0
        | x::xs' => x + sum_list xs'

(* 'a list * 'a list -> 'a list *)
fun append (xs,ys) =
    case xs of
          [] => ys
        | x::xs' => x :: append(xs',ys)

(* (int,int) tree -> int *)
fun sum_tree tr =
    case tr of
          Leaf i => i
        | Node(i,lft,rgt) => i + sum_tree lft + sum_tree rgt

(* ('a,int) tree -> int *)
fun sum_leaves tr =
    case tr of
          Leaf i => i
        | Node(i,lft,rgt) => sum_leaves lft + sum_leaves rgt