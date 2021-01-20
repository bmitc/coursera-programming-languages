(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals (slist) = 
    List.filter (fn x => (Char.isUpper(String.sub(x, 0)))) slist

(* 2 *)
fun longest_string1 (slist) = 
     List.foldl (fn (x, y) => if (String.size x > String.size y) then x else y) "" slist 

(* 3 *)
fun longest_string2 (slist) = 
     List.foldl (fn (x, y) => if (String.size y > String.size x) then y else x) "" slist 

(* 4 *)
fun longest_string_helper f = 
    if f (8,7)
    then fn slist => List.foldl (fn (x, y) => if (String.size x > String.size y) then x else y) "" slist 
    else fn slist => List.foldl (fn (x, y) => if (String.size y > String.size x) then y else x) "" slist

(* a *)
val longest_string3 = longest_string_helper(fn (x,y) => x > y)

(* b *)
val longest_string4 = longest_string_helper(fn (x,y) => y > x)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f slist = 
    case slist of
	[] => raise NoAnswer
     |  x::xs' => 
	   case f x of
	       SOME x => x
	    |  NONE => first_answer f xs'
	

(* 8 *)
fun all_answers f slist = 
    case slist of
	[] => SOME []
     |  p::ps => 
            let fun lstacc (acc, xs) = 
	            case xs of 
		        [] => SOME acc
	             |  x::xs' => 
		           case f x of
		               SOME k => lstacc((k@acc), xs')
		            |  NONE => NONE
            in 
	        lstacc ([], slist)
            end 
	    
(* 9 *)

(* a *)
val count_wildcards  = 
    let fun lstacc (x) = 
	    g (fn y => x + 1) (fn y => x)
    in
	lstacc 0
    end 

(* b *)
val count_wild_and_variable_lengths = 
    let fun lstacc (x) = 
	    g (fn y => x + 1) (fn y => x + String.size y)
    in
	lstacc 0
    end

(* c *)
fun count_some_var (str, patt) = 
    let fun lstacc (x) = 
	    g (fn y => x) (fn y => if y = str then x + 1 else x) patt
    in
	lstacc 0
    end



(* 10 *)
fun check_pat patt =
    let fun lstacc patt = 
	case patt of
	    Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p,i) => (lstacc p) @ i) [] ps
	  | ConstructorP(_,p) => lstacc p
	  | _                 => []
    in
	let fun exists (p) = 
		case p of 
		    [] => true
		 |  q::qs' => 
		        case List.exists (fn y => y = q) qs' of 
			    true => false
			 |  false => exists (qs')
        in 
		exists (lstacc patt)
        end
    end
(*  
fun all_answers f xs =
    let fun loop (acc,xs) =
        case xs of
		        [] => SOME acc
	        | x::xs' => case f x of 
                          NONE => NONE
              			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end
*)
(* 11 *)
fun match (value, patt) = 
    case (value, patt) of
	(_, Wildcard) => SOME []
     |  (_, Variable s) => SOME [(s, value)]
     |  (Unit, UnitP) => SOME []
     |  (Const x, ConstP y) => if x = y then SOME [] else NONE
     |  (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
						     then match (v, p)
						     else NONE
     |  (Tuple (vs), TupleP (ps)) => if length vs = length ps 
				 then all_answers match (ListPair.zip(vs,ps))
				 else NONE 
     |  _ => NONE 
	

fun first_match value plist =
    SOME (first_answer (fn patt => match (value, patt)) plist) handle NoAnswer => NONE 