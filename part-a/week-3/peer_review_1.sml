(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*aux function to reduce using of @ operator*)
fun reverse (l1) =
    let fun aux ([], l2) = l2 
	  | aux (l::ls, l2) = aux(ls, l::l2)
    in
	aux (l1, [])
    end
	

(*Problem 1a*)	    
fun all_except_option (name, names) =
    let fun aux ([], res) = NONE
	  | aux (n::ns, res) =
	    case same_string (n, name) of
		true => SOME (reverse(res) @ ns)
	      | _ =>  aux(ns, n::res)
    in
	aux (names, [])
    end

(*Problem 1b*)
fun get_substitutions1 (names, name) =
    case names of
	[] => []
        | n::ns => case all_except_option (name, n) of
		     NONE => get_substitutions1 (ns, name)
		    |  SOME (res)  => res @ get_substitutions1(ns, name)
								      

(*Problem 1c*)
fun get_substitutions2 (names, name) =
    let fun aux ([], res) = res 
	  | aux  (n::ns, res) =
	    case all_except_option (name, n) of
		NONE => aux (ns, res)
	      | SOME (result)  => aux (ns, res @ result)
    in
	aux (names, [])
    end
	
(*Problem 1d*)
fun similar_names (names, {first=f, middle=m, last=l}) =
    let fun aux ([], res) = reverse(res) 
	  | aux (n::ns, res) = aux(ns, {first=n, middle=m, last=l}::res)
    in
	   aux(get_substitutions2(names, f), [{first=f, middle=m, last=l}])
    end
			   
    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*Problem 2a*)
fun card_color(c) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

(*Problem 2b*)		 
fun card_value (c) =
    case c of
	(_, Num c_rank) => c_rank
      | (_, Ace) => 11
      | _ => 10

(*Problem 2c*)
fun remove_card (cs, c, e) =
    let fun aux (cards, res) =
	case cards of
	    [] => if (reverse(res) = cs ) then raise e
				 else reverse(res)
            |  crd::cards' => if (crd = c) then aux (cards', res)
						 else aux (cards', crd::res)
    in
	aux (cs, [])
    end

(*Problem 2d*)
fun all_same_color (cards) =
    case cards of
      [] => true
      | c1::[] => true
      | c1::c2::cards' => if card_color(c1) = card_color(c2)
			  then all_same_color (c1::cards')
			  else false
				   
(*Problem 2e*)
fun sum_cards (cards) =
    let fun aux ([], sum) = sum 
	  | aux (c::cards', sum) = aux(cards', sum + card_value(c))
    in
	aux (cards, 0)
    end
	
(*Problem 2g*)
fun score (cards, goal) =
    let
	val sum = sum_cards(cards)
	val pre_score = if (sum > goal)
		      then 3 * (sum - goal)
		      else (goal - sum)
    in
	case all_same_color(cards) of
	    true => pre_score div 2
	    | _ => pre_score 
    end
      
(*Problem 2f*)

fun officiate(cards, moves, goal) =
    let fun aux(cards, hand, moves) =
            case moves of
                [] => hand
              | Discard c::moves' => aux(cards, remove_card(hand, c, IllegalMove), moves')
              | Draw::moves' =>
                case cards of
                    [] => hand
                  | c::deck =>
                    if (card_value(c) + sum_cards(hand)) > goal
                    then c::hand
		    else aux(deck, c::hand, moves')
    in
        score(aux(cards, [], moves), goal)
    end