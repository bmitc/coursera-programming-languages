(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, lst) = 
case lst of
    [] => NONE
    | x::lst => case  same_string (x, str) of
        true => SOME lst
    |   false => case all_except_option(str, lst) of
            NONE => NONE
        |   SOME y => SOME (x::y)

fun  get_substitutions1(str_lsts, str)=
case str_lsts of
    [] => []
    | lst::str_lsts => let val tail_result = get_substitutions1(str_lsts, str)
        in
        case all_except_option(str, lst) of
            SOME x => x @ tail_result
            | NONE => tail_result
        end

fun  get_substitutions2(str_lsts, str)=
    let fun aux(str_lsts, str, acc) =
        case str_lsts of
            [] => acc
            | lst::str_lsts => case all_except_option(str, lst) of
                SOME x => aux(str_lsts, str, x @ acc)
                | NONE => aux(str_lsts, str, acc)
    in
        aux (str_lsts, str, [])
    end

fun similar_names(subs, full_name )=
case full_name of
    {first, middle, last} => let fun helper(lst) =
        case lst of 
            [] => []
            | x::lst => {first= x, middle= middle, last = last} :: helper(lst)
    in
        full_name :: helper(get_substitutions1(subs, first))
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
fun card_color(c) =
case c of
    (Clubs, _) => Black
    | (Spades, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red

fun card_value(c) =
case c of
    (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11
    | (_, Num x) => x

fun remove_card(cs, c, e) =
case cs of
    [] => raise e
    | cs::cs' => case cs = c of
        true => cs'
    |   false => case remove_card(cs', c, e) of
        y => (cs::y)

fun all_same_color(cs) =
    case cs of
	[] => true
      | c::[] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color (neck::rest)) 

fun sum_cards(cs) = 
    let fun aux(cs, acc) =
        case cs of
            [] => acc
            | cs::cs' => aux (cs',(acc + card_value(cs)))
    in
        aux (cs, 0)
    end

fun score(cs, goal) =
    let val pre_score =
        let val sum = sum_cards(cs) 
        in
            case sum > goal of
            true => 3 * (sum-goal)
            | _ => goal-sum
        end
    in
        case (all_same_color(cs)) of
            true => pre_score div 2
            | false => pre_score
    end

fun officiate(cs, ms, goal) =
    let fun aux(cs, ms, goal, hs) =
        case sum_cards(hs) > goal of
        true => score(hs, goal)
        | _ => case ms of
            [] => score(hs, goal)
            | ms::ms' => case ms of
                Discard(c) => aux(cs, ms', goal, remove_card(hs, c, IllegalMove))
                | Draw => case cs of
                    [] => score(hs, goal)
                    | cs::cs' => aux(cs', ms', goal, cs::hs)
    in
        aux (cs, ms, goal, [])
    end

