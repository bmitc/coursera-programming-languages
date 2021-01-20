fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*val all_except_option = fn : string * string list -> string list option *)
fun all_except_option(s1, s2_list)=
    case s2_list of
        [] => NONE
      | s2::s2_list' => let
          val smaller = all_except_option(s1,s2_list')
          val matches = same_string(s1, s2)
      in
          case (smaller, matches) of
              (NONE, false) => NONE
            | (NONE, true) => SOME (s2_list')
            | (SOME(e1), _) => SOME(s2::e1)
      end

fun get_substitutions1(s_list, s)=
    case s_list of
        []=> []
      | s_l::s_list' =>
        case all_except_option(s, s_l) of
            NONE => get_substitutions1(s_list',s)
          | SOME(e1) => e1 @ get_substitutions1(s_list',s)


fun get_substitutions2(s_list, s) =
    let fun aux(s_list, s, acc) =
            case s_list of
                []=> acc
              | s2::s_list' =>
                case all_except_option(s,s2) of
                    NONE => aux(s_list',s, acc)
                  | SOME (e1) => aux(s_list', s, e1@acc)
    in
        aux(s_list, s, [])
    end

fun similar_names (s_list, {first=s1, middle=s2, last=s3})=
    let
        val substituitions = get_substitutions2(s_list, s1)
        fun helper(substituitions, {first=s1, middle=s2, last=s3}) =
            case substituitions of
                []=> [{first=s1, middle=s2, last=s3}]
              | s::substituitions_rest =>
                helper(substituitions_rest, {first=s1, middle=s2, last=s3})@ [{first=s, middle=s2, last=s3}]
    in

       helper(substituitions, {first=s1, middle=s2, last=s3})
    end



datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color card =
    case card of
        (Clubs, _)=> Black
      | (Spades, _) => Black
      | (_,_) => Red

fun card_value card =
    case card of
        (_,Num i) => i
      | (_,Ace) => 11
      | (_,_) => 10

fun remove_card input_tuple = (* (cs, c, e) *)
    case input_tuple of
        ([],_,e)=> raise e
      | (c2::cs', c, e) => let
          val compare = (c2=c)
      in

          case compare of
              true => cs'
            | false => c2::remove_card(cs',c,e)
      end

fun all_same_color cs =
    case cs of
        [] => true
      | (c2::[]) => true
      | (c2::c3::cs_rest) =>
        all_same_color(c3::cs_rest) andalso (card_color(c2) = card_color(c3))

fun sum_cards cs =
    let fun aux(cs, acc) =
            case cs of
                []=> acc
              | c::cs_rest => aux(cs_rest, card_value c + acc)
    in
        aux(cs, 0)
    end

fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val pre_score = if sum>goal then 3*(sum-goal) else goal-sum

    in
        case all_same_color cs of
             true => pre_score div 2
           | false => pre_score
    end

fun officiate (cs, ms, goal)=
    let

        fun helper (cs,ms, hand_list, goal)=
            case ms of
                [] => score(hand_list, goal)
              | Draw::ms_rest  => (case cs of
                                       []=> score(hand_list,goal)
                                     | c1::cs_rest =>
                                       if sum_cards(c1::hand_list)>goal then
                                           score(c1::hand_list, goal) else
                                       helper(cs_rest, ms_rest, c1::hand_list, goal ))
              | Discard card :: ms_rest  =>
                helper(cs, ms_rest, remove_card(hand_list, card, IllegalMove), goal)
    in
        helper(cs,ms,[], goal)

    end