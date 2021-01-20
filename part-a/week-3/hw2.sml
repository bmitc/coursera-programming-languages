(* I disagree with the suggestion to use same_string just to avoid
   providing type annotations. This is especially so since same_string
   itself must use type annotations. So I didn't use it as suggested
   and instead provided a simple type annotation in a helper function. *)

(* Takes a string s and a string list and returns NONE if s is not
   in the list and SOME lst where lst is the same as the input string
   list with s removed. *)
fun all_except_option (s, strings) =
    let
        (* Helper function. Takes an element and list and removes the first
           occurence of the element in the list if it is in the list. *)
        fun remove_element (x : string, xs) =
            case xs of
                  []    => []
                | y::ys => if x=y
                           then ys
                           else y :: remove_element(x, ys)
        val result = remove_element(s,strings)
    in
        if result = strings
        then NONE
        else SOME result
    end

(* Takes a list of list of strings and a string and returns a list of
   strings that are in some list in substitutions that also has the string
   but the string itself should not be in the returned result. *)
fun get_substitutions1 (substitutions, s) =
    case substitutions of
          []    => []
        | x::xs => case all_except_option(s,x) of
                         NONE    => get_substitutions1(xs,s)
                       | SOME ys => ys @ get_substitutions1(xs,s)

(* Same as get_substitutions1 but uses a tail-recursive local helper function. *)
fun get_substitutions2 (substitutions, s) =
    let fun helper (ys,acc) =
        case ys of
              [] => acc
            | x::xs => case all_except_option(s,x) of
                             NONE => helper(xs,acc)
                           | SOME xs' => helper(xs, acc @ xs')
    in
        helper(substitutions,[])
    end

(* Type synonym for the record representing a full name. *)
type full_name = { first  : string,
                   middle : string,
                   last   : string }

(* Takes a list of substitutions for names and a name and returns a
   list of full name records that is a listing of possible similar
   names, including the original name. *)
fun similar_names (substitutions : string list list, name : full_name) =
    let
        val {first=first_name, middle=middle_name, last=last_name} = name
        fun build_names (first_names) =
            case first_names of
                  [] => []
                | x::xs => {first=x, middle=middle_name, last=last_name} :: build_names(xs)
    in
        name :: build_names(get_substitutions2(substitutions, first_name))
    end

(* Types for problem 2. *)

datatype suit = Clubs | Diamonds | Hearts | Spades

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

datatype color = Red | Black

datatype move = Discard of card | Draw

exception IllegalMove

(* I think it's strange we can't use #1 c as the match expression in the below
   two functions. It would remove the uncessary tuple match patterns. E.g.,
   fun card_color (c : card) =
       case #1 c of
             Clubs    => Black
           | Diamonds => Red
           | Hearts   => Red
           | Spades   => Black
   This is cleaner in my opinion, especially since these funtions still
   require type annotations due to ignoring either the suit or rank. *)

(* Also, I have type annotated a few functions because otherwise they would not
   have the correct type. I think the functions should have the type that they
   were intended and written for. *)

(* Takes a card and returns its color. *)
fun card_color (c : card) =
    case c of
          (Clubs,_)    => Black
        | (Diamonds,_) => Red
        | (Hearts,_)   => Red
        | (Spades,_)   => Black

(* Takes a card and returns its value. *)
fun card_value (c : card) =
    case c of
          (_,Num x) => x
        | (_,Ace)   => 11
        | (_,_)     => 10

(* Takes a list of cards, a card c, and an exception and returns a list
   with the first occurrence of c removed and returns an exception if c
   isn't in the list. *)
fun remove_card (cards, c : card, e) =
    case cards of
          []    => raise e (* Never met the card c, so raise the exception *)
        | x::xs => if x=c then xs else x::remove_card(xs,c,e)

(* Takes a list of cards and returns true if all the cards are te same
   color and false otherwise. *)
fun all_same_color cards =
    case cards of
          []       => true (* It isn't specified whether this should be true or false. *)
        | x::[]    => true (* A single-element list has all the same color. *)
        | x::y::xs => card_color(x)=card_color(y) andalso all_same_color (y::xs)
        (* Note that if the first condition results in false, it terminates the recursion. *)

(* Takes a list of cards and returns the total sum of their values. *)
fun sum_cards cards =
    let
        (* Tail-recursive helper function that sums a list of cards. *)
        fun sum (xs, acc) =
            case xs of
                  []    => acc
                | y::ys => sum(ys, acc + card_value(y))
    in
        sum(cards, 0)
    end

(* Takes a list of cards and a goal, an integer, and returns a game score. *)
fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val prelim_score =
            if sum > goal
            then 3 * (sum - goal)
            else (goal - sum)
    in
        if all_same_color cards
        then prelim_score div 2
        else prelim_score
    end

(* Takes a list of cards, list of moves, and a goal and runs the game as described
   in the homework 2 document. Returns the game's score or throws an IllegalMove
   exception in the instance of an illegal move. *)
fun officiate (cards, moves, goal) =
    let
        (* Helper function. Manages the state of the game via tail-call recursion. *)
        fun game_state (card_deck, remaining_moves, held_cards) =
            case remaining_moves of
                  []              => score(held_cards, goal) (* No more moves. Game is done. *)
                | (Discard c)::xs => game_state(card_deck, xs, remove_card(held_cards, c, IllegalMove)) (* Continue game after discarding. *)
                | Draw::xs        =>
                    case card_deck of
                          []    => score(held_cards, goal) (* No more cards to draw. Game is done. *)
                        | y::ys => if sum_cards(y::held_cards) > goal
                                   then score(y::held_cards, goal) (* Game is done. *)
                                   else game_state(ys, xs, y::held_cards) (* Continue game after drawing. *)
    in
        game_state(cards, moves, []) (* Start the game with the initial state. *)
    end