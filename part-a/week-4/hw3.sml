(* Helper function. Takes a string and returns true if the first character
   of the string is capitalized and false otherwise. *)
fun is_capitalized s = Char.isUpper(String.sub(s,0))

(* Takes a list of strings and returns a list containing only those strings
   that start with a capital letter. *)
val only_capitals = List.filter is_capitalized

(* Takes a list of strings and returns the longest string.
   Returns the first instance of the longest string when there's a tie. *)
fun longest_string1 xs =
    let
        (* Returns the largest string between the accumulator and current element.
           Favors the accumulator when there's a tie. *)
        fun nextAcc (s, acc) =
            if String.size s > String.size acc
            then s
            else acc
    in
        List.foldl nextAcc "" xs
    end

(* Takes a list of strings and returns the longest string.
   Returns the last instance of the longest sting when there's a tie. *)
fun longest_string2 xs =
    let
        (* Returns the longer string between the accumulator and current element.
           Favors the current element when there's a tie. *)
        fun nextAcc (s, acc) =
            if String.size s >= String.size acc
            then s
            else acc
    in
        List.foldl nextAcc "" xs
    end

(* Takes a string length compare function and a string list and returns a
   string that satisfies the compare function. When the compare function is
   >, the behavior should match longest_string1. When the compare function
   is >=, the behavior should match longest_string2. *)
fun longest_string_helper compare xs =
    let
        fun nextAcc (s, acc) =
            if compare(String.size s, String.size acc)
            then s
            else acc
    in
        List.foldl nextAcc "" xs
    end

(* Takes a list of strings and returns the longest string.
   Returns the first instance of the longest string when there's a tie. *)
val longest_string3 = longest_string_helper op>

(* Takes a list of strings and returns the longest string.
   Returns the last instance of the longest sting when there's a tie. *)
val longest_string4 = longest_string_helper op>=

(* Takes a list of strings and returns the longest capitalized string or
   the empty list when there are no capitalized strings. *)
val longest_capitalized = longest_string3 o only_capitals

(* Here's a piped version of the above function. *)
infix |>
fun x |> f = f x
fun longest_capitalized_piped xs = only_capitals xs |> longest_string3

(* Takes a string and returns the string reversed. *)
val rev_string = implode o List.rev o explode

exception NoAnswer

(* Takes a function and a list and returns v for the first element of the list
   that returns SOME v when the function is applied to it and raises the
   NoAnswer exception otherwise. *)
fun first_answer f xs =
    case xs of
          []    => raise NoAnswer (* Either given empty list or found all NONE values *)
        | y::ys => case f y of
              NONE   => first_answer f ys (* No answer, keep looking. *)
            | SOME v => v (* Found an answer, so return it. *)

(* Takes a function and a list and returns NONE if the function returns NONE
   when applied to any element of the list. Otherwise, returns SOME lst where
   lst is ls1,ls2,...,lstn when the function returns SOME lst1, SOME lst2, ...,
   SOME lstn when applied to each element of the list. *)
fun all_answers f xs =
    let
        (* Applies f to each element of a list. Returns an accumulated SOME list
           or NONE if f returns NONE for any element. *)
        fun helper acc xs' =
            case xs' of
                  []    => SOME acc
                | y::ys => case f y of
                      NONE     => NONE
                    | SOME lst => helper (acc @ lst) ys
    in
        helper [] xs
    end

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

(* Helper function. Takes a function to count Wildcard patterns, a function
   to count the value of Variable patterns, and a pattern and returns the
   total count for all the Wildcard and Variable patterns in the pattern. *)
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

(* Takes a pattern and returns how many Wildcard patterns it contains. *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* Takes a pattern and returns how many Wildcard patterns it contains plus
   the total sum of the string lengths in the Variable patterns. *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* Takes a string and a pattern and returns the number of times the string
   appears as a Variable in the pattern. *)
fun count_some_var (str,pat) = g (fn () => 0) (fn s => if s = str then 1 else 0) pat

(* Helper function. Takes a list and returns true if it has repeats and false otherwise. *)
fun has_repeats lst =
    case lst of
          []    => false
        | x::xs => List.exists (fn e => e = x) xs orelse has_repeats xs

(* Takes a pattern and returns true if all the Variables in the pattern are
   distinct and false otherwise. *)
fun check_pat pat =
    let
        (* Helper function. Takes a pattern and returns a list of the pattern's variable values. *)
        fun get_variable_list patt =
            case patt of
                  Variable x        => [x]
                | TupleP ps         => List.foldl (fn (e,acc) => acc @ get_variable_list e) [] ps
                | ConstructorP(_,y) => get_variable_list y
                | _                 => []
    in
        get_variable_list pat |> has_repeats |> not
    end

(* Takes a value and a pattern and returns SOME lst where lst is a list
   of the match bindings and NONE if there are no matches. *)
fun match (v,p) =
    case (v,p) of
          (_, Wildcard)         => SOME []
        | (v, Variable s)       => SOME [(s,v)]
        | (Unit, UnitP)         => SOME []
        | (Const x, ConstP y)   => if x=y then SOME [] else NONE
        | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                   then all_answers match (ListPair.zip(vs,ps))
                                   else NONE
        | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2 then match(v,p) else NONE
        | _ => NONE

(* Takes a value and a list of patterns and returns SOME list of bindings
   for the first mach made in the pattern list and NONE if there are
   no matches. *)
fun first_match v ps =
    SOME (first_answer (fn x => match(v,x)) ps)
    handle NoAnswer => NONE