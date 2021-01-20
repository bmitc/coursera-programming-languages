(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Const of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard         => f1 ()
          | Variable x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

fun only_capitals sl = List.filter (fn x => (Char.isUpper (String.sub (x, 0)))) sl

fun longest_string1 sl = foldl (fn (x, y) => if String.size x > String.size y
                                             then x else y) "" sl

fun longest_string2 sl = foldl (fn (x, y) => if String.size x >= String.size y
                                             then x else y) "" sl

fun longest_string_helper pred sl =
  foldl (fn (x, y) => if pred (String.size x, String.size y) then x else y) "" sl

(* note to use val binding instead of function binding to do partial application *)
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

(*
val caps_no_X_string = String.implode o (List.filter (fn x => #"X" <> x))
                                      o (List.map Char.toUpper) o String.explode
 *)

val rev_string = String.implode o rev o String.explode

fun first_answer pred lst =
  case lst of
       [] => raise NoAnswer
     | x::lst => case (pred x) of
                      SOME v => v
                    | NONE => first_answer pred lst

fun all_answers pred lst =
  let fun accum pred lst acc =
                      case lst of
                           [] => SOME acc
                         | x::lst => case (pred x) of
                                          NONE => NONE
                                        | SOME v => accum pred lst (acc @ v)
  in accum pred lst [] end

(* 9.a g takes two function f1: fn unit -> int and f2: fn string -> int, and a
 * variable of type pattern, and it applys f1 to every Wildcard in pattern and
 * f2 to every Variable recursively and sums the result up as return *)

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) String.size

fun count_some_var (str, patt) =
  g (fn x => 0) (fn x => if x = str then 1 else 0) patt

fun check_pat patt =
  let
    fun extract patt =
      case patt of
           ConstructorP (_, patt) => extract patt
         | Variable x => [x]
         | TupleP patts => foldl (fn (x, y) => ((extract x) @ y)) [] patts
         | _ => []
    fun check_redundance xs prev =
      case xs of
           [] => false
         | x::xs => (List.exists (fn y => x = y) prev) orelse
                    (check_redundance xs (x::prev))
  in
    not (check_redundance (extract patt) [])
  end

fun match (valu, patt) =
  case (valu, patt) of
       (_, Variable a) => SOME [(a, valu)]
     | (_, Wildcard) => SOME []
     | (Unit, UnitP) => SOME []
     | (Const x, ConstP y) => if x = y then SOME [] else NONE
     | (Constructor (a, x), ConstructorP (b, y)) => if a = b then match (x, y)
                                                   else NONE
     | (Tuple xs, TupleP ps) => if List.length xs <> List.length ps then NONE
                                else all_answers match (ListPair.zip (xs, ps))
     | _ => NONE

fun first_match valu patts =
  SOME (first_answer (fn patt => (match (valu, patt))) patts) handle NoAnswer => NONE
