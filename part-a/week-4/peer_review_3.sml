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

fun only_capitals ls =
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) ls


fun longest_string1 ls =
	foldl (fn (x, y) => if String.size x > String.size y then x else y) "" ls


fun longest_string2 ls =
	foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" ls


(* f is a function as argument, determining how to break tie *)
fun longest_string_helper f ls =
	foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" ls


val longest_string3 = longest_string_helper (fn (x, y) => x > y)


val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


val longest_capitalized = longest_string1 o only_capitals


val rev_string = String.implode o List.rev o String.explode


fun first_answer f [] = raise NoAnswer
  | first_answer f (x :: xs) =
		case f x of
	  		SOME v => v
		  | NONE => first_answer f xs


fun all_answers f [] = SOME []
  | all_answers f ls = 
		let
			fun aux acc [] = SOME acc
			  | aux acc	(NONE :: xs) = NONE
		 	  | aux acc (SOME(x) :: xs) = aux (x @ acc) xs
		in
		  	aux [] (map f ls)
		end


fun count_wildcards p =
	g (fn _ => 1) (fn _ => 0) p


fun count_wild_and_variable_lengths p =
	g (fn _ => 1) (fn x => String.size x) p


fun count_some_var(str, p) =
	g (fn _ => 0) (fn x => if x = str then 1 else 0) p


fun check_pat p =
	let
	  	fun str_ls (Variable v) = [v]
		  | str_ls (TupleP t) = List.foldl (fn (pt, acc) => (str_ls pt) @ acc) [] t
		  | str_ls (ConstructorP (_, cp)) = str_ls cp
		  | str_ls _ = []
		 
		fun noRepeat [] = true
		  | noRepeat (x :: xs) = not (List.exists (fn s => s=x) xs) andalso noRepeat xs
	in
		(noRepeat o str_ls) p
	end


fun match(v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	  | (_, Variable var) => SOME [(var, v)]
	  | (Unit, UnitP) => SOME []
	  | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
	  | (Tuple t, TupleP tp) => if List.length t = List.length tp
	  							then all_answers (fn (t', tp') => match(t', tp')) (ListPair.zip(t, tp))
								else NONE
	  | (Constructor(s1, p1), ConstructorP(s2, p2)) => if s1 = s2 then match(p1, p2) else NONE
	  | _ => NONE



fun first_match v p =
	SOME(first_answer (fn p => match(v, p)) p) handle NoAnswer => NONE