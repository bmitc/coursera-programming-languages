(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2_1 = longest_string1 ["A","bc","C"] = "bc"

val test2_2 = longest_string1 ["A","bc","C","ad"] = "bc"

val test3_1 = longest_string2 ["A","bc","C"] = "bc"

val test3_2 = longest_string2 ["A","bc","C","ad","D"] = "ad"

val test4a_1 = longest_string3 ["A","bc","C"] = "bc"

val test4a_2 = longest_string3 ["this","list","has","no","capital","letters"] = "capital"

val test4b_1 = longest_string4 ["A","B","C"] = "C"

val test4b_2 = longest_string4 ["this","list","has","no","capital","letters"] = "letters"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,1,5,6,7] = NONE

val test8_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1] = SOME [1,1]

val test9a_1 = count_wildcards Wildcard = 1

val test9a_2 = count_wildcards(TupleP  [Wildcard,Variable "a",Wildcard,ConstructorP ("abc",Wildcard)]) = 3

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b_2 = count_wild_and_variable_lengths(TupleP  [Wildcard,Variable "abc",Wildcard,ConstructorP ("abc",TupleP [Wildcard,Variable"a"])]) = 7

val test9c_1 = count_some_var ("x", Variable("x")) = 1

val test9c_2 = count_some_var ("a", TupleP  [Wildcard,Variable "a",Wildcard,ConstructorP ("abc",TupleP [Wildcard,Variable "a"])]) = 2

val test10_1 = check_pat (Variable("x")) = true

val test10_2 = check_pat (TupleP  [Wildcard,Variable "a",Wildcard,ConstructorP ("abc",TupleP [Wildcard,Variable "ab"])]) = true

val test10_3 = check_pat (TupleP  [Wildcard,Variable "a",Wildcard,ConstructorP ("abc",TupleP [Wildcard,Variable "a"])]) = false

val test11_1 = match (Const(1), UnitP) = NONE

val test11_2 = match (Const 4, ConstP 4) = SOME []

val test11_3 = match (Tuple [Const 1, Const 3], TupleP [ConstP 1, ConstP 3]) = SOME []

val test11_4 = match (Tuple [Const 1, Const 3], TupleP [ConstP 1, Variable "y"]) = SOME [("y", Const 3)]

val test12_1 = first_match Unit [UnitP] = SOME []

val test12_2 = first_match (Const 2) [ConstP 1, UnitP, TupleP [Wildcard]] = NONE

val test12_3 = first_match (Const 2) [ConstP 1, Variable "x", Wildcard] = SOME [("x",Const 2)]