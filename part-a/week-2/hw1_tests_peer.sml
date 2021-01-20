(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_1 = is_older ((1,2,3),(2,3,4)) = true

val test1_2 = is_older ((1990,1,10),(1990,1,11)) = true

val test1_3 = is_older ((1990,1,10),(1990,1,9)) = false

val test1_4 = is_older ((1990,1,10),(1989,1,10)) = false

val test1_5 = is_older ((1988,1,10),(1989,1,10)) = true

val test1_6 = is_older ((1989,2,10),(1989,1,10)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test4_2 = dates_in_month ([(2008,1,12),(1998,5,1),(1997,3,16),(2010,5,8)],5) = [(1998,5,1),(2010,5,8)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test6_2 = get_nth (["1","2","3","4","5","6"], 1) = "1"

val test6_3 = get_nth (["1","2","3","4","5","6"], 6) = "6"

val test7_1 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test7_2 = date_to_string (1990, 09, 20) = "September 20, 1990"

val test8_1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test8_2 = number_before_reaching_sum (2, [1,2,3,4,5]) = 1

val test8_3 = number_before_reaching_sum (1, [1,2,3,4,5]) = 0

val test9 = what_month 70 = 3

val test10_1 = month_range (31, 34) = [1,2,2,2]

val test10_2 = month_range (230, 14) = []

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12_1 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,3,2,4]) = 3

val test12_2 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,2,3,4,3]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test13_3 = reasonable_date (2018,1,32) = false

val test13_4 = reasonable_date (2001,11,1) = true

val test13_5 = reasonable_date (2000,2,29) = true

val test13_6 = reasonable_date (1990,2,28) = true

val test13_7 = reasonable_date (1990,2,29) = false