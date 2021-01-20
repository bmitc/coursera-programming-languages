(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

#load "hw1.fsx"

open Hw1

let test1_1 = isOlder ((1,2,3),(2,3,4)) = true

let test1_2 = isOlder ((1990,1,10),(1990,1,11)) = true

let test1_3 = isOlder ((1990,1,10),(1990,1,9)) = false

let test1_4 = isOlder ((1990,1,10),(1989,1,10)) = false

let test1_5 = isOlder ((1988,1,10),(1989,1,10)) = true

let test1_6 = isOlder ((1989,2,10),(1989,1,10)) = false

let test2 = numberInMonth ([(2012,2,28);(2013,12,1)],2) = 1

let test3 = numberInMonths ([(2012,2,28);(2013,12,1);(2011,3,31);(2011,4,28)], [2;3;4]) = 3

let test4_1 = datesInMonth ([(2012,2,28);(2013,12,1)], 2) = [(2012,2,28)]

let test4_2 = datesInMonth ([(2008,1,12);(1998,5,1);(1997,3,16);(2010,5,8)], 5) = [(1998,5,1);(2010,5,8)]

let test5 = datesInMonths ([(2012,2,28);(2013,12,1);(2011,3,31);(2011,4,28)], [2;3;4]) = [(2012,2,28);(2011,3,31);(2011,4,28)]

let test6_1 = getNth (["hi"; "there"; "how"; "are"; "you"], 2) = "there"

let test6_2 = getNth (["1";"2";"3";"4";"5";"6"], 1) = "1"

let test6_3 = getNth (["1";"2";"3";"4";"5";"6"], 6) = "6"

let test7_1 = dateToString (2013, 6, 1) = "June 1, 2013"

let test7_2 = dateToString (1990, 09, 20) = "September 20, 1990"

let test8_1 = numberBeforeReachingSum (10, [1;2;3;4;5]) = 3

let test8_2 = numberBeforeReachingSum (2, [1;2;3;4;5]) = 1

let test8_3 = numberBeforeReachingSum (1, [1;2;3;4;5]) = 0

let test9 = whatMonth 70 = 3

let test10_1 = monthRange (31, 34) = [1;2;2;2]

let test10_2 = monthRange (230, 14) = []

let test11 = oldest([(2012,2,28);(2011,3,31);(2011,4,28)]) = Some (2011,3,31)

let test12_1 = numberInMonthsChallenge ([(2012,2,28);(2013,12,1);(2011,3,31);(2011,4,28)], [2;3;4;3;2;4]) = 3

let test12_2 = datesInMonthsChallenge ([(2012,2,28);(2013,12,1);(2011,3,31);(2011,4,28)], [2;3;4;2;2;3;4;3]) = [(2012,2,28);(2011,3,31);(2011,4,28)]

(* Test cases from https://support.microsoft.com/en-us/help/214019/method-to-determine-whether-a-year-is-a-leap-year *)
let test13_1 = ((isLeapYear 1600) &&
                (isLeapYear 1988) &&
                (isLeapYear 1992) &&
                (isLeapYear 1996) &&
                (isLeapYear 2040)) = true

let test13_2 = ((isLeapYear 1700) ||
                (isLeapYear 1900) ||
                (isLeapYear 2100) ||
                (isLeapYear 2300) ||
                (isLeapYear 1999) ||
                (isLeapYear 2500)) = false

let test13_3 = reasonableDate (2018,1,32) = false

let test13_4 = reasonableDate (2001,11,1) = true

let test13_5 = reasonableDate (2000,2,29) = true

let test13_6 = reasonableDate (1990,2,28) = true

let test13_7 = reasonableDate (1990,2,29) = false