(* Helper functions. Gets the year, month, or day from a date type. *)
fun get_year (date : int*int*int) = #1 date
fun get_month (date : int*int*int) = #2 date
fun get_day (date : int*int*int) = #3 date

(* Takes two dates and returns true if date1 < date2 and false otherwise. *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
        val year1 = get_year date1
        val year2 = get_year date2
        val month1 = get_month date1
        val month2 = get_month date2
        val day1 = get_day date1
        val day2 = get_day date2
    in
        (year1 < year2) orelse
        (year1 = year2 andalso month1 < month2) orelse
        (year1 = year2 andalso month1 = month2 andalso day1 < day2)
    end

(* Helper function. Takes a date and a month and checks if the date is in the month. *)
fun is_date_in_month (date : int*int*int, month : int) =
    get_month date = month

(* Takes a list of dates and a month and returns the number of dates that are in the month. *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    let
        fun get_increment (date : int*int*int) =
            if is_date_in_month(date, month)
            then 1
            else 0
    in
        if null dates
        then 0
        else get_increment(hd dates) + number_in_month(tl dates, month)
    end

(* Takes a list of dates and a list of months and returns the number of dates
   that are in one of the months. *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Takes a list of dates and a month and returns a list of dates that are in the month. *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
        if is_date_in_month(hd dates, month)
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* Takes a list of dates and a list of months and returns a list of dates that
   are in one of the months. *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Helper function. Takes a generic list and an integer n and returns the nth element of the list. *)
fun get_nth_element (xs : 'a list, n : int) =
    if n = 1
    then hd xs
    else get_nth_element(tl xs, n-1)

(* Takes a list of strings and an integer n and returns the nth element of the list. *)
fun get_nth (strings : string list, n : int) =  get_nth_element(strings,n)

(* Takes a date and returns the date as a string formatted
   as "<month string> <day>, <year>". *)
fun date_to_string (date : int*int*int) =
    let
        val months = ["January",
                      "February",
                      "March",
                      "April",
                      "May",
                      "June",
                      "July",
                      "August",
                      "September",
                      "October",
                      "November",
                      "December"]
    in
        get_nth(months, get_month date) ^ " " ^ Int.toString(get_day date) ^ ", " ^ Int.toString(get_year date)
    end

(* Takes an integer sum and a list of integers and returns the largest number
   of elements, starting from the left, that sum up to less than sum. *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    if hd xs >= sum
    then 0
    else
        let
            val tail = tl xs
            val new_head = (hd xs) + (hd tail)
        in
            1 + number_before_reaching_sum(sum, new_head::(tl tail))
        end

(* Helper function. Returns a list of the number of days in each month with
   a leap year flag. *)
fun number_of_days (is_leap_year : bool) =
    [31,if is_leap_year then 29 else 28,31,30,31,30,31,31,30,31,30,31]

(* Takes a day of the year from 1 to 365 and returns the month it occurs in as an integer. *)
fun what_month (day_of_year : int) =
    (* Add one since number_before_reaching_sum stops before
       reaching day_of_year and not at or after. *)
    number_before_reaching_sum(day_of_year, number_of_days false) + 1

(* Takes two days and returns a list of months that the days range over. *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Helper function. Takes a list of dates and returns the oldest date. *)
fun max_date (dates : (int*int*int) list) =
    if null(tl dates)
    then hd dates
    else
        let val tl_ans = max_date(tl dates)
        in
            if is_older(hd dates, tl_ans)
            then hd dates
            else tl_ans
        end

(* Takes a list of dates and returns the oldest date as an option type. *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else SOME (max_date dates)

(* Helper function. Takes an element and a list and returns true if the
   element is in the list and false otherwise. *)
fun in_list (element : int, xs : int list) =
    if null xs
    then false
    else
        if element = hd xs
        then true
        else in_list(element, tl xs)

(* Helper function. Takes a list of integers and returns a list with duplicates removed. *)
fun remove_duplicates (xs : int list) =
    let
        (* We reverse the list so that the first appearnce of a number is kept,
           and the secondary and beyond appearances are removed. *)
        val reversed_list = rev xs
        (* This function removes duplicates but does so by keeping the last appearance
           of a duplicated number. To do it the other way requires iterating the list
           in the other direction (which is why we reverse it) or by keeping some other
           state. This reversing is probably not the most efficient, but it works. *)
        fun helper (ys : int list) =
            if null ys
            then []
            else
                if in_list(hd ys, tl ys)
                then helper(tl ys)
                else (hd ys) :: helper(tl ys)
    in
        (* Call helper on the reversed_list, and then reverse the retured list to
           return the numbers in their original order of appearance where the first
           appearance of a number is what is kept. *)
        rev(helper reversed_list)
    end

(* Is the same as number_in_months but handles duplicates in the list of months. *)
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

(* Is the same as dates_in_months but handles duplicates in the list of months. *)
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

(* Helper function. Takes two integers x and y and returns true if y divides x,
   i.e., if x/y has remainder 0, and false otherwise. *)
fun divides (x : int, y : int) =
    x mod y = 0

(* Helper function. Takes a year and returns true if its a leap year
   and false otherwise. Made into a separate function to make it easier
   to test. *)
fun is_leap_year (year : int) =
    divides(year,400) orelse (divides(year,4) andalso not(divides(year,100)))

(* Takes a date and returns true if the date describes a real date and false otherwise. *)
fun reasonable_date (date : int*int*int) =
    let
        fun is_good_year (year : int) =
            year > 0
        fun is_good_month (month : int) =
            (1 <= month) andalso (month <= 12)
        fun is_good_day (date_x : int*int*int) =
            let
                val days_in_month = get_nth_element(number_of_days(is_leap_year(get_year date_x)), get_month date_x)
            in
                (1 <= get_day(date_x)) andalso ((get_day date_x) <= days_in_month)
            end
    in
        is_good_year (get_year date) andalso is_good_month (get_month date) andalso is_good_day date
    end