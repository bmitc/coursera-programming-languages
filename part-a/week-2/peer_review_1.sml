fun is_older(d1: int*int*int, d2: int*int*int) =
  if #1 d1 = #1 d2
  then
    if #2 d1 = #2 d2 then #3 d1 < #3 d2
    else #2 d1 < #2 d2
  else
    #1 d1 < #1 d2

fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates then 0
  else
    if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)

fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null dates then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates then []
  else
    if #2 (hd dates) = month then hd dates :: dates_in_month(tl dates, month)
else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null dates then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, position: int) =
  if position = 1 then hd strings
  else get_nth(tl strings, position - 1)

fun date_to_string(date: int*int*int) =
  let
    val months = [
      "January","February","March","April","May","June","July","August","September","October","November","December"
    ]
  in
    get_nth(months, #2 date)
    ^ " " ^ Int.toString(#3 date)
    ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum: int, values: int list) =
  let
    fun count(values: int list, acc: int, n: int) =
      if hd values + acc >= sum then n
      else count(tl values, acc + hd values, n + 1)
  in
    count(values, 0, 0)
  end

fun what_month(day_of_year: int) =
  let
    val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day_of_year, days_in_month) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) =
  if null dates then NONE
  else
    let
      val tl_ans = oldest(tl dates)
    in
      if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
      else SOME (hd dates)
    end

fun delete(element: int, elements: int list) =
  if null elements then []
  else
    if element = hd elements then delete(element, tl elements)
    else hd elements :: delete(element, tl elements)

fun remove_duplicates(months: int list) =
  if null months then []
  else hd months :: remove_duplicates(delete(hd months, tl months))

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(date: int*int*int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    fun is_leap(year: int) =
      year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0

    fun get_nth(ints: int list, n: int) =
      if n = 1 then hd ints
      else get_nth(tl ints, n - 1)

    fun is_valid_year(year: int) = year > 0
    fun is_valid_month(month: int) = month >= 1 andalso month <= 12
    fun is_valid_day(day: int) = day >= 1 andalso day <= 31

    fun is_valid_date() =
      if is_leap(#1 date) then (#3 date) <= get_nth(days_in_month_leap, #2 date)
      else (#3 date) <= get_nth(days_in_month, #2 date)
  in
    is_valid_year(#1 date) andalso is_valid_month(#2 date)
    andalso is_valid_day(#3 date) andalso is_valid_date()
  end