fun is_older (date1 : int * int * int, date2 : int * int * int) =
	if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) then (#3 date1) < (#3 date2)
	else if (#1 date1) = (#1 date2) then (#2 date1) < (#2 date2)
	else (#1 date1) < (#1 date2)

fun number_in_month (dates : (int * int * int) list, month : int) =
	if null dates then 0
	else if #2 (hd dates) = month then number_in_month(tl dates, month) + 1
	else number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
	if null months then 0
	else number_in_months(dates, tl months) + number_in_month(dates, hd months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
	if null dates then []
	else if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
	if null months then []
	else dates_in_month(dates, (hd months)) @ dates_in_months(dates, tl months)

fun get_nth (strings : string list, n : int) =
	if n > 1
	then get_nth(tl strings, n - 1)
	else hd strings

fun date_to_string (date : int * int * int) =
	let
		val month_names = ["January", "February", "March", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(month_names, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum (integers : int list, sum : int) =
	let
		fun traverse (last : int, index : int, integers : int list) =
			if null integers then index - 1
			else if sum <= last then index - 1
			else traverse(last + (hd integers), index + 1, tl integers)
	in
		traverse(0, 0, integers)
	end

fun what_month (day : int) =
	let
		val number_of_dates_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum(number_of_dates_in_months, day) + 1
	end

fun month_range (day1 : int, day2 : int) =
	if day1 >= day2 then []
	else [what_month(day1)] @ month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
	if null dates then NONE
	else
		let
			fun find_oldest (dates : (int * int * int) list) =
				if null (tl dates) then hd dates
				else
					let val tl_oldest = find_oldest(tl dates)
					in
						if is_older(tl_oldest, hd dates) then tl_oldest else hd dates
					end
		in
			SOME(find_oldest dates)
		end

fun remove_duplicates [] = []
	| remove_duplicates (x::xs) = x::remove_duplicates(List.filter(fn y => y <> x) xs)

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
	let
		val unduped_months = remove_duplicates(months)
	in
		number_in_months(dates, unduped_months)
	end

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
	let
		val unduped_months = remove_duplicates(months)
	in
		dates_in_months(dates, unduped_months)
	end

fun reasonable_date (date : int * int * int) =
	let
		fun between(number : int, v1 : int, v2 : int) =
			number >= v1 andalso number <= v2
		val days_in_year = if ((#3 date) mod 4) = 0 andalso ((#3 date) mod 100) > 0 then 366 else 365
	in
		(#1 date) > 0 andalso between(#2 date, 1, 12) andalso between(#3 date, 1, days_in_year)
	end
