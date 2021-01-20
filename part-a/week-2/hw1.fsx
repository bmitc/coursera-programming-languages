// Helper functions. Gets the year, month, or day from a date type.
// These functions use F#'s tuple pattern matching.
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/tuples#obtaining-individual-values
let getYear ((year,_,_) : int*int*int) = year
let getMonth ((_,month,_) : int*int*int) = month
let getDay ((_,_,day) : int*int*int) = day

/// Takes two dates and returns true if date1 < date2 and false otherwise.
let isOlder (date1 : int*int*int, date2 : int*int*int) =
    let year1 = getYear date1
    let year2 = getYear date2
    let month1 = getMonth date1
    let month2 = getMonth date2
    let day1 = getDay date1
    let day2 = getDay date2
    (year1 < year2) || (year1 = year2 && month1 < month2) || (year1 = year2 && month1 = month2 && day1 < day2)

/// Helper function. Takes a date and a month and checks if the date is in the month.
let isDateInMonth (date : int*int*int, month : int) =
    getMonth date = month

/// Takes a list of dates and a month and returns the number of dates that are in the month.
let rec numberInMonth (dates : (int*int*int) list, month : int) =
    let getIncrement (date : int*int*int) =
        if isDateInMonth(date, month)
        then 1
        else 0
    if List.isEmpty dates
    then 0
    else getIncrement(List.head dates) + numberInMonth(List.tail dates, month)

/// Takes a list of dates and a list of months and returns the number of dates that are in one of the months.
let rec numberInMonths (dates : (int*int*int) list, months : int list) =
    if List.isEmpty months
    then 0
    else numberInMonth(dates, List.head months) + numberInMonths(dates, List.tail months)

/// Takes a list of dates and a month and returns a list of dates that are in the month.
let rec datesInMonth (dates : (int*int*int) list, month : int) =
    if List.isEmpty dates
    then []
    else
        if isDateInMonth(List.head dates, month)
        then (List.head dates) :: datesInMonth(List.tail dates, month)
        else datesInMonth(List.tail dates, month)

/// Takes a list of dates and a list of months and returns a list of dates that are in one of the months.
let rec datesInMonths (dates : (int*int*int) list, months : int list) =
    if List.isEmpty months
    then []
    else datesInMonth(dates, List.head months) @ datesInMonths(dates, List.tail months)

/// Helper function. Takes a generic list and an integer n and returns the nth element of the list.
let rec getNthElement (xs : 'a list, n : int) =
    if n = 1
    then List.head xs
    else getNthElement(List.tail xs, n-1)

/// Takes a list of strings and an integer n and returns the nth element of the list.
let getNth (strings : string list, n : int) =  getNthElement(strings,n)

/// Takes a date and returns the date as a string formatted as "<month string> <day>, <year>".
let dateToString (date : int*int*int) =
    let months = ["January";
                  "February";
                  "March";
                  "April";
                  "May";
                  "June";
                  "July";
                  "August";
                  "September";
                  "October";
                  "November";
                  "December"]
    getNth(months, getMonth date) + " " + string(getDay date) + ", " + string(getYear date)

/// Takes an integer sum and a list of integers and returns the largest number
/// of elements, starting from the left, that sum up to less than sum.
let rec numberBeforeReachingSum (sum : int, xs : int list) =
    if List.head xs >= sum
    then 0
    else
        let tail = List.tail xs
        let newHead = (List.head xs) + (List.head tail)
        1 + numberBeforeReachingSum(sum, newHead::(List.tail tail))

/// Helper function. Returns a list of the number of days in each month with a leap year flag.
let numberOfDays (isLeapYear : bool) =
    [31; (if isLeapYear then 29 else 28) ;31;30;31;30;31;31;30;31;30;31]

/// Takes a day of the year from 1 to 365 and returns the month it occurs in as an integer. *)
let whatMonth (dayOfYear : int) =
    (*Add one since numberBeforeReachingSum stops before
      reaching dayOfYear and not at or after.*)
    numberBeforeReachingSum(dayOfYear, numberOfDays false) + 1

/// Takes two days and returns a list of months that the days range over.
let rec monthRange (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else whatMonth(day1) :: monthRange(day1 + 1, day2)

/// Helper function. Takes a list of dates and returns the oldest date.
let rec maxDate (dates : (int*int*int) list) =
    if List.isEmpty(List.tail dates)
    then List.head dates
    else
        let tlAns = maxDate(List.tail dates)
        if isOlder(List.head dates, tlAns)
        then List.head dates
        else tlAns

/// Takes a list of dates and returns the oldest date as an option type.
let oldest (dates : (int*int*int) list) =
    if List.isEmpty dates
    then None
    else Some (maxDate dates)

/// Helper function. Takes an element and a list and returns true if the
/// element is in the list and false otherwise.
let rec inList (element : int, xs : int list) =
    if List.isEmpty xs
    then false
    else
        if element = List.head xs
        then true
        else inList(element, List.tail xs)

/// Helper function. Takes a list of integers and returns a list with duplicates removed.
let removeDuplicates (xs : int list) =
    (* We reverse the list so that the first appearnce of a number is kept,
       and the secondary and beyond appearances are removed. *)
    let reversedList = List.rev xs
    (* This function removes duplicates but does so by keeping the last appearance
       of a duplicated number. To do it the other way requires iterating the list
       in the other direction (which is why we reverse it) or by keeping some other
       state. This reversing is probably not the most efficient, but it works. *)
    let rec helper (ys : int list) =
        if List.isEmpty ys
        then []
        else
            if inList(List.head ys, List.tail ys)
            then helper(List.tail ys)
            else (List.head ys) :: helper(List.tail ys)
    (* Call helper on the reversedList, and then reverse the retured list to
       return the numbers in their original order of appearance where the first
       appearance of a number is what is kept. *)
    List.rev(helper reversedList)

/// Is the same as numberInMonths but handles duplicates in the list of months.
let numberInMonthsChallenge (dates : (int*int*int) list, months : int list) =
    numberInMonths(dates, removeDuplicates(months))

/// Is the same as datesInMonths but handles duplicates in the list of months.
let datesInMonthsChallenge (dates : (int*int*int) list, months : int list) =
    datesInMonths(dates, removeDuplicates(months))

/// Helper function. Takes two integers x and y and returns true if y divides x,
/// i.e., if x/y has remainder 0, and false otherwise.
let divides (x : int, y : int) =
    x % y = 0

/// Helper function. Takes a year and returns true if its a leap year and false otherwise.
/// Made into a separate function to make it easier to test.
let isLeapYear (year : int) =
    divides(year,400) || (divides(year,4) && not(divides(year,100)))

/// Takes a date and returns true if the date describes a real date and false otherwise.
let reasonableDate (date : int*int*int) =
    let isGoodYear (year : int) =
        year > 0
    let isGoodMonth (month : int) =
        (1 <= month) && (month <= 12)
    let isGoodDay (date_x : int*int*int) =
        let daysInMonth = getNthElement(numberOfDays(isLeapYear(getYear date_x)), getMonth date_x)
        (1 <= getDay(date_x)) && ((getDay date_x) <= daysInMonth)
    isGoodYear (getYear date) && isGoodMonth (getMonth date) && isGoodDay date