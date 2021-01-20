fun is_older(x : int*int*int, y : int*int*int) =
  if (#1 x) <> (#1 y) then (if (#1 x) > (#1 y) then false else true)
  else if (#2 x) <> (#2 y) then (if (#2 x) > (#2 y) then false else true)
       else if (#3 x) <> (#3 y) then (if (#3 x) > (#3 y) then false else true)
            else true

fun number_in_month(xs : (int*int*int) list, y : int) =  
    if null xs then 0
    else
        if (#2 (hd xs)) = y then 1 + number_in_month(tl xs, y)   
        else number_in_month(tl xs, y)  

fun number_in_months(xs : (int*int*int) list, y : int list) =
    if null y then 0
    else
        number_in_month(xs, hd y)+number_in_months(xs, tl y) 

fun dates_in_month(xs:(int*int*int) list, y:int) =
    if null xs then []
    else
        if (#2 (hd xs)) = y then (hd xs)::dates_in_month(tl xs,y)
        else dates_in_month(tl xs,y)

fun dates_in_months(xs:(int*int*int) list, y:int list) =
    if null y then []
    else
        dates_in_month(xs, hd y)@dates_in_months(xs,tl y)

fun get_nth(xs:string list,y:int) =
    if y=1 then (hd xs)
    else get_nth(tl xs,y-1)

fun date_to_string(x: int*int*int) =
    let val month=["January","February","March","April","May","June","July","August","September","October","November","December"] in get_nth(month,(#2 x))^" "^Int.toString(#3 x)^", "^Int.toString((#1 x)) end

fun number_before_reaching_sum(sum:int,int_list:int list)=
        if sum <= (hd(tl int_list))+(hd int_list) andalso sum > (hd int_list) then 1 else 1+number_before_reaching_sum(sum-(hd int_list),tl int_list)

fun what_month(x:int) = 
    let val month=[0,31,28,31,30,31,30,31,31,30,31,30,31] in number_before_reaching_sum(x,month) end 

fun month_range(x:int,y:int)=
    if x=y then what_month(y)::[] 
    else 
    what_month(x)::month_range(x+1,y)

fun oldest(dates:(int*int*int) list) =
    let
    fun internal(dates:(int*int*int) list) = 
        if is_older(hd dates,hd (tl dates)) = true then if null (tl (tl dates)) then (hd dates) else internal(((hd dates)::(tl(tl dates)))) else if null (tl (tl dates)) then (hd (tl dates)) else internal(tl dates)
    in
    if null dates then NONE else SOME (internal(dates)) 
    end
