

fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end 


fun number_in_month (dates : (int * int * int) list, month : int) =
    let
	fun check(date : (int * int * int)) =
	    if #2 date = month
	    then 1
	    else 0
    in
	if null dates
	then 0
	else check(hd dates) + number_in_month(tl dates, month)	
    end
		

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
    

fun dates_in_month (dates : (int * int * int) list, month : int) =
    let
	fun check(date : (int * int * int)) =
	    if #2 date = month
	    then date :: dates_in_month(tl dates, month)
	    else dates_in_month(tl dates, month)
    in	
	if null dates
	then []
	else check(hd dates) 					   
    end


fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

 

fun get_nth (xs : string list, index : int) =
    if index = 1
    then hd xs
    else get_nth(tl xs, index - 1)


fun date_to_string (date : (int * int * int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)	
    end


fun number_before_reaching_sum (sum : int, xs : int list) =
    let
	fun helper(i : int, s : int, list : int list) =
	    if s <= 0
	    then i
	    else helper(i + 1, s - hd list, tl list)
    in
	helper(~1, sum, xs)
    end

	
    
fun what_month (day : int) =
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, months) + 1
    end

fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else what_month d1 :: month_range(d1 + 1, d2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest(tl dates)
	in
	    if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
	    then tl_ans
	    else SOME (hd dates)
	end
		
	
