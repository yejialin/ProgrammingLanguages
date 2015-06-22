fun is_older(date1 : int * int * int , date2 : int * int * int) =
  (#1 date1) * 356 +(#2 date1) * 30 + (#3 date1) < (#1 date2) * 356 + (#2 date2) * 30 + (#3 date2)

fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else let
           fun in_month(xs : int * int * int) =
	       if (#2 xs) = month
	       then 1
	       else 0
     in
	 in_month(hd dates) + number_in_month((tl dates), month)
     end

fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if (#2 (hd dates)) = month
       then (hd dates) :: dates_in_month((tl dates), month)
       else dates_in_month((tl dates), month) 

fun dates_in_months(dates : (int * int * int) list, months : int list)=
  if null months
  then []
  else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

fun get_nth(value : string list, n : int) =
  if n = 1
  then (hd value)
  else get_nth((tl value), n-1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date : int * int * int)=
  get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum : int, numbers : int list) =
  let
      fun find_index(idx : int, current_sum : int, numbers : int list) =
	if current_sum + (hd numbers) >= sum 
	then idx
	else find_index(idx+1, current_sum + (hd numbers), (tl numbers))
  in find_index(0, 0, numbers)
  end

val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month(day : int) =
  number_before_reaching_sum(day, days) + 1

fun month_range(day1 : int, day2 : int) =
  if day2 - day1 < 0
  then []			       
  else what_month(day1) :: month_range(day1+1, day2)
	    
fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else let
      fun oldest_nonempty(dates : (int * int * int) list) =
	if null (tl dates)
	then (hd dates)
	else let val old_ans = oldest_nonempty(tl dates)
	     in 
		 if is_older((hd dates), old_ans)
		 then (hd dates)
		 else old_ans
	     end  
  in
      SOME (oldest_nonempty(dates))
  end

fun in_list(value : int, values : int list) = 
  if null values
  then false
  else if value = (hd values)
  then  true
  else in_list(value, (tl values))

fun remove_duplicates(values : int list) =
  if null values
  then []
  else if in_list((hd values), (tl values))
  then remove_duplicates(tl values)
  else (hd values) :: remove_duplicates(tl values)

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months: int list) =
  dates_in_months(dates, remove_duplicates(months))

val leap_days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun leap_year(year : int) = 
  if (year mod 400) = 0 orelse ((year mod 4) = 0 andalso (year mod 100) <> 0) 
  then true
  else false

fun list_idx(thelist : int list, idx : int) =
  if idx = 0
  then hd thelist
  else list_idx(tl thelist, idx - 1)

fun reasonable_date(date : int * int * int) = 
  if (#1 date) < 1
  then false
  else if (#2 date) > 12 orelse (#2 date) < 1
  then false
  else
      let 
	  val month_days = if leap_year(#1 date) then leap_days else days
      in
	  if #3 date < 1 orelse #3 date > list_idx(month_days, (#2 date) -1)
	  then false
	  else true
      end
