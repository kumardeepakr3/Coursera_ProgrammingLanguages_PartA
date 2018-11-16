fun is_older (date1: int*int*int, date2: int*int*int) = 
    (* If the years are unequal *)
    if #1 date1 <> #1 date2 
    then #1 date1 < #1 date2 
    else
        (
            (*If the months are unequal*)
            if #2 date1 <> #2 date2 
            then #2 date1 < #2 date2 
            else
                (
                    (*If the days are unequal*)
                    if #3 date1 <> #3 date2 
                    then #3 date1 < #3 date2 
                    else false
                )
        )

fun number_in_month (listOfDates: (int*int*int) list, month: int) =
    let
        fun isDateInMonth(date: (int*int*int)) =
            #2 date = month

        fun includeDate (date: (int*int*int)) = 
            if isDateInMonth(date) 
            then 1
            else 0

        fun countMonths(listOfDates: (int*int*int) list) = 
            if null listOfDates
            then 0
            else includeDate(hd listOfDates) + countMonths(tl listOfDates)
    in
        countMonths(listOfDates)
    end

fun number_in_months (listOfDates: (int*int*int) list, listOfMonths: int list) =
    if null listOfMonths
    then 0
    else number_in_month(listOfDates, hd listOfMonths) + number_in_months(listOfDates, tl listOfMonths)

fun dates_in_month (listOfDates: (int*int*int) list, month: int) =
    if null listOfDates
    then []
    else 
    (
        if #2 (hd listOfDates) = month
        then (hd listOfDates)::dates_in_month(tl listOfDates, month)
        else dates_in_month(tl listOfDates, month)
    )

fun dates_in_months (listOfDates: (int*int*int) list, listOfMonths: int list) =
    let 
        fun isDateInMonthList (date: (int*int*int), months: int list) = 
            if null months 
            then false
            else (#2 date = (hd months)) orelse isDateInMonthList(date, tl months)
    in
        if null listOfDates 
        then []
        else 
        (
            if isDateInMonthList(hd listOfDates, listOfMonths)
            then (hd listOfDates)::(dates_in_months(tl listOfDates, listOfMonths))
            else dates_in_months(tl listOfDates, listOfMonths)
        )
    end

fun get_nth (listOfStrings: string list, index: int) = 
    if index = 1
    then hd listOfStrings
    else get_nth(tl listOfStrings, index-1)

fun date_to_string(date: (int*int*int)) = 
    let 
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
        val monthString = get_nth(months, #2 date);
        val yearString = Int.toString(#1 date);
        val dayString = Int.toString(#3 date);
        val spaceString = " ";
        val commaString = ",";
    in
        monthString^spaceString^dayString^commaString^spaceString^yearString
    end

fun number_before_reaching_sum(sum: int, numList: int list) =
    if sum <= (hd numList) 
    then 0 
    else 1+number_before_reaching_sum(sum+(~(hd numList)), tl numList)


fun what_month(day: int) =
    let
        val daytillMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1+number_before_reaching_sum(day, daytillMonth)
    end

fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

fun oldest (listOfDates: (int*int*int) list) =
    if null listOfDates
    then NONE 
    else 
        let
            val old = oldest(tl listOfDates)
        in
            if isSome old andalso is_older(valOf old, (hd listOfDates))
            then old
            else SOME (hd listOfDates)
        end