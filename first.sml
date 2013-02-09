(* 1. *)
fun is_older (d1: int*int*int, d2: int*int*int) =
    let
        val dy = #1 d1 - #1 d2;
        val dm = #2 d1 - #2 d2;
        val dd = #3 d1 - #3 d2;
    in
        if dy <> 0
        then dy < 0
        else
            if dm <> 0
            then dm < 0
            else
                if dd <> 0
                then dd < 0
                else false
    end


(* 2. *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        if #2 (hd dates) = month
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)

(* 3. *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
        if #2 (hd dates) = month
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* 5. *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))

(* 6. *)
fun get_nth(ss: string list, n: int) =
    if n = 1
    then (hd ss)
    else get_nth(tl ss, n - 1)

(* 7. *)
fun date_to_string(date: int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* 8. *)
fun number_before_reaching_sum(sum: int, ns: int list) =
    let
        fun sum_with_acc(acc: int, n: int, ns: int list) =
            if null ns
            then n
            else
                if acc + (hd ns) >= sum
                then n
                else sum_with_acc(acc + (hd ns), n + 1, (tl ns))
    in
        sum_with_acc(0, 0, ns)
    end


(* 9. *)
fun what_month(day: int) =
    let
        val acc_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, acc_months) + 1
    end


(* 10. *)
fun month_range(d1: int, d2: int) =
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1 + 1, d2)

(* 11. *)
fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else
        let
            fun find_older_than(dates: (int*int*int) list, oldest_date: int*int*int) =
                if null dates
                then oldest_date
                else
                    let
                        val current_date = hd dates
                    in
                        if is_older(current_date, oldest_date)
                        then find_older_than(tl dates, current_date)
                        else find_older_than(tl dates, oldest_date)
                    end
        in
            SOME (find_older_than(tl dates, hd dates))
        end


(* 12. *)

fun remove_duplicates(ins: int list) =
    let
        fun member_of(n: int, ns: int list) =
            if null ns
            then false
            else
                if n = hd ns
                then true
                else member_of(n, tl ns)
        fun remove_all_occurences(n: int, ns: int list) =
            if null ns
            then []
            else
                if hd ns = n
                then remove_all_occurences(n, tl ns)
                else hd ns :: remove_all_occurences(n, tl ns)
    in
        if null ins
        then []
        else hd ins :: remove_duplicates(remove_all_occurences(hd ins, tl ins))
    end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
    dates_in_months(dates, remove_duplicates(months))


(* 13. *)

fun reasonable_date(date: int*int*int) =
    let
        fun is_leap_year(year: int) =
            (year mod 100) <> 0 andalso (((year mod 4) = 0) orelse ((year mod 400) = 0));
        fun days_per_month_for_year(year: int) =
            if is_leap_year(year)
            then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        (* some shadowing for an int version *)
        fun get_nth(ss: int list, n: int) =
            if n = 1
            then (hd ss)
            else get_nth(tl ss, n - 1)
        val day = #3 date;
        val month = #2 date;
        val year = #1 date;
        val valid_year = year > 0;
        val valid_month = month >= 1 andalso month <= 12
        val valid_day = day >= 1 andalso day <= 31
    in
        valid_year andalso valid_month andalso valid_day andalso day <= get_nth(days_per_month_for_year(year), month)
    end

