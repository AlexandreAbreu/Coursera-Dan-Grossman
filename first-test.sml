(* is_older *)
val oldest_date = (1, 1, 1);

is_older(oldest_date, (2, 1, 1));
is_older(oldest_date, (1, 2, 1));
is_older(oldest_date, (1, 1, 2));

not(is_older((1, 1, 2), oldest_date));
not(is_older((1, 2, 1), oldest_date));
not(is_older((2, 1, 1), oldest_date));

not(is_older(oldest_date, oldest_date));

(* number_in_month *)
number_in_months([], 2);
number_in_months([(1, 1, 1)], 2);
number_in_months([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1)], 2);

(* number_in_months *)
numbers_in_months([], [2]);
numbers_in_months([(1, 1, 1)], [2]);
numbers_in_months([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1)], [2]);
numbers_in_months([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1), (1, 3, 22)], [2, 1]);

(* dates_in_month *)
dates_in_month([], 2);
dates_in_month([(1, 1, 1)], 2);
dates_in_month([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1)], 2);

(* dates_in_months *)
dates_in_months([], [2]);
dates_in_months([(1, 1, 1)], [2]);
dates_in_months([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1)], [2]);
dates_in_months([(1, 2, 1), (1, 1, 1), (1, 2, 3), (11, 1, 1)], [2, 1]);

(* date_to_string *)
date_to_string(1, 12, 2012);
date_to_string(1, 1, 2012);

(* what_month *)
what_month(1);
what_month(70);

(* month_range *)
month_range(2, 1);
month_range(1, 1);
month_range(27, 33);

(* oldest *)
valOf(oldest([(1, 2, 3), (12, 2, 2), (2, 2, 4), (12, 2, 1)]));
valOf(oldest([(1, 2, 3)]));


