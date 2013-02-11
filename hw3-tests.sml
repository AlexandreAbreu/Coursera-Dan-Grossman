longest_capitalized([]);
longest_capitalized(["de", "deddd"]);
longest_capitalized(["de", "Deddd"]);
longest_capitalized(["de", "Deddd", "Re"]);

rev_string("DeDeDeDe");
rev_string("");
rev_string("dddd");
rev_string("dddd 1");
rev_string("123 456");

first_answer (fn a => NONE) [];
first_answer (fn a => NONE) [1, 2, 3];
first_answer (fn a => if a = 3 then SOME a else NONE) [1, 2, 3];
first_answer (fn a => if a = 3 then SOME a else NONE) [1, 2, 3, 4, 5];

all_answers(fn a => if a = 0 then NONE else SOME [a]) [];
all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2];
all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 3, 4];
all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 0, 4];
all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 4];

count_wildcards(TupleP [ConstP 1]);
count_wildcards(TupleP [ConstP 1, Wildcard, Wildcard]);
count_wildcards(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard])]);

count_wild_and_variable_lengths(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);

count_some_var("DD", TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
count_some_var("DDd", TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
count_some_var("DD", TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);

check_pat(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD"])]);
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "dDD"]]])]);
stdIn:313.140-313.143 Error: syntax error: deleting  RBRACKET RBRACKET RPAREN
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "dDD"]])]);
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "DD"]])]);
check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "DD1"]])]);
check_pat(ConstP 1);
