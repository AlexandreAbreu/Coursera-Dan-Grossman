(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. *)
fun only_capitals(sl: string list) =
    List.filter (fn s => Char.isUpper(String.sub (s, 0))) sl

(* 2. *)
fun longest_string1(sl: string list) =
    List.foldl (fn (s, acc) => if (String.size s > String.size acc) then s else acc) "" sl

(* 3. *)
fun longest_string2(sl: string list) =
    List.foldl (fn (s, acc) => if String.size s >= String.size acc then s else acc) "" sl

(* 4. *)
fun longest_string_helper f sl =
    List.foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) "" sl
    
fun longest_string3(sl: string list) =
    let
        val greater = fn (a, b) => a > b
    in
        longest_string_helper greater sl
    end

fun longest_string4(sl: string list) =
    let
        val greater_or_equal = fn (a, b) => a > b
    in
        longest_string_helper greater_or_equal sl
    end

(* 5. *)
fun longest_capitalized(sl: string list) =
    let
        val lc = (longest_string1 o only_capitals)
    in
        lc(sl)
    end
(*
- longest_capitalized([]);
val it = "" : string
- longest_capitalized(["de", "deddd"]);
val it = "" : string
- longest_capitalized(["de", "Deddd"]);
val it = "Deddd" : string
- longest_capitalized(["de", "Deddd", "Re"]);
val it = "Deddd" : string
*)

(* 6. *)
fun rev_string(s) = 
    (String.implode o List.rev o String.explode) s

(*
- rev_string("DeDeDeDe");
val it = "eDeDeDeD" : string
- rev_string("");
val it = "" : string
- rev_string("dddd");
val it = "dddd" : string
- rev_string("dddd 1");
val it = "1 dddd" : string
- rev_string("123 456");
val it = "654 321" : string
*)

(* 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs => case f(x) of
                     NONE => first_answer f xs
                   | SOME v => v

(*
- first_answer (fn a => NONE) [];
uncaught exception NoAnswer
  raised at: hw3.sml:107.21-107.29
- first_answer (fn a => NONE) [1, 2, 3];
uncaught exception NoAnswer
  raised at: hw3.sml:107.21-107.29
- first_answer (fn a => if a = 3 then SOME a else NONE) [1, 2, 3];
val it = 3 : int
- first_answer (fn a => if a = 3 then SOME a else NONE) [1, 2, 3, 4, 5];
val it = 3 : int
*)


(* 8. *)
fun all_answers f lst =
    let
        fun f_int (f, lst, acc) = case lst of
                                      [] => SOME acc
                                    | x::xs => case f(x) of
                                                   NONE => NONE
                                                 | SOME v => f_int(f, xs, v @ acc)
    in
        f_int(f, lst, [])
    end

(*
- all_answers(fn a => if a = 0 then NONE else SOME [a]) [];
val it = SOME [] : int list option
- all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2];
val it = SOME [1,2] : int list option
- all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 3, 4];
val it = SOME [1,2,3,4] : int list option
- all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 0, 4];
val it = NONE : int list option
- all_answers(fn a => if a = 0 then NONE else SOME [a]) [1, 2, 4];
val it = SOME [1,2,4] : int list option
*)

(* 9. *)
fun count_wildcards(p: pattern) =
    g (fn v => 1) (fn v => 0) p

(*
- count_wildcards(TupleP [ConstP 1]);
val it = 0 : int
- count_wildcards(TupleP [ConstP 1, Wildcard, Wildcard]);
val it = 2 : int
- count_wildcards(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard])]);
val it = 3 : int
*)

fun count_wild_and_variable_lengths(p: pattern) =
    g (fn v => 1) (fn s => String.size s) p
    
(*
- count_wild_and_variable_lengths(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = 5 : int
*)

fun count_some_var(vn: string, p: pattern) =
    g (fn v => 0) (fn s => if s = vn then 1 else 0) p

(*
- count_some_var("DD", TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = 1 : int
- count_some_var("DDd", TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = 0 : int
- count_some_var("DD", TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = 2 : int
*)


(* 10. *)
fun check_pat (p: pattern) =
    let
        fun variables(p: pattern) =
	    case p of
	        Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (v,vs) => vs @ variables(v)) [] ps
	      | ConstructorP(_,p) => variables(p)
	      | _                 => []
        fun repeats(sl: string list) =
            case sl of
                [] => true
              | x::xs => if List.exists (fn a => a = x) xs then false else repeats(xs)
    in
        repeats(variables(p))
    end

(*
- check_pat(TupleP [ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = true : bool
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "DD"])]);
val it = false : bool
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD"])]);
val it = true : bool
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "dDD"]]])]);
stdIn:313.140-313.143 Error: syntax error: deleting  RBRACKET RBRACKET RPAREN
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "dDD"]])]);
val it = false : bool
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "DD"]])]);
val it = false : bool
- check_pat(TupleP [Variable "DD", ConstP 1, Wildcard, ConstructorP ("S", TupleP [Wildcard, Wildcard, Variable "dDD", TupleP [Variable "DD1"]])]);
val it = true : bool
- check_pat(ConstP 1);
val it = true : bool
*)

(* 11. *)
fun match(v: valu, p: pattern) =
    case p of
        Variable x => SOME [(x, v)]
      | UnitP =>
        (case v of
             Unit => SOME []
           | _ => NONE)
      | Wildcard => SOME []
      | ConstP k =>
        (case v of
             Const(v) => if k = v then SOME [] else NONE
           | _ => NONE)
      | TupleP ps =>
        (case v of
             Tuple(vs) => if List.length vs = List.length ps
                          then all_answers match (ListPair.zip(vs, ps))
                          else NONE
           | _ => NONE)
      | ConstructorP(s1,pp) =>
        (case v of
             Constructor(s2,vv) =>
             if s1 = s2 then match(vv,pp) else NONE
           | _ => NONE)
            
(* 12. *)
fun first_match v ps =
    SOME(first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE

(* Challenge *)
(* fun typecheck_patterns *)
