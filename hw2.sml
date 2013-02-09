(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a *)
fun all_except_option(s: string, slist: string list) =
    let
        fun all_except_option_internal(s, slist, prev_slist) =
            case slist of
                [] => []
              | x::xs => if same_string(x,s) then prev_slist @ xs else all_except_option_internal(s, xs, prev_slist @ [x])
    in
        case all_except_option_internal(s, slist, []) of
            [] => NONE
          | result => SOME result
    end

(* 1.b *)
fun get_substitutions1(sll: string list list, s: string) =
    case sll of
        [] => []
      | x::xs => case all_except_option(s, x) of
                     NONE => get_substitutions1(xs, s)
                  | SOME l => l @ get_substitutions1(xs, s)

(* 1.c *)
fun get_substitutions2(sll: string list list, s: string) =
    let
        fun get_substitutions2_tr(sll: string list list, s: string, result: string list) =
            case sll of
                [] => result
              | x::xs => case all_except_option(s, x) of
                             NONE => get_substitutions2_tr(xs, s, result)
                           | SOME l => get_substitutions2_tr(xs, s, result @ l)
            
    in
        get_substitutions2_tr(sll, s, [])
    end

(* 1.d *)
fun similar_names(subs, {first= f, middle= m, last= l}) =
    let
        val full_name = {first= f, middle= m, last= l}
        fun generate_substitutions(substitutions: string list) =
            case substitutions of
                [] => []
             | x::xs => {first= x, middle= m, last= l} :: generate_substitutions(xs)
    in
        [full_name] @ generate_substitutions(get_substitutions2(subs, f))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2. a *)
fun card_color(one_card) =
    case one_card of
        (Spades, _) => Black
      | (Clubs, _) => Black
      | (_, _) => Red

(* 2. b *)
fun card_value(one_card) =
    case one_card of
        (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num value) => value

(* 2. c *)
fun remove_card(cs, c, e) =
    let
        fun remove_card_internal(cs, prev_cs) =
            case cs of
                [] => []
              | x::xs => if x = c then prev_cs @ xs else remove_card_internal(xs, prev_cs @ [x])
    in
        case remove_card_internal(cs, []) of
            [] => raise e
          | result => result
    end

(* 2. d *)
fun all_same_color(cs) =
    case cs of
        [] => true
     | x::xp::rest => card_color(x) = card_color(xp) andalso all_same_color(xp::rest)
     | x::[] => true

(* 2. e *)
fun sum_cards(cs) = 
    let
        fun sum_cards_tl (cs, count) =
            case cs of
                [] => count
              | x::xs => sum_cards_tl(xs, count + card_value(x))
    in
        sum_cards_tl(cs, 0)
    end

(* 2. f *)
fun score(cs: card list, goal: int) = 
    let
        val sum = sum_cards(cs)
        val preliminary_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color(cs) then preliminary_score div 2 else preliminary_score
    end

(* 2. g *)
fun officiate(cs: card list, moves: move list, goal: int) =
    let
        fun play_moves(cs: card list, held_cards: card list, moves: move list) =
            case moves of
                [] => held_cards
              | Discard c::ms => play_moves(cs, remove_card(held_cards, c, IllegalMove), ms)
              | Draw::ms =>
                case cs of
                    [] => held_cards
                  | c::rest_of_cards =>
                    if (card_value(c) + sum_cards(held_cards)) > goal
                    then c::held_cards else play_moves(rest_of_cards, c::held_cards, ms)
    in
        score(play_moves(cs, [], moves), goal)
    end

(* 3 a. *)
fun score_challenge(cs: card list, goal: int) = 
    let
        exception CardNotFound
        fun lowest_score(cs: card list, prev_list: card list, lowest: int) =
            case cs of
                [] => lowest
              | (s,Ace)::rest =>
                let
                    val cs_try = prev_list @ [(s, Num 1)] @ rest
                    val current_score = score(cs_try, goal)
                in
                    if current_score < lowest
                    then lowest_score(rest, prev_list @ [(s, Num 1)], current_score)
                    else lowest_score(rest, prev_list @ [(s, Ace)], lowest)
                end
              | c::rest => lowest_score(rest, prev_list @ [c], lowest)

        val base_score = score(cs, goal)
    in
        lowest_score(cs, [], base_score)
    end

fun officiate_challenge(cs: card list, moves: move list, goal: int) =
    let
        fun play_moves(cs: card list, held_cards: card list, moves: move list) =
            case moves of
                [] => held_cards
              | Discard c::ms => play_moves(cs, remove_card(held_cards, c, IllegalMove), ms)
              | Draw::ms =>
                case cs of
                    [] => held_cards
                  | c::rest_of_cards =>
                    if (card_value(c) + sum_cards(held_cards)) > goal
                    then c::held_cards else play_moves(rest_of_cards, c::held_cards, ms)
    in
        score_challenge(play_moves(cs, [], moves), goal)
    end

