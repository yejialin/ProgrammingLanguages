(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str : string, str_list : string list) =
  let fun remove(str_to_remove, remaining_list) =
	case remaining_list of
	    [] => []
	   | x :: xs  => if same_string(str_to_remove, x) then xs
			else x :: remove(str_to_remove, xs)
      val result = remove(str, str_list)
  in if result = str_list then NONE else SOME(result)
  end

fun get_substitutions1(substitutions : string list list, s : string) = 
  case substitutions of
      [] => []
     | x :: xs  => let val result = all_except_option(s, x) in
			  case result of
			      NONE => get_substitutions1(xs, s)
		            | SOME str_list  => str_list @ get_substitutions1(xs, s) 
		  end

fun get_substitutions2(substitutions : string list list, s : string) = 
  let fun aux(sub : string list list, acc : string list) = 
	case sub of
	    [] => acc
	   | x :: xs => let val result = all_except_option(s, x) in
			   case result of
			       NONE => aux(xs, acc)
			     | SOME str_list => aux(xs, str_list @ acc)
		       end
  in aux(substitutions, [])
  end   

fun similar_names(substitutions : string list list, {first=x, middle=y, last=z}) = 
  let val first_name = x :: get_substitutions2(substitutions, x)
      fun name(name_list) =
	case name_list of
	    [] => [] 
	  | x1 :: xs => {first=x1, middle=y, last=z} :: name(xs) 
  in
      name(first_name)
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

fun card_color(card_suit, _) = 
  case card_suit of
      Spades => Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red

fun card_value(_, card_rank) = 
  case card_rank of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num i => i 

fun remove_card(cs : card list, c : card, e) = 
  let fun filter_card(cl : card list) = 
	case cl of
	    [] => []
	 | x :: xs => if x = c then xs else x :: filter_card(xs)
      val result = filter_card(cs)
  in
      if result = cs
      then raise e
      else result
  end

fun all_same_color(cs : card list) =
  case cs of
      [] => true
    | _ :: [] => true
    | head :: (neck :: rest) => card_color(head) = card_color(neck)
				andalso all_same_color(neck :: rest)

fun sum_cards(cs : card list) =
  let fun aux(cs, acc) =
	case cs of
	    [] => acc
	  | x :: xs => aux(xs, acc + card_value(x)) 
  in
      aux(cs, 0)
  end

fun score(held_cards : card list, goal : int) = 
  let val sum = sum_cards(held_cards)
      val pre_scr = if sum < goal then goal - sum else 3 * (sum - goal)
      val same_color = all_same_color(held_cards)  in
      if same_color then pre_scr div 2 else pre_scr
  end

fun officiate(card_list : card list, move_list : move list, goal : int) =
  let fun aux(held_cards : card list, moves : move list, remain_cards : card list) =
	case moves of
	    [] => score(held_cards, goal)
	   | x :: xs => case x of
			    Discard card => aux(remove_card(held_cards, card, IllegalMove), xs, remain_cards)
			   | Draw => case remain_cards of
					 [] => score(held_cards, goal)
					 | y :: ys => let val new_held_cards = y :: held_cards in
							  if sum_cards(new_held_cards) > goal then score(new_held_cards, goal)
							  else aux(new_held_cards, xs, ys)
						      end
  in
      aux([], move_list, card_list)          
  end

