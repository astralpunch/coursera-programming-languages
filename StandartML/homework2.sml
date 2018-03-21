(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, lst) =
    let
	fun filter (list) =
	    case list of
		[] => []
	      | x::xs => if same_string (x, str)
			 then filter (xs)
			 else x :: filter(xs)
	val res = filter(lst)
    in
	if res = lst
	then NONE
	else SOME res
    end


fun get_substitutions1 (lists, str) =
    case lists of
	[] => []
      | x::xs => case all_except_option (str, x) of
		     SOME list => list @ get_substitutions1 (xs, str)
		   | NONE => get_substitutions1 (xs, str)

fun get_substitutions2 (lists, str) =
    let
	fun iter (elems, acc) =
	    case elems of
		[] => acc
	      | x::xs => case all_except_option (str, x) of
			     SOME list => iter (xs, acc @ list)
			   | NONE => iter (xs, acc)
    in
	iter (lists, [])
    end
	
     
 fun similar_names (lists, name) =
     let
	 val {first=x,middle=y,last=z} = name
	 val subs = get_substitutions2 (lists, x)
	 fun iter (acc, elems) =
	     case elems of
		 [] => acc
	       | hd::xs => iter (acc @ [{first=hd, last=z, middle=y}], xs)
     in
	 iter ([name], subs)
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

fun card_color (suit, rank) =
    case suit of
	Spades => Black
      | Clubs  => Black
      | Diamonds => Red
      | Hearts => Red
	
fun card_value (suit, rank) =
    case rank of
	Num value => value
      | Ace => 11
      | _ => 10

fun remove_card (cs,c,e) =
    case cs of
	[] => raise e
      | x::cs' => if x = c
		  then cs'
		  else x :: remove_card(cs',c,e)
   
	
fun all_same_color (cards) =
    case cards of
	[] => true
      | _::[] => true
      | hd::(neck::tl) => card_color (hd) = card_color (neck)
			  andalso all_same_color (neck::tl)

fun sum_cards (cards) =
    let
	fun iter (acc, list) =
	    case list of
		[] => acc
	      | hd::xs => iter (acc + card_value (hd), xs)
    in
	iter (0, cards)
    end

fun score (heldCards, goal) =
    let
	val heldSum = sum_cards (heldCards)
	val preliminary = if heldSum > goal
			  then 3 * (heldSum - goal)
			  else goal - heldSum
    in
	  case all_same_color (heldCards) of
	    true => preliminary div 2
	  | false => preliminary
    end

fun officiate (cards, moves, goal) =
    let    
	fun iter (currScore, held, currCards, currMoves) =
	    case currMoves of
		[] => score (held, goal)
	      | move::xs => case move of
				Discard card =>
				iter (
				    score(remove_card(held, card, IllegalMove), goal),
				    remove_card(held, card, IllegalMove),
				    currCards,
				    xs
				)
			      | Draw => case currCards of
					    [] => currScore
					  | hd::cs => if sum_cards (hd::held) > goal
						      then score(hd::held, goal)
						      else iter (score(hd::held, goal), hd::held, cs, xs)	
    in
	iter(0, [], cards, moves)
    end				
    
