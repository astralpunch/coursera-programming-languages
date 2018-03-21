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

(**** you can put all your code here ****)
			       
fun only_capitals list = List.filter (fn x => Char.isUpper(String.sub(x, 0))) list

fun longest_string1 list =
    case list of
	[] => ""
      | xs => List.foldl (fn (str, acc) => if String.size str > String.size acc 
					   then str
					   else acc)
			 "" list

fun longest_string2 list =
    case list of
	[] => ""
      | xs => List.foldl (fn (str, acc) => if String.size str > String.size acc orelse String.size str = String.size acc
					   then str
					   else acc)
			 "" list

fun longest_string_helper f = fn list =>
				 case list of
				     [] => ""
				   | xs => List.foldl (fn (str, acc) => if f (String.size str, String.size acc)
									then str
									else acc)
							  "" list

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x > y orelse  x = y)

fun longest_capitalized list =
    case only_capitals list of
	[] => ""
      | xs => longest_string1 xs 
											    
val rev_string = String.implode o List.rev o String.explode

fun first_answer f l =
    case l of
	[] => raise NoAnswer
      | x::xs => case f x of
		     NONE => first_answer f xs
		   | SOME x => x

fun all_answers f l =
    let
	fun iter (acc, list) =
	    case list of
		[] => SOME acc
	      | x::xs => case f x of
			     NONE => NONE
			   | SOME x => iter (acc @ x, xs)
    in
	iter ([], l)
    end

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

val count_wildcards = g (fn () => 1) (fn s => 0) 

val count_wild_and_variable_lengths = g (fn () => 1) String.size
					
fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat (pattern) =
    let
	fun get_var_names p =
	    case p of
		Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p, acc) => (get_var_names p) @ acc) [] ps
	      | ConstructorP(_,p) => get_var_names p
	      | _ => [] 
	val var_names = get_var_names pattern
	fun is_uniq vars =
	    case vars of
		[] => true
	      | hd::xs => case List.exists (fn x => hd = x) xs of
			      true => false
			    | false => is_uniq xs 
    in
	is_uniq var_names
    end



fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers (fn (vs', ps') => match (vs', ps')) (ListPair.zip (vs, ps))
				 else NONE
      | (Constructor(s2,v'), ConstructorP(s1,p')) => if s1 = s2
						     then match (v', p')
						     else NONE
      | _ => NONE 

		 
fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
						

   
