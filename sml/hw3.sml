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


fun only_capitals(string_list : string list) =
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) string_list

fun longest_string1(string_list : string list) =
  List.foldl(fn (x,y) => if String.size(x) > String.size(y) then x else y) "" string_list

fun longest_string2(string_list : string list) =
  List.foldl(fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" string_list

fun longest_string_helper f string_list =
  List.foldl(fn (x,y) => if f(String.size(x), String.size(y)) then x else y) "" string_list

fun longest_string3(string_list : string list) =
  longest_string_helper(fn (x, y) => x > y) string_list

fun longest_string4(string_list : string list) =
  longest_string_helper(fn (x, y) => x >= y) string_list

fun longest_capitalized(string_list : string list) =
  (longest_string1 o only_capitals) string_list

fun rev_string(str : string) = 
  (String.implode o rev o String.explode) str

fun first_answer f list = 
  case list of
      [] => raise NoAnswer
    | x :: xs => let val result = f x
		 in 
		     case result of
			 SOME x => x
		        | _ => first_answer f xs
		 end

fun all_answers f list =
  let fun helper f lst lst_acc =
	case lst of
	    [] => lst_acc
	  | x :: xs => case f x of
			   NONE => raise NoAnswer
		          | SOME y => (helper f xs (y @ lst_acc))
  in
     SOME (helper f list []) handle NoAnswer => NONE
  end

fun count_wildcards p =
  g (fn x => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
  g (fn x => 1) (fn x => String.size x) p

fun count_some_var(str : string, p : pattern) =
 g (fn x => 0) (fn x => if str = x then 1 else 0) p

fun check_pat p =
  let fun get_string p =
	case p of
	    Variable v => [v]
	   | TupleP ps => List.foldl(fn (x, y) => get_string(x) @ y) [] ps
	   | _ => []
      fun different(str_list : string list) =
	case str_list of
	    [] => true
	   | x :: xs => if List.exists(fn y => y = x) xs then false else true
  in
      different(get_string p)      
  end

fun match (v : valu, p : pattern) =
    case p of
        Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of 
                     Unit => SOME [] 
                   | _ => NONE)
      | ConstP i => (case v of 
                        Const j => if i = j then SOME [] else NONE
                      | _ => NONE)
      | TupleP ps => (case v of
                      Tuple vs => (all_answers (fn (x,y) => match(x,y)) (ListPair.zipEq(vs, ps)) handle UnequalLengths => NONE)
                                  | _ => NONE)
      | ConstructorP (s, pat) => (case v of
                                 Constructor (a, b) => if s = a then match(b, pat) else NONE
                                   | _ => NONE)

fun first_match v ps =
  SOME(first_answer(fn x => match(v, x)) ps) handle NoAnswer => NONE
