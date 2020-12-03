(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)
fun only_capitals l = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) l

fun longest_string1 l = 
    List.foldl (fn (x, y) => (if String.size(x) > String.size(y) then x else y)) "" l

fun longest_string2 l = 
    List.foldl (fn (x, y) => (if String.size(x) >= String.size(y) then x else y)) "" l

fun longest_string_helper f xs = 
	List.foldl (fn (x, y) => (if f(String.size(x), String.size(y)) then x else y)) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string4 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
    let fun helper l = case l of 
                            [] => raise NoAnswer
                            | x::xs => case f(x) of
                                        NONE => helper xs
                                        | SOME v => v
    in
        helper lst
    end

fun all_answers f lst =
    let fun helper (l, acc) = case l of 
                            [] => SOME acc 
                            | x::xs => case f(x) of
                                        NONE => NONE 
                                        | SOME v => helper (xs, acc@v) 
    in
        helper (lst, [])
    end

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


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

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

val count_wildcards = g (fn _=> 1)(fn _=> 0)

val count_wild_and_variable_lengths = g (fn _=> 1) (fn x => String.size x)

fun count_some_var (s,p) = g (fn _=> 0)(fn x => if x=s then 1 else 0) p

fun check_pat p = 
    let
        fun allVariablesList p = 
            case p of
                Variable s => [s]
                | TupleP ps => List.foldl (fn (pt, acc) => (allVariablesList pt)@acc) [] ps
                | ConstructorP (_, pt) => allVariablesList pt
                | _ => []

        fun repeatExists slist = 
            case slist of
                [] => false
                | x::xs => (List.exists (fn s => x=s) xs) orelse repeatExists(xs)
    in
        not(repeatExists(allVariablesList p))
    end

fun match tup = 
    case tup of
        (_, Wildcard) => SOME []
        | (v, Variable s) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const x, ConstP p) => if x=p then SOME [] else NONE
        | (Tuple vs, TupleP ps) => if (List.length vs = List.length ps)
                then all_answers match (ListPair.zip(vs, ps))
                else NONE
        | (Constructor(s2, v), ConstructorP(s1, p)) => if s1=s2
                then match (v, p)
                else NONE
        | (_, _) => NONE

fun first_match valu pattern_list =
    SOME(first_answer (fn pattern => match(valu, pattern)) pattern_list) handle NoAnswer => NONE