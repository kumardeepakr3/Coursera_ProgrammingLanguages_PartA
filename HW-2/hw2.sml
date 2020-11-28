(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s1: string, s2: string list) = 
   case s2 of
      [] => NONE
      | x::xs =>
         case (same_string(s1, x), all_except_option(s1, xs)) of
            (true, _) => SOME(xs)
            | (false, NONE) => NONE
            | (false, SOME(p)) => SOME(x::p)

fun get_substitutions1(sl: string list list, s: string) = 
   case sl of
      [] => []
      | x::xs => (
         case all_except_option(s, x) of
               NONE => get_substitutions1(xs, s)
               | SOME(x) => x@(get_substitutions1(xs, s))
      )

fun get_substitutions2(slist: string list list, s: string) = 
   let
      fun helperAccumulator(slist: string list list, s: string, acc: string list) = 
         case slist of
            [] => acc 
            | x::xs => (
               case all_except_option(s, x) of
                     NONE => helperAccumulator(xs, s, acc)
                     | SOME(x) => helperAccumulator(xs, s, acc@x) 
            )
   in
      helperAccumulator(slist, s, [])
   end

fun similar_names(slist: string list list, {first: string, middle: string, last: string}) = 
   let
      fun generatePermutations(firstList: string list) =
         case firstList of
            [] => [] 
            | x::xs => {first=x, middle=middle, last=last}::generatePermutations(xs)
   in
      generatePermutations(first::get_substitutions2(slist, first))
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
fun card_color(c: card) = 
   case c of
      (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

fun card_value(c: card) =
   case c of
      (_, Ace) => 11
      | (_, Num(x)) => x
      | (_, _) => 10

fun remove_card(cs: card list, c: card, e) = 
    case cs of
        [] => raise e
        | x::xs => if x=c then xs else x::remove_card(xs, c, e)

fun all_same_color(cs: card list) =
   case cs of
      [] => true
      | x::[] => true
      | x::y::xs => card_color(x)=card_color(y) andalso all_same_color(y::xs)

fun sum_cards(cs: card list) = 
   let
      fun helperAccumulator(cs: card list, acc: int) =
         case cs of
            [] => acc
            | x::xs => helperAccumulator(xs, acc+card_value(x))
   in
      helperAccumulator(cs, 0)
   end

fun score(cs: card list, goal: int) =
   let
      fun preliminaryScore(sum: int) =
         if sum > goal then (3*(sum - goal)) else (goal-sum)

      fun finalScore(prelimScore: int, areCardsSameColored: bool) =
         if areCardsSameColored then (prelimScore div 2) else prelimScore
   in
      finalScore(preliminaryScore(sum_cards(cs)), all_same_color(cs))
   end

fun officiate(cs: card list, ml: move list, goal: int) =
   let
      fun runGame(handHeldCards: card list, deckCards: card list, moves: move list) =
         case moves of
            [] => score(handHeldCards, goal)
            | move::remainingMoves => (
                  case (move, deckCards) of
                     (Discard(c), _) => runGame(remove_card(handHeldCards, c, IllegalMove), deckCards, remainingMoves)
                     | (Draw, []) => score(handHeldCards, goal)
                     | (Draw, drawnCard::remainingDeckCards) =>(
                              let
                                 val updatedHandheldCards = drawnCard::handHeldCards
                              in
                                 if sum_cards(updatedHandheldCards) > goal
                                   then score(updatedHandheldCards, goal)
                                   else runGame(updatedHandheldCards, remainingDeckCards, remainingMoves)
                              end
                           )
            )
   in
      runGame([], cs, ml)
   end
