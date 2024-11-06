type suit = Clubs | Diamonds | Hearts | Spades;;
type rank = Jack | Queen | King | Ace | Num of int;;
type card = suit * rank;;
type colour = Red | Black;;
type move = Discard of card | Draw;;
exception IllegalMove;;

let card_colour (c)= match c with
  | (Spades,_) | (Clubs,_) -> Black
  | (Diamonds,_) | (Hearts,_) -> Red;;

let card_value (c)= match c with
  | (_,Ace) -> 11
  | (_, Num n) -> n
  | _ -> 10;;

let rec remove_card cs c e : card list= match cs with
  | [] -> raise e
  | hd :: tl -> if hd = c then tl else hd::(remove_card tl c e);;

let rec all_same_colour cards= match cards with
  | [] -> false
  | [cards] -> true
  | card1 :: card2 :: tl -> if card_colour card1 = card_colour card2 then all_same_colour (card2 :: tl) else false;; 

let sum_cards cards = let rec helper acc remaining = match remaining with
    | [] -> acc
    | hd :: tl -> helper (acc + card_value hd) tl 
  in 
  helper 0 cards;;

let score held_cards goal = let sum = sum_cards held_cards 
  in
  if all_same_colour held_cards then (if sum > goal then 3* (sum - goal) else (goal-sum)/2 ) else if sum > goal 
  then 3*(sum-goal) else (goal-sum);;

let officiate card_list move_list goal =
  let rec game_helper held_cards remaining_moves remaining_cards =
    match remaining_moves with
    | [] -> score held_cards goal
    | move :: rest_moves ->
      (match move with
       | Discard c ->
         if List.mem c held_cards then
           let updated_held_cards = remove_card held_cards c IllegalMove in
           game_helper updated_held_cards rest_moves remaining_cards
         else
           raise IllegalMove
       | Draw -> ( match remaining_cards with
           | [] -> score held_cards goal
           | drawn_card :: remaining ->
             let updated_held_cards = drawn_card :: held_cards in
             let sum_held_cards = sum_cards updated_held_cards in
             if sum_held_cards > goal then
               score updated_held_cards goal
             else
               game_helper updated_held_cards rest_moves remaining ))
  in
  game_helper [] move_list card_list