
type splay_direction = No | Up | Left | Right

type stack_color = Red | Purple | Blue | Green | Yellow

type card_pile = 
  | Self_hand of int 
  | Other_hand of int 
  | Self_stack of stack_color 
  | Other_stack of stack_color
  | Self_score of int 
  | Other_score of int (*dont use this int*)

type effect = 
  | Draw of int
  | Meld of int 
  | Tuck of int
  | Splay of splay_direction
  | Return of int
  | Score of int
  | Transfer of card_pile * card_pile * int
  | Demand of effect list

type t = effect list

let map_effect_string = function
  | Draw _ -> "Draw"
  | Meld _ -> " Meld"
  | Tuck _ -> "Tuck"
  | Splay _ -> "Splay"
  | Return _ -> "Return"
  | Score _ -> "Score"
  | Transfer _ -> "Transfer"
  | Demand _ -> "Demand"

let rec print_effects = function
  | x::xs -> Printf.printf "%s" (map_effect_string x); print_effects xs
  | [] -> ()