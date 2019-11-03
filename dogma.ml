
type splay_direction = No | Up | Left | Right

type stack_color = Red | Purple | Blue | Green | Yellow

type card_pile = 
  | Self_hand of int 
  | Others_hand of int 
  | Self_stack of stack_color 
  | Others_stack of stack_color
  | Self_score of int 
  | Other_score of int (*dont use this int*)

type effect = 
  | Draw of int
  | Meld of int 
  | Tuck of int
  | Splay of splay_direction
  | Return of int
  | Score of int
  | Transfer of card_pile * card_pile
  | Demand of int * effect list

type t = effect list
