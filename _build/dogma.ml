
type splay_direction = No | Up | Left | Right

type stack_color = Red | Purple | Blue | Green | Yellow

type card_pile = Hand | Stack of stack_color

type effect = 
  | Draw 
  | Meld 
  | Tuck of stack_color * int
  | Splay of splay_direction
  | Return of int
  | Score 
  | Transfer of card_pile * card_pile
  | Demand of int * effect list

type t = effect list
