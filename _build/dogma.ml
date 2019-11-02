
type splay_direction = No | Up | Left | Right

type stack_color = Red | Purple | Blue | Green | Yellow

type card_pile = SelfHand of int | OthersHand of int | SelfStack of stack_color | OthersStack of stack_color

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
