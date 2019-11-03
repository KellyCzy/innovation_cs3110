
type splay_direction = No | Up | Left | Right

type stack_color = Red | Purple | Blue | Green | Yellow

<<<<<<< HEAD
type card_pile = 
    Self_hand of int | Other_hand of int | Self_stack of stack_color | Other_stack of stack_color | Self_score | Other_score 
=======
type card_pile = Self_hand of int | Others_hand of int | Self_stack of stack_color | Others_stack of stack_color
>>>>>>> 044ee0e00e1d8d38a9f44ea64d8a1ee704ecaa14

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
