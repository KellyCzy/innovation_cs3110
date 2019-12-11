(** [splay_direction] is the direction of splaying of a stack on board. *)
type splay_direction 

(** [stack_color] is the color of cards and stacks in the game. *)
type stack_color 

(** [card_pile] is type of cards pile in the game. *)
type card_pile  

(** [effect] is the dogma effects in the game. *)
type effect 

(** [t] is the dogma representation. *)
type t

(** [map_effect_string] is the string representing the effects. *)
val map_effect_string: effect -> string

(** [print_effects] prints out the effect list. *)
val print_effects : effect list -> unit
