open Card
open Dogma

type stack

type t

(** [get_hand t] is the list of hand cards of [t]. *)
val get_hand : t -> Card.t list

(** [add_hand t c] is the list of hand cards of [t] 
    after adding [c]. *)
val add_hand : t -> Card.t -> Card.t list

(** [remove_hand t i] is the list of hand cards of [t] 
    after removing the card with index [i]. *)
val remove_hand : t -> int -> Card.t list

(** [get_stack t i] is the list of cards of [t]'s stack 
    with index of [i]. *)
val get_stack : t -> int -> Card.t list

(** [add_stack t i_h] is the list of cards of [t]'s
    stack with the same color as the card with index of 
    [i_h] from [t]'s hand cards, after adding that card.
    Create a new stack if there is no such stack. ] *)
val add_stack : t -> int -> Card.t -> Card.t list

(** [remove_stack c i] is the list of cards of [t]'s
    stack with color [c] after removing its [n]th 
    card. *)
val remove_stack : t -> Dogma.stack_color -> int -> Card.t list

(** [get_score t] is the current score of [t]. *)
val get_score : t -> int

(** [add_score t x] is the current score of [t] after
    adding [x]. *)
val add_score : t -> int -> int

(** [remove_score t x] is the current score of [t] after
    removing [x]. *)
val remove_score : t -> int -> int

(** [get_achievement t] is the list of achievements of [t]. *)
val get_achievements : t -> int list

(** [add_achievement t e] is the list of achievements of [t]
    after adding the achievement represented by (in) [e] *)
val add_achievement : t -> int -> int list

(** [remove_achievement t e] is the list of achievements of [t]
    after removing the achievement represented by (in) [e]*)
val remove_achievement : t -> int -> int list
