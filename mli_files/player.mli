open Card
open Dogma

type stack

type t

let init_stack color

let update_stack_cards stack cards

let init_player id

let map_color_to_int

let compare_player player1 player2

let compare_stack stack1 stack2

(** [get_hand t] is the list of hand cards of [t]. *)
val get_hand : t -> Card.t list

(** [add_hand t c] is the new player with the list of hand cards of [t] 
    after adding [c]. *)
val add_hand : t -> Card.t -> t

(** [remove_hand t i] is the new player with the list of hand cards of [t] 
    after removing the card with index [i]. *)
val remove_hand : t -> int -> t

let remove_ith_card lst i

let get_ith_hand player i

let get_ith_stack player i

let get_color_stack (player: t) (c: Dogma.stack_color) : t 

let update_stack_list s_lst i new_s

let pop_card i lst

let pop_stack i lst

let add_card_to_stack (card: Card.t) (stack: stack) (top: bool): stack



(** [add_stack t i_h] is the new player with the list of cards of [t]'s
    stack with the same color as the card with index of 
    [i_h] from [t]'s hand cards, after adding that card.
    Create a new stack if there is no such stack. ] *)
val add_stack : t -> int -> Card.t -> t

(** [remove_stack c i] is the new player with the list of cards of [t]'s
    stack with color [c] after removing its [n]th 
    card. *)
val remove_stack : t -> Dogma.stack_color -> int -> t

let splay (player: t) (color: Dogma.stack_color) (direction: Dogma.splay_direction) : t

let get_score_cards player

(** [get_score t] is the current score of [t]. *)
val get_score : t -> int

let get_value (player:t) (idx:int) : int

(** [add_score t x] is the new player with the current score of [t] after
    adding [x]. *)
val add_score : t -> int -> t

(** [get_achievement t] is the list of achievements of [t]. *)
val get_achievements : t -> int list

(** [add_achievement t e] is the new player with the list of achievements of [t]
    after adding the achievement represented by (in) [e] *)
val add_achievement : t -> int -> t

let check_achieve player achievement

let transfer_card_to_stack (card_list Card.t list) (stack: stack) (idx: int)


