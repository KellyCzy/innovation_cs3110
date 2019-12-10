open Dogma
open Card 

type stack

val get_dir : stack -> Dogma.splay_direction

val get_top_card : stack -> Card.t

type t

val init_stack : Dogma.stack_color -> stack

val init_player : int -> t

val map_color_to_int : Dogma.stack_color -> int

val map_color_to_string : Dogma.stack_color -> string

val get_stack_color : stack -> Dogma.stack_color

val get_stack_cards : stack -> Card.t list

val get_stack_length : stack -> int

val compare_player : t -> t -> int

val compare_stack : stack -> stack -> int

val update_hand : Card.t list -> t -> t

val update_board : stack list -> t -> t

val update_splay_direction : stack -> Dogma.splay_direction -> stack

val update_stack_cards : stack -> Card.t list -> stack

val get_id : t -> int

val get_hand : t -> Card.t list

val get_hand_length : t -> int

val print_hand : t -> string

val get_ith_stack : t -> int -> stack

val get_stack_top : t -> int -> string

val get_top_card_name : t -> int -> string

val get_board_total_length : t -> int

val print_board : t -> int

val get_board : t -> stack list

val add_hand : t -> Card.t -> t

val get_ith_hand : t -> int -> Card.t

val get_color_stack : t -> Dogma.stack_color -> stack

val count_left : Card.t list -> Card.icon -> int -> int

val count_right : Card.t list -> Card.icon -> int -> int

val count_up : Card.t list -> Card.icon -> int -> int

val count_no : Card.t list -> Card.icon -> int -> int

val count_icon : stack list -> Card.icon -> int -> int

val get_icon : t -> Card.icon -> int

val update_stack_list : stack list -> int -> stack -> stack list

val pop_card : int -> Card.t list -> Card.t list * Card.t

val pop_stack : int -> stack -> stack * Card.t

val remove_hand : t -> int -> t

val push_stack : Card.t -> stack -> bool -> stack

val push_card : Card.t -> Card.t list -> Card.t list

val add_stack : t -> int -> bool -> t

val remove_stack : t -> Dogma.stack_color -> t * Card.t

val transfer_card_to_stack : Card.t list -> stack -> int -> bool -> Card.t list * stack

val transfer_stack_to_card : stack -> Card.t list -> stack * Card.t list

val transfer_card_to_card : Card.t list -> Card.t list -> int -> Card.t list * Card.t list

val transfer_stack_to_stack : stack -> stack -> bool -> stack * stack

val splay : t -> Dogma.stack_color -> Dogma.splay_direction -> t

val get_score_cards : t -> Card.t list

val get_score : t -> int

val get_value : t -> int -> int

val update_score : t -> Card.t list -> t

val update_achievements : t -> int list -> t

val add_score : t -> Card.t -> t

val get_achievements : t -> int list

val add_achievement : t -> int -> t

val print_player : t -> unit




