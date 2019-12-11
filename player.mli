open Dogma
open Card 


(** [stack] is the type representing the stack on the board. *)
type stack

(** [t] is the representation fo a player. *)
type t

(** [get_dir stack] is the splay direction of [stack]. *)
val get_dir : stack -> Dogma.splay_direction

(** [get_top_card stack] is the top card on [stack]. *)
val get_top_card : stack -> Card.t

(** [init_stack color] is the initialized stack of [color]. *)
val init_stack : Dogma.stack_color -> stack

(** [init_player id] is the initialized player of [id]. *)
val init_player : int -> t

(** [map_color_to_int] is corresponding index of a color. *)
val map_color_to_int : Dogma.stack_color -> int

(** [map_color_to_string] is the string representing the the color. *)
val map_color_to_string : Dogma.stack_color -> string

(** [get_stack_color stack] is color of [stack]. *)
val get_stack_color : stack -> Dogma.stack_color

(** [get_stack_cards stack] is the cards list of [stack]. *)
val get_stack_cards : stack -> Card.t list

(** [get_stack_length stack] is the length of the card list in [stack]. *)
val get_stack_length : stack -> int

(** [compare_player p1 p2] is 0 when the id of [p1] and [p2] is equal, 
    negative if [p1] is less than [p2] and positive otherwise *)
val compare_player : t -> t -> int

(** [compare_stack s1 s2] is 0 when the color of [s1] and [s2] is equal,
    negative if [s1] is less than [s2] and positive otherwise. *)
val compare_stack : stack -> stack -> int

(** [update_hand hand player] is a player 
    derived by updated [player] with a new [hand].*)
val update_hand : Card.t list -> t -> t

(** [update_board board player] is a player 
    derived by updated [player] with a new [board].*)
val update_board : stack list -> t -> t

(** [update_splay_direction] is a stack 
    with the splay of [stack] changes to [direction]. *)
val update_splay_direction : stack -> Dogma.splay_direction -> stack

(** [update_stack_cards stack cards] is a stack 
    with the cards of [stack] changes to [cards]. *)
val update_stack_cards : stack -> Card.t list -> stack

(** [get_id player] is the id of [player]. *)
val get_id : t -> int

(** [get_hand player] is the hand of [player]. *)
val get_hand : t -> Card.t list

(** [get_hand_length player] is the number of cards in the hand of [player]. *)
val get_hand_length : t -> int

(** [print_hand player] is the string representation of the hand cards of 
    [player]. *)
val print_hand : t -> string

(** [get_ith_stack player i] is the stack of index [i] on the board of 
    [player]. *)
val get_ith_stack : t -> int -> stack

(** [get_stack_top player index] is the top card of stack 
    with the index [index] on the board of [player].
    Raise: Failure when the stack is empty. *)
val get_stack_top : t -> int -> string

(** [get_top_cards_name player index] is the name of the top card of stack
    with the index [index] on the board of [player].
    Raise: Failure when the stack is empty.*)
val get_top_card_name : t -> int -> string

(** [get_board_total_length player] is total number of cards 
    on the board of [player].*)
val get_board_total_length : t -> int

(** [print_board player] is the string representation of the board 
    of [player]. *)
val print_board : t -> string

(** [get_board player] is the board of [player].*)
val get_board : t -> stack list

(** [add_hand player card] is the player with a
    [card] added to the hand of [player]. *)
val add_hand : t -> Card.t -> t

(** [get_ith_hand player i] is the card of index [i] in the hand of [player]. 
    Raise: Failure when i is not a valid index. *)
val get_ith_hand : t -> int -> Card.t

(** [get_color_stack player c] is the stack with color [c] of [player]. *)
val get_color_stack : t -> Dogma.stack_color -> stack

(** [count_left stack icon acc] is the number of [icon] in a left-splayed [stack], 
    the initial number is [acc]. *)
val count_left : Card.t list -> Card.icon -> int -> int

(** [count_right stack icon acc] is the number of [icon] in a right-splayed [stack],
    the initial number is [acc]. *)
val count_right : Card.t list -> Card.icon -> int -> int

(** [count_up stack icon acc] is the number of [icon] in a up-splayed [stack],
    the initial number is [acc].*)
val count_up : Card.t list -> Card.icon -> int -> int

(** [count_no stack icon acc] is the number of [icon] in a no-splayed [stack],
    the initial number is [acc].*)
val count_no : Card.t list -> Card.icon -> int -> int

(** [count_icon board icon acc] is the number of [icon] on the [board],
    the initial number is [acc].*)
val count_icon : stack list -> Card.icon -> int -> int

(** [get_icon player icon] is the number of [icon] on the board of [player]. *)
val get_icon : t -> Card.icon -> int

(** [update_stack_list s_lst i new_s]update the ith stack with 
    [new_s] in the stack list *)
val update_stack_list : stack list -> int -> stack -> stack list

(** [pop_card i lst] is a card list and card pair, where the card list is [lst] 
    with [i]th card removed and the card is the [i]th card in [lst]
    Raise: Failure if [lst] is empty or doesn't have a [i]th element. *)
val pop_card : int -> Card.t list -> Card.t list * Card.t

(** [pop_stack i stack] is a stack and card pair, where the stack is [stack] 
    with [i]th card removed and the card is the [i]th card in [stack]
    Raise: Failure if [stack] is empty or doesn't have a [i]th element. *)
val pop_stack : int -> stack -> stack * Card.t

(** [remove_hand player i] is a player with [player]'s [i]the hand card removed.*)
val remove_hand : t -> int -> t

(** [push_stack card stack top] is the stack after [card] is pushed into [stack],
    when [top] is ture, the card is pushed to the top of the stack 
    and when [top] is false the card is pushed to the bottom of the stack.*)
val push_stack : Card.t -> stack -> bool -> stack

(** [push_card card card_list] is a card_list with [card] added to [card_list].*)
val push_card : Card.t -> Card.t list -> Card.t list

(** [add_stack palyer hand_idx top] is the stack of [player] with his card
    in the hand with [hand_idx] add to his stack according to the color 
    of the card.  When [top] is true the card is pushed to the top of the stack,
    otherwise bottom. *)
val add_stack : t -> int -> bool -> t

(** [remove_stack player color] removes the top element from a stack with 
    color [color] of player [player], return the updated stack and 
    the card removed. *)
val remove_stack : t -> Dogma.stack_color -> t * Card.t

(** [transfer_card_to_stack card_list] transfer one card from [card_list] to 
    stack, return updated card list and updated stack pair. *)
val transfer_card_to_stack : Card.t list -> stack -> int -> bool -> Card.t list * stack

(** [transfer_stack_to_card stack card_list] transfer on card from [stack] to
    [card_list], return the updates stack and card list pair. *)
val transfer_stack_to_card : stack -> Card.t list -> stack * Card.t list

(** [transfer_card_to_card card_list1 card_list2 idx] 
    transfer card at position [idx] from [card_list1] to [card_list2]. *)
val transfer_card_to_card : Card.t list -> Card.t list -> int -> Card.t list * Card.t list

(** [transfer_stack_to_stack stack1 stackt2 idx] 
    transfer the card from [stack1] to [stack2]. When [top] is true, transfer
    the top card, transfer the bottom card otherwise. *)
val transfer_stack_to_stack : stack -> stack -> bool -> stack * stack

(** [splay player color direction] splay the [player]'s [color] 
    stack to [direction].*)
val splay : t -> Dogma.stack_color -> Dogma.splay_direction -> t

(** [get_score_cards player] is the score cards list of [player]. *)
val get_score_cards : t -> Card.t list

(** [get_score player] is the score of [player] by summing the score cards. *)
val get_score : t -> int

(** [get_vlaue player i] is the value of the card of 
    index [i] on the hand of [player]. *)
val get_value : t -> int -> int

(** [update_score player score] is the player with player [player]'s score 
    updayted by [score].*)
val update_score : t -> Card.t list -> t

(** [updated_achievements player a] is the player with [player]'s achievements 
    updated by [a]. *)
val update_achievements : t -> int list -> t

(** [add_score player score_card] is the player with [score_card] 
    add to [player]'s score pile.*)
val add_score : t -> Card.t -> t

(** [get_achiements player] is the achievements list of player [player]. *)
val get_achievements : t -> int list

(** [add_achievement player era] is the player with achievements with [era] 
    number added to the achievements list of [player]. *)
val add_achievement : t -> int -> t

(** [print_player player] is the string represatation of [player]. *)
val print_player : t -> unit




