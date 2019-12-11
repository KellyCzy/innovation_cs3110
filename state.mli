open Player
open Card
open Command
open Printf

(** [Win s] is raised when all the era cards are drawn *)
exception Win of string

(** [Empty_list s] is raised when there is no card in the list anymore *)
exception Empty_list of string

(** [t] is a type of state. A state includes players, era_cards, 
    achievements, current_player, and lowest_era *)
type t 

(** [range n] returns the first n natural numbers: [0; n-1] *)
val range : int -> int list

(** [init_some_players acc] returns initialized players *)
val init_some_players : Player.t list -> int -> Player.t list

(** [init_state cards_list] takes in a cards_list that is read from json and 
    returns the initial state generated from that cards_list *)
val init_state : Card.t list list -> t

(** [swap_player new_player player_list] returns a swapped list of player 
    with new_player inserted *)
val swap_player : Player.t -> Player.t list -> Player.t list

(** [update_player state new_player] returns a state with a new player 
    inserted into players in the original state *)
val update_player : t -> Player.t -> t

(** [update_player_lists players state] returns a state with new players 
    inserted into players in the original state *)
val update_player_lists : Player.t list -> t -> t

(** [update_era_cards era_cards state] returns a state with updated 
    era_cards *)
val update_era_cards : Card.t list list -> t -> t

(** [delete_one_achievement state] returns a state with first element 
    removed from achievements *)
val delete_one_achievement : t -> t

(** [get_current_player state] returns the current player in the state *)
val get_current_player : t -> int

(** [get_players state] returns players in the state *)
val get_players : t -> Player.t list

(** [get_player state id] returns the player in the state with that id. 
    It prints message when there's no player with id id *)
val get_player : t -> int -> Player.t

(** [get_era_cards state] returns all the cards that are in the era 
    cards pile, which are cards that are not drawn *)
val get_era_cards : t -> Card.t list list

(** [get_era_cards_top state] returns the first card in era_cards *)
val get_era_cards_top : t -> Card.t

(** [get_score_by_id state id] returns the score of player with specific id *)
val get_score_by_id : t -> int -> int

(** [get_icon_by_id state id icon] returns number of specific icon 
    in a player with specific id *)
val get_icon_by_id : t -> int -> Card.icon -> int


(** [get_hand_size_by_id state id] returns the number of cards one player 
    with specific id holds *)
val get_hand_size_by_id : t -> int -> int

(** [check_empty color stack] checks if the stack of color color is empty.
    If it's empty, it returns empty string. If it's not empty, 
    it returns that color *)
val check_empty : string -> Player.stack -> string

(** [get_emojis stack name acc] takes in a name, a stack list, and 
    an accumulator and returns a string list. It returns the string 
    representation of board cards with color 'name'. If name is red, 
    and there's no card on board, it returns " ", if there is a card, 
    it retuns "red" *)
val get_emojis : Player.stack list -> string -> string list -> string list

(** [get_emojis_by_id] is the emojis on stack with id [id] *)
val get_emojis_by_id : t -> int -> string list

(** [current_player state] takes in a state and returns the current player 
    in that state *)
val current_player : t -> Player.t

(** [get_current_player_score state] takes in a state and returns the 
    current player's score in that state *)
val get_current_player_score : t -> int

(** [print_hand state] takes in a state and returns the 
    string representation of current player's hand in that state. *)
val print_hand : t -> string

(** [lowest_era state] takes in a state and returns the 
    lowest_era of that state. *)
val lowest_era : t -> int

(** [update_era_list e_lst i new_e] updates the ith era in e_lst with 
    new_e, return an updated e_lst *)
val update_era_list : 'a list -> int -> 'a -> 'a list

(** [draw state player era] lets a player in state draws a card from 
    era 'era' and returns a new state *)
val draw : t -> Player.t -> int -> t

(** [meld state player hand_idx] lets a player in state melds a card from 
    hand with hand index hand_idx and returns a new state *)
val meld : t -> Player.t -> int -> t

(** [tuck state player hand_idx] lets a player tuck a card from hand with 
    hand index hand_idx and returns a new state *)
val tuck : t -> Player.t -> int -> t

(** [splay state player color direction] lets a player splay a stack 
    with color 'color' and the stack can be splayed toward right, up, 
    and left. *)
val splay : t -> Player.t -> Dogma.stack_color -> Dogma.splay_direction -> t

(** [update_era state card] takes in a card and returns all era cards with 
    this card inserted *)
val update_era : t -> Card.t -> Card.t list list

(** [return state player hand_idx] lets a player return a card 
    from hand hand_idx and returns a new state *)
val return : t -> Player.t -> int -> t

(** [match_card_pile card_pile myself other] returns the specific 
    card_pile from either self or other depending on the card_pile *)
val match_card_pile : Dogma.card_pile -> Player.t -> Player.t -> 
  Card.t list option * Player.stack option

(** [is_other card_pile1] takes in a card pile and determines if 
    it's other's pile or self's pile. It returns true if it's other's 
    pile and false if it's not *)
val is_other : Dogma.card_pile -> bool

(** [match_update_card_pile myself other card_pile 
    updated_card_list updated_stack] updates card pile depending on 
    card_pile. If card_pile is other's pile, it uses card_pile to 
    update other's stack, and vice cersa *)
val match_update_card_pile : Player.t -> Player.t -> Dogma.card_pile -> 
  Card.t list -> Player.stack -> Player.t * Player.t

(** [procress_cl1_cl2 cl1 cl2 card_pile1 card_pile2 fake_stack 
    myself other idx top] returns updated card lists cl1 and cl2. *)
val procress_cl1_cl2 : Card.t list -> Card.t list -> Dogma.card_pile ->
  Dogma.card_pile -> Player.stack -> Player.t -> Player.t -> 
  int -> 'a -> Player.t * Player.t

(** [process_cl1_s2 cl1 s2 card_pile1 card_pile2 fake_stack 
    fake_card_list myself other idx top] returns updated card lists cl1 
    and stacks s2. *)
val process_cl1_s2 : Card.t list -> Player.stack -> Dogma.card_pile ->
  Dogma.card_pile -> Player.stack -> Card.t list -> 
  Player.t -> Player.t -> int -> bool -> Player.t * Player.t

(** [process_s1_s2 s1 s2 card_pile1 card_pile2 fake_stack fake_card_list 
    myself other idx top] returns updated stack s1 and s2 *)
val process_s1_s2 : Player.stack -> Player.stack -> Dogma.card_pile -> 
  Dogma.card_pile -> 'a -> Card.t list -> Player.t -> Player.t -> 
  'b -> bool -> Player.t * Player.t

(** [process_s1_cl2 s1 cl2 card_pile1 card_pile2 
    fake_stack fake_card_list myself other idx top] returns updated 
    stack s1 and card list cl2 *)
val process_s1_cl2 : Player.stack -> Card.t list -> Dogma.card_pile -> 
  Dogma.card_pile -> Player.stack -> Card.t list -> 
  Player.t -> Player.t -> 'a -> 'b -> Player.t * Player.t

(** [get_fields state myself other card_pile1 card_pile2] match and 
    returns card list and stack options *)
val get_fields : t -> Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
  Card.t list option * Player.stack option * Card.t list option *
  Player.stack option * Player.stack * 'a list

(** [match_fields myself other card_pile1 card_pile2 
    fake_stack fake_card_list card_list1 stack1 card_list2 
    stack2 idx top] match card piles and updates myself and other accordingly *)
val match_fields : Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
  Player.stack -> Card.t list -> Card.t list option ->
  Player.stack option -> Card.t list option ->
  Player.stack option -> int -> bool -> Player.t * Player.t

(** [transfer state myself' other' card_pile1 card_pile2 idx top] lets 
    a player transfer card from one pile to another. top indicates if the 
    card is on top of the stack. *)
val transfer : t -> Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
  int -> bool -> t

(** [score state player hand_idx] lets 
    a player score a card from his hand with hand index hand_idx *)
val score : t -> Player.t -> int -> t

(** [achieve state player] lets 
    a player achieve a card achievements *)
val achieve : t -> Player.t -> t

(** [print_player_board state index] returns string representation of a 
    player's board *)
val print_player_board : t -> int -> string

(** [search_color board] returns the color of the specific board *)
val search_color : Player.stack list -> Dogma.stack_color

(** [give_color_to_dogma id state] returns the color of the specific 
    board of a player with id 'id' *)
val give_color_to_dogma : int -> t -> Dogma.stack_color

(** [search_color_exist board] returns true if the board of a player 
    is not entirely empty, and false if otherwise *)
val search_color_exist : Player.stack list -> bool

(** [check_color_to_dogma_exist id state] returns true if the player
    with id [id] has a board that is not entirely empty, and false
    if otherwise *)
val check_color_to_dogma_exist : int -> t -> bool

(** [next_player state] returns a state when current player is moved 
    to next player *)
val next_player : t -> t