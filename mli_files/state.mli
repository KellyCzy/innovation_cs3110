open Player
open Card
open Command
open Printf

exception Win of string
exception Empty_list of string

type t

val range : int -> int list

val init_some_players : Player.t list -> int -> Player.t list

val init_state : Card.t list list -> t

val swap_player : Player.t -> Player.t list -> Player.t list

val update_player : t -> Player.t -> t

val update_player_lists : Player.t list -> t -> t

val update_era_cards : Card.t list list -> t -> t

val delete_one_achievement : t -> t

val get_current_player : t -> int

val get_players : t -> Player.t list

val get_player : t -> int -> Player.t

val get_era_cards : t -> Card.t list list

val get_era_cards_top : t -> Card.t

val get_score_by_id : t -> int -> int

val get_icon_by_id : t -> int -> Card.icon -> int

val get_hand_size_by_id : t -> int -> int

val check_empty : string -> Player.stack -> string

val get_emojis : Player.stack list -> string -> string list -> string list

val get_stack_by_id : t -> int -> string list

val current_player : t -> Player.t

val get_current_player_score : t -> int

val print_hand : t -> string

val lowest_era : t -> int

val update_era_list : 'a list -> int -> 'a -> 'a list

val draw : t -> Player.t -> int -> t

val meld : t -> Player.t -> int -> t

val tuck : t -> Player.t -> int -> t

val splay : t -> Player.t -> Dogma.stack_color -> Dogma.splay_direction -> t

val update_era : t -> Card.t -> Card.t list list

val return : t -> Player.t -> int -> t

val match_card_pile : Dogma.card_pile -> Player.t -> Player.t -> 
                      Card.t list option * Player.stack option

val is_other : Dogma.card_pile -> bool

val match_update_card_pile : Player.t -> Player.t -> Dogma.card_pile -> 
                             Card.t list -> Player.stack -> Player.t * Player.t

val procress_cl1_cl2 : Card.t list -> Card.t list -> Dogma.card_pile ->
                       Dogma.card_pile -> Player.stack -> Player.t -> Player.t -> 
                       int -> 'a -> Player.t * Player.t

val process_cl1_s2 : Card.t list -> Player.stack -> Dogma.card_pile ->
                     Dogma.card_pile -> Player.stack -> Card.t list -> 
                     Player.t -> Player.t -> int -> bool -> Player.t * Player.t

val process_s1_s2 : Player.stack -> Player.stack -> Dogma.card_pile -> 
                    Dogma.card_pile -> 'a -> Card.t list -> Player.t -> Player.t -> 
                    'b -> bool -> Player.t * Player.t

val process_s1_cl2 : Player.stack -> Card.t list -> Dogma.card_pile -> 
                     Dogma.card_pile -> Player.stack -> Card.t list -> 
                     Player.t -> Player.t -> 'a -> 'b -> Player.t * Player.t

val get_fields : t -> Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
                     Card.t list option * Player.stack option * Card.t list option *
                     Player.stack option * Player.stack * 'a list

val match_fields : Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
                   Player.stack -> Card.t list -> Card.t list option ->
                   Player.stack option -> Card.t list option ->
                   Player.stack option -> int -> bool -> Player.t * Player.t

val transfer : t -> Player.t -> Player.t -> Dogma.card_pile -> Dogma.card_pile ->
                    int -> bool -> t

val score : t -> Player.t -> int -> t

val achieve : t -> Player.t -> t

val print_player_board : t -> int -> string

val search_color : Player.stack list -> Dogma.stack_color

val give_color_to_dogma : int -> t -> Dogma.stack_color

val search_color_exist : Player.stack list -> bool

val check_color_to_dogma_exist : int -> t -> bool

val next_player : t -> t