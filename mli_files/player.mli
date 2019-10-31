open Card

type t

(** [get_hand t] is a set-list list of hand cards of player [t]. *)
val get_hand : t -> Card.t list


(** [post_hand t c d] is a set-like list of hand cards of player [t],
    after the card [c] is moved to/away from hand, the direction of which 
    is specified by [d]. *)
val set_hand : t -> Card.t -> bool -> Card.t list

(** [get_board t] is a set-like list of board cards of player [t]. *)
val get_board : t -> Card.t list

(** [post_board t c d] is a set-like list of board cards of player [t],
    after the card [c] is moved to/away from board, the direction of which 
    is specified by [d]. *)
val set_board : t -> Card.t -> bool -> Card.t list

(** [get_score t] is the current score of player [t]. *)
val get_score : t -> int

(** [post_score t x d] is the current score of player [t], after [x] points
    are added to/subtracted from score, the direction of which is specified
    by [d]. *)
val set_score : t -> int -> bool -> int

(** [get_achievement t] is a set-like list of achievements of player [t]. *)
val get_achievements : t -> int list

(** [post_achievement t a d] is a set-like list of achievements of player [t], 
    after the achievement [a] is placed/removed, the direction of which is 
    specified by [d]. *)
val set_achievements : t -> int -> bool -> int list

(** [meld t a] is a set-like list of board cards of player [t]. *)
val meld : t -> bool -> Card.t list
(** [draw t a] is a set-like list of board cards of player [t]. *)
val draw : t -> Card.t list
val achieve : t -> Achievement.t list
val dogma : t -> bool -> State.t
val first_action : t -> bool -> State.t
val second_action : t -> bool -> State.t
