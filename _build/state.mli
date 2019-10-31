open Card

type t

val init_state : (Card.t list) -> t

val current_player: t -> Player.t

val draw

val meld

val 