open Card
open Player
open Dogma

type t

type card_pile = Dogma.card_pile

val init_state : (Card.t list) -> t

val current_player: t -> Player.t

val lowest_era : t -> int

val draw : t -> Player.t -> int -> t

val meld : t -> Player.t -> int -> t

val tuck : t -> Player.t -> int -> t

val splay : t -> Player.t -> color -> Dogma.splay_direction -> t

val return : t -> Player.t -> int -> t

val score : t -> Player.t -> int -> t

val transfer: t -> Player.t -> Player.t -> card_pile -> card_pile -> t

val achieve : t -> Player.t -> t

val next_player : t -> t