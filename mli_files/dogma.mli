
type splay_direction 

type stack_color 

type card_pile  

type effect 

type t

(* 
val meld: Card.t -> User.t -> User.t
(* choose a card list from the user, return the user with card melded *)

val draw: Card.t list -> User.t -> User.t

val tuck: Card.t -> Card.t list -> Card.t list

val splay: Card.t list -> splay_direction -> Card.t list

val score: Card.t -> Card.t list -> Card.t list

val demand: User.t list -> State.t
 *)
