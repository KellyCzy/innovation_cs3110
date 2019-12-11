open State
open Frontend

(** [rnd_list n] is a list of n random integers *)
val rnd_list : int -> int list

(** [dogma_effect_ai state dogma] is the state after executing the
    effects of a dogma. *)
val dogma_effect_ai : State.t -> Dogma.effect -> State.t

(** [go_through_effects state dogma] is state after executing one 
    dogma. *)
val go_through_effects : State.t -> Dogma.t -> State.t

(** [execute_dogmas state dogmas] is the state after executing the 
    dogmas of a card. *)
val execute_dogmas : State.t -> Dogma.t list -> State.t

(** [strategy1 id state] is the state after the ai player draws a card
    from the card pile state and then draws another a hand card from the 
    card pile. *)
val strategy1 : int -> State.t -> State.t

(** [strategy2 id state] is the state after the ai player draws a card
    from the card pile state and then melds a hand card to its board. *)
val strategy2 : int -> State.t -> State.t

(** [strategy3 id state] is the state after first determining if at least one 
    stack of the ai player with id [id] has a card, and if so the state is 
    the one after the ai player draws a card from the card pile state and 
    then executes the dogmas of a stack card. *)
val strategy3 : int -> State.t -> State.t

(** [get_max_index lst value index] is the index of the maximum entry in [lst]. *)
val get_max_index : 'a list -> 'a -> int -> int

(** [ai_play_nondeterministic id state] is the state after the ai player with 
    index [id] nondeterministicly applies a strategy. *)
val ai_play_nondeterministic : int -> State.t -> State.t

(** [player_or_ai_nondeterministic id state] is the state after first determining
    whether the next player is still an ai player, and if so the state is the 
    state after [ai_play_nondeterministic (id + 1) state], and if not is just the 
    original state. *)
val player_or_ai_nondeterministic : int -> State.t -> State.t

(** [ai_play_deterministic id state] is the state after the ai player with 
    index [id] deterministicly applies a strategy. *)
val ai_play_deterministic : int -> int -> State.t -> State.t

(** [player_or_ai_deterministic id state] is the state after first determining
    whether the next player is still an ai player, and if so the state is the 
    state after [ai_play_deterministic (id + 1) state], and if not is just the 
    original state. *)
val player_or_ai_deterministic : int -> int -> State.t -> State.t