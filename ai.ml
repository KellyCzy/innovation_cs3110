open State
open Frontend

(** [rnd_list n] is a list of n random integers *)
let rnd_list (n : int) : int list =
  QCheck.Gen.(generate ~n int)

(** [dogma_effect_ai state dogma] is the state after executing the
    effects of a dogma.  *)
let dogma_effect_ai (state: State.t) (dogma : Dogma.effect) : State.t = 
  match dogma with
  | Draw x -> if (x<0) then
      State.draw state (State.current_player state) 0
    else 
      State.draw state (State.current_player state) x 
  | Meld x -> if (x<0) then
      State.meld state (State.current_player state) 0
    else
      State.meld state (State.current_player state) x 
  | Tuck x -> if (x<0) then
      State.tuck state (State.current_player state) 0
    else 
      State.tuck state (State.current_player state) x
  | Return x -> if (x<0) then
      State.return state (State.current_player state) 0
    else 
      State.return state (State.current_player state) x
  | Score x -> if (x<0) then
      State.score state (State.current_player state) 0
    else
      State.score state (State.current_player state) x 
  | Transfer (cp1, cp2, id) -> let other = State.get_player state id in
    let myself = State.current_player state in 
    State.transfer state myself other cp1 cp2 0 true
  | Splay (dir,color) -> 
    State.splay state (State.current_player state) color dir
  | _ -> print_string "Need to be completed \n"; state

(** [go_through_effects state dogma] is state after executing one 
    dogma. *)
let rec go_through_effects (state: State.t) (dogma: Dogma.t) : State.t =
  match dogma with 
  | [] -> state
  | x :: t -> let new_state = dogma_effect_ai state x in
    go_through_effects new_state t

(** [execute_dogmas state dogmas] is the state after executing the 
    dogmas of a card. *)
let execute_dogmas state dogmas = 
  match dogmas with 
  | x :: y :: [] -> let state_after_x = go_through_effects state x in 
    go_through_effects state_after_x y
  | _ -> failwith "impossible"

(** [strategy1 id state] is the state after the ai player draws a card
    from the card pile state and then draws another a hand card from the 
    card pile. *)
let strategy1 id state = 
  let state_drawn_first = 
    State.draw state (State.current_player state) 0 in 
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id 
                ^ "(AI) has just drawn a card.\n");
  let state_drawn_second = 
    State.draw state_drawn_first 
      (State.current_player state_drawn_first) 0 in
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id 
                ^ "(AI) has just drawn another card.\n\n");
  State.next_player state_drawn_second


(** [strategy2 id state] is the state after the ai player draws a card
    from the card pile state and then melds a hand card to its board. *)
let strategy2 id state = 
  let state_drawn_first = 
    State.draw state (State.current_player state) 0 in 
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id 
                ^ "(AI) has just drawn a card.\n");
  let state_melded_second = State.meld state_drawn_first 
      (State.current_player state_drawn_first) 0 in
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id 
                ^ "(AI) has just melded a card.\n\n");
  State.next_player state_melded_second

(** [strategy3 id state] is the state after first determining if at least one 
    stack of the ai player with id [id] has a card, and if so the state is 
    the one after the ai player draws a card from the card pile state and 
    then executes the dogmas of a stack card. *)
let strategy3 id state = 
  let exist = State.check_color_to_dogma_exist id state in 
  match exist with 
  | true -> 
    let state_drawn_first = State.draw state 
        (State.current_player state) 0 in 
    print_string ("......\n");
    print_string ("Player" ^ string_of_int id 
                  ^ "(AI) has just drawn a card.\n");
    let color = State.give_color_to_dogma id state_drawn_first in 
    let num = Player.map_color_to_int color in
    let stack = Player.get_ith_stack 
        (State.current_player state_drawn_first) num in
    let card = Player.get_top_card stack in
    let dogma = Card.get_dogma card in 
    let state_dogmaed_second = execute_dogmas 
        state_drawn_first dogma in
    print_string ("......\n");
    print_string ("Player" ^ string_of_int id 
                  ^ "(AI) has just excecuted the dogma of a card.\n\n");
    State.next_player state_dogmaed_second
  | false -> strategy2 id state


(** [get_max_index lst value index] is the index of the maximum entry in [lst]. *)
let rec get_max_index lst value index : int = 
  match lst with 
  | [] -> index
  | x :: t -> 
    if x > value then get_max_index t x (index + 1)
    else get_max_index t value index

(** [ai_play_nondeterministic id state] is the state after the ai player with 
    index [id] nondeterministicly applies a strategy. *)
let rec ai_play_nondeterministic id state = 
  print_string "\n";
  let lst = rnd_list 3 in
  let max_index = get_max_index lst 0 0 in 
  match max_index with 
  | 0 -> state |> strategy1 id |> player_or_ai_nondeterministic id
  | 1 -> state |> strategy2 id |> player_or_ai_nondeterministic id
  | 2 -> state |> strategy3 id |> player_or_ai_nondeterministic id
  | _ -> failwith "Impossible"

(** [player_or_ai_nondeterministic id state] is the state after first determining
    whether the next player is still an ai player, and if so the state is the 
    state after [ai_play_nondeterministic (id + 1) state], and if not is just the 
    original state. *)
and player_or_ai_nondeterministic id state = 
  if id = 3 then state
  else ai_play_nondeterministic (id + 1) state

(** [ai_play_deterministic id state] is the state after the ai player with 
    index [id] deterministicly applies a strategy. *)
let rec ai_play_deterministic id strategy state = 
  print_string "\n";
  match strategy with 
  | 0 -> state |> strategy1 id |> player_or_ai_deterministic id strategy
  | 1 -> state |> strategy3 id |> player_or_ai_deterministic id strategy
  | 2 -> state |> strategy2 id |> player_or_ai_deterministic id strategy
  | _ -> failwith "Impossible"

(** [player_or_ai_deterministic id state] is the state after first determining
    whether the next player is still an ai player, and if so the state is the 
    state after [ai_play_deterministic (id + 1) state], and if not is just the 
    original state. *)
and player_or_ai_deterministic id strategy state =
  if id = 3 then state 
  else state |> ai_play_deterministic (id + 1) (strategy + 1)