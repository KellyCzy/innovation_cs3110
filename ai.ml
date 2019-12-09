open State
open Frontend

let game_end = ref 0

let rnd_list (n : int) : int list =
  QCheck.Gen.(generate ~n int)

let dogma_effect_ai (state: State.t) (dogma : Dogma.effect) : State.t = 
  match dogma with
  | Draw x -> if (x<0) then
      State.draw state (State.current_player state) 0
    else 
      State.draw state (State.current_player state) x 
  | Meld x -> State.meld state (State.current_player state) x 
  | Tuck x -> State.tuck state (State.current_player state) x 
  (* | Splay dir -> let new_state = State.splay state state.current_player col *)
  | Return x -> if (x<0) then 
      State.return state (State.current_player state) 0
    else 
      State.return state (State.current_player state) x
  | Score x -> State.score state (State.current_player state) x 
  | Transfer (cp1, cp2, id) -> let other = State.get_player state id in
    let myself = State.current_player state in 
    State.transfer state myself other cp1 cp2 0 true
  | _ -> print_string "Need to be completed \n"; state

let rec go_through_effects (state: State.t) (dogma: Dogma.t) : State.t =
  match dogma with 
  | [] -> state
  | x :: t -> let new_state = dogma_effect_ai state x in
    go_through_effects new_state t

let execute_dogmas state dogmas = 
  match dogmas with 
  | x :: y :: [] -> let state_after_x = go_through_effects state x in 
    go_through_effects state_after_x y
  | _ -> failwith "impossible"

(** Draw and draw *)
let strategy1 id state = 
  try (
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
    State.next_player state_drawn_second)
  with Win s -> print_endline(s); exit 0

(** Draw and meld *)
let strategy2 id state = 
  try (
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
    State.next_player state_melded_second)
  with Win s -> print_endline(s); exit 0

(** Draw and dogma *)
let strategy3 id state = 
  try (
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
    | false -> strategy2 id state)
  with Win s -> print_endline(s); exit 0

(** Return the index of the entry with maximum value *)
let rec get_max_index lst value index : int = 
  match lst with 
  | [] -> index
  | x :: t -> 
    if x > value then get_max_index t x (index + 1)
    else get_max_index t value index

let rec ai_play id state = 
  print_string "\n";
  let lst = rnd_list 3 in
  let max_index = get_max_index lst 0 0 in 
  match max_index with 
  | 0 -> state |> strategy1 id |> player_or_ai id
  | 1 -> state |> strategy2 id |> player_or_ai id
  | 2 -> state |> strategy3 id |> player_or_ai id
  | _ -> failwith "Impossible"

and player_or_ai id state = 
  if id = 3 then state
  else ai_play (id + 1) state
