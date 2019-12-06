open State
open Frontend

let rnd_list (n : int) : int list =
  QCheck.Gen.(generate ~n int)

let strategy1 id state = 
  let state_drawn_first = State.draw state (State.current_player state) 0 in 
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id ^ "(AI) has just drawn a card.\n");
  let state_drawn_second = State.draw state_drawn_first (State.current_player state_drawn_first) 0 in
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id ^ "(AI) has just drawn another card.\n\n");
  State.next_player state_drawn_second

let strategy2 id state = 
  let state_drawn_first = State.draw state (State.current_player state) 0 in 
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id ^ "(AI) has just drawn a card.\n");
  let state_melded_second = State.meld state_drawn_first (State.current_player state_drawn_first) 0 in
  print_string ("......\n");
  print_string ("Player" ^ string_of_int id ^ "(AI) has just melded a card.\n\n");
  State.next_player state_melded_second

let rec ai_play id state = 
  print_string "\n";
  let lst = rnd_list 2 in
  if List.nth lst 0 > List.nth lst 1 then state |> strategy1 id |> player_or_ai id
  else state |> strategy2 id |> player_or_ai id

and player_or_ai id state = 
  if id = 3 then state
  else ai_play (id + 1) state
