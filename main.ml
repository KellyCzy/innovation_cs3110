open Game
open State
open Player
open Dogma
open Command
open Printf
open Frontend
open Ai

let total_era = 1

let game_init f =
  let json = f |> Yojson.Basic.from_file in 
  (Game.all_cards json total_era)|> State.init_state 

(* let win (state: State.t): bool = 
   if (List.length (List.nth state.players state.current_player |> 
                   Player.get_achievements) >= total_era +1)
   then true else false *)

let input_number act = 
  print_endline ("Please enter the index of the card you want to" ^ act ^ ". \n");
  print_endline ">";
  let str = read_line () in
  match Command.parse str with
  | Number int -> int
  | _ -> failwith "unimplemented"

let rec rec_return state = 
  try 
    let i = input_number "" in
    State.return state (State.current_player state) i
  with _ -> rec_return state


let dogma_effect (state: State.t) (dogma : Dogma.effect) : State.t = 
  match dogma with
  | Draw x -> if (x<0) then let i = input_number "draw" in
      State.draw state (State.current_player state) i
    else 
      State.draw state (State.current_player state) x 
  | Meld x -> if (x<0) then let i = input_number "meld" in
      State.meld state (State.current_player state) i
    else
      State.meld state (State.current_player state) x 
  | Tuck x -> if (x<0) then let i = input_number "tuck"  in
      State.tuck state (State.current_player state) i 
    else 
      State.tuck state (State.current_player state) x
  | Return x -> if (x<0) then let i = input_number "return" in
      State.return state (State.current_player state) i
    else 
      State.return state (State.current_player state) x
  | Score x -> if (x<0) then let i = input_number "score" in
      State.score state (State.current_player state) i 
    else
      State.score state (State.current_player state) x 
  | Transfer (cp1, cp2, id) -> let other = State.get_player state id in
    let myself = State.current_player state in 
    State.transfer state myself other cp1 cp2 0 true
  | Splay (dir,color) -> 
    State.splay state (State.current_player state) color dir
  | _ -> print_string "Need to be completed \n"; state

let rec go_through_effects (state: State.t) (dogma: Dogma.t) : State.t =
  match dogma with 
  | [] -> state
  | x :: t -> let new_state = dogma_effect state x in
    go_through_effects new_state t

let execute_dogmas state dogmas = 
  match dogmas with 
  | x :: y :: [] -> let state_after_x = go_through_effects state x in 
    go_through_effects state_after_x y
  | _ -> failwith "impossible"

let check_win state = 
  let cards = state.era_cards in
  let win = List.for_all (fun lst -> List.length lst = 0) cards in
  if win then
    let rec get_max_score = function
      | p::ps -> let (prev_id, prev_score) = get_max_score ps in
        let new_score = Player.get_score p in
        if new_score > prev_score then (Player.get_id p, new_score)
        else (prev_id, prev_score)
      | [] -> (-1, -1) in
    get_max_score (State.get_players state)
  else (-1, -1)

(** Helper function *)
let rec run_game_1 state = 
  let (id,score) = state |> check_win in
  if (id <> -1) && (score <> -1) then (print_string ("Game ends!"); 
                                       Printf.printf "Player %d wins" id;
                                       print_string "\n"; exit 0)
  else
    print_string "> ";
  match read_line () with
  | exception End_of_file -> state
  | string -> 
    try match Command.parse string with
      | exception Empty -> 
        print_string "You didn't type in any command! \n";
        run_game_1 state
      | exception Malformed -> 
        print_string "No such command! \n";
        print_string "You can only Meld/Draw/Dogma/Achieve \n";
        run_game_1 state
      | Meld x -> 
        State.meld state (State.current_player state) x 
      | Draw x -> 
        State.draw state (State.current_player state) x
      | Achieve _ -> 
        State.achieve state (State.current_player state) 
      | Hand ->
        let str = State.print_hand state in
        Frontend.display state;
        printf "Hand: %s\n" str;
        run_game_1 state
      | Board x ->
        let str = State.print_player_board state x in
        Frontend.display state;
        printf "Board of player #%d:\n %s" x str;
        print_string "\n";
        run_game_1 state
      | Score -> 
        let score = State.get_current_player_score state in
        printf "Score: %d\n" score;
        run_game_1 state
      | Help -> Printf.printf "    You are player %d.\n
      Now it's your turn.\n
      Here're a few possible commands you could try.\n
      🌟 draw [era_num]: draw a card from era [era_num], starting from 0.\n
      🌟 meld [hand_idx]: meld a card with index [hand_idx] from your hand cards, 
      starting from 0.\n
      🌟 board [player_idx]: display the player [player_idx]'s board cards, 
      [player_idx] ranges from 0 to 3.\n
      🌟 hand [player_idx]: display the player [player_idx]'s hand cards, 
      [player_idx] ranges from 0 to 3.\n
      🌟 score [player_idx]: display the player [player_idx]'s scores, 
      [player_idx] ranges from 0 to 3.\n
      🌟 dogma [color]: use the dogma effect on stack with color [color]. 
      Colors are red, purple, blue, green, yellow.\n" 
                  (State.get_current_player state);
        run_game_1 state
      | Dogma col -> 
        let num = Player.map_color_to_int col in
        let stack = Player.get_ith_stack (State.current_player state) 
            num in
        let card = Player.get_top_card stack in
        let dogma = Card.get_dogma card in
        execute_dogmas state dogma 
      | _ -> print_string "You didn't type in any command! \n";
        run_game_1 state
    with 
    | Empty_list str -> print_string (str ^ "\n"); 
      run_game_1 state
    | Failure str -> print_string (str ^ "\n"); 
      run_game_1 state
    | _ -> run_game_1 state


(** Helper function *)
let rec run_game_2 state = 
  let (id,score) = state |> check_win in
  if (id <> -1) && (score <> -1) then (print_string ("Game ends!"); 
                                       Printf.printf "Player %d wins" id;
                                       print_string "\n"; exit 0)
  else
    print_string "> ";
  match read_line () with
  | exception End_of_file -> state
  | string -> try match Command.parse string with
    | exception Empty -> 
      print_string "You didn't type in any command! \n";
      run_game_2 state
    | exception Malformed -> 
      print_string "No such command! \n";
      print_string "You can only Meld/Draw/Dogma/Achieve \n";
      run_game_2 state
    | Meld x -> 
      State.meld state (State.current_player state) x 
    | Draw x -> 
      let str = State.print_hand state in
      printf "Hand card: %s\n" str;
      State.draw state (State.current_player state) x 
    | Achieve _ -> 
      State.achieve state (State.current_player state)
    | Hand ->
      let str = State.print_hand state in
      printf "Hand card: %s\n" str;
      Frontend.display state;
      run_game_2 state
    | Board x ->
      let str = State.print_player_board state x in
      printf "Board of %d:\n %s" x str;
      Frontend.display state;
      run_game_2 state
    | Score -> 
      let score = State.get_current_player_score state in
      printf "Score: %d\n" score;
      run_game_2 state
    | Dogma col -> 
      let num = Player.map_color_to_int col in
      let stack = Player.get_ith_stack (State.current_player state) 
          num in
      let card = Player.get_top_card stack in
      let dogma = Card.get_dogma card in
      (* effect list list *)
      execute_dogmas state dogma
    | _ -> print_string "You didn't type in any command! \n";
      run_game_2 state
    with 
    | Empty_list str -> print_string (str ^ "\n"); 
      run_game_2 state
    | Failure str -> print_string (str ^ "\n"); 
      run_game_2 state

let rec play_game state =
  Frontend.display state;
  print_string "\n\n";
  printf "It's player %d's first turn!\n" (State.get_current_player state);
  let state_after_1 = run_game_1 state in
  if state_after_1 = state then (print_string " \n"; exit 0)
  else
    Frontend.display state_after_1;
  let winner1 = state_after_1 |> check_win in
  if fst winner1 > 0 
  then let () = Printf.printf 
           "The game's winner is player %d and the score is %d" 
           (fst winner1) (snd winner1) in (Stdlib.exit 0)
  else
    print_string "\n\n";
  printf "It's player %d's second turn!\n" 
    (State.get_current_player state_after_1);
  let state_after_2 = run_game_2 state_after_1 in
  if state_after_1 = state then (print_string " \n"; exit 0)
  else 
    let winner2 = state_after_2 |> check_win in
    if fst winner2 > 0 
    then let () = Printf.printf 
             "The game's winner is player %d and the score is %d" 
             (fst winner2) (snd winner2) in (Stdlib.exit 0)
    else
      let next_player_state = State.next_player state_after_2 in
      play_game next_player_state

let rec play_game_ai state = 
  try (
    Frontend.display state;
    print_string "\n\n";
    printf "It's player %d's first turn!\n" (State.get_current_player state);
    let state_after_1 = run_game_1 state in
    Frontend.display state_after_1;
    print_string "\n\n";
    printf "It's player %d's second turn!\n" 
      (State.get_current_player state_after_1);
    let state_after_2 = run_game_2 state_after_1 in
    let next_player_state = State.next_player state_after_2 in
    let state_after_ai = (Ai.ai_play 1 next_player_state) in 
    play_game_ai state_after_ai
  )
  with 
  | Win s -> print_endline(s); 
    let (id,score) = state |> check_win in
    if (id <> -1) && (score <> -1) 
    then (print_string ("Game ends!"); 
          Printf.printf "Player %d wins" id;
          print_string "\n"; exit 0)
    else (print_string ("Game ends! No one Wins! \n");)
  | Failure s -> print_endline(s)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Innovation engine.\n");
  print_string "Do you want to play with AI? (y/n)";
  match read_line() with
  | "y" -> 
    ANSITerminal.(print_string [green]
                    "Game Rules:\n
                    Meld: put card from your hand to your board, on top of the stack of matching color. 
                    Continue a spaly if one is present.\n
                    Draw: Take a card of value equal to your highest top card from the supply piels. 
                    If empty, draw from the next higher pile.\n
                    Achieve: To claim, must have a score of at least 5x age number in points, 
                    and a top card of equal or higher value. Points are not spent, you keep them.\n
                    Dogma/take action: Pick a top card on your board, and execute each effect on it in order. 
                    Effects are mandatory unless “You may” precedes them.\n
                    I Demand effects are executed by each player with fewer of the features icon than you, 
                    going clockwise. Read the effect out loud to them.\n
                    Opponents execute non-demand effects before you, 
                    if they have as many or more of the featured icon, going clockwise.\n
                    If any opponent shared a non-demand effect, and anything happened, 
                    take a single free Draw action at the conclusion of your Dogma action.\n");
    "innov.json" |> game_init |> play_game_ai
  | "n" -> 
    ANSITerminal.(print_string [green]
                    "Game Rules:\n
    Meld: put card from your hand to your board, on top of the stack of matching color. 
    Continue a spaly if one is present.\n
    Draw: Take a card of value equal to your highest top card from the supply piels. 
    If empty, draw from the next higher pile.\n
    Achieve: To claim, must have a score of at least 5x age number in points, 
    and a top card of equal or higher value. Points are not spent, you keep them.\n
    Dogma/take action: Pick a top card on your board, and execute each effect on it in order. 
    Effects are mandatory unless “You may” precedes them.\n
    I Demand effects are executed by each player with fewer of the features icon than you, 
    going clockwise. Read the effect out loud to them.\n
    Opponents execute non-demand effects before you, 
    if they have as many or more of the featured icon, going clockwise.\n
    If any opponent shared a non-demand effect, and anything happened, 
    take a single free Draw action at the conclusion of your Dogma action.\n");
    "innov.json" |> game_init |> play_game
  | _ -> ()

(* Execute the game engine. *)
let () = main ()