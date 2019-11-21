open Game
open State
open Player
open Dogma
open Command
open Printf
open Frontend

let total_era = 1

let game_init f =
  let json = f |> Yojson.Basic.from_file in 
  (Game.all_cards json total_era)|> State.init_state 

let win (state: State.t): bool = 
  if (List.length (List.nth state.players state.current_player |> 
                   Player.get_achievements) >= total_era +1)
  then true else false

let input_number () = 
  print_endline "Please enter the index of the card. \n";
  print_endline ">";
  let str = read_line () in
  match Command.parse str with
  | Number int -> int
  | _ -> failwith "unimplemented"

let rec rec_return state = 
  try 
    let i = input_number () in
    State.return state (State.current_player state) i
  with _ -> rec_return state

let dogma_effect (state: State.t) (dogma : Dogma.effect) :State.t = 
  match dogma with
  | Draw x -> if (x<0) then let i = input_number () in
      State.draw state (State.current_player state) i
    else 
      State.draw state (State.current_player state) x 
  | Meld x -> State.meld state (State.current_player state) x 
  | Tuck x -> State.tuck state (State.current_player state) x 
  (* | Splay dir -> let new_state = State.splay state state.current_player col *)
  | Return x -> if (x<0) then let i = input_number () in
      State.return state (State.current_player state) i
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
  | x :: t -> let new_state = dogma_effect state x in
    go_through_effects new_state t

let execute_dogmas state dogmas = 
  match dogmas with 
  | x :: y :: [] -> let state_after_x = go_through_effects state x in 
    go_through_effects state_after_x y
  | _ -> failwith "impossible"

(** Helper function *)
let rec run_game_1 state = 
  if state |> win then (print_string ("Game ends!"); 
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
        printf "Hand: %s\n" str;
        Frontend.display state;
        run_game_1 state
      | Board x ->
        let str = State.print_player_board state x in
        printf "Board of player #%d:\n %s" x str;
        print_string "\n";
        Frontend.display state;
        run_game_1 state
      | Score -> 
        let score = State.get_current_player_score state in
        printf "Score: %d\n" score;
        run_game_1 state
      | Dogma col -> 
        let num = Player.map_color_to_int col in
        let stack = Player.get_ith_stack (State.current_player state) num in
        let card = Player.get_top_card stack in
        let dogma = Card.get_dogma card in
        execute_dogmas state dogma 
      | _ -> print_string "You didn't type in any command! \n";
        run_game_1 state
    with 
    | Failure str -> print_string (str ^ "\n"); 
      run_game_1 state
    | _ -> run_game_1 state

(** Helper function *)
let rec run_game_2 state = 
  if state |> win then (print_string ("Game ends!"); 
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
      let stack = Player.get_ith_stack (State.current_player state) num in
      let card = Player.get_top_card stack in
      let dogma = Card.get_dogma card in
      (* effect list list *)
      execute_dogmas state dogma
    | _ -> print_string "You didn't type in any command! \n";
      run_game_2 state

    with 
    | Failure str -> print_string (str ^ "\n"); 
      run_game_2 state

let rec play_game state =
  Frontend.display state;
  print_string "\n\n";
  printf "It's player %d's first turn!\n" (State.get_current_player state);
  let state_after_1 = run_game_1 state in
  Frontend.display state_after_1;
  print_string "\n\n";
  printf "It's player %d's second turn!\n" (State.get_current_player state_after_1);
  let state_after_2 = run_game_2 state_after_1 in
  let next_player_state = State.next_player state_after_2 in
  play_game next_player_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Innovation engine.\n");
  ANSITerminal.(print_string [green]
                  "\nInstructions:\n'draw x' to draw a card from card pile x\n'meld x' to meld your xth hand card\n'dogma c' to execute the dogmas of the top card in stack c\n\n");
  "innov.json" |> game_init |> play_game

(* Execute the game engine. *)
let () = main ()