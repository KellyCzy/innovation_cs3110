open Game
open State
open Player
open Dogma
open Command

let total_era = 1

let game_init f =
  let json = f |> Yojson.Basic.from_file in 
  Game.all_cards json total_era |> State.init_state 


let win (state: State.t): bool = 
  if (List.length (List.nth state.players state.current_player |> 
                   Player.get_achievements) >= total_era +1)
  then true else false

(** Helper function *)
let rec run_game_1 state = 
  if state |> win then (print_string ("Game ends!"); 
                        print_string "\n"; exit 0)
  else
    print_endline ("It's your turn!\n");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> state
  | string -> match Command.parse string with
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
    (* | Dogma col -> 
      let num = Player.map_color_to_int col in
      let stack = Player.get_ith_stack (State.current_player state) num in
      let card = Player.get_top_card stack in
      let dogma = Card.get_dogma card in
      dogma_effect state dogma; *)
    | _ -> print_string "You didn't type in any command! \n";
      run_game_1 state

(** Helper function *)
let rec run_game_2 state = 
  if state |> win then (print_string ("Game ends!"); 
                        print_string "\n"; exit 0)
  else
    print_endline ("It's your turn!\n");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> state
  | string -> match Command.parse string with
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
      State.draw state (State.current_player state) x 
    | Achieve _ -> 
      State.achieve state (State.current_player state)
    (* | Dogma col -> 
      let num = Player.map_color_to_int col in
      let stack = Player.get_ith_stack (State.current_player state) num in
      let card = Player.get_top_card stack in
      let dogma = Card.get_dogma card in
      dogma_effect state dogma; *)
    | _ -> print_string "You didn't type in any command! \n";
      run_game_2 state

(* let dogma_effect state dogma = begin
  match dogma with
  | Draw x -> let new_state = State.draw state state.current_player x 
    in run_game_2 new_state;
  | Meld x -> let new_state = State.meld state state.current_player x 
    in run_game_2 new_state;
  | Tuck x -> let new_state = State.tuck state state.current_player x 
    in run_game_2 new_state;
  | _ -> print_string "Need to be completed \n";
end *)



let rec play_game state =
  let state_after_1 = run_game_1 state in
  let state_after_2 = run_game_2 state_after_1 in 
  play_game state_after_2

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Innovation engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> file_name |> game_init |> play_game  

(* Execute the game engine. *)
let () = main ()