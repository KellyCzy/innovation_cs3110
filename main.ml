open Game
open State
open Player
open Dogma
open Command

let total_era = 1

let game_init f =
  f |> Yojson.Basic.from_file |> game.all_cards total_era
  |> State.init_state 


let win (state: State.t): bool = 
  if (List.length (List.nth state.players state.current_player |> 
                   Player.get_achievements) >= total_era +1)
  then true else false


(** Helper function *)
let rec run_game game state = 
  if state |> win then (print_string ("Game ends!"); 
                        print_string "\n"; exit 0)
  else
    print_endline ("It's your turn!\n");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | string -> match Command.parse string with
    | exception Empty -> 
      print_string "You didn't type in any command! \n";
      run_game game state
    | exception Malformed -> 
      print_string "No such command! \n";
      print_string "You can only Meld/Draw/Dogma/Achieve \n";
      run_game game state
    | 



      (** [main ()] prompts for the game to play, then starts it. *)
      let main () =
        ANSITerminal.(print_string [red]
                        "\n\nWelcome to the Innovation engine.\n");
        print_endline "Please enter the name of the game file you want to load.\n";
        print_string  "> ";
        match read_line () with
        | exception End_of_file -> ()
        | file_name -> game_init file_name

(* Execute the game engine. *)
let () = main ()
