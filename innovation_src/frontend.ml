open State

let get_score state =
  let idx = State.get_current_player state in 
  (State.get_score_by_id state idx) :: 
  (State.get_score_by_id state ((idx + 1) mod 4)) :: 
  (State.get_score_by_id state ((idx + 2) mod 4)) ::
  (State.get_score_by_id state ((idx + 3) mod 4)) :: []

let get_stack state id = 
  State.get_stacks_by_id state id 

let print_blist list =
  let helper cl =
    match cl with
    | "red" -> ANSITerminal.(print_string [red] "[]")
    | "purple" -> ANSITerminal.(print_string [magenta] "[]")
    | "yellow" -> ANSITerminal.(print_string [yellow] "[]")
    | "green" -> ANSITerminal.(print_string [green] "[]")
    | "blue" -> ANSITerminal.(print_string [blue] "[]")
    | " " -> ANSITerminal.(print_string [blue] "😰 ")
    | _ -> () in 
  list |> List.iter helper 

let hand state id = 
  State.get_hand_size_by_id state id 

let score state id = 
  State.get_score_by_id state id

let display state = 
  let current_id = State.get_current_player state in
  let player2 = (current_id + 1) mod 4 in 
  let player3 = (current_id + 2) mod 4 in 
  let player4 = (current_id + 3)  mod 4 in
  (* print_bytes "U+1F7EA"; *)
  print_string " -------------------------------------------------------------- \n";
  print_string ("|            " ^ "player: " ^ (string_of_int player3) ^ "                                         | \n");
  print_string ("|            " ^ "score: " ^ (string_of_int (score state player3)) ^ "                                          | \n");
  print_string ("|            " ^ "hand: " ^ (string_of_int (hand state player3)) ^ "                                           | \n");
  print_string ("|            " ^ "board: "); print_blist (get_stack state player3); print_string ("                                 | \n");
  print_string "|                                                              | \n";
  print_string "|                                                              | \n";
  print_string ("| " ^ "player: " ^ (string_of_int player4) ^ "                                 " ^ "next player: " ^ (string_of_int player2) ^ "     | \n");
  print_string ("| " ^ "score: " ^ (string_of_int (score state player4)) ^ "                                  " ^ "score: " ^ (string_of_int (score state player2)) ^ "           | \n");
  print_string ("| " ^ "hand: " ^ (string_of_int (hand state player4)) ^ "                                   " ^ "hand: " ^ (string_of_int (hand state player2)) ^ "            | \n");
  print_string ("| " ^ "board: "); print_blist (get_stack state player4); print_string ("                         " ^ "board: "); print_blist (get_stack state player2);  print_string ("  | \n");
  print_string "|                                                              | \n";
  print_string "|                                                              | \n";
  print_string ("|            " ^ "current player: " ^ (string_of_int current_id) ^ "                                 | \n");
  print_string ("|            " ^ "score: " ^  (string_of_int (score state current_id)) ^ "                                          | \n");
  print_string ("|            " ^ "hand: " ^  (string_of_int (hand state current_id)) ^ "                                           | \n");
  print_string ("|            " ^ "board: "); print_blist (get_stack state current_id); print_string ("                                 | \n");
  print_string " -------------------------------------------------------------- \n";

