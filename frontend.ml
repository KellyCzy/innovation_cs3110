open State

let get_score state =
  let idx = State.get_current_player state in 
  (State.get_score_by_id state idx) :: 
  (State.get_score_by_id state ((idx + 1) mod 4)) :: 
  (State.get_score_by_id state ((idx + 2) mod 4)) ::
  (State.get_score_by_id state ((idx + 3) mod 4)) :: []

let hand state id = 
  State.get_hand_size_by_id state id 

let score state id = 
  State.get_score_by_id state id

let display state = 
  let current_id = State.get_current_player state in
  let player2 = (current_id + 1) mod 4 in 
  let player3 = (current_id + 2) mod 4 in 
  let player4 = (current_id + 3) mod 4 in
  print_string " ---------------------------------------------------- \n";
  print_string ("|            " ^ "player: " ^ (string_of_int player3) ^ "                               | \n");
  print_string ("|            " ^ "score: " ^ (string_of_int (score state player3)) ^ "                                | \n");
  print_string ("|            " ^ "hand: " ^ (string_of_int (hand state player3)) ^ "                                 | \n");
  print_string "|                                                    | \n";
  print_string "|                                                    | \n";
  print_string ("| " ^ "player: " ^ (string_of_int player4) ^ "                           " ^ "player: " ^ (string_of_int player2) ^ "      | \n");
  print_string ("| " ^ "score: " ^ (string_of_int (score state player4)) ^ "                             " ^ "score: " ^ (string_of_int (score state player2)) ^ "      | \n");
  print_string ("| " ^ "hand: " ^ (string_of_int (hand state player4)) ^ "                               " ^ "hand: " ^ (string_of_int (hand state player2)) ^ "      | \n");
  print_string "|                                                    | \n";
  print_string "|                                                    | \n";
  print_string ("|            " ^ "player: " ^ (string_of_int current_id) ^ "                               | \n");
  print_string ("|            " ^ "score: " ^  (string_of_int (score state current_id)) ^ "                                | \n");
  print_string ("|            " ^ "hand: " ^  (string_of_int (hand state current_id)) ^ "                                 | \n");
  print_string " ----------------------------------------------------- \n";

