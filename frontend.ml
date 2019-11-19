open State

let get_score state =
  let idx = State.get_current_player state in 
  (State.get_score_by_id state idx) :: 
  (State.get_score_by_id state ((idx + 1) mod 4)) :: 
  (State.get_score_by_id state ((idx + 2) mod 4)) ::
  (State.get_score_by_id state ((idx + 3) mod 4)) :: []

let display state = 
  print_string " ---------------------------------------------------- \n";
  print_string ("|          " ^ "player: " ^ (string_of_int (((State.get_current_player state) + 2) mod 4)) ^ "                                 | \n");
  print_string ("|          " ^ "score: " ^ (string_of_int (List.nth (get_score state) 2)) ^ "                                  | \n");
  print_string "|                                                    | \n";
  print_string "|                                                    | \n";
  print_string ("| " ^ "player: " ^ (string_of_int (((State.get_current_player state) + 3) mod 4)) ^ "                           " ^ "player: " ^ (string_of_int (((State.get_current_player state) + 1) mod 4)) ^ "      | \n");
  print_string ("| " ^ "score: " ^ (string_of_int (List.nth (get_score state) 3)) ^ "                             " ^ "score: " ^ (string_of_int (List.nth (get_score state) 1)) ^ "      | \n");
  print_string "|                                                    | \n";
  print_string "|                                                    | \n";
  print_string ("|            " ^ "player: " ^ (string_of_int (State.get_current_player state)) ^ "                               | \n");
  print_string ("|            " ^ "score: " ^  (string_of_int (List.nth (get_score state) 0)) ^ "                                | \n");
  print_string " ----------------------------------------------------- \n";

