open State

(** [get_stack state id] is the stack of on the board of player [id]. *)
let get_stack state id = 
  State.get_emojis_by_id state id 

(** [print_blist list] prints out the string representing the board [list]. *)
let print_blist list =
  let helper cl =
    match cl with
    | "red" -> ANSITerminal.(print_string [red] "[]")
    | "purple" -> ANSITerminal.(print_string [magenta] "[]")
    | "yellow" -> ANSITerminal.(print_string [yellow] "[]")
    | "green" -> ANSITerminal.(print_string [green] "[]")
    | "blue" -> ANSITerminal.(print_string [blue] "[]")
    | " " -> ANSITerminal.(print_string [blue] "😰")
    | _ -> () in 
  list |> List.iter helper 

(** [hand state id] is the hand of player [id] in [state]. *)
let hand state id = 
  State.get_hand_size_by_id state id 

(** [score state id] is the score of player [id] in [state]. *)
let score state id = 
  State.get_score_by_id state id

(** [icon st id icon] is the number of [icon] 
    on the board of player [id] in [state].*)
let icon state id icon = 
  State.get_icon_by_id state id icon

(** [era_num st i] is the number of cards in era [i] in [st]. *)
let era_num state i=
  let era_cards = State.get_era_cards state in 
  List.length (List.nth era_cards i)

(** [card_num_dis st i] is the string of
    the number of cards in era [i] in [st]. *)
let card_num_dis st i = 
  let num = era_num st i in
  let str1 = if num < 10
    then  " " ^ (string_of_int num)
    else (string_of_int num) in
  "Era" ^ (string_of_int (i + 1)) ^ ": " ^ str1

(** [icon_dis st id ic] is the string representing the number of [ic] 
    on the board of player [id] in [st].*)
let icon_dis st id ic = 
  let num = icon st id ic in
  let str1 = if num < 10
    then  " " ^ (string_of_int num)
    else (string_of_int num) in
  match ic with 
  | Card.Castle -> ("🏰: " ^ str1)
  | Card.Lightbulb -> ("💡: " ^ str1)
  | Card.Clock -> ("⏰: " ^ str1)
  | Card.Crown -> ("👑: " ^ str1)
  | Card.Leaf -> ("🍃: " ^ str1)
  | Card.Factory -> ("🏭: " ^ str1)
  | _ -> ""

(** [printer st p1 p2 p3 p4] prints out 
    the current [st] with [p1], [p2], [p3], [p4]. *)
let printer state current_id player2 player3 player4 = 
  print_string " \n------------------------------------------------------------------------------ \n";
  print_string ("|            " ^ "player: " ^ (string_of_int player3) ^ "           " ^ (icon_dis state player3 Card.Castle) ^ "  " ^ (icon_dis state player3 Card.Crown) ^ "           " ^ (card_num_dis state 0) ^ "             | \n");
  print_string ("|            " ^ "score: " ^ (string_of_int (score state player3)) ^ "            " ^ (icon_dis state player3 Card.Lightbulb) ^ "  " ^ (icon_dis state player3 Card.Leaf) ^ "                                | \n");
  print_string ("|            " ^ "hand: " ^ (string_of_int (hand state player3)) ^ "             " ^ (icon_dis state player3 Card.Clock) ^ "  " ^ (icon_dis state player3 Card.Factory) ^ "                                | \n");
  print_string ("|            " ^ "board: "); print_blist (get_stack state player3); print_string ("                                                 | \n");
  print_string "|                                                                              | \n";
  print_string "|                                                                              | \n";
  print_string ("| " ^ "player: " ^ (string_of_int player4) ^ "       " ^ (icon_dis state player4 Card.Castle) ^ "  " ^ (icon_dis state player4 Card.Crown) ^ "           " ^ "next player: " ^ (string_of_int player2) ^ "     " ^ (icon_dis state player2 Card.Castle) ^ "  " ^ (icon_dis state player2 Card.Crown) ^ "   | \n");
  print_string ("| " ^ "score: " ^ (string_of_int (score state player4)) ^ "        " ^ (icon_dis state player4 Card.Lightbulb) ^ "  " ^ (icon_dis state player4 Card.Leaf) ^ "           " ^ "score: " ^ (string_of_int (score state player2)) ^ "           " ^ (icon_dis state player2 Card.Lightbulb) ^ "  " ^ (icon_dis state player2 Card.Leaf) ^ "   | \n");
  print_string ("| " ^ "hand: " ^ (string_of_int (hand state player4)) ^ "         " ^ (icon_dis state player4 Card.Clock) ^ "  " ^ (icon_dis state player4 Card.Factory) ^ "           " ^ "hand: " ^ (string_of_int (hand state player2)) ^ "            " ^ (icon_dis state player2 Card.Clock) ^ "  " ^ (icon_dis state player2 Card.Factory) ^ "   | \n");
  print_string ("| " ^ "board: "); print_blist (get_stack state player4); print_string ("                        " ^ "board: "); print_blist (get_stack state player2);  print_string ("                   | \n");
  print_string "|                                                                              | \n";
  print_string "|                                                                              | \n";
  print_string ("|            " ^ "current player: " ^ (string_of_int current_id) ^ "    " ^ (icon_dis state current_id Card.Castle) ^ "  " ^ (icon_dis state current_id Card.Crown) ^ "                               | \n");
  print_string ("|            " ^ "score: " ^  (string_of_int (score state current_id)) ^ "             " ^ (icon_dis state current_id Card.Lightbulb) ^ "  " ^ (icon_dis state current_id Card.Leaf) ^ "                               | \n");
  print_string ("|            " ^ "hand: " ^  (string_of_int (hand state current_id)) ^ "              " ^ (icon_dis state current_id Card.Clock) ^ "  " ^ (icon_dis state current_id Card.Factory) ^ "                               | \n");
  print_string ("|            " ^ "board: "); print_blist (get_stack state current_id); print_string ("                                                 | \n");
  print_string " ------------------------------------------------------------------------------ \n"; ()

(** [display st] prints out the current [st]. *)
let display state = 
  let current_id = State.get_current_player state in
  let player2 = (current_id + 1) mod 4 in 
  let player3 = (current_id + 2) mod 4 in 
  let player4 = (current_id + 3)  mod 4 in
  (printer state current_id player2 player3 player4)

