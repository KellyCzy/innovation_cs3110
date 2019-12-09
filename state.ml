open Player
open Card
open Command
open Printf

exception Win of string
exception Empty_list of string

type t = {
  players: Player.t list;
  era_cards: Card.t list list;
  achievements: int list;
  current_player: int;
  lowest_era: int;
}

(* the first n natural numbers: [0; n-1] *)
let range n = 
  let rec range' acc = function
    | 0 -> acc
    | n -> range' ((n-1)::acc) (n-2)
  in range' [] n

let rec init_some_players acc= function
  | -1 -> acc
  | i -> init_some_players ((Player.init_player i)::acc) (i-1)

let init_state (cards_list : Card.t list list) : t = {
  players = init_some_players [] 3;
  era_cards = cards_list;
  achievements = range 10;
  current_player = 0;
  lowest_era = 0;
}

let swap_player (new_player: Player.t) 
    (player_list: Player.t list) : Player.t list= 
  let index = Player.get_id new_player in
  let rec swap acc = function
    | [] -> failwith "player_list is empty"
    | x::xs -> if (Player.get_id x) = index 
      then (acc @ [new_player] @ xs)
      else swap (acc@[x]) xs in
  swap [] player_list

let update_player (state : t) (new_player : Player.t): t = {
  players = swap_player new_player state.players;
  era_cards = state.era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let update_player_lists (players : Player.t list) (state : t)  : t = {
  players = players;
  era_cards = state.era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let update_era_cards era_cards state  = {
  players = state.players;
  era_cards = era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let delete_one_achievement state = {
  players = state.players;
  era_cards = state.era_cards;
  achievements = List.tl state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let get_current_player (state: t): int = 
  state.current_player

let get_players (state:t) : Player.t list = 
  state.players

let get_player (state: t) (id: int) : Player.t = 
  try 
    (* Printf.printf "line 77: length %d" (List.length state.players);
       Printf.printf "index %d\n" id; *)
    if id < 0 then List.nth state.players (state.current_player+1)
    else List.nth state.players id 
  with f -> Printf.printf 
              "The index of player you're looking for is %d\n" id; raise f

let get_era_cards state = 
  state.era_cards

let get_score_by_id (state: t) (id: int) : int = 
  let player = get_player state id in 
  Player.get_score player

let get_hand_size_by_id (state: t) (id: int) : int = 
  let player = get_player state id in 
  Player.get_hand player |> List.length

let check_empty (color: string) (stack: stack) : string =
  match stack.cards with 
  | [] -> " "
  | _ -> color

let get_emojis (stack: stack list) (name: string) 
    (acc: string list) : string list =
  match name with 
  | "red" -> 
    (List.find (fun x -> Player.get_stack_color x = Dogma.Red) 
       stack |> check_empty "red") :: acc
  | "purple" -> 
    (List.find (fun x -> Player.get_stack_color x = Dogma.Purple) 
       stack |> check_empty "purple") :: acc
  | "blue" ->
    (List.find (fun x -> Player.get_stack_color x = Dogma.Blue) 
       stack |> check_empty "blue") :: acc
  | "green" -> 
    (List.find (fun x -> Player.get_stack_color x = Dogma.Green) 
       stack |> check_empty "green") :: acc
  | "yellow" ->
    (List.find (fun x -> Player.get_stack_color x = Dogma.Yellow) 
       stack |> check_empty "yellow") :: acc
  | _ -> failwith "impossible"

let get_stacks_by_id (state: t) (id: int) : string list =
  let player = get_player state id in 
  let board = player |> Player.get_board in 
  let after_red = get_emojis board "red" [] in 
  let after_purple  = get_emojis board "purple" after_red in
  let after_blue  = get_emojis board "blue" after_purple in
  let after_green  = get_emojis board "green" after_blue in
  let after_yellow = get_emojis board "yellow" after_green in
  after_yellow
(* String.concat " " after_yellow *)

let current_player (state: t) : Player.t= 
  (* Printf.printf "line 120: length %d" (List.length state.players);
     Printf.printf "index %d\n" state.current_player; *)
  let updated_player = List.nth state.players state.current_player in
  updated_player


let get_current_player_score (state:t): int = 
  state |> current_player |> Player.get_score

let print_hand state = 
  Player.print_hand (current_player state)

let lowest_era (state: t) : int = 
  state.lowest_era

(* update the ith era in e_lst with new_e, return an updated e_lst *)
let update_era_list e_lst i new_e = 
  let rec update' acc idx = function
    | [] -> acc
    | x::xs when idx = i -> acc @ [new_e] @ xs
    | x::xs -> update' (x::acc) (idx+1) xs 
  in update' [] 0 e_lst

let draw (state: t) (player: Player.t) (era: int): t = 
  let era_num = (max state.lowest_era era) in 
  let era_to_remove = try List.nth state.era_cards era_num  
    with _ -> raise (Empty_list "There is no card in this ear.") in
  if List.length era_to_remove != 0 then 
    let updated_era, card_to_draw = Player.pop_card 0 era_to_remove in
    ANSITerminal.(print_string [green] ("\nYou just drew a card [" ^ (Card.get_title card_to_draw) ^ "]\n"));
    let updated_player = Player.add_hand player card_to_draw in
    (* Player.print_player updated_player; *)
    let state_with_updated_player = update_player state updated_player in
    let updated_eras = update_era_list state.era_cards era_num 
        updated_era in
    state_with_updated_player |> update_era_cards updated_eras 
  else raise (Win "There is no more card to draw")
(* else let () = print_string "There is no more card to draw" in
   state *)

let meld (state: t) (player: Player.t) (hand_idx: int): t = 
  let melded = Player.get_ith_hand player hand_idx in
  ANSITerminal.(print_string [green] ("\nYou just melded a card [" ^ (Card.get_title melded) ^ "]\n"));
  let updated_player = Player.add_stack player hand_idx true in
  update_player state updated_player

(** No Test *)
let tuck (state: t) (player: Player.t) (hand_idx:int):t = 
  ANSITerminal.(print_string [green] ("\nYou just tucked a card [" ^ (Card.get_title (Player.get_ith_hand player hand_idx)) ^ "]\n"));
  let updated_player = Player.add_stack player hand_idx false in
  update_player state updated_player

(** NO Test*)
let splay (state: t) (player: Player.t) (color: Dogma.stack_color) 
    (direction: Dogma.splay_direction) = 
  let updated_player = Player.splay player color direction in
  update_player state updated_player

let update_era state card: Card.t list list =
  let era = Card.get_value card in
  (* Printf.printf "era %d\n" era;
     Printf.printf "length state.era_cards %d\n" 
     (List.length state.era_cards);
     Printf.printf "length state.era_cards[0] %d\n" 
     (state.era_cards |> List.hd |> List.length); *)
  (* let era_cards = List.hd state.era_cards in *)
  (* Printf.printf "length era_cards %d\n" (era_cards |> List.length); *)

  let era_cards = List.nth state.era_cards era in
  update_era_list state.era_cards era (era_cards@[card])

let return (state: t) (player: Player.t) (hand_idx: int): t = 
  let updated_hand_cards, card = Player.pop_card hand_idx 
      (Player.get_hand player) in
  ANSITerminal.(print_string [green] ("\nYou just returned a card [" ^ (Card.get_title card) ^ "]\n"));
  let updated_player = update_hand updated_hand_cards player in
  let updated_state = update_player state updated_player in 
  update_era_cards (update_era state card) updated_state

let match_card_pile (card_pile: Dogma.card_pile) 
    (myself: Player.t) (other: Player.t) = 
  match card_pile with 
  | Self_hand _ ->
    (* print_endline "self_hand"; *)
    Some (Player.get_hand myself), None
  | Other_hand _ -> 
    (* print_endline "Other_hand"; *)
    Some (Player.get_hand other), None
  | Self_score _ -> 
    (* print_endline "self_score"; *)
    Some (Player.get_score_cards myself), None
  | Other_score _ -> 
    (* print_endline "Other_score"; *)
    Some (Player.get_score_cards other), None
  | Self_stack c ->  
    None, Some (Player.get_color_stack myself c)
  | Other_stack c -> None, Some (Player.get_color_stack other c)

let is_other card_pile1 = 
  match card_pile1 with 
  | Dogma.Other_hand _ -> true
  | Dogma.Other_score _ -> true
  | Dogma.Other_stack _ -> true
  | _ -> false

let match_update_card_pile (myself: Player.t) (other: Player.t) 
    (card_pile: Dogma.card_pile) (updated_card_list: Card.t list) 
    (updated_stack: stack) : Player.t * Player.t=
  match card_pile with 
  | Self_hand _ -> Player.update_hand updated_card_list myself, other
  | Other_hand _ -> myself, Player.update_hand updated_card_list other
  | Self_score _ -> 
    (* print_endline "self score:"; *)
    (* Player.print_player (Player.update_score myself updated_card_list); *)
    Player.update_score myself updated_card_list, other
  | Other_score _ -> let temp = Player.update_score 
                         other updated_card_list in
    (* Printf.printf "temp score length %d\n" (List.length (Player.get_score_cards temp));
       print_endline "other score:"; 
       Player.print_player temp; *)
    myself, temp
  | Self_stack c -> update_board 
                      (Player.update_stack_list 
                         (Player.get_board myself) 
                         (Player.map_color_to_int c) updated_stack) 
                      myself, other
  | Other_stack c -> myself, update_board 
                       (Player.update_stack_list (Player.get_board other) 
                          (Player.map_color_to_int c) updated_stack) other

(*pile1 lose one card, pile2 get one card 
*)
let procress_cl1_cl2 
    cl1 cl2 card_pile1 card_pile2 fake_stack myself other idx top = 
  let updated_cl1, updated_cl2 = Player.transfer_card_to_card 
      cl1 cl2 idx in 
  if (is_other card_pile1) then
    let _, updated_pl1 = match_update_card_pile 
        myself other card_pile1 updated_cl1 fake_stack in
    let updated_pl2, _ = match_update_card_pile 
        myself other card_pile2 updated_cl2 fake_stack in
    updated_pl1, updated_pl2

  else
    let updated_pl1, _ = match_update_card_pile 
        myself other card_pile1 updated_cl1 fake_stack in
    let _, updated_pl2 = match_update_card_pile 
        myself other card_pile2 updated_cl2 fake_stack in
    updated_pl1, updated_pl2

let process_cl1_s2 cl1 s2 card_pile1 card_pile2 
    fake_stack fake_card_list myself other idx top = 
  let updated_cl1, updated_s2 = Player.transfer_card_to_stack cl1 
      s2 idx top in
  if (is_other card_pile1) then
    let _, updated_pl1= match_update_card_pile 
        myself other card_pile1 updated_cl1 fake_stack in
    let updated_pl2, _ = match_update_card_pile 
        myself other card_pile2 fake_card_list updated_s2 in
    updated_pl1, updated_pl2

  else
    let updated_pl1, _ = match_update_card_pile 
        myself other card_pile1 updated_cl1 fake_stack in
    let _, updated_pl2 = match_update_card_pile 
        myself other card_pile2 fake_card_list updated_s2 in
    updated_pl1, updated_pl2

let process_s1_s2 s1 s2 card_pile1 card_pile2 
    fake_stack fake_card_list myself other idx top = 
  let updated_s1, updated_s2 = Player.transfer_stack_to_stack 
      s1 s2 top in
  if (is_other card_pile1) then
    let _, updated_pl1= match_update_card_pile
        myself other card_pile1 fake_card_list updated_s1 in
    let updated_pl2, _ = match_update_card_pile 
        myself other card_pile2 fake_card_list updated_s2 in
    updated_pl1, updated_pl2
  else
    let updated_pl1, _ = match_update_card_pile
        myself other card_pile1 fake_card_list updated_s1 in
    let _, updated_pl2 = match_update_card_pile 
        myself other card_pile2 fake_card_list updated_s2 in
    updated_pl1, updated_pl2

let process_s1_cl2 s1 cl2 card_pile1 card_pile2 
    fake_stack fake_card_list myself other idx top = 
  let updated_s1, updated_cl2 = Player.transfer_stack_to_card 
      s1 cl2 in
  if (is_other card_pile1) then
    let _, updated_pl1= match_update_card_pile myself 
        other card_pile1 fake_card_list updated_s1 in
    let updated_pl2, _ = match_update_card_pile myself 
        other card_pile2 updated_cl2 fake_stack in
    updated_pl1, updated_pl2 

  else
    let updated_pl1, _ = match_update_card_pile myself 
        other card_pile1 fake_card_list updated_s1 in
    let _, updated_pl2 = match_update_card_pile myself 
        other card_pile2 updated_cl2 fake_stack in
    updated_pl1, updated_pl2 

let get_fields (state: t) (myself: Player.t) (other: Player.t) 
    (card_pile1: Dogma.card_pile) (card_pile2: Dogma.card_pile) = 
  let card_list1, stack1 = match_card_pile card_pile1 myself other in
  let card_list2, stack2 = match_card_pile card_pile2 myself other in

  let fake_stack = Player.init_stack Red and fake_card_list = [] in
  card_list1, stack1, card_list2, stack2, fake_stack, 
  fake_card_list

let match_fields myself other card_pile1 card_pile2 
    fake_stack fake_card_list card_list1 stack1 card_list2 
    stack2 idx top = 
  match card_list1, stack1, card_list2, stack2 with 
  | Some cl1, None, Some cl2, None -> 
    if (List.length cl1 == 0) then raise (Empty_list "The card list to remove from is empty.
     Please consider drawing a card, use another command, etc.
     Enter the command again.")
    else 
      (* Printf.printf "cl1 length %d\n" (List.length cl1);
         Printf.printf "cl2 length %d\n" (List.length cl2); *)
      procress_cl1_cl2 
        cl1 cl2 card_pile1 card_pile2 fake_stack myself other idx top
  | Some cl1, None, None, Some s2 -> 
    if (List.length cl1 == 0) then raise (Empty_list "The card list to remove from is empty.
    Please consider drawing a card, use another command, etc.
     Enter the command again.")
    else 
      process_cl1_s2
        cl1 s2 card_pile1 card_pile2 fake_stack fake_card_list 
        myself other idx top
  | None, Some s1, None, Some s2 ->
    if (List.length (Player.get_stack_cards s1) == 0) then raise (Empty_list "The stack to remove from is empty")
    else 
      process_s1_s2
        s1 s2 card_pile1 card_pile2 fake_stack fake_card_list 
        myself other idx top
  | None, Some s1, Some cl2, None -> 
    if List.length (Player.get_stack_cards s1) == 0 then raise (Empty_list "The stack to remove from is empty")
    else  
      process_s1_cl2 
        s1 cl2 card_pile1 card_pile2 fake_stack fake_card_list 
        myself other idx top
  | _, _, _, _ -> failwith "fail to match card list or stack"



let transfer (state: t) (myself': Player.t) (other': Player.t) 
    (card_pile1: Dogma.card_pile) (card_pile2: Dogma.card_pile) 
    (idx: int) (top: bool): t =
  (* if (match_card_pile card_pile1) then let myself = other' and  other = myself' in
     let card_list1, stack1, card_list2, stack2, fake_stack, 
        fake_card_list = get_fields state myself 
        other card_pile1 card_pile2 in
     let updated_player1, updated_player2 =
      match_fields myself other card_pile1 card_pile2 
        fake_stack fake_card_list card_list1 stack1 card_list2 stack2 idx top  
     in
     update_player (update_player state updated_player1) updated_player2
     else  *)
  let myself = myself' and other = other' in
  let card_list1, stack1, card_list2, stack2, fake_stack, 
      fake_card_list = get_fields state myself 
      other card_pile1 card_pile2 in
  let updated_player1, updated_player2 =
    match_fields myself other card_pile1 card_pile2 
      fake_stack fake_card_list card_list1 stack1 card_list2 stack2 idx top  
  in
  update_player (update_player state updated_player1) updated_player2

let score (state : t) (player : Player.t) (hand_idx : int) : t = 
  if hand_idx > (List.length (Player.get_hand player)) 
  then failwith "idx out of bound" 
  else transfer state player player (Dogma.Self_hand hand_idx) 
      (Dogma.Other_score (-1)) hand_idx false

let achieve (state: t) (player: Player.t) : t = 
  let achievement = List.hd state.achievements in
  let updated_player = Player.add_achievement player achievement in
  update_player (delete_one_achievement state) updated_player 

let print_player_board (state: t) (index: int): string = 
  try (
    let player = get_player state index in 
    Player.print_board player)
  with _ -> "wrong! That player doesn't exist."

let rec search_color board : Dogma.stack_color =
  match board with 
  | x :: t -> let lst = Player.get_stack_cards x in
    if lst = [] then search_color t
    else Player.get_stack_color x
  | _ -> failwith "impossible"

let give_color_to_dogma id state : Dogma.stack_color = 
  let player = get_player state id in
  let board = Player.get_board player in 
  search_color board 

let rec search_color_exist board : bool = 
  match board with 
  | x :: t -> let lst = Player.get_stack_cards x in
    if lst = [] then search_color_exist t
    else true
  | [] -> false

let check_color_to_dogma_exist id state : bool = 
  let player = get_player state id in
  let board = Player.get_board player in 
  search_color_exist board

let next_player (state : t) : t = 
  {
    players= state.players;
    era_cards = state.era_cards;
    achievements= state.achievements;
    current_player= (state.current_player + 1) mod 
                    (List.length state.players);
    lowest_era= state.lowest_era;
  }
