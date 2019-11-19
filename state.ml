open Player
open Card
open Command
open Printf

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
  lowest_era = 1;
}

let swap_player (new_player: Player.t) (player_list: Player.t list) : Player.t list= 
  let index = Player.get_id new_player in
  let rec swap acc = function
    | [] -> failwith "player_list is empty"
    | x::xs -> if (Player.get_id x) = index then (acc @ [new_player] @ xs)
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

let current_player (state: t) : Player.t= 
  (* let player_indices = List.map Player.get_id state.players in *)
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

(* *)
let draw (state: t) (player: Player.t) (era: int): t = 
  let era_num = (max state.lowest_era era) - 1 in
  let era_to_remove = List.nth state.era_cards era_num in
  let updated_era, card_to_draw = Player.pop_card 0 era_to_remove in
  let updated_player = Player.add_hand player card_to_draw in
  let state_with_updated_player = update_player state updated_player in
  let updated_eras = update_era_list state.era_cards era_num updated_era in
  state_with_updated_player |> update_era_cards updated_eras

let meld (state: t) (player: Player.t) (hand_idx: int): t = 
  let updated_player = Player.add_stack player (hand_idx - 1) true in
  update_player state updated_player

let tuck (state: t) (player: Player.t) (hand_idx:int):t = 
  let updated_player = Player.add_stack player hand_idx false in
  update_player state updated_player

let splay (state: t) (player: Player.t) (color: Dogma.stack_color) (direction: Dogma.splay_direction) = 
  let updated_player = Player.splay player color direction in
  update_player state updated_player

let update_era state player card: Card.t list list =
  let era = Card.get_value card in
  let era_cards = List.nth state.era_cards era in
  update_era_list state.era_cards era (era_cards@[card])

let return (state: t) (player: Player.t) (hand_idx: int): t = 
  let updated_hand_cards, card = Player.pop_card hand_idx (Player.get_hand player) in
  update_era_cards (update_era state player card) state


let match_card_pile (card_pile: Dogma.card_pile) (myself: Player.t) (other: Player.t) = 
  match card_pile with 
  | Self_hand _ -> Some (Player.get_hand myself), None
  | Other_hand _ -> Some (Player.get_hand other), None
  | Self_score _ -> Some (Player.get_score_cards myself), None
  | Other_score _ -> Some (Player.get_score_cards other), None
  | Self_stack c ->  None, Some (Player.get_color_stack myself c)
  | Other_stack c -> None, Some (Player.get_color_stack other c)

let match_update_card_pile (myself: Player.t) (other: Player.t) (card_pile: Dogma.card_pile) (updated_card_list: Card.t list) (updated_stack: stack) : Player.t * Player.t=
  match card_pile with 
  | Self_hand _ -> Player.update_hand updated_card_list myself, other
  | Other_hand _ -> myself, Player.update_hand updated_card_list other
  | Self_score _ -> Player.update_score myself updated_card_list, other
  | Other_score _ -> myself, Player.update_score other updated_card_list
  | Self_stack c -> update_board (Player.update_stack_list (Player.get_board myself) (Player.map_color_to_int c) updated_stack) myself, other
  | Other_stack c -> myself, update_board (Player.update_stack_list (Player.get_board other) (Player.map_color_to_int c) updated_stack) other

(*pile1 lose one card, pile2 get one card 
*)
let transfer (state: t) (myself: Player.t) (other: Player.t) (card_pile1: Dogma.card_pile) (card_pile2: Dogma.card_pile) (idx: int) (top: bool): t =
  let card_list1, stack1 = match_card_pile card_pile1 myself other in
  let card_list2, stack2 = match_card_pile card_pile2 myself other in
  let fake_stack = Player.init_stack Red and fake_card_list = [] in
  let updated_player1, updated_player2 =
    match card_list1, stack1, card_list2, stack2 with 
    | Some cl1, None, Some cl2, None -> 
      let updated_cl1, updated_cl2 = Player.transfer_card_to_card cl1 cl2 idx in 
      let updated_pl1, _ = match_update_card_pile myself other card_pile1 updated_cl1 fake_stack in
      let _, updated_pl2 = match_update_card_pile myself other card_pile2 updated_cl2 fake_stack in
      updated_pl1, updated_pl2
    | Some cl1, None, None, Some s2 -> 
      let updated_cl1, updated_s2 = Player.transfer_card_to_stack cl1 s2 idx top in
      let updated_pl1, _ = match_update_card_pile myself other card_pile1 updated_cl1 fake_stack in
      let _, updated_pl2 = match_update_card_pile myself other card_pile2 fake_card_list updated_s2 in
      updated_pl1, updated_pl2
    | None, Some s1, None, Some s2 ->
      let updated_s1, updated_s2 = Player.transfer_stack_to_stack s1 s2 top in
      let updated_pl1, _ = match_update_card_pile myself other card_pile1 fake_card_list updated_s1 in
      let _, updated_pl2 = match_update_card_pile myself other card_pile2 fake_card_list updated_s2 in
      updated_pl1, updated_pl2
    | None, Some s1, Some cl2, None -> 
      let updated_s1, updated_cl2 = Player.transfer_stack_to_card s1 cl2 in
      let updated_pl1, _ = match_update_card_pile myself other card_pile1 fake_card_list updated_s1 in
      let _, updated_pl2 = match_update_card_pile myself other card_pile2 updated_cl2 fake_stack in
      updated_pl1, updated_pl2 
    | _, _, _, _ -> failwith "fail to match card list or stack" in
  update_player (update_player state updated_player1) updated_player2

let score (state : t) (player : Player.t) (hand_idx : int) : t = 
  transfer state player player (Dogma.Self_hand hand_idx) (Dogma.Self_score (-1)) hand_idx false

let achieve (state: t) (player: Player.t) : t = 
  let achievement = List.hd state.achievements in
  let updated_player = Player.add_achievement player achievement in
  update_player (delete_one_achievement state) updated_player 

let print_player_board (state: t) (index: int): string = 
  let player = List.nth state.players index in
  Player.print_board player

let next_player (state : t) : t = 
  {
    players= state.players;
    era_cards = state.era_cards;
    achievements= state.achievements;
    current_player= (state.current_player + 1) mod (List.length state.players);
    lowest_era= state.lowest_era;
  }
