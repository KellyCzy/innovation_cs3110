open Player
open Card
open Command

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

let init_state (cards_list : Card.t list list) : t = {
  players = let rec init_some_players acc= function
      | 0 -> acc
      | i -> init_some_players (Player.init_player i)::acc i-1
    in init_some_players [] 4;
    era_cards = cards_list;
    achievements = rnd_sort_list 5;
    current_player = 0;
    lowest_era = 1;
}

let update_players (state : t) (player : Player.t) : t = {
  players = List.sort Player.compare (player::state.players);
  era_cards = state.era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let update_era_cards state era_cards = {
  players = state.player;
  era_cards = era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let current_player (state: t) : int= 
  List.nth state.players state.current_player

let lowest_era (state: t) : int = 
  state.lowest_era 

let update_era_list e_lst i new_e = 
  let rec update' acc idx = function
    | [] -> failwith "ith stack not in the list"
    | x::xs when idx = i -> acc @ [new_e] @ xs
    | x::xs -> update' (x::acc) (idx+1) xs 
  in update' [] 0 e_lst

(* *)
let draw (state: t) (player: Player.t) (era: int): t = 
  let era_num = max state.lowest_era era in
  let era_to_remove = List.nth era_num state.era_cards in
  let card_to_draw, updated_era = Player.pop_card 0 era_to_remove in
  let updated_player = Player.add_hand player card_to_draw in
  let updated_players = List.sort_uniq Player.compare updated_player::state.players in
  let updated_eras = update_era_list player.era_cards era_num updated_era in
  state |> update_players updated_players |> update_era_cards updated_eras

let meld (state: t) (player: Player.t) (hand_idx: int): t = 
  let updated_player = Player.add_stack player hand_idx true in
  update_players state updated_player

let tuck (state: t) (player: Player.t) (hand_idx:int):t = 
  let updated_player = Player.add_stack player hand_idx false in
  update_players state updated_player

let splay




let update_hand player hand_idx = 
  let new_hand = remove_ith_card player.hand hand_idx in
  Player.update_hand new_hand player

let update_era_cards state player hand_idx =
  let era = Player.get_value player hand_idx in
  let era_cards = List.nth state 



let return (state:t) (player:Player.t) (hand_idx:int):t = 
  let update_hand player hand_idx in


