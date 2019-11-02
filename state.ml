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

let update_players (state : t) (players : Play.t) : t = {
  players = players;
  era_cards = state.era_cards;
  achievements = state.achievements;
  current_player = state.current_player;
  lowest_era = state.lowest_era;
}

let current_player (state: t) : int= 
  List.nth state.players state.current_player

let lowest_era (state: t) : int = 
  state.lowest_era 


(* *)
let draw (state: t) (player: Player.t) (era: int): t = 
  let era_draw = max state.lowest_era era in
  let card_to_draw = era_draw |> List.nth state.era_cards |> List.nth era_cards in
  let updated_player = Player.add_hand player card_to_draw in
  let updated_players = List.sort_uniq Player.compare updated_player::state.players

