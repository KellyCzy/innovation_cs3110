type t = {
  players: Player.t list;
  era_cards: Card.t list;
  achievements: int list;
  current_player: int;
}

let change_state 