open Dogma
open Card 

type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list;
}

let get_top_card stack = 
  List.hd stack.cards

type t = {
  id : int;
  hand : Card.t list;
  board : stack list;
  score : Card.t list;
  achievements : int list;  
}

let init_stack color = {
  color = color;
  splay = No;
  cards = [];
}

let init_player id = {
  id = id;
  hand = [];
  board = List.map (init_stack) [Red; Purple; Blue; Green; Yellow];
  score = [];
  achievements = [];
}

let map_color_to_int = function
  | Dogma.Red -> 0
  | Dogma.Purple -> 1
  | Dogma.Blue -> 2
  | Dogma.Green -> 3
  | Dogma.Yellow -> 4

(* let map_int_to_color = function *)


let compare_player player1 player2 = 
  Stdlib.compare player1.id player2.id

let compare_stack stack1 stack2 = 
  Stdlib.compare (map_color_to_int stack1.color) (map_color_to_int stack2.color)

let update_hand hand player = {
  id = player.id;
  hand = hand;
  board = player.board;
  score = player.score;
  achievements = player.achievements;
}

let update_board board player= {
  id = player.id;
  hand = player.hand;
  board = board;
  score = player.score;
  achievements = player.achievements;
}

let update_splay_direction (stack: stack) (direction: Dogma.splay_direction) = {
  color = stack.color;
  splay = direction;
  cards = stack.cards;
}

let update_stack_cards stack cards = {
  color = stack.color;
  splay = stack.splay;
  cards = cards;
}

let get_id player =
  player.id

let get_hand player =
  player.hand

let print_hand player = 
  String.concat " " (List.map Card.card_to_string player.hand)

let get_ith_stack player i = 
  List.nth player.board i


let get_stack_top (player: t) (index: int): string =
  let stack = List.nth player.board index in
  try
    (List.nth stack.cards 0) |> Card.card_to_string
  with Failure _ ->  " empty stack"


let print_board player =
  String.concat "\n" 
    ["Red: " ^ get_stack_top player 0;
     "Purple: " ^ get_stack_top player 1;
     "Blue: " ^ get_stack_top player 2;
     "Green: " ^ get_stack_top player 3;
     "Yellow: " ^ get_stack_top player 4]



let get_board player =
  player.board

let add_hand player card =  
  update_hand (card::player.hand) player

let get_ith_hand player i = 
  List.nth player.hand i

let get_color_stack (player: t) (c: Dogma.stack_color) : stack = 
  get_ith_stack player (map_color_to_int c)

(* update the ith stack with [new_s] in the stack list**)
let update_stack_list s_lst i new_s = 
  let ith = List.nth s_lst i in
  let rec update' acc = function
    | [] -> failwith "ith stack not in the list"
    | x::xs -> begin match compare_stack ith x with
        | 0 -> new_s::acc @ xs
        | _ -> update' (x::acc) xs
      end in 
  update' [] s_lst |> List.sort compare_stack


let pop_card i lst = 
  match lst with
  | [] -> failwith "cannot pop element from empty list"
  | x::xs -> let ith = List.nth lst i  in
    (List.filter (fun x -> not (Card.equal x ith)) lst), ith

let pop_stack i stack = 
  let cards = stack.cards in
  match cards with
  | [] -> failwith "cannot pop element from empty stack"
  | x::xs -> let ith = List.nth cards i  in
    let updated_cards, ele = (List.filter (fun x -> Card.equal x ith) cards), ith in
    (update_stack_cards stack updated_cards), ith

(** Remove the [i]th hand card. *)
let remove_hand player i = 
  let card_list, _ = pop_card i player.hand in
  update_hand card_list player

let push_stack (card: Card.t) (stack: stack) (top: bool): stack = {
  color = stack.color;
  splay = stack.splay;
  cards = if top then card::stack.cards else stack.cards@[card]
}

let push_card (card: Card.t) (card_list: Card.t list) : Card.t list =
  card::card_list

(* add a card to top of bottom of a stack of corresponding color *)
let add_stack (player: t) (hand_idx: int) (top: bool): t = 
  let updated_hand, card_to_add = pop_card hand_idx player.hand  in
  let color = card_to_add |> Card.get_color in
  let card_c_idx = color |> map_color_to_int in
  let stack_to_update = push_stack card_to_add (List.nth player.board card_c_idx) top in
  let updated_stack_list = update_stack_list player.board card_c_idx stack_to_update in
  player |> update_hand updated_hand |> update_board updated_stack_list


(*remove the top element from a stack with color [color], return the updated stack and the card removed*)
let remove_stack player color = 
  let int_of_color = color |> map_color_to_int in
  let rest, ele = int_of_color |> get_ith_stack player |> pop_stack 0 in 
  let updated_board = rest |> update_stack_list player.board int_of_color in
  (update_board updated_board player), ele

(*transfer one card from card_list to stack, return updated card list and updated stack*)
let transfer_card_to_stack (card_list: Card.t list) (stack: stack) (idx: int) (top: bool)= 
  let updated_card_list, card = pop_card idx card_list in 
  if Card.get_color card <> stack.color then failwith "cannot transfer card of a different color"
  else begin 
    let updated_stack = push_stack card stack top in
    updated_card_list, updated_stack 
  end

let transfer_stack_to_card (stack: stack) (card_list: Card.t list) =
  let updated_stack, card = pop_stack 0 stack in
  let updated_card_list = push_card card card_list in
  updated_stack, updated_card_list

(*transfer card at position [idx] from card_list1 to card_list2*)
let transfer_card_to_card (card_list1: Card.t list) (card_list2: Card.t list) (idx:int) =
  let updated_card_list1, card = pop_card idx card_list1 in 
  let updated_card_list2 = push_card card card_list2 in
  updated_card_list1, updated_card_list2

(* transfer one card from stack1 to stack2*)
let transfer_stack_to_stack (stack1: stack) (stack2: stack) (top: bool) =
  if stack1.color <> stack2.color then failwith "cannot transfer card of a different color"
  else begin 
    let updated_stack1, card = pop_stack 0 stack1 in
    let updated_stack2 = push_stack card stack2 top in
    updated_stack1, updated_stack2
  end


(* splay the player's *)
let splay (player: t) (color: Dogma.stack_color) (direction: Dogma.splay_direction) : t =
  let color_idx = map_color_to_int color in 
  let stack = get_ith_stack player color_idx in
  let updated_stack = update_splay_direction stack direction in
  let updated_stack_list = update_stack_list player.board color_idx updated_stack in
  player |> update_board updated_stack_list

(* get player's score cards *)
let get_score_cards player = 
  player.score

(* get the sum of player's scores*)
let get_score player =
  List.fold_left (fun acc ele -> Card.get_value ele) 0 player.score

(** get the value of idx th card in player's hand *)
let get_value (player:t) (idx:int) : int= 
  let card = List.nth player.hand idx in
  card.value

let update_score player score = {
  id = player.id;
  hand = player.hand;
  board = player.board;
  score = score;
  achievements = player.achievements
}

let update_achievements (player: t) (a: int list) = {
  id = player.id;
  hand = player.hand;
  board = player.board;
  score = player.score;
  achievements = a;
}

let add_score player score_card = 
  score_card::player.score |> update_score player 

let get_achievements player = 
  player.achievements


let add_achievement (player: t) (era: int) = 
  if (get_score player < era*5) then failwith "not enough score to achieve"
  else era::player.achievements |> update_achievements player


