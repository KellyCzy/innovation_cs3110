open Card
open Dogma

type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list;
}

type t = {
  id : int;
  hand : Card.t list;
  board : stack list;
  score : Card.t list;
  achievements : int list;  
}

let init_stack c = {
  color = c;
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
  | Red -> 0
  | Purple -> 1
  | Blue -> 2
  | Green -> 3
  | Yellow -> 4
  | _ -> failwith "color doesn't exist"


let compare_player player1 player2 = 
  compare t1.id t2.id

let compare_stack stack1 stack2 = 
  compare (match_color_to_int stack1.color) (match_color_to_int stack2.color)

let update_hand hand player = {
  id = player.id;
  hand = hand;
  board = player.id;
  score = player.score;
  achievements = player.achievements;
}

let update_board board player= {
  id = player.id;
  hand = player.hand;
  board = board;
  score = player.score;
  achievements = plyaer.achievements;
}

let get_hand player =
  player.hand

let add_hand player c = 
  c :: player.hand |> update_hand player

(** Remove the [i]th element of [lst]. *)
let remove_ith_card lst i = 
  let ith = List.nth i lst in
  List.filter (fun x -> Card.compare x ith) lst

let get_ith_stack player i = 
  List.nth i player.board
(* 
let rec help_check_color lst c =
  match lst with 
  | [] -> false
  | x :: t -> x.color = c || help_check_color t c *)

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
  | x::xs -> let ith = List.nth i lst in
    (List.filter (fun x -> not (Card.equal x ith)) lst), ith

let add_stack player hand_idx = 
  let card_to_add, updated_hand = pop_card hand_idx player.hand  in
  let card_c_idx = card_to_add |> Card.get_color |> map_color_to_int in
  let stack_to_update = card_to_add::(List.nth player.board card_c_idx) in
  let updated_stack_list = update_stack_list player.board card_c_idx stack_to_update in
  player |> update_hand updated_hand |> update_board updated_stack_list

let remove_stack player color = 
  let int_of_color = color |> map_color_to_int in
  match int_of_color |> get_ith_stack player |> pop_card 0 with 
  | (new_s, ith) -> new_s |> update_stack_list player.board int_of_color |> update_board

let get_score player = 
  player.score

let update_score player score = {
  id = player.id;
  hand = player.hand;
  board = player.board;
  score = score;
  achievements = player.achievements
}

let update_achievements player a = {
  id = player.id;
  hand = player.hand;
  board = player.board;
  score = score;
  achievements = a;
}

let add_score player n = 
  player.score + n |> update_score player 

let get_achievements player = 
  player.achievements

let add_achievement player era = 
  era::player.achievements |> update_achievements player





