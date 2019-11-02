open Card
open Dogma

type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list
}

type t = {
  id : int
  hand : Card.t list;
  board : stack list;
  score : Card.t list;
  achievements : int list;  
}

let compare t1 t2 = 
  match t1.id - t2.id with
   | x when x > 0 -> 1
   | 0 -> 0
   | _ -> -1

let get_hand t =
  t.hand

let add_hand t c = 
  {id = t.id; hand = c :: t.hand; board = t.board; score = t.score; 
  achievements = t.achievements}

(** Remove the [i]th element of [lst]. *)
let help_remove lst i = 
  List.filter (fun x -> x != List.nth i lst) lst

let remove_hand t i = 
  {id = t.id; hand = help_remove t.hand i; board = t.board; score = t.score; 
  achievements = t.achievements}

let get_stack t i = 
  List.nth i t.board

let rec help_check_color lst c =
  match lst with 
   | [] -> false
   | x :: t -> x.color = c || help_check_color t c

let add_stack t i_h = 
  let card = List.nth i_h t.hand in
  {id = hand = t.hand; board = if help_check_color t.board card.color
  then -1
  else let new_stack = {color = card.color; splay = None; cards = card} in 
  new_stack t.board
  score = t.score; achievements = t.achievements}

