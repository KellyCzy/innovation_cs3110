open Dogma
open Card 

type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list;
}

let get_dir stack = 
  stack.splay

let get_top_card stack = 
  try List.hd stack.cards with _ -> failwith "Do not have this color on board."

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

let map_color_to_string = function
  | Dogma.Red -> "Red"
  | Dogma.Purple -> "Purple"
  | Dogma.Blue -> "Blue"
  | Dogma.Green -> "Green"
  | Dogma.Yellow -> "Yellow"

let get_stack_color stack =
  stack.color

let get_stack_cards stack = 
  stack.cards

let compare_player player1 player2 = 
  Stdlib.compare player1.id player2.id

let compare_stack stack1 stack2 = 
  Stdlib.compare (map_color_to_int stack1.color) 
    (map_color_to_int stack2.color)

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

let update_splay_direction (stack: stack) 
    (direction: Dogma.splay_direction) = {
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

let get_top_card_name player index = 
  let stack = List.nth player.board index in
  try
    (List.nth stack.cards 0)|>Card.get_title
  with Failure _ ->  "empty stack"

let get_board_total_length player = 
  let board = player.board in
  List.fold_left (fun acc b -> acc + List.length b.cards) 0 board

let print_board player =
  try (
    String.concat "\n" 
      ["Red: " ^ get_stack_top player 0 ^ "\n";
       "Purple: " ^ get_stack_top player 1 ^ "\n";
       "Blue: " ^ get_stack_top player 2 ^ "\n";
       "Green: " ^ get_stack_top player 3 ^ "\n";
       "Yellow: " ^ get_stack_top player 4 ^ "\n"]
  ) with _ -> "wrong!"

let get_board player =
  player.board

let add_hand player card =  
  update_hand (card::player.hand) player

let get_ith_hand player i = 
  try (List.nth player.hand i) with _ -> failwith ((string_of_int i) ^ "is not a valid index")

let get_color_stack (player: t) (c: Dogma.stack_color) : stack = 
  get_ith_stack player (map_color_to_int c)

let rec count_left stack icon acc = 
  match stack with
  | [] -> acc
  | a :: [] -> 
    let icons = Card.get_icons a in
    let rec num lst ac =
      match lst with 
      | [] -> ac
      | a :: t -> if a = icon then num t (ac + 1) else num t ac in
    count_left [] icon (num icons acc)
  | x :: xs ->
    let icons = Card.get_icons x in
    if List.nth icons 3 = icon 
    then count_left xs icon acc + 1 else count_left xs icon acc

let rec count_right stack icon acc = 
  match stack with
  | [] -> acc
  | a :: [] -> 
    let icons = Card.get_icons a in
    let rec num lst ac =
      match lst with 
      | [] -> ac
      | a :: t -> if a = icon then num t (ac + 1) else num t ac in
    count_right [] icon (num icons acc)
  | x :: xs ->
    let icons = Card.get_icons x in
    let num = if List.nth icons 0 = icon && List.nth icons 1 = icon then acc + 2 
      else if List.nth icons 1 = icon || List.nth icons 0 = icon then acc + 1
      else acc in
    count_right xs icon num 

let rec count_up stack icon acc = 
  match stack with
  | [] -> acc
  | a :: [] -> 
    let icons = Card.get_icons a in
    let rec num lst ac =
      match lst with 
      | [] -> ac
      | a :: t -> if a = icon then num t (ac + 1) else num t ac in
    count_up [] icon (num icons acc)
  | x :: xs ->
    let icons = Card.get_icons x in
    let num = 
      if List.nth icons 1 = icon && 
         List.nth icons 2 = icon && List.nth icons 3 = icon then acc + 3
      else if List.nth icons 1 = icon && List.nth icons 2 = icon then acc + 2 
      else if List.nth icons 1 = icon && List.nth icons 3 = icon then acc + 2 
      else if List.nth icons 2 = icon && List.nth icons 3 = icon then acc + 2
      else if List.nth icons 1 <> icon && 
              List.nth icons 2 <> icon && List.nth icons 3 <> icon then acc
      else acc + 1 in count_up xs icon num

let rec count_no stack icon acc = 
  match stack with
  | [] -> acc
  | a :: t -> 
    let icons = Card.get_icons a in
    let rec num lst ac =
      match lst with 
      | [] -> ac
      | a :: t -> if a = icon then num t (ac + 1) else num t ac in
    num icons acc

let rec count_icon board icon acc = 
  match board with 
  | [] -> acc
  | a :: t -> 
    match (get_dir a) with 
    | Left -> count_icon t icon (count_left (List.rev a.cards) icon acc)
    | Right -> count_icon t icon (count_right (List.rev a.cards) icon acc)
    | No -> count_icon t icon (count_no (List.rev a.cards) icon acc)
    | Up -> count_icon t icon (count_up (List.rev a.cards) icon acc)

let get_icon (player: t) (icon: Card.icon) = 
  count_icon player.board icon 0

(* update the ith stack with [new_s] in the stack list**)
let update_stack_list s_lst i new_s = 
  let ith = try (List.nth s_lst i) with _ -> failwith ((string_of_int i) ^ "is not a valid index") in
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
  | x::xs -> 
    try let ith = try (List.nth lst i) with _ -> failwith ((string_of_int i) ^ " is not a valid index") in
      (List.filter (fun x -> not (Card.equal x ith)) lst), ith
    with _ -> 
      print_endline "This card list has no i^th element. 
    Popped 0^th card by default.\n"; 
      lst, List.hd lst

let pop_stack i stack = 
  let cards = stack.cards in
  if i = 0 then 
    (update_stack_cards stack (List.tl cards)), List.hd cards
  else
    match cards with
    | [] -> failwith "cannot pop element from empty stack"
    | x::xs -> try let ith =  try (List.nth cards i) with _ -> failwith ((string_of_int i) ^ " is not a valid index") in
        let updated_cards, ele = (List.filter 
                                    (fun x -> Card.equal x ith) cards), 
                                 ith in
        (update_stack_cards stack updated_cards), ith
      with _ -> print_endline "The stack doesn't have i^th element.
      Popped 0^th card by default.\n"; stack, List.hd cards

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
  let stack_to_update = push_stack card_to_add 
      (List.nth player.board card_c_idx) top in
  let updated_stack_list = update_stack_list 
      player.board card_c_idx stack_to_update in
  player |> update_hand updated_hand |> update_board updated_stack_list


(*remove the top element from a stack with color [color], return the updated stack and the card removed*)
let remove_stack player color = 
  let int_of_color = color |> map_color_to_int in
  let rest, ele = int_of_color |> get_ith_stack player |> pop_stack 0 in 
  let updated_board = rest |> update_stack_list player.board 
                        int_of_color in
  (update_board updated_board player), ele

(*transfer one card from card_list to stack, return updated card list and updated stack*)
let transfer_card_to_stack (card_list: Card.t list) 
    (stack: stack) (idx: int) (top: bool)= 
  let updated_card_list, card = pop_card idx card_list in 
  if Card.get_color card <> stack.color 
  then failwith "cannot transfer card of a different color"
  else begin 
    ANSITerminal.(print_string [green] ("\nYou just transferred a card [" ^ (Card.get_title card) ^ "]\n"));
    let updated_stack = push_stack card stack top in
    updated_card_list, updated_stack 
  end

let transfer_stack_to_card (stack: stack) (card_list: Card.t list) =
  (* Printf.printf "stack length before %d\n" (List.length (stack.cards)); *)
  let updated_stack, card = pop_stack 0 stack in
  (* Printf.printf "stack length after %d\n" (List.length (updated_stack.cards)); *)
  ANSITerminal.(print_string [green] ("\nYou just transferred a card [" ^ (Card.get_title card) ^ "]\n"));
  let updated_card_list = push_card card card_list in
  updated_stack, updated_card_list

(*transfer card at position [idx] from card_list1 to card_list2*)
let transfer_card_to_card (card_list1: Card.t list) 
    (card_list2: Card.t list) (idx:int) =
  let updated_card_list1, card = pop_card idx card_list1 in 
  ANSITerminal.(print_string [green] ("\nYou just transferred a card [" ^ (Card.get_title card) ^ "]\n"));
  let updated_card_list2 = push_card card card_list2 in
  updated_card_list1, updated_card_list2

(* transfer one card from stack1 to stack2*)
let transfer_stack_to_stack (stack1: stack) (stack2: stack) (top: bool) =
  if stack1.color <> stack2.color 
  then failwith "cannot transfer card of a different color"
  else begin 
    let updated_stack1, card = pop_stack 0 stack1 in
    ANSITerminal.(print_string [green] ("\nYou just transferred a card [" ^ (Card.get_title card) ^ "]\n"));
    let updated_stack2 = push_stack card stack2 top in
    updated_stack1, updated_stack2
  end


(* splay the player's *)
let splay (player: t) (color: Dogma.stack_color) 
    (direction: Dogma.splay_direction) : t =
  let color_idx = map_color_to_int color in 
  let stack = get_ith_stack player color_idx in
  let updated_stack = update_splay_direction stack direction in
  let updated_stack_list = update_stack_list 
      player.board color_idx updated_stack in
  player |> update_board updated_stack_list

(* get player's score cards *)
let get_score_cards player = 
  player.score

(* get the sum of player's scores*)
let get_score player =
  List.fold_left (fun acc ele -> (Card.get_value ele) 
                                 + acc + 1) 0 player.score

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

let print_player player = 
  Printf.printf "id %d\n" player.id;
  Printf.printf "hand length %d\n" (List.length (get_hand player));
  Printf.printf "score %d\n" (get_score player);



