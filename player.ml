open Dogma
open Card 

(** [stack] is the type representing the stack on the board. *)
type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list;
}

(** [get_dir stack] is the splay direction of [stack]. *)
let get_dir stack = 
  stack.splay

(** [get_top_card stack] is the top card on [stack]. *)
let get_top_card stack = 
  try List.hd stack.cards with _ -> failwith "Do not have this color on board."

(** [t] is the representation of a player. *)
type t = {
  id : int;
  hand : Card.t list;
  board : stack list;
  score : Card.t list;
  achievements : int list;  
}

(** [init_stack color] is the initialized stack of [color]. *)
let init_stack color = {
  color = color;
  splay = No;
  cards = [];
}

(** [init_player id] is the initialized player of [id]. *)
let init_player id = {
  id = id;
  hand = [];
  board = List.map (init_stack) [Red; Purple; Blue; Green; Yellow];
  score = [];
  achievements = [];
}

(** [map_color_to_int] is corresponding index of a color. *)
let map_color_to_int = function
  | Dogma.Red -> 0
  | Dogma.Purple -> 1
  | Dogma.Blue -> 2
  | Dogma.Green -> 3
  | Dogma.Yellow -> 4

(** [map_color_to_string] is the string representing the the color. *)
let map_color_to_string = function
  | Dogma.Red -> "Red"
  | Dogma.Purple -> "Purple"
  | Dogma.Blue -> "Blue"
  | Dogma.Green -> "Green"
  | Dogma.Yellow -> "Yellow"

(** [get_stack_color stack] is color of [stack]. *)
let get_stack_color stack =
  stack.color

(** [get_stack_cards stack] is the cards list of [stack]. *)
let get_stack_cards stack = 
  stack.cards

(** [get_stack_length stack] is the length of the card list in [stack]. *)
let get_stack_length stack = 
  List.length (stack.cards)

(** [compare_player p1 p2] is 0 when the id of [p1] and [p2] is equal, 
    negative if [p1] is less than [p2] and positive otherwise*)
let compare_player player1 player2 = 
  Stdlib.compare player1.id player2.id

(** [compare_stack s1 s2] is 0 when the color of [s1] and [s2] is equal,
    negative if [s1] is less than [s2] and positive otherwise. *)
let compare_stack stack1 stack2 = 
  Stdlib.compare (map_color_to_int stack1.color) 
    (map_color_to_int stack2.color)

(** [update_hand hand player] is a player 
    derived by updated [player] with a new [hand].*)
let update_hand hand player = {
  id = player.id;
  hand = hand;
  board = player.board;
  score = player.score;
  achievements = player.achievements;
}

(** [update_board board player] is a player 
    derived by updated [player] with a new [board].*)
let update_board board player= {
  id = player.id;
  hand = player.hand;
  board = board;
  score = player.score;
  achievements = player.achievements;
}

(** [update_splay_direction] is a stack 
    with the splay of [stack] changes to [direction]. *)
let update_splay_direction (stack: stack) 
    (direction: Dogma.splay_direction) = {
  color = stack.color;
  splay = direction;
  cards = stack.cards;
}

(** [update_stack_cards stack cards] is a stack 
    with the cards of [stack] changes to [cards]. *)
let update_stack_cards stack cards = {
  color = stack.color;
  splay = stack.splay;
  cards = cards;
}

(** [get_id player] is the id of [player]. *)
let get_id player =
  player.id

(** [get_hand player] is the hand of [player]. *)
let get_hand player =
  player.hand

(** [get_hand_length player] is the number of cards in the hand of [player]. *)
let get_hand_length player = 
  List.length (player.hand)

(** [print_hand player] is the string representation of the hand cards of [player]. *)
let print_hand player = 
  String.concat " " (List.map Card.card_to_string player.hand)

(** [get_ith_stack player i] is the stack of index [i] on the board of [player]. *)
let get_ith_stack player i = 
  List.nth player.board i

(** [get_stack_top player index] is the top card of stack 
    with the index [index] on the board of [player].
    Raise: Failure when the stack is empty. *)
let get_stack_top (player: t) (index: int): string =
  let stack = List.nth player.board index in
  try
    (List.nth stack.cards 0) |> Card.card_to_string
  with Failure _ ->  " empty stack"

(** [get_top_cards_name player index] is the name of the top card of stack
    with the index [index] on the board of [player].
    Raise: Failure when the stack is empty.*)
let get_top_card_name player index = 
  let stack = List.nth player.board index in
  try
    (List.nth stack.cards 0)|>Card.get_title
  with Failure _ ->  "empty stack"

(** [get_board_total_length player] is total number of cards 
    on the board of [player].*)
let get_board_total_length player = 
  let board = player.board in
  List.fold_left (fun acc b -> acc + List.length b.cards) 0 board

(** [print_board player] is the string representation of the board of [player]. *)
let print_board player =
  try (
    String.concat "\n" 
      ["Red: " ^ get_stack_top player 0 ^ "\n";
       "Purple: " ^ get_stack_top player 1 ^ "\n";
       "Blue: " ^ get_stack_top player 2 ^ "\n";
       "Green: " ^ get_stack_top player 3 ^ "\n";
       "Yellow: " ^ get_stack_top player 4 ^ "\n"]
  ) with _ -> "wrong!"

(** [get_board player] is the board of [player].*)
let get_board player =
  player.board

(** [add_hand player card] is the player with a
    [card] added to the hand of [player]. *)
let add_hand player card =  
  update_hand (card::player.hand) player

(** [get_ith_hand player i] is the card of index [i] in the hand of [player]. 
    Raise: Failure when i is not a valid index. *)
let get_ith_hand player i = 
  try (List.nth player.hand i) 
  with _ -> failwith ((string_of_int i) ^ "is not a valid index")

(** [get_color_stack player c] is the stack with color [c] of [player]. *)
let get_color_stack (player: t) (c: Dogma.stack_color) : stack = 
  get_ith_stack player (map_color_to_int c)

(** [count_left stack icon acc] is the number of [icon] in a left-splayed [stack], 
    the initial number is [acc]. *)
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

(** [count_right stack icon acc] is the number of [icon] in a right-splayed [stack],
    the initial number is [acc]. *)
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

(** [count_up stack icon acc] is the number of [icon] in a up-splayed [stack],
    the initial number is [acc].*)
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

(** [count_no stack icon acc] is the number of [icon] in a no-splayed [stack],
    the initial number is [acc].*)
let rec count_no cards icon acc = 
  match cards with
  | [] -> acc
  | a :: t -> 
    let icons = Card.get_icons a in
    let icons_num = Card.count_icons icons icon 0 in
    count_no t icon (icons_num + acc)
(* let rec get_icon_num lst ac =
   match lst with 
   | [] -> ac
   | a :: t -> if a = icon then get_icon_num t (ac + 1) else get_icon_num t ac in
   get_icon_num icons acc *)

(** [count_icon board icon acc] is the number of [icon] on the [board],
    the initial number is [acc].*)
let rec count_icon board icon acc = 
  match board with 
  | [] -> acc
  | a :: t -> 
    match (get_dir a) with 
    | Left -> count_icon t icon (count_left (List.rev a.cards) icon acc)
    | Right -> count_icon t icon (count_right (List.rev a.cards) icon acc)
    | No -> count_icon t icon (count_no (List.rev a.cards) icon acc)
    | Up -> count_icon t icon (count_up (List.rev a.cards) icon acc)

(** [get_icon player icon] is the number of [icon] on the board of [player]. *)
let get_icon (player: t) (icon: Card.icon) = 
  count_icon player.board icon 0

(* update the ith stack with [new_s] in the stack list**)
let update_stack_list s_lst i new_s = 
  let ith = try (List.nth s_lst i) with _ -> failwith ((string_of_int i) ^ " is not a valid index") in
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
        let updated_cards = (List.filter 
                               (fun x -> not (Card.equal x ith)) cards) in
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
  let updated_stack, card = pop_stack 0 stack in
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



