open Yojson.Basic.Util
open Card
open Dogma

exception Invalid_json_format of string

let string_to_color str : Dogma.stack_color =
  match str with 
  | "Red" -> Dogma.Red
  | "Purple" -> Dogma.Purple
  | "Blue" -> Dogma.Blue
  | "Green" -> Dogma.Green
  | "Yellow" -> Dogma.Yellow
  | _ -> raise (Invalid_json_format str)

let string_to_icon str : Card.icon =
  match str with 
  | "Castle" -> Card.Castle
  | "Crown" -> Card.Crown
  | "Factory" -> Card.Factory
  | "Leaf" -> Card.Leaf
  | "Clock" -> Card.Clock
  | "Lightbulb" -> Card.Lightbulb
  | "Pattern" -> Card.Pattern
  | _ -> raise (Invalid_json_format str)

(** [eff1_lst json] is the list of effect 1 of [json]. *)
let eff1_lst json = 
  json |> member "effect1" |> to_list |> List.map to_string 

(** [eff2_lst json] is the list of effect 2 of [json]. *)
let eff2_lst json = 
  json |> member "effect2" |> to_list |> List.map to_string 

(** [transfer content] is the dogma effect represented by "Transfer [content]". *)
let transfer content = 
  (let piles = content |> String.split_on_char ',' in
   let helper2 str =  match str |> String.split_on_char '=' with
     | pile :: x :: [] -> (match pile with 
         | "Self_hand" -> Dogma.Self_hand (int_of_string x)
         | "Other_hand" -> Dogma.Other_hand (int_of_string x)
         | "Self_stack" -> Dogma.Self_stack (string_to_color x)
         | "Other_stack" -> Dogma.Other_stack (string_to_color x) 
         | "Self_score" -> Dogma.Self_score (int_of_string x)
         | "Other_score" -> Dogma.Other_score (int_of_string x)
         | _ -> raise (Invalid_json_format pile)) 
     | _ -> raise (Invalid_json_format str) in
   match piles with 
   | a :: b :: [] -> Dogma.Transfer (helper2 a, helper2 b, -1)
   | _ -> raise (Invalid_json_format content) )

(** [splay content] is the dogma effect represented by "Splay [content]". *)
let splay content = 
  let pair = content |> String.split_on_char ',' in 
  let dir = match pair with
    | _ :: "Up"::[] -> Dogma.Up
    | _ ::"Left"::[] -> Dogma.Left
    | _ ::"Right"::[] -> Dogma.Right 
    | _ -> raise (Invalid_json_format content) in 
  let color = match pair with 
    | a::b::[] -> string_to_color a 
    | _ -> raise (Invalid_json_format content) in
  Dogma.Splay (dir,color)

(** [matching st] is the dogma effect represented by [st]. *)
let rec matching st = 
  (match st |> String.split_on_char ' ' with 
   | eff :: content :: [] -> 
     (match eff with
      | "Draw" -> Dogma.Draw (int_of_string content)
      | "Meld" -> Dogma.Meld (int_of_string content)
      | "Tuck" -> Dogma.Tuck (int_of_string content)
      | "Return" -> Dogma.Return (int_of_string content)
      | "Score" -> Dogma.Score (int_of_string content)
      | "Splay" -> splay content
      | "Transfer" ->  transfer content
      | _ -> raise (Invalid_json_format eff))
   | _ -> raise (Invalid_json_format st)) 

(** [json_to_dogmas json] is the 2 effect list list represented by [json]. *)
let json_to_dogmas (json : Yojson.Basic.t) : Dogma.t list = 
  ((eff1_lst json) |> List.map matching) :: 
  ((eff2_lst json) |> List.map matching) :: []


(** [single_card json] is the card represented by [json]. *)
let single_card (json : Yojson.Basic.t) : Card.t = 
  {
    title = json |> member "title" |> to_string;
    description = json |> member "description" |> to_string;
    value = json |> member "value" |> to_int;
    dogmas = json |> member "dogmas" |> json_to_dogmas;
    dogmas_icon = json |> member "dogmas_icon" |> to_string |> string_to_icon;
    icons = json |> member "icons" |> to_list 
            |> List.map to_string |> List.map string_to_icon;
    color = json |> member "color" |> to_string |> string_to_color
  }

(** [era_cards json era] is the cards from [era] in [json]. *)
let era_cards (json : Yojson.Basic.t) (era : string) : Card.t list = 
  json |> member era |> to_list |> List.map single_card

(** [shuffle clist] is the [clist] after shuffle. *)
let shuffle clist = 
  clist
(* QCheck.Gen.(generate1 (shuffle_l clist)) *)

let rec all_cards (json : Yojson.Basic.t) (eras : int) : 
  Card.t list list = 
  match eras with 
  | 0 -> []
  | a -> (shuffle (era_cards json ("Era" ^ string_of_int a))) 
         :: (all_cards json (a - 1))





