open Yojson.Basic.Util
open Card
open Dogma

let string_to_color str : Dogma.stack_color =
  match str with 
  | "Red" -> Dogma.Red
  | "Purple" -> Dogma.Purple
  | "Blue" -> Dogma.Blue
  | "Green" -> Dogma.Green
  | "Yellow" -> Dogma.Yellow

let string_to_icon str : Card.icon =
  match str with 
  | "Castle" -> Card.Castle
  | "Crown" -> Card.Crown
  | "Factory" -> Card.Factory
  | "Leaf" -> Card.Leaf
  | "Clock" -> Card.Clock
  | "Lightbulb" -> Card.Lightbulb
  | "Pattern" -> Card.Pattern

(** JSON format:
    Draw : ["Draw i"] [i] is the era of the card
    Meld : ["Meld i"] [i] is the index of the card
    Tuck : ["Tuck i"] [i] is the index of the card
    Return : ["Return i"] [i] is the index of the card
    Score : ["Score i"] [i] is the index of the card
    Splay : ["Splay dir"] [dir] is the splay direction can be ["Up"], ["Right"], ["Left"]
    Transfer : ["Transfer pile:i/c,pile:i/c"]
             ["pile:i/c"] is the card pile can be:
              ["Self_hand:i"], ["Other_hand:i"], ["Self_stack:c"], ["Other_stack:c"], ["Self_score:i"], ["Other_score:i"]
             [i] is the index of the card or player
             [c] is the color of the stack, can be ["Red"], ["Purple"], ["Blue"], ["Green"], ["Yellow"]
    Demand : ["Demand dogma:content;dogma:content...."]
           [dogma:content] is a single effect 
           and effects are connected with ";"
*)
let json_to_dogmas (json : Yojson.Basic.t) : Dogma.t list = 
  let eff1_lst = json |> member "effect1" |> to_list |> List.map to_string in
  let eff2_lst = json |> member "effect2" |> to_list |> List.map to_string in 
  let rec matching st = 
    (match st |> String.split_on_char ' ' with 
     | eff :: content :: [] -> 
       match eff with
       | "Draw" -> Dogma.Draw (int_of_string content)
       | "Meld" -> Dogma.Meld (int_of_string content)
       | "Tuck" -> Dogma.Tuck (int_of_string content)
       | "Return" -> Dogma.Return (int_of_string content)
       | "Score" -> Dogma.Score (int_of_string content)
       | "Splay" -> 
         let dir = match content with
           | "Up" -> Dogma.Up
           | "Left" -> Dogma.Left
           | "Right" -> Dogma.Right in Dogma.Splay dir
       | "Tranfer" ->  
         (let piles = content |> String.split_on_char ',' in
          let helper2 str =  match str |> String.split_on_char ':' with
            | pile :: x :: [] -> match pile with 
              | "Self_hand" -> Dogma.Self_hand (int_of_string x)
              | "Other_hand" -> Dogma.Other_hand (int_of_string x)
              | "Self_stack" -> Dogma.Self_stack (string_to_color x)
              | "Other_stack" -> Dogma.Other_stack (string_to_color x) 
              | "Self_score" -> Dogma.Self_score (int_of_string x)
              | "Other_score" -> Dogma.Other_score (int_of_string x)in
          match piles with 
          | a :: b :: [] -> Dogma.Transfer (helper2 a, helper2 a))
       | "Demand" -> 
         let efs = content |> String.split_on_char ';' in
         let ef e = e |> String.split_on_char ':' |> String.concat " " in
         Dogma.Demand (efs |> List.map ef |> List.map matching))in
  (eff1_lst |> List.map matching) :: (eff2_lst |> List.map matching) :: []

let single_card (json : Yojson.Basic.t) : Card.t = 
  {
    title = json |> member "title" |> to_string;
    value = json |> member "value" |> to_int;
    dogmas = json |> member "dogmas" |> json_to_dogmas;
    dogmas_icon = json |> member "dogmas_icon" |> to_string |> string_to_icon;
    icons = json |> member "icons" |> to_list |> List.map to_string |>List.map string_to_icon;
    color = json |> member "color" |> to_string |> string_to_color
  }

let era_cards (json : Yojson.Basic.t) (era : string) : Card.t list = 
  json |> member era |> to_list |> List.map single_card

let rec all_cards (json : Yojson.Basic.t) (eras : int) : Card.t list list = 
  match eras with 
  | 0 -> []
  | a -> (era_cards json ("Era" ^ string_of_int a)) :: (all_cards json (a - 1))


