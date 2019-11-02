open Yojson.Basic.Util
open Card
open Dogma

let json_to_dogmas (json : Yojson.Basic.t) : Dogma.t list = 
  let eff1_lst = json |> member "dogmas" |> member "effect1" |> to_list in
  let eff2_lst = json |> member "dogmas" |> member "effect2" |> to_list in 
  let matching st = 
    match st |> String.split_on_char ' ' with 
    | eff :: content :: [] -> match eff with
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
        let piles = content |> String.split ',' in
        match 
      | 


        let from_json (json : Yojson.Basic.t) : Card.t = 
          {
            title = json |> member "title" |> to_string;
            value = json |> member "value" |> to_int;

          }

