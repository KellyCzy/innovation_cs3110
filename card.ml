open Dogma 

(** [icon] is the type of icons on the cards.*) 
type icon = Castle | Crown | Factory | Leaf | Clock | Lightbulb | Pattern

(** [color] is the type of color of the cards.*)
type color = Dogma.stack_color

(** [t] is the type of cards.*)
type t = {
  title : string;
  description : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon : icon;
  icons : icon list;
  color : color;
}

let equal card1 card2 = 
  card1.title = card2.title

let compare card1 card2 = 
  Stdlib.compare card1.title card2.title

let get_color card = card.color

let get_title (card : t) : string = card.title

let get_value card = card.value 

let get_dogma card =  card.dogmas

let get_dogmas_icon card = card.dogmas_icon

let get_icons card = card.icons

let rec count_icons icon_list icon acc = 
  match icon_list with
  | [] -> acc
  | x::xs -> if x = icon then count_icons xs icon (acc+1)
    else count_icons xs icon acc

let get_description card = card.description

let icon_to_string icon : string = 
  match icon with 
  | Castle -> "Castle"
  | Crown -> "Crown"
  | Factory -> "Factory"
  | Leaf -> "Leaf"
  | Clock -> "Clock"
  | Lightbulb -> "Lightbulb"
  | Pattern -> "Blank" 

let color_to_string color : string =
  match color with 
  | Dogma.Red -> "Red"
  | Dogma.Purple -> "Purple"
  | Dogma.Blue -> "Blue"
  | Dogma.Green -> "Green"
  | Dogma.Yellow -> "Yellow" 

let color_to_int color = 
  match color with
  | Dogma.Red -> 0
  | Dogma.Purple -> 1
  | Dogma.Blue -> 2
  | Dogma.Green -> 3
  | Dogma.Yellow -> 4

let card_to_string card : string =  
  "\n[Card name: " ^ card.title ^ "\n" ^ 
  "Description: " ^ card.description ^ "\n" ^
  "Color: " ^ (color_to_string card.color) ^ "\n" ^
  "Era: " ^ (string_of_int (card.value + 1)) ^ "\n" ^
  "Dogma icon: " ^ (icon_to_string card.dogmas_icon) ^ "\n" ^
  "Icons: " ^ (card.icons |> List.map icon_to_string 
               |> String.concat ", ") ^ "]\n"


