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

(** [equal c1 c2] is true if the title of [c1] and [c2] is the same, 
    otherwise flase.*)
let equal card1 card2 = 
  card1.title = card2.title

(** [compare c1 c2] is 0 if the title of c1 and c2 is equal, negative if 
    the title of c1 is less than c2, otherwise positive.*)
let compare card1 card2 = 
  Stdlib.compare card1.title card2.title

(** [get_color c] is the color of [c].*)
let get_color card = card.color

(** [get_title c] is the title of [c].*)
let get_title (card : t) : string = card.title

(** [get_value c] is the value of [c].*)
let get_value card = card.value 

(** [get_dogma c] is the dogma list of [c].*)
let get_dogma card =  card.dogmas

(** [get_dogmas_icon c] is the icon of the dogma of [c].*)
let get_dogmas_icon card = card.dogmas_icon

(** [get_icons c] is the icon list on the card of [c].*)
let get_icons card = card.icons

(** [coubt_icons icon_list icon acc] is number of icons [icon] in icon_list plus [acc]*)
let rec count_icons icon_list icon acc = 
  match icon_list with
  | [] -> acc
  | x::xs -> if x = icon then count_icons xs icon (acc+1)
    else count_icons xs icon acc

(** [get_description c] is the description of [c].*)
let get_description card = card.description

(** [icon_to_string icon] is the string corresboding to the name of [icon].*)
let icon_to_string icon : string = 
  match icon with 
  | Castle -> "Castle"
  | Crown -> "Crown"
  | Factory -> "Factory"
  | Leaf -> "Leaf"
  | Clock -> "Clock"
  | Lightbulb -> "Lightbulb"
  | Pattern -> "Blank" 

(** [color_to_string color] is the string corresboding to the name of [color]*)
let color_to_string color : string =
  match color with 
  | Dogma.Red -> "Red"
  | Dogma.Purple -> "Purple"
  | Dogma.Blue -> "Blue"
  | Dogma.Green -> "Green"
  | Dogma.Yellow -> "Yellow" 

(** [color_to_int color] is the index of [color] on the board of the player.*)
let color_to_int color = 
  match color with
  | Dogma.Red -> 0
  | Dogma.Purple -> 1
  | Dogma.Blue -> 2
  | Dogma.Green -> 3
  | Dogma.Yellow -> 4

(** [card_to_string card] is the string representing [card].*)
let card_to_string card : string =  
  "\n[Card name: " ^ card.title ^ "\n" ^ 
  "Description: " ^ card.description ^ "\n" ^
  "Color: " ^ (color_to_string card.color) ^ "\n" ^
  "Era: " ^ (string_of_int (card.value + 1)) ^ "\n" ^
  "Dogma icon: " ^ (icon_to_string card.dogmas_icon) ^ "\n" ^
  "Icons: " ^ (card.icons |> List.map icon_to_string 
               |> String.concat ", ") ^ "]\n"


