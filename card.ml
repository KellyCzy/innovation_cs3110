open Dogma

type icon = Castle | Crown | Factory | Leaf | Clock | Lightbulb | Pattern

type color = Dogma.stack_color

type t = {
  title : string;
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


let get_color card = 
  card.color

let get_title (card : t) : string = card.title

let get_value card = card.value

let get_dogma card = card.dogmas

let get_dogmas_icon card = card.dogmas_icon

let get_icons card = card.icons

let get_color card = card.color

