open Dogma

type icon = Castle | Crown | Factory | Plant | Clock | Lightbulb | Pattern

type color = Dogma.stack_color

type t = {
  title : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon : icon;
  icons : icon list;
  color : color;
}

let title (card : t) : string = card.title

let value card = card.value

let dogma card = card.dogmas

let dogmas_icon card = card.dogmas_icon

let icons card = card.icon

let color card = card.color

