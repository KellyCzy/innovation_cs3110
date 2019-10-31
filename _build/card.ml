open Dogma

type icon = Castle | Crown | Factory | Plant | Clock | Lightbulb | Pattern

type color = Red | Purple | Blue | Green | Yellow

type t = {
  title : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon: icon;
  icons : icon list;
  color : color
}


