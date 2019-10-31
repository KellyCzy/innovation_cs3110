open Dogma

type icon = Castle | Crown | Factory | Plant | Clock | Lightbulb | Pattern

type color = Dogma.stack_color

type t = {
  title : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon: icon;
  icons : icon list;
  color : color;
}


