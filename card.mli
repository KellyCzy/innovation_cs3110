open Dogma 

(** type of an icon.*)
type icon = Castle | Crown | Factory | Leaf | Clock | Lightbulb | Pattern

(** type of a color.*)
type color = Dogma.stack_color

(** type of a card. 
      A card should contains a [tile], [value],  [dogma], [icon], [color]*)
type t = {
  title : string;
  description : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon : icon;
  icons : icon list;
  color : color;
}

(** [get_card_title c] is the title of [c]*)
val get_title : t -> string

(** [get_value c] is the value/era of [c]*)
val get_value : t -> int

(** [get_dogma c] is the list of dogmas on the card [c]*)
val get_dogma : t -> Dogma.t list

(** [get_icon c] is the list of icons on the card [c]*)
val get_icons : t -> icon list

(** [get_color c] is the color of the card [c]*)
val get_color : t -> color

(** [equal c1 c2] is the ture if cards [c1] and [c2] is the same. *)
val equal : t -> t -> bool

(** [compare c1 c2] is 0 if the cards [c1] and [c2] is equal, negative if 
    the title of [c1] is less than [c2], otherwise positive.*)
val compare : t -> t -> int

(** [get_dogmas_icon c] is the icon of the dogma of [c].*)
val get_dogmas_icon : t -> icon

(** [get_description c] is the description of [c].*)
val get_description : t -> string

(** [count_icons icon_list icon acc] is number of icons [icon] 
    in icon_list plus [acc]*)
val count_icons : icon list -> icon -> int -> int

(** [icon_to_string] maps an icon to its string representation *)
val icon_to_string:  icon -> string

(** [color_to_string color] is the string corresboding to the name of [color] *)
val color_to_string : color -> string

(** [color_to_int color] is the index of [color] on the board of the player.*)
val color_to_int : color -> int

(** [card_to_string card] is the string representing [card].*)
val card_to_string : t -> string