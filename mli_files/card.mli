
(** type of a card. 
      A card should contains a [tile], [value],  [dogma], [icon], [color]*)
type t 

(** type of an icon.*)
type icon

(** type of a color.*)
type color 

(** [get_card_title c] is the title of [c]*)
val title : t -> string

(** [get_value c] is the value/era of [c]*)
val value : t -> int

(** [get_dogma c] is the list of dogmas on the card [c]*)
val dogma_list : t -> Dogma.t list

(** [get_icon c] is the list of icons on the card [c]*)
val icon : t -> icon list

(** [get_colorc c] is the color of the card [c]*)
val color : t -> color

