open Dogma
open Yojson.Basic.Util
open Card

(** [Invalid_json_format s] is raise when there is a invalid json 
    where the error is reported with [s]*)
exception Invalid_json_format of string

(** [string_to_color str] is the Dogma.stack_color type represented by [str]. *)
val string_to_color : string -> Dogma.stack_color

(** [string_to_icon str] is the Card.icon type represented by [str]. *)
val string_to_icon : string -> Card.icon

(** [all_cards json era] is all the cards ranging to [era] in [json]. *)
val all_cards : Yojson.Basic.t -> int -> Card.list list
