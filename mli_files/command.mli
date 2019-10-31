

open Dogma
type object_phrase = string list

type color = Dogma.stack_color


type command = 
  | Meld of int
  | Draw of int
  | Achieve of int
  | Dogma of color


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

val parse : string -> command


