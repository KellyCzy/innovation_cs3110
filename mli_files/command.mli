open Dogma


(** [command] is the type of command that users input. *)
type command = 
  | Meld of int
  | Draw of int
  | Achieve of int
  | Dogma of Dogma.stack_color
  | Hand
  | Board of int
  | Score
  | Help
  | Number of int


(** [erase_space list] erases all leading and trailing white space 
    of every element in the list and delete element 
    that are just white space*)
val erase_space : string list -> string list


(** [string_list string] takes a string and remove all its leadning 
    and trailing white space and splice it into pieces when there's a 
    white space and put all the pieces into a list*)
val string_list: string -> string list

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [color_of_b b] takes in a string and return the correspoding dogma color.
    It raises Malformed if the string is not a color*)
val color_of_b : string -> Dogma.stack_color

(** [int_of_b b] takes in a string and return an int of that string. 
    It raises Malformed if the string is not a number*)
val int_of_b: string -> int

(** [parse str] takes in a string and return the parsed 
    command of that input string. If the string is not a command, it returns 
    Malformed of "no such command" *)
val parse : string -> command

(** [match_color str] takes in a string and return its corresponding 
    Dogma color *)
val match_color : string -> Dogma.stack_color
