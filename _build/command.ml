(* open Dogma *)

(* type color = Dogma.stack_color *)
type color = Red|Black

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed


(** [erase_space list] erases all leading and trailing white space 
    of every element in the list and delete element 
    that are just white space*)
let rec erase_space list = 
  match list with
  | [] -> []
  | h::t -> if h = "" then erase_space t else String.trim h :: erase_space t

(** [string_list string] takes a string and remove all its leadning 
    and trailing white space and splice it into pieces when there's a 
    white space and put all the pieces into a list*)
let string_list string = 
  let trim_string = String.trim string in (
    if trim_string = "" then raise Empty else
      erase_space (String.split_on_char ' ' trim_string);
  )

type command = 
  | Meld of int
  | Draw of int
  | Achieve of int
  | Dogma of color

let match_color str = 
  match str with
  | "red"
  | "Red" 
    -> Red
  | "Black"
  | "black"
    -> Black
  | _ -> raise Empty

let parse str =
  if List.hd (string_list str) = "meld" || 
     List.hd (string_list str) = "Meld" then 
    Meld (int_of_string (List.hd (List.tl (string_list str))))
  else if List.hd (string_list str) = "draw" || 
          List.hd (string_list str) = "Draw" then 
    Draw (int_of_string (List.hd (List.tl (string_list str))))
  else if List.hd (string_list str) = "achieve" || 
          List.hd (string_list str) = "Achieve" then 
    Achieve (int_of_string (List.hd (List.tl (string_list str))))
  else if List.hd (string_list str) = "dogma" || 
          List.hd (string_list str) = "Dogma" then 
    Dogma (match_color (List.hd (List.tl (string_list str))))
  else raise Malformed

