open Dogma

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed of string


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
  | Dogma of Dogma.stack_color
  | Hand
  | Board of int
  | Score
  | Help
  | Number of int

let match_color str = 
  match str with
  | "Yellow"
  | "yellow" 
    -> Dogma.Yellow
  | "red"
  | "Red"
    -> Dogma.Red
  | "purple"
  | "Purple"
    -> Dogma.Purple
  | "blue"
  | "Blue"
    -> Dogma.Blue
  | "green"
  | "Green"
    -> Dogma.Green
  | _ -> raise Empty

(* let parse str =
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
   else if List.hd (string_list str) = "hand" || 
          List.hd (string_list str) = "Hand" then 
    Hand 
   else if List.hd (string_list str) = "board" || 
          List.hd (string_list str) = "board" then 
    try
      Board (int_of_string (List.hd (List.tl (string_list str))))
    with _ -> print_endline("String parse is not integer"); Board 0
   else if List.hd (string_list str) = "score" || 
          List.hd (string_list str) = "Score" then 
    Score
   else if List.hd (string_list str) = "help" || 
          List.hd (string_list str) = "Help" then 
    Help
   else
    (match string_list str with 
     | a :: [] -> (try Number (int_of_string a) with | _ -> raise Malformed)
     | _ -> raise Malformed)  *)

let int_of_b b = 
  try int_of_string b with | _ -> raise (Malformed (b ^ " is not a number. \n"))

let color_of_b b = 
  try match_color b with | _ -> raise (Malformed (b ^ " is not a color. \n"))

let parse str =
  match string_list str with
  | a :: [] ->
    (if a = "help" || a = "Help" then Help
     else if a = "hand" || a = "Hand" then Hand
     else if a = "score" || a = "Score" then Score
     else try Number (int_of_string a) with | _ -> 
       raise (Malformed  (a ^ " is not an index!\n")))
  | a :: b :: [] ->
    if a = "meld" ||  a = "Meld" then 
      Meld (int_of_b b)
    else if a = "draw" ||  a = "Draw" then 
      Draw (int_of_b b)
    else if a = "achieve" || a = "Achieve" then 
      Achieve (int_of_b b)
    else if a = "dogma" ||  a = "Dogma" then 
      Dogma (color_of_b b)
    else if a = "board" || a = "board" then 
      Board (int_of_b b)
    else raise (Malformed "No such command! \n You can only Meld/Draw/Dogma/Achieve \n")
  | _ -> raise (Malformed "No such command! \n You can only Meld/Draw/Dogma/Achieve \n")

