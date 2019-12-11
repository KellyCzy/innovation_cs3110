open Dogma


exception Empty

exception Malformed of string


let rec erase_space list = 
  match list with
  | [] -> []
  | h::t -> if h = "" then erase_space t else String.trim h :: erase_space t


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

