open Dogma 

type icon = Castle | Crown | Factory | Leaf | Clock | Lightbulb | Pattern

type color = Dogma.stack_color

type t = {
  title : string;
  description : string;
  value : int;
  dogmas : Dogma.t list;
  dogmas_icon : icon;
  icons : icon list;
  color : color;
}
let equal card1 card2 = 
  (* print_endline card1.title;
     print_endline card2.title;
     Printf.printf "is_equal %B\n" (card1.title = card2.title); *)
  card1.title = card2.title

let compare card1 card2 = 
  Stdlib.compare card1.title card2.title


let get_color card = 
  card.color

let get_title (card : t) : string = card.title

let get_value card = card.value 

let get_dogma card =  card.dogmas

let get_dogmas_icon card = card.dogmas_icon

let get_icons card = card.icons

let get_color card = card.color

let get_description card = card.description

let icon_to_string icon : string = 
  match icon with 
  | Castle -> "Castle"
  | Crown -> "Crown"
  | Factory -> "Factory"
  | Leaf -> "Leaf"
  | Clock -> "Clock"
  | Lightbulb -> "Lightbulb"
  | Pattern -> "Blank" 

let color_to_string color : string =
  match color with 
  | Dogma.Red -> "Red"
  | Dogma.Purple -> "Purple"
  | Dogma.Blue -> "Blue"
  | Dogma.Green -> "Green"
  | Dogma.Yellow -> "Yellow" 

(* let dogma_to_string = function
   | Draw _ -> 
    "Draw: You can use this dogma to draw cards. It counts as one move\n"
   | Meld _ -> 
    "Meld: You can use this dogma to meld cards. It counts as one move\n"
   | Tuck _ -> "Tuck: You can use this dogma to tuck cards. 
   Tuck means placing a card to the bottom of a color pile\n" 
   | Splay _ -> 
    "Splay: You can use this dogma to splay cards of one color pile. 
   Choose a direction to splay so that you have more icons on the board\n"
   | Return _ -> "Return: You can use this dogma to return cards. 
   You can only return cards from your hand. 
   Cards will be returned to era cards pile"
   | Score _ -> "Score: You can use this dogma to score cards. 
   You can only score cards from your hand. 
   If you score an era 2 card, you get two points. 
   If you score an era 1 card, you get one point. etc. \n"
   | Transfer _ -> "Transfer: You can use this dogma to transfer cards.
   Cards can be transferred either from board to board, from hand to hand or from board to hand."
   | Demand _ -> "" *)

(* let rec dogmas_to_string dogmas : string = 
   match dogmas with 
   | [] -> ""
   | h::t -> dogma_to_string h ^ "; " ^ (dogmas_to_string t)

   let rec dogmas_to_list dogmas =
   match dogmas with
   | [] -> dogmas_to_string []
   | h::t -> dogmas_to_string h ^ dogmas_to_list t  *)

let card_to_string card : string =  
  "\n[Card name: " ^ card.title ^ "\n" ^ 
  "Description: " ^ card.description ^ "\n" ^
  "Color: " ^ (color_to_string card.color) ^ "\n" ^
  "Era: " ^ (string_of_int (card.value + 1)) ^ "\n" ^
  "Dogma icon: " ^ (icon_to_string card.dogmas_icon) ^ "\n" ^
  (* "Dogma effect: " ^ (dogmas_to_list card.dogmas) ^ "\n" ^ *)
  "Icons: " ^ (card.icons |> List.map icon_to_string 
               |> String.concat ", ") ^ "]\n"


