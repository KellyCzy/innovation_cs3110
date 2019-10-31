open Card
open Dogma

type stack = {
  color : Dogma.stack_color;
  splay : Dogma.splay_direction;
  cards : Card.t list
}

type t = {
  hand : Card.t list;
  board : stack list;
  score : Card.t list;
  achievements : int list;  
}

