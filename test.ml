open OUnit2
open Dogma
open Game
open State
open Player
open Dogma
open Command
open Printf
open Frontend
open Yojson.Basic.Util

let innov = Yojson.Basic.from_file "innov.json"
let test = Yojson.Basic.from_file "test.json"

let get_all_cards json = Game.all_cards json 1

let make_init_state_test 
    (name : string) 
    (test_name: Yojson.Basic.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((get_all_cards test_name)|>State.init_state|>get_current_player))

let make_player_test 
    (name : string) 
    (test_name: Yojson.Basic.t) 
    (player_num: int) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((State.get_player ((get_all_cards test_name)|>State.init_state) player_num)|> Player.get_id)
    )

let old_state_draw = (get_all_cards test)|>State.init_state
let new_state_draw = State.draw old_state_draw (old_state_draw|>current_player) 0

let make_before_draw_test 
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let str = (Player.print_hand (state|>current_player)) in 
      (* Printf.printf "%s/n" str; *)
      assert_equal expected_output str
    )


let make_after_draw_test 
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let card = (Player.get_ith_hand (state|>current_player) 0) in 
      (* Printf.printf "%s/n" str; *)
      assert_equal expected_output (card|>Card.get_title)
    )

let old_state_meld = new_state_draw 
let new_state_meld = State.meld old_state_meld (old_state_meld|>current_player) 0

(* let make_before_meld_test 
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
   name >:: (fun _ -> 
      let str = (Player.print_board (state|>current_player)) in 
      Printf.printf "%s/n" str;
      assert_equal expected_output str
    ) *)

let make_after_meld_test 
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* let card = (Player.get_ith_stack (state|>current_player) 0) in  *)
      (* Printf.printf "%s/n" str; *)
      let card_name = Player.get_top_card_name (new_state_meld|>current_player) 0 in
      assert_equal expected_output card_name
    )

let new_state_dogma = 
  let stack = Player.get_ith_stack (new_state_meld|>current_player) 0 in
  let card = Player.get_top_card stack in
  Main.execute_dogmas new_state_meld (card|>Card.get_dogma)

let make_after_dogma_test
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      (* let card = (Player.get_ith_stack (state|>current_player) 0) in  *)
      (* Printf.printf "%s/n" str; *)
      let card = Player.get_ith_hand (state|>current_player) 0 in
      assert_equal expected_output (card|>Card.get_title)
    )

let test_tests = 
  [
    make_init_state_test "start player" test 0;

    make_player_test "player1" test 1 1;

    make_before_draw_test "before draw card" old_state_draw "";

    make_after_draw_test "after draw card" new_state_draw "Archery";

    (* make_before_meld_test "before meld card" old_state_meld ""; *)

    make_after_meld_test "after meld card" old_state_meld "Archery";

    make_after_dogma_test "after dogma" new_state_dogma "Masonry";
  ]

let suite = 
  "test suite for final project"  >::: List.flatten [
    test_tests;
  ]


let _ = run_test_tt_main suite