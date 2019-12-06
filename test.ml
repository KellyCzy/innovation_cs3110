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

let test_tests = 
  [
    make_init_state_test "start player" test 0;

  ]

let suite = 
  "test suite for final project"  >::: List.flatten [
    test_tests;
  ]


let _ = run_test_tt_main suite