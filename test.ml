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
let test1 = Yojson.Basic.from_file "test1.json"

let get_all_cards json = 
  Game.all_cards json 1
let init_state json = 
  State.init_state (get_all_cards json)

let player0 json = 
  State.get_player (init_state json) 0
let player1 json = 
  State.get_player (init_state json) 1
let player2 json = 
  State.get_player (init_state json) 2
let player3 json = 
  State.get_player (init_state json) 3

let make_init_state_test 
    (name : string) 
    (test_name: Yojson.Basic.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        ((get_all_cards test_name)|>State.init_state|>get_current_player))

let make_player_test 
    (name : string) 
    (test_name: Yojson.Basic.t) 
    (player_num: int) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        ((State.get_player 
            ((get_all_cards test_name)|>State.init_state) player_num)
         |> Player.get_id)
    )


let old_state_draw = (get_all_cards test)|>State.init_state

let new_state_draw = 
  State.draw old_state_draw 
    (old_state_draw|>current_player) 0


let old_state_draw_innov = (get_all_cards innov) |> State.init_state
let first_card = Card.get_title (State.get_era_cards_top old_state_draw_innov)
let new_state_draw_innov = State.draw old_state_draw_innov 
    (old_state_draw_innov|> current_player) 0

(* let old_state_draw_test1 = (get_all_cards test1) |> State.init_state
   let new_state_draw = 
   State.draw old_state_draw_test1 
    (old_state_draw_test1|>current_player) 0 *)

let new_state_draw_two = 
  (* print_int(let x = State.draw new_state_draw 
                (new_state_draw|>current_player) 0 in get_hand_size_by_id x 0); *)
  State.draw new_state_draw 
    (new_state_draw|>current_player) 0

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
      (* print_endline "------------------------------------------------------\n"; *)
      (* print_endline (Card.get_title card); *)
      (* Printf.printf "%s/n" str; *)
      assert_equal expected_output (card|>Card.get_title)
    )

let old_state_meld = 
  new_state_draw 

let old_state_meld_innov = 
  new_state_draw_innov

let new_state_meld = State.meld old_state_meld 
    (old_state_meld|>current_player) 0

let new_state_meld_innov = State.meld old_state_meld_innov 
    (old_state_meld_innov|>current_player) 0

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
      let card_name = Player.get_top_card_name 
          (new_state_meld|>State.current_player) 0 in
      assert_equal expected_output card_name
    )


let make_transfer_test
    (name : string)
    (state : State.t)
    (myself : Player.t)
    (other : Player.t)
    (card_pile1: Dogma.card_pile) 
    (card_pile2: Dogma.card_pile) 
    (idx: int) 
    (top: bool)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in
      assert_equal expected_output (state_after_transfer)
    )

let make_transfer_test_hh
    (name : string)
    (state : State.t)
    (myself_id : int)
    (other_id : int)
    (card_pile1: Dogma.card_pile) 
    (card_pile2: Dogma.card_pile) 
    (idx: int) 
    (top: bool)
    (expected_output : (int * int)) : test = 
  name >:: (fun _ -> 
      let myself = State.get_player state myself_id in
      let other = State.get_player state other_id in
      (* 
      Player.print_player myself; *)

      (* Printf.printf "hh before %d %d\n" myself_hand_length other_hand_length; *)
      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in

      let myself_updated = State.get_player state_after_transfer 
          myself_id in
      let other_updated = State.get_player state_after_transfer 
          other_id in

      let myself_updated_hand_length = List.length 
          (Player.get_hand myself_updated) in
      let other_updated_hand_length = List.length 
          (Player.get_hand other_updated) in

      (* Printf.printf "hh %d %d\n" myself_updated_hand_length other_updated_hand_length; *)
      (assert_equal (myself_updated_hand_length, 
                     other_updated_hand_length) expected_output) 
    )

let make_transfer_test_hb
    (name : string)
    (state : State.t)
    (myself_id : int)
    (other_id : int)
    (card_pile1: Dogma.card_pile) 
    (card_pile2: Dogma.card_pile) 
    (idx: int) 
    (top: bool)
    (expected_output : (int * int)) : test = 
  name >:: (fun _ -> 
      let myself = State.get_player state myself_id in
      let other = State.get_player state other_id in



      let state_after_transfer = 
        State.transfer state myself other card_pile1 card_pile2 idx top in

      let myself_updated = State.get_player state_after_transfer myself_id in
      let other_updated = State.get_player state_after_transfer other_id in

      let myself_updated_hand_length = 
        List.length (Player.get_hand myself_updated) in
      let other_updated_hand_length =
        Player.get_board_total_length other_updated in


      (assert_equal (myself_updated_hand_length, 
                     other_updated_hand_length) expected_output) 
    )

let make_transfer_test_bh
    (name : string)
    (state : State.t)
    (myself_id : int)
    (other_id : int)
    (card_pile1: Dogma.card_pile) 
    (card_pile2: Dogma.card_pile) 
    (idx: int) 
    (top: bool)
    (expected_output : (int * int)) : test = 
  name >:: (fun _ -> 
      let myself = State.get_player state myself_id in
      let other = State.get_player state other_id in

      let self_length =
        Player.get_board_total_length myself in
      let other_length = 
        List.length (Player.get_hand other) in
      Printf.printf "lengths before %d %d\n" self_length other_length;

      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in
      let myself_updated = State.get_player 
          state_after_transfer myself_id in
      let other_updated = State.get_player 
          state_after_transfer other_id in
      (* Printf.printf "ids %d %d" myself_id other_id; *)
      let self_updated_length =
        Player.get_board_total_length myself_updated in
      let other_updated_length = 
        List.length (Player.get_hand other_updated) in
      Printf.printf "lengths %d %d\n" self_updated_length other_updated_length;

      (assert_equal (self_updated_length, 
                     other_updated_length) expected_output) 
    )

let make_transfer_test_bb
    (name : string)
    (state : State.t)
    (myself_id : int)
    (other_id : int)
    (card_pile1: Dogma.card_pile) 
    (card_pile2: Dogma.card_pile) 
    (idx: int) 
    (top: bool)
    (expected_output : (int * int)) : test = 
  name >:: (fun _ -> 
      let myself = State.get_player state myself_id in
      let other = State.get_player state other_id in

      let self_length =
        Player.get_board_total_length myself in
      let other_length = 
        List.length (Player.get_hand other) in
      Printf.printf "lengths before %d %d\n" self_length other_length;

      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in
      let myself_updated = State.get_player 
          state_after_transfer myself_id in
      let other_updated = State.get_player 
          state_after_transfer other_id in
      (* Printf.printf "ids %d %d" myself_id other_id; *)
      let self_updated_length =
        Player.get_board_total_length myself_updated in
      let other_updated_length = 
        Player.get_board_total_length other_updated  in
      Printf.printf "lengths %d %d\n" self_updated_length other_updated_length;

      (assert_equal (self_updated_length, 
                     other_updated_length) expected_output) 
    )

let make_return_test
    (name: string)
    (state: State.t) 
    (player: Player.t) 
    (hand_idx: int)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      let state_after_return = State.return state player hand_idx in
      assert_equal expected_output (state_after_return)
    )

(** return one start *)
let init_state_ro = init_state test1
let init_player0_ro = player0 test1
let input_state_ro = State.draw init_state_ro init_player0_ro 0
let player_ro = State.get_player input_state_ro 0
let hand_idx_ro = 0
let expected_output_ro = init_state_ro
(** end *)

let make_score_test
    (name: string)
    (st: State.t)
    (hand_index: int)
    (expected_output: int) : test = 
  name >:: (fun _ ->
      let new_state = State.score st (st |> State.current_player) 
          hand_index in
      assert_equal expected_output 
        (State.get_score_by_id (new_state) 0)
    )
(* let make_tuck_test 
    (name : string)
    (st: State.t) *)

(** hand to hand start *)
let init_state_hh = init_state test1
let myself_id_hh = 0
let other_id_hh = 1
let init_player0_hh = State.get_player init_state_hh myself_id_hh 
let input_state_hh = State.draw init_state_hh (init_player0_hh) 0
(* let () = Player.print_player (State.get_player input_state_hh 0) *)
let card_pile1_hh = Dogma.Self_hand 0
let card_pile2_hh = Dogma.Other_hand 0
let idx_hh = 0
let top_hh = false
let expected_output_hh = (0,1)


(** hand to board start *)
let init_state_hb = init_state test1
let myself_id_hb = 1
let other_id_hb = 2
let init_player0_hb = State.get_player init_state_hb myself_id_hb
let input_state_hb = State.draw init_state_hb init_player0_hb 0

let card_pile1_hb = Dogma.Self_hand 0
let card_pile2_hb = Dogma.Other_stack Red
let idx_hb = 0
let top_hb = true
let expected_output_hb = (0,1)


let init_state_bh = init_state test1
let myself_id_bh = 2
let other_id_bh = 3
let init_player0_bh = State.get_player init_state_bh myself_id_bh
let input_state_bh = State.draw init_state_bh init_player0_bh 0

let card_pile1_bh = Dogma.Self_stack Red
let card_pile2_bh = Dogma.Other_hand 0
let idx_bh = 0
let top_bh = true
let expected_output_bh = (0,0) 

let updated_player0_bh = State.get_player input_state_bh myself_id_bh
let input_state_bh2 = State.meld input_state_bh updated_player0_bh 0

let expected_output_bh2 = (0,1)



let cards = List.hd (State.get_era_cards (init_state test1))
let card = List.hd cards



let init_state_bb = init_state test1
let myself_id_bb = 1
let other_id_bb = 2
let init_player0_bb = State.get_player init_state_bb myself_id_bb
let input_state_bb = State.draw init_state_bb init_player0_bb 0
let updated_player0_bb = State.get_player input_state_bb myself_id_bb
let input_state_bb2 = State.meld input_state_bb updated_player0_bb 0

let card_pile1_bb = Dogma.Self_stack Red
let card_pile2_bb = Dogma.Other_stack Red
let idx_bb = 0
let top_bb = true
let expected_output_bb = (0,1)

let test_tests = 
  [
    make_init_state_test "start player test" test 0;
    make_init_state_test "start player test1" test1 0;
    make_init_state_test "start player innov" innov 0;

    make_player_test "player2" innov 0 0;
    make_player_test "player1" test 1 1;
    make_player_test "player2" test1 2 2;
    make_player_test "player2" test1 3 3;

    make_before_draw_test "before draw card (test file)" 
      old_state_draw "";
    make_before_draw_test "before draw card (innov file)" 
      old_state_draw_innov "";

    make_after_draw_test "after draw card (test file)" 
      new_state_draw "Archery";
    make_after_draw_test "after draw card (innov file)" 
      new_state_draw_innov first_card;

    make_after_meld_test "after meld card (test file)" 
      new_state_meld "Archery";
    make_after_meld_test "after meld card (innov file)" 
      new_state_meld_innov "Archery"; 


    make_transfer_test_hh "hand to hand" input_state_hh 
      myself_id_hh other_id_hh card_pile1_hh card_pile2_hh 
      idx_hh top_hh expected_output_hh; 

    make_transfer_test_hb "hand to board" input_state_hb 
      myself_id_hb other_id_hb card_pile1_hb card_pile2_hb 
      idx_hb top_hb expected_output_hb; 

    make_transfer_test_bh "board to hand transferred" input_state_bh2 
      myself_id_bh other_id_bh card_pile1_bh card_pile2_bh
      idx_bh top_bh expected_output_bh2; 

    make_transfer_test_bb "board to board transferred" input_state_bb2 
      myself_id_bb other_id_bb card_pile1_bb card_pile2_bb
      idx_bb top_bb expected_output_bb; 

    make_return_test "return one" input_state_ro player_ro 
      hand_idx_ro expected_output_ro;

    make_score_test "score" new_state_draw 0 1;
  ]

let suite = 
  "test suite for final project"  >::: List.flatten [
    test_tests;
  ]

let _ = run_test_tt_main suite