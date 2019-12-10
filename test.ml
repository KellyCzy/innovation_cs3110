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

(* Test Plan: 
   The testing of system includes OUnit tests and manual test. 
   For manual tests, we used 'make play' and tried several different cases under 
   different situation and test to see if the outcome is what we want. The 
   manual test part ensures our connection and relationship between each modules
   and the progress of the game is correct. 
   For OUnit tests, we focus more on specific functions. We do not run the 
   whole game anymore, instead, we focus on specific feature in the game. We mainly
   have three parts. 

   The first part tests all the dogma effect we have (functions 
   in state.ml) Essential functions in state module are tested in state_tests, 
   including init_state, draw, meld, return, transfer, and score. 
   The second part tests all the essential functions in card module, including
   all the getter method. 
   The third part tests all the essential functions in player module, including 
   pop, remove, push, and count_icon. 

   All the test are developed using glass box testing. Based on the code we
   wrote, we developed test cases to ensure the functions that we wrote are 
   correct. We believe testing all these functions using OUnit tests and with 
   all our manual tests, it is enough to ensure our system is correct, because
   the first part ensures all essential functions in state are correct. If
   these are correct, the function in main is ensured to be correct. The 
   second part ensures all essential functions in card module are correct, which
   make sure the way we init a card from json file is correct, and getter methods
   in card module are correct as well. Since our game is a board game and relies 
   on cards, this part of the tests is important. The third part of the test
   make sure essential functions in player module is correct, which ensures
   the way the game interact with each player is corret. The crucial part 
   of the game includes state, player, and cards, which is why we developed 
   test cases to seperately ensures functions in these modules are correct.  *)

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
let first_card_index = Card.color_to_int (Card.get_color (State.get_era_cards_top old_state_draw_innov))
let new_state_draw_innov = State.draw old_state_draw_innov 
    (old_state_draw_innov|> current_player) 0


let new_state_draw_two = 
  State.draw new_state_draw 
    (new_state_draw|>current_player) 0

let make_before_draw_test 
    (name : string) 
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let str = (Player.print_hand (state|>current_player)) in 
      assert_equal expected_output str
    )

let make_after_draw_test 
    (name : string) 
    (state_cur: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let card = (Player.get_ith_hand (state_cur|>current_player) 0) in 
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

let make_after_meld_test 
    (name : string) 
    (index:int)
    (state: State.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let card_name = Player.get_top_card_name 
          (state|>State.current_player) index in
      assert_equal expected_output card_name
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


(** hand to hand start *)
let init_state_hh = init_state test1
let myself_id_hh = 0
let other_id_hh = 1
let init_player0_hh = State.get_player init_state_hh myself_id_hh 
let input_state_hh = State.draw init_state_hh (init_player0_hh) 0
let card_pile1_hh = Dogma.Self_hand 0
let card_pile2_hh = Dogma.Other_hand 0
let idx_hh = 0
let top_hh = false
let expected_output_hh = (0,1)


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
      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in

      let myself_updated = State.get_player state_after_transfer 
          myself_id in
      let other_updated = State.get_player state_after_transfer 
          other_id in

      let myself_updated_length = List.length 
          (Player.get_hand myself_updated) in
      let other_updated_length = List.length 
          (Player.get_hand other_updated) in

      (assert_equal (myself_updated_length, 
                     other_updated_length) expected_output) 
    )

(** hand to board start *)
let init_state_hb = init_state test1
let myself_id_hb = 1
let other_id_hb = 2
let init_player0_hb = State.get_player init_state_hb myself_id_hb
let input_state_hb = State.draw init_state_hb init_player0_hb 0

let card_pile1_hb = Dogma.Self_hand 0
let card_pile2_hb = Dogma.Other_stack Yellow
let idx_hb = 0
let top_hb = true
let expected_output_hb = (0,1)


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

      let myself_update_length = 
        List.length (Player.get_hand myself_updated) in
      let other_updated_length =
        Player.get_board_total_length other_updated in
      (assert_equal (myself_update_length, 
                     other_updated_length) expected_output) 
    )


let init_state_bh = init_state test1
let myself_id_bh = 2
let other_id_bh = 3
let init_player0_bh = State.get_player init_state_bh myself_id_bh
let input_state_bh = State.draw init_state_bh init_player0_bh 0

let card_pile1_bh = Dogma.Self_stack Yellow
let card_pile2_bh = Dogma.Other_hand 0
let idx_bh = 0
let top_bh = true
let expected_output_bh = (0,0) 

let updated_player0_bh = State.get_player input_state_bh myself_id_bh
let input_state_bh2 = State.meld input_state_bh updated_player0_bh 0

let expected_output_bh2 = (0,1)


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

      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in
      let myself_updated = State.get_player 
          state_after_transfer myself_id in
      let other_updated = State.get_player 
          state_after_transfer other_id in
      let self_updated_length =
        Player.get_board_total_length myself_updated in
      let other_updated_length = 
        List.length (Player.get_hand other_updated) in
      (assert_equal (self_updated_length, 
                     other_updated_length) expected_output) 
    )


let init_state_bb = init_state test1
let myself_id_bb = 1
let other_id_bb = 2
let init_player0_bb = State.get_player init_state_bb myself_id_bb
let input_state_bb = State.draw init_state_bb init_player0_bb 0
let updated_player0_bb = State.get_player input_state_bb myself_id_bb
let input_state_bb2 = State.meld input_state_bb updated_player0_bb 0

let card_pile1_bb = Dogma.Self_stack Yellow
let card_pile2_bb = Dogma.Other_stack Yellow
let idx_bb = 0
let top_bb = true
let expected_output_bb = (0,1)

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

      let state_after_transfer = 
        State.transfer state myself other card_pile1 
          card_pile2 idx top in
      let myself_updated = State.get_player 
          state_after_transfer myself_id in
      let other_updated = State.get_player 
          state_after_transfer other_id in

      let self_updated_length =
        Player.get_board_total_length myself_updated in
      let other_updated_length =
        Player.get_board_total_length other_updated in

      (assert_equal (self_updated_length, 
                     other_updated_length) expected_output) 
    )


let init_state_hs = init_state test1
let myself_id_hs = 0
let other_id_hs = 1
let init_player0_hs = State.get_player init_state_hs myself_id_hs
let input_state_hs = State.draw init_state_hs (init_player0_hs) 0
let card_pile1_hs = Dogma.Self_hand 0
let card_pile2_hs = Dogma.Other_score 0
let idx_hs = 0
let top_hs = false
let expected_output_hs = (0,1)

let make_transfer_test_hs
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

      let myself_updated_length = 
        List.length (Player.get_hand myself_updated) in
      let other_updated_length = 
        List.length (Player.get_score_cards other_updated) in

      (assert_equal (myself_updated_length, 
                     other_updated_length) expected_output)
    )

let state0 = init_state test
let player0 = State.get_player state0 0
let fake_player = Player.init_player 0

let state1 = State.draw state0 player0 0
let player1 = State.get_player state1 0
let players = State.get_players state1


let make_swap_player
    (name : string)
    (new_player: Player.t) 
    (player_list: Player.t list)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      let updated_player_list = State.swap_player new_player player_list in
      let first_player = List.hd updated_player_list in
      let updated_hand_length = Player.get_hand_length first_player in
      assert_equal updated_hand_length expected_output
    )

let state_tests = 
  [
    make_init_state_test "start player test" test 0;
    make_init_state_test "start player test1" test1 0;
    make_init_state_test "start player innov" innov 0;

    make_player_test "player2" innov 0 0;
    make_player_test "player1" innov 1 1;
    make_player_test "player1" test 1 1;
    make_player_test "player2" test1 2 2;
    make_player_test "player2" test1 3 3;

    make_swap_player "swap_player" fake_player players 0;


    make_before_draw_test "before draw card (test file)" 
      old_state_draw "";
    make_before_draw_test "before draw card (innov file)" 
      old_state_draw_innov "";

    make_after_draw_test "after draw card (test file)" 
      new_state_draw "Archery";
    make_after_draw_test "after draw card (innov file)" 
      new_state_draw_innov first_card;

    make_after_meld_test "after meld card (test file)" 
      0 new_state_meld "Archery";
    make_after_meld_test "after meld card (innov file)" 
      first_card_index new_state_meld_innov first_card; 

    make_transfer_test_hh "hand to hand" input_state_hh 
      myself_id_hh other_id_hh card_pile1_hh card_pile2_hh 
      idx_hh top_hh expected_output_hh; 

    make_transfer_test_hb "hand to board" input_state_hb 
      myself_id_hb other_id_hb card_pile1_hb card_pile2_hb 
      idx_hb top_hb expected_output_hb; 

    make_transfer_test_bh "board to hand" input_state_bh2 
      myself_id_bh other_id_bh card_pile1_bh card_pile2_bh
      idx_bh top_bh expected_output_bh2; 

    make_transfer_test_bb "board to board" input_state_bb2 
      myself_id_bb other_id_bb card_pile1_bb card_pile2_bb
      idx_bb top_bb expected_output_bb; 

    make_transfer_test_hs "hand to score" input_state_hs 
      myself_id_hs other_id_hs card_pile1_hs card_pile2_hs
      idx_hb top_hs expected_output_hs; 


    make_return_test "return one" input_state_ro player_ro 
      hand_idx_ro expected_output_ro;

    make_score_test "score" new_state_draw 0 1;
  ]

let test2 = Yojson.Basic.from_file "test2.json"

let test1_card = List.hd (List.hd (get_all_cards test1))

let test2_card = List.hd (List.hd (get_all_cards test2))


let make_card_get_color_test 
    (name : string) 
    (card:Card.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let str = (Card.get_color card |> Card.color_to_string) in 
      assert_equal expected_output str
    )

let rec all_card_icon icons = 
  match icons with
  | [] -> []
  | h::t -> (h |> Card.icon_to_string) :: (all_card_icon t)

let make_card_get_icons_test
    (name : string) 
    (card: Card.t)
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      let icons = (Card.get_icons card |> all_card_icon) in 
      assert_equal expected_output icons
    )

let make_card_get_dogma_icons_test
    (name : string) 
    (card: Card.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      let icon = (Card.get_dogmas_icon card |> Card.icon_to_string) in 
      assert_equal expected_output icon
    )

let make_card_get_value_test
    (name : string) 
    (card: Card.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      let value = (Card.get_value card) in 
      assert_equal expected_output value
    )


let card_test = 
  [
    make_card_get_color_test "card get color test1" test1_card "Yellow";
    make_card_get_color_test "card get color test2" test2_card "Yellow";

    make_card_get_icons_test "card get icon test1" 
      test1_card ["Blank";"Castle";"Castle";"Castle"];
    make_card_get_icons_test "card get icon test2"
      test2_card ["Blank";"Leaf";"Leaf";"Leaf"];

    make_card_get_dogma_icons_test "card get dogma icon test1"
      test1_card "Castle";
    make_card_get_dogma_icons_test "card get dogma icon test2"
      test2_card "Leaf";


    make_card_get_value_test "card get value test1"
      test1_card 0;
    make_card_get_value_test "card get value test2"
      test2_card 2;

  ]


let state_init = init_state test
let player0 = State.get_player state_init 0
let board = Player.get_board player0
let empty_stack = Player.init_stack Red and fake_card_list = []
let red = Dogma.Red
let red_int = 0

let state1 = State.draw state_init player0 0
let player1 = State.get_player state1 0
let state2 = State.meld state1 player1 0
let player2 = State.get_player state2 0


let make_get_color_stack_test 
    (name : string) 
    (player: Player.t)
    (c: Dogma.stack_color) 
    (expected_output : int): test = 
  name >:: (fun _ -> 
      let stack = Player.get_color_stack player c in
      let color = Player.get_stack_color stack in 
      let color_int = Player.map_color_to_int color in
      assert_equal color_int expected_output
    )

let make_update_stack_list_test 
    (name : string) 
    (player: Player.t) 
    (i: int)
    (new_s: stack) 
    (expected_output : int): test = 
  name >:: (fun _ -> 
      let board = Player.get_board player in
      let updated_stack_list = Player.update_stack_list board i new_s in
      let updated_ith = List.nth updated_stack_list i in
      let get_ith_length = Player.get_stack_length updated_ith in
      assert_equal get_ith_length expected_output
    )

let state_init1 = init_state test
let cards = List.hd (State.get_era_cards state_init1)


let make_pop_card_test 
    (name : string) 
    (i: int)
    (cards: Card.t list) 
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let popi, cardi = Player.pop_card i cards in
      assert_equal (Card.get_title cardi) expected_output
    )
let make_pop_card_rest_test 
    (name : string) 
    (i: int)
    (cards: Card.t list) 
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let popi, cardi = Player.pop_card i cards in
      let rest = List.hd popi in
      assert_equal (Card.get_title rest) expected_output
    )


let state3 = State.draw state2 player2 0
let player3 = State.get_player state3 0
let state4 = State.meld state3 player3 0
let player4 = State.get_player state4 0

let ith_stack = Player.get_ith_stack player4 0


let make_pop_stack_test 
    (name : string) 
    (i: int)
    (stack: stack) 
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let popi, cardi = Player.pop_stack i stack in
      assert_equal (Card.get_title cardi) expected_output
    )

let make_pop_stack_rest_test 
    (name : string) 
    (i: int)
    (stack: stack) 
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let popi, cardi = Player.pop_stack i stack in
      let rest = List.hd (Player.get_stack_cards popi) in
      assert_equal (Card.get_title rest) expected_output
    )

let state5 = State.draw state1 player1 0
let player5 = State.get_player state5 0

let card_to_push0 = Player.get_ith_hand player5 0
let card_to_push1 = Player.get_ith_hand player5 1

let one_card_stack = Player.push_stack card_to_push0 empty_stack true 

let make_remove_hand_test 
    (name : string) 
    (player: Player.t)
    (i: int)
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let updated_player = Player.remove_hand player i in
      let ith_hand = get_ith_hand updated_player 0 in
      let title = Card.get_title ith_hand in
      print_endline title;
      assert_equal title expected_output
    )


let make_push_stack_test 
    (name : string) 
    (card: Card.t)
    (stack: stack) 
    (top: bool)
    (expected_output : string): test = 
  name >:: (fun _ -> 
      let updated_stack = Player.push_stack card stack top in
      let _, top_card = Player.pop_stack 0 updated_stack in
      let title = Card.get_title top_card in
      assert_equal title expected_output
    )

let empty_board = Player.get_board player5
let state6 = State.meld state5 player5 0
let player6 = State.get_player state6 0
let board1 = Player.get_board player6
let state7 = State.meld state6 player6 0
let player7 = State.get_player state7 0
let board2 = Player.get_board player7

let make_count_icon_test 
    (name : string) 
    (board: stack list)
    (icon: Card.icon) 
    (expected_output : int): test = 
  name >:: (fun _ -> 
      let num = Player.count_icon board icon 0 in
      Printf.printf "num%d" num;
      assert_equal num expected_output
    )

let player_test = 
  [
    make_get_color_stack_test "get_color_stack" player0 red red_int;
    make_update_stack_list_test "update_stack_list" player2 0 empty_stack 0;
    make_pop_card_test "pop_card check popped card" 0 cards "Archery";
    make_pop_card_rest_test "pop_card check the rest of card 0" 0 cards "Oars";
    make_pop_card_test "pop_card check popped card" 1 cards "Oars";
    make_pop_card_rest_test "pop_card check the rest of card 1" 1 cards "Archery";

    make_pop_stack_test "pop_stack check popped stack" 0 ith_stack "Oars";
    make_pop_stack_rest_test "pop_stack check the rest of stack 0" 0 ith_stack "Archery";
    make_pop_stack_test "pop_stack check popped stack" 1 ith_stack "Archery";
    make_pop_stack_rest_test "pop_stack check the rest of stack 1" 1 ith_stack "Oars";

    make_remove_hand_test "remove card 0from hand" player5 0 "Archery";
    make_remove_hand_test "remove card 1 from hand" player5 1 "Oars";

    make_push_stack_test "push card onto an empty stack" card_to_push0 empty_stack true "Oars";
    make_push_stack_test "tuck card under an empty stack" card_to_push0 empty_stack false "Oars";

    make_push_stack_test "push card onto an stack with one card" card_to_push1 one_card_stack true "Archery";
    make_push_stack_test "tuck card under an empty stack with one card" card_to_push1 one_card_stack false "Oars";

    make_count_icon_test "count_icon empty" empty_board Card.Castle 0;
    make_count_icon_test "count_icon castle" board1 Card.Castle 2;
    make_count_icon_test "count_icon crown" board1 Card.Crown 1;
    make_count_icon_test "count_icon stacked cards" board2 Card.Pattern 2;

  ]


let suite = 
  "test suite for final project"  >::: List.flatten [
    state_tests;
    card_test;
    player_test;
  ]

let _ = run_test_tt_main suite