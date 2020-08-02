open OUnit2
open Deck
open Command 
open State
open Formatter

(* Test Plan
   For our state, command, deck, autoplayer, and formatter modules, we tested 
   our functions automatically using OUnit. For our main module, we manually 
   tested by running our game engine and playing the mahjong game. 

   Our approach to the automatic testing was similar to previous projects like
   A2 and A3. We used a combination of black box and glass box testing to create
   our test suite. We created our test suite by using helper functions to 
   generate tests for functions in the modules we wanted to test. We used black 
   box testing to ensure proper coverage of normal and edge cases. This was 
   accomplished by making multiple variations of games that would allow us to 
   cover the myriad moves and combinations seen in mahjong.

   We also manually tested our entire game engine by playing mahjong in the
   terminal. This allowed us to check that the game was properly progressing and
   following the cycle of a player drawing and discarding a tile to begin and 
   end their turn. This was especially useful when we introduced the autoplayers
   to ensure that the handoff to the autoplayers to take their turns was 
   successful. 

   By using automatic testing to ensure individual functions worked properly and
   then manual testing to ensure our game ran as intended, we were able to
   demonstrate the correctness of our system.
*)

(* State Tests *)
(** [make_parse_difficulty_test name str expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [parse_difficulty str]. *)
let make_parse_difficulty_test 
    (name : string) 
    (str : string) 
    (expected_output : difficulty) : test =
  name >:: (fun _ -> assert_equal expected_output (parse_difficulty str))

(** [make_parse_difficulty_error_test name str expected_output] constructs an
    OUnit test named [name] that asserts an error being raised by 
    [parse_difficulty str]. *)
let make_parse_difficulty_error_test 
    (name : string) 
    (str : string) 
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> parse_difficulty str))

(** [make_get_difficulty_test name st expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [get_difficulty st]. *)
let make_get_difficulty_test 
    (name : string) 
    (st : State.t) 
    (expected_output : difficulty) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_difficulty st))

(** [make_get_curr_id_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [get_curr_id state]. *)
let make_get_curr_id_test 
    (name : string) 
    (state : State.t) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_curr_id state))

(** [make_compare_tile_test name tile1 tile2 expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [compare_tile tile1 tile2]. *)
let make_compare_tile_test 
    (name : string) 
    (tile1 : Deck.tile) 
    (tile2 : Deck.tile) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (compare_tile tile1 tile2))

(** [make_get_current_hand_test name state id expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [get_current_hand state id]. *)
let make_get_current_hand_test 
    (name : string) 
    (state : State.t) 
    (id : int) 
    (expected_output : Deck.tile list) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_current_hand state id))

(** [make_get_display_hand_test name state id expected] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [get_display_hand state id]. *)
let make_get_display_hand_test 
    (name : string) 
    (state : State.t) 
    (id : int) 
    (expected_output : Deck.tile list) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_display_hand state id))

(** [make_remaining_test name state expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with 
    [remaining state]. *)
let make_remaining_test 
    (name : string) 
    (state : State.t) 
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> assert_equal expected_output (remaining state))

(** [make_discarded_test name state expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [discarded state]. *)
let make_discarded_test 
    (name : string) 
    (state : State.t) 
    (expected_output : Deck.tile list) : test = 
  name >:: (fun _ -> assert_equal expected_output (discarded state))

(** [make_last_discarded_test name state expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [last_discarded state]. *)
let make_last_discarded_test 
    (name : string) 
    (state : State.t) 
    (expected_output : Deck.tile) : test = 
  name >:: (fun _ -> assert_equal expected_output (last_discarded state))

(** [make_last_discarded_error_test name state expected_output] constructs an
    OUnit test named [name] that asserts an error being raised by 
    [last_discarded state]. *)
let make_last_discarded_error_test 
    (name : string) 
    (state : State.t) 
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> last_discarded state))

(** [make_check_eat_test name hand discard expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [check_eat hand discard]. *)
let make_check_eat_test 
    (name : string) 
    (hand : Deck.tile list) 
    (discard : Deck.tile) 
    (expected_output : (Deck.tile * Deck.tile * Deck.tile) list option) : test = 
  name >:: (fun _ -> assert_equal expected_output (check_eat hand discard))

(** [make_index_test name lst idx expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [index lst idx]. *)
let make_index_test 
    (name : string) 
    (lst : (Deck.tile * Deck.tile * Deck.tile) list) 
    (idx : int) 
    (expected_output : Deck.tile * Deck.tile * Deck.tile) : test = 
  name >:: (fun _ -> assert_equal expected_output (index lst idx))

(** [make_check_bump_test name hand discard expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [check_bump hand discard]. *)
let make_check_bump_test 
    (name : string) 
    (hand : Deck.tile list) 
    (discard : Deck.tile) 
    (expected_output : (Deck.tile * Deck.tile * Deck.tile list) option) : test = 
  name >:: (fun _ -> assert_equal expected_output (check_bump hand discard))

(** [make_win_helper_test] constructs an OUnit test named [name] that asserts 
    the quality of [expected_output] with [win_helper hand]. *)
let make_win_helper_test 
    (name : string) 
    (hand : Deck.tile list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (win_helper hand))

(** [make_remove_two_tiles_test] constructs an OUnit test named [name] that 
    asserts the quality of [expected_output] with [remove_two_tiles tile1 tile2 
    tile_list acc flag]. *)
let make_remove_two_tiles_test 
    (name : string) 
    (tile1 : Deck.tile) 
    (tile2 : Deck.tile) 
    (tile_list : Deck.tile list) 
    (acc : Deck.tile list) 
    (flag: bool) 
    (expected_output : Deck.tile list) : test = 
  name >:: (fun _ -> assert_equal expected_output (remove_two_tiles tile1 tile2 
                                                     tile_list acc flag))

(** [make_check_pair_test] constructs an OUnit test named [name] that asserts 
    the quality of [expected_output] with [check_pair hand acc]. *)
let make_check_pair_test 
    (name : string) 
    (hand : Deck.tile list)
    (acc : Deck.tile list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (check_pair hand acc))

(** [make_check_gang_test name hand discard expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [check_gang hand discard]. *)
let make_check_gang_test 
    (name : string) 
    (hand : Deck.tile list) 
    (discard : Deck.tile) 
    (expected_output : (Deck.tile list) option) : test = 
  name >:: (fun _ -> assert_equal expected_output (check_gang hand discard))

(** [make_next_draw_test name state expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [next_draw state]. *)
let make_next_draw_test 
    (name : string) 
    (state : State.t) 
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output (next_draw state))

(* Command Tests *)
(** [make_parse_test name command expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [check_bump hand discard]. *)
let make_parse_test 
    (name : string) 
    (command : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse command))

(** [make_parse_error_test name command expected_output] constructs an
    OUnit test named [name] that asserts an error being raised by 
    [parse command]. *)
let make_parse_error_test 
    (name : string) 
    (command : string) 
    (expected_output : exn) : test =
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> parse command))

(** [make_tile_from_phrase_test name phrase expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [tile_from_phrase phrase]. *)
let make_tile_from_phrase_test 
    (name : string) 
    (phrase : string list) 
    (expected_output : Deck.tile) : test =
  name >:: (fun _ -> assert_equal expected_output (tile_from_phrase phrase))

(** [make_tile_from_phrase_error_test name phrase expected_output] constructs 
    an OUnit test named [name] that asserts an error being raised by 
    [tile_from_phrase phrase]. *)
let make_tile_from_phrase_error_test 
    (name : string) 
    (phrase : string list) 
    (expected_output : exn) : test =
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> tile_from_phrase phrase))

(** [make_phrase_from_tile_test name tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [phrase_from_tile tile]. *)
let make_phrase_from_tile_test 
    (name : string) 
    (tile : Deck.tile) 
    (expected_output : string list) : test =
  name >:: (fun _ -> assert_equal expected_output (phrase_from_tile tile))

(* Formatter Tests *)
(** [make_format_current_test name state id expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [format_current state id]. *)
let make_format_current_test 
    (name : string) 
    (state : State.t)
    (id : int) 
    (expected_output : string) : test =
  name >:: (fun _ -> assert_equal expected_output (format_current state id))

(** [make_format_display_test name state id expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [format_display state id]. *)
let make_format_display_test 
    (name : string) 
    (state : State.t)
    (id : int) 
    (expected_output : string) : test =
  name >:: (fun _ -> assert_equal expected_output (format_display state id))

(** [make_format_eat_test name lst acc opts expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [format_eat lst acc opts]. *)
let make_format_eat_test 
    (name : string) 
    (lst : (Deck.tile * Deck.tile * Deck.tile) list)
    (acc : string)
    (opts : int) 
    (expected_output : string) : test =
  name >:: (fun _ -> assert_equal expected_output (format_eat lst acc opts))

(* Deck Tests *)
(** [make_draw_test name deck expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [draw deck]. *)
let make_draw_test 
    (name : string) 
    (deck : Deck.t) 
    (expected_output : (Deck.tile * Deck.t)) : test = 
  name >:: (fun _ -> assert_equal expected_output (Deck.draw deck))

(** [make_format_test name tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [format tile]. *)
let make_format_test 
    (name : string) 
    (tile : Deck.tile) 
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output (format tile))

(** [make_off_top_test name deck num expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [off_top deck num]. *)
let make_off_top_test 
    (name : string) 
    (deck : Deck.t) 
    (num : int) 
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> assert_equal expected_output (off_top deck num))

(** [make_order_test name tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [order tile]. *)
let make_order_test 
    (name : string) 
    (tile : Deck.tile) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (order tile))

(** [make_get_stick_num_test name init tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [get_stick_num init tile]. *)
let make_get_stick_num_test 
    (name : string) 
    (init : int list) 
    (tile : Deck.tile) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_stick_num init tile))

(** [make_get_tens_num_test name init tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [get_tens_num init tile]. *)
let make_get_tens_num_test 
    (name : string) 
    (init : int list) 
    (tile : Deck.tile) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_tens_num init tile))

(** [make_get_disc_num_test name init tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [get_disc_num init tile]. *)
let make_get_disc_num_test 
    (name : string) 
    (init : int list) 
    (tile : Deck.tile) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_disc_num init tile))

(** [make_get_num_test name tile expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [get_num tile]. *)
let make_get_num_test 
    (name : string) 
    (tile : Deck.tile) 
    (expected_output : int) : test = 
  name >:: (fun _ -> assert_equal expected_output (get_num tile))

(* Autoplayer Tests *)
(** [make_discards_test name discards hand] constructs an OUnit 
    test named [name] that asserts the quality of [discards] with 
    [get_discards hand]. *)
let make_discards_test
    (name : string)
    (discards : tile list)
    (hand : tile list)
    (difficulty : State.difficulty) : test =
  name >:: (fun _ -> 
      assert_equal discards
        (List.sort State.compare_tile 
           (Autoplayer.get_discards hand difficulty)))

(** [make_straights_test name discards hand] constructs an OUnit 
    test named [name] that asserts the quality of [straights] with 
    [get_straights hand]. *)
let make_straights_test
    (name : string)
    (hand: Deck.tile list)
    (straights : Deck.tile list): test =
  name >:: (fun _ -> 
      assert_equal straights
        (Autoplayer.get_straights hand))

(** [make_discard_commands_test name discards hand] constructs an OUnit 
    test named [name] that asserts the quality of [discards] with 
    [get_discards hand]. *)
let make_discard_commands_test
    (name : string)
    (discards : command list)
    (st : State.t)
    (id : int) : test =
  name >:: (fun _ -> 
      assert_equal discards
        (Autoplayer.get_discard_moves st id))

(** [make_auto_play_test name play st id] constructs an OUnit 
    test named [name] that asserts the quality of [play] with 
    [get_play_moves st id]. *)
let make_auto_play_test
    (name : string)
    (play : command)
    (st : State.t)
    (id : int) : test =
  name >:: (fun _ -> 
      assert_equal play
        (Autoplayer.get_play_moves st id))

let game1_remaining = 
  Deck.from_tile_list ([Disc 1; Disc 1; Disc 2; Disc 2; Disc 2; Disc 3; Disc 3; 
                        Disc 3; Disc 4; Disc 4; Disc 4; Disc 5; Disc 5; Disc 6; 
                        Disc 6; Disc 6; Disc 6; Disc 7; Disc 7; Disc 7; Disc 7; 
                        Disc 8; Disc 8; Disc 9; Disc 9; Disc 9; 
                        Stick 1; Stick 2; Stick 2; Stick 2; Stick 3; Stick 3; 
                        Stick 3; Stick 3; Stick 4; Stick 4; Stick 4; Stick 5; 
                        Stick 5; Stick 5; Stick 6; Stick 6; Stick 6; Stick 7; 
                        Stick 7; Stick 7; Stick 8; Stick 8; Stick 8; Stick 9; 
                        Stick 9; Stick 9;
                        Tens 1; Tens 1; Tens 1; Tens 2; Tens 2; Tens 2; Tens 3; 
                        Tens 3; Tens 3; Tens 4; Tens 4; Tens 4; Tens 5; Tens 5; 
                        Tens 5; Tens 6; Tens 6; Tens 6; Tens 7; Tens 7; Tens 7; 
                        Tens 7; Tens 8; Tens 8; Tens 8; Tens 8; Tens 9; 
                        North; East; South; Green; Red
                       ])

let (game1 : State.t) = {
  current = [(0, [Stick 1; Stick 1; Stick 2; Stick 4; Stick 5; Stick 6; Stick 7;
                  North; East; South; West; White; White]);
             (1, [Disc 1; Disc 2; Disc 3; Disc 4; Disc 8; Disc 8; Disc 9; 
                  Tens 1; Tens 5; Tens 9; Green; Red; White]);
             (2, [Disc 1; Disc 5; Disc 5; Stick 1; Stick 8; Stick 9; Tens 2; 
                  Tens 3; Tens 4; Tens 6; Tens 9; Tens 9; Red]);
             (3, [North; North; East; East; South; South; West; West; West; 
                  Green; Green; Red; White])];
  curr_id = 0;
  display = [(0, []); (1, []); (2, []); (3, [])];
  remaining = game1_remaining;
  discarded = [];
  difficulty = Hard
}

let easy_game = {game1 with difficulty = Easy}

let game1' = {game1 with curr_id = 3}
let game1_discard = match discard White game1' with 
  | Legal st -> st 
  | Illegal -> game1'
let game1_bump = match bump 0 game1_discard with 
  | Legal st -> st
  | Illegal -> game1_discard

let game2 = {game1 with discarded = [Disc 8]}

let game3 = {game1 with curr_id = 2; discarded = [Stick 7]}
let game3_eat = match eat 2 game3 (Stick 7, Stick 8, Stick 9) with 
  | Legal st -> st 
  | Illegal -> game3

let game4 = {game1 with discarded = [Stick 3]}
let game_4_eat_opts = match (check_eat (get_current_hand game4 0) (Stick 3)) 
  with 
  | Some lst -> lst
  | None -> []
let game5 = {game1 with discarded = [Stick 5]}
let game6 = {game2 with curr_id = 2}

let game7 = {game1 with discarded = [West]}
let game7_gang = match gang 3 game7 with 
  | Legal st -> st 
  | Illegal -> game7

let game8 = 
  {game1 with 
   current = [(0, [Stick 1; Stick 1; Stick 2; Stick 4; Stick 5; Stick 6; Stick 7;
                   North; East; South; South; White; White]);
              (1, [Disc 1; Disc 2; Disc 3; Disc 4; Disc 8; Disc 8; Disc 9; 
                   Tens 1; Tens 5; Tens 9; Green; Red; White]);
              (2, [Disc 1; Disc 5; Disc 5; Stick 1; Stick 8; Stick 9; Tens 2; 
                   Tens 3; Tens 4; Tens 6; Tens 9; Tens 9; Red]);
              (3, [North; North; East; East; South; West; West; West; West; 
                   Green; Green; Red; White])]}

let game8_gang = match gang_hand 3 game8 with 
  | Legal st -> st 
  | Illegal -> game8

let auto_hand1 = [Disc 1; Disc 2; Disc 3; Disc 4; Disc 7; Disc 8; Disc 9; 
                  Tens 1; Tens 3; Tens 4; Green; White; White; White]
let hand1_discards = [Disc 4; Tens 4; Green]
let auto_hand2 = [Disc 1; Disc 2; Disc 3; Disc 4; Disc 7; Disc 8; Disc 9; 
                  Tens 1; Tens 3; Green; Green; White; White; White]
let hand2_discards = [Disc 4]
let auto_hand3 = [Disc 1; Disc 2; Disc 4; Disc 5; Disc 7; Disc 8; Disc 9; 
                  Tens 1; Tens 3; Green; Green; White; White; White]
let hand3_discards = [Disc 1; Disc 2; Disc 4; Disc 5; Tens 1; Tens 3]
let command_discards = [Discard ["4"; "Discs"]; Discard ["8"; "Discs"]; Discard 
                          ["8"; "Discs"]; Discard ["9"; "Discs"];
                        Discard ["1"; "Ten"]; Discard ["5"; "Tens"]; 
                        Discard ["9"; "Tens"]; Discard ["Green"]; 
                        Discard ["Red"]; Discard ["White"]]

let auto_tests = [
  make_straights_test "straights" auto_hand1 [Disc 1; Disc 2; Disc 3; 
                                              Disc 7; Disc 8; Disc 9];
  make_discards_test "auto discards" hand1_discards auto_hand1 Hard;
  make_discards_test "auto discards filters out pairs and consecutive twos"
    hand2_discards auto_hand2 Hard;
  make_discards_test "auto discards when you must choose
  from consecutive twos" hand3_discards auto_hand3 Hard;
  make_auto_play_test "auto player picks bump correctly" Bump game2 1;
  make_auto_play_test "auto player picks eat correctly" Eat game3 2;
  make_auto_play_test "auto player picks draw correctly" Draw game6 2;
]

let state_tests = [
  make_parse_difficulty_test "parse easy" "easy" Easy;
  make_parse_difficulty_test "parse hard" "hard" Hard;

  make_parse_difficulty_error_test "parse empty string" "" UnknownDifficulty;
  make_parse_difficulty_error_test "parse invalid string" " easy " 
    UnknownDifficulty;
  make_parse_difficulty_error_test "parse invalid string" " aefaejf ewfjeaiwj " 
    UnknownDifficulty;

  make_get_difficulty_test "difficulty of game1" game1 Hard;
  make_get_difficulty_test "difficulty of easy_game" easy_game Easy;

  make_get_curr_id_test "game1 current player id" game1 0;
  make_get_curr_id_test "game1_discard current player id" game1_discard 0;
  make_get_curr_id_test "game7_gang current player id" game7_gang 3;

  make_compare_tile_test "compare Disc and Stick" (Disc 1) (Stick 1) (-1);
  make_compare_tile_test "compare same tile" (Tens 1) (Tens 1) 0;
  make_compare_tile_test "compare Stick and Disc" (Stick 1) (Disc 1) 1;

  make_get_current_hand_test "current hand of player 0" game1 0 
    [Stick 1; Stick 1; Stick 2; Stick 4; Stick 5; Stick 6; Stick 7; North; 
     East; South; West; White; White];
  make_get_current_hand_test "current hand of player 1" game1 1 
    [Disc 1; Disc 2; Disc 3; Disc 4; Disc 8; Disc 8; Disc 9; Tens 1; Tens 5; 
     Tens 9; Green; Red; White];
  make_get_current_hand_test "current hand of player 2" game1 2
    [Disc 1; Disc 5; Disc 5; Stick 1; Stick 8; Stick 9; Tens 2; Tens 3; 
     Tens 4; Tens 6; Tens 9; Tens 9; Red];
  make_get_current_hand_test "current hand of player 3" game1 3 
    [North; North; East; East; South; South; West; West; West; Green; Green; 
     Red; White];
  make_get_current_hand_test "current hand of player 3" game7_gang 3 
    [Disc 1; North; North; East; East; South; South; Green; Green; Red; White];
  make_get_current_hand_test "current hand of player 3 gang hand" game8_gang 3 
    [Disc 1; North; North; East; East; South; Green; Green; Red; White];

  make_get_display_hand_test "display hand of player 0 game1" game1 0 [];
  make_get_display_hand_test "display hand of player 0 game1_bump" game1_bump 0 
    [White; White; White];
  make_get_display_hand_test "display hand of player 2 game3_eat" game3_eat 2 
    [Stick 7; Stick 8; Stick 9];
  make_get_display_hand_test "display hand of player 3 game7_gang" game7_gang 3 
    [West; West; West; West];

  make_remaining_test "remaining deck of game1" game1 game1_remaining;

  make_discarded_test "discarded of game1" game1 [];
  make_discarded_test "discarded of game1'" game1_discard [White];
  make_discarded_test "discarded of game1_bump" game1_bump [];
  make_discarded_test "discarded of game7" game7 [West];
  make_discarded_test "discarded of game7_gang" game7_gang [];

  make_last_discarded_test "last discarded of game2" game2 (Disc 8);
  make_last_discarded_test "last discarded of game1'" game1_discard White;

  make_last_discarded_error_test "last discarded error" game1 UnknownTile;

  make_check_eat_test "check eat of player 2 in game3" 
    (get_current_hand game3 2) (Stick 7) (Some [(Stick 7, Stick 8, Stick 9)]);
  make_check_eat_test "check eat of player 0 in game4" 
    (get_current_hand game4 0) (Stick 3) 
    (Some [(Stick 1, Stick 2, Stick 3); (Stick 2, Stick 3, Stick 4); 
           (Stick 3, Stick 4, Stick 5)]);
  make_check_eat_test "check eat of player 0 in game5" 
    (get_current_hand game5 0) (Stick 5) 
    (Some [(Stick 4, Stick 5, Stick 6); (Stick 5, Stick 6, Stick 7)]);

  make_index_test "index 0 of eat options" game_4_eat_opts 0 
    (Stick 1, Stick 2, Stick 3);
  make_index_test "index 1 of eat options" game_4_eat_opts 1 
    (Stick 2, Stick 3, Stick 4);
  make_index_test "index 2 of eat options" game_4_eat_opts 2 
    (Stick 3, Stick 4, Stick 5);

  make_check_bump_test "check_bump of player 0 in game1_discard" 
    (get_current_hand game1_discard 0) White (Some (White, White, [White]));
  make_check_bump_test "check_bump of player 1 in game1_discard" 
    (get_current_hand game1_discard 1) White None;

  make_win_helper_test "winning hand 1" [Disc 3; Disc 2; Disc 1] true;
  make_win_helper_test "invalid hand 1" [Stick 2; Stick 1; Stick 1] false;
  make_win_helper_test "invalid hand 2" [North; Stick 1; Stick 1] false;
  make_win_helper_test "invalid hand 3" [Stick 2; Stick 2; Stick 1; Stick 1] 
    false;
  make_win_helper_test "invalid hand 4" [Tens 10; Tens 10; Tens 10; Tens 10] 
    false;
  make_win_helper_test "winning hand 2" [Disc 5; Disc 4; Disc 4; Disc 3; Disc 3;
                                         Disc 3; Disc 2; Disc 2; Disc 1] true;
  make_win_helper_test "winning hand 3" [Disc 5; Disc 5; Disc 5; Tens 4; Tens 3;
                                         Tens 2; South; South; South] true;

  make_remove_two_tiles_test "remove two different tiles from unique list" 
    (Disc 1) (Disc 2) [Disc 1; Disc 2; Disc 3] [] false [Disc 3];
  make_remove_two_tiles_test "remove two different tiles from duplicate list" 
    (Disc 1) (Disc 2) [Disc 1; Disc 2; Disc 2] [] false [Disc 2];
  make_remove_two_tiles_test "remove two same tiles from duplicate list" 
    (Disc 1) (Disc 1) [Disc 1; Disc 1; Disc 1] [] false [Disc 1];

  make_check_pair_test "winning hand 10" [Disc 1; Disc 2; Disc 3; Disc 4; 
                                          Disc 4; Disc 5; Disc 5; Disc 5; 
                                          Disc 6; Disc 7;Disc 8; North; North; 
                                          North]
    [Disc 1; Disc 2; Disc 3; Disc 4; Disc 4; Disc 5; Disc 5; Disc 5; Disc 6; 
     Disc 7; Disc 8; North; North; North] true;
  make_check_pair_test "winning hand 11" [Disc 1; Disc 2; Disc 2; Disc 3; 
                                          Disc 3; Disc 3; Disc 4; Disc 4; 
                                          Disc 5; North; North; Red; Red; Red]
    [Disc 1; Disc 2; Disc 2; Disc 3; Disc 3; Disc 3; Disc 4; Disc 4; Disc 5; 
     North; North; Red; Red; Red] true;
  make_check_pair_test "invalid hand 10" [Disc 2; Disc 2; Disc 2; Disc 3; 
                                          Disc 3; Disc 3; Disc 4; Disc 4; 
                                          Disc 5; North; North; Red; Red; Red]
    [Disc 2; Disc 2; Disc 2; Disc 3; Disc 3; Disc 3; Disc 4; Disc 4; Disc 5; 
     North; North; Red; Red; Red] false;
  make_check_pair_test "winning hand 12" 
    [Disc 1; Disc 2; Disc 3; Disc 7; Disc 8; Disc 9; Tens 1; Tens 2; Tens 3;
     Green; Green; Green; White; White]
    [Disc 1; Disc 2; Disc 3; Disc 7; Disc 8; Disc 9; Tens 1; Tens 2; Tens 3;
     Green; Green; Green; White; White] true;

  make_check_gang_test "valid check_gang" 
    [Stick 7; Stick 8; Stick 8; Stick 8; Stick 9] (Stick 8) 
    (Some [Stick 8; Stick 8; Stick 8; Stick 8]);
  make_check_gang_test "invalid check_gang" 
    [Stick 1; Stick 2; Stick 3; Stick 4; Stick 5; Stick 5] (Stick 5) None;

  make_next_draw_test "next draw of game1" game1 ("1 Disc");
  make_next_draw_test "next draw of game7_gang" game7_gang ("1 Disc");
]

let command_tests = [
  make_parse_test "parse draw" "draw" Draw;
  make_parse_test "parse eat" "eat" Eat;
  make_parse_test "parse bump" "bump" Bump;
  make_parse_test "parse win" "win" Win;
  make_parse_test "parse help" "help" Help;
  make_parse_test "parse quit" "quit" Quit;
  make_parse_test "parse start" "start" Start;
  make_parse_test "parse pass" "pass" Pass;
  make_parse_test "parse discard" "discard sticks" (Discard ["sticks"]);

  make_parse_error_test "parse DRAW" "DRAW" Malformed;
  make_parse_error_test "parse empty string" "" Empty;
  make_parse_error_test "parse discard nothing" "discard" Malformed;
  make_parse_error_test "parse draw" "draw     " Malformed;

  make_tile_from_phrase_test "tile from phrase 1 disc" ["1"; "Disc"] (Disc 1);
  make_tile_from_phrase_test "tile from phrase 1 stick" ["1"; "Stick"](Stick 1);
  make_tile_from_phrase_test "tile from phrase 1 ten" ["1"; "Ten"] (Tens 1);
  make_tile_from_phrase_test "tile from phrase 2 discs" ["2"; "Discs"] (Disc 2);
  make_tile_from_phrase_test "tile from phrase 2 sticks" ["2"; "Sticks"] 
    (Stick 2);
  make_tile_from_phrase_test "tile from phrase 2 tens" ["2"; "Tens"] (Tens 2);
  make_tile_from_phrase_test "tile from west" ["West"] West;
  make_tile_from_phrase_test "tile from east" ["East"] East;
  make_tile_from_phrase_test "tile from north" ["North"] North;
  make_tile_from_phrase_test "tile from south" ["South"] South;
  make_tile_from_phrase_test "tile from green" ["Green"] Green;
  make_tile_from_phrase_test "tile from white" ["White"] White;
  make_tile_from_phrase_test "tile from red" ["Red"] Red;

  make_tile_from_phrase_error_test "tile from empty string list" [] Empty;
  make_tile_from_phrase_error_test "tile from invalid phrase 1" ["2"; "Disc"] 
    Malformed;
  make_tile_from_phrase_error_test "tile from invalid phrase 2" ["West "] 
    Malformed;
  make_tile_from_phrase_error_test "tile from list with empty string" [""] 
    Malformed;
  make_tile_from_phrase_error_test "tile from invalid phrase 3" [" Red"] 
    Malformed;
  make_tile_from_phrase_error_test "tile from 1 discs" ["1 Discs"] Malformed;

  make_phrase_from_tile_test "phrase from disc 1" (Disc 1) ["1"; "Disc"];
  make_phrase_from_tile_test "phrase from stick 1" (Stick 1) ["1"; "Stick"];
  make_phrase_from_tile_test "phrase from tens 1" (Tens 1) ["1"; "Ten"]; 
  make_phrase_from_tile_test "phrase from disc 3" (Disc 3) ["3"; "Discs"];
  make_phrase_from_tile_test "phrase from stick 4" (Stick 4) ["4"; "Sticks"];
  make_phrase_from_tile_test "phrase from tens 5" (Tens 5) ["5"; "Tens"];  
  make_phrase_from_tile_test "phrase from west" West ["West"];
  make_phrase_from_tile_test "phrase from east" East ["East"];
  make_phrase_from_tile_test "phrase from north" North ["North"];
  make_phrase_from_tile_test "phrase from south" South ["South"];
  make_phrase_from_tile_test "phrase from green" Green ["Green"];
  make_phrase_from_tile_test "phrase from white" White ["White"];
  make_phrase_from_tile_test "phrase from red" Red ["Red"];
]

let formatter_tests = [
  make_format_current_test "formatted hand of player 0 in game1" game1 0 
    ("1 Stick, 1 Stick, 2 Sticks, 4 Sticks, 5 Sticks, 6 Sticks, 7 Sticks, " ^ 
     "North, East, South, West, White, White");
  make_format_current_test "formatted hand of player 3 in game1" game1 3 
    ("North, North, East, East, South, South, West, West, West, Green, Green" ^ 
     ", Red, White");

  make_format_display_test "formatted display hand of empty display hand" game1 
    0 "";
  make_format_display_test "formatted display hand of player 0 in game1_bump" 
    game1_bump 0 "White, White, White";
  make_format_display_test "formatted display hand of player 2 in game3_eat" 
    game3_eat 2 "7 Sticks, 8 Sticks, 9 Sticks";

  make_format_eat_test "format eat of empty list" [] "" 0 ""; 
  make_format_eat_test "format eat of 1 option" [(Stick 5, Stick 6, Stick 7)] 
    "" 1 "Combination 0 (5 Sticks, 6 Sticks, 7 Sticks)";
  make_format_eat_test "format eat of 2 options" 
    [(Stick 5, Stick 6, Stick 7); (Stick 6, Stick 7, Stick 8)] "" 2 
    ("Combination 0 (5 Sticks, 6 Sticks, 7 Sticks); Combination 1 " ^ 
     "(6 Sticks, 7 Sticks, 8 Sticks)");
  make_format_eat_test "format eat of 3 options" 
    [(Stick 5, Stick 6, Stick 7); (Stick 6, Stick 7, Stick 8); 
     (Stick 7, Stick 8, Stick 9)] "" 3 
    ("Combination 0 (5 Sticks, 6 Sticks, 7 Sticks); Combination 1 (6 Sticks," ^ 
     " 7 Sticks, 8 Sticks); Combination 2 (7 Sticks, 8 Sticks, 9 Sticks)");
]

let deck_tests = [
  make_draw_test "draw from a deck" 
    (Deck.from_tile_list [Disc 1; Disc 4; Green]) 
    (Disc 1, Deck.from_tile_list[Disc 4; Green]);
  make_draw_test "draw from deck with 1 tile" (Deck.from_tile_list [North]) 
    (North, Deck.from_tile_list[]);

  make_format_test "format Tens 1" (Tens 1) "1 Ten";
  make_format_test "format Disc 1" (Disc 1) "1 Disc";
  make_format_test "format Stick 1" (Stick 1) "1 Stick";
  make_format_test "format Tens 9" (Tens 9) "9 Tens";
  make_format_test "format Disc 8" (Disc 8) "8 Discs";
  make_format_test "format Stick 7" (Stick 7) "7 Sticks";
  make_format_test "format west" West "West";
  make_format_test "format north" North "North";
  make_format_test "format south" South "South";
  make_format_test "format east" East "East";
  make_format_test "format green" Green "Green";
  make_format_test "format red" Red "Red";
  make_format_test "format white" White "White";

  make_off_top_test "0 off top a deck" 
    (Deck.from_tile_list [Stick 6; Tens 2; Green; Red]) 0 
    (Deck.from_tile_list [Stick 6; Tens 2; Green; Red]);
  make_off_top_test "2 off top a deck" 
    (Deck.from_tile_list [Stick 6; Tens 2; Green; Red]) 2 
    (Deck.from_tile_list [Green; Red]);  
  make_off_top_test "all off top a deck" 
    (Deck.from_tile_list [Stick 6; Tens 2; Green; Red]) 4 
    (Deck.from_tile_list []);  

  make_order_test "order disc 8" (Disc 8) 8;
  make_order_test "order stick 4" (Stick 4) 13;
  make_order_test "order tens 3" (Tens 3) 21;
  make_order_test "order west" West 31;

  make_get_stick_num_test "num of Stick 7" [] (Stick 7) [7];
  make_get_stick_num_test "num of Stick 2" [9; 8] (Stick 2) [2; 9; 8];

  make_get_tens_num_test "num of Tens 5" [] (Tens 5) [5];
  make_get_tens_num_test "num of Tens 1" [1; 1; 1] (Tens 1) [1; 1; 1; 1];

  make_get_disc_num_test "num of Disc 4" [] (Disc 4) [4];
  make_get_disc_num_test "num of Disc 9" [1; 2; 3; 4; 5; 6; 7; 8] (Disc 9) 
    [9; 1; 2; 3; 4; 5; 6; 7; 8];

  make_get_num_test "num of Disc 8" (Disc 8) 8;
  make_get_num_test "num of Tens 6" (Tens 6) 6;
  make_get_num_test "num of Stick 3" (Stick 3) 3;
]

let suite = "test suite for Mahjong" >::: List.flatten [
    auto_tests;
    state_tests;
    command_tests;
    formatter_tests;
    deck_tests;
  ]

let _ = run_test_tt_main suite