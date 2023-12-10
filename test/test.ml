(* Test Plan - WHAT AND HOW DID WE TEST USING OUNIT: For out testing, we first
   tested our 'bags'. This includes our word bags which hold a random set of
   words that the user has to type and the items bags, which hold the items the
   user has.

   We also test that given a state, the associated items are functioning as
   expected. Each state has different health and time levels which we take into
   account.

   Finally, we test all the items themselves.

   For our test methodology, we used a combinating of Black Box Testing and
   Glass Box Testing. For Black Box testing, we test edge cases for the stats
   (for example what happens if our health hits 0), typical inputs, and we pair
   different initializers with the code for different items (producer consumer
   pairs). For Glass Box Testing, I also made sure to test all possible states,
   and the items associated with each state.

   WHAT PARTS WERE OUNIT TESTED VS MANUALLY TESTED: Everything stated above was
   tested by OUnit. However, there were some things that had to be manually
   tested. For example, the UI interface was all manually tested to make sure
   the user could actually type into the game and we could then check the
   "score" of what the user typed. We also had some states (for example the
   chaos state) where we had randomized stats (eg a randomized health). Because
   they were randomized we could not create OUnit tests for them and so kept
   track of scores and stats when we were in that mode or used those items
   manually. Finally, we had to randomly sample words from a word bag, which was
   also manually tested.

   WHY OUR TESTING PLAN WORKS: We used a combination of Black Box testing, Glass
   Box Testing (also stated above) and manual testing to test the critical
   aspects of our system which are the word bags, the items, associated with
   specific game modes and a functioning user interface. Testing those things
   ensures that when a user runs the game, no bugs like health being below 0, or
   level being negative occur. We use a functor to make our test cases so that
   we make sure items work for every game mode other than the chaos one, which
   is randomized and cannot be tested easily. We ensured testing coverage
   through bisect, ensuring that all items that weren't UI related or randomized
   were tested.*)

open OUnit2
module WB = TypeGame.Words.WordBag
module ItemTester = TypeGame.Items
module IBag = ItemTester.ArrayItemBag
module StateTester = TypeGame.State
module GamHard = StateTester.HardGameMutable
module GamEasy = StateTester.EasyGameMutable
module GamExtreme = StateTester.ExtremeGameMutable
module GamSudden = StateTester.SuddenDeathMutable
module GamNormal = StateTester.NormalGameMutable

(********************************************************************
  word tests
 ********************************************************************)

(* [cmp_bag_like_lists lst1 lst2] compares two lists to see whether they are
   equivalent bag-like lists. That means checking that they they contain the
   same elements with the same number of repetitions, though not necessarily in
   the same order. *)
let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

(* [cmp_bags b1 b2] compares two bags to see whether they are equivalent. *)
let cmp_bags b1 b2 =
  let lst1 = WB.to_list b1 in
  let lst2 = WB.to_list b2 in
  cmp_bag_like_lists lst1 lst2

let empty1 = WB.of_list []
let empty2 = WB.of_list []
let b1 = WB.of_list [ "1" ]
let b2 = WB.of_list [ "1"; "2"; "3" ]
let b3 = WB.of_list [ "3"; "1"; "2" ]
let b6 = WB.of_list [ "3"; "1"; "2"; "4"; "cat"; "dog" ]
let b7 = WB.of_list [ "3"; "1"; "4"; "cat"; "dog"; "2" ]

let of_list_tests =
  [
    ("of_list empty" >:: fun _ -> assert_equal ~cmp:cmp_bags empty1 empty2);
    ("of_list 1 str" >:: fun _ -> assert_equal ~cmp:cmp_bags b1 b1);
    ("of_list 3 str" >:: fun _ -> assert_equal ~cmp:cmp_bags b2 b3);
    ("of_list long" >:: fun _ -> assert_equal ~cmp:cmp_bags b6 b7);
  ]

let to_list_tests =
  [
    ( "to_list empty" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [] (WB.to_list empty1) );
    ( "to_list 1 str" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1" ] (WB.to_list b1) );
    ( "to_list 3 str" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "2"; "3" ] (WB.to_list b2) );
    ( "to_list long" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists
        [ "3"; "1"; "2"; "4"; "cat"; "dog" ]
        (WB.to_list b6) );
  ]

let join_tests =
  [
    ( "join empty" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists []
        (WB.join empty1 empty2 |> WB.to_list) );
    ( "join empty+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1" ]
        (WB.join empty1 b1 |> WB.to_list) );
    ( "join 1+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "1" ]
        (WB.join b1 b1 |> WB.to_list) );
    ( "join 3+1" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists [ "1"; "1"; "2"; "3" ]
        (WB.join b1 b2 |> WB.to_list) );
    ( "join 3+long" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists
        [ "3"; "1"; "2"; "4"; "cat"; "dog"; "1"; "2"; "3" ]
        (WB.join b2 b6 |> WB.to_list) );
  ]

(* manually tested sample becuase it randomly pics words from a bag *)

(********************************************************************
  item tests
 ********************************************************************)
module type GMString = sig
  val gm_name : string
end

module ItemTest =
functor
  (GM : TypeGame.State.GameStateMutable)
  (N : GMString)
  ->
  struct
    let apple_tests =
      [
        ( "apple_effect" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 10;
          ItemTester.apple_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 5) );
        ( "apple_effect max health" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 1;
          ItemTester.apple_effect () N.gm_name;
          assert_equal (GM.health ()) start_health );
      ]

    let banana_tests =
      [
        ( "banana_effect" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 10;
          ItemTester.banana_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 5) );
        ( "banana_effect max health" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 1;
          ItemTester.banana_effect () N.gm_name;
          assert_equal (GM.health ()) start_health );
      ]

    let broken_clock_tests =
      [
        ( "broken_clock_effect" >:: fun _ ->
          GM.initialize ();
          GM._time := 50;
          ItemTester.broken_clock_effect () N.gm_name;
          assert_equal (GM.time ()) 60 );
      ]

    let edible_clock_tests =
      [
        ( "edible_clock_effect" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 20;
          let initial_time = GM.time () in
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 10);
          assert_equal (GM.time ()) (initial_time + 15) );
        ( "edible_clock_overflow" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal start_health (GM.health ());
          assert_equal (start_time + 15) (GM.time ()) );
      ]

    let forgotten_altar_tests =
      [
        ( "initialize_forgotten" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          GM._health := start_health / 4;
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal (start_health / 4 * 2) (GM.health ());
          assert_equal (GM.time ()) (start_time / 2) );
        ( "initialize_forgotten_overflow" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health * 3 / 4;
          GM._time := 1;
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal start_health (GM.health ());
          assert_equal 0 (GM.time ()) );
      ]

    let bloody_altar_tests =
      [
        ( "initialize_bloodAltar" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health / 2);
          assert_equal (GM.time ()) (start_time * 2) );
        ( "bloody_altar_effect 1 hp" >:: fun _ ->
          GM.initialize ();
          let start_time = GM.time () in
          GM._health := 1;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal ~printer:string_of_int (GM.health ()) 1;
          assert_equal ~printer:string_of_int (GM.time ()) (start_time * 2) );
      ]

    let obfuscinator_tests =
      [
        ( "initialize_obfuscinator_Easy" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          ItemTester.obfuscinator_effect () N.gm_name;
          assert_equal (GM.health ()) (min (GM.max_health ()) start_time);
          assert_equal (GM.time ()) start_health );
        ( "initialize_obfuscinator_lowHealth" >:: fun _ ->
          GM.initialize ();
          GM._health := 50;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 25;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 12;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 6;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 3;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 1;
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) 1;
          ItemTester.obfuscinator_effect () N.gm_name;
          assert_equal (GM.health ()) (GM.max_health ());
          assert_equal (GM.time ()) 1;
          ItemTester.obfuscinator_effect () N.gm_name;
          assert_equal (GM.health ()) 1;
          assert_equal (GM.time ()) (GM.max_health ());
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) 0;
          let mid_time = GM.time () in
          ItemTester.obfuscinator_effect () N.gm_name;
          assert_equal ~printer:string_of_int (GM.health ()) mid_time;
          assert_equal (GM.time ()) 0 );
      ]

    let jet_pack_tests =
      [
        ( "initialize_jetpack" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 10) );
        ( "jetpack_effect" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 10);
          assert_equal (GM.cur_level ()) 1 );
        ( "reverse_jetpack_effect" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          ItemTester.jetpack_effect () N.gm_name;
          ItemTester.reverse_jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 20);
          assert_equal (GM.cur_level ()) 0 );
      ]

    let fryingpan_tests =
      [
        ( "initialize_fryingpan_Easy" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_max_health = GM.max_health () in
          ItemTester.big_frying_pan_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 5);
          assert_equal (GM.max_health ()) (start_max_health - 5) );
      ]

    let word_eviscer_inator_tests =
      [
        ( "initialize_eviscer" >:: fun _ ->
          GM.initialize ();
          let start_words = GM.num_words () in
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) (start_words - 4) );
        ( "eviscer_overflow" >:: fun _ ->
          GM.initialize ();
          GM._num_words := 12;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal 10 (GM.num_words ()) );
      ]

    let black_cat_tests =
      [
        ( "initialize_black_cat " >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          GM._health := start_health - 30;
          ItemTester.black_cat_trinket_effect () N.gm_name;
          assert_equal (GM.health ()) 30;
          assert_equal (GM.max_health ()) 30 );
        ( "black_cat_overflow" >:: fun _ ->
          GM.initialize ();
          ItemTester.black_cat_trinket_effect () N.gm_name;
          assert_equal (GM.health ()) 1;
          assert_equal (GM.max_health ()) 1 );
      ]

    let regression_tests =
      [
        ( "initialize_regression" >:: fun _ ->
          GM.initialize ();
          ItemTester.regression_stone_effect () N.gm_name;
          assert_equal (GM.cur_level ()) 0 );
        ( "regression_usual" >:: fun _ ->
          GM.initialize ();
          GM._cur_level := 4;
          ItemTester.regression_stone_effect () N.gm_name;
          assert_equal ~printer:string_of_int (GM.cur_level ()) 2 );
      ]

    let clarity_tests =
      [
        ( "initialize_clarity" >:: fun _ ->
          GM.initialize ();
          let start_time = GM.time () in
          ItemTester.crystal_of_clarity_effect () N.gm_name;
          assert_equal ~printer:string_of_int (GM.time ()) (start_time - 5) );
        ( "clarity overflow test" >:: fun _ ->
          GM.initialize ();
          GM._time := 3;
          ItemTester.crystal_of_clarity_effect () N.gm_name;
          assert_equal (GM.time ()) 1 );
      ]

    let edge_tests =
      [
        ( "max_health + 5, losing health and changing level" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          ItemTester.banana_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 10);
          assert_equal (GM.cur_level ()) 1;
          ItemTester.reverse_jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (start_health - 20);
          assert_equal (GM.cur_level ()) 0 );
        ( "0 health , changing level" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) (max (start_health / 2) 1);
          assert_equal (GM.time ()) (start_time * 2);
          ItemTester.bloody_altar_effect () N.gm_name;
          assert_equal (GM.health ()) (max (start_health / 4) 1);
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (max ((start_health / 4) - 10) 0);
          assert_equal (GM.cur_level ()) 1;
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) (max ((start_health / 4) - 20) 0);
          assert_equal (GM.cur_level ()) 2;
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.health ()) 0;
          assert_equal (GM.cur_level ()) 3 );
        ( "too much health, swapping health and time " >:: fun _ ->
          GM.initialize ();
          let start_time = GM.time () in
          let start_health = GM.health () in
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (start_time + 15);
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (start_time + 30);
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (start_time + 45);
          ItemTester.obfuscinator_effect () N.gm_name;
          assert_equal (GM.health ()) (GM.max_health ());
          assert_equal (GM.time ()) start_health );
        ( "dropping time " >:: fun _ ->
          GM.initialize ();
          let start_time = GM.time () in
          let start_health = GM.health () in
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (max (start_time / 2) 0);
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (max (start_time / 4) 0);
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (max (start_time / 8) 0);
          ItemTester.forgotton_altar_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (max (start_time / 16) 0);
          ItemTester.broken_clock_effect () N.gm_name;
          assert_equal (GM.health ()) start_health;
          assert_equal (GM.time ()) (max (start_time / 16) 0 + 10) );
        ( "dropping number of words " >:: fun _ ->
          GM.initialize ();
          GM._num_words := 50;
          assert_equal (GM.num_words ()) 50;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 46;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 42;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 38;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 34;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 30;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 26;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 22;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 18;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 14;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 10;
          ItemTester.word_eviscer_inator_effect () N.gm_name;
          assert_equal (GM.num_words ()) 10 );
        ( "attempting level below 0" >:: fun _ ->
          GM.initialize ();
          let start_health = GM.health () in
          let start_time = GM.time () in
          assert_equal (GM.cur_level ()) 0;
          ItemTester.reverse_jetpack_effect () N.gm_name;
          assert_equal (GM.cur_level ()) 0;
          assert_equal (GM.health ()) (max (start_health - 10) 0);
          ItemTester.regression_stone_effect () N.gm_name;
          assert_equal ~printer:string_of_int (GM.cur_level ()) 0;
          ItemTester.jetpack_effect () N.gm_name;
          assert_equal (GM.cur_level ()) 1;
          assert_equal (GM.health ()) (max (start_health - 20) 0);
          assert_equal (GM.time ()) start_time;
          ItemTester.regression_stone_effect () N.gm_name;
          assert_equal (GM.cur_level ()) 0;
          let mid_time = GM.time () in
          ItemTester.edible_clock_effect () N.gm_name;
          assert_equal (GM.health ()) (max (start_health - 10) 0);
          assert_equal (GM.time ()) (mid_time + 15);
          ItemTester.apple_effect () N.gm_name;
          assert_equal (GM.health ()) (max (start_health - 5) 0);
          ItemTester.banana_effect () N.gm_name;
          assert_equal (GM.health ()) start_health );
      ]
  end

module EasyString : GMString = struct
  let gm_name = "Easy"
end

module NormalString : GMString = struct
  let gm_name = "Normal"
end

module HardString : GMString = struct
  let gm_name = "Hard"
end

module ExtremeString : GMString = struct
  let gm_name = "Extreme"
end

module SuddenDeathString : GMString = struct
  let gm_name = "Sudden Death"
end

module EasyTest = ItemTest (GamEasy) (EasyString)
module NormalTest = ItemTest (GamNormal) (NormalString)
module HardTest = ItemTest (GamHard) (HardString)
module ExtremeTest = ItemTest (GamExtreme) (ExtremeString)
module SuddenDeathTest = ItemTest (GamSudden) (SuddenDeathString)

(* manually tested chaos effect, what items get chosen, chaos items (item has a
   random effect) *)
let word_tests = to_list_tests @ of_list_tests @ join_tests

let item_tests =
  EasyTest.edge_tests @ EasyTest.apple_tests @ EasyTest.banana_tests
  @ EasyTest.broken_clock_tests @ EasyTest.edible_clock_tests
  @ EasyTest.forgotten_altar_tests @ EasyTest.bloody_altar_tests
  @ EasyTest.obfuscinator_tests @ EasyTest.jet_pack_tests
  @ EasyTest.clarity_tests @ EasyTest.fryingpan_tests
  @ EasyTest.word_eviscer_inator_tests @ EasyTest.black_cat_tests
  @ EasyTest.regression_tests @ NormalTest.edge_tests @ NormalTest.apple_tests
  @ NormalTest.banana_tests @ NormalTest.clarity_tests
  @ NormalTest.broken_clock_tests @ NormalTest.edible_clock_tests
  @ NormalTest.forgotten_altar_tests @ NormalTest.bloody_altar_tests
  @ NormalTest.obfuscinator_tests @ NormalTest.jet_pack_tests
  @ NormalTest.fryingpan_tests @ NormalTest.word_eviscer_inator_tests
  @ NormalTest.black_cat_tests @ NormalTest.regression_tests
  @ HardTest.edge_tests @ HardTest.apple_tests @ HardTest.banana_tests
  @ HardTest.clarity_tests @ HardTest.broken_clock_tests
  @ HardTest.edible_clock_tests @ HardTest.forgotten_altar_tests
  @ HardTest.bloody_altar_tests @ HardTest.obfuscinator_tests
  @ HardTest.jet_pack_tests @ HardTest.fryingpan_tests
  @ HardTest.word_eviscer_inator_tests @ HardTest.black_cat_tests
  @ HardTest.regression_tests @ ExtremeTest.edge_tests @ ExtremeTest.apple_tests
  @ ExtremeTest.banana_tests @ ExtremeTest.clarity_tests
  @ ExtremeTest.broken_clock_tests @ ExtremeTest.edible_clock_tests
  @ ExtremeTest.forgotten_altar_tests @ ExtremeTest.bloody_altar_tests
  @ ExtremeTest.obfuscinator_tests @ ExtremeTest.jet_pack_tests
  @ ExtremeTest.fryingpan_tests @ ExtremeTest.word_eviscer_inator_tests
  @ ExtremeTest.black_cat_tests @ ExtremeTest.regression_tests
  @ SuddenDeathTest.edge_tests @ SuddenDeathTest.apple_tests
  @ SuddenDeathTest.banana_tests @ SuddenDeathTest.broken_clock_tests
  @ SuddenDeathTest.clarity_tests @ SuddenDeathTest.edible_clock_tests
  @ SuddenDeathTest.forgotten_altar_tests @ SuddenDeathTest.bloody_altar_tests
  @ SuddenDeathTest.obfuscinator_tests @ SuddenDeathTest.jet_pack_tests
  @ SuddenDeathTest.fryingpan_tests @ SuddenDeathTest.word_eviscer_inator_tests
  @ SuddenDeathTest.black_cat_tests @ SuddenDeathTest.regression_tests

let tests = "test suite" >::: word_tests @ item_tests
let () = run_test_tt_main tests
