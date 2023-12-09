open OUnit2
module WB = TypeGame.Words.WordBag
module ItemTester = TypeGame.Items
module IBag = ItemTester.ArrayItemBag
module StateTester = TypeGame.State
module GamHard = StateTester.HardGameMutable
module GamEasy = StateTester.EasyGameMutable
module GamExtreme = StateTester.ExtremeGameMutable
module GamSudden = StateTester.SuddenDeathMutable

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

(* TODO: add sample tests *)

let item_tests =
  [
    ( "initialize_jetpack" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 90 );
    ( "initialize_bloodAltar" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.bloody_altar_effect () "Hard";
      assert_equal (GamHard.health ()) 50 );
    ( "max_health + 5, losing health and changing level" >:: fun _ ->
      GamHard.initialize ();
      ItemTester.banana_effect () "Hard";
      assert_equal (GamHard.health ()) 100;
      ItemTester.jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 90;
      assert_equal (GamHard.cur_level ()) 1;
      ItemTester.reverse_jetpack_effect () "Hard";
      assert_equal (GamHard.health ()) 80;
      assert_equal (GamHard.cur_level ()) 0 );
    ( "0 health " >:: fun _ ->
      GamEasy.initialize ();
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 50;
      assert_equal (GamEasy.time ()) 240;
      ItemTester.bloody_altar_effect () "Easy";
      assert_equal (GamEasy.health ()) 25;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 15;
      assert_equal (GamEasy.cur_level ()) 1;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) 5;
      assert_equal (GamEasy.cur_level ()) 2;
      ItemTester.jetpack_effect () "Easy";
      assert_equal (GamEasy.health ()) ~-5;
      assert_equal (GamEasy.cur_level ()) 3 );
    ( "too much health " >:: fun _ ->
      GamExtreme.initialize ();
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 70;
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 80;
      ItemTester.edible_clock_effect () "Extreme";
      assert_equal (GamExtreme.health ()) 50;
      assert_equal (GamExtreme.time ()) 90;
      ItemTester.obfuscinator_effect () "Extreme";
      print_endline (string_of_int (GamExtreme.health ()) ^ "im dead");
      assert_equal (GamExtreme.time ()) 50;
      assert_equal (GamExtreme.health ()) 50 );
    ( "dropping time " >:: fun _ ->
      GamSudden.initialize ();
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 45;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 22;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 11;
      ItemTester.forgotton_altar_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 5;
      ItemTester.broken_clock_effect () "Sudden Death";
      assert_equal (GamSudden.health ()) 100;
      assert_equal (GamSudden.time ()) 15 );
  ]

let test_list = to_list_tests @ of_list_tests @ join_tests @ item_tests
let tests = "test suite" >::: test_list
let () = run_test_tt_main tests