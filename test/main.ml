open OUnit2
module WB = TypeGame.Game.WordBag
module ItemTester = TypeGame.Items
module IBag = ItemTester.ArrayItemBag
module StateTester = TypeGame.State
module Gam = StateTester.HardGameMutable

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
    ( "initialize" >:: fun _ ->
      Gam.initialize ();
      ItemTester.jetpack_effect () "Hard";
      assert_equal (Gam.health ()) 90 );
    ( "initialize" >:: fun _ ->
      Gam.initialize ();
      ItemTester.bloody_altar_effect () "Hard";
      assert_equal (Gam.health ()) 50 );
  ]

let test_list = to_list_tests @ of_list_tests @ join_tests @ item_tests
let tests = "test suite" >::: test_list
let () = run_test_tt_main tests
