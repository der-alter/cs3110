open OUnit2
open Sum
open Calendar
open Exercises

let make_sum_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (sum input) ~printer:string_of_int

let make_next_weekday_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (next_weekday input)

let make_product_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (product input) ~printer:string_of_int

let make_fifth_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (fifth input) ~printer:string_of_int

let make_sort_desc_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (sort_desc input)

let tests =
  "test suite for sum"
  >::: [
         make_sum_test "empty" 0 [];
         make_sum_test "singleton" 1 [1];
         make_sum_test "two_elements" 3 [1; 2];
         make_next_weekday_test "mon_after_fri" Monday Friday;
         make_next_weekday_test "mon_after_sat" Monday Saturday;
         make_next_weekday_test "mon_after_sun" Monday Sunday;
         make_next_weekday_test "tue_after_mon" Tuesday Monday;
         make_next_weekday_test "wed_after_tue" Wednesday Tuesday;
         make_next_weekday_test "thu_after_wed" Thursday Wednesday;
         make_next_weekday_test "fri_after_thu" Friday Thursday;
         make_product_test "empty" 1 [];
         make_product_test "single" 4 [4];
         make_product_test "two" 10 [5; 2];
         make_fifth_test "empty" 0 [];
         make_fifth_test "fifth" 6 [1; 2; 2; 4; 6];
         make_sort_desc_test "empty" [] [];
         make_sort_desc_test "empty" [3;2;1] [1;2;3];
       ]

let _ = run_test_tt_main tests
