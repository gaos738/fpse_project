(* open Core *)
open OUnit2
open Insert
open Delete
open Replace
open Undo


let insert_test_1 _ =
  insert_line := 0;
  insert_col := 0;
  insert_A := "123";
  let res = insert_string "000" in
  assert_equal res "123000"

let delete_test_1 _ =
  delete_line := 0;
  delete_col := 0;
  (* delete_A := "123"; *)
  let res = delete_string "123" in
  assert_equal res "23"

let replace_test_1 _ =
  replace_A := "123";
  replace_B := "321";
  let res = replace_string "dsfj123jkljfkldsajflkajds" in
  assert_equal res "dsfj321jkljfkldsajflkajds"


let undo_test_1 _ = 
  let input_lines = ref [] in
  let input_history = ref [] in
  update_lines ["123"] input_lines input_history;
  update_lines ["321"] input_lines input_history;
  update_lines ["456"] input_lines input_history;
  update_lines ["789"] input_lines input_history;
  let _ = undo input_lines input_history in
  assert_equal !input_lines ["456"]

let insert_tests = "insert tests" >: test_list [
  "insert1" >:: insert_test_1;
]
let delete_tests = "delete tests" >: test_list [
  "delete" >:: delete_test_1;
]
let replace_tests = "replace tests" >: test_list [
  "replace" >:: replace_test_1;
]
let undo_tests = "undo tests" >: test_list [
  "undo" >:: undo_test_1;
]



let series = "project Tests" >::: [
    insert_tests;
    delete_tests;
    replace_tests;
    undo_tests;
  ]









let () = 
  run_test_tt_main series
