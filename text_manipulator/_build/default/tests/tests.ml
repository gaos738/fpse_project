(* open Core *)
open OUnit2
open Insert
open Delete
open Replace
open Undo
open Merge
open Diff_algo


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

(*Diff tests*)
let string_parser_test _ =
  assert_equal [ (Diff_algo.String_parser.Common, "bc") ]
  @@ String_parser.coordinates_to_content_list
      (int_tuple_ls2word_coordinate_ls [ (1, 1); (2, 2); (3, 3) ])
       [| "a"; "b"; "c" |] [| "d"; "e"; "f" |];
  assert_equal
    [
      (Diff_algo.String_parser.Add, "e");
      (Diff_algo.String_parser.Del, "b");
      (Diff_algo.String_parser.Common, "c");
    ]
  @@ String_parser.coordinates_to_content_list
    (int_tuple_ls2word_coordinate_ls [ (1, 1); (1, 2); (2, 2); (3, 3) ])
       [| "a"; "b"; "c"; "d" |] [| "d"; "e"; "f"; "g" |];
  assert_equal "\027[32me\027[31mb\027[30mc"
  @@ String_parser.sect_ls2string
       [
         (Diff_algo.String_parser.Add, "e");
         (Diff_algo.String_parser.Del, "b");
         (Diff_algo.String_parser.Common, "c");
       ]

let string_process_test _ =
  assert_equal "aaaa" @@ String_process.rm_whitespace " a a a a ";
  assert_equal true
  @@ String_process.str_equal_at_index [| "ab"; "b"; "c" |]
       [| "ab "; "b "; "d " |] (int_tuple2word_coordinate (0, 0));
  assert_equal false
  @@ String_process.str_equal_at_index [| "ab"; "b"; "c" |]
       [| "ab "; "b "; "d " |] (int_tuple2word_coordinate(2, 2));
  assert_equal ("a", "bcd") @@ String_process.string_hd "abcd";
  assert_equal [| " "; "a "; "b "; "c" |]
  @@ String_process.string2word_array " a b c";
  assert_equal [| " "; "i "; "am "; "groot "; "! " |]
  @@ String_process.string2word_array " i am groot ! "

let diff_test _ =
  assert_equal "\027[31m a \027[32m b " @@ Graph_search.get_diff (Graph_search.string2content " a ") (Graph_search.string2content " b ");
  assert_equal "\027[31ma \027[30mb \027[32mc \027[30md " @@ Graph_search.get_diff (Graph_search.string2content "a b d ") (Graph_search.string2content "b c d ");
  assert_equal "\027[31ma \027[30ma a " @@ Graph_search.get_diff (Graph_search.string2content "a a a ") (Graph_search.string2content "a a ");
  assert_equal "\027[31m \027[32m \027[30mi am \027[32mnot \027[30mgroot " @@ Graph_search.get_diff (Graph_search.string2content " i am groot ") (Graph_search.string2content " i am not groot ");
  assert_equal "\027[32m a \027[31m \027[30mb \027[32ma \027[30mb " @@ Graph_search.get_diff (Graph_search.string2content " b b ") (Graph_search.string2content " a b a b ")


let merge_test _ =
  assert_equal "a b c d " @@ Graph_search_for_merge.merge_all (Graph_search_for_merge.string2content "a b d ") (Graph_search_for_merge.string2content "b c d ");
  assert_equal "a b c d f s " @@ Graph_search_for_merge.merge_all (Graph_search_for_merge.string2content "a b d s ") (Graph_search_for_merge.string2content "b c d f s ");
  assert_equal "i am not groot " @@ Graph_search_for_merge.merge_all (Graph_search_for_merge.string2content "i am groot ") (Graph_search_for_merge.string2content "i am not groot ");
  assert_equal "a b " @@ Graph_search_for_merge.merge_selected (Graph_search_for_merge.string2content "a b d ") (Graph_search_for_merge.string2content "b c ") 1;
  assert_equal "b d " @@ Graph_search_for_merge.merge_selected (Graph_search_for_merge.string2content "a b d ") (Graph_search_for_merge.string2content "b c ") 2





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

let diff_tests =
  "Diff test"
  >::: [
         "string parser test" >:: string_parser_test;
         "string process test" >:: string_process_test;
         "diff test" >:: diff_test;
       ]

let merge_tests =
  "Merge test"
  >::: [
         "Merge test" >:: merge_test;
       ]



let series = "project Tests" >::: [
    insert_tests;
    delete_tests;
    replace_tests;
    undo_tests;
    diff_tests;
    merge_tests;
  ]

let () = 
  run_test_tt_main series
