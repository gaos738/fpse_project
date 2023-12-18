(* open Core *)
open OUnit2
open Basic_feature
open Merge
open Diff_algo
open File_parse

let insert_test_1 _ =
  insert_line := 0;
  insert_col := 0;
  insert_A := "123";
  let res = insert_string "000" in
  assert_equal res "123000"

let insert_test_beyond _ =
  insert_col := 8;
  insert_A := "DEF";
  let res = insert_string "01234" in
  assert_equal res "01234   DEF"

let delete_test_1 _ =
  delete_line := 0;
  delete_col := 0;
  (* delete_A := "123"; *)
  let res = delete_string "123" in
  assert_equal res "23"

let delete_test_beyond _ =
  delete_col := 10;
  let res = delete_string "abcde" in
  assert_equal res "abcde"

let replace_test_parts _ =
  replace_A := "123";
  replace_B := "321";
  let res = replace_part "dsfj123jkljfkldsajflkajds" in
  assert_equal res "dsfj321jkljfkldsajflkajds"

let replace_test_words _ =
  replace_A := "on";
  replace_B := "love";
  let res = replace_string "on one money only" in
  assert_equal res "love one money only"

let undo_test_1 _ =
  let input_lines = ref [] in
  let input_history = ref [] in
  update_lines [ "123" ] input_lines input_history;
  update_lines [ "321" ] input_lines input_history;
  update_lines [ "456" ] input_lines input_history;
  update_lines [ "789" ] input_lines input_history;
  let _ = undo input_lines input_history in
  assert_equal !input_lines [ "456" ]

let undo_test_multiple _ =
  let input_lines = ref [] in
  let input_history = ref [] in
  update_lines [ "A" ] input_lines input_history;
  update_lines [ "B" ] input_lines input_history;
  update_lines [ "C" ] input_lines input_history;
  let _ = undo input_lines input_history in
  (* Reverts to ["B"] *)
  let _ = undo input_lines input_history in
  (* Reverts to ["A"] *)
  assert_equal !input_lines [ "A" ]

let undo_test_no_operation _ =
  let input_lines = ref [ "No changes" ] in
  let input_history = ref [] in
  let _ = undo input_lines input_history in
  (* No updates to undo *)
  assert_equal !input_lines [ "No changes" ]

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
       [| "ab "; "b "; "d " |]
       (int_tuple2word_coordinate (0, 0));
  assert_equal false
  @@ String_process.str_equal_at_index [| "ab"; "b"; "c" |]
       [| "ab "; "b "; "d " |]
       (int_tuple2word_coordinate (2, 2));
  assert_equal ("a", "bcd") @@ String_process.string_hd "abcd";
  assert_equal [| " "; "a "; "b "; "c" |]
  @@ String_process.string2word_array " a b c";
  assert_equal [| " "; "i "; "am "; "groot "; "! " |]
  @@ String_process.string2word_array " i am groot ! "

let diff_test _ =
  assert_equal "\027[31m a \027[32m b "
  @@ Graph_search.get_diff
       (Graph_search.string2content " a ")
       (Graph_search.string2content " b ");
  assert_equal "\027[31ma \027[30mb \027[32mc \027[30md "
  @@ Graph_search.get_diff
       (Graph_search.string2content "a b d ")
       (Graph_search.string2content "b c d ");
  assert_equal "\027[31ma \027[30ma a "
  @@ Graph_search.get_diff
       (Graph_search.string2content "a a a ")
       (Graph_search.string2content "a a ");
  assert_equal "\027[31m \027[32m \027[30mi am \027[32mnot \027[30mgroot "
  @@ Graph_search.get_diff
       (Graph_search.string2content " i am groot ")
       (Graph_search.string2content " i am not groot ");
  assert_equal "\027[32m a \027[31m \027[30mb \027[32ma \027[30mb "
  @@ Graph_search.get_diff
       (Graph_search.string2content " b b ")
       (Graph_search.string2content " a b a b ")

let merge_test _ =
  let sample1 =
    "let rm_whitespace (str : string) (_ : string) : string =\n\
    \      String.filter str ~f:(fun char -> (Char.equal char ' '))"
  in
  let sample2 =
    "let rm_whitespace (str : string) : string =\n\
    \    String.filter str ~f:(fun char -> not (Char.equal char ' '))"
  in
  let result =
    "let rm_whitespace (str : string) (_ : string) : string =\n\
    \      String.filter str ~f:(fun char -> not (Char.equal char ' ')) "
  in

  assert_equal "a b c d "
  @@ Graph_search_for_merge.merge_all
       (Graph_search_for_merge.string2content "a b d")
       (Graph_search_for_merge.string2content "b c d");
  assert_equal "a b c d f s "
  @@ Graph_search_for_merge.merge_all
       (Graph_search_for_merge.string2content "a b d s")
       (Graph_search_for_merge.string2content "b c d f s");
  assert_equal "i am not groot "
  @@ Graph_search_for_merge.merge_all
       (Graph_search_for_merge.string2content "i am groot")
       (Graph_search_for_merge.string2content "i am not groot");
  assert_equal result
  @@ Graph_search_for_merge.merge_all
       (Graph_search_for_merge.string2content sample1)
       (Graph_search_for_merge.string2content sample2)

let file_parse_test _ =
  let file_content_in_list =
    [
      "(* Ocaml test file 1 *)";
      "";
      "let rec rev l =";
      "  match l with";
      "  |  [] -> []";
      "  |  hd :: tl -> rev tl @ [hd];;";
      "";
      "let rec zero_out_all_negs l =";
      "  match l with";
      "  |  [] -> []";
      "  |  hd :: tl -> (if hd < 0 then 0 else hd) :: zero_out_all_negs tl;;";
    ]
  in
  let file_content =
    "(* Ocaml test file 1 *) \n\
    \ \n\
     let rec rev l = \n\
    \  match l with \n\
    \  |  [] -> [] \n\
    \  |  hd :: tl -> rev tl @ [hd];; \n\
    \ \n\
     let rec zero_out_all_negs l = \n\
    \  match l with \n\
    \  |  [] -> [] \n\
    \  |  hd :: tl -> (if hd < 0 then 0 else hd) :: zero_out_all_negs tl;; \n"
  in
  let filename =
    File_struct.string2filename
      "/home/shang/fpse/final_version/fpse_project/text_manipulator/tests/ocaml_pair_1.ml"
  in
  assert_equal "  2" @@ File_struct.get_line_number 2;
  assert_equal file_content_in_list
  @@ File_struct.get_string_ls_content filename;
  assert_equal file_content
  @@ File_struct.ls2str (File_struct.file2string_ls filename)

let test_create_file _ =
  let test_path = "test_file_ocaml.txt" in
  create_file test_path;
  let file_exists = Sys.file_exists test_path in
  assert_equal true file_exists ~msg:"File does not exist";
  (* Cleanup: Delete the file after testing *)
  Sys.remove test_path

let test_write_file _ =
  let test_path = "test_write_read_ocaml.txt" in
  let test_content = [ "Line 1"; "Line 2"; "Line 3" ] in
  create_file test_path;
  write_to_file test_path test_content;
  let test_path_ref = ref test_path in
  let read_content = read test_path_ref in
  assert_equal test_content read_content
    ~msg:"Read content does not match written content";
  Sys.remove test_path

let insert_tests = "insert tests" >: test_list [ "insert1" >:: insert_test_1 ]

let insert_tests_2 =
  "insert tests beyond"
  >: test_list [ "insert_test_beyond" >:: insert_test_beyond ]

let delete_tests = "delete tests" >: test_list [ "delete" >:: delete_test_1 ]

let delete_test_beyond =
  "delete test beyond"
  >: test_list [ "delete_test_beyond" >:: delete_test_beyond ]

let replace_tests =
  "replace tests parts" >: test_list [ "replace" >:: replace_test_parts ]

let replace_tests_2 =
  "replace tests words" >: test_list [ "replace_words" >:: replace_test_words ]

let undo_tests = "undo tests" >: test_list [ "undo" >:: undo_test_1 ]

let undo_multi =
  "undo_multi" >: test_list [ "undo_multi" >:: undo_test_multiple ]

let undo_no_action =
  "undo_no_action" >: test_list [ "undo_no_action" >:: undo_test_no_operation ]

let file_parse_test =
  "File parse test" >: test_list [ "File parse test" >:: file_parse_test ]

let diff_tests =
  "Diff test"
  >::: [
         "string parser test" >:: string_parser_test;
         "string process test" >:: string_process_test;
         "diff test" >:: diff_test;
       ]

let merge_tests = "Merge test" >::: [ "Merge test" >:: merge_test ]

let test_create =
  "test create file" >: test_list [ "test create file" >:: test_create_file ]

let test_write =
  "test write and read"
  >: test_list [ "test write and read" >:: test_write_file ]

let series =
  "project Tests"
  >::: [
         insert_tests;
         insert_tests_2;
         delete_tests;
         delete_test_beyond;
         replace_tests;
         replace_tests_2;
         undo_tests;
         undo_multi;
         undo_no_action;
         diff_tests;
         merge_tests;
         file_parse_test;
         test_create;
         test_write;
       ]

let () = run_test_tt_main series