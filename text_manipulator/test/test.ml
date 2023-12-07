[@@@warning "-33"]

open Core
open OUnit2
open Merge
open Diff_algo

(*Diff tests*)
let string_parser_test _ =
  assert_equal [ (Diff_algo.String_parser.Common, "bc") ]
  @@ String_parser.coordinates_to_content_list
       [ (1, 1); (2, 2); (3, 3) ]
       [| "a"; "b"; "c" |] [| "d"; "e"; "f" |];
  assert_equal
    [
      (Diff_algo.String_parser.Add, "e");
      (Diff_algo.String_parser.Del, "b");
      (Diff_algo.String_parser.Common, "c");
    ]
  @@ String_parser.coordinates_to_content_list
       [ (1, 1); (1, 2); (2, 2); (3, 3) ]
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
       [| "ab "; "b "; "d " |] (0, 0);
  assert_equal false
  @@ String_process.str_equal_at_index [| "ab"; "b"; "c" |]
       [| "ab "; "b "; "d " |] (2, 2)

let diff_tests =
  "Diff test"
  >::: [
         "string parser test" >:: string_parser_test;
         "string process test" >:: string_process_test;
       ]

let series = "fpse project Tests" >::: [ diff_tests ]
let () = run_test_tt_main series
