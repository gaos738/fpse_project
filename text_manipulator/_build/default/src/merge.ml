(*
Introduction:
This file is based on diff_algo with some modification. See diff_algo to get implementation detail
*)

[@@@warning "-32"]
[@@@warning "-27"]

open Core
open Diff_algo
open File_parse

module String_merge_parser = struct
  include String_parser

  (* Convert list of string with label to string with no color *)
  let ls2no_color_str (in_ls : content_section list) : string =
    let rec ls2no_color_str_aux (unchecked : content_section list)
        (checked : string) : string =
      match unchecked with
      | [] -> checked
      | (t, v) :: tl -> (
          match t with
          | Common -> ls2no_color_str_aux tl (checked ^ v)
          | Del -> ls2no_color_str_aux tl (checked ^ v)
          | Add -> ls2no_color_str_aux tl (checked ^ v)
          | N -> assert false)
    in
    ls2no_color_str_aux in_ls ""
end

module Graph_search_for_merge = struct
  include Graph_search
  include File_struct

  (* Merge all differences in two files into one file *)
  let merge_all (str1 : string_file_content) (str2 : string_file_content) :
      string =
    let array1, array2 =
      ( String_process.string2word_array (content2string str1 ^ " "),
        String_process.string2word_array (content2string str2 ^ " ") )
    in
    let seq = get_coordinate_seq array1 array2 in
    let content_ls =
      String_merge_parser.coordinates_to_content_list seq array1 array2
    in
    let common_string = String_merge_parser.ls2no_color_str content_ls in
    common_string
  (* Display merged result, but return nothing *)

  let disp_merge_all (filename1 : filename) (filename2 : filename) : unit =
    let file_content1 = File_struct.get_string_content filename1 ^ " " in
    let file_content2 = File_struct.get_string_content filename2 ^ " " in
    print_string
      (merge_all (string2content file_content1) (string2content file_content2))
end