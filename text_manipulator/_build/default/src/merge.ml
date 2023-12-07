[@@@warning "-32"]
[@@@warning "-27"]

open Core
open Diff_algo

module String_merge_parser = struct
  include String_parser

  let remove_unselected_content (in_ls : content_section list) (in_index : int)
      : content_section list =
    let rec remove_unselected_content_aux (checked : content_section list)
        (unchecked : content_section list) (index : int) : content_section list
        =
      match unchecked with
      | [] -> checked
      | (label, content) :: tl ->
          if Core.equal (compare_section_type label Common) 0 then
            remove_unselected_content_aux
              (checked @ [ (label, content) ])
              tl index
          else if Core.equal in_index index then
            remove_unselected_content_aux
              (checked @ [ (label, content) ])
              tl (index + 1)
          else remove_unselected_content_aux checked tl (index + 1)
    in
    remove_unselected_content_aux [] in_ls 1

  let ls2no_color_str (in_ls : content_section list) : string =
    let rec remove_unselected_content_aux (checked : string)
        (unchecked : content_section list) : string =
      match unchecked with
      | [] -> checked
      | (label, content) :: tl ->
          remove_unselected_content_aux (checked ^ content) tl
    in
    remove_unselected_content_aux "" in_ls
end

let merge_all (str1 : string) (str2 : string) : unit =
  let array1, array2 =
    ( String_process.string2word_array str1,
      String_process.string2word_array str2 )
  in
  let seq = get_coordinate_seq array1 array2 in
  let content_ls =
    String_merge_parser.coordinates_to_content_list seq array1 array2
  in
  let common_string = String_merge_parser.ls2no_color_str content_ls in
  print_string common_string

let merge_selected (str1 : string) (str2 : string) (index : int) : unit =
  let array1, array2 =
    ( String_process.string2word_array str1,
      String_process.string2word_array str2 )
  in
  let seq = get_coordinate_seq array1 array2 in
  let all_content_ls =
    String_merge_parser.coordinates_to_content_list seq array1 array2
  in
  let content_ls =
    String_merge_parser.remove_unselected_content all_content_ls index
  in
  let result_string = String_merge_parser.ls2no_color_str content_ls in
  print_string result_string
