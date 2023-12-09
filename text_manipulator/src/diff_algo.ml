(*
Introduction:
This file is implementing an minimum number of edits based method to find diff.
It uses a 2D graph to search for shortest path from point (0,0) to (lenght of string 1, length of string 2)
Reference:https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/ 
*)

[@@@warning "-32"]
[@@@warning "-27"]

open Core
open Display

(*BFS traverse for finding smallest edit path*)
type bfs_node =
  | Null
  | Node of {
      coordinate : int * int;
      parent : bfs_node;
      path : (int * int) list;
    }

module String_parser = struct
  type section_type = Del | Common | Add | N [@@deriving compare]
  type content_section = section_type * string

  let match_label_string (curr : int * int) (prev : int * int)
      (sect_ls : content_section list) (str1 : string array)
      (str2 : string array) : content_section list =
    let x, y = curr in
    let x_prev, y_prev = prev in
    if Core.equal x (x_prev + 1) && Core.equal y (y_prev + 1) then
      let curr_str = Array.get str1 (x - 1) in
      sect_ls @ [ (Common, curr_str) ]
    else if Core.equal x (x_prev + 1) && Core.equal y y_prev then
      let curr_str = Array.get str1 (x - 1) in
      sect_ls @ [ (Del, curr_str) ]
    else if Core.equal x x_prev && Core.equal y (y_prev + 1) then
      let curr_str = Array.get str2 (y - 1) in
      sect_ls @ [ (Add, curr_str) ]
    else failwith "expected string"

  let sanitize (sec_ls : content_section list) : content_section list =
    let rec sanitize_aux (checked : content_section list)
        (unchecked : content_section list) : content_section list =
      match unchecked with
      | [] -> checked
      | (t, v) :: tl ->
          if String.equal v "" then sanitize_aux checked tl
          else sanitize_aux (checked @ [ (t, v) ]) tl
    in
    sanitize_aux [] sec_ls

  let combine_section (sec_ls : content_section list) : content_section list =
    let rec combine_section_aux (checked : content_section list)
        (unchecked : content_section list) (last : content_section) :
        content_section list =
      let last_type, last_value = last in
      match unchecked with
      | [] -> checked @ [ last ]
      | (t, v) :: tl ->
          if Core.equal (compare_section_type t last_type) 0 then
            combine_section_aux checked tl (last_type, last_value ^ v)
          else combine_section_aux (checked @ [ last ]) tl (t, v)
    in
    combine_section_aux [] sec_ls (N, "")

  let coordinates_to_content_list (ls_in : (int * int) list)
      (sample_str1_in : string array) (sample_str2_in : string array) :
      content_section list =
    let rec aux (ls : (int * int) list) (sample_str1 : string array)
        (sample_str2 : string array) (result_ls : content_section list) :
        content_section list =
      match ls with
      | (x, y) :: (x_next, y_next) :: tl ->
          let curr_ls =
            match_label_string (x_next, y_next) (x, y) result_ls sample_str1
              sample_str2
          in
          aux ((x_next, y_next) :: tl) sample_str1 sample_str2 curr_ls
      | _ -> result_ls
    in
    let unsanitized = aux ls_in sample_str1_in sample_str2_in [] in
    unsanitized |> combine_section |> sanitize

  let sect_ls2string (ls_in : content_section list) : string =
    let rec sect_ls2string_aux (unchecked : content_section list)
        (checked : string) : string =
      match unchecked with
      | [] -> checked
      | (t, v) :: tl -> (
          match t with
          | Common -> sect_ls2string_aux tl (checked ^ "\027[30m" ^ v)
          | Del -> sect_ls2string_aux tl (checked ^ "\027[31m" ^ v)
          | Add -> sect_ls2string_aux tl (checked ^ "\027[32m" ^ v)
          | N -> failwith "invalid type")
    in
    sect_ls2string_aux ls_in ""
end

module String_process = struct
  let rm_whitespace (str : string) : string =
    String.filter str ~f:(fun char -> not (Char.equal char ' '))

  let str_equal_at_index (str1 : string array) (str2 : string array)
      (coordinate : int * int) : bool =
    let x, y = coordinate in
    let x_content = rm_whitespace (Array.get str1 x) in
    let y_content = rm_whitespace (Array.get str2 y) in
    String.equal x_content y_content

  let string_hd (input_str : string) : string * string =
    let str_hd = String.sub input_str ~pos:0 ~len:1 in
    let str_tl =
      String.sub input_str ~pos:1 ~len:(String.length input_str - 1)
    in
    (str_hd, str_tl)

  let rec string2word_list (input_str : string) (curr_ls : string list)
      (curr_word : string) (last_ws : bool) : string list =
    match String.equal input_str "" with
    | true -> curr_word :: curr_ls
    | false -> (
        let str_hd, str_tl = string_hd input_str in
        let is_whitespace = String.equal str_hd " " in
        match phys_equal last_ws true && phys_equal is_whitespace false with
        | true ->
            string2word_list str_tl (curr_word :: curr_ls) str_hd is_whitespace
        | false ->
            string2word_list str_tl curr_ls (curr_word ^ str_hd) is_whitespace)

  let string2word_array (input_l : string) : string array =
    let string_ls = string2word_list input_l [] "" true in
    let reverse = List.rev string_ls in
    Array.of_list reverse
    (*match reverse with [] -> assert false | hd :: tl -> Array.of_list tl*)
end

(*Check if the graph serarch should stop*)
let is_last_point ~(str1 : string array) ~(str2 : string array)
    ~(node : bfs_node) : bool =
  match node with
  | Node { coordinate = x, y; parent = _; path = _ } ->
      if Core.equal x (Array.length str1) && Core.equal y (Array.length str2)
      then true
      else false
  | Null -> assert false

(*Check if the graph serarch should stop at current layer*)
let stop_at_layer (str1 : string array) (str2 : string array)
    (dfs_tree : bfs_node list list) : bool =
  match List.hd dfs_tree with
  | None -> false
  | Some layer ->
      List.fold
        ~f:(fun acc point ->
          if is_last_point ~str1 ~str2 ~node:point then true else acc || false)
        ~init:false layer

(*Check if a point in graph is valid(inside the graph)*)
let valid_point (str1 : string array) (str2 : string array)
    (coordinate : int * int) : bool =
  let x, y = coordinate in
  Array.length str1 >= x && x >= 0 && Array.length str2 >= y && y >= 0

(*Check if a point can go diagonal path in the graph*)
let rec diagonal_path (str1 : string array) (str2 : string array)
    (coordinate : int * int) (path : (int * int) list) :
    (int * int) * (int * int) list =
  let x, y = coordinate in
  if
    valid_point str1 str2 (x + 1, y + 1)
    && String_process.str_equal_at_index str1 str2 (x, y)
  then diagonal_path str1 str2 (x + 1, y + 1) ((x, y) :: path)
  else (coordinate, path)

let add_map (hashtable : (string, int) Hashtbl_intf.Hashtbl.t ref) (k : string)
    (v : int) : unit =
  match Hashtbl.add !hashtable ~key:k ~data:v with
  | `Duplicate -> ()
  | `Ok -> ()

(*Append the valid point into current BFS layer*)
let append_point (str1 : string array) (str2 : string array)
    (common_parent : bfs_node) (coordinate : int * int)
    (hashtable : (string, int) Hashtbl_intf.Hashtbl.t ref) : bfs_node option =
  let x, y = coordinate in
  match valid_point str1 str2 (x, y) with
  | true -> (
      let coordinate_new, path_new = diagonal_path str1 str2 coordinate [] in
      let new_point =
        Node
          {
            coordinate = coordinate_new;
            parent = common_parent;
            path = path_new;
          }
      in
      let x, y = coordinate_new in
      let key = string_of_int x ^ "," ^ string_of_int y in
      match Hashtbl.find !hashtable key with
      | Some _ -> None
      | None ->
          add_map hashtable key 1;
          Some new_point)
  | false -> None

 (*Search for next valid point into current BFS layer*)
let search_next_point (str1 : string array) (str2 : string array)
    (common_parent : bfs_node) (next_step_ls : bfs_node list)
    (hashtable : (string, int) Hashtbl_intf.Hashtbl.t ref) : bfs_node list =
  match common_parent with
  | Null -> assert false
  | Node { coordinate = x, y; parent = _; path = _ } -> (
      match
        ( append_point str1 str2 common_parent (x + 1, y) hashtable,
          append_point str1 str2 common_parent (x, y + 1) hashtable )
      with
      | Some p1, Some p2 -> p1 :: p2 :: next_step_ls
      | None, Some p2 -> p2 :: next_step_ls
      | Some p1, None -> p1 :: next_step_ls
      | None, None -> next_step_ls)

(*Search for next layer into current graph*)
let rec search_next_layer (str1 : string array) (str2 : string array)
    (curr_layer : bfs_node list) (next_layer : bfs_node list)
    (hashtable : (string, int) Hashtbl_intf.Hashtbl.t ref) : bfs_node list =
  match curr_layer with
  | [] -> next_layer
  | point :: tl ->
      let new_next_layer =
        search_next_point str1 str2 point next_layer hashtable
      in
      search_next_layer str1 str2 tl new_next_layer hashtable

(*Search whole graph*)
let rec search_whole (str1 : string array) (str2 : string array)
    (graph : bfs_node list list)
    (hashtable : (string, int) Hashtbl_intf.Hashtbl.t ref) : bfs_node list list
    =
  if stop_at_layer str1 str2 graph then graph
  else
    match graph with
    | [] ->
        search_whole str1 str2
          [ [ Node { coordinate = (0, 0); parent = Null; path = [] } ] ]
          hashtable
    | curr_layer :: _ ->
        let new_layer = search_next_layer str1 str2 curr_layer [] hashtable in
        search_whole str1 str2 (new_layer :: graph) hashtable

(*Get the last valid point in the graph*)
let get_tail (str1 : string array) (str2 : string array)
    (graph : bfs_node list list) : bfs_node =
  match List.hd graph with
  | None -> failwith "unexpected error with graph"
  | Some last_layer ->
      let tail =
        List.fold
          ~f:(fun acc value ->
            if is_last_point ~str1 ~str2 ~node:value then value else acc)
          ~init:Null last_layer
      in
      tail

(*Use last point to search back the "linked list" and find the whole path from begining*)
let rec get_sequence (tail : bfs_node) (sequence : (int * int) list) :
    (int * int) list =
  match tail with
  | Null -> sequence
  | Node { coordinate = x, y; parent = next_tail; path = seq } ->
      get_sequence next_tail (sequence @ ((x, y) :: seq))


(*Get the sequence of del, add or common strings*)
let get_coordinate_seq (array1 : string array) (array2 : string array) :
    (int * int) list =
  let coordinate_table = Hashtbl.create (module String) in
  let hash_ptr = ref coordinate_table in
  let g = search_whole array1 array2 [] hash_ptr in
  let tl = get_tail array1 array2 g in
  List.rev (get_sequence tl [])

let get_diff (str1 : string) (str2 : string) : string =
  let array1, array2 =
    ( String_process.string2word_array str1,
      String_process.string2word_array str2 )
  in
  let seq = get_coordinate_seq array1 array2 in
  let content_ls =
    String_parser.coordinates_to_content_list seq array1 array2
  in
  let colored_common_string = String_parser.sect_ls2string content_ls in
  colored_common_string

let disp_file_diff (filename1 : string) (filename2 : string) : unit =
  let file_content1 = File_struct.get_string_content filename1 in
  let file_content2 = File_struct.get_string_content filename2 in
  print_string (get_diff file_content1 file_content2)

  