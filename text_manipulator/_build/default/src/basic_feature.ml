(*this file is including the basic feature of the text manipulator*)

(*delete*)
(*take the input value*)
let delete_line = ref 0
let delete_col = ref 0
let delete_A = ref ""

(*this is the function use to delete the word selected*)
let delete_string str =
  let rec delete_char str i =
    if i = String.length str then str
    else if i = !delete_col then
      (*if inside the range of the content*)
      String.sub str 0 i ^ String.sub str (i + 1) (String.length str - i - 1)
    else delete_char str (i + 1)
  in
  delete_char str 0

(*Create*)
(*create a new file in the seleted path*)
let create_file path =
  let oc = open_out path in
  close_out oc

(*insert*)
(*take the input content*)
let insert_line = ref 0
let insert_col = ref 0
let insert_A = ref ""

(*this function is use to insert content*)
let insert_string str =
  let rec insert_char str i =
    if i = String.length str then
      (*insert the string in the selected position*)
      if i = !insert_col then str ^ !insert_A else str
    else if i = !insert_col then
      String.sub str 0 i ^ !insert_A ^ String.sub str i (String.length str - i)
    else insert_char str (i + 1)
  in
  (*if the column is beyond its original have, padding it with space*)
  if !insert_col > String.length str then
    let padding = String.make (!insert_col - String.length str) ' ' in
    insert_char (str ^ padding) 0
  else insert_char str 0

(*insert new line*)
let insert_new_line lines line_number =
  let rec helper acc i = function
    | [] when i = line_number -> List.rev acc @ [""]
    | hd :: tl when i = line_number -> List.rev acc @ [""; hd] @ tl
    | hd :: tl -> helper (hd :: acc) (i + 1) tl
    | [] -> List.rev acc  (* this is in case that line_number is greater than the list length *)
  in
  helper [] 0 lines
  
(*read*)
let read input_file_path =
  (* read file content from input_file_path, from doc: "https://ocaml.org/docs/file-manipulation" *)
  let ic = open_in !input_file_path in
  try
    let rec read_lines lines =
      try
        let line = input_line ic in
        read_lines (line :: lines)
      with End_of_file -> List.rev lines
    in
    let lines = read_lines [] in
    close_in ic;
    lines
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e

(*replace*)
(*take the input content*)
let replace_A = ref ""
let replace_B = ref ""

(*this is the funciton use to replace the specfic words in the content*)
let replace_string str =
  Str.global_replace (Str.regexp ("\\b" ^ !replace_A ^ "\\b")) !replace_B str

(*this one will replace all the selected parts in the contents
   e.g on one and only    (replace on to abbbb)
   ->   abbb abbbe and abbbly
*)
let replace_part str = Str.global_replace (Str.regexp !replace_A) !replace_B str

(*write*)
(*this function is write the content back to the specifc path*)
let write_to_file file_path lines =
  let out_channel = open_out file_path in
  try
    List.iter (fun line -> output_string out_channel (line ^ "\n")) lines;
    close_out out_channel
  with Sys_error msg ->
    close_out_noerr out_channel;
    failwith ("Error writing to file: " ^ msg)

(*undo*)
(*this function is used to update the content when making change*)
(*it will store every version in the input _history so that it is able to undo*)
let update_lines lines input_lines input_history =
  input_history := !input_lines :: !input_history;
  input_lines := lines

(*go back to the last version in the input_history, so that it can be take as undo*)
let undo input_lines input_history =
  match !input_history with
  | [] -> print_endline "\027[31mNo more history to undo\027[37m"
  | h :: t ->
      input_lines := h;
      input_history := t;
      print_endline "\027[32mUNDO SUCCESSFULLY:\027[37m";
      print_endline (String.concat "\n" !input_lines)