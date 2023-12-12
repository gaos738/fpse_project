open Write
open Insert
open Delete
open Replace
open Undo
open Read
open Diff_algo
open File_parse
open Merge
(* open Arg *)

let split_string str =
  Str.split (Str.regexp "[ \t]+") str

type action =
  | Write
  | Replace
  | Search
  | Insert
  | Undo
  | Read
  | Show
  | Delete
  | Diff
  | Mergeall
  | MergeSelect
  | Unknow

let action = ref Unknow
let input_history = ref []
let input_lines = ref (Str.split (Str.regexp "[\n]+") "OCaml Coding Information
We are using OCaml version 5.0.0.
Installing OCaml 5.0.0 and associated tools
We require that you use the opam packaging system for installing OCaml and its 
extensions. 
Once you get opam installed and working, everything else should be easy to 
install .. so the only hard part is the first step.
For Linux or Mac see The OPAM install page for install instructions.
For Mac users, the above requires Homebrew (a package manager for Linux-ish 
libraries) so here is a more detailed suggestion o")

let input_file_path = ref ""
let output_file_path = ref ""
let search_string = ref ""

let search_A = ref ""

let diff_path = ref ""

let mergeall_path = ref ""

let merge_select_path = ref ""
let merge_index = ref 0

let merge_disp = ref ""


let usage_msg = "\027[37mExample usage: -action <argument>
-read <file_path>
-write <file_path>
-replace <string_A> <string_B>
-search <string>
-insert <line> <column> <string>
-delete <line> <column>
-diff <diff_file_path>
-mergeall <merged file path> 
-mergeselect <merged file path> <index> 
-undo
"

let speclist = [
  ("-read", Arg.String (fun p -> input_file_path := p; action := Read), "read a file");
  ("-write", Arg.String (fun p -> output_file_path := p; action := Write), "write a file");
  ("-show", Arg.Unit (fun _ -> action := Show), "show the editing content");
  ("-replace", Arg.Tuple [Set_string replace_A; Set_string replace_B; Arg.Unit (fun _ -> action := Replace)] , "replace A by B");
  ("-undo", Arg.Unit (fun _ -> action := Undo), "revert a previous action");
  ("-search", Arg.Tuple [Set_string search_A; Arg.Unit (fun _ -> action := Search)], "search A");
  ("-insert", Arg.Tuple [Set_int insert_line; Set_int insert_col; Arg.String (fun v -> action := Insert; insert_A := v)], "insert A at line x and column y");
  ("-delete", Arg.Tuple [Set_int delete_line; Set_int delete_col; Arg.Unit (fun () -> action := Delete)], "delete a character at line x and column y");
  ("-diff", Arg.String (fun p -> diff_path := p; action := Diff), "diff a file");
  ("-mergeall", Arg.Tuple [Arg.String (fun p -> mergeall_path := p; action := Mergeall); ], "merge all");
  ("-mergeselect", Arg.Tuple [Arg.String (fun p -> mergeall_path := p; action := MergeSelect); ], "merge select index");
]

(* we use Arg to parse the command, notice we should always pass current as 0 since
  the Arg parser has a internal state to maintain the parse position. 
    *)
let parse_command command =

  (*By default, parse recognizes two unit options, -help and --help, which will print to standard output usage_msg 
  and the list of options, and exit the program. You can override this behaviour by specifying your own -help and --help options in speclist.*)
  Arg.parse_argv ?current:(Some (ref 0)) command speclist (fun x -> print_endline x) usage_msg;

  match !action with
  | Show ->
    File_struct.display_content !input_lines;

  | Write ->
    write_to_file !output_file_path !input_lines;

  | Insert ->
    let updated = List.mapi (fun i line -> if i = !insert_line then insert_string line else line) !input_lines in
    update_lines updated input_lines input_history;
    print_endline "\027[32mINSERTED SUCCESSFULLY:";
    File_struct.display_content !input_lines;

  | Delete ->
    let updated = List.mapi (fun i line -> if i = !delete_line then delete_string line else line) !input_lines in
    update_lines updated input_lines input_history;
    print_endline "\027[32mDELETED SUCCESSFULLY:";
    File_struct.display_content !input_lines;
    
  | Replace ->
    let updated = List.map replace_string !input_lines in
    update_lines updated input_lines input_history;
    print_endline "\027[32mREPLACED SUCCESSFULLY:";
    File_struct.display_content !input_lines;

  | Search ->
    let searched = List.filter (fun line -> Str.string_match (Str.regexp (".*" ^ !search_A)) line 0) !input_lines in
    List.iter (fun line -> print_endline line) searched;

  | Undo -> undo input_lines input_history

  | Read ->
    let lines = read input_file_path in
    update_lines lines input_lines input_history;
    print_endline "\027[32mREAD FILE SUCCESSFULLY:";
    File_struct.display_content !input_lines;

  | Diff ->
    let diff_content = File_struct.get_string_content (!diff_path) in
    (*let () = print_string diff_content in*)
    let input_content = File_struct.ls2str !input_lines in
    (*let () = print_string input_content in*)
    let diff_res = Graph_search.get_diff (Graph_search.string2content input_content) (Graph_search.string2content diff_content) in
    print_endline diff_res

  | Mergeall ->
    let mergeall_content = String.concat "\n" (read mergeall_path) in
    let input_content = String.concat "\n" !input_lines in
    if !merge_disp = "-disp" then
      Graph_search_for_merge.disp_merge_all (Graph_search_for_merge.string2filename input_content) (Graph_search_for_merge.string2filename mergeall_content)
    else
      let mergeall_res = Graph_search_for_merge.merge_all (Graph_search_for_merge.string2content input_content) (Graph_search_for_merge.string2content mergeall_content) in
      update_lines (Str.split (Str.regexp "[\n]+") mergeall_res) input_lines input_history;

  | MergeSelect ->
    let mergeall_content = String.concat "\n" (read mergeall_path) in
    let input_content = String.concat "\n" !input_lines in
    if !merge_disp = "-disp" then
      Graph_search_for_merge.disp_merge_selected (Graph_search_for_merge.string2filename input_content) (Graph_search_for_merge.string2filename mergeall_content) !merge_index
    else
      let mergeall_res = Graph_search_for_merge.merge_selected (Graph_search_for_merge.string2content input_content) (Graph_search_for_merge.string2content mergeall_content) !merge_index in
      update_lines (Str.split (Str.regexp "[\n]+") mergeall_res) input_lines input_history;

  | Unknow -> 
    print_endline "\027[31munknow action, please use -help to see the usage"


(* read input as command, then pass this command to parse_command, 
    parse_command will parse it and perform some effects, record the action and arguments,
    after executing the command, start_repl call it self to accept the next command
    *)
let rec start_repl () =
  let () = print_string "\027[33m>>> " in
  let () = flush stdout in
  let line = read_line () in
  try 
    (* add the first argument to make Arg parser happy *)
    let _ = parse_command (Array.of_list ("_"::(split_string line))) in
    (* let () = print_endline line in *)
    start_repl () 
  with
  | (Arg.Bad e) -> 
    let () = print_endline e in
    start_repl ()
  | (Arg.Help e) -> 
    let () = print_endline e in
    start_repl ()