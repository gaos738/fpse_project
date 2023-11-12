open Core


let disp_range=20;;


let file2string_ls (filepath: string) : string list = 
  In_channel.input_lines (In_channel.create filepath)
;;


let get_range (lines: string list) (start_number: int) : string list =
  let (_,tl1) = List.split_n lines start_number in
  let (hd2,_) = List.split_n tl1 disp_range in
  hd2
;;

let get_line_number (number_in: int) : string =
  let rec make_num_label number =
    if Core.equal (String.length number) 3
      then number
    else make_num_label (" "^number)
  in make_num_label (string_of_int number_in)
;;

let print_lines (str_lines: string list) (start_number: int) : unit =
  let rec print_lines_aux (lines: string list) (line_number: int) : unit = 
    match lines with
    | [] -> ()
    | line::tl -> 
      (*Stdio.printf "%s| " (get_line_number line_number);*)
      let str_fr = "\027[30m "^(get_line_number line_number)^"|" in
      print_string str_fr;
      let str = "\027[31m "^line^" \n" in
      print_string str;
      (*Stdio.printf "%s\n" line; *)
      print_lines_aux tl (line_number+1) 
  in print_lines_aux str_lines start_number
;;


let display_content (arguments: string list) : unit =
  match arguments with 
  | "disp"::filename::start_line::[] ->
    let start_line = (int_of_string start_line) in
    let lines = filename |> file2string_ls |> (fun x -> get_range x start_line) in
    print_lines lines start_line; Out_channel.flush stdout
  | _ -> Stdio.printf"Unexpected display error\n"; assert false
;;