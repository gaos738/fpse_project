open Core

module File_struct = struct
  type filename = string
  type file_content = string list
  type arguments = string list

  let file2string_ls (filepath : filename) : file_content =
    In_channel.input_lines (In_channel.create filepath)

  let content_to_str_ls (lines : file_content) : string list = lines

  let get_range (lines : file_content) (start_number : int) (disp_range : int) :
      file_content =
    let _, tl1 = List.split_n lines start_number in
    let hd2, _ = List.split_n tl1 disp_range in
    hd2

  let get_line_number (number_in : int) : string =
    let rec make_num_label number =
      if Core.equal (String.length number) 3 then number
      else make_num_label (" " ^ number)
    in
    make_num_label (string_of_int number_in)

  let print_lines (str_lines : file_content) (start_number : int) : unit =
    let rec print_lines_aux (lines : file_content) (line_number : int) : unit =
      match lines with
      | [] -> ()
      | line :: tl ->
          let line_label = "\027[30m " ^ get_line_number line_number ^ "|" in
          print_string line_label;
          let str = "\027[31m " ^ line ^ " \n" in
          print_string str;
          print_lines_aux tl (line_number + 1)
    in
    print_lines_aux str_lines start_number

  let display_content (arguments : arguments) : unit =
    match arguments with
    | [ "disp"; filename; start_line ] ->
        let start_line = int_of_string start_line in
        let lines =
          filename |> file2string_ls
          |> (fun x -> get_range x start_line 20)
          |> content_to_str_ls
        in
        print_lines lines start_line;
        Out_channel.flush stdout
    | _ ->
        Stdio.printf "Unexpected display error\n";
        assert false

  let get_string_content (filepath : filename) : string =
    let str_list_content = filepath |> file2string_ls |> content_to_str_ls in
    let string_content =
      List.fold ~f:(fun acc line -> acc ^ line) ~init:"" str_list_content
    in
    string_content

  let get_string_ls_content (filepath : filename) : string list =
    let str_list_content = filepath |> file2string_ls |> content_to_str_ls in
    str_list_content
end
