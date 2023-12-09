let read input_file_path = 
begin
  (* read file content from input_file_path, from doc: "https://ocaml.org/docs/file-manipulation" *)
  let ic = open_in !input_file_path in
  try
    let rec read_lines lines =
      try
        let line = input_line ic in
        read_lines (line :: lines)
      with
      | End_of_file -> List.rev lines
    in
    let lines = read_lines [] in
    close_in ic;
    lines

  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e
  end
  