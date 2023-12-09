
let write_to_file file_path lines =
  let out_channel = open_out file_path in
  try
    List.iter (fun line -> output_string out_channel (line ^ "\n")) lines;
    close_out out_channel
  with
  | Sys_error msg ->
    close_out_noerr out_channel;
    failwith ("Error writing to file: " ^ msg)


