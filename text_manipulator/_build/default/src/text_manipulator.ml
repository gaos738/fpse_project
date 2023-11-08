

open Core
(*open Str*)
open Display
(*
let () = 
  let args = Sys.get_argv () |> Array.to_list
  in
  match args with
  | _::arg::_ -> 
    (match arg with
    | "disp" -> 
    | _ -> Stdio.printf "not implemented\n")
  | _ -> Stdio.printf "Invalid format\n" 
  *)
  (*  Caml_unix.select *)
  let split_string input = 
    Str.split (Str.regexp " +") input
  ;;

  let rec run () = 
    let input_data = In_channel.(input_line_exn stdin) in
    let input_list = split_string input_data in
    match input_list with
    | "exit"::[] -> () (*Quit the text manipulator*)
    | "disp"::_ -> display_content input_list; run ()  
    | _ -> Stdio.printf "Invalid operation\n"; Out_channel.flush stdout; run ()
  
  let () = run ()
