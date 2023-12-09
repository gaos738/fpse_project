
let update_lines lines input_lines input_history =
  input_history := !input_lines :: !input_history;
  input_lines := lines
;;


let undo input_lines input_history =
begin
  match !input_history with
    | [] -> print_endline "\027[31mNo more history to undo\027[37m"
    | h::t ->
      input_lines := h;
      input_history := t;
      print_endline "\027[32mUNDO SUCCESSFULLY:\027[37m";
      print_endline (String.concat "\n" !input_lines)
end