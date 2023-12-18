(*delete*)
val delete_line : int ref
val delete_col : int ref
val delete_A : string ref
val delete_string : string -> string

(*create*)
val create_file : string -> unit

(*insert*)
(*this is a reference to the line number where the insertion will occur. *)
val insert_line : int ref

(*this is a reference to the column number where the insertion will start. *)
val insert_col : int ref

(*this is a reference to the string that will be inserted. *)
val insert_A : string ref

(*this will inserts the string got from [insert_A] at the position set from [insert_line] and [insert_col]
  If [insert_col] is beyond the end of the text str, no insertion is made. *)
val insert_string : string -> string

(*read*)
val read : string ref -> string list

(*replace*)
(*these are the inputs from user*)
val replace_A : string ref
val replace_B : string ref

(* It will replace replace_A with the replace_B then returns the modified string. *)
val replace_string : string -> string
val replace_part : string -> string

(*write*)
(*this function is to write the edited text to the original file *)
val write_to_file : string -> string list -> unit

(*undo*)
(*updates the current lines and then stored in [input_lines] and records
   the previous value of [input_lines] in [input_history]. *)
val update_lines :
  string list -> string list ref -> string list list ref -> unit

(*undo the last update to [input_lines]. If there is no more history to undo, it prints a message indicating this.
     Otherwise, it restore the most recent history from [input_history] to [input_lines] and prints the restored lines. *)
val undo : string list ref -> string list list ref -> unit