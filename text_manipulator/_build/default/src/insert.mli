(*this is a reference to the line number where the insertion will occur. *)
val insert_line: int ref

(*this is a reference to the column number where the insertion will start. *)
val insert_col: int ref

(*this is a reference to the string that will be inserted. *)
val insert_A: string ref

(*this will inserts the string got from [insert_A] at the position set from [insert_line] and [insert_col] 
If [insert_col] is beyond the end of the text str, no insertion is made. *)
val insert_string : string -> string