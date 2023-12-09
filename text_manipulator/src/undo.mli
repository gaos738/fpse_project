(*updates the current lines and then stored in [input_lines] and records 
   the previous value of [input_lines] in [input_history]. *)
val update_lines : string list -> string list ref -> string list list ref -> unit

(*undo the last update to [input_lines]. If there is no more history to undo, it prints a message indicating this.
     Otherwise, it restore the most recent history from [input_history] to [input_lines] and prints the restored lines. *)
val undo : string list ref -> string list list ref -> unit 