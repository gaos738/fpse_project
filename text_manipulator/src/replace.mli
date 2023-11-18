
(*
Put in a fine path, a string to replace, a string as the substitution, and return modified string  
*)

val replace_all : stirng -> string -> string -> string


(*
Put in a file path, a start line number, an end line number, a string to replace, a string as the substitution, and return the string that all occurances between start line and end line are replaced
*)

val replace_section : stirng -> int -> int -> string -> string


(*
Replace the n th occurance of a file, return the result
*)

val replace_n_th : stirng -> int  -> string -> string
