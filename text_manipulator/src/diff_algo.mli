


(*
Get two file path (string), return a string of diff with all deletes and adds color labeled.
*)

val find_diff : string -> string -> string

(*
Becuause our diff algorithm is based on "smallest number of edits" on one string to make it be another string, it is possible that multiple solution exists.
It is not guaranteed that the solution found is optimal for human readability.
For example, "smallest number of edits" of "AABA" and "ABA" can be "_ABA" or "A_BA" where "_" represents an edit (delete or add).
In previous example, "_ABA" would make more sense for human. So we want to design a "accurate_diff" which could get the solution with more human readability.
The reason for providing two function is beacuse finding the more human readable solution increases the time complexity and space complexity, while at most of the time result of 
"find_diff" is still interpretable. So we want to let user choose the tool.

*)
val accurate_diff : string -> string -> string


(*
Given two files, a flag indicating using "accurate_diff" or "find_diff", merge the n th difference where n is an argument to this function. Return new content.
*)

val merge : string -> string -> bool -> int -> string
