
module Graph_search_for_merge : sig 
    type string_file_content
    type filename
    val string2filename : string -> filename
    val string2content : string -> string_file_content
    val merge_all : string_file_content -> string_file_content -> string
    val disp_merge_all : filename -> filename -> unit
    val merge_selected : string_file_content -> string_file_content -> int -> string
    val disp_merge_selected : filename -> filename -> int -> unit
  end