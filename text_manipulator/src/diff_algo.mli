type word_coordinate
val int_tuple2word_coordinate : int * int -> word_coordinate
val int_tuple_ls2word_coordinate_ls : (int * int) list -> word_coordinate list

(*Functions used to parse string and split into sections(like add section, del section)*)
module String_parser : sig
    type section_type = Del | Common | Add | N [@@deriving compare]
    type content_section = section_type * string
  
    val coordinates_to_content_list :
    word_coordinate list -> string array -> string array -> content_section list
  
    val sect_ls2string : content_section list -> string
end
  
  (*Functions converting file content to word array*)
module String_process : sig
    val rm_whitespace : string -> string
    val str_equal_at_index : string array -> string array -> word_coordinate -> bool
    val string_hd : string -> string * string
    val string2word_list : string -> string list -> string -> bool -> string list
    val string2word_array : string -> string array
end
  (*Functions used to perform the diff algorithm and get results*)
module Graph_search : sig 
  type string_file_content
  type filename
  val content2string : string_file_content -> string
  val string2content : string -> string_file_content
  val get_coordinate_seq : string array -> string array -> word_coordinate list
  val get_diff : string_file_content -> string_file_content -> string
  val disp_file_diff : filename -> filename -> unit
end