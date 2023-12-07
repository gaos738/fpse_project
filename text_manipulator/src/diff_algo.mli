module String_parser : sig
  type section_type = Del | Common | Add | N [@@deriving compare]
  type content_section = section_type * string

  val coordinates_to_content_list :
    (int * int) list -> string array -> string array -> content_section list

  val sect_ls2string : content_section list -> string
end

module String_process : sig
  val rm_whitespace : string -> string
  val str_equal_at_index : string array -> string array -> int * int -> bool
  val string_hd : string -> string * string
  val string2word_list : string -> string list -> string -> bool -> string list
  val string2word_array : string -> string array
end

val get_coordinate_seq : string array -> string array -> (int * int) list
val get_diff : string -> string -> string
