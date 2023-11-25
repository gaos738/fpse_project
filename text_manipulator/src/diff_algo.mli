

module String_parser : sig
    type section_type = Del | Common | Add | N [@@deriving compare]
    type content_section = (section_type*string)

val coordinates_to_content_list : (int * int) list -> string array -> string array -> content_section list

val sect_ls2string : content_section list -> string

end

val string2array : string -> string array

val get_coordinate_seq : string array -> string array -> (int * int) list

val lcs_ls_to_color_str : (int * int) list -> string array -> string array -> string -> string -> string

val get_diff : string -> string -> string




