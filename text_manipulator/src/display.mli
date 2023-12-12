

module File_struct : sig
    
  type filename = string
  type file_content = string list
  type arguments = string list

  val display_content : arguments -> unit

  val get_string_content : filename -> string

  val get_string_ls_content : filename -> string list

  val ls2str : file_content -> string

end