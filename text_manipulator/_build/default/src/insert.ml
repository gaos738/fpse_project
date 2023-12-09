let insert_line = ref 0
let insert_col = ref 0
let insert_A = ref ""


let insert_string str =
  let rec insert_char str i =
    if i = String.length str then
      if i = !insert_col then str ^ !insert_A
      else str
    else if i = !insert_col then
      (String.sub str 0 i) ^ !insert_A ^ (String.sub str i (String.length str - i))
    else insert_char str (i + 1)
  in
  if !insert_col > String.length str then
    let padding = String.make (!insert_col - String.length str) ' ' in
    insert_char (str ^ padding) 0
  else
    insert_char str 0