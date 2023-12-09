let delete_line = ref 0
let delete_col = ref 0
let delete_A = ref ""

let delete_string str =
  let rec delete_char str i =
    if i = String.length str then str
    else if i = !delete_col then
      (String.sub str 0 i) ^ (String.sub str (i + 1) (String.length str - i - 1))
    else delete_char str (i + 1)
  in
  delete_char str 0