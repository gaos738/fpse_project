let replace_A = ref ""
let replace_B = ref ""


let replace_string str =
  Str.global_replace (Str.regexp !replace_A) !replace_B str