(* Ocaml test file 1 *)

let rec rev l =
  match l with
  |  [] -> []
  |  hd :: tl -> rev tl @ [hd];;

let rec zero_out_all_negs l =
  match l with
  |  [] -> []
  |  hd :: tl -> (if hd < 0 then 0 else hd) :: zero_out_all_negs tl;;