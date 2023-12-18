(* Ocaml test file 2 *)

let rec rev l =
  match l with
  |  [] -> []
  |  hd :: tl -> rev tl @ [hd];;

let rec zero_negs l =
  match l with
  |  [] -> [] 
  |  hd :: tl -> (if hd < 0 then 0 else hd) :: zero_negs tl;;

let rec join (l: 'a list list) = match l with
  | [] -> [] (* "joining together a list of no-lists is an empty list" *)
  | l :: ls -> l @ join ls (* "by induction assume (join ls) will turn list-of-lists to single list" *)