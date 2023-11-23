[@@@warning "-32"]
[@@@warning "-27"]

open Core
open Diff_algo


  let add_word_to_ls (curr:(int*int)) (prev:(int*int)) (total_ls:(string*string) list) (str1:string array) (str2:string array) : (string*string) list =
    let x,y = curr in
    let x_prev,y_prev = prev in
    if Core.equal x (x_prev+1) && Core.equal y (y_prev+1)  then
      let curr_str = (Array.get str1 (x-1)) in
      total_ls @ [("common",curr_str)]

    else if Core.equal x (x_prev+1) && Core.equal y y_prev  then 
      let curr_str = (Array.get str1 (x-1)) in
      total_ls @ [("del",curr_str)]

    else if Core.equal x x_prev && Core.equal y (y_prev+1)  then 
      let curr_str = (Array.get str2 (y-1)) in
      total_ls @ [("add",curr_str)]

    else 
      failwith "expected string"



  let rec lcs_ls_to_labeled_ls (ls:(int*int) list) (sample_str1:string array) (sample_str2:string array) (result_ls:(string*string) list) : (string*string) list =
    match ls with
    | (x,y)::(x_next,y_next)::tl -> 
      let curr_str = add_word_to_ls (x_next,y_next) (x,y) result_ls sample_str1 sample_str2 in
      lcs_ls_to_labeled_ls ((x_next,y_next)::tl) sample_str1 sample_str2 curr_str
    | _ -> List.fold ~f:(fun acc (label,value) -> if String.equal value "" then acc else acc@[(label,value)] ) ~init:[] result_ls 
      

    

  let merge_all (str1:string) (str2:string) : string =
    let array1, array2 = string2array str1, string2array str2 in
    let seq = get_coordinate_seq array1 array2 in
    let all_words_ls = (lcs_ls_to_labeled_ls seq array1 array2 []) in
    let extracted_string = List.fold ~f:(fun acc (_,value) -> acc^value ) ~init:"" all_words_ls in
    extracted_string

  

  let combine_same_section (in_ls:(string*string) list) : (string*string) list =
    let rec combine_same_section_aux (checked:(string*string) list) (unchecked:(string*string) list) (curr_section:(string*string)) : (string*string) list =
      let last_label,last_content = curr_section in
      match unchecked with
      | [] -> checked@[curr_section]
      | (label,content)::tl -> 
        if String.equal label last_label then 
          combine_same_section_aux checked tl (last_label, (last_content^content))
        else
          combine_same_section_aux (checked@[curr_section]) tl (label,content)
    in combine_same_section_aux [] in_ls ("common","")


  
  let remove_unselected_content (in_ls:(string*string) list) (in_index:int) : string =
    let rec remove_unselected_content_aux (checked:string) (unchecked:(string*string) list) (index:int) : string =
      match unchecked with
      | [] -> checked
      | (label,content)::tl -> 
        if String.equal label "common" then 
          remove_unselected_content_aux (checked^content) tl index
        else
          (if Core.equal in_index index then
            remove_unselected_content_aux (checked^content) tl (index+1)
          else remove_unselected_content_aux (checked) tl (index+1))
    in remove_unselected_content_aux "" in_ls 1



  let merge_selected (str1:string) (str2:string) (index:int): string =
    let array1, array2 = string2array str1, string2array str2 in
    let seq = get_coordinate_seq array1 array2 in
    let all_words_ls = (lcs_ls_to_labeled_ls seq array1 array2 []) in
    all_words_ls |> combine_same_section |> (fun x -> remove_unselected_content x index)

    let merge_selected_test (str1:string) (str2:string) (index:int): (string*string) list =
      let array1, array2 = string2array str1, string2array str2 in
      let seq = get_coordinate_seq array1 array2 in
      (lcs_ls_to_labeled_ls seq array1 array2 []) 