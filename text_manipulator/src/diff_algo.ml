
open Core

module Match_graph=
  struct

    let initialize (x:int) (y:int) : int list list =
      let rec initialize_aux (x_aux:int) (y_aux:int) (curr_y: int) (graph: int list list): int list list =
        if Core.equal y_aux curr_y then graph
        else
          let new_row = List.init x_aux ~f:(fun _ -> 0) in
          initialize_aux x_aux y_aux (curr_y+1) (new_row::graph)
      in initialize_aux x y 0 []
      


    let modify_row (value:int) (x:int) (row:int list): int list =
      let (hd,tl) = List.split_n row x in
      match tl with 
      |  _::tl_tl -> hd@(value::tl_tl)
      |  _ -> failwith "modify row indexing error"



    let modify (value:int) (x:int) (y:int) (graph:int list list): int list list =
      let (front,back) = List.split_n graph y in
      match back with
      | row::tl -> front@[modify_row value x row]@tl
      | _ -> failwith "modify error"

    
    
    let get_str_hd (str:string): string*string = 
      String.sub str ~pos:0 ~len:1, String.sub str ~pos:1 ~len:((String.length str) -1)


    
    let crop_graph (x:int) (y:int) (graph:int list list): int list list =
      let (rows_cropped,_) = List.split_n graph y in
      List.map ~f:(fun row -> match List.split_n row x with (hd,_) -> hd) rows_cropped
    


    type point_value={coordinate:int*int; value:int}



    let rec find_row_max (const_y:int) (x_ind:int) (current_max:point_value) (row:int list): point_value =
      let {coordinate=_, _; value=local_max} = current_max in
      match row with
      | [] -> current_max
      | hd::tl -> 
        if hd > local_max then
          let new_max = {coordinate=x_ind, const_y; value=hd} in
          find_row_max const_y (x_ind+1) new_max tl
        else
          find_row_max const_y (x_ind+1) current_max tl



    let rec find_2Dls_max (row_index:int) (current_max:point_value) (ls:int list list): point_value =
      match ls with
      | [] -> current_max
      | hd::tl -> 
        let new_max = find_row_max row_index 0 current_max hd in
        find_2Dls_max (row_index+1) new_max tl



    let find_graph_max (graph:int list list): point_value =
      let init_max = {coordinate=(0,0); value=0} in
      find_2Dls_max 0 init_max graph
    


    let find_prev_max (x:int) (y:int) (graph:int list list): int =
      let rec find_prev_max_aux (graph_max:int) (modified_graph:int list list): int =
        match modified_graph with
        | [] -> graph_max
        | cur_row::tl -> 
          let (inrange_row,_) = List.split_n cur_row x in
          let local_max = List.fold ~f:(fun acc value -> if acc > value then acc else value ) ~init:0 inrange_row in
          find_prev_max_aux local_max tl
      in
      let (inrange_graph,_) = List.split_n graph y in
      find_prev_max_aux 0 inrange_graph 


  
    let check_and_label (character1:string) (row:int) (character2:string) (col:int) (graph:int list list): int list list =
      if String.equal character1 character2 then
        let local_max = find_prev_max col row graph in
        modify (local_max+1) col row graph
      else
        graph



    let rec label_row (character:string) (const_row:int) (str:string) (col:int) (col_target:int) (graph:int list list): int list list =
      if Core.equal col col_target then graph
      else 
        let (str_hd, str_tl) = get_str_hd str in
        let new_graph = check_and_label character const_row str_hd col graph in
        label_row character const_row str_tl (col+1) col_target new_graph



    let label_graph (str1:string) (str2:string) (graph:int list list): int list list =

      let rec label_graph_aux (str1:string) (index1:int) (index_target:int) (str2:string) (graph:int list list): int list list =
        let str2_len =(String.length str2) in
        if Core.equal index1 index_target then graph
        else 
          let (str_hd, str_tl) = get_str_hd str1 in
          let new_graph = label_row str_hd index1 str2 0 str2_len graph in
          label_graph_aux str_tl (index1+1) index_target str2 new_graph
      in

      let len =(String.length str1) in
      label_graph_aux str1 0 len str2 graph

    

    let get_lcs_list (graph_in:int list list) : point_value list =
      let rec get_lcs_aux (graph:int list list) (results:point_value list): point_value list =
        let local_max = find_graph_max graph in
        let {coordinate=local_x, local_y; value=val_max} = local_max in
        let new_ls = local_max::results in
        if Core.equal val_max 1 then new_ls
        else 
          let cropped = crop_graph local_x local_y graph in
          get_lcs_aux cropped new_ls
      in
      get_lcs_aux graph_in []



    let rec lcs_ls_to_str (ls:point_value list) (sample_str:string) (result_str:string): string =
      match ls with
      | [] -> result_str
      | hd::tl -> 
        let {coordinate=local_x, _; value=_} = hd in
        let curr_char = String.make 1 (String.get sample_str local_x) in
        lcs_ls_to_str tl sample_str (result_str^curr_char)
    


    let find_lcs (str1:string) (str2:string): string =
      let g = initialize (String.length str2) (String.length str1) in
      let labeled_g =label_graph str1 str2 g in
      let lcs_list = get_lcs_list labeled_g in
      lcs_ls_to_str lcs_list str1 ""


    
    

      (*
      open Diff_algo;;
      let g = Match_graph.initialize 4 4;;
      let labeled_g = Match_graph.label_graph "abcd" "bcde" g;;  
      let lcs_list = Match_graph.get_lcs_list labeled_g;;
      let lcs_string = Match_graph.lcs_ls_to_str lcs_list "abcd" "";;

      "Currently it is not possible to change the font family or size outside the editor. You can however zoom the entire user interface in and out from the View menu."
      "So I wonder if you can change it somehow. family or size outside the editor. You can how much non answers get up vote "
      
      *)

      
    

  end