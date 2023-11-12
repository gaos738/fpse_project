


open Core

module Match_graph=
  struct

    type point_value={coordinate:int*int; value:int}

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


    (*
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
    *)


    let find_prev_max (x:int) (y:int) (graph:int list list): int =
      let rec find_prev_max_aux (graph_max:int) (modified_graph:int list list): int =
        match modified_graph with
        | [] -> graph_max
        | cur_row::tl -> 
          let local_max = List.fold ~f:(fun acc value -> if acc > value then acc else value ) ~init:graph_max cur_row in
          let () = Stdio.printf "%i\n" local_max in
          find_prev_max_aux local_max tl
      in
      let cropped = crop_graph x y graph in
      find_prev_max_aux 0 cropped 

  
    let check_and_label (character1:string) (row:int) (character2:string) (col:int) (graph:int list list): (int list list)*point_value option =
      if String.equal character1 character2 then
        let local_max = find_prev_max col row graph in
        let new_max_point = {coordinate=col, row; value=(local_max+1)} in
        ((modify (local_max+1) col row graph), Some new_max_point)
      else
        (graph,None)



    let rec label_row (character:string) (const_row:int) (str:string) (col:int) (col_target:int) (graph:int list list) (point_ls:point_value list):(int list list)*(point_value list) =
      if Core.equal col col_target then (graph, point_ls)
      else 
        let (str_hd, str_tl) = get_str_hd str in
        let (new_graph, point) = check_and_label character const_row str_hd col graph in
        match point with
        | None -> label_row character const_row str_tl (col+1) col_target new_graph point_ls
        | Some p -> label_row character const_row str_tl (col+1) col_target new_graph (p::point_ls)



    let label_graph (str1:string) (str2:string) (graph:int list list): (int list list)*(point_value list) =

      let rec label_graph_aux (str1:string) (index1:int) (index_target:int) (str2:string) (graph:int list list) (point_ls:point_value list): (int list list)*(point_value list) =
        let str2_len =(String.length str2) in
        if Core.equal index1 index_target then (graph,point_ls)
        else 
          let (str_hd, str_tl) = get_str_hd str1 in
          let (new_graph,new_point_ls) = label_row str_hd index1 str2 0 str2_len graph [] in
          label_graph_aux str_tl (index1+1) index_target str2 new_graph (new_point_ls@point_ls)
      in

      let len =(String.length str1) in
      label_graph_aux str1 0 len str2 graph []


    
    let find_prev_max_point (prev_max:point_value) (all_points:point_value list) : point_value option =
      let {coordinate=local_x, local_y; value=val_max} = prev_max in
      if Core.equal val_max 1 then None
      else
        let local_max = List.fold ~f:(fun acc value ->
          let {coordinate=cur_x, cur_y; value=val_cur} = value in
          if cur_x<local_x && cur_y<local_y && Core.equal val_cur (val_max-1) then value
          else acc) ~init:prev_max all_points in
        Some local_max

    

    let find_max (all_points:point_value list) : point_value  =
      let init_point = {coordinate=0, 0; value=0} in
      let local_max = List.fold ~f:(fun acc value ->
        let {coordinate=_, _; value=val_cur} = value in
        let {coordinate=_, _; value=val_max} = acc in
        if val_cur > val_max then value
        else acc) ~init:init_point all_points in
      local_max 

      

    let get_valid_lcs_list (all_points:point_value list) : point_value list =
      let rec get_lcs_aux (prev_point:point_value) (critical_path_ls:point_value list): point_value list =
        match find_prev_max_point prev_point all_points with
        | None -> critical_path_ls
        | Some p -> get_lcs_aux p (p::critical_path_ls)
      in
      let global_max = find_max all_points in
      get_lcs_aux global_max []


    
     let rec lcs_ls_to_str (ls:point_value list) (sample_str:string) (result_str:string): string =
      match ls with
      | [] -> result_str
      | hd::tl -> 
        let {coordinate=local_x, _; value=_} = hd in
        let curr_char = String.make 1 (String.get sample_str local_x) in
        lcs_ls_to_str tl sample_str (result_str^curr_char)


    let lcs_ls_split (in_ls:point_value list) : (int list)*(int list)=
      let rec lcs_ls_split_aux (ls:point_value list) (pair1: int list) (pair2: int list): (int list)*(int list)=
        match ls with
        | [] -> (pair1, pair2)
        | hd::tl -> 
          let {coordinate=x, y; value=_} = hd in
          lcs_ls_split_aux tl (x::pair1) (y::pair2)
      in lcs_ls_split_aux in_ls [] []

    
    
    let exist_in_list (index:int) (ls:int list) : bool =
      List.fold ~f:(fun acc value -> if Core.equal index value then true else (acc || false) ) ~init:false ls



    let get_str_hd (str:string) : string*string = 
      String.sub str ~pos:0 ~len:1, String.sub str ~pos:1 ~len:((String.length str) -1)



    let build_color_string (input_string:string) (point_list:int list) : string =
      let rec build_color_string_aux (index:int) (modified:string) (unmodified:string) (common_prev:bool) (point_ls:int list): string =
        if String.equal unmodified "" then modified
        else 
          let hd,tl = get_str_hd unmodified in
          if exist_in_list index point_ls then 
            if common_prev then build_color_string_aux (index+1) (modified^hd) tl true point_ls
            else build_color_string_aux (index+1) (modified^"\027[31m"^hd) tl true point_ls
          else
            if common_prev then build_color_string_aux (index+1) (modified^"\027[30m"^hd) tl false point_ls
            else build_color_string_aux (index+1) (modified^hd) tl false point_ls 
      in
      build_color_string_aux 0 "\027[30m" input_string false point_list



    let print_color_diff (str1:string) (str2:string): unit =
      let g = initialize (String.length str2) (String.length str1) in
      let (_, all_points) =label_graph str1 str2 g in
      let valid_points = get_valid_lcs_list all_points in

      let common = lcs_ls_to_str valid_points str2 "" in
      Stdio.printf "%s\n" common;

      let (str2_points, str1_points) = lcs_ls_split valid_points in
      let color_str1 = build_color_string str1 str1_points in
      let color_str2 = build_color_string str2 str2_points in
      Stdio.printf "\027[32m <First file>\n";
      print_string color_str1;
      Stdio.printf "\n\027[32m <Second file>\n";
      print_string color_str2;
      (*(color_str1, color_str2)*)


      





  
    
      (*
      open Diff_algo;;
      let g = Match_graph.initialize 4 4;;
      let str1 = "Here are the groups with their assigned advisors heading into the first lab day. This will be showed again in class and might be subject to some change.";;
      let str2 = "Here are the groups with their assigned advisors heading iApologies if this doesn't contain your preferred name. Please let me know if we have a group wrong.and might be subject to some change.";;
      let labeled_g = Match_graph.label_graph str1 str2 g;;  
      let lcs_list = Match_graph.get_lcs_list labeled_g;;
      let lcs_string = Match_graph.lcs_ls_to_str lcs_list "abcd" ""

      Match_graph.print_color_diff str1 str2;;
      ;;

    
      
      *)

      
    

  end