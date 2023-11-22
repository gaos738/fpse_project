

[@@@warning "-32"]
open Core

    type bfs_node = Null | Node of {coordinate:int*int; parent:bfs_node; path:(int*int) list}



    let stop_judge ~str1:(str1:string array) ~str2:(str2:string array) ~node:(node : bfs_node) : bool =
      match node with
        | Node {coordinate=(x,y); parent=_; path=_} -> 
          (if Core.equal x ((Array.length str1)) && Core.equal y ((Array.length str2)) then true else false)
        | Null -> assert false



    let stop_condition (str1:string array) (str2:string array) (dfs_tree:bfs_node list list): bool =
      match List.hd dfs_tree with
      | None -> false
      | Some layer -> 
      (List.fold ~f:(fun acc point ->
        if (stop_judge ~str1:str1 ~str2:str2 ~node:point) then true else acc || false) ~init:false layer)
    


    let str_equal (str1:string array) (str2:string array) (coordinate:int*int) : bool =
      let (x,y)=coordinate in
      String.equal (Array.get str1 x) (Array.get str2 y)



    let valid_point (str1:string array) (str2:string array) (coordinate:int*int) : bool =
      let (x,y)=coordinate in
      Array.length str1 >= x && x>=0 && Array.length str2 >= y && y>=0



    let rec diagonal_path (str1:string array) (str2:string array) (coordinate:int*int) (path:(int*int) list): (int*int)*((int*int) list) =
      let (x,y) = coordinate in
      if valid_point str1 str2 (x+1,y+1) && str_equal str1 str2 (x,y) then 
        diagonal_path str1 str2 (x+1,y+1) ((x,y)::path)
      else
        (coordinate, path)



    let add_map (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref) (k:string) (v:int) : unit = 
      match Hashtbl.add !hashtable ~key:k ~data:v with
      | `Duplicate -> ()
      | `Ok -> ()



    let append_point (str1:string array) (str2:string array) (common_parent:bfs_node) (coordinate:int*int) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node option =
      let (x,y) = coordinate in
      if valid_point str1 str2 (x,y) then
        let (coordinate_new, path_new) = diagonal_path str1 str2 coordinate [] in
        let new_point = Node {coordinate=coordinate_new; parent=common_parent; path=path_new} in
        let (x,y) = coordinate_new in
        let key = (string_of_int x)^","^(string_of_int y) in
        (match Hashtbl.find !hashtable key with
        | Some _ -> None
        | None -> add_map hashtable key 1; Some new_point)
      else None



    let search_next_point (str1:string array) (str2:string array) (common_parent:bfs_node) (next_step_ls:bfs_node list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list =
      match common_parent with
      | Null -> assert false 
      | Node {coordinate=(x,y); parent=_; path=_} ->
        match (append_point str1 str2 common_parent (x+1,y) hashtable, append_point str1 str2 common_parent (x,y+1) hashtable) with
        | Some p1, Some p2 -> p1::p2::next_step_ls
        | None, Some p2 -> p2::next_step_ls
        | Some p1, None -> p1::next_step_ls
        | None,None -> next_step_ls

      
        
    let rec search_next_layer (str1:string array) (str2:string array) (curr_layer:bfs_node list) (next_layer:bfs_node list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list =
      match curr_layer with
      | [] -> next_layer
      | point::tl -> 
        (let new_next_layer=search_next_point str1 str2 point next_layer hashtable in
        search_next_layer str1 str2 tl new_next_layer hashtable)



    let rec search_whole (str1:string array) (str2:string array) (graph:bfs_node list list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list list=
      if stop_condition str1 str2 graph then graph
      else
        match graph with
        | [] -> search_whole str1 str2 [[Node {coordinate=(0,0); parent=Null; path=[]}]] hashtable
        | curr_layer::_ -> 
          let new_layer = search_next_layer str1 str2 curr_layer [] hashtable in
          search_whole str1 str2 (new_layer::graph) hashtable



      let string_hd (input_str:string) : string*string=
        let str_hd = String.sub input_str ~pos:0 ~len:1 in
        let str_tl = String.sub input_str ~pos:1 ~len:((String.length input_str) -1) in
        (str_hd,str_tl)
      


      let rec string2list (input_str:string) (curr_ls:string list) (curr_word:string) (last_ws:bool): string list=
        if String.equal input_str "" then 
          curr_word::curr_ls
        else
          let str_hd, str_tl = string_hd input_str in
          let is_whitespace = String.equal str_hd " " in
          if phys_equal is_whitespace last_ws then
            string2list str_tl curr_ls (curr_word^str_hd) last_ws
          else
            string2list str_tl ( curr_word::curr_ls ) str_hd is_whitespace
      


      let string2array (input_l:string): string array =
        let string_ls = string2list input_l [] "" true in
        let reverse = List.rev string_ls in
        Array.of_list reverse 
      


    let get_tail (str1:string array) (str2:string array) (graph:bfs_node list list) : bfs_node =
      match List.hd graph with
      | None -> failwith "unexpected error with graph"
      | Some last_layer -> 
        let tail = List.fold ~f:(fun acc value -> (if stop_judge ~str1:str1 ~str2:str2 ~node:value then value else acc) ) ~init:Null last_layer
        in tail
    


    let rec get_sequence (tail:bfs_node) (sequence:(int*int) list) : (int*int) list =
      match tail with   
        | Null -> sequence
        | Node {coordinate=(x,y); parent=next_tail; path=seq} ->
          get_sequence next_tail (sequence@((x,y)::seq)) 



    let append_string (curr:(int*int)) (prev:(int*int)) (total_string:string) (str1:string array) (str2:string array): string =
      let x,y = curr in
      let x_prev,y_prev = prev in
      if Core.equal x (x_prev+1) && Core.equal y (y_prev+1) then
        let curr_str = (Array.get str1 (x-1)) in
        total_string^"\027[30m"^curr_str
      else if Core.equal x (x_prev+1) && Core.equal y y_prev then 
        let curr_str1 = (Array.get str1 (x-1)) in
        total_string^"\027[31m"^curr_str1
      else if Core.equal x x_prev && Core.equal y (y_prev+1) then 
        let curr_str2 = (Array.get str2 (y-1)) in
        total_string^"\027[32m"^curr_str2
      else 
        let () = Stdio.printf "%i,%i %i,%i\n" x y x_prev y_prev in
        failwith "unexpected coordinate"



    let rec lcs_ls_to_str (ls:(int*int) list) (sample_str1:string array) (sample_str2:string array) (result_str:string): string =
        match ls with
        | (x,y)::(x_next,y_next)::tl -> 
          (let curr_string = append_string (x_next,y_next) (x,y) result_str sample_str1 sample_str2 in
          lcs_ls_to_str ((x_next,y_next)::tl) sample_str1 sample_str2 curr_string)
        | _ -> result_str



    let find_diff (str1:string) (str2:string) : ((int*int) list)*string =
      let array1, array2 = string2array str1, string2array str2 in
      let coordinate_table = Hashtbl.create (module String) in
      let hash_ptr = ref coordinate_table in
      let g = search_whole array1 array2 [] hash_ptr in
      let tl = get_tail array1 array2 g in
      let seq = (List.rev (get_sequence tl []))  in
      let common_string = (lcs_ls_to_str seq array1 array2 "") in
      print_string common_string;
      (seq,common_string)
          


      
    
