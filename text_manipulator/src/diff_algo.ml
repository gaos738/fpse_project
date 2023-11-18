

[@@@warning "-32"]
open Core

    type bfs_node = Null | Node of {coordinate:int*int; parent:bfs_node; path:(int*int) list}


    let stop_judge ~str1:(str1:string) ~str2:(str2:string) ~node:(node : bfs_node) : bool =
      match node with
        | Node {coordinate=(x,y); parent=_; path=_} -> 
          (if Core.equal x ((String.length str1)) && Core.equal y ((String.length str2)) then true else false)
        | Null -> assert false



    let stop_condition (str1:string) (str2:string) (dfs_tree:bfs_node list list): bool =
      match List.hd dfs_tree with
      | None -> false
      | Some layer -> 
      (List.fold ~f:(fun acc point ->
        if (stop_judge ~str1:str1 ~str2:str2 ~node:point) then true else acc || false) ~init:false layer)
    


    let char_equal (str1:string) (str2:string) (coordinate:int*int) : bool =
      let (x,y)=coordinate in
      Char.equal (String.get str1 x) (String.get str2 y)



    let valid_point (str1:string) (str2:string) (coordinate:int*int) : bool =
      let (x,y)=coordinate in
      String.length str1 >= x && x>=0 && String.length str2 >= y && y>=0



    let rec diagonal_path (str1:string) (str2:string) (coordinate:int*int) (path:(int*int) list): (int*int)*((int*int) list) =
      let (x,y) = coordinate in
      if valid_point str1 str2 (x+1,y+1) && char_equal str1 str2 (x,y) then 
        diagonal_path str1 str2 (x+1,y+1) ((x,y)::path)
      else
        (coordinate, path)



    let add_map (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref) (k:string) (v:int) : unit = 
      match Hashtbl.add !hashtable ~key:k ~data:v with
      | `Duplicate -> ()
      | `Ok -> ()


    let append_point (str1:string) (str2:string) (common_parent:bfs_node) (coordinate:int*int) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node option =
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



    let search_next_point (str1:string) (str2:string) (common_parent:bfs_node) (next_step_ls:bfs_node list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list =
      match common_parent with
      | Null -> assert false 
      | Node {coordinate=(x,y); parent=_; path=_} ->
        match (append_point str1 str2 common_parent (x+1,y) hashtable, append_point str1 str2 common_parent (x,y+1) hashtable) with
        | Some p1, Some p2 -> p1::p2::next_step_ls
        | None, Some p2 -> p2::next_step_ls
        | Some p1, None -> p1::next_step_ls
        | None,None -> next_step_ls

    (*let parent_example = Node {coordinate=(0,-1); parent=Null; path=[]};;*)
      
        
    let rec search_next_layer (str1:string) (str2:string) (curr_layer:bfs_node list) (next_layer:bfs_node list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list =
      match curr_layer with
      | [] -> next_layer
      | point::tl -> 
        (let new_next_layer=search_next_point str1 str2 point next_layer hashtable in
        search_next_layer str1 str2 tl new_next_layer hashtable)



    let rec search_whole (str1:string) (str2:string) (graph:bfs_node list list) (hashtable:(string, int) Hashtbl_intf.Hashtbl.t ref): bfs_node list list=
      if stop_condition str1 str2 graph then graph
      else
        match graph with
        | [] -> search_whole str1 str2 [[Node {coordinate=(0,0); parent=Null; path=[]}]] hashtable
        | curr_layer::_ -> 
          let new_layer = search_next_layer str1 str2 curr_layer [] hashtable in
          search_whole str1 str2 (new_layer::graph) hashtable



    let get_tail (str1:string) (str2:string) (graph:bfs_node list list) : bfs_node =
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



    let append_string (curr:(int*int)) (prev:(int*int)) (total_string:string) (str1:string) (str2:string): string =
      let x,y = curr in
      let x_prev,y_prev = prev in
      if Core.equal x (x_prev+1) && Core.equal y (y_prev+1) then
        let curr_char = String.make 1 (String.get str1 (x-1)) in
        total_string^"\027[30m"^curr_char
      else if Core.equal x (x_prev+1) && Core.equal y y_prev then 
        let curr_char1 = String.make 1 (String.get str1 (x-1)) in
        total_string^"\027[31m"^curr_char1
      else if Core.equal x x_prev && Core.equal y (y_prev+1) then 
        let curr_char2 = String.make 1 (String.get str2 (y-1)) in
        total_string^"\027[32m"^curr_char2
      else 
        let () = Stdio.printf "%i,%i %i,%i\n" x y x_prev y_prev in
        failwith "unexpected coordinate"



      let rec lcs_ls_to_str (ls:(int*int) list) (sample_str1:string) (sample_str2:string) (result_str:string): string =
        match ls with
        | (x,y)::(x_next,y_next)::tl -> 
          (let curr_string = append_string (x_next,y_next) (x,y) result_str sample_str1 sample_str2 in
          lcs_ls_to_str ((x_next,y_next)::tl) sample_str1 sample_str2 curr_string)
        | _ -> result_str

      



    let raw_result (str1:string) (str2:string) : ((int*int) list)*string =
      let coordinate_table = Hashtbl.create (module String) in
      let hash_ptr = ref coordinate_table in
      let g = search_whole str1 str2 [] hash_ptr in
      let tl = get_tail str1 str2 g in
      let seq = (List.rev (get_sequence tl []))  in
      let common_string = (lcs_ls_to_str seq str1 str2 "") in
      print_string common_string;
      (seq,common_string)
          


      
    
