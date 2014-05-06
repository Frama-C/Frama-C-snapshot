(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Promelaast
(*open Graph.Pack.Digraph

let st_array = ref (Array.make 1 (V.create 0)) ;;

let auto2digraph (stl,trl) =
  Aorai_option.feedback "auto2digraph:"  ;
  let digraph = create ()  in
  st_array:= Array.make (List.length stl) (V.create 0);
  Aorai_option.feedback "   array : ok\n"  ;

  let _ = List.iter
    (fun st -> 
       (!st_array).(st.nums)<-(V.create st.nums);
       add_vertex digraph (!st_array).(st.nums)
       )
    stl 
  in
  Aorai_option.feedback "   array remplissage : ok\n"  ;
  List.iter 
    (fun tr -> add_edge digraph (V.create tr.start.nums) (V.create tr.stop.nums))
    trl;
  digraph
;;
  
    
let existing_path auto st1 st2 =
  Aorai_option.feedback "existing path ..\n"  ;
  let digraph = auto2digraph auto in
  let start = (!st_array).(st1.nums) in 
  let stop = (!st_array).(st2.nums) in 
  Aorai_option.feedback "%s" ("test : Etats choisis ("^(string_of_int (V.label start))^","^(string_of_int (V.label stop))^")\n") ;

  display_with_gv digraph;
  Aorai_option.feedback "   affichage : ok\n"  ;
  Aorai_option.feedback "shortest path : "  ;

  let path=shortest_path digraph start stop in
  Aorai_option.feedback "done.\n"  ;
  path
;;

let test (stl,trl) =
  let st2 = List.hd stl in
  let st1 = List.hd (List.tl stl) in
    
  let _ = existing_path (stl,trl) st1 st2 in
  Aorai_option.feedback "Fini.\n"  ;
  ()
;;
*)

let voisins (_,trans_l) st =
  List.fold_left
    (fun vl tr -> if tr.start.nums=st.nums then (tr.stop,1)::vl else vl)
    []
    trans_l

let empty () = [] ;;
let is_empty heap = (List.length heap)=0 ;;
let add (length,(st,path)) heap = (length,(st,path))::heap ;;
let extract_min heap = 
  let (min,h) = 
    List.fold_left
      (fun ((lmin,min),h) (lcur,cur) -> 
	 if lmin<=lcur then
	   ((lmin,min),(lcur,cur)::h) 
	 else
	   ((lcur,cur),(lmin,min)::h) 
      )
      ((List.hd heap),[])
      (List.tl heap)
  in
  (min,h)
  
  

(* Source : wikipedia*)

(* l'adjacence est donnee sous la forme d'une fonction : adj v est la liste des voisins de v,
   avec leur distance ; la fonction suivante cherche le plus court chemin de v1 a v2 *)
let dijkstra (adj: 'a -> ('a * int) list) (v1:'a) (v2:'a) =
  let visited = Hashtbl.create 97 in
  let rec loop h =
    if is_empty h then raise Not_found;
    let (w,(v,p)),h = extract_min h in
      if v = v2 then 
        List.rev p, w
      else
        let h =
  	  if not (Hashtbl.mem visited v) then begin
  	    Hashtbl.add visited v ();
  	    List.fold_left (fun h (e,d) -> add (w+d, (e, e::p)) h) h (adj v)
  	  end else
  	    h
        in
          loop h
  in
    loop (add (0,(v1,[])) (empty()))




let existing_path (stl,_ as auto)  stn1 stn2 =
  let st1 = ref (List.hd stl) in
  let st2 = ref (List.hd stl) in
  List.iter 
    (fun st -> 
       if st.nums=stn1 then st1:=st;
       if st.nums=stn2 then st2:=st;
    )
    stl;
  
  try
    let _ = dijkstra (voisins auto) !st1 !st2 in
    true
  with 
    | Not_found -> false
;;


(** since Nitrogen-20111001 *)
let get_transitions_of_state st (_,tr) =
  List.fold_left
    (fun acc tr ->
      if tr.start.nums = st.nums then tr::acc else acc)
    [] tr

let get_transitions_to_state st (_,tr) =
  List.fold_left
    (fun acc tr ->
      if tr.stop.nums = st.nums then tr::acc else acc)
    [] tr

let get_edges st1 st2 (_,tr) =
  List.find_all
    (fun tr -> tr.start.nums = st1.nums && tr.stop.nums = st2.nums)
    tr

let get_init_states (st,_) = List.filter (fun x -> x.init = Bool3.True) st

let at_most_one_path (states,transitions as auto) st1 st2 =
  try
    let path,_ = dijkstra (voisins auto) st1 st2 in
    match path with
      | [] | [ _ ] -> true
      | x::y::_ ->
        let (trans1,trans2) = 
          List.partition 
            (fun t -> t.start.nums = x.nums && t.stop.nums = y.nums) 
            transitions
        in
        let transitions = (List.tl trans1) @ trans2 in
        let auto = states, transitions in 
        ignore (dijkstra (voisins auto) st1 st2);
        false
  with Not_found -> true

let test (stl,_ as auto) =
  let st2 = List.hd stl in
  let st1 = List.hd (List.tl stl) in
  Aorai_option.feedback "test : Etats choisis (%d,%d)" st1.nums st2.nums;
  let (res,_) = dijkstra (voisins auto) st1 st2 in
  Aorai_option.feedback "Fini.@\n%a"
    (Pretty_utils.pp_list ~pre:"@[[" ~sep:",@ " ~suf:"@]]"
       (fun fmt st -> Format.fprintf fmt "%d" st.nums))
    res

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
