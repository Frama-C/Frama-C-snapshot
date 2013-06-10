(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(** Compute dominator information for the statements in a function *)
open Cil_types
module IH = Datatype.Int.Hashtbl

module DF = Dataflow

let debug = false

(* For each statement we maintain a set of statements that dominate it *)
module BS = Cil_datatype.Stmt.Hptset

(** Customization module for dominators *)
module DT = struct
  let name = "dom"

  let debug = ref debug

  type t = BS.t

  module StmtStartData =
    Dataflow.StartData(struct type t = BS.t let size = 17 end)
   (** For each statement in a function we keep the set of dominator blocks.
    * Indexed by statement id *)

  let copy (d: t) = d

  let pretty fmt (d: t) =
    Pretty_utils.pp_list ~pre:"@[{" ~sep:",@," ~suf:"}@]"
      (fun fmt s -> Format.fprintf fmt "%d" s.sid)
      fmt (BS.elements d)

  let computeFirstPredecessor (s: stmt) (d: BS.t) : BS.t =
    (* Make sure we add this block to the set *)
    BS.add s d

  let combinePredecessors (s: stmt) ~(old: BS.t) (d: BS.t) : BS.t option =
    (* First, add this block to the data from the predecessor *)
    let d' = BS.add s d in
    if BS.subset old d' then
      None
    else
      Some (BS.inter old d')

  let doInstr _ (_i: instr) (_d: t) = DF.Default

  let doStmt (_s: stmt) (_d: t) = DF.SDefault

  let doGuard _ _condition _ = DF.GDefault, DF.GDefault


  let filterStmt _ = true

  let stmt_can_reach _ _ = true

  let doEdge _ _ d = d
end

module NDom = struct

    
  module G = struct

    module V = struct
      type t = stmt
      let equal s1 s2 = (s1.sid == s2.sid) (* sid unique in same function *)
      let hash s1 = s1.sid
      let compare s1 s2 = Pervasives.compare s1.sid s2.sid
    end

(*
    module E = struct
      type t = V.t * V.t (* (src,dest) *)
      let src e = fst e
      let dst e = snd e
      let make src dst = (src,dst)
      let equal e1 e2 = (V.equal (fst e1) (fst e2)) && (V.equal (snd e1) (snd e2))
      let hash e = V.hash (fst e) + 971 * V.hash (snd e)
      let compare e1 e2  = 
	let cmp = V.compare (src e1) (src e2) in
	if (cmp == 0) then V.compare (dst e1) (dst e2)
	else cmp
    end
*)
    
    let get_start_vertices (f : fundec) = 
      match f.sbody.bstmts with 
	| [] -> []
	| start :: _ -> [ start ]
	  
    let succs stmt = stmt.succs
    let preds stmt = stmt.preds
  end

  module HV = Hashtbl.Make(G.V)
  module SV = Set.Make(G.V)
(*  module SE = Set.Make(G.E) *)

  module MI = Datatype.Int.Map

  module DFS = struct
    (* depth-first search enabling *)
    (* also tags exit nodes (i.e. nodes which have no successor in the DFS tree,
       and are Return statements) *)
    (* TODO: memoize per fundec ;
       also won't compute if there are statements unreachable from
       the first statement of the function, e.g. after a while(1) 
    *)


    let build ( f:fundec ) = 
      let depth_last_comp_parent_tbl = HV.create 997 
      and is_exit_list = ref []
      and todo_set = ref SV.empty
      and isdone_set = ref SV.empty
      and time = ref 0 
      and ccomp = ref 0 in
      let rec search v voption = 
	let lsuccs = G.succs v in
	HV.add depth_last_comp_parent_tbl v (!time,-1,!ccomp,voption)
	;
	incr time
	;
	todo_set := SV.remove v !todo_set
	;
	isdone_set := SV.add v !isdone_set
	;
	let is_exit = ref true in
	List.iter 
	  (fun w -> 
	    if not(SV.mem w !isdone_set) then (
	      is_exit := false ;
	      search w (Some v)
	    )
	  ) lsuccs
	;
(*
	( match lsuccs with 
	  | [] -> is_exit_list := v :: !is_exit_list 
	  | [ w ] when G.V.equal v w  (* case while(1) ; *) 
	    -> is_exit_list := v :: !is_exit_list 
	  | _ -> ()

	)
	;
*)
	if !is_exit then is_exit_list := v :: !is_exit_list ;
	let h = HV.find depth_last_comp_parent_tbl v
	in match h with (d,_,c,o) ->
	  HV.replace depth_last_comp_parent_tbl v (d,!time,c,o) 
	  ;
	  Kernel.debug "depth of vertex %d is %d" v.sid d (*;
	  incr time*)
      in 
      match G.get_start_vertices f with
	| [] -> None
	| start_v :: _ -> 		    
	  List.iter (fun v -> todo_set := SV.add v !todo_set) (G.get_start_vertices f) ;
	  while not(SV.is_empty !todo_set) do
	    let v = SV.choose !todo_set
	    in 
	    search v None
	    ;
	    incr ccomp
	  done
	  ;
	  (* order wrt depth *)
	  let depth_to_vertex = 
	    HV.fold (fun (v : G.V.t) info acc_map ->
	      match info with (d,_,_,_) -> MI.add d v acc_map) 
	      depth_last_comp_parent_tbl 
	      MI.empty in
	  let depths_vec = 
	    Array.make (HV.length depth_last_comp_parent_tbl) start_v  in
	  let _ =
	  MI.fold (fun _d v acc_cnt ->
	    depths_vec.(acc_cnt) <- v ;
	    acc_cnt +1) 
	    depth_to_vertex 0    
	  in 
	  Some (depths_vec, depth_last_comp_parent_tbl, !is_exit_list)
  end

  module PostDominator = struct

    exception ExitFound

    type pdom_vertex = Exit | V of G.V.t

    let build (f : fundec) = 
      let pdoms = HV.create 997 in
      match DFS.build f with
	| None -> None
	| Some (depths_vec, depth_last_comp_parent_tbl, exit_vertices_list) ->
	  let reverse_dfs_prefix (f : G.V.t -> unit) = 
	    for i = Array.length depths_vec -1 downto 0 do
	      f (Array.unsafe_get depths_vec i)
	    done      
	  in
	  let get_depth (v : G.V.t) = 
	    try
	      let (d,_,_,_) = HV.find depth_last_comp_parent_tbl v in 
	      d
	    with Not_found -> assert false
	  in
	  let is_exit_vertex (v : G.V.t) = 
	    try 
	      let _ = List.find (G.V.equal v) exit_vertices_list 
	      in true
	    with Not_found -> false
	  in
	  List.iter
	    (fun v -> HV.add pdoms v (V v))
	    exit_vertices_list ;
	  let rchanged = ref true
	  and max_finger = (Array.length depths_vec) + 1 in
	  let intersect b1 b2 = 
	    let finger1 = ref b1
	    and finger2 = ref b2 
	    and dfinger1 = ref (get_depth b1)
	    and dfinger2 = ref (get_depth b2) in
	    Kernel.debug "intersect sid %d with sid %d" b1.sid b2.sid ;
	    while(!dfinger1 <> !dfinger2) do
	      Kernel.debug "finger1 = %d finger2 = %d" !finger1.sid !finger2.sid ;
	      while (!dfinger1 < !dfinger2) do
		Kernel.debug "*) finger1 = %d(%d) finger2 = %d(%d)" 
		  !finger1.sid !dfinger1 !finger2.sid !dfinger2 ;
		let dom1 = HV.find pdoms !finger1 in
		Kernel.debug "looking for dom of %d" !finger1.sid ;
		match dom1 with 
		    Exit -> raise ExitFound
		  | V v1 -> 
		    Kernel.debug "found %d\n" v1.sid ;
		    if (G.V.equal v1 !finger1) then 
		      (
			Kernel.debug "Same node max_finger = %d" max_finger ;
			dfinger1 := max_finger 
		      )
		    else (
		      finger1 := v1 ; dfinger1 := get_depth v1 ;
		      Kernel.debug "* NSN %d(%d)" v1.sid (get_depth v1) 
		    )
	      done
	      ;
	      Kernel.debug "half intersect dfinger1 = %d dfinger2 = %d" 
		!dfinger1 !dfinger2 ;
	      while (!dfinger2 < !dfinger1) do
		Kernel.debug "+) finger1 = %d(%d) finger2 = %d(%d)" 
		  !finger1.sid !dfinger1 !finger2.sid !dfinger2;
		let dom2 = HV.find pdoms !finger2
		in
		Kernel.debug "looking for dom of %d" !finger2.sid ;
		match dom2 with 
		    Exit -> raise ExitFound
		  | V v2 -> 
		    Kernel.debug "** found %d\n" v2.sid ;
		    if (G.V.equal v2 !finger2) then
		      (
			Kernel.debug "Same node max_finger = %d" max_finger ;
			dfinger2 := max_finger
		      )
		    else (
		      Kernel.debug "** NSN";
		      finger2 := v2 ; dfinger2 := get_depth v2
		    )
	      done
	    done
	    ;
	    Kernel.debug "end intersect dfinger1 = %d dfinger2 = %d" !dfinger1 !dfinger2 ;
	    !finger1
	  in 
	  while(!rchanged) do
	    rchanged := false;
	    reverse_dfs_prefix 
	      (fun b ->
		Kernel.debug "treating %d" b.sid ;
		if not(is_exit_vertex b) then (
		  let lsuccs = List.filter (fun v -> HV.mem pdoms v) (G.succs b) in  
		  match lsuccs with
		    | [] -> assert false		      
		    | hd :: tl_succs -> 
		      let rnew_ipdom = ref hd
		      and ripdom = ref Exit in
		      ( 
			try
			  List.iter 
			    (fun p -> 
			      if (HV.mem pdoms p) then 
				rnew_ipdom :=  intersect p !rnew_ipdom) 
			    tl_succs
			  ;
			  Kernel.debug "new pdom of %d is %d\n" b.sid !rnew_ipdom.sid ;
			  ripdom := V !rnew_ipdom
			with ExitFound -> 
			  Kernel.debug "ExitFound for pdom of %d" b.sid;
			  ripdom := Exit
		      )
		      ;
		      if not(HV.mem pdoms b) then (
			HV.add pdoms b !ripdom
			;
			rchanged := true
		      ) else (
			match HV.find pdoms b, !ripdom with
			  | Exit, Exit -> ()
			  | V v1, V v2 when G.V.equal v1 v2 -> ()
			  | _ -> 
			    try
			      HV.replace pdoms b !ripdom
			      ;		
			      rchanged := true
			    with Not_found -> ()
		      )			  
		) else Kernel.debug "%d is an exit vertex" b.sid 
	      )
	  done ;
	  Kernel.debug "end of Pdom comp";
	  let pdoms_final = HV.create 997 in
	  for i = Array.length depths_vec -1 downto 0 do
	    let v = Array.unsafe_get depths_vec i in
	    let pdom_v = HV.find pdoms v in
	    match pdom_v with
	      | Exit -> HV.add pdoms_final v None
	      | V w -> HV.add pdoms_final v (Some w)
	  done ;
	  
	  Some pdoms_final


  end

  module Dominator = struct

    let build (f : fundec) = 
      (* based on "A Simple, Fast Dominance Algorithm" by K.D. Cooper et al *)
      let doms = HV.create 997 in
      match DFS.build f with
	| None -> None
	| Some (depths_vec, depth_last_comp_parent_tbl, _) ->
	  let dfs_prefix (f : G.V.t -> unit) = 
	    for i = 0 to Array.length depths_vec -1 do
	      f (Array.unsafe_get depths_vec i)
	    done
	  in 
	  let get_depth (v : G.V.t) = 
	    try
	      let (d,_,_,_) = HV.find depth_last_comp_parent_tbl v in d
	    with Not_found -> assert false
	  in
	  let is_start_vertex (v : G.V.t) = 
	    G.V.equal v (Array.unsafe_get depths_vec 0)
	  in
	  List.iter 
	    (fun v -> HV.add doms v (Some v)) 
	    (G.get_start_vertices f) ; 
	  let rchanged = ref true
	  and intersect b1 b2 = 
	    let finger1 = ref b1
	    and finger2 = ref b2 
	    and dfinger1 = ref (get_depth b1)
	    and dfinger2 = ref (get_depth b2)
	    in
	    while(!dfinger1 <> !dfinger2) do
	      while (!dfinger1 > !dfinger2) do
		let dom1 = HV.find doms !finger1
		in
		match dom1 with 
		    None -> assert false
		  | Some v1 -> finger1 := v1 ; dfinger1 := get_depth v1
	      done
	      ;
	      while (!dfinger2 > !dfinger1) do
		let dom2 = HV.find doms !finger2
		in
		match dom2 with 
		    None -> assert false
		  | Some v2 -> finger2 := v2; dfinger2 := get_depth v2
	      done
	    done
	    ;
	    !finger1
	  in 
	  while(!rchanged) do
	    rchanged := false;
	    dfs_prefix 
	      (fun b ->
		if not(is_start_vertex b) then (
		  let lpreds = 
		    List.filter (fun v -> HV.mem doms v) 
		      (G.preds b) in
		  try
		    let rnew_idom = ref (List.hd lpreds)
		    and ridom = ref None in
		    List.iter 
		      (fun p -> 
			if (HV.mem doms p) then 
			  rnew_idom :=  intersect p !rnew_idom) 
		      (List.tl lpreds)
		    ;
		    ridom := Some !rnew_idom
		    ;
		    if not(HV.mem doms b) then (
		      HV.add doms b !ridom
		      ;
		      rchanged := true
		    ) else (
		      match HV.find doms b, !ridom with
			| None, None -> ()
			| Some v1, Some v2 when G.V.equal v1 v2 -> ()
			| _ -> 
			  try
			    HV.replace doms b !ridom
			    ;		
			    rchanged := true
			  with Not_found -> ()
		    )			  
		  with Not_found -> 
		    assert false
		)
	      )
	  done ;
	  List.iter 
	    (fun v -> HV.replace doms v None)
	    (G.get_start_vertices f) ; 

	  Some doms
  end

end

module Dom = Dataflow.Forwards(DT)

let getStmtDominators (s: stmt) : BS.t =
  try DT.StmtStartData.find s
  with Not_found -> BS.empty (* Not reachable *)

let getIdom (idomInfo: stmt option IH.t) (s: stmt) =
  try IH.find idomInfo s.sid
  with Not_found ->
    Kernel.fatal "Immediate dominator information not set for statement %d" s.sid

(** Check whether one block dominates another. This assumes that the "idom"
    * field has been computed. *)
let rec dominates idomData (s1: stmt) (s2: stmt) =
  s1.sid = s2.sid ||
  (let s2idom = fillOneIdom idomData s2 in
   match s2idom with
       None -> false
     | Some s2idom -> dominates idomData s1 s2idom)

(* Now fill the immediate dominators for all nodes *)
and fillOneIdom idomData (s: stmt) =
  try
    IH.find idomData s.sid
  (* Already set *)
  with Not_found -> begin
    (* Get the dominators *)
    let sdoms = getStmtDominators s in
    (* Fill the idom for the dominators first *)
    let idom =
      BS.fold
        (fun d (sofar: stmt option) ->
          if d.sid = s.sid then
            sofar (* Ignore the block itself *)
          else begin
            match sofar with
		None -> Some d
              | Some sofar' ->
                 (* See if d is dominated by sofar. We know that the
                  * idom information has been computed for both sofar
                  * and for d*)
                 if dominates idomData sofar' d then
                   Some d
                 else
                   sofar
           end)
        sdoms
        None
    in
    IH.replace idomData s.sid idom;
    idom
  end

let computeIDom (f: fundec) : stmt option IH.t =
  (* CEA : DO NOT DO IT AGAIN
  (* We must prepare the CFG info first *)
     prepareCFG f;
     computeCFGInfo f false;
  *)
  let compute_ipdom () = 
    match NDom.PostDominator.build f with
      | None -> IH.create 13 
      | Some pdoms -> 
	let sz = NDom.HV.length pdoms in 
	let ipdomData : stmt option IH.t = IH.create sz in
	NDom.HV.iter 
	  (fun stmt ipdom_opt ->
	    IH.add ipdomData stmt.sid ipdom_opt ) pdoms
	;
	ipdomData
  in
  let compute_idom_new () = 
    match NDom.Dominator.build f with
      | None -> IH.create 13 
      | Some doms -> 
	let sz = NDom.HV.length doms in 
	let idomData : stmt option IH.t = IH.create sz in
	NDom.HV.iter 
	  (fun stmt idom_opt ->
	    IH.add idomData stmt.sid idom_opt ) doms
	;
	idomData
  in 
  let _compute_idom_old () = 
    DT.StmtStartData.clear ();
    let idomData : stmt option IH.t = IH.create 13 in
    let _ =
      match f.sbody.bstmts with
	  [] -> () (* function has no body *)
	| start :: _ -> begin
        (* We start with only the start block *)
          DT.StmtStartData.add start (BS.singleton start);

          Dom.compute [start];
        (* Dump the dominators information *)
          if debug then
            List.iter
              (fun s ->
		let sdoms = getStmtDominators s in
		if not (BS.mem s sdoms) then begin
                 (* It can be that the block is not reachable *)
                  if s.preds <> [] then
                    (Kernel.error "Statement %d is not in its list of dominators"
                       s.sid);
		end;
		Kernel.debug "Dominators for %d: %a\n" s.sid
                  DT.pretty (BS.remove s sdoms))
              f.sallstmts;
        (* Scan all blocks and compute the idom *)
          List.iter (fun x -> ignore (fillOneIdom idomData x)) f.sallstmts
	end 
    in
    idomData
  in
  let _check_equivalence idomData idomData_mine = 
    (* compare results of old vs new algorithm *)
    IH.iter (fun stmt_sid idom_opt ->
      try
	let idom_opt2 = IH.find idomData_mine stmt_sid in
	match idom_opt,idom_opt2 with
	  | None , None  -> ()
	  | Some v1, Some v2 when (v1.sid == v2.sid) -> ()
	  | None, Some v -> Kernel.debug "no former idom for %d / new idom is %d"
	    (stmt_sid) v.sid
	  | Some v, None -> Kernel.debug "former idom for %d was %d / no new idom"
	    (stmt_sid) v.sid
	  | Some v1, Some v2 -> Kernel.debug "former idom for %d=%d / new idom is %d"
	    stmt_sid v1.sid v2.sid
      with Not_found -> (
	match idom_opt with
	  | None -> ()
	  | Some v -> Kernel.debug "no idom for %d (former was %d)" stmt_sid v.sid
      )
    ) idomData;
    IH.iter (fun stmt_sid idom_opt ->
      try
	let idom_opt2 = IH.find idomData stmt_sid in
	match idom_opt,idom_opt2 with
	  | None , None  -> ()
	  | Some v1, Some v2 when (v1.sid == v2.sid) -> ()
	  | None, Some v -> Kernel.debug "no new idom for %d / former idom is %d"
	    (stmt_sid) v.sid
	  | Some v, None -> Kernel.debug "new idom for %d was %d / no former idom"
	    (stmt_sid) v.sid
	  | Some v1, Some v2 -> Kernel.debug "new idom for %d=%d / former idom is %d"
	    stmt_sid v1.sid v2.sid
      with Not_found -> (
	match idom_opt with
	  | None -> ()
	  | Some v -> Kernel.debug "no idom for %d (new is %d)" stmt_sid v.sid
      )
    ) idomData_mine      
  in let check_pdoms pdoms = 
       IH.iter (fun stmt_sid ipdom_opt ->
	 match ipdom_opt with
	   | None -> Kernel.debug "no pdom for %d" stmt_sid
	   | Some v -> Kernel.debug "pdom for %d is %d" stmt_sid v.sid
       ) pdoms
     in let doms = compute_idom_new ()
     and pdoms=  compute_ipdom () in     
	check_pdoms pdoms ; 
	doms
  (* this code can be used to check both algorithms
     give the same result:

  let data1 =
    compute_idom_new ()
  and data2 = 
    _compute_idom_old ()
  in 
  _check_equivalence data1 data2 ;
  data1
  *)
  


(** Compute the start of the natural loops. For each start, keep a list of
 * origin of a back edge. The loop consists of the loop start and all
 * predecessors of the origins of back edges, up to and including the loop
 * start *)
let findNaturalLoops (f: fundec)
                     (idomData: stmt option IH.t) : (stmt * stmt list) list =
  let loops =
    List.fold_left
      (fun acc b ->
        (* Iterate over all successors, and see if they are among the
         * dominators for this block *)
        List.fold_left
          (fun acc s ->
            if dominates idomData s b then
              (* s is the start of a natural loop *)
              let rec addNaturalLoop = function
                  [] -> [(s, [b])]
                | (s', backs) :: rest when s'.sid = s.sid ->
                    (s', b :: backs) :: rest
                | l :: rest -> l :: addNaturalLoop rest
              in
              addNaturalLoop acc
            else
              acc)
          acc
          b.succs)
      []
      f.sallstmts
  in

  if debug then
    begin
      let pp_back fmt b = Format.pp_print_int fmt b.sid in
      let pp_loop fmt (s,backs) =
	Format.fprintf fmt "Start:%d, backs:%a"
	  s.sid (Pretty_utils.pp_list pp_back) backs in
      Kernel.debug
	"Natural loops:\n%a"
	(Pretty_utils.pp_list ~sep:"@\n" pp_loop)
	loops
    end ;
  loops
