(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

module type INPUT = sig
  
  module Label : sig
    type t
    val create : unit -> t
    val equal : t -> t -> bool
    val hash : t -> int
    val to_string : t -> string
  end

  type predicate
    
  val ptrue : predicate

  val string_of_predicate : predicate -> string

  type statement
    
  val void_stmt : statement

  val append_stmt : statement -> statement -> statement

  val assert_stmt : predicate -> statement
    
  val string_of_stmt : statement -> string

end

module Make(X : INPUT) = struct

  type asm =
    | Ainvariant of X.predicate
    | Ajump of X.Label.t
    | Acond of X.Label.t * X.statement * X.statement
    | Ahalt
    | Aother of X.statement

  type asm_code = (X.Label.t * asm) list
  
  type seq_code = {
    seq_pre : X.predicate option;
    seq_code : X.statement;
  }

  (* phase 1: from assembly code to CFG *)

  type node_kind =
    | Ninvariant of X.predicate
    | Nother

  type node_status =
    | NStodo
    | NSinprogress
    | NSdone of seq_code list

  type node = { 
    node_id : int;
    node_name : X.Label.t;
    node_kind : node_kind;
    mutable node_status : node_status;
  }

  type graph = node -> (node * X.statement) list

  module HL = Hashtbl.Make(X.Label)
      
  module HN = Hashtbl.Make(struct
			     type t = node
			     let hash n = Hashtbl.hash n.node_id
			     let equal n1 n2 = n1.node_id = n2.node_id
			   end)

  let make_cfg asm init =
    (* first pass: find out labels which are node starting points *)
    let node_labels = HL.create 97 in
    HL.add node_labels init ();
    List.iter (fun (_, a)-> match a with
		 | Ainvariant _ | Ahalt | Aother _ -> ()
		 | Ajump l | Acond (l,_,_) -> HL.add node_labels l ()) asm;
    (* second pass: create nodes *)
    let nodes = HL.create 97 in
    let create_node =
      let id = ref 0 in
      fun lab kind -> 
	incr id; 
	let node = { node_id = !id; node_name = lab; 
		     node_kind = kind; node_status = NStodo } in
	HL.add nodes lab node;
	node
    in
    let lcfg = HL.create 97 in (* label -> (label * stmt) list *)
    let rec descend st = function
      | (l, Aother st1) :: asm when not (HL.mem node_labels l) ->
 	  descend (X.append_stmt st st1) asm
      | asm ->
	  st, asm
    in
    let rec make_nodes = function
      | [] -> 
	  (* end of code -> dummy node (~ halt) *)
	  let l = X.Label.create () in
	  let _ = create_node l Nother in
	  HL.add lcfg l [];
	  l
      | (l, Ainvariant i) :: asm ->
	  (* invariant -> create a node *)
	  let _ = create_node l (Ninvariant i) in
	  let m = make_nodes asm in
	  HL.add lcfg l [m, X.void_stmt];
	  l
      | (l, a) :: asm  ->
	  let _ = create_node l Nother in
	  begin match a with
	    | Ainvariant _ -> 
		assert false
	    | Ahalt -> 
		HL.add lcfg l []; 
		ignore (make_nodes asm)
	    | Ajump l1 -> 
		HL.add lcfg l [l1, X.void_stmt]; 
		ignore (make_nodes asm)
	    | Acond (lt, st, sf) ->
		let lf = make_nodes asm in
		HL.add lcfg l [lt, st; lf, sf]
	    | Aother s ->
		let s,asm = descend s asm in
		let l1 = make_nodes asm in
		HL.add lcfg l [l1, s]
	  end;
	  l
    in
    ignore (make_nodes asm);
    (* finally create the CFG *)
    let cfg = HN.create 97 in
    let node l = try HL.find nodes l with Not_found -> assert false in
    HL.iter (fun l succ -> 
	       HN.add cfg (node l) 
		 (List.map (fun (l1,s1) -> (node l1, s1)) succ)) lcfg;
    HN.find cfg, node init

  (* debug *)

  open Format

  let print_cfg fmt cfg init =
    let visited = HN.create 17 in
    let rec dfs n =
      if not (HN.mem visited n) then begin
	HN.add visited n ();
	fprintf fmt "node %s:@\n" (X.Label.to_string n.node_name);
	let succ = cfg n in
	List.iter 
	  (fun (m,s) -> 
	    fprintf fmt "  %s -> %s@\n" (X.string_of_stmt s) 
	      (X.Label.to_string m.node_name)) succ;
	List.iter (fun (m,_) -> dfs m) succ
      end
    in
    dfs init

  let display_cfg cfg init =
    let c = open_out "cfg.dot" in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "digraph cfg {@\n";
    let visited = HN.create 17 in
    let rec dfs n =
      if not (HN.mem visited n) then begin
	HN.add visited n ();
	let name_n = X.Label.to_string n.node_name in
	let succ = cfg n in
	List.iter 
	  (fun (m,s) -> 
	    let name_m = X.Label.to_string m.node_name in
	    fprintf fmt "  %s -> %s;@\n" name_n name_m) 
	  succ;
	List.iter (fun (m,_) -> dfs m) succ
      end
    in
    dfs init;
    fprintf fmt "}@.";
    close_out c

  (* phase 2: from CFG to purely sequential programs *)

  module S = struct
    include Set.Make(struct type t = int let compare = compare end)
    let add n = add n.node_id
    let mem n = mem n.node_id
  end

(***
  let transform g init =
    let invariants = HN.create 17 in
    let seq = ref [] in
    let rec dfs visited path n = match n.node_kind with
      | Ninvariant inv ->
	  seq := (Sassert inv :: path) :: !seq;
	  if not (H.mem invariants n) then dfs0 n
      | Nassert _ when S.mem n visited ->  
	  failwith "loop without any invariant"
      | Nassert p ->
	  dfs_children visited (Sassert p :: path) n
    and dfs_children visited path n =
      let children = g n in
      if children = [] then 
	seq := path :: !seq
      else
	let visited = S.add n visited in
	List.iter (fun (m,s) -> dfs visited (Sstmt s :: path) m) (g n)
    and dfs0 n = 
      H.add invariants n ();
      let path = match n.node_kind with
	| Ninvariant inv -> [Spre inv]
	| Nassert p -> [Sassert p]
      in
      dfs_children S.empty path n
    in
    dfs0 init;
    !seq
***)

  let make_seq cfg init =
    let invariants = HN.create 17 in
    let todo = ref [init] in
    let invariant n = match n.node_status with
      | NStodo -> HN.add invariants n (); todo := n :: !todo
      | _ -> ()
    in
    let rec dfs n = 
      if n.node_status = NSinprogress then 
	failwith ("loop without any invariant: " ^ 
		     X.Label.to_string n.node_name);
      if n.node_status = NStodo then begin
	let pre = match n.node_kind with 
	  | Ninvariant i -> n.node_status <- NSdone []; Some i 
	  | Nother -> n.node_status <- NSinprogress; None 
	in 
	let code s = { seq_pre = pre; seq_code = s } in
	let cl = match cfg n with
	  | [] -> 
	      [[code X.void_stmt]]
	  | sl ->
	      List.map
		(fun (m,s) -> match m.node_kind with
		   | Ninvariant i ->
		       invariant m; 
		       let s1 = X.append_stmt s (X.assert_stmt i) in
		       [code s1]
		   | Nother ->
		       dfs m;
		       match m.node_status with
			 | NSdone cl ->
			     List.map 
			       (fun c -> code (X.append_stmt s c.seq_code)) cl
			 | _ ->
			     assert false)
		sl
	in
	n.node_status <- NSdone (List.flatten cl)
      end
    in
    let rec loop () = match !todo with
      | [] -> ()
      | n :: l -> todo := l; dfs n; loop ()
    in
    loop ();
    let code n = match n.node_status with NSdone c -> c | _ -> assert false in
    HN.fold (fun n _ acc -> code n @ acc) invariants (code init)

  let transform ~show_graph asm init =
    let cfg,ninit = make_cfg asm init in
    if show_graph then begin
      print_cfg err_formatter cfg ninit;
      display_cfg cfg ninit
    end;
    make_seq cfg ninit

end

