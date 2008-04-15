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

open Gobject.Data
open Options

exception No_such_prover

type prover = {
  pr_id : DpConfig.prover_id;
  pr_info : DpConfig.prover_data;
  pr_result : int GTree.column;
  pr_icon : GtkStock.id GTree.column;
  mutable pr_viewcol : GTree.view_column option;
  pr_enc : Options.encoding;
}
  
let cols = new GTree.column_list
let name = cols#add string
let fullname = cols#add string
let parent = cols#add string
let total = cols#add int
let result = cols#add int
let stat = cols#add string
  
let first_row = ref None
    
let simplify = {
  pr_id = DpConfig.Simplify;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = NoEncoding;
  }

let graph = {
  pr_id = DpConfig.Graph;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = NoEncoding;
  }

let simplify_pred = {
  pr_id = DpConfig.Simplify;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = Predicates;
  }
let simplify_strat = {
  pr_id = DpConfig.Simplify;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = Stratified;
  }
let simplify_sstrat = {
  pr_id = DpConfig.Simplify;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = SortedStratified;
  }
let simplify_rec = {
  pr_id = DpConfig.Simplify;
  pr_info = DpConfig.simplify;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = Recursive;
  }

(*
let zenon = {
  pr_name = "Zenon";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Zenon;
  pr_enc = NoEncoding;
}
let zenon_pred = {
  pr_name = "Zenon(P)";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Zenon;
  pr_enc = Predicates;
}
let zenon_strat = {
  pr_name = "Zenon(S)";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Zenon;
  pr_enc = Stratified;
}
let zenon_rec = {
  pr_name = "Zenon(R)";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Zenon;
  pr_enc = Recursive;
}
let harvey = {
  pr_name = "haRVey";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Harvey;
  pr_enc = NoEncoding;
}
let cvcl = {
  pr_name = "CVCL";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Cvcl;
  pr_enc = SortedStratified;
}
let rvsat = {
  pr_name = "rv-sat";
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_id = Dispatcher.Rvsat;
  pr_enc = SortedStratified;
  }
*)
let yices = {
  pr_id = DpConfig.Yices;
  pr_info = DpConfig.yices;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = Monomorph;
  }
let yicesSStrat = {
  pr_id = DpConfig.Yices;
  pr_info = DpConfig.yices;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = SortedStratified;
  }
let ergo = {
  pr_id = DpConfig.Ergo;
  pr_info = DpConfig.alt_ergo;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = NoEncoding;
  }
let ergo_select = {
  pr_id = DpConfig.ErgoSelect;
  pr_info = DpConfig.alt_ergo;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = NoEncoding;
  }
let ergoSS = {
  pr_id = DpConfig.Ergo;
  pr_info = DpConfig.alt_ergo;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = SortedStratified;
  }
let cvc3 = {
  pr_id = DpConfig.Cvc3;
  pr_info = DpConfig.cvc3;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = SortedStratified;
  }
let z3SS = {
  pr_id = DpConfig.Z3;
  pr_info = DpConfig.z3;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = SortedStratified;
  }


let coq = {
  pr_id = DpConfig.Coq;
  pr_info = DpConfig.coq;
  pr_result = cols#add int;
  pr_icon = cols#add GtkStock.conv;
  pr_viewcol = None;
  pr_enc = NoEncoding;
}

  
let provers = [
  ergo; ergo_select; (*ergoSS;*) graph; simplify; z3SS ; yicesSStrat; cvc3; 
  (*simplify_sstrat;*) simplify_strat; yices; 
  coq ;
  (* rvsat; *)
  (* zenon; zenon_pred; zenon_strat; zenon_rec;*)
  (* harvey; cvcl *)]
let provers_selected = ref provers
let provers_s = Hashtbl.create 17
let get_provers () = !provers_selected
let _ = assert (List.length provers > 0)


(*
 * Default prover
 *)
let default_prover = ref (List.hd provers)
let get_default_prover () = !default_prover
let set_prover p = 
  if List.mem p !provers_selected 
  then default_prover := p

let enc_name ~nl n p =
  let nl x = if nl then "\n" ^ x else x in 
  match p.pr_enc with
    | NoEncoding -> 
	begin match p.pr_id with
	  | DpConfig.Graph -> n ^ nl "(Graph)"
	  | DpConfig.ErgoSelect -> n ^ nl "(Select)"
	  | _ -> n ^ nl "" 
	end
    | SortedStratified -> n ^ nl "(SS)"
    | Monomorph -> n ^ nl "(mono)"
    | Recursive -> n ^ nl "(rec)"
    | Stratified -> n ^ nl "(Strat)"
    | Predicates -> n ^ nl "(pred)"

let prover_id p = 
  enc_name ~nl:false p.pr_info.DpConfig.name p

let prover_name_with_version_and_enc p = 
  let v = p.pr_info.DpConfig.version in
  let n = p.pr_info.DpConfig.name in
  let n = if v <> "" then n ^ "\n" ^ v else n ^ "\n(uninstalled)" in
  enc_name ~nl:true n p
	 
let get_prover s = 
  let rec next = function
    | [] -> 
	raise No_such_prover
    | p' :: r -> 
	if prover_id p' = s then p' else next r
  in next !provers_selected

let update_prover_s () = 
  List.iter 
    (fun p -> Hashtbl.add provers_s p "")
    !provers_selected

let add_all_provers () = 
  provers_selected := provers;
  update_prover_s ()

let add_provers l = 
  assert (List.length l > 0);
  provers_selected := 
    List.rev (List.fold_left
      (fun prs pr -> 
	 let n = prover_id pr in
	 if List.mem n l then pr::prs else prs)
      []
      provers);
  if !provers_selected = [] then 
    begin 
      provers_selected := provers
    end;
  default_prover := List.hd !provers_selected;
  List.iter 
    (fun p -> Hashtbl.add provers_s p "")
    !provers_selected

(*
let affiche () = 
  Hashtbl.iter
    (fun p _s -> print_endline (print_prover p))
    provers_s
*)

let select_prover p = 
  if not (Hashtbl.mem provers_s p) then
    Hashtbl.add provers_s p ""

let deselect_prover p = 
  Hashtbl.remove provers_s p

let get_provers_s () = 
  Hashtbl.fold 
    (fun k _v acc -> k::acc)
    provers_s
    []
  
(* all obligations *)
let obligs = Hashtbl.create 97
let find_oblig = Hashtbl.find obligs
  
(* obligation name -> its model row *)
let orows = Hashtbl.create 97
(* obligation name -> its failure messages *)
let fwrows = Hashtbl.create 97
  
(* function -> its model row *)
let frows = Hashtbl.create 17 
let find_fct = Hashtbl.find frows
  
(* function -> list of its obligations *)
let fobligs = Hashtbl.create 97
let find_fobligs = Hashtbl.find fobligs
let iter_fobligs fct f = Queue.iter f (Hashtbl.find fobligs fct)
  
(* functions *)
let fq = Queue.create ()
  
let add_failure row (p:prover) (message:string) = 
  try 
    let messages = Hashtbl.find fwrows row in
    if Hashtbl.mem messages p then
      Hashtbl.replace messages p message
    else Hashtbl.add messages p message
  with Not_found -> begin
    let h = Hashtbl.create 97 in
    Hashtbl.add h p message;
    Hashtbl.add fwrows row h
  end

let _ = 
  let h = Hashtbl.create 1 in
  Hashtbl.add h simplify "";
  Hashtbl.add fwrows " " h;
  Hashtbl.clear h;
  Hashtbl.clear fwrows
    
let create_model () =
  let model = GTree.tree_store cols in
  Dispatcher.iter
    (fun ((_loc,expl,s,_seq) as o) ->
       Hashtbl.add obligs s o;
       let f,n = Tools.decomp_name s in
       let row =
	 try 
	   Hashtbl.find frows f
	 with Not_found ->
	   let row = model#append () in
	   Queue.add f fq;
	   Hashtbl.add frows f row;
	   Hashtbl.add fobligs f (Queue.create ());
	   let fname =
	     if f="" then "User goals" else
	       try
		 let (id,beh,(_f,_l,_b,_e)) = Hashtbl.find Util.program_locs f in
		 id ^ "\n" ^ beh
	       with Not_found -> "why " ^ f
	   in
	   model#set ~row ~column:name fname;
	   model#set ~row ~column:fullname f;
	   model#set ~row ~column:parent f;
	   model#set ~row ~column:total 0;
	   List.iter 
	     (fun p -> model#set ~row ~column:p.pr_result 0) 
	     provers;
	   row
       in
       let row_n = model#append ~parent:row () in
       (match !first_row with None -> first_row := Some(row_n) | Some _ -> ());
       Hashtbl.add orows s row_n;
       Queue.add row_n (Hashtbl.find fobligs f);
       let msg =
	 match expl.Logic_decl.vc_kind with
	   | Logic_decl.EKLemma -> "Lemma " ^ expl.Logic_decl.lemma_or_fun_name
	   | k -> Explain.msg_of_kind k
       in
       model#set ~row:row_n ~column:name (if f="" then msg else (n^". "^msg));
       model#set ~row:row_n ~column:fullname s;
       model#set ~row:row_n ~column:parent f;
       model#set ~row:row_n ~column:result 0;
       List.iter
	 (fun p -> model#set ~row:row_n ~column:p.pr_icon `REMOVE)
	 provers
    );
  model
    
