(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Computes a TODO-list for properties                                --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Properties_status

type proof = {
  p_emitter : string ;
  p_pending : Property.t list ; (* not yet complete *)
}

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let compare_proof p1 p2 =
  let n1 = List.length p1.p_pending in
  let n2 = List.length p2.p_pending in
  let c = Datatype.Int.compare n1 n2 in
  if c<>0 then c else String.compare p1.p_emitter p2.p_emitter

(* -------------------------------------------------------------------------- *)
(* --- Consolidation calculus with Memoization                            --- *)
(* -------------------------------------------------------------------------- *)

module Hip = Property.Hashtbl

let rec get_proofs hmap ip : proof list =
  try Hip.find hmap ip
  with Not_found ->
    Hip.add hmap ip [] ;
    (* Force dependencies to be computed *)
    ignore (Properties_status.strongest ip) ;
    (* Now report *)
    let ctree = Consolidation_tree.get ip in
    let proofs =
      List.fold_left
	(fun ps s ->
	   match s.Consolidation_tree.value with
	     | Checked {valid=True;emitter=e} , _ ->
		 let hs =
		   List.filter
		     (fun s -> not (is_complete hmap s))
		     s.Consolidation_tree.hypothesis
		 in
		 let hips =
		   List.sort
		     Property.compare
		     (List.map (fun h -> h.Consolidation_tree.property) hs)
		 in
		 { p_emitter=e ; p_pending=hips } :: ps
	     | _ -> ps
	) [] ctree.Consolidation_tree.status in
    let ordered = List.sort compare_proof proofs in
    Hip.replace hmap ip ordered ; ordered

and is_complete hmap (s : Consolidation_tree.t) =
  List.exists
    (fun p -> p.p_pending=[])
    (get_proofs hmap s.Consolidation_tree.property)

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

module Self = Plugin.Register
  (struct
     let name = "report"
     let shortname = "report"
     let help = "Properties Status Report (experimental)"
   end)

module Enabled =
  Self.Action(struct
		let option_name = "-report"
		let help = "display a summary of properties status"
		let kind = `Tuning
	      end)

module Emitter =
  Self.False(struct
	       let option_name = "-report-emitter"
	       let help = "display the list of emitters for available proofs"
               let kind = `Tuning
	     end)

module Pending =
  Self.False(struct
	       let option_name = "-report-pending"
	       let help = "display the list of pending properties for partial proofs"
               let kind = `Tuning
	     end)

module OnlyValid =
  Self.False(struct
	       let option_name = "-report-valid"
	       let help = "only report on validated properties"
	       let kind = `Tuning
	     end)

let txt_unknown = " Unknown "
let txt_valid   = "  Valid  "
let txt_partial = " Partial "
let bar = String.make 60 '-'

let report ~partials ~completes ~untried hmap fmt title ips =
  if ips <> [] then
    begin
      Format.fprintf fmt "%s@\n   %s@\n%s@\n@\n"
	bar title bar ;
      let ips = List.sort Property.compare ips in
      List.iter
	(fun ip ->
	   let proofs = get_proofs hmap ip in
	   let status =
	     if proofs = [] then
	       ( incr untried ; txt_unknown )
	     else
	       if List.exists (fun p -> p.p_pending=[]) proofs
	       then ( incr completes ; txt_valid )
	       else ( incr partials ; txt_partial )
	   in
	   let pkf = Property.get_kf ip in
	   (match pkf with
	   | None ->
	     Format.fprintf fmt "[%s] Global @[%a@]@\n"
	       status Property.pretty ip
	   | Some kf ->
	     Format.fprintf fmt "[%s] Function '%s' @[%a@]@\n"
	       status (Kernel_function.get_name kf) Property.pretty ip);
	   if Pending.get () || Emitter.get () then
	     List.iter
	       (fun p ->
		  begin
		    match Emitter.get () , Pending.get () , p.p_pending with
		      | _ , _ , [] ->
			  Format.fprintf fmt "     Emitter %s [complete]@\n"
			    p.p_emitter
		      | true , false , hs ->
			  Format.fprintf fmt "     Emitter %s [%d pending]@\n"
			    p.p_emitter (List.length hs) ;
		      | _ , true , _ ->
			  Format.fprintf fmt "     Emitter %s:@\n"
			    p.p_emitter
		      | false , false , _ -> assert false (* englobing 'if' *)
		  end ;
		  if Pending.get () then
		    List.iter
		      (fun h ->
			 match Property.get_kf h , pkf with
			 | Some hkf, Some kf when Kernel_function.equal hkf kf ->
			   Format.fprintf fmt "      - @[pending %a@]@\n"
			     Property.pretty h
			   | Some hkf , _ ->
			       Format.fprintf fmt "      - @[pending %a@ from function '%s'@]@\n"
				 Property.pretty h (Kernel_function.get_name hkf)
			   | None , _ ->
			       Format.fprintf fmt "      - @[pending global %a@]@\n"
				 Property.pretty h)
		      p.p_pending ;
		  Format.pp_print_flush fmt ()
	       ) proofs
	) ips ;
      Format.pp_print_newline fmt () ;
    end

let print () =
  begin
    let hmap = Hip.create 131 in
    let forest = Consolidation_tree.get_all () in
    let globals = ref [] in
    let reports = ref Kernel_function.Map.empty in
    Self.feedback "Computing properties status..." ;
    List.iter
      (fun t ->
	 let ip = t.Consolidation_tree.property in
	 let proofs = get_proofs hmap ip in
	 if (if OnlyValid.get () then proofs <> [] else true) then
	   match Property.get_kf ip with
	   | None -> globals := ip :: !globals
	   | Some kf ->
	     let ipfs =
	       try Kernel_function.Map.find kf !reports with Not_found -> []
	     in
	     reports := Kernel_function.Map.add kf (ip::ipfs) !reports)
      forest;
    Log.print_on_output "%t"
      (fun fmt ->
	 if !globals = [] && Kernel_function.Map.is_empty !reports then
	   Format.fprintf fmt "No properties status@." ;
	 let partials = ref 0 in
	 let completes = ref 0 in
	 let untried = ref 0 in
	 report ~partials ~completes ~untried hmap fmt "Global Properties" !globals ;
	 Kernel_function.Map.iter
	   (fun kf ips ->
	      let title =
		Printf.sprintf "Properties for Function '%s'"
		  (Kernel_function.get_name kf)
	      in report ~partials ~completes ~untried hmap fmt title ips
	   ) !reports ;
	 let s = !untried + !partials + !completes in
	 Format.fprintf fmt "%s@\n" bar ;
	 Format.fprintf fmt "  No proofs       : %4d@\n" !untried ;
	 Format.fprintf fmt "  Partial proofs  : %4d@\n" !partials ;
	 Format.fprintf fmt "  Complete proofs : %4d@\n" !completes ;
	 Format.fprintf fmt "  Total           : %4d@\n" s ;
	 Format.fprintf fmt "%s@." bar ;
      ) ;
  end

let main () = 
  if Enabled.get () then 
    begin
      print () ;
      Enabled.clear () ; (* Hack for not printing the report after -then *)
    end

let () =
  begin
    Db.Report.print := print ;
    Db.Main.extend main ;
  end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
