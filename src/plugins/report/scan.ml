(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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

(* -------------------------------------------------------------------------- *)
(* --- Iterator for Report                                                --- *)
(* -------------------------------------------------------------------------- *)

open Property_status
module E = Emitter.Usable_emitter

class type inspector =
object

  method empty : unit
  method started : unit
  method global_section : unit
  method function_section : Kernel_function.t -> unit
  method property : Property.t -> Consolidation.t -> unit
  method finished : unit
    
end

let dead_reasons (ps:Consolidation.pending) =
  E.Map.fold 
    (fun _ -> E.Map.fold (fun _ -> Property.Set.union))
    ps Property.Set.empty

let partial_pending (ps:Consolidation.pending) =
  E.Map.map
    (fun best -> E.Map.fold 
       (fun _ -> Property.Set.union)
       best Property.Set.empty)
    ps


(** Filters on which properties should be reported *)

(* someone has tried to prove [ip], or show ips that no one attempted  *)
let report_untried ip =
  match Consolidation.get ip with
  | Consolidation.Never_tried -> Report_parameters.Untried.get ()
  | _ -> true

(* ip is not the specialization of a property, or those specializations should
   be shown.  *)
let report_specialized ip =
  (Report_parameters.Specialized.get ()) ||
    (match ip with Property.IPPropertyInstance _ -> false | _ -> true)

let report_proven ip =
  let open Consolidation in
  Report_parameters.Proven.get () ||
    match get ip with
    | Considered_valid | Valid _
    | Invalid_but_dead _ | Valid_but_dead _ | Unknown_but_dead _
    | Valid_under_hyp _ -> false

    | Never_tried | Unknown _ | Invalid _ | Invalid_under_hyp _ | Inconsistent _
      -> true


let report_ip ip =
  report_untried ip && report_specialized ip && report_proven ip


let rec add_property ips ip =
  if report_ip ip && not (Property.Set.mem ip !ips) then
    begin
      ips := Property.Set.add ip !ips ;
      add_consolidation ips (Consolidation.get ip)
    end
    
and add_consolidation ips = function
  | Consolidation.Never_tried 
  | Consolidation.Considered_valid 
  | Consolidation.Valid _ 
  | Consolidation.Invalid _ 
  | Consolidation.Inconsistent _ -> ()

  | Consolidation.Valid_under_hyp ps 
  | Consolidation.Unknown ps 
  | Consolidation.Invalid_under_hyp ps 
  | Consolidation.Valid_but_dead ps
  | Consolidation.Invalid_but_dead ps
  | Consolidation.Unknown_but_dead ps -> 
      add_pending ips ps

and add_pending ipref (ps:Consolidation.pending) =
  E.Map.iter
    (fun _ m ->
       E.Map.iter
	 (fun _ ips -> 
	    Property.Set.iter (add_property ipref) ips
	 ) m
    ) ps

let iter (inspector:inspector) =
  begin
    (* Collect noticeable properties (tried + their pending) *)
    let properties = ref Property.Set.empty in
    Property_status.iter (fun ip -> add_property properties ip) ;
    let globals = ref Property.Set.empty in
    let functions = ref Kernel_function.Map.empty in
    (* Dispatch properties into globals and per-function map *)
    Property.Set.iter
      (fun ip ->
	 match Property.get_kf ip with
	   | None -> globals := Property.Set.add ip !globals
	   | Some kf ->
	       if not (Ast_info.is_frama_c_builtin (Kernel_function.get_name kf))
		 then try
 		   let fips = Kernel_function.Map.find kf !functions in
		   fips := Property.Set.add ip !fips
		 with Not_found ->
		   let ips = Property.Set.singleton ip in
		   functions := Kernel_function.Map.add kf (ref ips) !functions)
      !properties ;
    (* Report a set of ip in a section *)
    let report s f ips = if not (Property.Set.is_empty ips) then
      ( s () ; Property.Set.iter (fun ip -> f ip (Consolidation.get ip)) ips ) 
    in
    if Property.Set.is_empty !globals && Kernel_function.Map.is_empty !functions then
      inspector#empty
    else
      begin
	inspector#started ;
	report (fun () -> inspector#global_section) inspector#property !globals ;
	Kernel_function.Map.iter
	  (fun kf ips ->
            let vi = Kernel_function.get_vi kf in
            if not (Cil.is_unused_builtin vi) then
	      report (fun () -> inspector#function_section kf) inspector#property !ips) 
	  !functions ;
	inspector#finished ;
      end
  end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
