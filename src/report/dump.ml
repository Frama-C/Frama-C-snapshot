(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Dump Report on Output                                              --- *)
(* -------------------------------------------------------------------------- *)

open Property_status

let bar = String.make 80 '-'
let dim = 9 (* Size for status [----] *)
let tab = String.make (dim+3) ' '

let pp_status fmt s =
  let n = String.length s in
  if n < dim then
    let m = String.make dim ' ' in
    let p = (dim - n) / 2 in
    String.blit s 0 m p n ;
    Format.fprintf fmt "[%s]" m
  else Format.fprintf fmt "[%s]" s

open Consolidation
module E = Emitter.Usable_emitter

class dumper out = 
object(self)

  val mutable st_unknown  = 0 ; (* no status *)
  val mutable st_partial  = 0 ; (* locally valid but missing hyp *)
  val mutable st_extern   = 0 ; (* considered valid *)
  val mutable st_complete = 0 ; (* valid and complete *)
  val mutable st_bug      = 0 ; (* invalid and complete *)
  val mutable st_alarm    = 0 ; (* invalid but missing hyp *)
  val mutable st_dead     = 0 ; (* under invalid hyp *)
  val mutable st_maybe_unreachable  = 0 ; (* possible unreachable *)
  val mutable st_unreachable  = 0 ; (* confirmed unreachable *)
  val mutable st_inconsistent = 0 ; (* unsound *)
  val mutable kf : Description.kf = `Always

  method started = ()

  method global_section =
    Format.fprintf out "%s@\n--- Global Properties@\n%s@\n@." bar bar

  method function_section thekf =
    Format.fprintf out "@\n%s@\n--- Properties of Function '%s'@\n%s@\n@." 
      bar (Kernel_function.get_name thekf) bar ;
    kf <- `Context thekf

  method category ip st =
    match ip, st with
      (* Special display for unreachable *)
      | Property.IPReachable _, Invalid_under_hyp _ ->
          st_maybe_unreachable <- succ st_maybe_unreachable;
          "Possibly unreachable"
      | Property.IPReachable _, Invalid _ ->
          st_unreachable <- succ st_unreachable; "Unreachable"

      (* All other cases, including some unreachable *)
      | _, (Never_tried | Unknown _) -> st_unknown <- succ st_unknown ; "-"
      | _, Considered_valid -> st_extern <- succ st_extern ; "Extern"
      | _, Valid _ -> st_complete <- succ st_complete ; "Valid"
      | _, Invalid _ -> st_bug <- succ st_bug ; "Bug"
      | _, Valid_under_hyp _ -> st_partial <- succ st_partial ; "Partial"
      | _, Invalid_under_hyp _ -> st_alarm <- succ st_alarm ; "Alarm"
      | _, (Valid_but_dead _ | Invalid_but_dead _ | Unknown_but_dead _) -> 
          st_dead <- succ st_dead ; "Dead"
      | _, Inconsistent _ -> st_inconsistent <- succ st_inconsistent ; "Unsound"

  method emitter e = Format.fprintf out "%s@[<hov 2>by %a.@]@\n" tab E.pretty e

  method emitters es = E.Set.iter self#emitter es

  method tried_emitters ps = 
    let es = E.Map.fold (fun e _ es -> e::es) ps [] in
    match es with
      | [] -> ()
      | e::es -> 
	  Format.fprintf out "%s@[<hov 2>tried with %a" tab E.pretty e ;
	  List.iter (fun e -> Format.fprintf out ",@ %a" E.pretty e) es ;
	  Format.fprintf out ".@]@\n" 

  method dead_reasons ps =
    E.Map.iter
      (fun e ps -> 
	 Format.fprintf out "%s@[<hov 2>By %a because:@]@\n" tab E.pretty e ;
	 Property.Set.iter
	   (fun p -> Format.fprintf out "%s@[<hov 3> - %a@]@\n" tab 
	      (Description.pp_localized ~kf ~ki:true ~kloc:true) p) ps
      ) (Scan.partial_pending ps)
      
  method partial_pending ps =
    E.Map.iter
      (fun e ps -> 
	 Format.fprintf out "%s@[<hov 2>By %a, with pending:@]@\n" tab E.pretty e ;
	 Property.Set.iter
	   (fun p -> Format.fprintf out "%s@[<hov 3> - %a@]@\n" tab 
	      (Description.pp_localized ~kf ~ki:true ~kloc:true) p) ps
      ) (Scan.partial_pending ps)

  method property ip st =
    begin
      Format.fprintf out "%a @[%a@]@\n" pp_status (self#category ip st) 
	(Description.pp_localized ~kf:`Never ~ki:true ~kloc:true) ip ;
      if Report_parameters.PrintProperties.get () then
        Format.fprintf out "%s@[%a@]@\n" tab Property.pretty ip;
      match st with
	| Never_tried -> ()
	| Unknown emitters -> self#tried_emitters emitters
	| Valid emitters -> self#emitters emitters
	| Invalid emitters -> self#emitters emitters
	| Invalid_but_dead pending ->
	    Format.fprintf out "%sLocally invalid, but unreachable.@\n" tab ;
	    self#dead_reasons pending
	| Valid_but_dead pending ->
	    Format.fprintf out "%sLocally valid, but unreachable.@\n" tab ;
	    self#dead_reasons pending
	| Unknown_but_dead pending ->
	    Format.fprintf out "%sLocally unknown, but unreachable.@\n"tab ;
	    self#dead_reasons pending
	| Invalid_under_hyp pending | Valid_under_hyp pending -> 
	    self#partial_pending pending
	| Considered_valid -> 
	    Format.fprintf out "%sUnverifiable but considered Valid.@\n" tab
	| Inconsistent s -> 
	    let p = ref 0 in
	    let n = String.length s in
	    while !p < n do
	      try
		let k = String.index_from s !p '\n' in
		Format.fprintf out "%s%s@\n" tab (String.sub s !p (k - !p)) ;
		p := succ k ;
	      with Not_found ->
		Format.fprintf out "%s%s@\n" tab (String.sub s !p (n - !p)) ;
		p := n ;
	    done
	    
    end

  method finished =
    Format.fprintf out "@\n%s@\n--- Status Report Summary@\n%s@\n" bar bar ;
    if st_complete > 0 then 
      Format.fprintf out "  %4d Completely validated@\n" st_complete ;
    if st_partial > 0  then 
      Format.fprintf out "  %4d Locally validated@\n" st_partial ;
    if st_extern > 0   then
      Format.fprintf out "  %4d Considered valid@\n" st_extern ;
    if st_unknown > 0  then
      Format.fprintf out "  %4d To be validated@\n" st_unknown ;
    if st_alarm = 1    then
      Format.fprintf out "  %4d Alarm emitted@\n" st_alarm ;
    if st_alarm > 1    then
      Format.fprintf out "  %4d Alarms emitted@\n" st_alarm ;
    if st_bug > 0      then
      Format.fprintf out "  %4d Bugs found@\n" st_bug ;
    if st_dead > 1     then
      Format.fprintf out "  %4d Dead properties@\n" st_dead ;
    if st_dead = 1     then
      Format.fprintf out "     1 Dead property@\n" ;
    if st_maybe_unreachable > 0     then
      Format.fprintf out "  %4d Unconfirmed unreachable@\n"
        st_maybe_unreachable ;
    if st_unreachable > 0     then
      Format.fprintf out "  %4d Unreachable@\n" st_unreachable ;
    if st_inconsistent > 1
    then Format.fprintf out "  %4d Inconsistencies@\n" st_inconsistent ;
    if st_inconsistent = 1
    then Format.fprintf out "     1 Inconsistency@\n" ;
    let total =
      st_complete + st_partial + st_extern + st_unknown + st_alarm + st_bug 
      + st_dead + st_inconsistent
    in
    Format.fprintf out " %5d Total@\n%s@." total bar ;

  method empty = 
    Format.fprintf out "%s@\n--- No status to report@\n%s@." bar bar ;
    
end

let create out = (new dumper out :> Scan.inspector)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
