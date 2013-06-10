(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Cil_types

let display_aux pp =
  !Db.Semantic_Callgraph.topologically_iter_on_functions
    (fun k ->
      if !Db.Value.is_called k then
        pp ("Function %a:@\n%a@." : (_, _, _, _, _, _) format6)
          Kernel_function.pretty k !Db.From.pretty k)

let display fmt =
  Format.fprintf fmt "@[<v>";
  display_aux (Format.fprintf fmt);
  Format.fprintf fmt "@]"

module SortCalls = struct
  type t = stmt
  (* Sort first by original source code location, then by sid *)
  let compare s1 s2 =
    let r = Cil_datatype.Location.compare
      (Cil_datatype.Stmt.loc s1) (Cil_datatype.Stmt.loc s2) in
    if r = 0
    then Cil_datatype.Stmt.compare s1 s2 (* This is not really stable, but no
                                            good criterion is left *)
    else r
end
module MapStmtCalls = Map.Make(SortCalls)

let iter_callwise_calls_sorted f =
  let hkf = Kernel_function.Hashtbl.create 17 in  
  let kglobal = ref None in
  !Db.From.Callwise.iter
    (fun ki d ->
       match ki with
         | Kglobal -> kglobal := Some d
         | Kstmt s ->
             let kf = Kernel_function.find_englobing_kf s in
             let m =
               try Kernel_function.Hashtbl.find hkf kf
               with Not_found ->  MapStmtCalls.empty
             in
             let m = MapStmtCalls.add s d m in
             Kernel_function.Hashtbl.replace hkf kf m
    );
  !Db.Semantic_Callgraph.topologically_iter_on_functions
    (fun kf ->
       try
         let m = Kernel_function.Hashtbl.find hkf kf in
         MapStmtCalls.iter (fun s d -> f (Kstmt s) d) m
       with Not_found -> ()
    );
  match !kglobal with
    | None -> ()
    | Some d -> f Kglobal d


let main () =
  let not_quiet = From_parameters.verbose_atleast 1 in
  let forcedeps = From_parameters.ForceDeps.get () in
  let forcecalldeps = From_parameters.ForceCallDeps.get () in
  if forcedeps then begin
    !Db.From.compute_all ();
    From_parameters.ForceDeps.output
      (fun () ->
        From_parameters.feedback "====== DEPENDENCIES COMPUTED ======@\n\
These dependencies hold at termination for the executions that terminate:";
        display_aux (fun fm -> From_parameters.result fm);
        From_parameters.feedback "====== END OF DEPENDENCIES ======"
      )
  end;
  if forcecalldeps then !Db.From.compute_all_calldeps ();
  if not_quiet && forcecalldeps then begin
    From_parameters.ForceCallDeps.output
      (fun () ->
        From_parameters.feedback "====== DISPLAYING CALLWISE DEPENDENCIES ======";
        iter_callwise_calls_sorted
         (fun ki d ->
         let id,typ =
           match ki with
             | Cil_types.Kglobal ->
                 "entry point",
                 Kernel_function.get_type (fst (Globals.entry_point ()))
             | Cil_types.Kstmt s ->
                 let set = Db.Value.call_to_kernel_function s in
                 let f =
                   try Kernel_function.Hptset.min_elt set
                   with Not_found ->
                     From_parameters.fatal
                       ~source:(fst (Cil_datatype.Stmt.loc s))
                       "Invalid call %a@." Printer.pp_stmt s
                 in
                 let id =
                   Pretty_utils.sfprintf "%a at %a (by %a)%t"
                     Kernel_function.pretty f
                     Cil_datatype.Location.pretty (Cil_datatype.Stmt.loc s)
                     Kernel_function.pretty
		     (Kernel_function.find_englobing_kf s)
                     (fun fmt ->
                        if From_parameters.debug_atleast 1 then
                          Format.fprintf fmt " <sid %d>" s.Cil_types.sid)
                 in
                 id,
                 Kernel_function.get_type f
         in
         From_parameters.result
           "@[call %s:@\n%a@\n@]@ "
           id (Function_Froms.pretty_with_type typ) d);
    From_parameters.feedback "====== END OF CALLWISE DEPENDENCIES ======";
      )
  end

let () = Db.Main.extend main


let update_from loc new_v mem =
  let exact =
    Locations.valid_cardinal_zero_or_one ~for_writing:true loc
  in
  let z = Locations.enumerate_valid_bits ~for_writing:true loc in
  Lmap_bitwise.From_Model.add_binding exact mem z new_v

let access_from looking_for mem =
  Lmap_bitwise.From_Model.find mem looking_for


(* Registration for most Db.From functions is done at the end of the
   Functionwise and Callwise modules *)
let () =
  Db.From.display := display;
  Db.From.update := update_from;
  Db.From.access := access_from;



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
