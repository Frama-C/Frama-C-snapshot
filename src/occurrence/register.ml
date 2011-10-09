(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Cil_datatype
open Cil
open Visitor
open Options

module Occurrences: sig
  val add: varinfo -> kinstr -> lval -> unit
  val get: varinfo -> (kinstr * lval) list
  val self: State.t
  val iter: (varinfo -> (kinstr * lval) list -> unit) -> unit
end = struct

  module IState =
    Cil_state_builder.Varinfo_hashtbl
      (Datatype.Pair(Kinstr)(Lval))
      (struct
         let size = 17
         let name = "Occurrences.State"
         let dependencies = [ Db.Value.self ]
         let kind = `Internal
       end)

  module LastResult =
    State_builder.Option_ref
      (Varinfo)
      (struct
         let name = "Occurrences.LastResult"
         let dependencies = [ Ast.self; IState.self ]
         let kind = `Internal
       end)

  let add vi ki lv = IState.add vi (ki, lv)

  let unsafe_get vi = try IState.find_all vi with Not_found -> []

  let get vi =
    LastResult.set vi;
    unsafe_get vi

  let get_last_result () =
    try
      let vi = LastResult.get () in
      Some (unsafe_get vi, vi)
    with Not_found ->
      None

  let () =
    Db.register Db.Journalization_not_required
      Db.Occurrence.get_last_result get_last_result

  let iter f =
    let old, l =
      IState.fold
        (fun v elt (old, l) -> match v, old with
        | v, None ->
          assert (l = []);
          Some v, [ elt ]
        | v, (Some old as some) when Varinfo.equal v old ->
          some, elt :: l
        | v, Some old ->
          f old l;
          Some v, [ elt ])
        (None, [])
    in
    Extlib.may (fun v -> f v l) old

  let self = IState.self

end

class occurrence = object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (inplace_visit ()) as super

  val mutable decls = []

  method private current_ki =
    match self#current_stmt with None -> Kglobal | Some s -> Kstmt s

  method vvdec vi =
    let ki = self#current_ki in
    if Db.Value.is_accessible ki then begin
      let z =
        !Db.Value.lval_to_zone
          ki ~with_alarms:CilE.warn_none_mode (Var vi, NoOffset)
      in
      decls <-  (vi, z) :: decls
    end;
    DoChildren

  method vlval lv =
    let ki = self#current_ki in
    if Db.Value.is_accessible ki then begin
      let z = !Db.Value.lval_to_zone ki ~with_alarms:CilE.warn_none_mode lv in
      if not (Locations.Zone.equal Locations.Zone.bottom z) then
        List.iter
          (fun (vi, zvi) ->
             if Locations.Zone.intersects z zvi then Occurrences.add vi ki lv)
          decls
    end;
    DoChildren

  method vterm_lval tlv =
    (try
       let lv = !Db.Properties.Interp.term_lval_to_lval ~result:None tlv in
       ignore (self#vlval lv)
     with Invalid_argument msg ->
       error ~current:true "%s@." msg);
    DoChildren

  method vstmt_aux s =
    !Db.progress ();
    super#vstmt_aux s

  initializer !Db.Value.compute ()

end

let compute, _self =
  let run () =
    feedback "beginning analysis";
    ignore (visitFramacFile (new occurrence) (Ast.get ()));
    feedback "analysis done"
  in
  State_builder.apply_once "Occurrence.compute" [ Occurrences.self ] run

let get vi =
  compute ();
  try Occurrences.get vi with Not_found -> assert false

let d_ki fmt = function
  | Kglobal -> Format.fprintf fmt "Global"
  | Kstmt s -> Format.fprintf fmt "%d" s.sid

let print_one fmt v l =
  Format.fprintf fmt "variable %s (%s):@\n" 
    v.vname 
    (if v.vglob then "global"
     else 
	let kf_name = match l with
	  | [] | (Kglobal, _) :: _ -> assert false
	  | (Kstmt s, _) :: _ ->
	    Kernel_function.get_name (Kernel_function.find_englobing_kf s)
	in
	if v.vformal then "parameter of " ^ kf_name
	else "local of " ^ kf_name);
  List.iter
    (fun (ki, lv) ->
       Format.fprintf fmt "  sid %a: %a@\n" d_ki ki d_lval lv) l

let print_all () =
  compute ();
  result "%t" (fun fmt -> Occurrences.iter (print_one fmt))

let main _fmt = if Print.get () then !Db.Occurrence.print_all ()
let () = Db.Main.extend main

let () =
  Db.register
    (Db.Journalize
       ("Occurrence.get",
        Datatype.func
          Varinfo.ty
        (* [JS 2011/04/01] Datatype.list buggy in presence of journalisation.
           See comment in datatype.ml *)
        (*(Datatype.list (Datatype.pair Kinstr.ty Lval.ty))*)
          (let module L = Datatype.List(Datatype.Pair(Kinstr)(Lval)) in
           L.ty)))
    Db.Occurrence.get
    get;
  Db.register
    (Db.Journalize
       ("Occurrence.print_all", Datatype.func Datatype.unit Datatype.unit))
    (* pb: print_all should take a formatter as argument *)
    Db.Occurrence.print_all
    print_all;
  Db.Occurrence.self := Occurrences.self

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
