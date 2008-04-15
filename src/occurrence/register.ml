(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat � l'�nergie Atomique)                             *)
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

(* $Id: register.ml,v 1.20 2008/11/06 13:03:28 uid568 Exp $ *)

open Cil_types
open Cilutil
open Cil
open Visitor

module Occurrences: sig
  val add: varinfo -> kinstr -> lval -> unit
  val get: varinfo -> (kinstr * lval) list
  val self: Project.Computation.t
  val iter: (varinfo -> (kinstr * lval) list -> unit) -> unit
end = struct

  module State =
    Cil_computation.VarinfoHashtbl
      (Datatype.Couple(Cil_datatype.Kinstr)(Cil_datatype.Lval))
      (struct
	 let size = 17
	 let name = "Occurrences.State"
	 let dependencies = [ Db.Value.self ]
       end)

  module LastResult =
    Computation.OptionRef
      (Cil_datatype.Varinfo)
      (struct
	 let name = "Occurrences.LastResult"
	 let dependencies = [ Cil_state.self; State.self ]
       end)

  let add vi ki lv = State.add vi (ki, lv)

  let unsafe_get vi = try State.find_all vi with Not_found -> []

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
    Db.register ~journalize:None Db.Occurrence.get_last_result get_last_result

  let iter f =
    let old, l =
      State.fold
	(fun v elt (old, l) -> match v, old with
	 | v, None ->
	     assert (l = []);
	     Some v, [ elt ]
	 | v, (Some old as some) when VarinfoComparable.equal v old ->
	     some, elt :: l
	 | v, Some old ->
	     f old l;
	     Some v, [ elt ])
	(None, [])
    in
    Extlib.may (fun v -> f v l) old

  let self = State.self

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
       let lv = !Db.Properties.Interp.term_lval_to_lval tlv in
       ignore (self#vlval lv)
     with Invalid_argument msg ->
       if Cmdline.Occurrence.Debug.get () > 0 then
	 Format.printf "[occurrence:] %s@." msg);
    DoChildren

  method vstmt_aux s =
    !Db.progress ();
    super#vstmt_aux s

  initializer !Db.Value.compute ()

end

let compute, _self =
  let run () =
    if Cmdline.Occurrence.Debug.get () > 0 then
      Format.printf "[occurrence] Beginning analysis...@.";
    ignore (visitFramacFile (new occurrence) (Cil_state.file ()));
    if Cmdline.Occurrence.Debug.get () > 0 then
      Format.printf "[occurrence] Done.@.";
  in
  Computation.apply_once "Occurrence.compute" [ Occurrences.self ] run

let get vi =
  compute (); 
  try Occurrences.get vi with Not_found -> assert false

let print_one v l =
  Format.printf "variable %s (%d):\n" v.vname v.vid;
  List.iter
    (fun (ki, lv) ->
       Format.printf "  sid %a: %a\n"
	 (fun fmt ki -> match ki with
	  | Kglobal -> Format.fprintf fmt "Global"
	  | Kstmt s -> Format.fprintf fmt "%d" s.sid)
	 ki
	 d_lval lv)
    l;
  Format.print_flush ()

let print_all () =
  compute ();
  Occurrences.iter print_one

let main _fmt =
  if Cmdline.Occurrence.Print.get () then 
    !Db.Occurrence.print_all ()

let () = Db.Main.extend main

let debug =
  [ "-debug",
    Arg.Int Cmdline.Occurrence.Debug.set,
    ": level of debug" ]

let options =
  [ "-occurrence",
    Arg.Unit Cmdline.Occurrence.Print.on,
    ": print results of occurrence analysis" ]

let () =
  Db.register 
    ~journalize:
    (Some ("Occurrence.get",
	   Type.func Kernel_type.varinfo 
	     (Type.list (Type.couple Kernel_type.kinstr Kernel_type.lval))))
    Db.Occurrence.get 
    get;
  Db.register
    ~journalize:
    (Some ("Occurrence.print_all", Type.func Type.unit Type.unit))
    (* pb: print_all prend maintenant un formatter *)
    Db.Occurrence.print_all 
    print_all;
  Db.Occurrence.self := Occurrences.self;
  Options.add_plugin
    ~name:"occurrence"
    ~descr:"Compute occurrences of variable declarations"
    ~debug
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
