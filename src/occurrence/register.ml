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

open Cil_types
open Cil_datatype
open Cil
open Visitor
open Options

module Occurrence_datatype =
  Datatype.Triple(Datatype.Option(Kernel_function))(Kinstr)(Lval)

module Occurrences: sig
  val add: varinfo -> kernel_function option -> kinstr -> lval -> unit
  val get: varinfo -> (kernel_function option * kinstr * lval) list
  val self: State.t
  val iter: (varinfo ->
             (kernel_function option * kinstr * lval) list -> unit) -> unit
  val iter_sorted: (varinfo ->
             (kernel_function option * kinstr * lval) list -> unit) -> unit
end = struct

  module IState =
    Cil_state_builder.Varinfo_hashtbl
      (Occurrence_datatype)
      (struct
         let size = 17
         let name = "Occurrences.State"
         let dependencies = [ Db.Value.self ]
       end)

  module LastResult =
    State_builder.Option_ref
      (Varinfo)
      (struct
         let name = "Occurrences.LastResult"
         let dependencies = [ Ast.self; IState.self ]
       end)

  let add vi kf ki lv = IState.add vi (kf, ki, lv)

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

  let iter_aux fold f =
    let old, l =
      fold
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

  let fold_sorted f init =
    let map = IState.fold Varinfo.Map.add Varinfo.Map.empty in
    Varinfo.Map.fold f map init       

  let iter = iter_aux IState.fold
  let iter_sorted = iter_aux fold_sorted

  let self = IState.self

end

class occurrence = object (self)

  inherit Visitor.frama_c_inplace as super

  method! vlval lv =
    let ki = self#current_kinstr in
    if Db.Value.is_accessible ki then begin
      let z = !Db.Value.lval_to_zone ki ~with_alarms:CilE.warn_none_mode lv in
      try
        Locations.Zone.fold_topset_ok
          (fun b _ () ->
            match b with
              | Base.Var (vi, _) | Base.Initialized_Var (vi, _) ->
                  Occurrences.add vi self#current_kf ki lv
              | _ -> ()
          ) z ()
      with Locations.Zone.Error_Top ->
        error ~current:true "Found completely imprecise value (%a). Ignoring@."
          Printer.pp_lval lv
    end;
    DoChildren

  method! vterm_lval tlv =
    (try
       let lv = !Db.Properties.Interp.term_lval_to_lval ~result:None tlv in
       ignore (self#vlval lv)
     with
       | Invalid_argument "not an lvalue" -> () (* Translation to lval failed.*)
       | Invalid_argument msg -> error ~current:true "%s@." msg);
    DoChildren

  method! vstmt_aux s =
    !Db.progress ();
    super#vstmt_aux s

  initializer !Db.Value.compute ()

end

type access_type = Read | Write | Both

(** Try to find [lv] somewhere within a Cil value *)
class is_sub_lval lv = object
  inherit Cil.nopCilVisitor

  method! vlval lv' =
    if Cil_datatype.Lval.equal lv lv' then raise Exit;
    DoChildren
end

(** Occurrence has found the given [lv] somewhere inside [ki]. We try to find
    whether this was inside a read or a write operation. This is difficult to
    do directly inside the {!occurrence} class, as the [vlval] method
    has no information about the origin of the lval it was called on *)
let classify_accesses (_kf, ki, lv) =
  let vis = new is_sub_lval lv in
  let aux f v = try ignore (f vis v); false with Exit -> true in
  let is_lv = Cil_datatype.Lval.equal lv in
  let contained_exp = aux Cil.visitCilExpr in
  match ki with
    | Kglobal -> (* Probably initializers *) Read

    | Kstmt { skind = Instr i } ->
      (match i with
        | Set (lv', e, _) ->
            if is_lv lv' then
              if contained_exp e then Both
              else Write
            else Read

        | Call (Some lv', f, args, _) ->
            if is_lv lv' then
              if contained_exp f || List.exists contained_exp args then Both
              else Write
            else Read

        | Asm (_, _, out, inp, _, _,_) ->
            if List.exists (fun (_, _, out) -> is_lv out) out then
              if List.exists (fun (_, _, inp) -> contained_exp inp) inp
              then Both
              else Write
            else Read

        | _ -> Read)
    | _ -> Read

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
  | None, Kglobal -> Format.fprintf fmt "global"
  | Some kf, Kglobal ->
      Format.fprintf fmt "specification of %a" Kernel_function.pretty kf
  | _, Kstmt s -> Format.fprintf fmt "sid %d" s.sid

let print_one fmt v l =
  Format.fprintf fmt "variable %s (%s):@\n" 
    v.vname 
    (if v.vglob then "global"
     else 
	let kf_name = match l with
	  | [] -> assert false
          | (Some kf, _, _) :: _ -> Kernel_function.get_name kf
          | (None,Kstmt _,_)::_ -> assert false
          | (None,Kglobal,_)::_ ->
              fatal "inconsistent context for occurence of variable %s" v.vname
	in
	if v.vformal then "parameter of " ^ kf_name
	else "local of " ^ kf_name);
  List.iter
    (fun (kf, ki, lv) ->
      Format.fprintf fmt "  %a: %a@\n" d_ki (kf,ki) Printer.pp_lval lv) l

let print_all () =
  compute ();
  result "%t" (fun fmt -> Occurrences.iter_sorted (print_one fmt))

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
          (let module L = Datatype.List(Occurrence_datatype) in L.ty)))
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
