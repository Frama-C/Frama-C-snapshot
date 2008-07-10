(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: kernel_function.ml,v 1.15 2008/05/27 21:55:35 uid528 Exp $ *)

open Cil_types
open Db_types

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

let dummy = 
  { fundec = Definition (Cil.dummyFunDec, Cil.locUnknown); 
    return_stmt = None; 
    spec = Cil.empty_funspec ();
    stmts_graph = None }

let get_vi kf = match kf.fundec with
  | Definition (d, _) -> d.svar
  | Declaration (_,vi,_, _) -> vi

let get_name kf = (get_vi kf).vname

let get_location kf = 
 match kf.fundec with
  | Definition (_, loc) -> loc
  | Declaration (_,vi,_, _) -> vi.vdecl

let get_type kf = (get_vi kf).vtype

let get_global f = match f.fundec with
  | Definition (d, loc) -> GFun(d,loc)
  | Declaration (spec, vi, _, loc) -> GVarDecl(spec, vi,loc)

let get_formals f = match f.fundec with
  | Definition(d, _) -> d.sformals
  | Declaration(_, _, None, _) -> []
  | Declaration(_,_,Some args,_) -> args

exception No_Definition
let get_definition kf = match kf.fundec with
  | Definition (f,_) -> f
  | Declaration _ -> raise No_Definition

(* ************************************************************************* *)
(** {2 Kernel functions are comparable} *)
(* ************************************************************************* *)

module Comparable = struct
  type t = kernel_function
  let equal = (==)
  let hash kf = Hashtbl.hash (get_vi kf).vid
  let compare t1 t2 = Pervasives.compare (get_vi t1).vid (get_vi t2).vid
end
include Comparable

(* ************************************************************************* *)
(** {2 Searching} *)
(* ************************************************************************* *)

module Kf =
  Computation.OptionRef
    (Kernel_datatype.IntHashtbl
       (Datatype.Couple(Kernel_datatype.KernelFunction)(Kernel_datatype.Stmt)))
    (struct
       let name = Project.Computation.Name.make "KF"
       let dependencies = [ Cil_state.self ]
     end)

let compute () =
  Kf.memo
    (fun () ->
       let p = Cil_state.file () in
       let h = Inthash.create 97 in
       let visitor = object(self)
	 inherit Cil.nopCilVisitor
	 val mutable current_kf = None
	 method kf =
	   match current_kf with None -> assert false | Some kf -> kf
	 method vstmt s =
	   Inthash.add h s.sid (self#kf, s);
	   Cil.DoChildren
	 method vglob g =
	   begin match g with
	   | GFun (fd, _) ->
	       let kf = Globals.Functions.get fd.svar in
	       current_kf <- Some kf;
	   | _ ->
	       ()
	   end;
	   Cil.DoChildren
       end
       in
       Cil.visitCilFile (visitor :> Cil.cilVisitor) p;
       h)

let find_from_sid sid =
  let table = compute () in
  let kf,s = Inthash.find table sid in
  s, kf

exception Got_return of stmt
let find_return kf =
  match kf.return_stmt with
  | None ->
      let find_return fd =
        let visitor = object
          inherit Cil.nopCilVisitor as super
          method vstmt s =
            match s.skind with
              | Return _ -> raise (Got_return s)
              | _ -> Cil.DoChildren
        end
        in
        try
          ignore (Cil.visitCilFunction (visitor :> Cil.cilVisitor) fd);
          assert false
        with Got_return s -> s
      in
      let ki = find_return (get_definition kf) in
      kf.return_stmt <- Some ki;
      ki
  | Some ki ->
      ki

exception No_Statement
let find_first_stmt kf =
  try
    List.hd ((get_definition kf).sbody.bstmts)
  with No_Definition | Not_found ->
    raise No_Statement

exception Found_label of stmt ref
let find_label kf label =
  match kf.fundec with
  | Declaration _ -> raise Not_found
  | Definition (fundec,_) ->
      let label_finder = object
        inherit Cil.nopCilVisitor
        method vstmt s = begin
          if List.exists
            (fun lbl -> match lbl with
             | Label (s,_,_) -> s = label
             | Case _ -> false
             | Default _ -> label="default")
            s.labels then raise (Found_label (ref s));
	  Cil.DoChildren
        end
        method vexpr _ = Cil.SkipChildren
        method vtype _ = Cil.SkipChildren
        method vinst _ = Cil.SkipChildren
      end
      in
      try
        ignore (Cil.visitCilFunction label_finder fundec);
        (* Ok: this is not a code label *)
        raise Not_found
      with Found_label s -> s

(* ************************************************************************* *)
(** {2 Checkers} *)
(* ************************************************************************* *)

let is_definition kf =
  match kf.fundec with
  | Definition _ -> true
  | Declaration _ -> false

let returns_void kf =
  let result_type,_,_,_ = Cil.splitFunctionType (get_type kf) in
  match Cil.unrollType result_type with
  | TVoid _ -> true
  | _ -> false

(* ************************************************************************* *)
(** {2 Membership of variables} *)
(* ************************************************************************* *)

let is_formal v kf =  List.exists (fun vv -> v.vid = vv.vid) (get_formals kf)

let is_local v kf = match kf.fundec with
    | Definition(fd, _) -> Ast_info.Function.is_local v fd
    | Declaration _ -> false

let is_formal_or_local v kf = (not v.vglob) && is_formal v kf || is_local v kf

(* ************************************************************************* *)
(** {2 Specifications} *)
(* ************************************************************************* *)

let populate_spec = Extlib.mk_fun "Kernel_function.populate_spec"

let get_spec f =
  if is_definition f then
    f.spec
  else
    ((* Do not overwrite an existing assign clause*)
      !populate_spec f;
      f.spec)

let postcondition kf =
  Logic_const.pands
    (List.map Ast_info.behavior_postcondition (get_spec kf).spec_behavior)

let precondition kf =
  Logic_const.pands
    (List.map Logic_const.pred_of_id_pred (get_spec kf).spec_requires)

(* ************************************************************************* *)
(** {2 Pretty printer} *)
(* ************************************************************************* *)

let pretty_name fmt kf = Ast_info.pretty_vname fmt (get_vi kf)

(* ************************************************************************* *)
(** {2 Collections} *)
(* ************************************************************************* *)

module Make_Table = Computation.Hashtbl(Comparable)

module Set = struct
  module S = Set.Make(Comparable)
  include S
  module Datatype = Datatype.Make_Set(S)(Kernel_datatype.KernelFunction)
  let pretty fmt = iter (fun kf -> pretty_name fmt kf; Format.printf "@ ")
end

(* ************************************************************************* *)
(** {2 Setters} *)
(* ************************************************************************* *)

let register_stmt kf s =
  let tbl = try Kf.get () with Not_found -> assert false in
  Inthash.add tbl s.sid (kf,s)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
