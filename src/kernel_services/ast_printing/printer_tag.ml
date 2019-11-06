(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* --- Localizable API                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

type localizable =
  | PStmt of (kernel_function * stmt)
  | PStmtStart of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PExp of (kernel_function option * kinstr * exp)
  | PTermLval of (kernel_function option * kinstr * Property.t * term_lval)
  | PVDecl of (kernel_function option * kinstr * varinfo)
  | PGlobal of global
  | PIP of Property.t

module Localizable =
  Datatype.Make
    (struct
      include Datatype.Undefined
      type t = localizable

      let name = "Printer_tag.Localizable"
      let reprs = List.map (fun g -> PGlobal g) Global.reprs
      let mem_project = Datatype.never_any_project

      let hash = function
        | PStmtStart (_,s) ->
          Hashtbl.hash( 0, Stmt.hash s )
        | PStmt (_,s) ->
          Hashtbl.hash( 1, Stmt.hash s )
        | PLval (_,ki,lv) ->
          Hashtbl.hash( 2, Kinstr.hash ki, Lval.hash lv )
        | PTermLval(_,ki,pi,lv) ->
          Hashtbl.hash( 3, Kinstr.hash ki, Property.hash pi, Term_lval.hash lv)
        | PVDecl(_,_,v) ->
          Hashtbl.hash( 4, Varinfo.hash v )
        | PExp(_,_,e) ->
          Hashtbl.hash( 5, Exp.hash e )
        | PIP ip ->
          Hashtbl.hash( 6, Property.hash ip )
        | PGlobal g ->
          Hashtbl.hash( 7, Global.hash g )

      let equal l1 l2 = match l1,l2 with
        | PStmt (_,ki1), PStmt (_,ki2) -> ki1.sid = ki2.sid
        | PStmtStart (_,ki1), PStmtStart (_,ki2) -> ki1.sid = ki2.sid
        | PLval (_,ki1,lv1), PLval (_,ki2,lv2) ->
          Kinstr.equal ki1 ki2 && lv1 == lv2
        | PTermLval (_,ki1,pi1,lv1), PTermLval (_,ki2,pi2,lv2) ->
          Kinstr.equal ki1 ki2 && Property.equal pi1 pi2 &&
          Logic_utils.is_same_tlval lv1 lv2
        (* [JS 2008/01/21] term_lval are not shared: cannot use == *)
        | PVDecl (_,_,v1), PVDecl (_,_,v2) -> Varinfo.equal v1 v2
        | PExp (_,_,e1), PExp(_,_,e2) -> Exp.equal e1 e2
        | PIP ip1, PIP ip2 -> Property.equal ip1 ip2
        | PGlobal g1, PGlobal g2 -> Global.equal g1 g2
        | (PStmt _ | PStmtStart _ | PLval _ | PExp _ | PTermLval _ | PVDecl _
          | PIP _ | PGlobal _), _
          ->  false

      let pp_ki_loc fmt ki =
        match ki with
        | Kglobal -> (* no location, print 'global' *)
          Format.fprintf fmt "global"
        | Kstmt st ->
          Cil_datatype.Location.pretty fmt (Stmt.loc st)

      let pretty fmt = function
        | PStmtStart (_, s) ->
          Format.fprintf fmt "LocalizableStart %d (%a)"
            s.sid Printer.pp_location (Cil_datatype.Stmt.loc s)
        | PStmt (_, s) ->
          Format.fprintf fmt "LocalizableStmt %d (%a)"
            s.sid Printer.pp_location (Cil_datatype.Stmt.loc s)
        | PLval (_, ki, lv) ->
          Format.fprintf fmt "LocalizableLval %a (%a)"
            Printer.pp_lval lv pp_ki_loc ki
        | PExp (_, ki, lv) ->
          Format.fprintf fmt "LocalizableExp %a (%a)"
            Printer.pp_exp lv pp_ki_loc ki
        | PTermLval (_, ki, _pi, tlv) ->
          Format.fprintf fmt "LocalizableTermLval %a (%a)"
            Printer.pp_term_lval tlv pp_ki_loc ki
        | PVDecl (_, _, vi) ->
          Format.fprintf fmt "LocalizableVDecl %a" Printer.pp_varinfo vi
        | PGlobal g ->
          Format.fprintf fmt "LocalizableGlobal %a" Printer.pp_global g
        | PIP ip ->
          Format.fprintf fmt "LocalizableIP %a" Description.pp_property ip
    end)

(* -------------------------------------------------------------------------- *)
(* --- Utility Accessors                                                  --- *)
(* -------------------------------------------------------------------------- *)

let kf_of_localizable loc =
  match loc with
  | PLval (kf_opt, _, _)
  | PExp (kf_opt,_,_)
  | PTermLval(kf_opt, _,_,_)
  | PVDecl (kf_opt, _, _) -> kf_opt
  | PStmt (kf, _) | PStmtStart(kf,_) -> Some kf
  | PIP ip -> Property.get_kf ip
  | PGlobal (GFun ({svar = vi}, _)) -> Some (Globals.Functions.get vi)
  | PGlobal _ -> None

let ki_of_localizable loc = match loc with
  | PLval (_, ki, _)
  | PExp (_, ki, _)
  | PTermLval(_, ki,_,_)
  | PVDecl (_, ki, _) -> ki
  | PStmt (_, st) | PStmtStart(_, st) -> Kstmt st
  | PIP ip -> Property.get_kinstr ip
  | PGlobal _ -> Kglobal

let varinfo_of_localizable loc =
  match kf_of_localizable loc with
  | Some kf -> Some (Kernel_function.get_vi kf)
  | None ->
    match loc with
    | PGlobal (GVar (vi, _, _) | GVarDecl (vi, _)
              | GFunDecl (_, vi, _) | GFun ({svar = vi }, _)) -> Some vi
    | _ -> None

let loc_of_localizable = function
  | PStmt (_,st) | PStmtStart(_,st)
  | PLval (_,Kstmt st,_) | PExp(_,Kstmt st, _)
  | PTermLval(_,Kstmt st,_,_) ->
    Stmt.loc st
  | PIP ip ->
    (match Property.get_kinstr ip with
     | Kglobal ->
       (match Property.get_kf ip with
          None -> Location.unknown
        | Some kf -> Kernel_function.get_location kf)
     | Kstmt st -> Stmt.loc st)
  | PVDecl (_,_,vi) -> vi.vdecl
  | PGlobal g -> Global.loc g
  | (PLval _ | PTermLval _ | PExp _) as localize ->
    (match kf_of_localizable localize with
     | None -> Location.unknown
     | Some kf -> Kernel_function.get_location kf)

(* -------------------------------------------------------------------------- *)
(* --- Printer API                                                        --- *)
(* -------------------------------------------------------------------------- *)

module type TAG =
sig
  val create : localizable -> string
  val unfold : stmt -> bool
end

(* We delay the creation of the class to execution time, so that all
   pretty-printer extensions get properly registered (as we want to inherit
   from them). The only known solution is to use a functor *)
module BUILD(Tag : TAG)(X: Printer.PrinterClass) : Printer.PrinterClass =
struct

  class printer : Printer.extensible_printer = object(self)

    inherit X.printer as super

    val mutable current_property = None

    method private current_kinstr =
      match self#current_stmt with
      | None -> Kglobal
      | Some st -> Kstmt st

    method private current_sid =
      match super#current_stmt with
      | None -> assert false
      | Some st -> st.sid

    method private current_kf =
      match super#current_function with
      | None -> None
      | Some fd -> Some (Globals.Functions.get fd)

    val mutable current_ca = None

    val mutable active_behaviors = []

    method private current_behavior_or_loop =
      match current_ca with
        None ->
        let active = Datatype.String.Set.of_list active_behaviors in
        Property.Id_contract (active ,Extlib.the self#current_behavior)
      | Some ca -> Property.Id_loop ca

    (* When [stmt] is a call, this method "inlines" the preconditions of the
       functions that may be called here, with some context. This way,
       bullets are more precise, etc. *)
    method private preconditions_at_call fmt stmt =
      match stmt.skind with
      | Instr (Call _)
      | Instr (Local_init (_, ConsInit _, _)) ->
        let extract_instance_predicate = function
          | Property.IPPropertyInstance {Property.ii_pred} -> ii_pred
          (* Other cases should not happen, unless a plugin has replaced call
             preconditions. In this case, print nothing but do not crash. *)
          | _ -> raise Not_found
        in
        let extract_predicate = function
          | Property.IPPredicate {Property.ip_pred} -> ip_pred
          | _ -> assert false
        in
        (* Functons called at this point *)
        let called = Statuses_by_call.all_functions_with_preconditions stmt in
        let warn_missing = false in
        let add_by_kf kf acc =
          let ips =
            Statuses_by_call.all_call_preconditions_at ~warn_missing kf stmt
          in
          if ips = [] then acc else (kf, ips) :: acc
        in
        let ips_all_kfs = Kernel_function.Hptset.fold add_by_kf called [] in
        let pp_one fmt (original_p, p) =
          match extract_instance_predicate p with
          | Some pred -> Format.fprintf fmt "@[%a@]" self#requires_aux (p, pred)
          | None ->
            let pred = extract_predicate original_p in
            (* Makes the original predicate non clickable, as it may involve
               the formal parameters which are not in scope at the call site. *)
            Format.fprintf fmt "@[Non transposable: %s@]"
              (Format.asprintf "@[%a@]" self#requires_aux (original_p, pred))
          | exception Not_found -> ()
        in
        let pp_by_kf fmt (kf, ips) =
          Format.fprintf fmt "@[preconditions of %a:@]@ %a"
            Kernel_function.pretty kf
            (Pretty_utils.pp_list ~pre:"" ~sep:"@ " ~suf:"" pp_one) ips
        in
        if ips_all_kfs <> [] then
          Pretty_utils.pp_list ~pre:"@[<v 3>/* " ~sep:"@ " ~suf:" */@]@ "
            pp_by_kf fmt ips_all_kfs
      | _ -> ()


    method! next_stmt next fmt current =
      if Tag.unfold current
      then self#preconditions_at_call fmt current;
      Format.fprintf fmt "@{<%s>%a@}"
        (Tag.create (PStmt (Extlib.the self#current_kf,current)))
        (super#next_stmt next) current

    method! lval fmt lv =
      match self#current_kinstr with
      | Kglobal -> super#lval fmt lv
      (* Do not highlight the lvals in initializers. *)
      | Kstmt _ as ki ->
        Format.fprintf fmt "@{<%s>"
          (Tag.create (PLval (self#current_kf,ki,lv)));
        (match lv with
         | Var vi, (Field _| Index _ as o) ->
           (* Small hack to be able to click on the arrays themselves
              in the easy cases *)
           self#lval fmt (Var vi, NoOffset);
           self#offset fmt o
         | _ -> super#lval fmt lv
        );
        Format.fprintf fmt "@}"

    method! exp fmt e =
      match e.enode with
      | Lval lv ->
        (* Do not mark immediate l-values as they would not be
               selectable anyway because of the embedded tags of self#lval.
               This is only an optimization. *)
        self#lval fmt lv
      | _ ->
        Format.fprintf fmt "@{<%s>"
          (Tag.create (PExp (self#current_kf,self#current_kinstr,e)));
        super#exp fmt e;
        Format.fprintf fmt "@}"

    method! term_lval fmt lv =
      (* similar to pLval, except that term_lval can appear in specifications
         of functions (ki = None, kf <> None). Initializers are ignored. *)
      if self#current_kinstr = Kglobal && self#current_kf = None then begin
        super#term_lval fmt lv (* Do not highlight the lvals in initializers. *)
      end else begin
        match current_property with
        | None -> (* Also use default printer for this case (possible inside
                     pragmas, for example). *)
          super#term_lval fmt lv
        | Some ip ->
          Format.fprintf fmt "@{<%s>"
            (Tag.create
               (PTermLval (self#current_kf, self#current_kinstr, ip, lv)));
          (match lv with
           | TVar vi, (TField _| TIndex _ as o) ->
             self#term_lval fmt (TVar vi, TNoOffset);
             self#term_offset fmt o
           | _ -> super#term_lval fmt lv
          );
          Format.fprintf fmt "@}"
      end

    method! vdecl fmt vi =
      Format.fprintf fmt "@{<%s>%a@}"
        (Tag.create (PVDecl (self#current_kf, self#current_kinstr, vi)))
        super#vdecl vi

    method private tag_property p =
      current_property <- Some p;
      Tag.create (PIP p)

    method! code_annotation fmt ca =
      match ca.annot_content with
      | APragma p when not (Logic_utils.is_property_pragma p) ->
        (* Not currently localizable. Will be linked to the next stmt *)
        super#code_annotation fmt ca
      | AAssert _ | AInvariant _ | APragma _ | AVariant _ ->
        let ip =
          Property.ip_of_code_annot_single
            (Extlib.the self#current_kf)
            (Extlib.the self#current_stmt)
            ca
        in
        Format.fprintf fmt "@{<%s>%a@}"
          (self#tag_property ip)
          super#code_annotation ca;
      | AStmtSpec (active,_) | AExtended(active,_,_) ->
        (* tags will be set in the inner nodes. *)
        active_behaviors <- active;
        super#code_annotation fmt ca;
        active_behaviors <- [];
      | AAllocation _
      | AAssigns _  ->
        (* tags will be set in the inner nodes. *)
        current_ca <- Some ca;
        super#code_annotation fmt ca;
        current_ca <- None

    method! global fmt g =
      match g with
      (* these globals are already covered by PVDecl *)
      | GVarDecl _ | GVar _ | GFunDecl _ | GFun _ -> super#global fmt g
      | _ ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PGlobal g))
          super#global
          g

    method! extended fmt ext =
      let loc =
        match self#current_kf with
        | None -> Property.ELGlob
        | Some kf -> Property.e_loc_of_stmt kf self#current_kinstr
      in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property Property.(ip_of_extended loc ext))
        super#extended ext;

    method private requires_aux fmt (ip, p) =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property ip)
        super#requires p;

    method! requires fmt p =
      let b = Extlib.the self#current_behavior in
      let ip =
        Property.ip_of_requires
          (Extlib.the self#current_kf) self#current_kinstr b p
      in
      self#requires_aux fmt (ip, p)

    method! behavior fmt b =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_behavior
              (Extlib.the self#current_kf)
              self#current_kinstr
              active_behaviors b))
        super#behavior b

    method! decreases fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_decreases
              (Extlib.the self#current_kf) self#current_kinstr t))
        super#decreases t;

    method! terminates fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_terminates
              (Extlib.the self#current_kf) self#current_kinstr t))
        super#terminates t;

    method! complete_behaviors fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_complete
              (Extlib.the self#current_kf)
              self#current_kinstr
              active_behaviors
              t))
        super#complete_behaviors t

    method! disjoint_behaviors fmt t =
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_disjoint
              (Extlib.the self#current_kf)
              self#current_kinstr
              active_behaviors
              t))
        super#disjoint_behaviors t

    method! assumes fmt p =
      let b = Extlib.the self#current_behavior in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_assumes
              (Extlib.the self#current_kf) self#current_kinstr b p))
        super#assumes p;

    method! post_cond fmt pc =
      let b = Extlib.the self#current_behavior in
      Format.fprintf fmt "@{<%s>%a@}"
        (self#tag_property
           (Property.ip_of_ensures
              (Extlib.the self#current_kf) self#current_kinstr b pc))
        super#post_cond pc;

    method! assigns s fmt a =
      match
        Property.ip_of_assigns (Extlib.the self#current_kf) self#current_kinstr
          self#current_behavior_or_loop a
      with
        None -> super#assigns s fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (self#tag_property ip) (super#assigns s) a

    method! from s fmt ((_, f) as from) =
      match f with
      | FromAny -> super#from s fmt from
      | From _ ->
        let ip =
          Extlib.the
            (Property.ip_of_from
               (Extlib.the self#current_kf) self#current_kinstr
               self#current_behavior_or_loop from)
        in
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) (super#from s) from

    method! global_annotation fmt a =
      match Property.ip_of_global_annotation_single a with
      | None -> super#global_annotation fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) super#global_annotation a

    method! allocation ~isloop fmt a =
      match
        Property.ip_of_allocation (Extlib.the self#current_kf) self#current_kinstr
          self#current_behavior_or_loop a
      with
        None -> super#allocation ~isloop fmt a
      | Some ip ->
        Format.fprintf fmt "@{<%s>%a@}"
          (Tag.create (PIP ip)) (super#allocation ~isloop) a;

    method! stmtkind sattr next fmt sk =
      (* Special tag denoting the start of the statement, WITHOUT any ACSL
         assertion/statement contract, etc. *)
      let s = Extlib.the self#current_stmt in
      let f = Extlib.the self#current_kf in
      let tag = Tag.create (PStmtStart(f,s)) in
      Format.fprintf fmt "@{<%s>%a@}" tag (super#stmtkind sattr next) sk

    initializer force_brace <- true

  end
end

module type Tag =
sig
  val create : localizable -> string
end

module type S_pp =
sig
  include Printer_api.S_pp
  val with_unfold_precond : (stmt -> bool) ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'a -> unit)
end

module Make(T : Tag) =
struct

  let unfold = ref (fun (_ : stmt) -> false)

  let printer () =
    let pp = Printer.current_printer () in
    let module PP = (val pp: Printer.PrinterClass) in
    let module TAG = struct
      let create = T.create
      let unfold s = !unfold s
    end in
    let module TagPrinterClass = BUILD(TAG)(PP) in
    new TagPrinterClass.printer

  let with_unfold_precond unfolder f fmt x =
    let stack = !unfold in
    try unfold := unfolder ; f fmt x ; unfold := stack
    with err -> unfold := stack ; raise err

  include Printer_builder.Make_pp(struct let printer = printer end)

end

(* -------------------------------------------------------------------------- *)
