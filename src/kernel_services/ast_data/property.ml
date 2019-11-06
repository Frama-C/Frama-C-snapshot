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
(* --- Property Type                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

type behavior_or_loop =
    Id_contract of Datatype.String.Set.t * funbehavior
  | Id_loop of code_annotation

type identified_code_annotation = {
  ica_kf : kernel_function;
  ica_stmt : stmt;
  ica_ca : code_annotation
}

type identified_assigns = {
  ias_kf : kernel_function;
  ias_kinstr : kinstr;
  ias_bhv : behavior_or_loop;
  ias_froms : from list
}

type identified_allocation = {
  ial_kf : kernel_function;
  ial_kinstr : kinstr;
  ial_bhv : behavior_or_loop;
  ial_allocs : identified_term list * identified_term list
}

type identified_from = {
  if_kf : kernel_function;
  if_kinstr : kinstr;
  if_bhv : behavior_or_loop;
  if_from : from
}

type identified_decrease = {
  id_kf : kernel_function;
  id_kinstr : kinstr;
  id_ca : code_annotation option;
  id_variant : variant
}

type identified_behavior = {
  ib_kf : kernel_function;
  ib_kinstr : kinstr;
  ib_active : Datatype.String.Set.t;
  ib_bhv : funbehavior
}

type identified_complete = {
  ic_kf : kernel_function;
  ic_kinstr : kinstr;
  ic_active : Datatype.String.Set.t;
  ic_bhvs : string list
}

type identified_disjoint = identified_complete

type predicate_kind =
  | PKRequires of funbehavior
  | PKAssumes of funbehavior
  | PKEnsures of funbehavior * termination_kind
  | PKTerminates

type identified_predicate = {
  ip_kind : predicate_kind;
  ip_kf : kernel_function;
  ip_kinstr : kinstr;
  ip_pred : Cil_types.identified_predicate
}

type program_point = Before | After

type identified_reachable = {
  ir_kf : kernel_function option;
  ir_kinstr : kinstr;
  ir_program_point : program_point
}

type other_loc =
  | OLContract of kernel_function
  | OLStmt of kernel_function * stmt
  | OLGlob of location

type extended_loc =
  | ELContract of kernel_function
  | ELStmt of kernel_function * stmt
  | ELGlob

type identified_extended = {
  ie_loc : extended_loc;
  ie_ext : Cil_types.acsl_extension
}

and identified_axiomatic = {
  iax_name : string;
  iax_props : identified_property list
}

and identified_lemma = {
  il_name : string;
  il_labels : logic_label list;
  il_args : string list;
  il_pred : predicate;
  il_loc : location
}

and identified_axiom = identified_lemma

and identified_instance = {
  ii_kf : kernel_function;
  ii_stmt : stmt;
  ii_pred : Cil_types.identified_predicate option;
  ii_ip : identified_property
}

and identified_type_invariant = {
  iti_name : string;
  iti_type : typ;
  iti_pred : predicate;
  iti_loc : location
}

and identified_global_invariant = {
  igi_name : string;
  igi_pred : predicate;
  igi_loc : location
}

and identified_other = {
  io_name : string;
  io_loc : other_loc
}

and identified_property =
  | IPPredicate of identified_predicate
  | IPExtended of identified_extended
  | IPAxiom of identified_axiom
  | IPAxiomatic of identified_axiomatic
  | IPLemma of identified_lemma
  | IPBehavior of identified_behavior
  | IPComplete of identified_complete
  | IPDisjoint of identified_disjoint
  | IPCodeAnnot of identified_code_annotation
  | IPAllocation of identified_allocation
  | IPAssigns of identified_assigns
  | IPFrom of identified_from
  | IPDecrease of identified_decrease
  | IPReachable of identified_reachable
  | IPPropertyInstance of identified_instance
  | IPTypeInvariant of identified_type_invariant
  | IPGlobalInvariant of identified_global_invariant
  | IPOther of identified_other

let pretty_predicate_kind fmt = function
  | PKRequires _ -> Format.pp_print_string fmt "requires"
  | PKAssumes _ -> Format.pp_print_string fmt "assumes"
  | PKEnsures(_, tk)  ->
    Format.pp_print_string fmt
      (match tk with
       | Normal -> "ensures"
       | Exits -> "exits"
       | Breaks -> "breaks"
       | Continues -> "continues"
       | Returns -> "returns")
  | PKTerminates -> Format.pp_print_string fmt "terminates"

let other_loc_equal le1 le2 =
  match le1, le2 with
  | OLContract kf1, OLContract kf2 -> Kernel_function.equal kf1 kf2
  | OLStmt (_,s1), OLStmt (_,s2) -> Cil_datatype.Stmt.equal s1 s2
  | OLGlob loc1, OLGlob loc2 -> Cil_datatype.Location.equal loc1 loc2
  | (OLContract _ | OLStmt _ | OLGlob _ ), _ -> false

let other_loc_compare le1 le2 =
  match le1, le2 with
  | OLContract kf1, OLContract kf2 -> Kernel_function.compare kf1 kf2
  | OLContract _, _ -> 1 | _, OLContract _ -> -1
  | OLStmt (_,s1), OLStmt (_,s2) -> Cil_datatype.Stmt.compare s1 s2
  | OLStmt _, _ -> 1 | _, OLStmt _ -> -1
  | OLGlob loc1, OLGlob loc2 -> Cil_datatype.Location.compare loc1 loc2

(* -------------------------------------------------------------------------- *)
(* --- Getters                                                            --- *)
(* -------------------------------------------------------------------------- *)

let ki_of_e_loc = function
  | ELContract _ | ELGlob -> Kglobal
  | ELStmt (_,s) -> Kstmt s

let ki_of_o_loc = function
  | OLContract _ | OLGlob _ -> Kglobal
  | OLStmt (_,s) -> Kstmt s

let e_loc_of_stmt kf = function
  | Kglobal -> ELContract kf
  | Kstmt s -> ELStmt (kf,s)

let o_loc_of_stmt kf = function
  | Kglobal -> OLContract kf
  | Kstmt s -> OLStmt (kf,s)

let get_kinstr = function
  | IPPredicate {ip_kinstr=ki}
  | IPBehavior {ib_kinstr=ki}
  | IPComplete {ic_kinstr=ki}
  | IPDisjoint {ic_kinstr=ki}
  | IPAllocation {ial_kinstr=ki}
  | IPAssigns {ias_kinstr=ki}
  | IPFrom {if_kinstr=ki}
  | IPReachable {ir_kinstr=ki}
  | IPDecrease {id_kinstr=ki} -> ki
  | IPAxiom _
  | IPAxiomatic _
  | IPLemma _  -> Kglobal
  | IPOther {io_loc} -> ki_of_o_loc io_loc
  | IPExtended {ie_loc} -> ki_of_e_loc ie_loc
  | IPCodeAnnot {ica_stmt=stmt}
  | IPPropertyInstance {ii_stmt=stmt} -> Kstmt stmt
  | IPTypeInvariant _ | IPGlobalInvariant _ -> Kglobal

let kf_of_loc_e = function
  | ELContract kf | ELStmt (kf,_) -> Some kf
  | ELGlob  -> None

let kf_of_loc_o = function
  | OLContract kf | OLStmt (kf,_) -> Some kf
  | OLGlob _ -> None

let get_kf = function
  | IPPredicate {ip_kf=kf}
  | IPBehavior {ib_kf=kf}
  | IPCodeAnnot {ica_kf=kf}
  | IPComplete {ic_kf=kf}
  | IPDisjoint {ic_kf=kf}
  | IPAllocation {ial_kf=kf}
  | IPAssigns {ias_kf=kf}
  | IPFrom {if_kf=kf}
  | IPDecrease {id_kf=kf}
  | IPPropertyInstance {ii_kf=kf} -> Some kf
  | IPAxiom _
  | IPAxiomatic _
  | IPLemma _ -> None
  | IPReachable {ir_kf} -> ir_kf
  | IPExtended {ie_loc} -> kf_of_loc_e ie_loc
  | IPOther {io_loc} -> kf_of_loc_o io_loc
  | IPTypeInvariant _ | IPGlobalInvariant _ -> None

let loc_of_kf_ki kf = function
  | Kstmt s -> Cil_datatype.Stmt.loc s
  | Kglobal -> Kernel_function.get_location kf

let loc_of_loc_o = function
  | OLContract kf -> Kernel_function.get_location kf
  | OLStmt(_,s) -> Cil_datatype.Stmt.loc s
  | OLGlob loc -> loc

let rec location = function
  | IPPredicate {ip_pred} -> ip_pred.ip_content.pred_loc
  | IPBehavior {ib_kf=kf; ib_kinstr=ki}
  | IPComplete {ic_kf=kf; ic_kinstr=ki}
  | IPDisjoint {ic_kf=kf; ic_kinstr=ki}
  | IPReachable {ir_kf=Some kf; ir_kinstr=ki} -> loc_of_kf_ki kf ki
  | IPReachable {ir_kf=None; ir_kinstr=Kstmt s}
  | IPPropertyInstance {ii_stmt=s} -> Cil_datatype.Stmt.loc s
  | IPCodeAnnot {ica_stmt=s; ica_ca=ca} -> (
      match Cil_datatype.Code_annotation.loc ca with
      | None -> Cil_datatype.Stmt.loc s
      | Some loc -> loc)
  | IPReachable {ir_kf=None; ir_kinstr=Kglobal} -> Cil_datatype.Location.unknown
  | IPAssigns {ias_kf=kf; ias_kinstr=ki; ias_froms=a} ->
    (match a with
     | [] -> loc_of_kf_ki kf ki
     | (t,_) :: _ -> t.it_content.term_loc)
  | IPAllocation {ial_kf=kf; ial_kinstr=ki; ial_allocs=fa} ->
    (match fa with
     | [],[] -> loc_of_kf_ki kf ki
     | (t :: _),_
     | _,(t :: _) -> t.it_content.term_loc)
  | IPFrom {if_from=(t, _)} -> t.it_content.term_loc
  | IPDecrease {id_variant=(t, _)} -> t.term_loc
  | IPAxiom {il_loc} -> il_loc
  | IPAxiomatic {iax_props} ->
    (match iax_props with
     | [] -> Cil_datatype.Location.unknown
     | p :: _ -> location p)
  | IPLemma {il_loc} -> il_loc
  | IPExtended {ie_ext={ext_loc}} -> ext_loc
  | IPOther {io_loc} -> loc_of_loc_o io_loc
  | IPTypeInvariant {iti_loc=loc} | IPGlobalInvariant {igi_loc=loc} -> loc

let source ip =
  let loc = location ip in
  if Cil_datatype.Location.equal loc Cil_datatype.Location.unknown
  then None
  else Some (fst loc)

(* Pretty information about the localization of a IPPropertyInstance *)
let pretty_instance_location fmt (kf, stmt) =
  if Kernel_function.(equal kf (find_englobing_kf stmt))
  then Format.fprintf fmt "at stmt %d" stmt.sid
  else
    Format.fprintf fmt "at stmt %d and function %a"
      stmt.sid Kernel_function.pretty kf

let get_pk_behavior = function
  | PKRequires b | PKAssumes b | PKEnsures (b,_) -> Some b
  | PKTerminates -> None

let get_behavior = function
  | IPPredicate {ip_kind} -> get_pk_behavior ip_kind
  | IPBehavior {ib_bhv=b}
  | IPAllocation {ial_bhv=Id_contract (_, b)}
  | IPAssigns {ias_bhv=Id_contract (_, b)}
  | IPFrom {if_bhv=Id_contract (_, b)} -> Some b
  | IPAllocation {ial_bhv=Id_loop _}
  | IPAssigns {ias_bhv=Id_loop _}
  | IPFrom {if_bhv=Id_loop _}
  | IPAxiom _
  | IPAxiomatic _
  | IPExtended _
  | IPLemma _
  | IPCodeAnnot _
  | IPComplete _
  | IPDisjoint _
  | IPDecrease _
  | IPReachable _
  | IPPropertyInstance _
  | IPTypeInvariant _
  | IPGlobalInvariant _
  | IPOther _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Property Status                                                    --- *)
(* -------------------------------------------------------------------------- *)

let has_status_ext ({ext_has_status} : Cil_types.acsl_extension) =
  ext_has_status

let has_status_ca = function
  | AAssert _ | AStmtSpec _ | AInvariant _ | AVariant _ | AAllocation _
  | AAssigns _ -> true
  | AExtended(_,_,e) -> has_status_ext e
  | APragma _ -> false

let has_status_pkind = function
  | PKAssumes _ -> false
  | PKEnsures _ | PKRequires _ | PKTerminates
    -> true

let rec has_status = function
  | IPPredicate {ip_kind} -> has_status_pkind ip_kind
  | IPExtended {ie_ext} -> has_status_ext ie_ext
  | IPCodeAnnot {ica_ca={annot_content}} -> has_status_ca annot_content
  | IPPropertyInstance {ii_ip} -> has_status ii_ip
  | IPOther _ | IPReachable _
  | IPAxiom _ | IPAxiomatic _ | IPBehavior _
  | IPDisjoint _ | IPComplete _
  | IPAssigns _ | IPFrom _
  | IPAllocation _ | IPDecrease _ | IPLemma _
  | IPTypeInvariant _ | IPGlobalInvariant _
    -> true

(* -------------------------------------------------------------------------- *)
(* --- Datatype                                                           --- *)
(* -------------------------------------------------------------------------- *)

include Datatype.Make_with_collections
    (struct

      include Datatype.Serializable_undefined

      type t = identified_property
      let name = "Property.t"
      let reprs = [IPAxiom {il_name="";il_labels=[];il_args=[];
                            il_pred=Logic_const.ptrue;il_loc=Location.unknown}]
      let mem_project = Datatype.never_any_project

      let pp_active fmt active =
        let sep = ref false in
        let print_one a =
          Format.fprintf fmt "%s%s" (if !sep then ", " else "") a;
          sep:=true
        in
        Datatype.String.Set.iter print_one active

      let rec pretty fmt = function
        | IPPredicate {ip_kind; ip_pred} ->
          Format.fprintf fmt "%a@ %a"
            pretty_predicate_kind ip_kind
            Cil_printer.pp_identified_predicate ip_pred
        | IPExtended {ie_ext} -> Cil_printer.pp_extended fmt ie_ext
        | IPAxiom {il_name} -> Format.fprintf fmt "axiom@ %s" il_name
        | IPAxiomatic {iax_name} -> Format.fprintf fmt "axiomatic@ %s" iax_name
        | IPLemma {il_name} -> Format.fprintf fmt "lemma@ %s" il_name
        | IPTypeInvariant {iti_name; iti_type} ->
          Format.fprintf fmt "invariant@ %s for type %a" iti_name
            Cil_printer.pp_typ iti_type
        | IPGlobalInvariant {igi_name} ->
          Format.fprintf fmt "global invariant@ %s" igi_name
        | IPBehavior {ib_bhv; ib_kinstr; ib_active} ->
          if Cil.is_default_behavior ib_bhv then
            Format.pp_print_string fmt "default behavior"
          else
            Format.fprintf fmt "behavior %s" ib_bhv.b_name;
          (match ib_kinstr with
           | Kstmt s -> Format.fprintf fmt " for statement %d" s.sid
           | Kglobal -> ());
          pp_active fmt ib_active
        | IPCodeAnnot {ica_ca} -> Cil_printer.pp_code_annotation fmt ica_ca
        | IPComplete {ic_active; ic_bhvs} ->
          Format.fprintf fmt "complete@ %a"
            (Pretty_utils.pp_list ~sep:","
               (fun fmt s ->  Format.fprintf fmt "@ %s" s))
            ic_bhvs;
          pp_active fmt ic_active
        | IPDisjoint {ic_active; ic_bhvs} ->
          Format.fprintf fmt "disjoint@ %a"
            (Pretty_utils.pp_list ~sep:","
               (fun fmt s ->  Format.fprintf fmt "@ %s" s))
            ic_bhvs;
          pp_active fmt ic_active
        | IPAllocation {ial_allocs=(f,a)} ->
          Cil_printer.pp_allocation fmt (FreeAlloc(f,a))
        | IPAssigns {ias_froms} -> Cil_printer.pp_assigns fmt (Writes ias_froms)
        | IPFrom {if_from} -> Cil_printer.pp_from fmt if_from
        | IPDecrease {id_ca=None; id_variant=v} -> Cil_printer.pp_decreases fmt v
        | IPDecrease {id_variant=v} -> Cil_printer.pp_variant fmt v
        | IPReachable {ir_kf=None; ir_kinstr=Kstmt _} ->  assert false
        | IPReachable {ir_kf=None; ir_kinstr=Kglobal} ->
          Format.fprintf fmt "reachability of entry point"
        | IPReachable {ir_kf=Some kf; ir_kinstr=Kglobal} ->
          Format.fprintf fmt "reachability of function %a" Kf.pretty kf
        | IPReachable {ir_kf=Some kf; ir_kinstr=Kstmt stmt; ir_program_point=ba} ->
          Format.fprintf fmt "reachability %s stmt %a in %a"
            (match ba with Before -> "of" | After -> "post")
            Cil_datatype.Location.pretty_line (Cil_datatype.Stmt.loc stmt)
            Kf.pretty kf
        | IPPropertyInstance {ii_kf; ii_stmt; ii_ip} ->
          Format.fprintf fmt "status of '%a'%t %a"
            pretty ii_ip
            (fun fmt -> match get_kf ii_ip with
               | Some kf -> Format.fprintf fmt " of %a" Kernel_function.pretty kf
               | None -> ())
            pretty_instance_location (ii_kf, ii_stmt)
        | IPOther {io_name} -> Format.pp_print_string fmt io_name

      let rec hash =
        let hash_bhv_loop = function
          | Id_contract (a,b) -> (0, Hashtbl.hash (a,b.b_name))
          | Id_loop ca -> (1, ca.annot_id)
        in
        function
        | IPPredicate {ip_pred=x} -> Hashtbl.hash (1, x.ip_id)
        | IPAxiom {il_name=x} -> Hashtbl.hash (2, (x:string))
        | IPAxiomatic {iax_name=x} -> Hashtbl.hash (3, (x:string))
        | IPLemma {il_name=x} -> Hashtbl.hash (4, (x:string))
        | IPCodeAnnot {ica_ca=ca} -> Hashtbl.hash (5, ca.annot_id)
        | IPComplete {ic_kf=f; ic_kinstr=ki; ic_bhvs=y; ic_active=x} ->
          (* complete list is more likely to discriminate than active list. *)
          Hashtbl.hash
            (6, Kf.hash f, Kinstr.hash ki,
             (y:string list), (x:Datatype.String.Set.t))
        | IPDisjoint {ic_kf=f; ic_kinstr=ki; ic_bhvs=y; ic_active=x} ->
          Hashtbl.hash
            (7, Kf.hash f, Kinstr.hash ki,
             (y: string list), (x:Datatype.String.Set.t))
        | IPAssigns {ias_kf=f; ias_kinstr=ki; ias_bhv=b} ->
          Hashtbl.hash (8, Kf.hash f, Kinstr.hash ki, hash_bhv_loop b)
        | IPFrom {if_kf=kf; if_kinstr=ki; if_bhv=b; if_from=(t, _)} ->
          Hashtbl.hash
            (9, Kf.hash kf, Kinstr.hash ki,
             hash_bhv_loop b, Identified_term.hash t)
        | IPDecrease {id_kf=kf; id_kinstr=ki} ->
          (* At most one loop variant per statement anyway, no
             need to discriminate against the code annotation itself *)
          Hashtbl.hash (10, Kf.hash kf, Kinstr.hash ki)
        | IPBehavior {ib_kf=kf; ib_kinstr=s; ib_active=a; ib_bhv=b} ->
          Hashtbl.hash
            (11, Kf.hash kf, Kinstr.hash s,
             (b.b_name:string), (a:Datatype.String.Set.t))
        | IPReachable {ir_kf=kf; ir_kinstr=ki; ir_program_point=ba} ->
          Hashtbl.hash(12, Extlib.may_map Kf.hash ~dft:0 kf,
                       Kinstr.hash ki, Hashtbl.hash ba)
        | IPAllocation {ial_kf=f; ial_kinstr=ki; ial_bhv=b} ->
          Hashtbl.hash (13, Kf.hash f, Kinstr.hash ki, hash_bhv_loop b)
        | IPPropertyInstance {ii_kf=kf_caller; ii_stmt=stmt; ii_ip=ip} ->
          Hashtbl.hash (14, Kf.hash kf_caller,
                        Stmt.hash stmt, hash ip)
        | IPOther {io_name=s} -> Hashtbl.hash (15, (s:string))
        | IPTypeInvariant {iti_name=s} -> Hashtbl.hash (16, (s:string))
        | IPGlobalInvariant {igi_name=s} -> Hashtbl.hash (17, (s:string))
        | IPExtended {ie_ext={ext_id}} -> Hashtbl.hash (18, ext_id)

      let rec equal p1 p2 =
        let eq_bhv (f1,ki1,b1) (f2,ki2,b2) =
          Kf.equal f1 f2 && Kinstr.equal ki1 ki2
          &&
          (match b1, b2 with
           | Id_loop ca1, Id_loop ca2 ->
             ca1.annot_id = ca2.annot_id
           | Id_contract (a1,b1), Id_contract (a2,b2) ->
             Datatype.String.Set.equal a1 a2 &&
             Datatype.String.equal b1.b_name b2.b_name
           | Id_loop _, Id_contract _
           | Id_contract _, Id_loop _ -> false)
        in
        match p1, p2 with
        | IPPredicate {ip_pred=s1}, IPPredicate {ip_pred=s2} -> s1.ip_id = s2.ip_id
        | IPExtended {ie_ext={ext_id=i1}}, IPExtended {ie_ext={ext_id=i2}} ->
          Datatype.Int.equal i1 i2
        | IPAxiom {il_name=s1}, IPAxiom {il_name=s2}
        | IPAxiomatic {iax_name=s1}, IPAxiomatic {iax_name=s2}
        | IPTypeInvariant {iti_name=s1}, IPTypeInvariant {iti_name=s2}
        | IPGlobalInvariant {igi_name=s1}, IPGlobalInvariant {igi_name=s2}
        | IPLemma {il_name=s1}, IPLemma {il_name=s2} ->
          Datatype.String.equal s1 s2
        | IPCodeAnnot {ica_ca=ca1}, IPCodeAnnot {ica_ca=ca2} ->
          ca1.annot_id = ca2.annot_id
        | IPComplete {ic_kf=f1;ic_kinstr=ki1;ic_active=a1;ic_bhvs=x1},
          IPComplete {ic_kf=f2;ic_kinstr=ki2;ic_active=a2;ic_bhvs=x2}
        | IPDisjoint {ic_kf=f1;ic_kinstr=ki1;ic_active=a1;ic_bhvs=x1},
          IPDisjoint {ic_kf=f2;ic_kinstr=ki2;ic_active=a2;ic_bhvs=x2} ->
          Kf.equal f1 f2 && Kinstr.equal ki1 ki2 && a1 = a2 && x1 = x2
        | IPAllocation {ial_kf=f1;ial_kinstr=ki1;ial_bhv=b1},
          IPAllocation {ial_kf=f2;ial_kinstr=ki2;ial_bhv=b2}
        | IPAssigns {ias_kf=f1;ias_kinstr=ki1;ias_bhv=b1},
          IPAssigns {ias_kf=f2;ias_kinstr=ki2;ias_bhv=b2} ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2)
        | IPFrom {if_kf=f1;if_kinstr=ki1;if_bhv=b1;if_from=(t1,_)},
          IPFrom {if_kf=f2;if_kinstr=ki2;if_bhv=b2;if_from=(t2,_)} ->
          eq_bhv (f1,ki1,b1) (f2,ki2,b2) && t1.it_id = t2.it_id
        | IPDecrease {id_kf=f1; id_kinstr=ki1},
          IPDecrease {id_kf=f2; id_kinstr=ki2} ->
          Kf.equal f1 f2 && Kinstr.equal ki1 ki2
        | IPReachable {ir_kf=kf1; ir_kinstr=ki1; ir_program_point=ba1},
          IPReachable {ir_kf=kf2; ir_kinstr=ki2; ir_program_point=ba2} ->
          Extlib.opt_equal Kf.equal kf1 kf2 && Kinstr.equal ki1 ki2 && ba1 = ba2
        | IPBehavior {ib_kf=f1; ib_kinstr=k1; ib_active=a1; ib_bhv=b1},
          IPBehavior {ib_kf=f2; ib_kinstr=k2; ib_active=a2; ib_bhv=b2} ->
          Kf.equal f1 f2
          && Kinstr.equal k1 k2
          && Datatype.String.Set.equal a1 a2
          && Datatype.String.equal b1.b_name b2.b_name
        | IPOther {io_name=s1;io_loc=l1}, IPOther {io_name=s2;io_loc=l2} ->
          Datatype.String.equal s1 s2
          && other_loc_equal l1 l2
        | IPPropertyInstance {ii_kf=kf1;ii_stmt=s1;ii_ip=ip1},
          IPPropertyInstance {ii_kf=kf2;ii_stmt=s2;ii_ip=ip2} ->
          Kernel_function.equal kf1 kf2 &&
          Stmt.equal s1 s2 && equal ip1 ip2
        | (IPPredicate _ | IPAxiom _ | IPExtended _ | IPAxiomatic _ | IPLemma _
          | IPCodeAnnot _ | IPComplete _ | IPDisjoint _ | IPAssigns _
          | IPFrom _ | IPDecrease _ | IPBehavior _ | IPReachable _
          | IPAllocation _ | IPOther _ | IPPropertyInstance _
          | IPTypeInvariant _ | IPGlobalInvariant _), _ -> false

      let rec compare x y =
        let cmp_bhv (f1,ki1,b1) (f2,ki2,b2) =
          let n = Kf.compare f1 f2 in
          if n = 0 then
            let n = Kinstr.compare ki1 ki2 in
            if n = 0 then
              match b1, b2 with
              | Id_contract (a1,b1), Id_contract (a2,b2) ->
                let n = Datatype.String.compare b1.b_name b2.b_name in
                if n = 0 then Datatype.String.Set.compare a1 a2 else n
              | Id_loop ca1, Id_loop ca2 ->
                Datatype.Int.compare ca1.annot_id ca2.annot_id
              | Id_contract _, Id_loop _ -> -1
              | Id_loop _, Id_contract _ -> 1
            else n
          else n
        in
        match x, y with
        | IPPredicate {ip_pred=s1}, IPPredicate {ip_pred=s2} ->
          Datatype.Int.compare s1.ip_id s2.ip_id
        | IPExtended {ie_ext={ext_id=i1}}, IPExtended {ie_ext={ext_id=i2}} ->
          Datatype.Int.compare i1 i2
        | IPCodeAnnot {ica_ca=ca1}, IPCodeAnnot {ica_ca=ca2} ->
          Datatype.Int.compare ca1.annot_id ca2.annot_id
        | IPBehavior {ib_kf=f1; ib_kinstr=k1; ib_active=a1; ib_bhv=b1},
          IPBehavior {ib_kf=f2; ib_kinstr=k2; ib_active=a2; ib_bhv=b2} ->
          cmp_bhv (f1, k1, Id_contract (a1,b1)) (f2, k2, Id_contract (a2,b2))
        | IPComplete {ic_kf=f1;ic_kinstr=ki1;ic_active=a1;ic_bhvs=x1},
          IPComplete {ic_kf=f2;ic_kinstr=ki2;ic_active=a2;ic_bhvs=x2}
        | IPDisjoint {ic_kf=f1;ic_kinstr=ki1;ic_active=a1;ic_bhvs=x1},
          IPDisjoint {ic_kf=f2;ic_kinstr=ki2;ic_active=a2;ic_bhvs=x2} ->
          let n = Kf.compare f1 f2 in
          if n = 0 then
            let n = Kinstr.compare ki1 ki2 in
            if n = 0 then
              let n = Extlib.compare_basic x1 x2 in
              if n = 0 then
                Datatype.String.Set.compare a1 a2
              else n
            else n
          else n
        | IPAssigns {ias_kf=f1;ias_kinstr=ki1;ias_bhv=b1},
          IPAssigns {ias_kf=f2;ias_kinstr=ki2;ias_bhv=b2} ->
          cmp_bhv (f1,ki1,b1) (f2,ki2,b2)
        | IPFrom {if_kf=f1;if_kinstr=ki1;if_bhv=b1;if_from=(t1,_)},
          IPFrom {if_kf=f2;if_kinstr=ki2;if_bhv=b2;if_from=(t2,_)} ->
          let n = cmp_bhv (f1,ki1,b1) (f2,ki2,b2) in
          if n = 0 then Identified_term.compare t1 t2 else n
        | IPDecrease {id_kf=f1; id_kinstr=ki1},
          IPDecrease {id_kf=f2; id_kinstr=ki2} ->
          let n = Kf.compare f1 f2 in
          if n = 0 then Kinstr.compare ki1 ki2 else n
        | IPReachable {ir_kf=kf1; ir_kinstr=ki1; ir_program_point=ba1},
          IPReachable {ir_kf=kf2; ir_kinstr=ki2; ir_program_point=ba2} ->
          let n = Extlib.opt_compare Kf.compare kf1 kf2 in
          if n = 0 then
            let n = Kinstr.compare ki1 ki2 in
            if n = 0 then Transitioning.Stdlib.compare ba1 ba2 else n
          else
            n
        | IPAxiom {il_name=s1}, IPAxiom {il_name=s2}
        | IPAxiomatic {iax_name=s1}, IPAxiomatic {iax_name=s2}
        | IPTypeInvariant {iti_name=s1}, IPTypeInvariant {iti_name=s2}
        | IPGlobalInvariant {igi_name=s1}, IPGlobalInvariant {igi_name=s2}
        | IPLemma {il_name=s1}, IPLemma {il_name=s2} ->
          Datatype.String.compare s1 s2
        | IPOther {io_name=s1;io_loc=l1}, IPOther {io_name=s2;io_loc=l2} ->
          let s = Datatype.String.compare s1 s2 in
          if s <> 0 then s else other_loc_compare l1 l2
        | IPAllocation {ial_kf=f1;ial_kinstr=ki1;ial_bhv=b1},
          IPAllocation {ial_kf=f2;ial_kinstr=ki2;ial_bhv=b2} ->
          cmp_bhv (f1,ki1,b1) (f2,ki2,b2)
        | IPPropertyInstance {ii_kf=kf1;ii_stmt=s1;ii_ip=ip1},
          IPPropertyInstance {ii_kf=kf2;ii_stmt=s2;ii_ip=ip2} ->
          let c = Kernel_function.compare kf1 kf2 in
          if c <> 0 then c else
            let c = Stmt.compare s1 s2 in
            if c <> 0 then c else compare ip1 ip2
        | (IPPredicate _ | IPExtended _ | IPCodeAnnot _ | IPBehavior _ | IPComplete _ |
           IPDisjoint _ | IPAssigns _ | IPFrom _ | IPDecrease _ |
           IPReachable _ | IPAxiom _ | IPAxiomatic _ | IPLemma _ |
           IPOther _ | IPAllocation _ | IPPropertyInstance _ |
           IPTypeInvariant _ | IPGlobalInvariant _) as x, y ->
          let nb = function
            | IPPredicate _ -> 1
            | IPAssigns _ -> 2
            | IPDecrease _ -> 3
            | IPAxiom _ -> 4
            | IPAxiomatic _ -> 5
            | IPLemma _ -> 6
            | IPCodeAnnot _ -> 7
            | IPComplete _ -> 8
            | IPDisjoint _ -> 9
            | IPFrom _ -> 10
            | IPBehavior _ -> 11
            | IPReachable _ -> 12
            | IPAllocation _ -> 13
            | IPOther _ -> 14
            | IPPropertyInstance _ -> 15
            | IPTypeInvariant _ -> 16
            | IPGlobalInvariant _ -> 17
            | IPExtended _ -> 18
          in
          Datatype.Int.compare (nb x) (nb y)

    end)

let rec short_pretty fmt p = match p with
  | IPPredicate {ip_pred={ip_content={pred_name=name::_}}} ->
    Format.pp_print_string fmt name
  | IPPredicate _ -> pretty fmt p
  | IPExtended {ie_ext={ext_name}} -> Format.pp_print_string fmt ext_name
  | IPAxiom {il_name=n} | IPLemma {il_name=n}
  | IPTypeInvariant {iti_name=n} | IPGlobalInvariant {igi_name=n}
  | IPAxiomatic {iax_name=n} -> Format.pp_print_string fmt n
  | IPBehavior {ib_kf; ib_bhv={b_name}} ->
    Format.fprintf fmt "behavior %s in function %a"
      b_name Kernel_function.pretty ib_kf
  | IPComplete {ic_kf} ->
    Format.fprintf fmt "complete clause in function %a"
      Kernel_function.pretty ic_kf
  | IPDisjoint {ic_kf} ->
    Format.fprintf fmt "disjoint clause in function %a"
      Kernel_function.pretty ic_kf
  | IPCodeAnnot {ica_ca={annot_content=AAssert (_, _, {pred_name=name::_})}}
  | IPCodeAnnot {ica_ca={annot_content=AInvariant (_, _, {pred_name=name::_})}} ->
    Format.pp_print_string fmt name
  | IPCodeAnnot _ -> pretty fmt p
  | IPAllocation {ial_kf} ->
    Format.fprintf fmt "allocates/frees clause in function %a"
      Kernel_function.pretty ial_kf
  | IPAssigns {ias_kf} ->
    Format.fprintf fmt "assigns clause in function %a"
      Kernel_function.pretty ias_kf
  | IPFrom {if_kf; if_from=(t, _)} ->
    Format.fprintf fmt "from clause of term %a in function %a"
      Cil_printer.pp_identified_term t Kernel_function.pretty if_kf
  | IPDecrease {id_kf} ->
    Format.fprintf fmt "decrease clause in function %a"
      Kernel_function.pretty id_kf
  | IPPropertyInstance {ii_kf; ii_stmt; ii_ip} ->
    Format.fprintf fmt "specialization of %a %a" short_pretty ii_ip
      pretty_instance_location (ii_kf, ii_stmt)
  | IPReachable _ | IPOther _ -> pretty fmt p

let pp_behavior_or_loop_debug fmt = function
  | Id_contract(s,fb) ->
    Format.fprintf fmt "Id_contract(%a,%a)"
      Datatype.String.Set.pretty s
      Cil_types_debug.pp_funbehavior fb
  | Id_loop ca ->
    Format.fprintf fmt "Id_loop(%a)"
      Cil_types_debug.pp_code_annotation ca

let pp_predicate_type_debug fmt = function
  | PKRequires fb ->
    Format.fprintf fmt "PKRequires(%a)"
      Cil_types_debug.pp_funbehavior fb
  | PKAssumes fb ->
    Format.fprintf fmt "PKAssumes(%a)"
      Cil_types_debug.pp_funbehavior fb
  | PKEnsures (fb, tk) ->
    Format.fprintf fmt "PKEnsures(%a,%a)"
      Cil_types_debug.pp_funbehavior fb
      Cil_types_debug.pp_termination_kind tk
  | PKTerminates ->
    Format.fprintf fmt "PKTerminates"

let pp_program_point fmt = function
  | Before -> Format.fprintf fmt "Before"
  | After -> Format.fprintf fmt "After"

let pp_extended_loc fmt = function
  | ELContract kf ->
    Format.fprintf fmt "ELContract(%a)"
      Cil_types_debug.pp_kernel_function kf
  | ELStmt(kf,s) ->
    Format.fprintf fmt "ELStmt(%a,%a)"
      Cil_types_debug.pp_kernel_function kf
      Cil_types_debug.pp_stmt s
  | ELGlob -> Format.pp_print_string fmt "ELGlob"

let pp_other_loc fmt = function
  | OLContract kf ->
    Format.fprintf fmt "ELContract(%a)"
      Cil_types_debug.pp_kernel_function kf
  | OLStmt (kf,s) ->
    Format.fprintf fmt "ELStmt(%a,%a)"
      Cil_types_debug.pp_kernel_function kf
      Cil_types_debug.pp_stmt s
  | OLGlob loc ->
    Format.fprintf fmt "OLGlob(%a)" Cil_types_debug.pp_location loc


let rec pretty_debug fmt = function
  | IPPredicate {ip_kind;ip_kf;ip_kinstr;ip_pred} ->
    Format.fprintf fmt "IPPredicate(%a,%a,%a,%a)"
      pp_predicate_type_debug ip_kind
      Cil_types_debug.pp_kernel_function ip_kf
      Cil_types_debug.pp_kinstr ip_kinstr
      Cil_types_debug.pp_identified_predicate ip_pred
  | IPExtended {ie_ext;ie_loc} ->
    Format.fprintf fmt "IPExtended(%a,%a)"
      pp_extended_loc ie_loc
      Cil_types_debug.pp_acsl_extension ie_ext
  | IPCodeAnnot {ica_kf; ica_stmt; ica_ca} ->
    Format.fprintf fmt "IPCodeAnnot(%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ica_kf
      Cil_types_debug.pp_stmt ica_stmt
      Cil_types_debug.pp_code_annotation ica_ca
  | IPComplete {ic_kf; ic_kinstr; ic_active; ic_bhvs} ->
    Format.fprintf fmt "IPComplete(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ic_kf
      Cil_types_debug.pp_kinstr ic_kinstr
      Datatype.String.Set.pretty ic_active
      (Cil_types_debug.pp_list Cil_types_debug.pp_string) ic_bhvs
  | IPDisjoint {ic_kf; ic_kinstr; ic_active; ic_bhvs} ->
    Format.fprintf fmt "IPDisjoint(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ic_kf
      Cil_types_debug.pp_kinstr ic_kinstr
      Datatype.String.Set.pretty ic_active
      (Cil_types_debug.pp_list Cil_types_debug.pp_string) ic_bhvs
  | IPDecrease {id_kf; id_kinstr; id_ca; id_variant} ->
    Format.fprintf fmt "IPDecrease(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function id_kf
      Cil_types_debug.pp_kinstr id_kinstr
      (Cil_types_debug.pp_option Cil_types_debug.pp_code_annotation) id_ca
      Cil_types_debug.pp_variant id_variant
  | IPAxiom {il_name; il_labels; il_args; il_pred; il_loc} ->
    Format.fprintf fmt "IPAxiom(%a,%a,%a,%a,%a)"
      Cil_types_debug.pp_string il_name
      (Cil_types_debug.pp_list Cil_types_debug.pp_logic_label) il_labels
      (Cil_types_debug.pp_list Cil_types_debug.pp_string) il_args
      Cil_types_debug.pp_predicate il_pred
      Cil_types_debug.pp_location il_loc
  | IPAxiomatic {iax_name; iax_props} ->
    Format.fprintf fmt "IPAxiomatic(%a,%a)"
      Cil_types_debug.pp_string iax_name
      (Cil_types_debug.pp_list pretty_debug) iax_props
  | IPLemma {il_name; il_labels; il_args; il_pred; il_loc} ->
    Format.fprintf fmt "IPLemma(%a,%a,%a,%a,%a)"
      Cil_types_debug.pp_string il_name
      (Cil_types_debug.pp_list Cil_types_debug.pp_logic_label) il_labels
      (Cil_types_debug.pp_list Cil_types_debug.pp_string) il_args
      Cil_types_debug.pp_predicate il_pred
      Cil_types_debug.pp_location il_loc
  | IPTypeInvariant {iti_name; iti_type; iti_pred; iti_loc} ->
    Format.fprintf fmt "IPTypeInvariant(%a,%a,%a,%a)"
      Cil_types_debug.pp_string iti_name
      Cil_types_debug.pp_typ iti_type
      Cil_types_debug.pp_predicate iti_pred
      Cil_types_debug.pp_location iti_loc
  | IPGlobalInvariant {igi_name; igi_pred; igi_loc} ->
    Format.fprintf fmt "IPGlobalInvariant(%a,%a,%a)"
      Cil_types_debug.pp_string igi_name
      Cil_types_debug.pp_predicate igi_pred
      Cil_types_debug.pp_location igi_loc
  | IPAllocation {ial_kf; ial_kinstr; ial_bhv; ial_allocs} ->
    Format.fprintf fmt "IPAllocation(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ial_kf
      Cil_types_debug.pp_kinstr ial_kinstr
      pp_behavior_or_loop_debug ial_bhv
      (Cil_types_debug.pp_pair
         (Cil_types_debug.pp_list Cil_types_debug.pp_identified_term)
         (Cil_types_debug.pp_list Cil_types_debug.pp_identified_term)
      ) ial_allocs
  | IPAssigns {ias_kf; ias_kinstr; ias_bhv; ias_froms} ->
    Format.fprintf fmt "IPAssigns(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ias_kf
      Cil_types_debug.pp_kinstr ias_kinstr
      pp_behavior_or_loop_debug ias_bhv
      (Cil_types_debug.pp_list Cil_types_debug.pp_from) ias_froms
  | IPFrom {if_kf; if_kinstr; if_bhv; if_from} ->
    Format.fprintf fmt "IPFrom(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function if_kf
      Cil_types_debug.pp_kinstr if_kinstr
      pp_behavior_or_loop_debug if_bhv
      Cil_types_debug.pp_from if_from
  | IPReachable {ir_kf; ir_kinstr; ir_program_point} ->
    Format.fprintf fmt "IPReachable(%a,%a,%a)"
      (Cil_types_debug.pp_option Cil_types_debug.pp_kernel_function) ir_kf
      Cil_types_debug.pp_kinstr ir_kinstr
      pp_program_point ir_program_point
  | IPBehavior {ib_kf; ib_kinstr; ib_active; ib_bhv} ->
    Format.fprintf fmt "IPBehavior(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ib_kf
      Cil_types_debug.pp_kinstr ib_kinstr
      Datatype.String.Set.pretty ib_active
      Cil_types_debug.pp_funbehavior ib_bhv
  | IPPropertyInstance {ii_kf; ii_stmt; ii_pred; ii_ip} ->
    Format.fprintf fmt "IPPropertyInstance(%a,%a,%a,%a)"
      Cil_types_debug.pp_kernel_function ii_kf
      Cil_types_debug.pp_stmt ii_stmt
      (Cil_types_debug.pp_option Cil_types_debug.pp_identified_predicate)
      ii_pred pretty_debug ii_ip
  | IPOther {io_name; io_loc} ->
    Format.fprintf fmt "IPOther(%a,%a)"
      Cil_types_debug.pp_string io_name
      pp_other_loc io_loc

(* -------------------------------------------------------------------------- *)
(* --- Legacy Property Names                                              --- *)
(* -------------------------------------------------------------------------- *)

(* Shall be deprecated *)
module LegacyNames =
struct

  module NamesTbl =
    State_builder.Hashtbl(Datatype.String.Hashtbl)(Datatype.Int)
      (struct
        let name = "PropertyNames"
        let dependencies = [ ]
        let size = 97
      end)
  module IndexTbl =
    State_builder.Hashtbl(Hashtbl)(Datatype.String)
      (struct
        let name = "PropertyIndex"
        let dependencies = [ Ast.self; NamesTbl.self; Globals.Functions.self ]
        let size = 97
      end)

  let self = IndexTbl.self

  let kf_prefix kf = (Ast_info.Function.get_vi kf.fundec).vname ^ "_"

  let ident_names names =
    List.filter (function "" -> true
                        | _ as n -> '\"' <> (String.get n 0) ) names

  let pp_names fmt l =
    let l = ident_names l in
    match l with [] -> ()
               | _ -> Format.fprintf fmt "_%a"
                        (Pretty_utils.pp_list ~sep:"_" Format.pp_print_string) l

  let pp_code_annot_names fmt ca =
    match ca.annot_content with
    | AAssert(for_bhv,_,named_pred) | AInvariant(for_bhv,_,named_pred) ->
      let pp_for_bhv fmt l =
        match l with
        | [] -> ()
        | _ -> Format.fprintf fmt "_for_%a"
                 (Pretty_utils.pp_list ~sep:"_" Format.pp_print_string) l
      in
      Format.fprintf fmt "%a%a" pp_names named_pred.pred_name pp_for_bhv for_bhv
    | AVariant(term, _) -> pp_names fmt term.term_name
    | _ -> () (* TODO : add some more names ? *)

  let behavior_prefix b =
    if Cil.is_default_behavior b then ""
    else b.b_name ^ "_"

  let variant_suffix = function
    | (_,Some s) -> s
    | _ -> ""

  let string_of_termination_kind = function
      Normal -> "post"
    | Exits -> "exit"
    | Breaks -> "break"
    | Continues -> "continue"
    | Returns -> "return"

  let stmt_prefix _s = "stmt_"

  let ki_prefix = function Kglobal -> "" | Kstmt s -> stmt_prefix s

  let extended_loc_prefix = function
    | ELContract kf -> kf_prefix kf
    | ELStmt (kf,s) -> kf_prefix kf ^ stmt_prefix s
    | ELGlob -> "global_"

  let other_loc_prefix = function
    | OLContract kf -> kf_prefix kf
    | OLStmt (kf,s) -> kf_prefix kf ^ stmt_prefix s
    | OLGlob _ -> "global_"

  let predicate_kind_txt pk ki =
    let name = match pk with
      | PKRequires b -> (behavior_prefix b) ^ "pre"
      | PKAssumes b -> (behavior_prefix b) ^ "assume"
      | PKEnsures (b, tk) -> (behavior_prefix b) ^ string_of_termination_kind tk
      | PKTerminates -> "term"
    in
    (ki_prefix ki) ^ name

  let active_prefix fmt a =
    let print_one a = Format.fprintf fmt "_%s" a in
    Datatype.String.Set.iter print_one a

  let rec id_prop_txt p = match p with
    | IPPredicate {ip_kind=pk;ip_kf=kf;ip_kinstr=ki;ip_pred=idp} ->
      Format.asprintf "%s%s%a"
        (kf_prefix kf) (predicate_kind_txt pk ki)
        pp_names idp.ip_content.pred_name
    | IPExtended {ie_ext={ext_name};ie_loc=le} ->
      Format.asprintf  "%sextended%a" (extended_loc_prefix le) pp_names [ext_name]
    | IPCodeAnnot {ica_kf=kf; ica_ca=ca} ->
      let name = match ca.annot_content with
        | AAssert (_, Assert, _) -> "assert"
        | AAssert (_, Check, _) -> "check"
        | AInvariant (_,true,_) -> "loop_inv"
        | AInvariant _ -> "inv"
        | APragma _ -> "pragma"
        | AStmtSpec _ -> "contract"
        | AAssigns _ -> "assigns"
        | AVariant _ -> "variant"
        | AAllocation _ -> "allocates"
        | AExtended(_,_,{ext_name}) -> ext_name
      in Format.asprintf "%s%s%a" (kf_prefix kf) name pp_code_annot_names ca
    | IPComplete {ic_kf=kf; ic_kinstr=ki; ic_active=a; ic_bhvs=lb} ->
      Format.asprintf  "%s%s%acomplete%a"
        (kf_prefix kf) (ki_prefix ki) active_prefix a pp_names lb
    | IPDisjoint {ic_kf=kf; ic_kinstr=ki; ic_active=a; ic_bhvs=lb} ->
      Format.asprintf  "%s%s%adisjoint%a"
        (kf_prefix kf) (ki_prefix ki) active_prefix a pp_names lb
    | IPDecrease {id_kf=kf; id_ca=None; id_variant=variant} ->
      (kf_prefix kf) ^ "decr" ^ (variant_suffix variant)
    | IPDecrease {id_kf=kf; id_variant=variant} ->
      (kf_prefix kf) ^ "loop_term" ^ (variant_suffix variant)
    | IPAxiom {il_name=name; il_pred} ->
      Format.asprintf "axiom_%s%a" name pp_names il_pred.pred_name
    | IPAxiomatic {iax_name} -> "axiomatic_" ^ iax_name
    | IPLemma {il_name=name; il_pred} ->
      Format.asprintf "lemma_%s%a" name pp_names il_pred.pred_name
    | IPTypeInvariant {iti_name; iti_pred} ->
      Format.asprintf "type_invariant_%s%a"
        iti_name pp_names iti_pred.pred_name
    | IPGlobalInvariant {igi_name; igi_pred} ->
      Format.asprintf "global_invariant_%s%a"
        igi_name pp_names igi_pred.pred_name
    | IPAllocation {ial_kf=kf; ial_kinstr=ki; ial_bhv=Id_contract (a,b)} ->
      Format.asprintf "%s%s%a%salloc"
        (kf_prefix kf) (ki_prefix ki) active_prefix a (behavior_prefix b)
    | IPAllocation {ial_kf=kf; ial_kinstr=Kstmt _; ial_bhv=Id_loop ca} ->
      Format.asprintf "%sloop_alloc%a"
        (kf_prefix kf) pp_code_annot_names ca
    | IPAllocation _ -> assert false
    | IPAssigns {ias_kf=kf; ias_kinstr=ki; ias_bhv=Id_contract (a,b)} ->
      Format.asprintf "%s%s%a%sassign"
        (kf_prefix kf) (ki_prefix ki) active_prefix a (behavior_prefix b)
    | IPAssigns {ias_kf=kf; ias_kinstr=Kstmt _; ias_bhv=Id_loop ca} ->
      Format.asprintf "%sloop_assign%a"
        (kf_prefix kf) pp_code_annot_names ca
    | IPAssigns _ -> assert false
    | IPFrom {if_from=(out,_)} ->
      "from_id_"^(string_of_int (out.it_id))
    | IPReachable _ -> "reachable_stmt"
    | IPBehavior {ib_kf=kf; ib_kinstr=ki; ib_active=a; ib_bhv=b} ->
      Format.asprintf "%s%s%a%s"
        (kf_prefix kf) (ki_prefix ki) active_prefix a b.b_name
    | IPPropertyInstance {ii_kf=kf; ii_stmt=stmt; ii_ip=ip} ->
      Format.asprintf "specialization_%s_at_%t" (id_prop_txt ip)
        (fun fmt ->
           Format.fprintf fmt "%a_stmt_%d" Kernel_function.pretty kf stmt.sid)
    | IPOther {io_name; io_loc} -> other_loc_prefix io_loc ^ io_name

  (** function used to normalize basename *)
  let normalize_basename s =
    let is_valid_char_id = function
      | 'a'..'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    and is_numeric = function
      | '0'..'9' -> true
      | _ -> false
    in
    if s ="" then "property"
    else
      let s = String.map (fun c -> if not (is_valid_char_id c) then '_' else c) s in
      if is_numeric s.[0] then "property_" ^ s else s

  (** returns the name that should be returned by the function [get_prop_name_id] if the given property has [name] as basename. That name is reserved so that [get_prop_name_id prop] can never return an identical name. *)
  let reserve_name_id basename =
    let basename = normalize_basename basename in
    try
      let speed_up_start = NamesTbl.find basename in
      (* this basename is already reserved *)
      let n,unique_name = Extlib.make_unique_name NamesTbl.mem ~sep:"_" ~start:speed_up_start basename
      in NamesTbl.replace basename (succ n) ; (* to speed up Extlib.make_unique_name for next time *)
      unique_name
    with Not_found -> (* first time that basename is reserved *)
      NamesTbl.add basename 2 ;
      basename

  (** returns the basename of the property. *)
  let get_prop_basename ip = normalize_basename (id_prop_txt ip)

  (** returns a unique name identifying the property.
      This name is built from the basename of the property. *)
  let get_prop_name_id ip =
    try IndexTbl.find ip
    with Not_found -> (* first time we are asking for a name for that [ip] *)
      let basename = get_prop_basename ip in
      let unique_name = reserve_name_id basename in
      IndexTbl.add ip unique_name ;
      unique_name

end

(* -------------------------------------------------------------------------- *)
(* --- Property Names                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Names =
struct

  open Cil_types

  type part =
    | B of behavior
    | K of kernel_function
    | A of string
    | I of identified_predicate
    | P of predicate
    | T of term
    | S of stmt

  let add_part buffer = function
    | B bhv ->
      if not (Cil.is_default_behavior bhv)
      then Sanitizer.add_string buffer bhv.b_name
    | K kf -> Sanitizer.add_string buffer (Kernel_function.get_name kf)
    | A msg -> Sanitizer.add_string buffer msg
    | S stmt -> Sanitizer.add_string buffer (Printf.sprintf "s%d" stmt.sid)
    | I { ip_content = { pred_name = a } }
    | P { pred_name = a } | T { term_name = a } -> Sanitizer.add_list buffer a

  let rec add_parts buffer = function
    | [] -> ()
    | p::ps ->
      let open Sanitizer in
      add_part buffer p ; add_sep buffer ; add_parts buffer ps

  let rec parts_of_property ip : part list =
    match ip with
    | IPBehavior {ib_kf; ib_kinstr=Kglobal; ib_bhv} ->
      [ K ib_kf ; B ib_bhv ]
    | IPBehavior {ib_kf; ib_kinstr=Kstmt s; ib_bhv} ->
      [ K ib_kf ; B ib_bhv ; S s]

    | IPPredicate {ip_kind=PKAssumes bhv; ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "assumes" ; I ip ]
    | IPPredicate {ip_kind=PKRequires bhv; ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "requires" ; I ip ]
    | IPPredicate {ip_kind=PKEnsures (bhv, Normal); ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "ensures" ; I ip ]
    | IPPredicate {ip_kind=PKEnsures (bhv, Exits); ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "exits" ; I ip ]
    | IPPredicate {ip_kind=PKEnsures (bhv, Breaks); ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "breaks" ; I ip ]
    | IPPredicate {ip_kind=PKEnsures (bhv, Continues); ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "continues" ; I ip ]
    | IPPredicate {ip_kind=PKEnsures (bhv, Returns); ip_kf=kf; ip_pred=ip} ->
      [ K kf ; B bhv ; A "returns" ; I ip ]
    | IPPredicate {ip_kind=PKTerminates; ip_kf=kf; ip_pred=ip} ->
      [ K kf ; A "terminates" ; I ip ]

    | IPAllocation {ial_kf=kf; ial_bhv=Id_contract (_, bhv)} ->
      [ K kf ; B bhv ; A "allocates" ]
    | IPAllocation {ial_kf=kf; ial_bhv=Id_loop _} ->
      [ K kf ; A "loop_allocates" ]

    | IPAssigns {ias_kf=kf; ias_bhv=Id_contract (_, bhv)} ->
      [ K kf ; B bhv ; A "assigns" ]
    | IPAssigns {ias_kf=kf; ias_bhv=Id_loop _} ->
      [ K kf ; A "loop_assigns" ]

    | IPFrom {if_kf=kf; if_bhv=Id_contract (_, bhv)} ->
      [ K kf ; B bhv ; A "assigns_from" ]
    | IPFrom {if_kf=kf; if_bhv=Id_loop _} ->
      [ K kf ; A "loop_assigns_from" ]

    | IPDecrease {id_kf=kf; id_ca=None} ->
      [ K kf ; A "variant" ]
    | IPDecrease {id_kf=kf; id_ca=Some _} ->
      [ K kf ; A "loop_variant" ]

    | IPCodeAnnot {ica_kf=kf; ica_stmt=stmt; ica_ca={annot_content=AStmtSpec _}} ->
      [ K kf ; A "contract" ; S stmt ]
    | IPCodeAnnot {ica_kf=kf; ica_stmt=stmt; ica_ca={annot_content=APragma _}} ->
      [ K kf ; A "pragma" ; S stmt ]
    | IPCodeAnnot {ica_kf=kf; ica_stmt=stmt;
                   ica_ca={annot_content=AExtended (_, _, {ext_name})}} ->
      [ K kf ; A ext_name ; S stmt ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AAssert (_, Assert,p)}} ->
      [K kf ; A "assert" ; P p ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AAssert (_, Check,p)}} ->
      [K kf ; A "check" ; P p ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AInvariant (_, true, p)}} ->
      [K kf ; A "loop_invariant" ; P p ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AInvariant (_, false, p)}} ->
      [K kf ; A "invariant" ; P p ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AVariant (e, _)}} ->
      [K kf ; A "loop_variant" ; T e ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AAssigns _}} ->
      [K kf ; A "loop_assigns" ]
    | IPCodeAnnot {ica_kf=kf; ica_ca={annot_content=AAllocation _}} ->
      [K kf ; A "loop_allocates" ]

    | IPComplete {ic_kf=kf; ic_bhvs=cs} ->
      (K kf :: A "complete" :: List.map (fun a -> A a) cs)
    | IPDisjoint {ic_kf=kf; ic_bhvs=cs} ->
      (K kf :: A "disjoint" :: List.map (fun a -> A a) cs)

    | IPReachable {ir_kf=None} -> []
    | IPReachable {ir_kf=Some kf; ir_kinstr=Kglobal; ir_program_point=Before} ->
      [ K kf ; A "reachable" ]
    | IPReachable {ir_kf=Some kf; ir_kinstr=Kglobal; ir_program_point=After} ->
      [ K kf ; A "reachable_post" ]
    | IPReachable {ir_kf=Some kf; ir_kinstr=Kstmt s; ir_program_point=Before} ->
      [ K kf ; A "reachable" ; S s ]
    | IPReachable {ir_kf=Some kf; ir_kinstr=Kstmt s; ir_program_point=After} ->
      [ K kf ; A "reachable_after" ; S s ]

    | IPAxiomatic _
    | IPAxiom _ -> []
    | IPLemma {il_name=name; il_pred=p} ->
      [ A "lemma" ; A name ; P p ]

    | IPTypeInvariant {iti_name=name}
    | IPGlobalInvariant {igi_name=name} ->
      [ A "invariant" ; A name ]

    | IPOther {io_name=name; io_loc=OLGlob _} -> [ A name ]
    | IPOther {io_name=name; io_loc=OLContract kf} -> [ K kf ; A name ]
    | IPOther {io_name=name; io_loc=OLStmt (kf, s)} -> [ K kf ; A name ; S s ]

    | IPExtended {ie_loc=ELGlob; ie_ext={ext_name}} -> [A ext_name]
    | IPExtended {ie_loc=ELContract kf; ie_ext={ext_name}} -> [K kf ; A ext_name]
    | IPExtended {ie_loc=ELStmt (kf,s); ie_ext={ext_name}} ->
      [K kf ; A ext_name ; S s]

    | IPPropertyInstance {ii_ip} -> parts_of_property ii_ip

  let get_prop_basename ?truncate ip =
    let buffer =
      match truncate with
      | None -> Sanitizer.create ~truncate:false 20
      | Some n -> Sanitizer.create ~truncate:true n
    in
    add_parts buffer (parts_of_property ip) ;
    Sanitizer.contents buffer

  (* Numerotation of properties with same basename *)
  module NamesTbl =
    State_builder.Hashtbl(Datatype.String.Hashtbl)(Datatype.Int)
      (struct
        let name = "Property.Names.NamesTbl"
        let dependencies = [ ]
        let size = 97
      end)

  (* Computed name of properties *)
  module IndexTbl = (* indexed by Property *)
    State_builder.Hashtbl(Hashtbl)(Datatype.String)
      (struct
        let name = "Property.Names.IndexTbl"
        let dependencies = [ Ast.self; NamesTbl.self; Globals.Functions.self ]
        let size = 97
      end)

  let self = IndexTbl.self

  let compute_name_id basename =
    try
      let speed_up_start = NamesTbl.find basename in
      (* this basename is already reserved *)
      let n,unique_name = Extlib.make_unique_name NamesTbl.mem ~sep:"_" ~start:speed_up_start basename
      in NamesTbl.replace basename (succ n) ; (* to speed up Extlib.make_unique_name for next time *)
      unique_name
    with Not_found -> (* first time that basename is reserved *)
      NamesTbl.add basename 2 ;
      basename

  let get_prop_name_id ip =
    try IndexTbl.find ip
    with Not_found -> (* first time we are asking for a name for that [ip] *)
      let basename = get_prop_basename ip in
      let unique_name = compute_name_id basename in
      IndexTbl.add ip unique_name ;
      unique_name

end

(* -------------------------------------------------------------------------- *)
(* --- Smart Constructors                                                 --- *)
(* -------------------------------------------------------------------------- *)

let ip_other io_name io_loc = IPOther {io_name; io_loc}

let ip_reachable_stmt kf ki =
  IPReachable {ir_kf=Some kf; ir_kinstr=Kstmt ki; ir_program_point=Before}

let ip_reachable_ppt p =
  let ir_kf = get_kf p in
  let ir_kinstr = get_kinstr p in
  let ir_program_point = match p with
    | IPPredicate {ip_kind=(PKRequires _ | PKAssumes _ | PKTerminates)}
    | IPAxiom _ | IPAxiomatic _ | IPLemma _ | IPComplete _
    | IPDisjoint _ | IPCodeAnnot _ | IPAllocation _
    | IPDecrease _ | IPPropertyInstance _ | IPOther _
    | IPTypeInvariant _ | IPGlobalInvariant _
      -> Before
    | IPPredicate _ | IPAssigns _ | IPFrom _
    | IPExtended _
    | IPBehavior _
      -> After
    | IPReachable _ -> Kernel.fatal "IPReachable(IPReachable _) is not possible"
  in
  IPReachable {ir_kf; ir_kinstr; ir_program_point}

let ip_of_ensures ip_kf ip_kinstr b (k,ip_pred) =
  IPPredicate {ip_kind=PKEnsures (b, k); ip_kf; ip_kinstr; ip_pred}

let ip_of_extended ie_loc ie_ext = IPExtended {ie_loc; ie_ext}

let ip_ensures_of_behavior kf st b =
  List.map (ip_of_ensures kf st b) b.b_post_cond

let ip_of_allocation ial_kf ial_kinstr ial_bhv = function
  | FreeAllocAny -> None
  | FreeAlloc(f,a) ->
    Some (IPAllocation {ial_kf;ial_kinstr;ial_bhv; ial_allocs=(f,a)})

let ip_allocation_of_behavior kf st ~active b =
  let a = Datatype.String.Set.of_list active in
  ip_of_allocation kf st (Id_contract (a,b)) b.b_allocation

let ip_of_assigns ias_kf ias_kinstr ias_bhv = function
  | WritesAny -> None
  | Writes [(a,_)] when Logic_utils.is_result a.it_content ->
    (* We're only assigning the result (with dependencies), but no
       global variables, this amounts to \nothing.
    *)
    Some (IPAssigns {ias_kf; ias_kinstr; ias_bhv; ias_froms=[]})
  | Writes ias_froms ->
    Some (IPAssigns {ias_kf; ias_kinstr; ias_bhv; ias_froms})

let ip_assigns_of_behavior kf st ~active b =
  let a = Datatype.String.Set.of_list active in
  ip_of_assigns kf st (Id_contract (a,b)) b.b_assigns

let ip_of_from if_kf if_kinstr if_bhv if_from =
  match snd if_from with
  | FromAny -> None
  | From _ -> Some (IPFrom {if_kf;if_kinstr;if_bhv;if_from})

let ip_from_of_behavior kf st ~active b =
  match b.b_assigns with
  | WritesAny -> []
  | Writes l ->
    let treat_from acc (out, froms) = match froms with
      | FromAny -> acc
      | From _ ->
        let a = Datatype.String.Set.of_list active in
        let ip =
          Extlib.the (ip_of_from kf st (Id_contract (a,b)) (out, froms))
        in
        ip :: acc
    in
    List.fold_left treat_from [] l

let ip_allocation_of_code_annot kf st ca = match ca.annot_content with
  | AAllocation (_,a) -> ip_of_allocation kf st (Id_loop ca) a
  | _ -> None

let ip_assigns_of_code_annot kf st ca = match ca.annot_content with
  | AAssigns (_,a) -> ip_of_assigns kf st (Id_loop ca) a
  | _ -> None

let ip_from_of_code_annot kf st ca = match ca.annot_content with
  | AAssigns(_,WritesAny) -> []
  | AAssigns (_,Writes l) ->
    let treat_from acc (out, froms) = match froms with FromAny -> acc
                                                     | From _ ->
                                                       let ip =
                                                         Extlib.the (ip_of_from kf st (Id_loop ca) (out, froms))
                                                       in
                                                       ip::acc
    in
    List.fold_left treat_from [] l
  | _ -> []

let ip_post_cond_of_behavior kf st ~active b =
  ip_ensures_of_behavior kf st b
  @ (Extlib.list_of_opt (ip_assigns_of_behavior kf st ~active b))
  @ ip_from_of_behavior kf st active b
  @ (Extlib.list_of_opt (ip_allocation_of_behavior kf st ~active b))

let ip_of_behavior ib_kf ib_kinstr ~active ib_bhv =
  let ib_active = Datatype.String.Set.of_list active in
  IPBehavior {ib_kf; ib_kinstr; ib_active; ib_bhv}

let ip_of_requires ip_kf ip_kinstr b ip_pred =
  IPPredicate {ip_kind=PKRequires b; ip_kf; ip_kinstr; ip_pred}

let ip_requires_of_behavior kf st b =
  List.map (ip_of_requires kf st b) b.b_requires

let ip_of_assumes ip_kf ip_kinstr b ip_pred =
  IPPredicate {ip_kind=PKAssumes b; ip_kf; ip_kinstr; ip_pred}

let ip_assumes_of_behavior kf st b =
  List.map (ip_of_assumes kf st b) b.b_assumes

let ip_all_of_behavior kf st ~active b =
  ip_of_behavior kf st ~active b
  :: ip_requires_of_behavior kf st b
  @ ip_assumes_of_behavior kf st b
  @ ip_post_cond_of_behavior kf st ~active b
  @ List.map (ip_of_extended (e_loc_of_stmt kf st)) b.b_extended

let ip_of_complete ic_kf ic_kinstr ~active ic_bhvs =
  let ic_active = Datatype.String.Set.of_list active in
  IPComplete {ic_kf; ic_kinstr; ic_active; ic_bhvs}

let ip_complete_of_spec kf st ~active s =
  List.map (ip_of_complete kf st ~active) s.spec_complete_behaviors

let ip_of_disjoint ic_kf ic_kinstr ~active ic_bhvs =
  let ic_active = Datatype.String.Set.of_list active in
  IPDisjoint {ic_kf; ic_kinstr; ic_active; ic_bhvs}

let ip_disjoint_of_spec kf st ~active s =
  List.map (ip_of_disjoint kf st ~active) s.spec_disjoint_behaviors

let ip_of_terminates ip_kf ip_kinstr ip_pred =
  IPPredicate {ip_kind = PKTerminates; ip_kf; ip_kinstr; ip_pred}

let ip_terminates_of_spec kf st s = match s.spec_terminates with
  | None -> None
  | Some p -> Some (ip_of_terminates kf st p)

let ip_of_decreases id_kf id_kinstr id_variant =
  IPDecrease {id_kf; id_kinstr; id_ca = None; id_variant}

let ip_decreases_of_spec kf st s =
  Extlib.opt_map (ip_of_decreases kf st) s.spec_variant

let ip_post_cond_of_spec kf st ~active s =
  List.concat
    (List.map (ip_post_cond_of_behavior kf st ~active) s.spec_behavior)

let ip_of_spec kf st ~active s =
  List.concat (List.map (ip_all_of_behavior kf st ~active) s.spec_behavior)
  @ ip_complete_of_spec kf st active s
  @ ip_disjoint_of_spec kf st active s
  @ (Extlib.list_of_opt (ip_terminates_of_spec kf st s))
  @ (Extlib.list_of_opt (ip_decreases_of_spec kf st s))

let ip_axiom s = IPAxiom s
let ip_lemma s = IPLemma s
let ip_type_invariant s = IPTypeInvariant s
let ip_global_invariant s = IPGlobalInvariant s

let ip_property_instance ii_kf ii_stmt ii_pred ii_ip =
  IPPropertyInstance {ii_kf; ii_stmt; ii_pred; ii_ip}

let ip_of_code_annot kf stmt ca =
  let ki = Kstmt stmt in
  match ca.annot_content with
  | AAssert _ | AInvariant _ -> [ IPCodeAnnot {ica_kf=kf; ica_stmt=stmt; ica_ca=ca} ]
  | AStmtSpec (active,s) -> ip_of_spec kf ki active s
  | AVariant t ->
    [ IPDecrease {id_kf=kf;id_kinstr=ki;id_ca=Some ca; id_variant=t}]
  | AAllocation _ ->
    Extlib.list_of_opt (ip_allocation_of_code_annot kf ki ca)
    @ ip_from_of_code_annot kf ki ca
  | AAssigns _ ->
    Extlib.list_of_opt (ip_assigns_of_code_annot kf ki ca)
    @ ip_from_of_code_annot kf ki ca
  | APragma p when Logic_utils.is_property_pragma p ->
    [ IPCodeAnnot {ica_kf=kf; ica_stmt=stmt; ica_ca=ca} ]
  | APragma _ -> []
  | AExtended(_,_,ext) ->
    if ext.ext_has_status then
      [IPExtended {ie_loc=ELStmt(kf,stmt); ie_ext=ext}]
    else []

let ip_of_code_annot_single kf stmt ca = match ip_of_code_annot kf stmt ca with
  | [] ->
    (* [JS 2011/06/07] using Kernel.error here seems very strange.
       Actually it is incorrect in case of pragma which is not a property (see
       function ip_of_code_annot above. *)
    Kernel.error
      "@[cannot find a property to extract from code annotation@\n%a@]"
      Cil_printer.pp_code_annotation ca;
    raise (Invalid_argument "ip_of_code_annot_single")
  | [ ip ] -> ip
  | ip :: _ ->
    Kernel.warning
      "@[choosing one of multiple properties associated \
       to code annotation@\n%a@]"
      Cil_printer.pp_code_annotation ca;
    ip

(* Must ensure that the first property is the best one in order to represent
   the annotation (see ip_of_global_annotation_single) *)
let ip_of_global_annotation a =
  let once = true in
  let rec aux acc = function
    | Daxiomatic(iax_name, l, _, _) ->
      let iax_props = List.fold_left aux [] l in
      IPAxiomatic {iax_name; iax_props} :: (iax_props @ acc)
    | Dlemma(il_name, true, il_labels, il_args, il_pred, _, il_loc) ->
      ip_axiom {il_name; il_labels; il_args; il_pred; il_loc} :: acc
    | Dlemma(il_name, false, il_labels, il_args, il_pred, _, il_loc) ->
      ip_lemma {il_name; il_labels; il_args; il_pred; il_loc} :: acc
    | Dinvariant(l, igi_loc) ->
      let igi_pred = match l.l_body with
        | LBpred p -> p
        | _ -> assert false
      in
      IPGlobalInvariant {igi_name=l.l_var_info.lv_name; igi_pred; igi_loc} :: acc
    | Dtype_annot(l, iti_loc) ->
      let parameter = match l.l_profile with
        | h :: [] -> h
        | _ -> assert false
      in
      let iti_type = match parameter.lv_type with
        | Ctype x -> x
        | _ -> assert false
      in
      let iti_pred = match l.l_body with
        | LBpred p -> p
        | _ -> assert false
      in
      IPTypeInvariant {iti_name=l.l_var_info.lv_name;
                       iti_type; iti_pred; iti_loc} :: acc
    | Dcustom_annot(_c, _n, _, _) ->
      (* TODO *)
      Kernel.warning ~once "ignoring status of custom annotation";
      acc
    | Dmodel_annot _ | Dfun_or_pred _ | Dvolatile _ | Dtype _ ->
      (* no associated status for these annotations *)
      acc
    | Dextended(ie_ext,_,_) ->
      IPExtended {ie_loc = ELGlob; ie_ext} :: acc
  in
  aux [] a

let ip_of_global_annotation_single a = match ip_of_global_annotation a with
  | [] -> None
  | ip :: _ ->
    (* the first one is the good one, see ip_of_global_annotation *)
    Some ip

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
