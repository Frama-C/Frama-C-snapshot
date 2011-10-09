(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(* --- Proof Environment to deal with Arrays and Record in Memory Models  --- *)
(* -------------------------------------------------------------------------- *)

open Formula
open Cil_types
open Ctypes

module Create(M : Mvalues.Data) =
struct
  module F = M.F

  (* Declaration of record format *)

  module Cformat = F.DRegister
    (struct
       include F.Compinfo
       let prefix = "Cfmt"
       let section = S_Cons
       let clear () = ()
       let pp_title fmt c =
         Format.fprintf fmt "Format for %s '%s'"
           (if c.cstruct then "struct" else "union") c.cname
       let declare comp _ =
         Function ([],Formula.ADT("format",[Record comp]))
     end)

  (* Declaration of array format *)

  module Aformat = F.DRegister
    (struct
       include F.Arrayinfo
       let prefix = "Afmt"
       let section = S_Cons
       let clear () = ()
       let pp_title fmt arr =
         Format.fprintf fmt "Format for array %a"
           Ctypes.pretty (C_array arr)
       let pp_descr fmt _ =
         Format.fprintf fmt "Decode array from memory"
       let declare arr _ =
         Function ([],Formula.ADT("format",[Array arr]))
     end)

  (* ------------------------------------------------------------------------ *)
  (* --- Utilities                                                        --- *)
  (* ------------------------------------------------------------------------ *)

  let in_range arr xi =
    let i_pos = F.p_icmp Cleq F.i_zero xi in
    let i_max = match arr.arr_flat with
      | None -> F.p_true
      | Some a -> F.p_icmp Clt xi (F.e_icst (Int64.to_string a.arr_size))
    in F.p_and i_pos i_max

  module Compound(R : sig val prefix : string end) :
    sig
      val add_axiom : string -> Lexing.position option
        -> string -> string -> F.pred -> unit
      val on_array : (arrayinfo -> unit) -> unit
      val on_record : (compinfo -> unit) -> unit
      val define : c_object -> unit
    end
    =
  struct

    module Hcomp = F.Compinfo.H
    module Harray = F.Arrayinfo.H

    let cindex = Hcomp.create 131
    let aindex = Harray.create 131
    let clear () = Hcomp.clear cindex ; Harray.clear aindex
    let () = Fol_decl.register_prefix R.prefix
    let () = F.on_clear clear

    let add_axiom obj src name path prop =
      let dname = F.fresh_name R.prefix name in
      F.add_declaration {
        d_name = dname ;
        d_section = S_Model_Prop ;
        d_source = src ;
        d_item = Formula.Axiom prop ;
        d_title = (fun fmt -> Format.fprintf fmt "%s in loaded object %s" R.prefix obj) ;
        d_descr = (fun fmt -> Format.fprintf fmt "Path %s" path) ;
      }

    let define_array = ref (fun _ -> assert false)
    let define_comp = ref (fun _ -> assert false)

    let on_array f = define_array := f
    let on_record f = define_comp := f

    let define = function
      | C_array arr ->
          let key = F.Arrayinfo.index arr in
          if not (Harray.mem aindex key) then
            ( Harray.add aindex key () ; !define_array arr )

      | C_comp comp ->
          let key = F.Compinfo.index comp in
          if not (Hcomp.mem cindex key) then
            ( Hcomp.add cindex key () ; !define_comp comp )
      | _ -> ()

  end

  (* ------------------------------------------------------------------------ *)
  (* --- Axioms for accessing sub-terms in loaded compounds               --- *)
  (* ------------------------------------------------------------------------ *)

  module LoadedCompound =
  struct

    include Compound(struct let prefix = "Loaded" end)

    let rec define_access obj pool name path xs mem loc tr =
      match tr with

        | C_array arr ->
            let te   = Ctypes.object_of arr.arr_element in
            let xi   = F.p_fresh pool "i" (Model Formula.Integer) in
            let loc' = M.index loc te (F.var xi) in
            let r    = M.logic_of_value (M.load_mem mem tr loc) in
            let v    = F.acc_index (F.unwrap r) (F.var xi) in
            let v'   = M.logic_of_value (M.load_mem mem te loc') in
            let xs'  = xs @ [xi] in
            let prop =
              F.p_forall xs'
                (F.p_implies (in_range arr (F.var xi)) (M.equal te v v'))
            in
            let name' = Pretty_utils.sfprintf "%s_idx" name in
            let path' = Pretty_utils.sfprintf "%s[%a]" path F.pp_term (F.var xi) in
            add_axiom obj (F.Arrayinfo.location arr) name' path' prop ;
            define_access obj pool name' path' xs' mem loc' te

        | C_comp comp ->
            List.iter
              (fun f ->
                 let te   = Ctypes.object_of f.ftype in
                 let loc' = M.field loc f in
                 let r    = M.logic_of_value (M.load_mem mem tr loc) in
                 let v    = F.acc_field (F.unwrap r) f in
                 let v'   = M.logic_of_value (M.load_mem mem te loc') in
                 let prop = F.p_forall xs (M.equal te v v') in
                 let name' = Format.sprintf "%s_%s" name f.fname in
                 let path' = Format.sprintf "%s.%s" name f.fname in
                 add_axiom obj (F.Compinfo.location comp) name' path' prop ;
                 define_access obj pool name' path' xs mem loc' te
              ) comp.cfields

        | _ -> ()

    let define_array arr =
      let pool = F.pool () in
      let name = F.Arrayinfo.basename arr in
      let path = "array" in
      let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
      let xa,a = M.forall_loc pool in
      define_access name pool name path (xm::xa) (F.var xm) a (C_array arr)

    let define_comp comp =
      let pool = F.pool () in
      let name = F.Compinfo.basename comp in
      let path = comp.cname in
      let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
      let xa,a = M.forall_loc pool in
      define_access name pool name path (xm::xa) (F.var xm) a (C_comp comp)

    let () = on_array define_array
    let () = on_record define_comp

  end

  (* -------------------------------------------------------------------------- *)
  (* --- Axioms for accessing sub-terms in stored compounds                 --- *)
  (* -------------------------------------------------------------------------- *)

  module StoredCompound =
  struct

    include Compound(struct let prefix = "Stored" end)

    let rec define_update obj pool name path xs mem loc tr r =
      match tr with

        | C_array arr ->
            let xi = F.p_fresh pool "i" (Model Formula.Integer) in
            let te = Ctypes.object_of arr.arr_element in
            let mem' = M.store_mem mem tr loc (M.value_of_logic tr r) in
            let loc' = M.index loc te (F.var xi) in
            let ve   = F.acc_index (F.unwrap r) (F.var xi) in
            let v'   = M.logic_of_value (M.load_mem mem' te loc') in
            let xs'  = xs @ [xi] in
            let prop = F.p_forall xs'
              (F.p_implies (in_range arr (F.var xi)) (M.equal te ve v')) in
            let name' = Pretty_utils.sfprintf "%s_idx" name in
            let path' = Pretty_utils.sfprintf "%s[%a]" path F.pp_var xi in
            add_axiom obj (F.Arrayinfo.location arr) name' path' prop ;
            define_update obj pool name' path' xs' mem' loc' te ve

        | C_comp comp ->
            List.iter
              (fun f ->
                 let te   = Ctypes.object_of f.ftype in
                 let mem' = M.store_mem mem tr loc (M.value_of_logic tr r) in
                 let loc' = M.field loc f in
                 let ve   = F.acc_field (F.unwrap r) f in
                 let v'   = M.logic_of_value (M.load_mem mem' te loc') in
                 let prop = F.p_forall xs (M.equal te ve v') in
                 let name' = Format.sprintf "%s_%s" name f.fname in
                 let path' = Format.sprintf "%s.%s" name f.fname in
                 add_axiom obj (F.Compinfo.location comp) name' path' prop ;
                 define_update obj pool name' path' xs mem loc' te ve
              ) comp.cfields

        | _ -> ()


    let define_array arr =
      let pool = F.pool () in
      let name = F.Arrayinfo.basename arr in
      let path = "array" in
      let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
      let xr = F.p_fresh pool "r" (Model (Array arr)) in
      let xa,a = M.forall_loc pool in
      define_update name pool name path
        (xm::xr::xa) (F.var xm) a (C_array arr) (F.var xr)

    let define_comp comp =
      let pool = F.pool () in
      let name = F.Compinfo.basename comp in
      let path = comp.cname in
      let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
      let xr = F.p_fresh pool "r" (Model (Record comp)) in
      let xa,a = M.forall_loc pool in
      define_update name pool name path
        (xm::xr::xa) (F.var xm) a (C_comp comp) (F.var xr)

    let () = on_array define_array
    let () = on_record define_comp

  end

  (* -------------------------------------------------------------------------- *)
  (* --- Axioms for loading an object after a store inside                  --- *)
  (* -------------------------------------------------------------------------- *)

  module UpdatedCompound =
  struct

    include Compound(struct let prefix = "Updated" end)

    let define_array arr =
      let tr = C_array arr in
      let te = Ctypes.object_of arr.arr_element in
      let pool = F.pool () in
      let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
      let xa,loc = M.forall_loc pool in
      let xi = F.p_fresh pool "i" (Model Integer) in
      let xv = F.p_fresh pool "e" (Model (M.tau_of_object te)) in
      let mem = F.var xm in
      let loc' = M.index loc te (F.var xi) in
      let mem' = M.store_mem mem te loc' (M.value_of_logic te (F.var xv)) in
      let r0 = M.logic_of_value (M.load_mem mem tr loc) in
      let r1 = M.logic_of_value (M.load_mem mem' tr loc) in
      let r2 = F.wrap (F.upd_index (F.unwrap r0) (F.var xi) (F.var xv)) in
      let prop = F.p_forall (xm::xi::xv::xa)
        (F.p_implies (in_range arr (F.var xi)) (M.equal tr r1 r2)) in
      let name = F.Arrayinfo.basename arr in
      let path = Pretty_utils.sfprintf "%s.[%a]" name F.pp_var xi in
      add_axiom name (F.Arrayinfo.location arr) name path prop

    let define_comp comp =
      List.iter
        (fun f ->
           let tr = C_comp comp in
           let te = Ctypes.object_of f.ftype in
           let pool = F.pool () in
           let xm = F.p_fresh pool "m" (Model M.tau_of_mem) in
           let xa,loc = M.forall_loc pool in
           let xv = F.p_fresh pool "e" (Model (M.tau_of_object te)) in
           let mem = F.var xm in
           let loc' = M.field loc f in
           let mem' = M.store_mem mem te loc' (M.value_of_logic te (F.var xv)) in
           let r0 = M.logic_of_value (M.load_mem mem tr loc) in
           let r1 = M.logic_of_value (M.load_mem mem' tr loc) in
           let r2 = F.wrap (F.upd_field (F.unwrap r0) f (F.var xv)) in
           let prop = F.p_forall (xm::xv::xa) (M.equal tr r1 r2) in
           let name = F.Compinfo.basename comp in
           let path = Pretty_utils.sfprintf "%s.%s" name f.fname in
           add_axiom name (F.Compinfo.location comp) name path prop
        ) comp.cfields

    let () = on_array define_array
    let () = on_record define_comp

  end

  (* -------------------------------------------------------------------------- *)
  (* --- API                                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let record_format comp = F.e_app0 (Cformat.get_definition comp).d_name
  let array_format arr = F.e_app0 (Aformat.get_definition arr).d_name

  (* -------------------------------------------------------------------------- *)
  (* --- Registration                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let loaded te = LoadedCompound.define te ; UpdatedCompound.define te
  let stored te = StoredCompound.define te

end
