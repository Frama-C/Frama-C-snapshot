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

open Db

(* Update the associated value [new_v] to an lvalue [lv] in [mem] *)
let update_from looking_for new_v mem =
  let exact =
    Locations.valid_cardinal_zero_or_one ~for_writing:true looking_for
  in
  (*Format.printf "Update is exact : %b %a\n" exact
    Locations.pretty looking_for;*)
  (Lmap_bitwise.From_Model.add_binding
     exact
     mem
     (Locations.valid_enumerate_bits ~for_writing:true looking_for)
     new_v)

let access_from looking_for mem =
  let r = Lmap_bitwise.From_Model.find
    mem
    looking_for
  in
  r

let access_value_of_lval kinstr lv =
  let state = Value.get_state kinstr in
  snd (!Value.eval_lval  ~with_alarms:CilE.warn_none_mode None state lv)

let access_value_of_expr kinstr e =
  let state = Value.get_state kinstr in
  !Value.eval_expr ~with_alarms:CilE.warn_none_mode state e

let access_value_of_location kinstr loc =
  let state = Value.get_state kinstr in
  Value.find state loc

let access_value_of_lval_after ki lv =
  match ki with
  | Cil_types.Kstmt {Cil_types.succs = (_::_ ) as l} ->
      let result =
        List.fold_left
          (fun acc s ->
             let ks = Cil_types.Kstmt s in
             Cvalue.V.join (access_value_of_lval ks lv) acc)
          Cvalue.V.bottom
          l
      in
      begin match Bit_utils.sizeof_lval lv with
      | Int_Base.Bottom -> assert false
      | Int_Base.Top -> result
      | Int_Base.Value size ->
          Cvalue.V.anisotropic_cast ~size result
      end
  | _ -> raise Not_found

let access_offsetmap_of_lval_after ki lv =
  match ki with
  | Cil_types.Kstmt {Cil_types.succs = (_::_ ) as l} ->
      let result =
        List.fold_left
          (fun acc s ->
             let ks = Cil_types.Kstmt s in
             let state = Db.Value.get_state ks in
             let loc =
               Locations.valid_part ~for_writing:false
               (!Db.Value.lval_to_loc_state state lv)
             in
             let offsetmap =
(*             try *)
                 Cvalue.Model.copy_offsetmap
                   ~with_alarms:CilE.warn_none_mode loc state
(*             with Lmap.Cannot_copy ->
                 let _,exp = !Value.eval_lval ~with_alarms:CilE.warn_none_mode None state lv in
                 if Cvalue.V.is_bottom exp
                 then None
                 else
                   Some (Cvalue.V_Offsetmap.update_ival
                            ~with_alarms:CilE.warn_none_mode
                            ~validity:Base.All
                            ~offsets:Ival.zero
                            ~exact:true
                            ~size:(Abstract_interp.Int.of_int (Cil.bitsSizeOf (Cil.typeOfLval lv)))
                            Cvalue.V_Offsetmap.empty
                            (Cvalue.V_Or_Uninitialized.initialized exp))
*)
             in
             match acc, offsetmap with
             | None, x | x , None -> x
             | Some acc, Some offsetmap ->
                 Some (snd (Cvalue.V_Offsetmap.join acc offsetmap)))
          None
          l
      in
      result
  | _ -> raise Not_found

let access_value_of_location_after ki loc =
  match ki with
    | Cil_types.Kstmt {Cil_types.succs=(_::_ ) as l} ->
        List.fold_left
          (fun acc s ->
             let ks = Cil_types.Kstmt s in
             Cvalue.V.join (access_value_of_location ks loc) acc)
          Cvalue.V.bottom
          l
    | _ -> raise Not_found

(* Register functions in the kernel *)
let () =
  From.update := update_from;
  From.access := access_from;
  Value.access := access_value_of_lval;
  Value.access_after := access_value_of_lval_after;
  Value.access_location_after := access_value_of_location_after;
  Value.access_location := access_value_of_location;
  Value.access_expr := access_value_of_expr;
  Value.lval_to_offsetmap_after := access_offsetmap_of_lval_after

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
