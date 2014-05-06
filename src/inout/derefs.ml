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
open Cil
open Locations


class virtual do_it_ = object(self)
  inherit [Zone.t] Cumulative_analysis.cumulative_visitor
  val mutable derefs = Zone.bottom

  method bottom = Zone.bottom

  method result = derefs

  method join new_ =
    derefs <- Zone.join new_ derefs;

  method! vlval (base,_ as lv) =
    begin match base with
      | Var _ -> ()
      | Mem e ->
          let state =
            Db.Value.get_state (Kstmt (Extlib.the self#current_stmt))
          in
          let r = 
	    !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state e 
	  in
          let loc = loc_bytes_to_loc_bits r in
          let size = Bit_utils.sizeof_lval lv in
          self#join
            (enumerate_valid_bits ~for_writing:false (make_loc loc size))
    end;
    DoChildren

  method compute_funspec (_: kernel_function) =
    Zone.bottom

  method clean_kf_result (_ : kernel_function) (r: Locations.Zone.t) = r

end

module Analysis = Cumulative_analysis.Make(
  struct
    let analysis_name ="derefs"

    type t = Locations.Zone.t
    module T = Locations.Zone

    class virtual do_it = do_it_
end)

let get_internal = Analysis.kernel_function

let externalize _return fundec x =
  Zone.filter_base
    (fun v -> not (Base.is_formal_or_local v fundec))
    x

module Externals =
  Kernel_function.Make_Table(Locations.Zone)
    (struct
       let name = "External derefs"
       let dependencies = [ Analysis.Memo.self ]
       let size = 17
     end)

let get_external =
  Externals.memo
    (fun kf ->
       !Db.Value.compute ();
       if Kernel_function.is_definition kf then
         try
           externalize
             (Kernel_function.find_return kf)
             (Kernel_function.get_definition kf)
             (get_internal kf)
         with Kernel_function.No_Statement ->
           assert false
       else
         (* assume there is no deref for leaf functions *)
         Zone.bottom)

let compute_external kf = ignore (get_external kf)

let _pretty_internal fmt kf =
  Format.fprintf fmt "@[Derefs (internal) for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Zone.pretty (get_internal kf)

let pretty_external fmt kf =
  Format.fprintf fmt "@[Derefs for function %a:@\n@[<hov 2>  %a@]@]@\n"
    Kernel_function.pretty kf
    Zone.pretty (get_external kf)

let () =
  Db.Derefs.self_internal := Analysis.Memo.self;
  Db.Derefs.self_external := Externals.self;
  Db.Derefs.get_internal := get_internal;
  Db.Derefs.get_external := get_external;
  Db.Derefs.compute := compute_external;
  Db.Derefs.display := pretty_external;
  Db.Derefs.statement := Analysis.statement
