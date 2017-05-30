(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Eval

module type InputDomain = sig
  include Abstract_domain.Lattice
  include Datatype.S with type t = state
  val storage: unit -> bool
end

module Make (Domain: InputDomain) = struct

  let name = Domain.name ^ "_Store"

  module Storage =
    State_builder.Ref (Datatype.Bool)
      (struct
        let dependencies = [Db.Value.self]
        let name = name ^ ".Storage"
        let default () = false
      end)

  (* Do NOT add dependencies to Kernel parameters here, but at the top of
     Value/Value_parameters *)
  let dependencies =
    [ Ast.self;
      Alarms.self;
      Annotations.code_annot_state ]

  let size = 1789

  module States_by_callstack =
    Value_types.Callstack.Hashtbl.Make (Domain)

  module Table_By_Callstack =
    Cil_state_builder.Stmt_hashtbl(States_by_callstack)
      (struct
        let name = Domain.name ^ " results by callstack"
        let size = size
        let dependencies = dependencies
      end)
  module Table =
    Cil_state_builder.Stmt_hashtbl (Domain)
      (struct
        let name = Domain.name ^ " results"
        let size = size
        let dependencies = [ Table_By_Callstack.self ]
      end)
  (* Clear Value's various caches each time [Db.Value.is_computed] is updated,
     including when it is set, reset, or during project change. Some operations
     of Value depend on -ilevel, -plevel, etc, so clearing those caches when
     Value ends ensures that those options will have an effect between two runs
     of Value. *)
  let () = Table_By_Callstack.add_hook_on_update
      (fun _ ->
         Cvalue.V_Offsetmap.clear_caches ();
         Cvalue.Model.clear_caches ();
         Locations.Location_Bytes.clear_caches ();
         Locations.Zone.clear_caches ();
         Function_Froms.Memory.clear_caches ();
      )

  module AfterTable_By_Callstack =
    Cil_state_builder.Stmt_hashtbl (States_by_callstack)
      (struct
        let name = Domain.name ^ " results after states by callstack"
        let size = size
        let dependencies = dependencies
      end)


  let self = Table_By_Callstack.self
  let only_self = [ self ]


  module Called_Functions_By_Callstack =
    State_builder.Hashtbl
      (Kernel_function.Hashtbl)
      (States_by_callstack)
      (struct
        let name = name ^ ".called_functions_by_callstack"
        let size = 11
        let dependencies = only_self
      end)

  module Called_Functions_Memo =
    State_builder.Hashtbl
      (Kernel_function.Hashtbl)
      (Domain)
      (struct
        let name = name ^ ".called_functions_memo"
        let size = 11
        let dependencies = [ Called_Functions_By_Callstack.self ]
      end)

  let update_callstack_table ~after stmt callstack v =
    let open Value_types in
    let find,add =
      if after
      then AfterTable_By_Callstack.find, AfterTable_By_Callstack.add
      else Table_By_Callstack.find, Table_By_Callstack.add
    in
    try
      let by_callstack = find stmt in
      begin try
          let o = Callstack.Hashtbl.find by_callstack callstack in
          Callstack.Hashtbl.replace by_callstack callstack (Domain.join o v)
        with Not_found ->
          Callstack.Hashtbl.add by_callstack callstack v
      end;
    with Not_found ->
      let r = Callstack.Hashtbl.create 7 in
      Callstack.Hashtbl.add r callstack v;
      add stmt r

  let register_initial_state callstack state =
    let storage = match callstack with
      | [_, Cil_types.Kglobal] -> let s = Domain.storage () in Storage.set s; s
      | _ -> Storage.get ()
    in
    if storage then
      let open Value_types in
      let kf = match callstack with (kf, _) :: _ -> kf | _ -> assert false in
      let by_callstack =
        try Called_Functions_By_Callstack.find kf
        with Not_found ->
          let h = Callstack.Hashtbl.create 7 in
          Called_Functions_By_Callstack.add kf h;
          h
      in
      try
        let old = Callstack.Hashtbl.find by_callstack callstack in
        Callstack.Hashtbl.replace by_callstack callstack (Domain.join old state)
      with Not_found ->
        Callstack.Hashtbl.add by_callstack callstack state

  let get_initial_state kf =
    if not (Storage.get ())
    then `Value Domain.top
    else
      try `Value (Called_Functions_Memo.find kf)
      with Not_found ->
      try
        let by_callstack = Called_Functions_By_Callstack.find kf in
        let state =
          Value_types.Callstack.Hashtbl.fold
            (fun _cs state acc -> Bottom.join Domain.join acc (`Value state))
            by_callstack `Bottom
        in
        ignore (state >>-: Called_Functions_Memo.add kf);
        state
      with Not_found -> `Bottom

  let get_initial_state_by_callstack kf =
    try Some (Called_Functions_By_Callstack.find kf)
    with Not_found -> None

  let get_stmt_state s =
    if not (Storage.get ())
    then `Value Domain.top
    else
      try `Value (Table.find s)
      with Not_found ->
        let ho = try Some (Table_By_Callstack.find s) with Not_found -> None in
        let state =
          match ho with
          | None -> `Bottom
          | Some h ->
            Value_types.Callstack.Hashtbl.fold
              (fun _cs state acc -> Bottom.join Domain.join acc (`Value state))
              h `Bottom
        in
        ignore (state >>-: Table.add s);
        state

  let get_stmt_state_by_callstack ~after stmt =
    try
      Some (if after then AfterTable_By_Callstack.find stmt
            else Table_By_Callstack.find stmt)
    with Not_found -> None


  let register_state_before_stmt callstack stmt state =
    if Storage.get ()
    then update_callstack_table ~after:false stmt callstack state

  let register_state_after_stmt callstack stmt state =
    if Storage.get ()
    then update_callstack_table ~after:true stmt callstack state

end
