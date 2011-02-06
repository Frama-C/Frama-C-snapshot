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
open Cil
open Cilutil
open Cil_datatype

module Ki = Cil_datatype.Kinstr (* do not mask kinstr in src/value *)

 type record =
      {
	mutable superposition : State_imp.t ;
	mutable widening : int ;
	mutable widening_state : Relations_type.Model.t ;
      }
 
  let empty_record () =
    { superposition = State_imp.empty () ;
      widening = Value_parameters.WideningLevel.get () ;
      widening_state = Relations_type.Model.bottom }

  type t = record Ki.Hashtbl.t

  let create () = 
    Ki.Hashtbl.create 257

  let clear t = Ki.Hashtbl.clear t

  let find_current current_table kinstr =
    try
      Ki.Hashtbl.find current_table kinstr
    with Not_found ->
      let record = empty_record () in
      Ki.Hashtbl.add current_table kinstr record;
      record

  let find_widening_info current_table kinstr =
    let r = find_current current_table kinstr in
    r.widening, r.widening_state

  let update_current_exn current_table kinstr v =
    assert (kinstr <> Kglobal);
    let record = find_current current_table kinstr in
    State_imp.merge_set_into v record.superposition 


  let update_current current_table kinstr v =
    try
      update_current_exn current_table kinstr v
    with State_imp.Unchanged -> ()


  let update_and_tell_if_changed current_table kinstr d =
    let record = find_current current_table kinstr in
    State_imp.merge_set_return_new d record.superposition 


   let update_widening_info current_table kinstr wcounter wstate =
     let record = find_current current_table kinstr in
     record.widening <- wcounter;
     record.widening_state <- wstate;
     record.superposition <- State_imp.singleton wstate

  let merge_db_table hash_states =
   let treat_instr k sum =
      let current_state = Db.Value.noassert_get_state k in
      let is_top_already =
	Relations_type.Model.is_top current_state
      in
      if not is_top_already
      then Db.Value.update_table k sum
   in
   if Mark_noresults.should_memorize_function
     (Kernel_function.get_definition (Value_util.current_kf()))
   then
     Ki.Hashtbl.iter treat_instr (Lazy.force hash_states)

   let superpositions current_table =
     let r = Ki.Hashtbl.create (Ki.Hashtbl.length current_table)
     in
     Ki.Hashtbl.iter
       (fun k record ->
	 let sup2 = 
	   State_imp.fold 
	     State_set.add
	      record.superposition
	     State_set.empty
	 in
	 Ki.Hashtbl.add r k sup2)
       current_table;
     r

   let states current_table =
     let r = Ki.Hashtbl.create (Ki.Hashtbl.length current_table)
     in
     Ki.Hashtbl.iter
       (fun k record ->
	 Ki.Hashtbl.add r k
	   (State_imp.join_dropping_relations record.superposition))
       current_table;
     r


   let find_superposition current_table s =
     (find_current current_table s).superposition
