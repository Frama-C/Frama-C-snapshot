(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Eval

module type Conversion = sig
  type extended_value
  type internal_value

  val extend_val : internal_value -> extended_value
  val restrict_val : extended_value -> internal_value
end

module Make
    (Loc: Abstract_location.Internal)
    (Convert : Conversion with type internal_value := Loc.value)
= struct

  (* Import most of [Loc] *)
  include (Loc: Abstract_location.Internal
           with type value := Loc.value (* we are converting this type *)
            and type location = Loc.location
            and type offset = Loc.offset)
  type value = Convert.extended_value

  (* Now lift the functions that contain {!value} in their type. *)

  let to_value loc = Convert.extend_val (Loc.to_value loc)

  let forward_index typ value offset =
    Loc.forward_index typ (Convert.restrict_val value) offset

  let reduce_index_by_array_size ~size_expr ~index_expr size value =
    let v = Convert.restrict_val value in
    Loc.reduce_index_by_array_size ~size_expr ~index_expr size v >>=: fun v ->
    Convert.extend_val v

  let forward_pointer typ value offset =
    Loc.forward_pointer typ (Convert.restrict_val value) offset

  let backward_pointer value offset loc =
    let v = Convert.restrict_val value in
    Loc.backward_pointer v offset loc >>-: fun (value, off) ->
    Convert.extend_val value, off

  let backward_index typ ~index ~remaining offset =
    let index = Convert.restrict_val index in
    Loc.backward_index typ ~index ~remaining offset >>-: fun (value, off) ->
    Convert.extend_val value, off


end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
