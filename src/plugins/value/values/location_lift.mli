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

module type Conversion = sig
  type extended_value
  type internal_value

  val extend_val : internal_value -> extended_value
  val restrict_val : extended_value -> internal_value
end

module Make
    (Loc: Abstract_location.Internal)
    (Convert : Conversion with type internal_value := Loc.value)
  : Abstract_location.Internal with type location = Loc.location
                                and type offset = Loc.offset
                                and type value = Convert.extended_value


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
