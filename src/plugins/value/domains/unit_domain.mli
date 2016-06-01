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

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
  : Abstract_domain.Internal with type state = unit
                              and type value = Value.t
                              and type location = Loc.location


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
