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
    (Left:  Abstract_domain.Internal with type value = Value.t)
    (Right: Abstract_domain.Internal with type value = Left.value
                                      and type location = Left.location)
  : Abstract_domain.Internal with type value = Value.t
                              and type location = Left.location
                              and type state = Left.state * Right.state


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
