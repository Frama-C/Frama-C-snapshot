(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: analysis.mli,v 1.10 2008/04/01 09:25:22 uid568 Exp $ *)

module Make(S : Lattice.S) : sig

  val init: unit -> unit
  
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
