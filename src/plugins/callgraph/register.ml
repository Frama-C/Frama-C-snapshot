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

let main () =
  if Options.Filename.get () <> "" then
    if Options.Services.get () then begin
      if not (Services.is_computed ()) then Services.dump ()
    end else
      if not (Cg.is_computed ()) then Cg.dump ()

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
