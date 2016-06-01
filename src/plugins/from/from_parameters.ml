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

include Plugin.Register
  (struct
     let name = "from analysis"
     let shortname = "from"
     let help = "functional dependencies"
   end)

module ForceDeps =
  WithOutput
    (struct
       let option_name = "-deps"
       let help = "force dependencies display"
       let output_by_default = true
     end)

module ForceCallDeps =
  WithOutput
    (struct
       let option_name = "-calldeps"
       let help = "force callsite-wise dependencies"
       let output_by_default = true
     end)

module ShowIndirectDeps =
False
    (struct
       let option_name = "-show-indirect-deps"
       let help = "experimental"
     end)

module VerifyAssigns =
False
    (struct
       let option_name = "-from-verify-assigns"
       let help = "verification of assigns/from clauses for functions with \
         bodies. Implies -calldeps"
     end)
let () =
  VerifyAssigns.add_set_hook
    (fun _ new_ ->
      if new_ then ForceCallDeps.set true)

module PathDeps =
False
    (struct
       let option_name = "-experimental-path-deps"
       let help = "experimental"
     end)

module MemDeps =
False
    (struct
       let option_name = "-experimental-mem-deps"
       let help = "experimental"
     end)



(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
