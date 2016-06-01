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

let category = File.register_code_transformation_category "variadic"

let () = 
  Cmdline.run_after_configuring_stage
    begin fun () ->
      if Options.Enabled.get () then
        File.add_code_transformation_before_cleanup
          category Translate.translate_variadics
    end

