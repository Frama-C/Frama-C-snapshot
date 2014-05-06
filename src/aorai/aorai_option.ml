(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

include Plugin.Register
  (struct
     let name = "aorai"
     let shortname = "aorai"
     let help = "verification of behavioral properties (experimental)"
   end)

module Ltl_File =
  EmptyString
    (struct
       let option_name = "-aorai-ltl"
       let arg_name = ""
       let help = "specifies file name for LTL property"
     end)

module To_Buchi =
  EmptyString
    (struct
       let option_name = "-aorai-to-buchi"
       let arg_name = "f"
       let help =
         "only generates the buchi automata (in Promela language) in file <s>"
     end)

module Buchi =
  EmptyString
    (struct
       let option_name = "-aorai-buchi"
       let arg_name = "f"
       let help = "considers the property described by the buchi automata \
                   (in Promela language) from file <f>."
     end)

module Ya =
  EmptyString
    (struct
       let option_name = "-aorai-automata"
       let arg_name = "f"
       let help = "considers the property described by the ya automata \
                   (in Ya language) from file <f>."
     end)


module Output_Spec =
  False(struct
          let option_name = "-aorai-show-op-spec"
          let help =
            "displays computed pre and post-condition of each operation"
        end)

module Output_C_File =
  EmptyString
    (struct
       let option_name = "-aorai-output-c-file"
       let arg_name = ""
       let help = "specifies generated file name for annotated C code"
     end)

module Dot =
  False(struct
          let option_name = "-aorai-dot"
          let help = "generates a dot file of the Buchi automata"
        end)

module DotSeparatedLabels =
  False(struct
          let option_name = "-aorai-dot-sep-labels"
          let help = "tells dot to not output guards directly over the edges"
        end)

module AbstractInterpretation =
  False(struct
          let option_name = "-aorai-simple-AI"
          let help = "use simple abstract interpretation"
        end)

module AbstractInterpretationOff  =
  False(struct
          let option_name = "-aorai-AI-off"
          let help = "does not use abstract interpretation"
        end)

let () = Parameter_customize.set_negative_option_name "-aorai-spec-off"
module Axiomatization =
  True(struct
         let option_name = "-aorai-spec-on"
         let help = "if set, does not axiomatize automata"
       end)

module ConsiderAcceptance =
  False(struct
         let option_name = "-aorai-acceptance"
         let help = "if set, considers acceptation states"
       end)

let () = Parameter_customize.set_negative_option_name "-aorai-raw-auto"
module AutomataSimplification=
  True
    (struct
       let option_name = "-aorai-simplified-auto"
       let help = "If set, does not simplify automata"
     end)

module Test =
  Zero(struct
         let option_name = "-aorai-test"
         let arg_name = ""
         let help = "Testing mode (0 = no test)"
       end)

module AddingOperationNameAndStatusInSpecification =
  False
    (struct
      let option_name = "-aorai-add-oper"
      let help = "Adding current operation name (and statut) in pre/post \
conditions"
     end)

module Deterministic=
  State_builder.Ref
    (Datatype.Bool)
    (struct
        let name = "Aorai_option.Deterministic"
        let dependencies = []
        let default () = false
     end)

let is_on () =
  not (Ltl_File.is_default () && To_Buchi.is_default () &&
       Buchi.is_default ()    && Ya.is_default () )

(* [JS 2009/10/04]
   Preserve the behaviour of svn release <= r5012.
   However it works only if aorai is run from the command line. *)
let init () =
  if is_on () then begin
    Kernel.SimplifyCfg.on ();
    Kernel.KeepSwitch.on ()
  end

let () = Cmdline.run_after_configuring_stage init

let promela_file () =
  if Buchi.get () = "" then To_Buchi.get () else Buchi.get ()

let advance_abstract_interpretation () =
  not (AbstractInterpretationOff.get ()) && not (AbstractInterpretation.get ())

let emitter = 
  Emitter.create
    "Aorai"
    [ Emitter.Code_annot; Emitter.Funspec; Emitter.Global_annot ] 
    ~correctness:
    [ Ltl_File.parameter; To_Buchi.parameter; Buchi.parameter;
      Ya.parameter; Axiomatization.parameter; ConsiderAcceptance.parameter;
      AutomataSimplification.parameter ]
    ~tuning:
    [ AbstractInterpretation.parameter;
      AddingOperationNameAndStatusInSpecification.parameter ]

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
