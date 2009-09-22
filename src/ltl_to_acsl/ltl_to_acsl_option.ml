(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(* $Id: ltl_to_acsl_option.ml,v 1.3 2009-03-11 12:40:58 uid588 Exp $ *)

include Plugin.Register
  (struct
     let name = "ltl to acsl"
     let shortname = "ltl"
     let descr = "verification of LTL properties (experimental)"
   end)

module Ltl_File =
  EmptyString
    (struct
       let option_name = "-ltl"
       let arg_name = ""
       let descr = "specifies file name for LTL property"
     end)

module To_Buchi =
  EmptyString
    (struct
       let option_name = "-to-buchi"
       let arg_name = "f"
       let descr =
	 "only generates the buchi automata (in Promela language) in file <s>"
     end)

module Buchi =
  EmptyString
    (struct
       let option_name = "-buchi"
       let arg_name = "f"
       let descr = "considers the property described by the buchi automata (in
  Promela language) from file <f>."
     end)

module Ya =
  EmptyString
    (struct
       let option_name = "-ltl-automata"
       let arg_name = "f"
       let descr = "considers the property described by the ya automata (in
  Ya language) from file <f>."
     end)


module Output_Spec =
  False(struct
	  let option_name = "-show-op-spec"
	  let descr =
	    "displays computed pre and post-condition of each operation"
	end)

module Output_C_File =
  EmptyString
    (struct
       let option_name = "-ltl-output-c-file"
       let arg_name = ""
       let descr = "specifies generated file name for annotated C code"
     end)

module Dot =
  False(struct
	  let option_name = "-ltl-dot"
	  let descr = "generates a dot file of the Buchi automata"
	end)

module AbstractInterpretation =
  False(struct
	  let option_name = "-ltl-simple-AI"
	  let descr = "use simple abstract interpretation"
	end)

module AbstractInterpretationOff  =
  False(struct
	  let option_name = "-ltl-AI-off"
	  let descr = "does not use abstract interpretation"
	end)

let () = Plugin.set_negative_option_name "-ltl-spec-off"
module Axiomatization =
  True(struct
	 let option_name = "-ltl-spec-on"
	 let descr = "if set, does not axiomatize automata"
       end)

module ConsiderAcceptance =
  False(struct
	 let option_name = "-ltl-acceptance"
	 let descr = "if set, considers acceptation states"
       end)

module AutomataSimplification=
  True
    (struct
       let option_name = "-ltl-raw-auto"
       let descr = "If set, does not simplify automata"
     end)

module Test =
  Zero(struct
	 let option_name = "-ltl-test"
	 let arg_name = ""
	 let descr = "Testing mode (0 = no test)"
       end)

module AddingOperationNameAndStatusInSpecification =
 	False(struct
		let option_name = "-ltl-add-oper" 
		let descr = "Adding current operation name (and statut) in pre/post conditions"
		end) 

let is_on () =
  not (Ltl_File.is_default () && To_Buchi.is_default () &&
       Buchi.is_default ()    && Ya.is_default () )

(* [JS 2009/10/04]
   Preserve the behaviour of svn release <= r5012.
   However it works only if ltl_to_acsl is run from the command line. *)
let init () =
  if is_on () then begin
    Parameters.SimplifyCfg.on ();
    Parameters.KeepSwitch.on ()
  end

let () = 
  Cmdline.run_after_configuring_stage init;
  Parameters.SimplifyCfg.depend Ltl_File.self;
  Parameters.SimplifyCfg.depend To_Buchi.self;
  Parameters.SimplifyCfg.depend Buchi.self;
  Parameters.SimplifyCfg.depend Ya.self;
  Parameters.KeepSwitch.depend Ltl_File.self;
  Parameters.KeepSwitch.depend To_Buchi.self;
  Parameters.KeepSwitch.depend Buchi.self;
  Parameters.KeepSwitch.depend Ya.self

let promela_file () =
  if Buchi.get () = "" then To_Buchi.get () else Buchi.get ()

let advance_abstract_interpretation () =
  not (AbstractInterpretationOff.get ()) && not (AbstractInterpretation.get ())

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
