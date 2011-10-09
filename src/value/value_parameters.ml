(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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

(* Dependencies to kernel options *)
let kernel_parameters_correctness = [
  Kernel.MainFunction.parameter;
  Kernel.LibEntry.parameter;
  Kernel.AbsoluteValidRange.parameter;
  Kernel.Overflow.parameter;
  Kernel.SafeArrays.parameter;
  Kernel.UnspecifiedAccess.parameter;
]

let kernel_parameters_precision = [
  Kernel.PreciseUnions.parameter;
  Kernel.ArrayPrecisionLevel.parameter;
]

let parameters_correctness = ref []
let parameters_tuning = ref []
let add_dep p =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self [State.get p.Parameter.name]
let add_correctness_dep p =
  add_dep p;
  parameters_correctness := p :: !parameters_correctness
let add_precision_dep p =
  add_dep p;
  parameters_tuning := p :: !parameters_tuning
let () =
  List.iter add_correctness_dep kernel_parameters_correctness;
  List.iter add_precision_dep kernel_parameters_precision;
;;


include Plugin.Register
    (struct
       let name = "value analysis"
       let shortname = "value"
       let module_name = "Value"
       let help =
 "automatically computes variation domains for the variables of the program"
    end)

module ForceValues =
  WithOutput
    (struct
       let option_name = "-val"
       let help = "compute values"
       let output_by_default = true
     end)

let precision_tuning = add_group "Precision vs. time"
let initial_context = add_group "Initial Context"
let performance = add_group "Results memoization vs. time"


(* -------------------------------------------------------------------------- *)
(* --- Performance options                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = Plugin.set_group performance
module NoResultsFunctions =
  StringSet
    (struct
       let option_name = "-no-results-function"
       let arg_name = "f"
       let help = "do not record the values obtained for the statements of function f"
     end)
let () = add_dep NoResultsFunctions.parameter

let () = Plugin.set_group performance
module NoResultsAll =
  False
    (struct
       let option_name = "-no-results"
       let help = "do not record values for any of the statements of the program"
     end)
let () = add_dep NoResultsAll.parameter

let () = Plugin.set_group performance
module ObviouslyTerminatesFunctions =
  StringSet
    (struct
       let option_name = "-obviously-terminates-function"
       let arg_name = "f"
       let help = "do not record the values obtained for the statements of function f"
     end)
let () = add_dep ObviouslyTerminatesFunctions.parameter

let () = Plugin.set_group performance
module ObviouslyTerminatesAll =
  False
    (struct
       let option_name = "-obviously-terminates"
       let help = "undocumented. Among effects of this options are the same effects as -no-results"
     end)
let () = add_dep ObviouslyTerminatesAll.parameter

let () = Plugin.set_group performance
module ResultsAfter =
  Bool
    (struct
       let option_name = "-val-after-results"
       let help = "record precisely the values obtained after the evaluation of each statement"
       let default = !Config.is_gui 
     end)
let () = add_dep ResultsAfter.parameter

let () = Plugin.set_group performance
module MemoryFootprint =
  Int
    (struct
       let option_name = "-memory-footprint"
       let default = 2
       let arg_name = ""
       let help = "tell the analyser to compromise towards speed or towards low memory use. 1 : small memory; 2 : medium (suitable for recent notebooks); 3 : big (suitable for workstations with 3Gb physical memory or more). Defaults to 2"
     end)
let () =
  MemoryFootprint.add_set_hook
    (fun _ x ->
       Binary_cache.MemoryFootprint.set x;
       Buckx.MemoryFootprint.set x);
  State_dependency_graph.Static.add_dependencies
    ~from:MemoryFootprint.self
    [ Binary_cache.MemoryFootprint.self; Buckx.MemoryFootprint.self ]


(* ------------------------------------------------------------------------- *)
(* --- Misc                                                              --- *)
(* ------------------------------------------------------------------------- *)

module PropagateTop =
  False
    (struct
       let option_name = "-propagate-top"
       let help = "do not stop value analysis even if it is degenerating"
     end)
let () = add_correctness_dep PropagateTop.parameter

module AllRoundingModes =
  False
    (struct
       let option_name = "-all-rounding-modes"
       let help = "Take more target FPU and compiler behaviors into account"
       let kind = `Correctness
     end)
let () = add_correctness_dep AllRoundingModes.parameter

module UndefinedPointerComparisonPropagateAll =
  False
    (struct
       let option_name = "-undefined-pointer-comparison-propagate-all"
       let help = "if the target program appears to contain undefined pointer comparisons, propagate both outcomes {0; 1} in addition to the emission of an alarm"
     end)
let () = add_correctness_dep UndefinedPointerComparisonPropagateAll.parameter

module SignedOverflow =
  False
    (struct
       let option_name = "-val-signed-overflow-alarms"
       let help =
         "Emit alarms for overflows in signed arithmetic. Experimental"
     end)
let () = add_correctness_dep SignedOverflow.parameter

module IgnoreRecursiveCalls =
  False
    (struct
       let option_name = "-val-ignore-recursive-calls"
       let help =
         "Pretend function calls that would be recursive do not happen. Causes unsoundness"
     end)
let () = add_correctness_dep IgnoreRecursiveCalls.parameter


(* ------------------------------------------------------------------------- *)
(* --- Initial context                                                   --- *)
(* ------------------------------------------------------------------------- *)

let () = Plugin.set_group initial_context
module AutomaticContextMaxDepth =
  Int
    (struct
       let option_name = "-context-depth"
       let default = 2
       let arg_name = "n"
       let help = "use <n> as the depth of the default context for value analysis. (defaults to 2)"
     end)
let () = add_correctness_dep AutomaticContextMaxDepth.parameter

let () = Plugin.set_group initial_context
module AutomaticContextMaxWidth =
  Int
    (struct
       let option_name = "-context-width"
       let default = 2
       let arg_name = "n"
       let help = "use <n> as the width of the default context for value analysis. (defaults to 2)"
     end)
let () = AutomaticContextMaxWidth.set_range ~min:1 ~max:max_int
let () = add_correctness_dep AutomaticContextMaxWidth.parameter

let () = Plugin.set_group initial_context
module SeparateStmtStart =
  StringSet
    (struct
       let option_name = "-separate-stmts"
       let arg_name = "n1,..,nk"
       let help = "Undocumented"
     end)
let () = add_correctness_dep SeparateStmtStart.parameter

let () = Plugin.set_group initial_context
module SeparateStmtWord =
  Int
    (struct
       let option_name = "-separate-n"
       let default = 0
       let arg_name = "n"
       let help = "Undocumented"
     end)
let () = SeparateStmtWord.set_range ~min:0 ~max:1073741823
let () = add_correctness_dep SeparateStmtWord.parameter

let () = Plugin.set_group initial_context
module SeparateStmtOf =
  Int
    (struct
       let option_name = "-separate-of"
       let default = 0
       let arg_name = "n"
       let help = "Undocumented"
     end)
let () = SeparateStmtOf.set_range ~min:0 ~max:1073741823
let () = add_correctness_dep SeparateStmtOf.parameter

let () = Plugin.set_group initial_context
module AllocatedContextValid =
  False
    (struct
       let option_name = "-context-valid-pointers"
       let help = "only allocate valid pointers until context-depth, and then use NULL (defaults to false)"
     end)
let () = add_correctness_dep AllocatedContextValid.parameter


(* ------------------------------------------------------------------------- *)
(* --- Tuning                                                            --- *)
(* ------------------------------------------------------------------------- *)

let () = Plugin.set_group precision_tuning
module WideningLevel =
  Int
    (struct
       let default = 3
       let option_name = "-wlevel"
       let arg_name = "n"
       let help =
         "do <n> loop iterations before widening (defaults to 3)"
     end)
let () = add_precision_dep WideningLevel.parameter

let () = Plugin.set_group precision_tuning
module SemanticUnrollingLevel =
  Zero
    (struct
       let option_name = "-slevel"
       let arg_name = "n"
       let help =
         "use <n> as number of path to explore in parallel (defaults to 0)"
     end)
let () = add_precision_dep SemanticUnrollingLevel.parameter

let split_option =
  let rx = Str.regexp_string ":" in
  fun s ->
    try
      match Str.split rx s with
        | [ f ; n ] -> (f, n)
        | _ -> failwith ""
    with _ -> failwith "split_option"

let () = Plugin.set_group precision_tuning
module SlevelFunction =
  StringHashtbl
    (struct
       let option_name = "-slevel-function"
       let arg_name = "f:n"
       let help = "override slevel with <n> when analyzing <f>"
     end)
    (struct
      include Datatype.Int

      let parse s =
        try
          let f, n =  split_option s in
          let n = int_of_string n in
          f, n
        with
       | Failure _ -> abort "Could not parse option \"-slevel-function %s\"" s
      let no_binding _ = SemanticUnrollingLevel.get ()
    end)
let () = add_precision_dep SlevelFunction.parameter

let () = Plugin.set_group precision_tuning
module BuiltinsOverrides =
  StringHashtbl
    (struct
       let option_name = "-val-builtin"
       let arg_name = "f:ffc"
       let help = "when analyzing function <f>, try to use Frama-C builtin <ffc> instead. Fall back to <f> if <ffc> cannot handle its arguments (experimental)."
     end)
    (struct
        include Datatype.String
        let parse s =
          try split_option s
          with Failure _ -> abort "Could not parse option \"-val-builtin %s\"" s
        let no_binding _ = raise Not_found
     end)
let () = add_precision_dep BuiltinsOverrides.parameter

let () = Plugin.set_group precision_tuning
module Subdivide_float_in_expr =
  Zero
    (struct
      let option_name = "-subdivide-float-var"
      let arg_name = "n"
      let help =
        "use <n> as number of subdivisions allowed for float variables in expressions (experimental, defaults to 0)"
    end)
let () = add_precision_dep Subdivide_float_in_expr.parameter

let () = Plugin.set_group precision_tuning
module UsePrototype =
  StringSet
    (struct
      let option_name = "-val-use-spec"
      let arg_name = "f1,..,fn"
      let help = "undocumented"
    end)
let () = add_precision_dep UsePrototype.parameter

let () = Plugin.set_group precision_tuning
module RmAssert =
  False
    (struct
      let option_name = "-remove-redundant-alarms"
      let help = "after the analysis, try to remove redundant alarms, so that the user needs inspect fewer of them"
    end)
let () = add_precision_dep RmAssert.parameter


(* ------------------------------------------------------------------------- *)
(* --- Messages                                                          --- *)
(* ------------------------------------------------------------------------- *)

let () = Plugin.set_group messages
module ValShowProgress =
  True
    (struct
       let option_name = "-val-show-progress"
       let help = "Show progression messages during value analysis"
     end)

let () = Plugin.set_group messages
module PrintCallstacks =
  False
    (struct
       let option_name = "-val-print-callstacks"
       let help = "When printing a message, also show the current analysis context."
     end)


let parameters_correctness = !parameters_correctness
let parameters_tuning = !parameters_tuning


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
