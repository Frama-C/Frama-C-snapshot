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

include Plugin.Register
    (struct
       let name = "value analysis"
       let shortname = "value"
       let module_name = "Value"
       let help =
 "automatically computes variation domains for the variables of the program"
    end)

module ForceValues =
  Action
    (struct
       let option_name = "-val"
       let help = "compute values"
       let kind = `Tuning
     end)

module MemFunctions =
  StringSet
    (struct
       let option_name = "-mem-exec"
       let arg_name = "f"
       let help = "do not unroll calls to function f (experimental)"
       let kind = `Tuning
     end)

module MemExecAll =
  False
    (struct
       let option_name = "-mem-exec-all"
       let help = "(experimental)"
       let kind = `Tuning
     end)

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ MemFunctions.self;
      MemExecAll.self;
    ]

module NoResultsFunctions =
  StringSet
    (struct
       let option_name = "-no-results-function"
       let arg_name = "f"
       let help = "do not record the values obtained for the statements of function f"
       let kind = `Tuning
     end)

module NoResultsAll =
  False
    (struct
       let option_name = "-no-results"
       let help = "do not record values for any of the statements of the program"
       let kind = `Tuning
     end)

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [
     NoResultsFunctions.self;
      NoResultsAll.self;
    ]


module SignedOverflow =
  False
    (struct
       let option_name = "-val-signed-overflow-alarms"
       let help =
	 "Emit alarms for overflows in signed arithmetic. Experimental"
       let kind = `Correctness
     end)

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [
      SignedOverflow.self;
    ]

module IgnoreRecursiveCalls =
  False
    (struct
       let option_name = "-val-ignore-recursive-calls"
       let help =
	 "Pretend function calls that would be recursive do not happen. Causes unsoundness"
       let kind = `Correctness
     end)

module MemoryFootprint =
  Int
    (struct
       let option_name = "-memory-footprint"
       let default = 2
       let arg_name = ""
       let help = "tell the analyser to compromise towards speed or towards low memory use. 1 : small memory; 2 : medium (suitable for recent notebooks); 3 : big (suitable for workstations with 3Gb physical memory or more). Defaults to 2"
       let kind = `Tuning
     end)


let () =
  MemoryFootprint.add_set_hook
    (fun _ x ->
       Binary_cache.MemoryFootprint.set x;
       Buckx.MemoryFootprint.set x);
  State_dependency_graph.Static.add_dependencies
    ~from:MemoryFootprint.self
    [ Binary_cache.MemoryFootprint.self; Buckx.MemoryFootprint.self ]

let initial_context = add_group "Initial Context"

let () = Plugin.set_group initial_context
module AutomaticContextMaxDepth =
  Int
    (struct
       let option_name = "-context-depth"
       let default = 2
       let arg_name = "n"
       let help = "use <n> as the depth of the default context for value analysis. (defaults to 2)"
       let kind = `Correctness
     end)

let () = Plugin.set_group initial_context
module AutomaticContextMaxWidth =
  Int
    (struct
       let option_name = "-context-width"
       let default = 2
       let arg_name = "n"
       let help = "use <n> as the width of the default context for value analysis. (defaults to 2)"
       let kind = `Correctness
     end)
let () = AutomaticContextMaxWidth.set_range ~min:1 ~max:max_int

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [
      AutomaticContextMaxWidth.self;
      AutomaticContextMaxDepth.self;
    ]

let () = Plugin.set_group initial_context
module SeparateStmtStart =
  StringSet
    (struct
       let option_name = "-separate-stmts"
       let arg_name = "n1,..,nk"
       let help = "Undocumented"
       let kind = `Correctness
     end)

let () = Plugin.set_group initial_context
module SeparateStmtWord =
  Int
    (struct
       let option_name = "-separate-n"
       let default = 0
       let arg_name = "n"
       let help = "Undocumented"
       let kind = `Correctness
     end)
let () = SeparateStmtWord.set_range ~min:0 ~max:1073741823

let () = Plugin.set_group initial_context
module SeparateStmtOf =
  Int
    (struct
       let option_name = "-separate-of"
       let default = 0
       let arg_name = "n"
       let help = "Undocumented"
       let kind = `Correctness
     end)
let () = SeparateStmtOf.set_range ~min:0 ~max:1073741823


let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ 
      SeparateStmtStart.self;
      SeparateStmtWord.self;
      SeparateStmtOf.self;
    ]


let () = Plugin.set_group initial_context
module AllRoundingModes =
  False
    (struct
       let option_name = "-all-rounding-modes"
       let help = "Take more target FPU and compiler behaviors into account"
       let kind = `Correctness
     end)

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ 
      AllRoundingModes.self;
    ]

let () = Plugin.set_group initial_context
module AllocatedContextValid =
  False
    (struct
       let option_name = "-context-valid-pointers"
       let help = "only allocate valid pointers until context-depth, and then use NULL (defaults to false)"
       let kind = `Correctness
     end)
let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ 
      AllocatedContextValid.self;
    ]

let () = Plugin.set_group initial_context
module UndefinedPointerComparisonPropagateAll =
  False
    (struct
       let option_name = "-undefined-pointer-comparison-propagate-all"
       let help = "if the target program appears to contain undefined pointer comparisons, propagate both outcomes {0; 1;} in addition to the emission of an alarm"
       let kind = `Correctness
     end)
let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ 
      UndefinedPointerComparisonPropagateAll.self;
    ]

let precision_tuning = add_group "Precision tuning"

let () = Plugin.set_group precision_tuning
module PropagateTop =
  False
    (struct
       let option_name = "-propagate-top"
       let help = "do not stop value analysis even if it is degenerating"
       let kind = `Tuning
     end)

let () = Plugin.set_group precision_tuning
module WideningLevel =
  Int
    (struct
       let default = 3
       let option_name = "-wlevel"
       let arg_name = "n"
       let help =
	 "do <n> loop iterations before widening (defaults to 3)"
       let kind = `Tuning
     end)

let () = Plugin.set_group precision_tuning
module ArrayPrecisionLevel =
  Int
    (struct
       let default = 200
       let option_name = "-plevel"
       let arg_name = "n"
       let help = "use <n> as the precision level for arrays accesses. Array accesses are precise as long as the interval for the index contains less than n values. (defaults to 200)"
       let kind = `Tuning
     end)

let () = Plugin.set_group precision_tuning
module SemanticUnrollingLevel =
  Zero
    (struct
       let option_name = "-slevel"
       let arg_name = "n"
       let help =
	 "use <n> as number of path to explore in parallel (defaults to 0)"
       let kind = `Tuning
     end)

let () = Plugin.set_group precision_tuning
module SlevelFunction =
  StringHashtbl
    (struct
       let option_name = "-slevel-function"
       let arg_name = "f:n"
       let help = "override slevel with <n> when analyzing <f>"
       let kind = `Tuning
     end)
    (struct
      include Datatype.Int
      let rx = Str.regexp_string ":"
      let parse s =
	try
	  match Str.split rx s with
	    [ f ; n ]  ->
	      let n = int_of_string n in
              f, n
	  | _ -> failwith ""
	with
       | Failure _ -> abort "Could not parse option \"-slevel-function %s\"" s
      let no_binding _ = SemanticUnrollingLevel.get ()
    end)

let () = Plugin.set_group precision_tuning
module Subdivide_float_in_expr =
  Zero
    (struct
      let option_name = "-subdivide-float-var"
      let arg_name = "n"
      let help =
	"use <n> as number of subdivisions allowed for float variables in expressions (experimental, defaults to 0)"
      let kind = `Tuning
    end)

let () =
  State_dependency_graph.Static.add_codependencies
    ~onto:Db.Value.self
    [ 
      PropagateTop.self;
      WideningLevel.self;
      ArrayPrecisionLevel.self;
      SemanticUnrollingLevel.self;
      SlevelFunction.self;
      Subdivide_float_in_expr.self;
    ]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
