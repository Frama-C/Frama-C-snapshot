(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
       let descr =
 "automatically computes variation domains for the variables of the program"
    end)

let add_dependency = Project.Computation.add_dependency Db.Value.self

module ForceValues =
  Action
    (struct
       let option_name = "-val"
       let descr = "compute values"
     end)

module MemFunctions =
  StringSet
    (struct
       let option_name = "-mem-exec"
       let arg_name = "f"
       let descr = "do not unroll calls to function f (experimental)"
     end)

module MemExecAll =
  False
    (struct
       let option_name = "-mem-exec-all"
       let descr = "(experimental)"
     end)

module NoResultsFunctions =
  StringSet
    (struct
       let option_name = "-no-results-function"
       let arg_name = "f"
       let descr = "do not record the values obtained for the statements of function f"
     end)

module NoResultsAll =
  False
    (struct
       let option_name = "-no-results"
       let descr = "do not record values for any of the statements of the program"
     end)

let () =
  add_dependency MemFunctions.self;
  add_dependency MemExecAll.self;
  add_dependency NoResultsFunctions.self;
  add_dependency NoResultsAll.self

module SignedOverflow =
  False
    (struct
       let option_name = "-val-signed-overflow-alarms"
       let descr = 
	 "Emit alarms for overflows in signed arithmetic. Experimental"
     end)

let () =
  add_dependency SignedOverflow.self


module MemoryFootprint =
  Int
    (struct
       let option_name = "-memory-footprint"
       let default = 2
       let arg_name = ""
       let descr = "tell the analyser to compromise towards speed or towards low memory use. 1 : small memory; 2 : medium (suitable for recent notebooks); 3 : big (suitable for workstations with 3Gb physical memory or more). Defaults to 2"
     end)

let () =
  MemoryFootprint.add_set_hook
    (fun _ x ->
       Binary_cache.MemoryFootprint.set x;
       Buckx.MemoryFootprint.set x);
  Binary_cache.MemoryFootprint.depend MemoryFootprint.self;
  Buckx.MemoryFootprint.depend MemoryFootprint.self

let initial_context = add_group "Initial Context"

let () = Plugin.set_group initial_context
module AutomaticContextMaxDepth =
  Int
    (struct
       let option_name = "-context-depth"
       let default = 2
       let arg_name = "n"
       let descr = "use <n> as the depth of the default context for value analysis. (defaults to 2)"
     end)

let () = Plugin.set_group initial_context
module AutomaticContextMaxWidth =
  Int
    (struct
       let option_name = "-context-width"
       let default = 2
       let arg_name = "n"
       let descr = "use <n> as the width of the default context for value analysis. (defaults to 2)"
     end)

let () =
  add_dependency AutomaticContextMaxWidth.self;
  add_dependency AutomaticContextMaxDepth.self

let () = Plugin.set_group initial_context
module AllRoundingModes =
  False
    (struct
       let option_name = "-all-rounding-modes"
       let descr = "Take more FPU and compiler behaviors into account"
     end)

let () =
  add_dependency AllRoundingModes.self

let () = Plugin.set_group initial_context
module AllocatedContextValid =
  False
    (struct
       let option_name = "-context-valid-pointers"
       let descr = "only allocate valid pointers until context-depth, and then use NULL (defaults to false)"
     end)
let () = add_dependency AllocatedContextValid.self

let precision_tuning = add_group "Precision tuning"

let () = Plugin.set_group precision_tuning
module PropagateTop =
  False
    (struct
       let option_name = "-propagate-top"
       let descr = "do not stop value analysis even if it is degenerating"
     end)
let () = add_dependency PropagateTop.self

let () = Plugin.set_group precision_tuning
module WideningLevel =
  Int
    (struct
       let default = 3
       let option_name = "-wlevel"
       let arg_name = "n"
       let descr =
	 "do <n> loop iterations before widening (defaults to 3)"
     end)
let () = add_dependency WideningLevel.self

let () = Plugin.set_group precision_tuning
module ArrayPrecisionLevel =
  Int
    (struct
       let default = 200
       let option_name = "-plevel"
       let arg_name = "n"
       let descr = "use <n> as the precision level for arrays accesses. Array accesses are precise as long as the interval for the index contains less than n values. (defaults to 200)"
     end)
let () = add_dependency ArrayPrecisionLevel.self

let () = Plugin.set_group precision_tuning
module SemanticUnrollingLevel =
  Zero
    (struct
       let option_name = "-slevel"
       let arg_name = "n"
       let descr =
	 "use <n> as number of path to explore in parallel (defaults to 0)"
     end)
let () = add_dependency SemanticUnrollingLevel.self

let () = Plugin.set_group precision_tuning
module SlevelFunction =
  StringHashtbl
    (struct
       let option_name = "-slevel-function"
       let arg_name = "f:n"
       let descr = "override slevel with <n> when analyzing <f>"
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
let () = add_dependency SlevelFunction.self

let () = Plugin.set_group precision_tuning
module Subdivide_float_in_expr =
  Zero 
    (struct
      let option_name = "-subdivide-float-var"
      let arg_name = "n"
      let descr =
	"use <n> as number of subdivisions allowed for float variables in expressions (experimental, defaults to 0)"
    end)
let () = add_dependency Subdivide_float_in_expr.self 


