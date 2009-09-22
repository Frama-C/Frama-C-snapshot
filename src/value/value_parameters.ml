(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

module WarnUnspecifiedOrder =
  False
    (struct
       let option_name = "-warn-unspecified-order"
       let descr=
	 "if set, warns for side effects occuring in unspecified order"
     end)

module MemFunctions =
  StringSet
    (struct
       let option_name = "-mem-exec"
       let arg_name = ""
       let descr = "do not unroll calls to function name (experimental)"
     end)

module MemExecAll =
  False
    (struct
       let option_name = "-mem-exec-all"
       let descr = "(experimental)"
     end)

let () =
  add_dependency MemFunctions.self;
  add_dependency MemExecAll.self

module KeepOnlyLastRun =
  False
    (struct
       let option_name = "-klr"
       let descr = "keep only last run of value analysis. All other computations become relative to the last run. This is a debugging option."
     end)

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
  Buckx.MemoryFootprint.depend MemoryFootprint.self;
  add_dependency KeepOnlyLastRun.self

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
       let descr = "do not stop value analysis even if state is degenerating"
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
	 "set <n> as number of iterations before widening (defaults to 3)"
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


