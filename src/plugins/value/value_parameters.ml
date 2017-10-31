(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
  Kernel.SafeArrays.parameter;
  Kernel.UnspecifiedAccess.parameter;
  Kernel.SignedOverflow.parameter;
  Kernel.UnsignedOverflow.parameter;
  Kernel.SignedDowncast.parameter;
  Kernel.UnsignedDowncast.parameter;
  Kernel.ConstReadonly.parameter;
]

let parameters_correctness = ref Typed_parameter.Set.empty
let parameters_tuning = ref Typed_parameter.Set.empty
let add_dep p =
  State_dependency_graph.add_codependencies
    ~onto:Db.Value.self 
    [State.get p.Typed_parameter.name]
let add_correctness_dep p =
  if Typed_parameter.Set.mem p !parameters_correctness then
    Kernel.abort "adding correctness parameter %a twice"
      Typed_parameter.pretty p;
  add_dep p;
  parameters_correctness := Typed_parameter.Set.add p !parameters_correctness
let add_precision_dep p =
  if Typed_parameter.Set.mem p !parameters_tuning then
    Kernel.abort "adding tuning parameter %a twice"
      Typed_parameter.pretty p;
  add_dep p;
  parameters_tuning := Typed_parameter.Set.add p !parameters_tuning

let () = List.iter add_correctness_dep kernel_parameters_correctness

let sdkey_initial_state = "initial-state"
let sdkey_final_states = "final-states"
let sdkey_alarm = "alarm"
let sdkey_garbled_mix = "garbled-mix" (* not activated by default *)
let sdkey_pointer_comparison = "pointer-comparison"
let sdkey_cvalue_domain = "d-cvalue"


let () =
  Plugin.default_msg_keys
    [sdkey_initial_state; sdkey_final_states; sdkey_alarm; sdkey_cvalue_domain]

include Plugin.Register
    (struct
       let name = "value analysis"
       let shortname = "value"
       let help =
 "automatically computes variation domains for the variables of the program"
    end)

let () = Help.add_aliases [ "-val-h" ]

module ForceValues =
  WithOutput
    (struct
       let option_name = "-val"
       let help = "compute values"
       let output_by_default = true
     end)

let domains = add_group "Abstract Domains"
let precision_tuning = add_group "Precision vs. time"
let initial_context = add_group "Initial Context"
let performance = add_group "Results memoization vs. time"
let interpreter = add_group "Deterministic programs"
let alarms = add_group "Propagation and alarms "
let malloc = add_group "Dynamic allocation"

(* -------------------------------------------------------------------------- *)
(* --- Eva domains                                                        --- *)
(* -------------------------------------------------------------------------- *)

(* Set of parameters defining the abstractions used in an Eva analysis. *)
let parameters_abstractions = ref Typed_parameter.Set.empty

(* This functor must be used to create parameters for new domains of Eva. *)
module Domain_Parameter
    (X:sig include Parameter_sig.Input val default: bool end)
= struct
  Parameter_customize.set_group domains;
  module Parameter = Bool (X);;
  add_precision_dep Parameter.parameter;
  parameters_abstractions :=
    Typed_parameter.Set.add Parameter.parameter !parameters_abstractions;
  include Parameter
end

module CvalueDomain = Domain_Parameter
    (struct
      let option_name = "-eva-cvalue-domain"
      let help = "Use the default domain of eva."
      let default = true
    end)

module EqualityDomain = Domain_Parameter
    (struct
      let option_name = "-eva-equality-domain"
      let help = "Use the equality domain of eva. Experimental."
      let default = false
    end)

module GaugesDomain = Domain_Parameter
    (struct
      let option_name = "-eva-gauges-domain"
      let help = "Use the gauges domain of Eva. Experimental."
      let default = false
    end)

module SymbolicLocsDomain = Domain_Parameter
    (struct
      let option_name = "-eva-symbolic-locations-domain"
      let help = "Use dedicated domain for symbolic equalities. Experimental."
      let default = false
    end)

let apron_help = "Experimental binding of the numerical domains provided \
                  by the APRON library: http://apron.cri.ensmp.fr/library \n"

module ApronOctagon = Domain_Parameter
    (struct
      let option_name = "-eva-apron-oct"
      let help = apron_help ^ "Use the octagon domain of apron."
      let default = false
    end)

module ApronBox = Domain_Parameter
    (struct
      let option_name = "-eva-apron-box"
      let help = apron_help ^ "Use the box domain of apron."
      let default = false
    end)

module PolkaLoose = Domain_Parameter
    (struct
      let option_name = "-eva-polka-loose"
      let help = apron_help ^ "Use the loose polyhedra domain of apron."
      let default = false
    end)

module PolkaStrict = Domain_Parameter
    (struct
      let option_name = "-eva-polka-strict"
      let help = apron_help ^ "Use the strict polyhedra domain of apron."
      let default = false
    end)

module PolkaEqualities = Domain_Parameter
    (struct
      let option_name = "-eva-polka-equalities"
      let help = apron_help ^ "Use the linear equalities domain of apron."
      let default = false
    end)

module InoutDomain = Domain_Parameter
    (struct
      let option_name = "-eva-inout-domain"
      let help = "Compute inputs and outputs within Eva. Experimental."
      let default = false
    end)

module SignDomain = Domain_Parameter
    (struct
      let option_name = "-eva-sign-domain"
      let help = "Use the sign domain of Eva. For demonstration purposes only."
      let default = false
    end)

module BitwiseOffsmDomain = Domain_Parameter
    (struct
      let option_name = "-eva-bitwise-domain"
      let help = "Use the bitwise abstractions of eva."
      let default = false
    end)


let () = Parameter_customize.set_group domains
module EqualityStorage =
  Bool
    (struct
      let option_name = "-eva-equality-storage"
      let help = "Stores the states of the equality domain during \
                 the analysis."
      let default = true
    end)
let () = add_precision_dep EqualityStorage.parameter

let () = Parameter_customize.set_group domains
module SymbolicLocsStorage =
  Bool
    (struct
      let option_name = "-eva-symbolic-locations-storage"
      let help = "Stores the states of the symbolic locations domain during \
                 the analysis."
      let default = true
    end)
let () = add_precision_dep SymbolicLocsStorage.parameter

let () = Parameter_customize.set_group domains
module GaugesStorage =
  Bool
    (struct
      let option_name = "-eva-gauges-storage"
      let help = "Stores the states of the gauges domain during the analysis."
      let default = true
    end)
let () = add_precision_dep GaugesStorage.parameter

let () = Parameter_customize.set_group domains
module ApronStorage =
  Bool
    (struct
      let option_name = "-eva-apron-storage"
      let help = "Stores the states of the apron domains during the \
                 analysis."
      let default = true
    end)
let () = add_precision_dep ApronStorage.parameter

let () = Parameter_customize.set_group domains
module BitwiseOffsmStorage =
  Bool
    (struct
      let option_name = "-eva-bitwise-storage"
      let help = "Stores the states of the bitwise domain during the \
                 analysis."
      let default = true
    end)
let () = add_precision_dep BitwiseOffsmStorage.parameter


(* -------------------------------------------------------------------------- *)
(* --- Performance options                                                --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group performance

module NoResultsFunctions =
  Fundec_set
    (struct
       let option_name = "-no-results-function"
       let arg_name = "f"
       let help = "do not record the values obtained for the statements of \
function f"
     end)
let () = add_dep NoResultsFunctions.parameter

let () = Parameter_customize.set_group performance
let () = Parameter_customize.set_negative_option_name "-val-store-results"
module NoResultsAll =
  False
    (struct
       let option_name = "-no-results"
       let help = "do not record values for any of the statements of the \
program"
     end)
let () = add_dep NoResultsAll.parameter

let () = Parameter_customize.set_group performance
let () = Parameter_customize.is_invisible ()
module ResultsAfter =
  Bool
    (struct
       let option_name = "-val-after-results"
       let help = "record precisely the values obtained after the evaluation of each statement"
       let default = true
     end)
let () =
  ResultsAfter.add_set_hook
    (fun _ new_ ->
      if new_ then
        Kernel.feedback "@[Option -val-after-results is now always set.@]"
      else
        Kernel.warning "@[Option -val-after-results can no longer be unset.@]")

let () = Parameter_customize.set_group performance
let () = Parameter_customize.is_invisible ()
module ResultsCallstack =
  Bool
    (struct
       let option_name = "-val-callstack-results"
       let help = "always enabled, cannot be disabled: used to record precisely the values obtained for each callstack leading to each statement"
       let default = false
     end)
let () = add_precision_dep ResultsCallstack.parameter

let () = Parameter_customize.set_group performance
module JoinResults =
  Bool
    (struct
       let option_name = "-val-join-results"
       let help = "precompute consolidated states once value is computed"
       let default = true
     end)

let () = Parameter_customize.set_group performance
let () = Parameter_customize.is_invisible ()
module MemoryFootprint =
  Int
    (struct
       let option_name = "-memory-footprint"
       let default = 2
       let arg_name = ""
       let help = ""
     end)
let () = MemoryFootprint.set_range ~min:1 ~max:16
let () =
  MemoryFootprint.add_set_hook
    (fun _ _ -> Kernel.error
      "@[Option -memory-footprint@ has been replaced@ by an environment@ \
       variable@ %s.@ The range of@ possible values@ is 1-10.@ Memory usage@ \
       allocated@ to caches@ doubles@ with each@ increase.@ '2' is a good@ \
       starting point." Binary_cache.memory_footprint_var_name
    )


(* ------------------------------------------------------------------------- *)
(* --- Non-standard alarms                                               --- *)
(* ------------------------------------------------------------------------- *)
    
let () = Parameter_customize.set_group alarms
module AllRoundingModes =
  False
    (struct
       let option_name = "-all-rounding-modes"
       let help = "Take more target FPU and compiler behaviors into account"
     end)
let () = add_correctness_dep AllRoundingModes.parameter

let () = Parameter_customize.set_group alarms
module AllRoundingModesConstants =
  False
    (struct
       let option_name = "-all-rounding-modes-constants"
       let help = "Take into account the possibility of constants not being converted to the nearest representable value, or being converted to higher precision"
     end)
let () = add_correctness_dep AllRoundingModesConstants.parameter

let () = Parameter_customize.set_group alarms
module UndefinedPointerComparisonPropagateAll =
  False
    (struct
       let option_name = "-undefined-pointer-comparison-propagate-all"
       let help = "if the target program appears to contain undefined pointer comparisons, propagate both outcomes {0; 1} in addition to the emission of an alarm"
     end)
let () = add_correctness_dep UndefinedPointerComparisonPropagateAll.parameter

let () = Parameter_customize.set_group alarms
module WarnPointerComparison =
  String
    (struct
      let option_name = "-val-warn-undefined-pointer-comparison"
      let help = "warn on all pointer comparisons (default), on comparisons \
                  where the arguments have pointer type, or never warn"
      let default = "all"
      let arg_name = "all|pointer|none"
    end)
let () = WarnPointerComparison.set_possible_values ["all"; "pointer"; "none"]
let () = add_correctness_dep WarnPointerComparison.parameter


let () = Parameter_customize.set_group alarms
module WarnLeftShiftNegative =
  True
    (struct
       let option_name = "-val-warn-left-shift-negative"
       let help =
         "Emit alarms when left-shifting negative integers"
     end)
let () = add_correctness_dep WarnLeftShiftNegative.parameter

let () = Parameter_customize.set_group alarms
let () = Parameter_customize.is_invisible ()
module LeftShiftNegativeOld =
  True
    (struct
       let option_name = "-val-left-shift-negative-alarms"
       let help =
         "Emit alarms when left shifting negative integers"
     end)
let () = LeftShiftNegativeOld.add_set_hook
  (fun _oldv newv ->
    let no = if newv then "" else "no-" in
    warning "New option name for \
        -%sval-left-shift-negative-alarms is -%sval-warn-left-shift-negative"
      no no;
    WarnLeftShiftNegative.set newv)


let () = Parameter_customize.set_group alarms
module WarnSignedConvertedDowncast =
  False
    (struct
       let option_name = "-val-warn-signed-converted-downcast"
       let help = "Signed downcasts are decomposed into two operations: \
                   a conversion to the signed type of the original width, \
                   then a downcast. Warn when the downcast may exceed the \
                   destination range."
     end)
let () = add_correctness_dep WarnSignedConvertedDowncast.parameter


let () = Parameter_customize.set_group alarms
module WarnPointerSubstraction =
  True
    (struct
       let option_name = "-val-warn-pointer-subtraction"
       let help =
         "Warn when subtracting two pointers that may not be in the same \
          allocated block, and return the pointwise difference between the \
          offsets. When unset, do not warn but generate imprecise offsets."
     end)
let () = add_correctness_dep WarnPointerSubstraction.parameter

let () = Parameter_customize.set_group alarms
module IgnoreRecursiveCalls =
  False
    (struct
       let option_name = "-val-ignore-recursive-calls"
       let help =
         "Pretend function calls that would be recursive do not happen. Causes unsoundness"
     end)
let () = add_correctness_dep IgnoreRecursiveCalls.parameter

let () = Parameter_customize.set_group alarms

module WarnCopyIndeterminate =
  Kernel_function_set
    (struct
       let option_name = "-val-warn-copy-indeterminate"
       let arg_name = "f | @all"
       let help = "warn when a statement of the specified functions copies a \
value that may be indeterminate (uninitialized or containing escaping address). \
Set by default; can be deactivated for function 'f' by '=-f', or for all \
functions by '=-@all'."
     end)
let () = add_correctness_dep WarnCopyIndeterminate.parameter
let () = WarnCopyIndeterminate.Category.(set_default (all ()))

let () = Parameter_customize.set_group alarms
module ReduceOnLogicAlarms =
  False
    (struct
      let option_name = "-val-reduce-on-logic-alarms"
      let help = "Force reductions by a predicate to ignore logic alarms \
                  emitted while the predicated is evaluated (experimental)"
    end)
let () = add_correctness_dep ReduceOnLogicAlarms.parameter

let () = Parameter_customize.set_group alarms
module InitializedLocals =
  False
    (struct
      let option_name = "-val-initialized-locals"
      let help = "Local variables enter in scope fully initialized. \
                  Only useful for the analysis of programs buggy w.r.t. \
                  initialization."
    end)
let () = add_correctness_dep InitializedLocals.parameter

(* ------------------------------------------------------------------------- *)
(* --- Initial context                                                   --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group initial_context
module AutomaticContextMaxDepth =
  Int
    (struct
       let option_name = "-context-depth"
       let default = 2
       let arg_name = "n"
       let help = "use <n> as the depth of the default context for value analysis. (defaults to 2)"
     end)
let () = add_correctness_dep AutomaticContextMaxDepth.parameter

let () = Parameter_customize.set_group initial_context
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

let () = Parameter_customize.set_group initial_context
module AllocatedContextValid =
  False
    (struct
       let option_name = "-context-valid-pointers"
       let help = "only allocate valid pointers until context-depth, and then use NULL (defaults to false)"
     end)
let () = add_correctness_dep AllocatedContextValid.parameter

let () = Parameter_customize.set_group initial_context
module InitializationPaddingGlobals =
  String
    (struct
      let default = "yes"
      let option_name = "-val-initialization-padding-globals"
      let arg_name = "yes|no|maybe"
      let help = "Specify how padding bits are initialized inside global \
        variables. Possible values are <yes> (padding is fully initialized), \
        <no> (padding is completely uninitialized), or <maybe> \
        (padding may be uninitialized). Default is <yes>."
     end)
let () = InitializationPaddingGlobals.set_possible_values
  ["yes"; "no"; "maybe"]
let () = add_correctness_dep InitializationPaddingGlobals.parameter

let () = Parameter_customize.set_group initial_context
let () = Parameter_customize.set_negative_option_name
  "-uninitialized-padding-globals"
let () = Parameter_customize.is_invisible ()
module InitializedPaddingGlobals =
  True
    (struct
       let option_name = "-initialized-padding-globals"
       let help = "Padding in global variables is uninitialized"
     end)
let () = add_correctness_dep InitializedPaddingGlobals.parameter
let () = InitializedPaddingGlobals.add_update_hook
  (fun _ v ->
    warning "This option is deprecated. Use %s instead"
      InitializationPaddingGlobals.name;
    InitializationPaddingGlobals.set (if v then  "yes" else "no"))

(* ------------------------------------------------------------------------- *)
(* --- Tuning                                                            --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group precision_tuning
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

let () = Parameter_customize.set_group precision_tuning
module ILevel =
  Int
    (struct
       let option_name = "-val-ilevel"
       let default = 8
       let arg_name = "n"
       let help =
         "Sets of integers are represented as sets up to <n> elements. \
            Above, intervals with congruence information are used \
            (defaults to 8; experimental)"
     end)
let () = add_precision_dep ILevel.parameter
let () = ILevel.add_update_hook (fun _ i -> Ival.set_small_cardinal i)
let () = ILevel.set_range 4 128

let () = Parameter_customize.set_group precision_tuning
module SemanticUnrollingLevel =
  Zero
    (struct
       let option_name = "-slevel"
       let arg_name = "n"
       let help =
         "superpose up to <n> states when unrolling control flow. The larger n, the more precise and expensive the analysis (defaults to 0)"
     end)
let () = add_precision_dep SemanticUnrollingLevel.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module SlevelFunction =
  Kernel_function_map
    (struct
      include Datatype.Int
      type key = Cil_types.kernel_function
      let of_string ~key:_ ~prev:_ s =
        Extlib.opt_map
          (fun s ->
            try int_of_string s
            with Failure _ ->
              raise (Cannot_build ("'" ^ s ^ "' is not an integer")))
          s
      let to_string ~key:_ = Extlib.opt_map string_of_int
     end)
    (struct
       let option_name = "-slevel-function"
       let arg_name = "f:n"
       let help = "override slevel with <n> when analyzing <f>"
       let default = Kernel_function.Map.empty
     end)
let () = add_precision_dep SlevelFunction.parameter

let () = Parameter_customize.set_group precision_tuning
module SlevelMergeAfterLoop =
  Kernel_function_set
    (struct
       let option_name = "-val-slevel-merge-after-loop"
       let arg_name = "f | @all"
       let help =
         "when set, the different execution paths that originate from the body \
          of a loop are merged before entering the next excution. Experimental."
     end)
let () = add_precision_dep SlevelMergeAfterLoop.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module SplitReturnFunction =
  Kernel_function_map
    (struct
      (* this type is ad-hoc: cannot use Kernel_function_multiple_map here *)
      include Split_strategy
      type key = Cil_types.kernel_function
      let of_string ~key:_ ~prev:_ s =
        try Extlib.opt_map Split_strategy.of_string s
        with Split_strategy.ParseFailure s ->
          raise (Cannot_build ("unknown split strategy " ^ s))
      let to_string ~key:_ v =
        Extlib.opt_map Split_strategy.to_string v
     end)
    (struct
       let option_name = "-val-split-return-function"
       let arg_name = "f:n"
       let help = "split return states of function <f> according to \
                     \\result == n and \\result != n"
       let default = Kernel_function.Map.empty
     end)
let () = add_precision_dep SplitReturnFunction.parameter

let () = Parameter_customize.set_group precision_tuning
module SplitReturn =
  String
    (struct
       let option_name = "-val-split-return"
       let arg_name = "mode"
       let default = ""
       let help = "when 'mode' is a number, or 'full', this is equivalent \
       to -val-split-return-function f:mode for all functions f. \
       When mode is 'auto', automatically split states at the end \
       of all functions, according to the function return code"
     end)
module SplitGlobalStrategy = State_builder.Ref (Split_strategy)
    (struct
       let default () = Split_strategy.NoSplit
       let name = "Value_parameters.SplitGlobalStrategy"
       let dependencies = [SplitReturn.self]
     end)
let () =
  SplitReturn.add_set_hook
    (fun _ x -> SplitGlobalStrategy.set
        (try Split_strategy.of_string x
         with Split_strategy.ParseFailure s ->
           abort "@[@[incorrect argument for option %s@ (%s).@]"
             SplitReturn.name s))
let () = add_precision_dep SplitReturn.parameter

let () = Parameter_customize.is_invisible ()
module SplitReturnAuto =
  False
    (struct
       let option_name = "-val-split-return-auto"
       let help = ""
     end)
let () =
  SplitReturnAuto.add_set_hook
    (fun _ b ->
       warning "option \"-val-split-return-auto\" has been replaced by \
         \"-val-split-return auto\"";
       SplitGlobalStrategy.set
         Split_strategy.(if b then SplitAuto else NoSplit))


let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module BuiltinsOverrides =
  Kernel_function_map
    (struct
      include Datatype.String
      type key = Cil_types.kernel_function
      let of_string ~key:kf ~prev:_ nameopt =
        begin match nameopt with
        | Some name ->
           if not (!Db.Value.mem_builtin name) then
             abort "option '-val-builtin %a:%s': undeclared builtin '%s'@.\
                    declared builtins: @[%a@]"
               Kernel_function.pretty kf name name
               (Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string)
               (List.map fst (!Db.Value.registered_builtins ()))
        | _ -> abort
                 "option '-val-builtin':@ \
                  no builtin associated to function '%a',@ use '%a:<builtin>'"
                 Kernel_function.pretty kf Kernel_function.pretty kf
        end;
        nameopt
      let to_string ~key:_ name = name
     end)
    (struct
       let option_name = "-val-builtin"
       let arg_name = "f:ffc"
       let help = "when analyzing function <f>, try to use Frama-C builtin \
<ffc> instead. Fall back to <f> if <ffc> cannot handle its arguments \
(experimental)."
       let default = Kernel_function.Map.empty
     end)
let () = add_precision_dep BuiltinsOverrides.parameter

let () = Parameter_customize.set_group precision_tuning
module BuiltinsAuto =
  True
    (struct
       let option_name = "-val-builtins-auto"
       let help = "When set, builtins will be used automatically to replace \
                   known C functions"
     end)
let () = add_correctness_dep BuiltinsAuto.parameter

let () = Parameter_customize.set_group precision_tuning
module BuiltinsList =
  False
    (struct
       let option_name = "-val-builtins-list"
       let help = "Lists the existing builtins, and which functions they \
                   are automatically associated to (if any)"
     end)

let () = Parameter_customize.is_invisible ()
module Subdivide_float_in_expr =
  Zero
    (struct
      let option_name = "-subdivide-float-var"
      let arg_name = "n"
      let help =
        "use <n> as number of subdivisions allowed for float variables in expressions (experimental, defaults to 0)"
    end)
let () =
  Subdivide_float_in_expr.add_set_hook
    (fun _ _ ->
     Kernel.abort "@[option -subdivide-float-var has been replaced by \
                   -val-subdivide-non-linear@]")

let () = Parameter_customize.set_group precision_tuning
module LinearLevel =
  Zero
    (struct
      let option_name = "-val-subdivide-non-linear"
      let arg_name = "n"
      let help =
        "Improve precision when evaluating expressions in which a variable \
         appears multiple times, by splitting its value at most n times. \
         Experimental, defaults to 0."
    end)
let () = add_precision_dep LinearLevel.parameter

let () = Parameter_customize.set_group precision_tuning
let () = Parameter_customize.argument_may_be_fundecl ()
module UsePrototype =
  Kernel_function_set
    (struct
      let option_name = "-val-use-spec"
      let arg_name = "f1,..,fn"
      let help = "use the ACSL specification of the functions instead of their definitions"
    end)
let () = add_precision_dep UsePrototype.parameter

let () = Parameter_customize.set_group precision_tuning
module RmAssert =
  True
    (struct
      let option_name = "-remove-redundant-alarms"
      let help = "after the analysis, try to remove redundant alarms, so that the user needs inspect fewer of them"
    end)
let () = add_precision_dep RmAssert.parameter

let () = Parameter_customize.set_group precision_tuning
module MemExecAll =
  False
    (struct
      let option_name = "-memexec-all"
      let help = "(experimental) speed up analysis by not recomputing functions already analyzed in the same context. Incompatible with some plugins and callbacks"
    end)
let () =
  MemExecAll.add_set_hook
    (fun _bold bnew ->
      if bnew then
        try
          Dynamic.Parameter.Bool.set "-inout-callwise" true
        with Dynamic.Unbound_value _ | Dynamic.Incompatible_type _ ->
          abort "Cannot set option -memexec-all. Is plugin Inout registered?"
    )

let () = Parameter_customize.set_group precision_tuning
module ArrayPrecisionLevel =
  Int
    (struct
       let default = 200
       let option_name = "-plevel"
       let arg_name = "n"
       let help = "use <n> as the precision level for arrays accesses. \
Array accesses are precise as long as the interval for the index contains \
less than n values. (defaults to 200)"
     end)
let () = add_precision_dep ArrayPrecisionLevel.parameter
let () = ArrayPrecisionLevel.add_update_hook
  (fun _ v -> Offsetmap.set_plevel v)

(* Options SaveFunctionState and LoadFunctionState are related
   and mutually dependent for sanity checking.
   Also, they depend on BuiltinsOverrides, so they cannot be defined before it. *)
let () = Parameter_customize.set_group initial_context
module SaveFunctionState =
  Kernel_function_map
    (struct
      include Datatype.String
      type key = Cil_types.kernel_function
      let of_string ~key:_ ~prev:_ file = file
      let to_string ~key:_ file = file
    end)
    (struct
      let option_name = "-val-save-fun-state"
      let arg_name = "function:filename"
      let help = "save state of function <function> in file <filename>"
      let default = Kernel_function.Map.empty
    end)
let () = Parameter_customize.set_group initial_context
module LoadFunctionState =
  Kernel_function_map
    (struct
      include Datatype.String
      type key = Cil_types.kernel_function
      let of_string ~key:_ ~prev:_ file = file
      let to_string ~key:_ file = file
    end)
    (struct
      let option_name = "-val-load-fun-state"
      let arg_name = "function:filename"
      let help = "load state of function <function> from file <filename>"
      let default = Kernel_function.Map.empty
    end)
let () = add_correctness_dep SaveFunctionState.parameter
let () = add_correctness_dep LoadFunctionState.parameter
(* checks that SaveFunctionState has a unique argument pair, and returns it. *)
let get_SaveFunctionState () =
  let is_first = ref true in
  let (kf, filename) = SaveFunctionState.fold
      (fun (kf, opt_filename) _acc ->
         if !is_first then is_first := false
         else abort "option `%s' requires a single function:filename pair"
             SaveFunctionState.name;
         let filename = Extlib.the opt_filename in
         kf, filename
      ) (Kernel_function.dummy (), "")
  in
  if filename = "" then abort "option `%s' requires a function:filename pair"
      SaveFunctionState.name
  else kf, filename
(* checks that LoadFunctionState has a unique argument pair, and returns it. *)
let get_LoadFunctionState () =
  let is_first = ref true in
  let (kf, filename) = LoadFunctionState.fold
      (fun (kf, opt_filename) _acc ->
         if !is_first then is_first := false
         else abort "option `%s' requires a single function:filename pair"
             LoadFunctionState.name;
         let filename = Extlib.the opt_filename in
         kf, filename
      ) (Kernel_function.dummy (), "")
  in
  if filename = "" then abort "option `%s' requires a function:filename pair"
      LoadFunctionState.name
  else kf, filename
(* perform early sanity checks to avoid aborting the analysis only at the end *)
let () = Ast.apply_after_computed (fun _ ->
    (* check the function to save returns 'void' *)
    if SaveFunctionState.is_set () then begin
      let (kf, _) = get_SaveFunctionState () in
      if not (Kernel_function.returns_void kf) then
        abort "option `%s': function `%a' must return void"
          SaveFunctionState.name Kernel_function.pretty kf
    end;
    if SaveFunctionState.is_set () && LoadFunctionState.is_set () then begin
      (* check that if both save and load are set, they do not specify the
         same function name (note: cannot compare using function ids) *)
      let (save_kf, _) = get_SaveFunctionState () in
      let (load_kf, _) = get_LoadFunctionState () in
      if Kernel_function.equal save_kf load_kf then
        abort "options `%s' and `%s' cannot save/load the same function `%a'"
          SaveFunctionState.name LoadFunctionState.name
          Kernel_function.pretty save_kf
    end;
    if LoadFunctionState.is_set () then
      let (kf, _) = get_LoadFunctionState () in
      BuiltinsOverrides.add (kf, Some "Frama_C_load_state");
  )

(* ------------------------------------------------------------------------- *)
(* --- Messages                                                          --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group messages
module ValShowProgress =
  False
    (struct
       let option_name = "-val-show-progress"
       let help = "Show progression messages during analysis"
     end)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.is_invisible ()
module ValShowInitialState =
  True
    (struct
       let option_name = "-val-show-initial-state"
       (* deprecated in Silicon *)
       let help = "[deprecated] Show initial state before analysis starts. \
                   This option has been replaced by \
                   -value-msg-key=[-]initial-state and has no effect anymore."
    end)
let () =
  ValShowInitialState.add_set_hook
    (fun _ new_ ->
       if new_ then
         Kernel.warning "@[Option -val-show-initial-state has no effect, \
                         it has been replaced by -value-msg-key=initial-state@]"
       else
         Kernel.warning "@[Option -no-val-show-initial-state has no effect, \
                         it has been replaced by -value-msg-key=-initial-state@]"
    )

let () = Parameter_customize.set_group messages
module ValShowPerf =
  False
    (struct
       let option_name = "-val-show-perf"
       let help = "Compute and shows a summary of the time spent analyzing function calls"
     end)

let () = Parameter_customize.set_group messages
module ValPerfFlamegraphs =
  String
    (struct
       let option_name = "-val-flamegraph"
       let help = "Dumps a summary of the time spent analyzing function calls \
                   in a format suitable for the Flamegraph tool \
                   (http://www.brendangregg.com/flamegraphs.html)"
       let arg_name = "file"
       let default = ""
     end)


let () = Parameter_customize.set_group messages
module ShowSlevel =
  Int
    (struct
       let option_name = "-val-show-slevel"
       let default = 100
       let arg_name = "n"
       let help = "Period for showing consumption of the alloted slevel during analysis"
     end)
let () = ShowSlevel.set_range ~min:1 ~max:max_int

let () = Parameter_customize.set_group messages
module PrintCallstacks =
  False
    (struct
       let option_name = "-val-print-callstacks"
       let help = "When printing a message, also show the current call stack"
     end)

let () = Parameter_customize.set_group messages
module AlarmsWarnings =
  True
    (struct
       let option_name = "-val-warn-on-alarms"
       let help = "if set, possible alarms are printed in the analysis log as \
                   warnings"
     end)

let () = Parameter_customize.set_group alarms
module WarnBuiltinOverride =
  True(struct
    let option_name = "-val-warn-builtin-override"
    let help = "Warn when EVA built-ins will override function definitions"
  end)
let () = add_correctness_dep WarnBuiltinOverride.parameter

(* ------------------------------------------------------------------------- *)
(* --- Interpreter mode                                                  --- *)
(* ------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group interpreter
module InterpreterMode =
  False
    (struct
       let option_name = "-val-interpreter-mode"
       let help = "Stop at first call to a library function, if main() has \
arguments, on undecided branches"
     end)

let () = Parameter_customize.set_group interpreter

module ObviouslyTerminatesFunctions =
  Fundec_set
    (struct
       let option_name = "-obviously-terminates-function"
       let arg_name = "f"
       let help = ""
     end)
let () = add_dep ObviouslyTerminatesFunctions.parameter

let () = Parameter_customize.set_group interpreter
module ObviouslyTerminatesAll =
  False
    (struct
       let option_name = "-obviously-terminates"
       let help = "undocumented. Among effects of this options are the same \
effects as -no-results"
     end)
let () = add_dep ObviouslyTerminatesAll.parameter

let () = Parameter_customize.set_group interpreter
module StopAtNthAlarm =
  Int(struct
         let option_name = "-val-stop-at-nth-alarm"
         let default = max_int
         let arg_name = "n"
         let help = "Aborts the analysis when the nth alarm is emitted."
       end)

(* -------------------------------------------------------------------------- *)
(* --- Ugliness required for correctness                                  --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.is_invisible ()
module InitialStateChanged =
  Int (struct
         let option_name = "-new-initial-state"
         let default = 0
         let arg_name = "n"
         let help = ""
       end)
(* Changing the user-supplied initial state (or the arguments of main) through
   the API of Db.Value does reset the state of Value, but *not* the property
   statuses that Value has positioned. Currently, statuses can only depend
   on a command-line parameter. We use the dummy one above to force a reset
   when needed. *)
let () =
  add_correctness_dep InitialStateChanged.parameter;
  Db.Value.initial_state_changed :=
    (fun () -> InitialStateChanged.set (InitialStateChanged.get () + 1))


(* -------------------------------------------------------------------------- *)
(* --- Eva options                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group precision_tuning
module EnumerateCond =
  Bool
    (struct
      let option_name = "-eva-enumerate-cond"
      let help = "Activate reduce_by_cond_enumerate."
      let default = true
    end)
let () = add_precision_dep EnumerateCond.parameter


let () = Parameter_customize.set_group precision_tuning
module OracleDepth =
  Int
    (struct
      let option_name = "-eva-oracle-depth"
      let help = "Maximum number of successive uses of the oracle by the domain \
                  for the evaluation of an expression. Set 0 to disable the \
                  oracle."
      let default = 2
      let arg_name = ""
    end)
let () = add_precision_dep OracleDepth.parameter

let () = Parameter_customize.set_group precision_tuning
module ReductionDepth =
  Int
    (struct
      let option_name = "-eva-reduction-depth"
      let help = "Maximum number of successive backward reductions that the \
                  domain may initiate."
      let default = 4
      let arg_name = ""
    end)
let () = add_precision_dep ReductionDepth.parameter


(* -------------------------------------------------------------------------- *)
(* --- Dynamic allocation                                                 --- *)
(* -------------------------------------------------------------------------- *)

let () = Parameter_customize.set_group malloc
module MallocFunctions=
  Filled_string_set
    (struct
      let option_name = "-val-malloc-functions"
      let arg_name = "f1,...,fn"
      let help = "The malloc builtins use the call site of malloc() to know \
                  where to create new bases. This detection does not work for \
                  custom allocators or wrappers on top of malloc, unless they \
                  are listed here. By default, only contains malloc."
      let default = Datatype.String.Set.singleton "malloc"
    end)

let () = Parameter_customize.set_group malloc
module MallocReturnsNull=
  True
    (struct
      let option_name = "-val-malloc-returns-null"
      let help = "The allocation built-ins are modeled as nondeterministically \
                  returning a null pointer"
     end)

let () = Parameter_customize.set_group malloc
module MallocLevel =
  Int
    (struct
      let option_name = "-val-mlevel"
      let default = 0
      let arg_name = "m"
      let help = "sets to [m] the number of precise dynamic allocation for any \
                  given callstack"
     end)


(* Message keys *)

let dkey_initial_state = register_category sdkey_initial_state 
let dkey_final_states = register_category sdkey_final_states
let dkey_alarm = register_category sdkey_alarm
let dkey_garbled_mix = register_category sdkey_garbled_mix
let dkey_pointer_comparison = register_category sdkey_pointer_comparison
let dkey_cvalue_domain = register_category sdkey_cvalue_domain


(* -------------------------------------------------------------------------- *)
(* --- Freeze parameters. MUST GO LAST                                    --- *)
(* -------------------------------------------------------------------------- *)

let parameters_correctness =
  Typed_parameter.Set.elements !parameters_correctness
let parameters_tuning =
  Typed_parameter.Set.elements !parameters_tuning
let parameters_abstractions =
  Typed_parameter.Set.elements !parameters_abstractions



(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
