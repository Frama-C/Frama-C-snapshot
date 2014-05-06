(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* ************************************************************************* *)
(** {2 Slicing options} *)
(* ************************************************************************* *)

include Plugin.Register
  (struct
     let name = "slicing"
     let shortname = "slicing"
     let help = "code slicer"
   end)

module Select = struct

  module Calls =
    StringSet
      (struct
         let option_name = "-slice-calls"
         let arg_name = "f1, ..., fn"
         let help =
           "select every calls to functions f1,...,fn, and all their effect"
       end)

  module Return =
    StringSet
      (struct
         let option_name = "-slice-return"
         let arg_name = "f1, ..., fn"
         let help =
           "select the result (returned value) of functions f1,...,fn"
       end)

  module Threat =
    StringSet
      (struct
         let option_name = "-slice-threat"
         let arg_name = "f1, ..., fn"
         let help = "select the threats of functions f1,...,fn"
       end)

  module Assert =
    StringSet
      (struct
         let option_name = "-slice-assert"
         let arg_name = "f1, ..., fn"
         let help = "select the assertions of functions f1,...,fn"
       end)

  module LoopInv =
    StringSet
      (struct
         let option_name = "-slice-loop-inv"
         let arg_name = "f1, ..., fn"
         let help = "select the loop invariants of functions f1,...,fn"
       end)

  module LoopVar =
    StringSet
      (struct
         let option_name = "-slice-loop-var"
         let arg_name = "f1, ..., fn"
         let help = "select the loop variants of functions f1,...,fn"
       end)

  module Pragma =
    StringSet
      (struct
         let option_name = "-slice-pragma"
         let arg_name = "f1, ..., fn"
         let help =
           "use the slicing pragmas in the code of functions f1,...,fn as \
slicing criteria:\n\
//@ slice pragma ctrl;  to reach this control-flow point\n\
//@ slice pragma expr <expr_desc;>  to preserve the value of an expression at \
this control-flow point\n\
//@ slice pragma stmt;  to preserve the effect of the next statement"
         end)
    module RdAccess =
      StringSet
        (struct
           let option_name = "-slice-rd"
           let arg_name = "v1, ..., vn"
           let help =
             "select the read accesses to left-values v1,...,vn \
         (addresses are evaluated at the beginning of the function given as \
entry point)"
         end)
    module WrAccess =
      StringSet
        (struct
           let option_name = "-slice-wr"
           let arg_name = "v1, ..., vn"
           let help =
             "select the write accesses to left-values v1,...,vn \
         (addresses are evaluated at the beginning of the function given as\
 entry point)"
         end)
    module Value =
      StringSet
        (struct
           let option_name = "-slice-value"
           let arg_name = "v1, ..., vn"
           let help =
             "select the result of left-values v1,...,vn at the end of the \
function given as entry point (addresses are evaluated at the beginning of \
the function given as entry point)"
         end)
end

module Mode = struct

  module Callers =
    True(struct
           let option_name = "-slice-callers"
           let help = "propagate the slicing to the function callers"
         end)

  module Calls =
    Int
      (struct
         let option_name = "-slicing-level"
         let default = 2
         let arg_name = ""
         let help = "set the default level of slicing used to propagate to \
the calls\n\
        0 : don't slice the called functions\n\
        1 : don't slice the called functions but propagate the marks anyway\n\
        2 : try to use existing slices, create at most one\n\
        3 : most precise slices\n\
  note: this value (defaults to 2) is not used for calls to undefined \
functions\n\
        except when '-slice-undef-functions' option is set"
         end)
  let () = Calls.set_range ~min:0 ~max:3

  module SliceUndef =
    False(struct
            let option_name = "-slice-undef-functions"
            let help = "allow the use of the -slicing-level option for calls \
to undefined functions"
          end)

  module KeepAnnotations =
    False(struct
            let option_name = "-slicing-keep-annotations"
            let help = "keep annotations as long as the used variables are \
declared and the accessibility of the program point is preserved (even if the \
value of the data is not preserved)"
          end)
end

module ProjectName =
  String(struct
           let option_name = "-slicing-project-name"
           let arg_name = "ident"
           let help = "name of the slicing project (defaults to \"Slicing\").\
This name is used as basename when building the name of the exported project (see -slicing-exported-project-postfix option)"
           let default = "Slicing"
         end)

module ExportedProjectPostfix =
  String(struct
           let option_name = "-slicing-exported-project-postfix"
           let arg_name = "postfix"
           let help = "postfix added to the slicing project name for building \
the name of the exported project (defaults to \" export\")"
           let default = " export"
         end)

module Print = struct
  let new_command = "<normal slicing command> -then-on 'Slicing export' -print"
  include False(struct
    let option_name = "-slice-print"
    let help = "deprecated. Use instead " ^ new_command
  end)
  (* Just a small hack to inform the end-user that he is using a deprecated
     option without changing the old behavior (incompatible with -ocode for
     instance). *)
  let get () = 
    let b = get () in
    if b then deprecated "-slice-print" ~now:new_command (fun () -> ())	();
    b
end

module Force =
  True(struct
       let option_name = "-slice-force"
       let help = "force slicing"
     end)

module OptionModified =
  State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Slicing.OptionModified"
       let dependencies = []
       let default () = true
     end)

let () =
  State_dependency_graph.add_codependencies
    ~onto:OptionModified.self
    [ Select.Calls.self;
      Select.Return.self;
      Select.Threat.self;
      Select.Assert.self;
      Select.LoopInv.self;
      Select.LoopVar.self;
      Select.Pragma.self;
      Select.RdAccess.self;
      Select.WrAccess.self;
      Select.Value.self;
      Mode.Callers.self;
      Mode.Calls.self;
      Mode.SliceUndef.self;
      Mode.KeepAnnotations.self;
      Print.self ]

let is_on () =
  (Force.get () || OptionModified.get ())
  &&
    (not (Select.Calls.is_empty ()
          && Select.Return.is_empty ()
          && Select.Threat.is_empty ()
          && Select.Assert.is_empty ()
          && Select.LoopInv.is_empty ()
          && Select.LoopVar.is_empty ()
          && Select.Pragma.is_empty ()
          && Select.RdAccess.is_empty ()
          && Select.WrAccess.is_empty ()
          && Select.Value.is_empty ()))


let set_off () =
  Force.off () ;
  OptionModified.set false


let clear () =
  Force.clear () ;
  Select.Calls.clear () ;
  Select.Return.clear () ;
  Select.Threat.clear () ;
  Select.Assert.clear () ;
  Select.LoopInv.clear () ;
  Select.LoopVar.clear () ;
  Select.Pragma.clear () ;
  Select.RdAccess.clear () ;
  Select.WrAccess.clear () ;
  Select.Value.clear () ;
  OptionModified.clear ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
