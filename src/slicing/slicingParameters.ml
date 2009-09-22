(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
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
     let descr = "code slicer"
   end)

module Select = struct

  module Calls =
    StringSet
      (struct
	 let option_name = "-slice-calls"
	 let arg_name = "f1, ..., fn"
	 let descr =
	   "select every calls to functions f1,...,fn, and all their effect"
       end)

  module Return =
    StringSet
      (struct
	 let option_name = "-slice-return"
	 let arg_name = "f1, ..., fn"
	 let descr =
	   "select the result (returned value) of functions f1,...,fn"
       end)

  module Threat =
    StringSet
      (struct
	 let option_name = "-slice-threat"
	 let arg_name = "f1, ..., fn"
	 let descr = "select the threats of functions f1,...,fn"
       end)

  module Assert =
    StringSet
      (struct
	 let option_name = "-slice-assert"
	 let arg_name = "f1, ..., fn"
	 let descr = "select the assertions of functions f1,...,fn"
       end)

  module LoopInv =
    StringSet
      (struct
	 let option_name = "-slice-loop-inv"
	 let arg_name = "f1, ..., fn"
	 let descr = "select the loop invariants of functions f1,...,fn"
       end)

  module LoopVar =
    StringSet
      (struct
	 let option_name = "-slice-loop-var"
	 let arg_name = "f1, ..., fn"
	 let descr = "select the loop variants of functions f1,...,fn"
       end)

  module Pragma =
    StringSet
      (struct
	 let option_name = "-slice-pragma"
	 let arg_name = "f1, ..., fn"
	 let descr =
	   "use the slicing pragmas in the code of functions f1,...,fn as \
slicing criteria"
	 let () = Plugin.set_optional_descr
"@;<0 0>@[<hov>@[<hov 4>//@@slice pragma ctrl; @ to@ reach@ this@ \
control-flow@ point@]@\n\
@[<hov 4>//@@slice pragma expr <expr_desc;> @ to@ preserve@ the@ value@ of@ \
an@ expression@ at@ this@ control-flow@ point@]@\n\
@[<hov 4>//@@slice pragma stmt; @ to@ preserve@ the@ effect@ of@ the@ next@ \
statement@]@]"
	 end)
    module RdAccess =
      StringSet
	(struct
	   let module_name = "Slicing.Select.RdAccess"
	   let option_name = "-slice-rd"
	   let arg_name = "v1, ..., vn"
	   let descr =
	     "select the read accesses to left-values v1,...,vn
	 (addresses are evaluated at the beginning of the function given as entry point)"
	 end)
    module WrAccess =
      StringSet
	(struct
	   let module_name = "Slicing.Select.WrAccess"
	   let option_name = "-slice-wr"
	   let arg_name = "v1, ..., vn"
	   let descr =
	     "select the write accesses to left-values v1,...,vn
	 (addresses are evaluated at the beginning of the function given as entry point)"
	 end)
    module Value =
      StringSet
	(struct
	   let module_name = "Slicing.Select.Value"
	   let option_name = "-slice-value"
	   let arg_name = "v1, ..., vn"
	   let descr =
	     "select the result of left-values v1,...,vn at the end of the function given as entry point
	 (addresses are evaluated at the beginning of the function given as entry point)"
	 end)
end

module Mode = struct

  module Callers =
    True(struct
	   let option_name = "-slice-callers"
	   let descr = "propagate the slicing to the function callers"
	 end)

  module Calls =  
    Int
      (struct
	 let option_name = "-slicing-level"
	 let default = 2
	 let arg_name = ""
	 let descr = "set the default level of slicing used to propagate to the calls
	0 : don't slice the called functions
	1 : don't slice the called functions but propagate the marks anyway
	2 : try to use existing slices, create at most one
	3 : most precise slices
  note: this value (defaults to 2) is not used for calls to undefined functions
	except when '-slice-undef-functions' option is set"
	 end)
  let () = Calls.set_range ~min:0 ~max:3
      
  module SliceUndef =
    False(struct
	    let option_name = "-slice-undef-functions"
	    let descr = "allow the use of the -slicing-level option for calls to undefined functions"
	  end)

  module KeepAnnotations =
    False(struct
	    let option_name = "-slicing-keep-annotations"
	    let descr = "keep annotations as long as the used variables are declared and the accessibility of the program point is preserved (even if the value of the data is not preserved)"
	  end)
end

module Print =
  False(struct
	  let option_name = "-slice-print"
	  let descr = "pretty print the sliced code"
	end)

let is_on () =
  not (Select.Calls.is_empty ()
       && Select.Return.is_empty ()
       && Select.Threat.is_empty ()
       && Select.Assert.is_empty ()
       && Select.LoopInv.is_empty ()
       && Select.LoopVar.is_empty ()
       && Select.Pragma.is_empty ()
       && Select.RdAccess.is_empty ()
       && Select.WrAccess.is_empty ()
       && Select.Value.is_empty ())

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
