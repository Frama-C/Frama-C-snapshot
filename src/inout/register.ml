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

(* This code duplicates more or less Option_signature.With_output. Since Inout prints
   the results of all its options interleaved, it is difficult to proceed
   otherwise *)
module ShouldOutput =
  State_builder.True_ref
    (struct
      let dependencies = [Db.Value.self] (* To be completed if some computations
                                          use some other results than value *)
      let name = "Inout.Register.ShouldOuput"
     end)
let () = Inout_parameters.Output.add_set_hook
  (fun _ v -> if v then ShouldOutput.set true)


let main () =
  let forceout = Inout_parameters.ForceOut.get () in
  let forceexternalout = Inout_parameters.ForceExternalOut.get () in
  let forceinput = Inout_parameters.ForceInput.get () in
  let forceinout = Inout_parameters.ForceInout.get () in
  let forceinoutwithformals =
    Inout_parameters.ForceInoutExternalWithFormals.get ()
  in
  let forcederef = Inout_parameters.ForceDeref.get () in
  let forceinputwithformals = Inout_parameters.ForceInputWithFormals.get () in
  if (forceout || forceexternalout || forceinput || forceinputwithformals
      || forcederef || forceinout || forceinoutwithformals) &&
    Inout_parameters.Output.get () && ShouldOutput.get ()
  then begin
    ShouldOutput.set false;
    !Db.Semantic_Callgraph.topologically_iter_on_functions
      (fun kf ->
         if Kernel_function.is_definition kf
         then begin
           if forceout
           then Inout_parameters.result "%a" Outputs.pretty_internal kf ;
           if forceexternalout
           then Inout_parameters.result "%a" Outputs.pretty_external kf ;
           if forceinput
           then Inout_parameters.result "%a" Inputs.pretty_external kf;
           if forcederef then begin
             Derefs.compute_external kf;
             Inout_parameters.result "%a" Derefs.pretty_external kf;
           end;
           if forceinout then
             Inout_parameters.result "%a"
               Operational_inputs.pretty_operational_inputs_internal kf;
           if forceinoutwithformals then
             Inout_parameters.result "%a"
               Operational_inputs.pretty_operational_inputs_external_with_formals kf;
           if forceinputwithformals
           then
             Inout_parameters.result "%a"
               Inputs.pretty_with_formals kf ;
         end)
  end

let () = Db.Main.extend main


(*
Local Variables:
compile-command: "make -C ../.. -j"
End:
*)
