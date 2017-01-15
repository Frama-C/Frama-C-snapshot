(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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



(* Default behaviour: print one alarm per kinstr. *)
module Alarm_key = Datatype.Pair_with_collections
  (Cil_datatype.Kinstr)(Alarms)(struct
      let module_name = "Alarm_key"
  end);;
module Alarm_cache = State_builder.Hashtbl(Alarm_key.Hashtbl)(Datatype.Unit)(struct
  let name = "Value_messages.Alarm_cache"
  let dependencies = [Db.Value.self]
  let size = 35
end)

let loc ki = match ki with
  | Cil_types.Kglobal -> (* can occur in case of obscure bugs (already happened)
                            with wacky initializers. Module Initial_state of
                            value analysis correctly positions the loc *)
    Cil.CurrentLoc.get ()
  | Cil_types.Kstmt s -> Cil_datatype.Stmt.loc s

let default_alarm_report ki alarm str =
  Alarm_cache.memo (fun (_ki,_alarm) ->
      let loc = loc ki in
      Value_util.alarm_report ~source:(fst loc) "%s" str
    ) (ki,alarm)
;;

let new_alarm ki alarm _property _annot str =
  default_alarm_report ki alarm str

let warning x =
  Format.kfprintf (fun _fmt ->
      let str = Format.flush_str_formatter() in
      Kernel.warning ~once:true ~current:true "%s" str
    )
    Format.str_formatter x
;;

  
(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
