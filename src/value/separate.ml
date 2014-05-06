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

let mask = ref 0

let prologue () =
  let sep_of = Value_parameters.SeparateStmtOf.get() in
  if sep_of <> 0
  then begin
      let sep_case = Value_parameters.SeparateStmtWord.get() in
      Value_parameters.feedback "Part of a case analysis: %d of 0..%d"
        sep_case
        sep_of;
      assert (sep_of >= 1);
      assert (sep_of <= 1073741823); (* should be enough for anyone *)
      assert (sep_of land (succ sep_of) = 0); (* pred of power of two *)
      assert (sep_case >= 0);
      assert (sep_case <= sep_of);

      mask := (succ sep_of) lsr 1;
    end
  else begin
      mask := 0;
    end

let filter_if stmt (th, el as thel) =
  if th = Dataflow2.GUnreachable || el = Dataflow2.GUnreachable
  then thel
  else
    let sep = !mask in
    if sep <> 0 &&
      (      Value_parameters.SeparateStmtStart.is_empty() ||
          (Value_parameters.SeparateStmtStart.exists
              (fun s -> stmt.Cil_types.sid = int_of_string s)) )
    then begin
        mask := sep lsr 1;
        let c =
          (Value_parameters.SeparateStmtWord.get()) land sep <> 0
        in
        Value_parameters.warning ~current:true
          "Statement %d: only propagating for condition %B"
          stmt.Cil_types.sid
          c;
        if c
        then
          th, Dataflow2.GUnreachable
        else
          Dataflow2.GUnreachable, el
      end
    else thel

let epilogue () =
  let sep = !mask in
  let word1 = Value_parameters.SeparateStmtWord.get() in
  let next =
    if sep <> 0
    then begin
        let unimportant = sep lor pred sep in
        let important = lnot unimportant in
        let c = word1 in
        let mn = c land important in
        let mx = c lor unimportant in
        let next = succ mx in
        Value_parameters.feedback "This analysis covers cases %d to %d" mn mx;
        next
    end
  else
    succ word1
  in
  if next <= Value_parameters.SeparateStmtOf.get()
  then
    Value_parameters.feedback "Next case to cover in sequential order: %d"
      next;
