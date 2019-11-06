(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

type scope =
  | Global
  | Function
  | Block

module H = Datatype.String.Hashtbl
let tbl = H.create 7
let globals = H.create 7

let get ~scope s =
  let _, u =
    Extlib.make_unique_name
      (fun s -> H.mem tbl s || H.mem globals s)
      ~sep:"_"
      s
  in
  let add = match scope with
    | Global -> H.add globals
    | Function | Block -> H.add tbl
  in
  add u ();
  u

let clear_locals () = H.clear tbl

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
