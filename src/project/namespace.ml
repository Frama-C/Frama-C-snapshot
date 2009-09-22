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

(* $Id: namespace.ml,v 1.2 2008-10-06 09:24:35 uid568 Exp $ *)

module Make(X:sig end) = struct

  type t = string
  module S = Set.Make(String)
  exception AlreadyExists of string

  let names = ref S.empty
  let dont_check = ref S.empty

  let make s =
    assert ( (* do not check with -noassert *)
      if S.mem s !names && not (S.mem s !dont_check) then 
	raise (AlreadyExists s);
      names := S.add s !names;
      true);
    s

  let gen_extend s =
    dont_check := S.add s  !dont_check;
    s

  let extend s from = gen_extend (from ^ " " ^ s)

  let extend2 s from1 from2 =
    gen_extend ("(" ^ from1 ^ ", " ^ from2 ^ ") " ^ s)

  let extend3 s from1 from2 from3 =
    gen_extend ("(" ^ from1 ^ ", " ^ from2 ^ ", " ^ from3 ^ ") " ^ s)

  let get s = s

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
