(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(* $Id: widen_type.mli,v 1.15 2008/10/03 13:09:17 uid568 Exp $ *)
open BaseUtils
  
type t
  
type widen_hint = Ival.Widen_Hints.t
    
(** Key for the first map : from Base.t to Ival.Widen_Hints.t *)
type var_key = Default | All | VarKey of Base.t

(** Key for the second map : from stmt to the first map *)
type stmt_key = Cil_types.stmt option
 
(** an [empty] set of hints *)
val empty : t
  
(** a [default] set of hints *)
val default : t
  
(** add a set of hints for a [stmt, var], [Default] or [All] (stmts, keys) *)
val add_num_hints : stmt_key -> var_key -> widen_hint -> t -> t
  
(** add a set of Base for a [stmt] *)
val add_var_hints : Cil_types.stmt -> BaseSet.t -> t -> t
  
(** widen hints from a [Cil_types.stmt, Base] *)
val hints_from_keys : Cil_types.stmt -> t -> (BaseSet.t * (Base.t -> Locations.Location_Bytes.widen_hint))

module Datatype : Project.Datatype.S with type t = t
