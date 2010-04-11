(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Substitute all its internal varinfo from removel by the associated exp from addl.
    Parse a cil expression of type exp_node that have to be a Lval.
*)
let rec lval_substitution (lv:exp_node) (removel:string list) (addl:Cil_types.exp_node list) =
  match lv with
    | Lval(lh,off) -> 
	begin
	  match lh with  
	    | Mem _ as t-> Lval(t,off)
	    | Var vi -> 
		match removel, addl with
		  | [], [] -> lv
		  | r::rl,a::al -> 
		      if(String.compare  vi.vname r)=0 
		      then a 
		      else lval_substitution lv rl al
		  | _,_ -> Aorai_option.fatal "removel and addl parameters must have the same size."

	end
    | _ -> Aorai_option.fatal "lval_substitution have to be called with a Lval parameter"


(** Substitute all its internal varinfo from removel by the associated varinfo from addl.
    Parse a cil expression of type exp.
*)
let rec exp_substitution (e:exp) (removel:string list) (addl:Cil_types.exp_node list) = 
  {
    eid = e.eid; 	
    enode = exp_node_substitution e.enode removel addl ;
  } 


(** Substitute all its internal varinfo from removel by the associated expression from addl.
    Parse a cil expression of type exp_node.
*)
and exp_node_substitution (exp:exp_node) (removel:string list) (addl:Cil_types.exp_node list) = 
  match exp with 
    | Const _  as t -> t
    | Lval _ as t -> lval_substitution t removel addl

    | SizeOf _ 
    | SizeOfE _ 
    | SizeOfStr _ as t -> t

    | AlignOf _ 
    | AlignOfE _  as t -> t

    | UnOp (op,exp,typ) -> UnOp (op , exp_substitution exp removel addl , typ)
    | BinOp (op, exp1,exp2,typ) -> BinOp (op, 
					  exp_substitution exp1 removel addl,
					  exp_substitution exp2 removel addl,
					  typ)

    | CastE (typ,exp) ->  CastE (typ,exp_substitution exp removel addl)

    | AddrOf _  as t -> t
    | StartOf _  as t -> t

    | Info (exp,exp_info) -> Info (exp_substitution exp removel addl,exp_info)





(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
