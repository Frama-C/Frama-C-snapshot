(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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
    eloc = e.eloc;
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









let rec term_lval_substitution ((host,offset):term_lval) (removel:string list) (addl:Cil_types.term_lhost list) =
  match host with
    | TVar(lv) ->
        let res= ref (host,offset) in
        List.iter2
          (fun r a -> if (String.compare r lv.lv_name)=0 then res:= (a,offset) )
          removel
          addl;
        !res
    | _  -> (host,offset)


and term_substitution (t:term) (removel:string list) (addl:Cil_types.term_lhost list) =
  let node = match t.term_node with
    |   TLval (tl)                   -> TLval          (term_lval_substitution tl removel addl)


    |   TSizeOfE t                   -> TSizeOfE       (term_substitution t removel addl)
    |   TAlignOfE t                  -> TAlignOfE      (term_substitution t removel addl)
    |   TUnOp (o,t)                  -> TUnOp          (o,term_substitution t removel addl)
    |   TBinOp (o,t1,t2)             -> TBinOp         (o,
                                                        term_substitution t1 removel addl,
                                                        term_substitution t2 removel addl)
    |   TCastE (ty,t)                -> TCastE         (ty,term_substitution t removel addl)
    |   TAddrOf tl                   -> TAddrOf        (term_lval_substitution tl removel addl)
    |   TStartOf tl                  -> TStartOf       (term_lval_substitution tl removel addl)
    |   Tapp (li,lll,tl)             -> Tapp           (li,lll,term_list_substitution tl removel addl)
    |   Tlambda(q,t)                 -> Tlambda        (q,term_substitution t removel addl)
    |   TDataCons (l,tl)             -> TDataCons      (l,term_list_substitution tl removel addl)
    |   Tif (t1,t2,t3)               -> Tif            (term_substitution t1 removel addl,
                                                        term_substitution t2 removel addl,
                                                        term_substitution t3 removel addl)
    |   Tat (t,ll)                   -> Tat            (term_substitution t removel addl,ll)
    |   Tbase_addr (t)               -> Tbase_addr     (term_substitution t removel addl)
    |   Tblock_length (t)            -> Tblock_length  (term_substitution t removel addl)
    |   TCoerce (t,ty)               -> TCoerce        (term_substitution t removel addl,ty)
    |   TCoerceE (t1,t2)             -> TCoerceE       (term_substitution t1 removel addl,
                                                        term_substitution t2 removel addl)
    |   TUpdate (t1,fi,t2)           -> TUpdate        (term_substitution t1 removel addl,
                                                        fi,
                                                        term_substitution t2 removel addl)
    |   Ttypeof (t)                  -> Ttypeof        (term_substitution t removel addl)
    |   Tunion (tl)                  -> Tunion         (term_list_substitution tl removel addl)
    |   Tinter (tl)                  -> Tinter         (term_list_substitution tl removel addl)
    |   Tcomprehension (t,q,Some(p)) -> Tcomprehension (term_substitution t removel addl,
                                                        q,
                                                        Some(named_predicate_substitution p removel addl))
    |   Tcomprehension (t,q,None)    -> Tcomprehension (term_substitution t removel addl,
                                                        q,
                                                        None)
    |   Trange (Some(t),None)        -> Trange         (Some(term_substitution t removel addl),
                                                        None)
    |   Trange (None,Some(t))        -> Trange         (None,
                                                        Some(term_substitution t removel addl))
    |   Trange (None,None)           -> Trange         (None,None)
    |   Trange (Some(t1),Some(t2))   -> Trange         (Some(term_substitution t1 removel addl),
                                                        Some(term_substitution t2 removel addl))

    |   Tlet (li,t)                  -> Tlet           (li,term_substitution t removel addl)


    |   TConst _
    |   TSizeOfStr _
    |   TAlignOf _
    |   Tnull
    |   Ttype _
    |   Tempty_set
    |   TSizeOf _ as c               -> c

  in
  {term_node=node ; term_loc=t.term_loc ; term_type=t.term_type ; term_name=t.term_name}


and term_list_substitution (tl:term list) (removel:string list) (addl:Cil_types.term_lhost list) =
  List.fold_left
    (fun lt t -> (term_substitution t removel addl)::lt)
    []
    tl


and named_predicate_substitution (npred:predicate named) (removel:string list) (addl:Cil_types.term_lhost list) =
  { name    = npred.name ;
    loc     = npred.loc ;
    content = predicate_substitution npred.content removel addl}


and predicate_substitution (pred:predicate) (removel:string list) (addl:Cil_types.term_lhost list) =
  match pred with
    |   Pfalse
    |   Ptrue as p -> p

    |   Papp (li, l, term_list) -> Papp         (li, l,(term_list_substitution term_list removel addl))
    |   Pseparated term_list    -> Pseparated   (term_list_substitution term_list removel addl)
    |   Prel (relation,t1,t2)   -> Prel         (relation,
                                                 term_substitution t1 removel addl,
                                                 term_substitution t2 removel addl)

    |   Pand     (p1,p2)        -> Pand         (named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)
    |   Por      (p1,p2)        -> Por          (named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)
    |   Pxor     (p1,p2)        -> Pxor         (named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)
    |   Pimplies (p1,p2)        -> Pimplies     (named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)
    |   Piff     (p1,p2)        -> Piff         (named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)

    |   Pnot (p)                -> Pnot         (named_predicate_substitution p removel addl)

    |   Pif (t,p1,p2)           -> Pif          (term_substitution t removel addl,
                                                 named_predicate_substitution p1 removel addl,
                                                 named_predicate_substitution p2 removel addl)


    |   Plet (li,p)             -> Plet         (li,
                                                 named_predicate_substitution p removel addl)
    |   Pforall (q,p)           -> Pforall      (q,
                                                 named_predicate_substitution p removel addl)
    |   Pexists (q,p)           -> Pexists      (q,
                                                 named_predicate_substitution p removel addl)
    |   Pat (p,l)               -> Pat          (named_predicate_substitution p removel addl,l)
    |   Pvalid (t)              -> Pvalid       (term_substitution t removel addl)
    |   Pvalid_index (t1,t2)    -> Pvalid_index (term_substitution t1 removel addl,
                                                 term_substitution t2 removel addl)
    |   Pvalid_range (t1,t2,t3) -> Pvalid_range (term_substitution t1 removel addl,
                                                 term_substitution t2 removel addl,
                                                 term_substitution t3 removel addl)

    |   Pfresh (t)              -> Pfresh       (term_substitution t removel addl)
    |   Pinitialized (t)        -> Pinitialized (term_substitution t removel addl)
    |   Psubtype (t1,t2)        -> Psubtype     (term_substitution t1 removel addl,
                                                 term_substitution t2 removel addl)












(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
