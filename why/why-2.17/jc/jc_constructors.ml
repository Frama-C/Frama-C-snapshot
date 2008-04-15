(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_constructors.ml,v 1.26 2008/12/09 09:14:18 marche Exp $ *)

open Jc_env
open Jc_region
open Jc_ast 
open Jc_fenv

class positioned ~pos =
object
  method pos: Loc.position = 
    match pos with None -> Loc.dummy_position | Some pos -> pos 
end

class typed ~typ =
object
  method typ: jc_type = typ
end

class labeled ~label =
object
  val mutable llab: label option = label
  method label = llab
  method set_label lab = llab <- lab
end

class marked ~mark =
object
  method mark: string = mark
end

class regioned ~region =
object
  val mutable r: region = region
  method region = r
  method set_region x = r <- x
end

class identifier ?pos name = 
object
  inherit positioned pos
  method name: string = name
end

class ['a] node_positioned ?pos node = 
object
  inherit positioned pos
  method node: 'a = node
end

class ptype ?(pos = Loc.dummy_position) node = 
object
  inherit [ptype_node] node_positioned ~pos node
end

class pexpr ?(pos = Loc.dummy_position) node =
object
  inherit [pexpr_node] node_positioned ~pos node
end

class pexpr_with ?pos ?node e =
  let pos = match pos with None -> e#pos | Some pos -> pos in
  let node = match node with None -> e#node | Some node -> node in
  pexpr ~pos node

class nexpr ?(pos = Loc.dummy_position) ?label node =
object
  inherit labeled label
  inherit [nexpr_node] node_positioned ~pos node
end

class nexpr_with ?pos ?node e =
  let pos = match pos with None -> e#pos | Some pos -> pos in
  let node = match node with None -> e#node | Some node -> node in
  let llab = e#label in
  nexpr ~pos ~label:llab node

class pattern ?(pos = Loc.dummy_position) ~typ node =
object
  inherit typed typ
  inherit [pattern_node] node_positioned ~pos node
end

class pattern_with ?pos ?node ?typ p =
  let pos = match pos with None -> p#pos | Some pos -> pos in
  let node = match node with None -> p#node | Some node -> node in
  let typ = match typ with None -> p#typ | Some typ -> typ in
  pattern ~pos ~typ node

class term ?(pos = Loc.dummy_position) ~typ ?(mark="") ?label ?region node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit typed typ
  inherit regioned region
  inherit marked mark
  inherit labeled label
  inherit [term_node] node_positioned ~pos node
end

class term_with ?pos ?typ ?mark ?region ?node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let typ = match typ with None -> t#typ | Some typ -> typ in
  let mark = 
    match mark with None -> t#mark | Some mark -> mark 
  in
  let llab = t#label in
  let region = match region with None -> t#region | Some region -> region in
  let node = match node with None -> t#node | Some node -> node in
  term ~pos ~typ ~mark ?label:llab ~region node

class term_var ?(pos = Loc.dummy_position) ?(mark="") v =
  term ~pos ~typ:v.jc_var_info_type ~mark ~region:v.jc_var_info_region
    (JCTvar v)

class location ?(pos = Loc.dummy_position) ?label ?region node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit regioned region
  inherit labeled label
  inherit [location_node] node_positioned ~pos node
end

(* ignore argument's label *)
class location_with ?pos ?label ?region ~node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let region = match region with None -> t#region | Some region -> region in
  location ~pos ?label ~region node

class location_set ?(pos = Loc.dummy_position) ?label ?region node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit regioned region
  inherit labeled label
  inherit [location_set_node] node_positioned ~pos node
end

(* ignore argument's label *)
class location_set_with ?pos ?label ?region ~node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let region = match region with None -> t#region | Some region -> region in
  location_set ~pos ?label ~region node

class expr ?(pos = Loc.dummy_position) ~typ ?(mark="") ?region
  ?original_type node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit typed typ
  inherit regioned region
  inherit marked mark
  inherit [expr_node] node_positioned ~pos node
  method original_type = 
    match original_type with None -> typ | Some original_type -> original_type
end

class expr_with ?pos ?typ ?mark ?region ?node ?original_type e =
  let pos = match pos with None -> e#pos | Some pos -> pos in
  let typ = match typ with None -> e#typ | Some typ -> typ in
  let mark = 
    match mark with None -> e#mark | Some mark -> mark 
  in
  let region = match region with None -> e#region | Some region -> region in
  let node = match node with None -> e#node | Some node -> node in
  let original_type = match original_type with
    | None -> e#original_type
    | Some original_type -> original_type
  in
  expr ~pos ~typ ~mark ~region ~original_type node

class assertion ?(mark="") ?label ?(pos = Loc.dummy_position) node =
object
  inherit marked mark
  inherit labeled label
  inherit [assertion_node] node_positioned ~pos node
end

class assertion_with ?pos ?mark ?node a =
  let pos = match pos with None -> a#pos | Some pos -> pos in
  let mark = 
    match mark with None -> a#mark | Some mark -> mark 
  in
  let llab = a#label in
  let node = match node with None -> a#node | Some node -> node in
  assertion ~pos ~mark ?label:llab node

class ['expr] ptag ?(pos = Loc.dummy_position) node =
object
  inherit ['expr ptag_node] node_positioned ~pos node
end

class ['expr] ptag_with ?pos ?node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let node = match node with None -> t#node | Some node -> node in
  ['expr] ptag ~pos node

class tag ?(pos = Loc.dummy_position) node =
object
  inherit [tag_node] node_positioned ~pos node
end

class tag_with ?pos ?node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let node = match node with None -> t#node | Some node -> node in
  tag ~pos node

class ['expr] decl ?(pos = Loc.dummy_position) node =
object
  inherit ['expr decl_node] node_positioned ~pos node
end

class ['expr] decl_with ?pos ?node t =
  let pos = match pos with None -> t#pos | Some pos -> pos in
  let node = match node with None -> t#node | Some node -> node in
  ['expr] decl ~pos node

(*******************************************************************************)
(*                             constant constructors                           *)
(*******************************************************************************)

(* These functions also exist in the other constructor modules such as PExpr. *)
module Const = struct
  let mkvoid = JCCvoid
  let mknull = JCCnull
  let mkboolean value = JCCboolean value
  let mkint ?value ?valuestr () =
    match value, valuestr with
      | Some value, None -> JCCinteger (string_of_int value)
      | None, Some valuestr -> JCCinteger valuestr
      | _ -> failwith "Jc_constructors.Const.mkint: use with ~value OR \
~valuestr only"
  let mkreal ?value ?valuestr () =
    match value, valuestr with
      | Some value, None -> JCCreal (string_of_float value)
      | None, Some valuestr -> JCCreal(valuestr)
      | _ -> failwith "Jc_constructors.Const.mkint: use with ~value OR \
~valuestr only"
end

(*******************************************************************************)
(*                               pexpr constructors                            *)
(*******************************************************************************)

let oo a b = match a with
  | None -> b
  | Some a -> a

module PExpr = struct
  let mk ?pos ~node () = new pexpr ?pos node

  let mkconst ~const = mk ~node:(JCPEconst const)
  let mkvoid = mkconst ~const:Const.mkvoid
  let mknull = mkconst ~const:Const.mknull
  let mkboolean ~value = mkconst ~const:(Const.mkboolean value)
  let mkint ?value ?valuestr = mkconst ~const:(Const.mkint ?value ?valuestr ())
  let mkreal ?value ?valuestr = mkconst ~const:(Const.mkreal ?value ?valuestr ())

  let mkbinary ~expr1 ~op ~expr2 = mk ~node:(JCPEbinary(expr1, op, expr2))
  let mkbinary_list ~default ~op ?expr1 ?expr2 ?list ?pos
      () =
    match expr1, expr2, list with
      | None, None, Some el ->
          List.fold_left
            (fun expr2 expr1 -> mkbinary ?pos ~expr1 ~expr2 ~op ())
            default el
      | Some expr1, Some expr2, None -> mkbinary ~expr1 ~op ~expr2 ?pos ()
      | _ -> failwith "Jc_constructors.PExpr.mkbinary_list should be used \
either with (~expr1 AND ~expr2) OR ~list only."
  let mkand = mkbinary_list ~default:(mkboolean ~value:true ()) ~op:`Bland
  let mkor = mkbinary_list ~default:(mkboolean ~value:false ()) ~op:`Blor
  let mkadd = mkbinary_list ~default:(mkint ~value:0 ()) ~op:`Badd

  let mklabel ~label ~expr = mk ~node:(JCPElabel(label, expr))
  let mkvar ~name = mk ~node:(JCPEvar name)
  let mkderef ~expr ~field = mk ~node:(JCPEderef(expr, field))
  let mkunary ~expr ~op = mk ~node:(JCPEunary(op, expr))
  let mkapp ~fun_name ?(labels = []) ?(args = []) =
    mk ~node:(JCPEapp(fun_name, labels, args))
  let mkassign ~location ~value ?field ?op =
    let location = match field with
      | None -> location
      | Some field -> mkderef ~expr:location ~field:field ()
    in
    match op with
      | None -> mk ~node:(JCPEassign(location, value))
      | Some op -> mk ~node:(JCPEassign_op(location, op, value))
  let mkinstanceof ~expr ~typ = mk ~node:(JCPEinstanceof(expr, typ))
  let mkcast ~expr ~typ = mk ~node:(JCPEcast(expr, typ))
  let mkquantifier ~quantifier ~typ ~vars ~body =
    mk ~node:(JCPEquantifier(quantifier, typ, vars, body))
  let mkforall = mkquantifier ~quantifier:Forall
  let mkexists = mkquantifier ~quantifier:Exists
  let mkold ~expr = mk ~node:(JCPEold expr)
  let mkat ~expr ~label = mk ~node:(JCPEat(expr, label))
  let mkoffset ~kind ~expr = mk ~node:(JCPEoffset(kind, expr))
  let mkoffset_min = mkoffset ~kind:Offset_min
  let mkoffset_max = mkoffset ~kind:Offset_max
  let mkif ~condition ~expr_then ?(expr_else = mkvoid ()) =
    mk ~node:(JCPEif(condition, expr_then, expr_else))
  let mkblock ~exprs = mk ~node:(JCPEblock exprs)
  let mkdecl ~typ ~var ?init = mk ~node:(JCPEdecl(typ, var, init))
  let mklet ?typ ~var ?init ~body ?pos () =
    match typ with
      | None -> mk ~node:(JCPElet(typ, var, init, body)) ?pos ()
      | Some typ ->
	  mkblock ~exprs:[
	    mkdecl ~typ ~var ?init ?pos ();
	    body
	  ] ?pos ()
  let mklet_nodecl ?typ ~var ?init ~body =
    mk ~node:(JCPElet(typ, var, init, body))
  let mkrange ?left ?right ?locations ?pos () =
    let r = mk ~node:(JCPErange(left, right)) ?pos () in
    match locations with
      | None -> r
      | Some l -> mkadd ~expr1:l ~expr2:r ?pos ()
  let mkalloc ?(count = mkint ~value:1 ()) ~typ =
    mk ~node:(JCPEalloc(count, typ))
  let mkfree ~expr = mk ~node:(JCPEfree expr)
  let mkmutable ~expr ~tag = mk ~node:(JCPEmutable(expr, tag))
  let mktag_equality ~tag1 ~tag2 = mk ~node:(JCPEeqtype(tag1, tag2))
  let mkmatch ~expr ~cases = mk ~node:(JCPEmatch(expr, cases))
  let mkassert ~expr = mk ~node:(JCPEassert ([],Aassert,expr))
  let mkwhile ?(condition = mkboolean ~value:true ())
      ?(invariant = []) ?variant ~body =
    mk ~node:(JCPEwhile(condition, invariant, variant, body))
  let mkfor ?(inits = []) ?(condition = mkboolean ~value:true ())
      ?(updates = []) ?(invariant = []) ?variant ~body =
    mk ~node:(JCPEfor(inits, condition, updates, invariant, variant, body))
  let mkreturn ?(expr = mkvoid ()) = mk ~node:(JCPEreturn expr)
  let mkbreak ?(label = "") = mk ~node:(JCPEbreak label)
  let mkcontinue ?(label = "") = mk ~node:(JCPEcontinue label)
  let mkgoto ~label = mk ~node:(JCPEgoto label)
  let mktry ~expr ?(catches = []) ?(finally = mkvoid ()) =
    mk ~node:(JCPEtry(expr, catches, finally))
  let mkthrow ~exn ?(argument = mkvoid ()) = mk ~node:(JCPEthrow(exn, argument))
  let mkpack ~expr ?tag = mk ~node:(JCPEpack(expr, tag))
  let mkunpack ~expr ?tag = mk ~node:(JCPEunpack(expr, tag))
  let mkswitch ~expr ?(cases = []) = mk ~node:(JCPEswitch(expr, cases))

  let mkcatch ~exn ?(name = "") ?body ?pos () =
    exn, name, (match body with None -> mkvoid ?pos () | Some body -> body)

  let mkshift ~expr ~offset = mkadd ~expr1:expr ~expr2:offset
  let mknot = mkunary ~op:`Unot
  let mkeq = mkbinary ~op:`Beq
  let mkimplies = mkbinary ~op:`Bimplies
  let mkiff = mkbinary ~op:`Biff
  let mkincr_heap ~expr ~field ?(op = `Upostfix_inc) =
    mkunary ~op ~expr:(mkderef ~expr ~field ())

  let mkcontract ~requires ~decreases ~behaviors ~expr =
    mk ~node:(JCPEcontract(requires, decreases, behaviors, expr))
end

module PDecl = struct
  open PExpr

  let mk ?pos ~node () =
    new node_positioned ?pos node
  let mkfun_def ?(result_type = new ptype (JCPTnative Tunit)) ~name
      ?(params = []) ?(clauses = []) ?body =
    mk ~node:(JCDfun(result_type, name, params, clauses, body))
  let mklemma_def ~name ?(axiom = false) ?(labels = []) ~body =
    mk ~node:(JCDlemma(name, axiom, labels, body))
  let mklogic_var_def ~typ ~name ?body =
    mk ~node:(JCDlogic_var(typ, name, body))
  let mklogic_def ?typ ~name ?(labels = []) ?(params = []) ?reads ?body ?inductive = 
  let roe = match reads, body, inductive with
      | None, None, None -> JCreads []
      | Some r, None, None -> JCreads r
      | None, Some b, None -> JCexpr b
      | None, None, Some l -> JCinductive l 
      | _ ->
          raise
            (Invalid_argument "mklogic_def: cannot use both ~reads and ~body and ~inductive")
    in
    mk ~node:(JCDlogic(typ, name, labels, params, roe))
      
  let mkaxiomatic ~name ~decls =
    mk ~node:(JCDaxiomatic(name,decls))
  let mklogic_type ~name =
    mk ~node:(JCDlogic_type(name))
  let mkvar_def ~typ ~name ?init =
    mk ~node:(JCDvar(typ, name, init))
  let mkglobal_inv_def ~name ~body =
    mk ~node:(JCDglobal_inv(name, body))
  let mktag_def ~name ?(params = []) ?super ?(fields = []) ?(invariants = []) =
    mk ~node:(JCDtag(name, params, super, fields, invariants))
  let mkenum_type_def ~name ~left ~right =
    mk ~node:(JCDenum_type(name, left, right))
  let mkexception_def ~name ?arg_type =
    mk ~node:(JCDexception(name, arg_type))
  let mkvariant_type_def ~name ?(tags = []) =
    mk ~node:(JCDvariant_type(name, tags))

  let mkinvariant_policy_def ~value = mk ~node:(JCDinvariant_policy value)
  let mkseparation_policy_def ~value = mk ~node:(JCDseparation_policy value)
  let mkannotation_policy_def ~value = mk ~node:(JCDannotation_policy value)
  let mkabstract_domain_def ~value = mk ~node:(JCDabstract_domain value)
  let mkint_model_def ~value = mk ~node:(JCDint_model value)

  let mkbehavior ?(pos = Loc.dummy_position) ~name ?throws ?assumes ?requires
      ?assigns ?(ensures = mkboolean ~value:true ()) () =
    (pos, name, throws, assumes, requires, assigns, ensures)

  let mkrequires_clause expr = JCCrequires expr

  let mkbehavior_clause ?(pos = Loc.dummy_position) ~name ?throws ?assumes ?requires
      ?assigns ?(ensures = mkboolean ~value:true ()) () =
      JCCbehavior (mkbehavior ~pos ~name ?throws ?assumes ?requires ?assigns ~ensures ())

  let mkbehavior_clause_with ?pos ?name ?throws ?assumes ?requires ?assigns ?ensures =
    function
      | JCCbehavior(pos', name', throws', assumes', requires', assigns',
                    ensures') ->
          JCCbehavior(
            oo pos pos',
            oo name name',
            oo throws throws',
            oo assumes assumes',
            oo requires requires',
            oo assigns assigns',
            oo ensures ensures'
          )
      | _ -> raise (Invalid_argument "mkbehavior_with")
  let mkassigns ?(pos = Loc.dummy_position) ?(locations = []) () =
    pos, locations

  let mktag_invariant ~name ~var ~body = name, var, body

  let behavior_ensures = function
    | JCCbehavior(_, _, _, _, _, _, e) -> e
    | _ -> raise (Invalid_argument "behavior_ensures")
end


(******************************************************************************)
(*                              nexpr constructors                            *)
(******************************************************************************)

module NExpr = struct
  let mk ?pos ~node () = new nexpr ?pos node

  let mkcast ~expr ~typ = mk ~node:(JCNEcast(expr, typ))
end


(*******************************************************************************)
(*                               expr constructors                             *)
(*******************************************************************************)

module Expr = struct
  let mk ?pos ~typ ?mark ?region ?original_type ~node () =
    new expr ?pos ~typ ?mark ?region ?original_type node

  let mkconst ~const = mk ~typ:(JCTnative Tinteger) ~node:(JCEconst const)
  let mkint ?value ?valuestr = mkconst ~const:(Const.mkint ?value ?valuestr ())
  let mkbinary ~expr1 ~op ~expr2 = mk ~node:(JCEbinary(expr1, op, expr2))

  let mklet ~var ?init ~body =
    mk ~typ:var.jc_var_info_type ~node:(JCElet(var, init, body))
  let mkvar ~var = 
    mk ~typ:var.jc_var_info_type ~node:(JCEvar var)

  let is_app e = match e#node with JCEapp _ -> true | _ -> false
end

(*******************************************************************************)
(*                               term constructors                             *)
(*******************************************************************************)

module Term = struct
  let mk ?pos ~typ ?mark ?region ~node () =
    new term ?pos ~typ ?mark ?region node

  let mkconst ~const = mk ~typ:(JCTnative Tinteger) ~node:(JCTconst const)
  let mkint ?value ?valuestr = mkconst ~const:(Const.mkint ?value ?valuestr ())
  let mkbinary ~term1 ~op ~term2 = mk ~node:(JCTbinary(term1, op, term2))

  let mkvar ~var = 
    mk ~typ:var.jc_var_info_type ~node:(JCTvar var)
end

(*******************************************************************************)
(*                           assertion constructors                            *)
(*******************************************************************************)

module Assertion = struct
  let mk ?pos ?mark ~node () =
    new assertion ?pos ?mark node

  let fake ?pos ?mark ~value () = value

  let is_true a = 
    match a#node with 
      | JCAtrue -> true
      | JCAbool_term t when t#node = JCTconst(JCCboolean true) -> true
      | _ -> false
    
  let is_false a =
    match a#node with 
      | JCAfalse -> true
      | JCAbool_term t when t#node = JCTconst(JCCboolean false) -> true
      | _ -> false

  let mktrue = mk ~node:JCAtrue
  let mkfalse = mk ~node:JCAfalse

  let mkand ~conjuncts =
    (* optimization *)
    let conjuncts = List.filter (fun a -> not (is_true a)) conjuncts in
    match conjuncts with
      | [] -> mktrue
      | [a] -> fake ~value:a
      | alist -> mk ~node:(JCAand alist)

  let mkor ~disjuncts = 
    (* optimization *)
    let disjuncts = List.filter (fun a -> not (is_false a)) disjuncts in
    match disjuncts with
      | [] -> mkfalse
      | [a] -> fake ~value:a
      | alist -> mk ~node:(JCAor alist)

  let mknot ~asrt:a = mk ~node:(JCAnot a)
end

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
