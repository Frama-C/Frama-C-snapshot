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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jc_constructors.ml,v 1.3 2008/04/15 13:09:53 moy Exp $ *)

open Jc_env
open Jc_fenv
open Jc_region
open Jc_ast 

class located ~loc =
object
  method loc: Loc.position = 
    match loc with None -> Loc.dummy_position | Some loc -> loc 
end

class typed ~typ =
object
  method typ: jc_type = typ
end

class logic_labeled ~logic_label =
object
  val mutable llab: logic_label option = logic_label
  method logic_label = llab
  method set_logic_label lab = llab <- lab
end

class name_labeled ~name_label =
object
  method name_label: string = name_label
end

class regioned ~region =
object
  val mutable r: region = region
  method region = r
  method set_region x = r <- x
end

class identifier ?loc name = 
object
  inherit located loc
  method name: string = name
end

class ['a] node_located ?loc node = 
object
  inherit located loc
  method node: 'a = node
end

class ptype ?(loc = Loc.dummy_position) node = 
object
  inherit [ptype_node] node_located ~loc node
end

class pexpr ?(loc = Loc.dummy_position) node =
object
  inherit [pexpr_node] node_located ~loc node
end

class pexpr_with ?loc ?node e =
  let loc = match loc with None -> e#loc | Some loc -> loc in
  let node = match node with None -> e#node | Some node -> node in
  pexpr ~loc node

class nexpr ?(loc = Loc.dummy_position) ?logic_label node =
object
  inherit logic_labeled logic_label
  inherit [nexpr_node] node_located ~loc node
end

class nexpr_with ?loc ?node e =
  let loc = match loc with None -> e#loc | Some loc -> loc in
  let node = match node with None -> e#node | Some node -> node in
  let llab = e#logic_label in
  nexpr ~loc ~logic_label:llab node

class pattern ?(loc = Loc.dummy_position) ~typ node =
object
  inherit typed typ
  inherit [pattern_node] node_located ~loc node
end

class pattern_with ?loc ?node ?typ p =
  let loc = match loc with None -> p#loc | Some loc -> loc in
  let node = match node with None -> p#node | Some node -> node in
  let typ = match typ with None -> p#typ | Some typ -> typ in
  pattern ~loc ~typ node

class term ?(loc = Loc.dummy_position) ~typ ?(name_label="") ?region node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit typed typ
  inherit regioned region
  inherit name_labeled name_label
  inherit [term_node] node_located ~loc node
end

class term_with ?loc ?typ ?name_label ?region ?node t =
  let loc = match loc with None -> t#loc | Some loc -> loc in
  let typ = match typ with None -> t#typ | Some typ -> typ in
  let name_label = 
    match name_label with None -> t#name_label | Some name_label -> name_label 
  in
  let region = match region with None -> t#region | Some region -> region in
  let node = match node with None -> t#node | Some node -> node in
  term ~loc ~typ ~name_label ~region node

class term_var ?(loc = Loc.dummy_position) ?(name_label="") v =
  term ~loc ~typ:v.jc_var_info_type ~name_label ~region:v.jc_var_info_region
    (JCTvar v)

class expr ?(loc = Loc.dummy_position) ~typ ?(name_label="") ?region
  ?original_type node =
  let region = 
    match region with None -> dummy_region | Some region -> region 
  in
object
  inherit typed typ
  inherit regioned region
  inherit name_labeled name_label
  inherit [expr_node] node_located ~loc node
  method original_type = 
    match original_type with None -> typ | Some original_type -> original_type
end

class expr_with ?loc ?typ ?name_label ?region ?node ?original_type e =
  let loc = match loc with None -> e#loc | Some loc -> loc in
  let typ = match typ with None -> e#typ | Some typ -> typ in
  let name_label = 
    match name_label with None -> e#name_label | Some name_label -> name_label 
  in
  let region = match region with None -> e#region | Some region -> region in
  let node = match node with None -> e#node | Some node -> node in
  let original_type = match original_type with
    | None -> e#original_type
    | Some original_type -> original_type
  in
  expr ~loc ~typ ~name_label ~region ~original_type node

class assertion ?(name_label="") ?(loc = Loc.dummy_position) node =
object
  inherit name_labeled name_label
  inherit [assertion_node] node_located ~loc node
end

class assertion_with ?loc ?name_label ?node a =
  let loc = match loc with None -> a#loc | Some loc -> loc in
  let name_label = 
    match name_label with None -> a#name_label | Some name_label -> name_label 
  in
  let node = match node with None -> a#node | Some node -> node in
  assertion ~loc ~name_label node

class ['expr] ptag ?(loc = Loc.dummy_position) node =
object
  inherit ['expr ptag_node] node_located ~loc node
end

class ['expr] ptag_with ?loc ?node t =
  let loc = match loc with None -> t#loc | Some loc -> loc in
  let node = match node with None -> t#node | Some node -> node in
  ['expr] ptag ~loc node

class tag ?(loc = Loc.dummy_position) node =
object
  inherit [tag_node] node_located ~loc node
end

class tag_with ?loc ?node t =
  let loc = match loc with None -> t#loc | Some loc -> loc in
  let node = match node with None -> t#node | Some node -> node in
  tag ~loc node

class ['expr] decl ?(loc = Loc.dummy_position) node =
object
  inherit ['expr decl_node] node_located ~loc node
end

class ['expr] decl_with ?loc ?node t =
  let loc = match loc with None -> t#loc | Some loc -> loc in
  let node = match node with None -> t#node | Some node -> node in
  ['expr] decl ~loc node

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
      | None, Some valuestr -> JCCreal valuestr
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
  let mk ?loc ~node () = new pexpr ?loc node

  let mkconst ~const = mk ~node:(JCPEconst const)
  let mkvoid = mkconst ~const:Const.mkvoid
  let mknull = mkconst ~const:Const.mknull
  let mkboolean ~value = mkconst ~const:(Const.mkboolean value)
  let mkint ?value ?valuestr = mkconst ~const:(Const.mkint ?value ?valuestr ())
  let mkreal ?value ?valuestr = mkconst ~const:(Const.mkreal ?value ?valuestr ())

  let mkbinary ~expr1 ~op ~expr2 = mk ~node:(JCPEbinary(expr1, op, expr2))
  let mkbinary_list ~default ~op ?expr1 ?expr2 ?list ?loc
      () =
    match expr1, expr2, list with
      | None, None, Some el ->
          List.fold_left
            (fun expr2 expr1 -> mkbinary ?loc ~expr1 ~expr2 ~op ())
            default el
      | Some expr1, Some expr2, None -> mkbinary ~expr1 ~op ~expr2 ?loc ()
      | _ -> failwith "Jc_constructors.PExpr.mkbinary_list should be used \
either with (~expr1 AND ~expr2) OR ~list only."
  let mkand = mkbinary_list ~default:(mkboolean ~value:true ()) ~op:`Bland
  let mkor = mkbinary_list ~default:(mkboolean ~value:true ()) ~op:`Blor
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
  let mklet ?typ ~var ?init ~body ?loc () =
    match typ with
      | None -> mk ~node:(JCPElet(typ, var, init, body)) ?loc ()
      | Some typ ->
	  mkblock ~exprs:[
	    mkdecl ~typ ~var ?init ?loc ();
	    body
	  ] ?loc ()
  let mklet_nodecl ?typ ~var ?init ~body =
    mk ~node:(JCPElet(typ, var, init, body))
  let mkrange ?left ?right ?locations ?loc () =
    let r = mk ~node:(JCPErange(left, right)) ?loc () in
    match locations with
      | None -> r
      | Some l -> mkadd ~expr1:l ~expr2:r ?loc ()
  let mkalloc ?(count = mkint ~value:1 ()) ~typ =
    mk ~node:(JCPEalloc(count, typ))
  let mkfree ~expr = mk ~node:(JCPEfree expr)
  let mkmutable ~expr ~tag = mk ~node:(JCPEmutable(expr, tag))
  let mktag_equality ~tag1 ~tag2 = mk ~node:(JCPEtagequality(tag1, tag2))
  let mkmatch ~expr ~cases = mk ~node:(JCPEmatch(expr, cases))
  let mkassert ~expr = mk ~node:(JCPEassert expr)
  let mkwhile ?(condition = mkboolean ~value:true ())
      ?(invariant = mkboolean ~value:true ()) ?variant ~body =
    mk ~node:(JCPEwhile(condition, invariant, variant, body))
  let mkfor ?(inits = []) ?(condition = mkboolean ~value:true ())
      ?(updates = []) ?(invariant = mkboolean ~value:true ()) ?variant ~body =
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

  let mkcatch ~exn ?(name = "") ?body ?loc () =
    exn, name, (match body with None -> mkvoid ?loc () | Some body -> body)

  let mkshift ~expr ~offset = mkadd ~expr1:expr ~expr2:offset
  let mknot = mkunary ~op:`Unot
  let mkeq = mkbinary ~op:`Beq
  let mkimplies = mkbinary ~op:`Bimplies
  let mkiff = mkbinary ~op:`Biff
  let mkincr_heap ~expr ~field ?(op = `Upostfix_inc) =
    mkunary ~op ~expr:(mkderef ~expr ~field ())
end

module PDecl = struct
  open PExpr

  let mk ?loc ~node () =
    new node_located ?loc node
  let mkfun_def ?(result_type = new ptype (JCPTnative Tunit)) ~name
      ?(params = []) ?(clauses = []) ?body =
    mk ~node:(JCDfun(result_type, name, params, clauses, body))
  let mklemma_def ~name ?(axiom = false) ?(labels = []) ~body =
    mk ~node:(JCDlemma(name, axiom, labels, body))
  let mklogic_var_def ~typ ~name ?body =
    mk ~node:(JCDlogic_var(typ, name, body))
  let mklogic_def ?typ ~name ?(labels = []) ?(params = []) ?reads ?body =
    let roe = match reads, body with
      | Some r, None -> JCreads r
      | None, Some b -> JCexpr b
      | None, None -> JCreads []
      | Some _, Some _ ->
          raise
            (Invalid_argument "mklogic_def: cannot use both ~reads and ~body")
    in
    mk ~node:(JCDlogic(typ, name, labels, params, roe))
  let mkvar_def ~typ ~name ?init =
    mk ~node:(JCDvar(typ, name, init))
  let mkglobal_inv_def ~name ~body =
    mk ~node:(JCDglobal_inv(name, body))
  let mktag_def ~name ?super ?(fields = []) ?(invariants = []) =
    mk ~node:(JCDtag(name, super, fields, invariants))
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

  let mkrequires expr = JCCrequires expr
  let mkbehavior ?(loc = Loc.dummy_position) ~name ?throws ?assumes ?requires
      ?assigns ?(ensures = mkboolean ~value:true ()) () =
    JCCbehavior(loc, name, throws, assumes, requires, assigns, ensures)
  let mkbehavior_with ?loc ?name ?throws ?assumes ?requires ?assigns ?ensures =
    function
      | JCCbehavior(loc', name', throws', assumes', requires', assigns',
                    ensures') ->
          JCCbehavior(
            oo loc loc',
            oo name name',
            oo throws throws',
            oo assumes assumes',
            oo requires requires',
            oo assigns assigns',
            oo ensures ensures'
          )
      | _ -> raise (Invalid_argument "mkbehavior_with")
  let mkassigns ?(loc = Loc.dummy_position) ?(locations = []) () =
    loc, locations

  let mktag_invariant ~name ~var ~body = name, var, body

  let behavior_ensures = function
    | JCCbehavior(_, _, _, _, _, _, e) -> e
    | _ -> raise (Invalid_argument "behavior_ensures")
end

(*******************************************************************************)
(*                               expr constructors                             *)
(*******************************************************************************)

module Expr = struct
  let mk ?loc ~typ ?name_label ?region ?original_type ~node () =
    new expr ?loc ~typ ?name_label ?region ?original_type node

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
  let mk ?loc ~typ ?name_label ?region ~node () =
    new term ?loc ~typ ?name_label ?region node

  let mkvar ~var = 
    mk ~typ:var.jc_var_info_type ~node:(JCTvar var)
end

(*******************************************************************************)
(*                           assertion constructors                            *)
(*******************************************************************************)

module Assertion = struct
  let mk ?loc ?name_label ~node () =
    new assertion ?loc ?name_label node

  let fake ?loc ?name_label ~value () = value

  let mktrue = mk ~node:JCAtrue
  let mkfalse = mk ~node:JCAfalse
  let mkand ~conjuncts =
    match conjuncts with
      | [] -> mktrue
      | [a] -> fake ~value:a
      | alist -> mk ~node:(JCAand alist)
end

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
