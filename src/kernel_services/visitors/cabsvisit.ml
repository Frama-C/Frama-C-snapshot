(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(* cabsvisit.ml *)
(* tree visitor and rewriter for cabs *)

open Cabs
open Cabshelper

open Cil

type nameKind =
    NVar                                (* Variable or function prototype
                                           name *)
  | NFun                                (* A function definition name *)
  | NField                              (* The name of a field *)
  | NType                               (* The name of a type *)

(* All visit methods are called in preorder! (but you can use
 * ChangeDoChildrenPost to change the order) *)
class type cabsVisitor = object
  method vexpr: expression -> expression visitAction   (* expressions *)
  method vinitexpr: init_expression -> init_expression visitAction
  method vstmt: statement -> statement list visitAction
  method vblock: block -> block visitAction
  method vvar: string -> string                  (* use of a variable
                                                        * names *)
  method vdef: definition -> definition list visitAction
  method vtypespec: typeSpecifier -> typeSpecifier visitAction
  method vdecltype: decl_type -> decl_type visitAction

      (* For each declaration we call vname *)
  method vname: nameKind -> specifier -> name -> name visitAction
  method vspec: specifier -> specifier visitAction     (* specifier *)
  method vattr: attribute -> attribute list visitAction

  method vEnterScope: unit -> unit
  method vExitScope: unit -> unit
end

(* a default visitor which does nothing to the tree *)
class nopCabsVisitor : cabsVisitor = object
  method vexpr (_e:expression) = DoChildren
  method vinitexpr (_e:init_expression) = DoChildren
  method vstmt (s: statement) =
    CurrentLoc.set (get_statementloc s);
    DoChildren
  method vblock (_b: block) = DoChildren
  method vvar (s: string) = s
  method vdef (d: definition) =
    CurrentLoc.set (get_definitionloc d);
    DoChildren
  method vtypespec (_ts: typeSpecifier) = DoChildren
  method vdecltype (_dt: decl_type) = DoChildren
  method vname _k (_s:specifier) (_n: name) = DoChildren
  method vspec (_s:specifier) = DoChildren
  method vattr (_a: attribute) = DoChildren

  method vEnterScope () = ()
  method vExitScope () = ()
end

let doVisit vis startvisit children node =
  Cil.doVisit vis vis (fun x -> x) startvisit children node

let doVisitList vis startvisit children node =
  Cil.doVisitList vis vis (fun x -> x) startvisit children node

let rec visitCabsTypeSpecifier (vis: cabsVisitor) (ts: typeSpecifier) =
  doVisit vis vis#vtypespec childrenTypeSpecifier ts

and childrenTypeSpecifier vis ts =
  let childrenFieldGroup input = match input with
    | FIELD (s, nel) ->
	let s' = visitCabsSpecifier vis s in
	let doOneField ((n, eo) as input) =
	  let n' = visitCabsName vis NField s' n in
	  let eo' =
            match eo with
		None -> None
              | Some e -> let e' = visitCabsExpression vis e in
		if e' != e then Some e' else eo
	  in
	  if n' != n || eo' != eo then (n', eo') else input
	in
	let nel' = mapNoCopy doOneField nel in
	if s' != s || nel' != nel then FIELD (s', nel') else input
    | TYPE_ANNOT _ -> input
  in
  match ts with
    Tstruct (n, Some fg, extraAttrs) ->
      (*(trace "sm" (dprintf "visiting struct %s\n" n));*)
      let fg' = mapNoCopy childrenFieldGroup fg in
      if fg' != fg then Tstruct( n, Some fg', extraAttrs) else ts
  | Tunion (n, Some fg, extraAttrs) ->
      let fg' = mapNoCopy childrenFieldGroup fg in
      if fg' != fg then Tunion( n, Some fg', extraAttrs) else ts
  | Tenum (n, Some ei, extraAttrs) ->
      let doOneEnumItem ((s, e, loc) as ei) =
        let e' = visitCabsExpression vis e in
        if e' != e then (s, e', loc) else ei
      in
      vis#vEnterScope ();
      let ei' = mapNoCopy doOneEnumItem ei in
      vis#vExitScope();
      if ei' != ei then Tenum( n, Some ei', extraAttrs) else ts
  | TtypeofE e ->
      let e' = visitCabsExpression vis e in
      if e' != e then TtypeofE e' else ts
  | TtypeofT (s, dt) ->
      let s' = visitCabsSpecifier vis s in
      let dt' = visitCabsDeclType vis false dt in
      if s != s' || dt != dt' then TtypeofT (s', dt') else ts
  | ts -> ts

and childrenSpecElem (vis: cabsVisitor) (se: spec_elem) : spec_elem =
  match se with
    SpecTypedef | SpecInline | SpecStorage _ | SpecPattern _ -> se
  | SpecCV _ -> se    (* cop out *)
  | SpecAttr a -> begin
      let al' = visitCabsAttribute vis a in
      match al' with
        [a''] when a'' == a -> se
      | [a''] -> SpecAttr a''
      | _ -> Kernel.fatal "childrenSpecElem: visitCabsAttribute returned a list"
  end
  | SpecType ts ->
      let ts' = visitCabsTypeSpecifier vis ts in
      if ts' != ts then SpecType ts' else se

and visitCabsSpecifier (vis: cabsVisitor) (s: specifier) : specifier =
  doVisit vis vis#vspec childrenSpec s
and childrenSpec vis s = mapNoCopy (childrenSpecElem vis) s


and visitCabsDeclType vis (isfundef: bool) (dt: decl_type) : decl_type =
  doVisit vis vis#vdecltype (childrenDeclType isfundef) dt
and childrenDeclType isfundef vis dt =
  match dt with
    JUSTBASE -> dt
  | PARENTYPE (prea, dt1, posta) ->
      let prea' = mapNoCopyList (visitCabsAttribute vis)  prea in
      let dt1' = visitCabsDeclType vis isfundef dt1 in
      let posta'= mapNoCopyList (visitCabsAttribute vis)  posta in
      if prea' != prea || dt1' != dt1 || posta' != posta then
        PARENTYPE (prea', dt1', posta') else dt
  | ARRAY (dt1, al, e) ->
      let dt1' = visitCabsDeclType vis isfundef dt1 in
      let al' = mapNoCopy (childrenAttribute vis) al in
      let e'= visitCabsExpression vis e in
      if dt1' != dt1 || al' != al || e' != e then ARRAY(dt1', al', e') else dt
  | PTR (al, dt1) ->
      let al' = mapNoCopy (childrenAttribute vis) al in
      let dt1' = visitCabsDeclType vis isfundef dt1 in
      if al' != al || dt1' != dt1 then PTR(al', dt1') else dt
  | PROTO (dt1, snl, gsnl, b) ->
      (* Do not propagate isfundef further *)
      let dt1' = visitCabsDeclType vis false dt1 in
      let _ = vis#vEnterScope () in
      let snl' = mapNoCopy (childrenSingleName vis NVar) snl in
      let gsnl' = mapNoCopy (childrenSingleName vis NVar) gsnl in
      (* Exit the scope only if not in a function definition *)
      let _ = if not isfundef then vis#vExitScope () in
      if dt1' != dt1 || snl' != snl || gsnl' != gsnl then
        PROTO(dt1', snl', gsnl' , b)
      else dt


and childrenNameGroup vis (kind: nameKind) ((s, nl) as input) =
  let s' = visitCabsSpecifier vis s in
  let nl' = mapNoCopy (visitCabsName vis kind s') nl in
  if s' != s || nl' != nl then (s', nl') else input

and visitCabsName vis (k: nameKind) (s: specifier)
                      (n: name) : name =
  doVisit vis (vis#vname k s) (childrenName s k) n
and childrenName (_s: specifier) (k: nameKind) vis (n: name) : name =
  let (sn, dt, al, loc) = n in
  let dt' = visitCabsDeclType vis (k = NFun) dt in
  let al' = mapNoCopy (childrenAttribute vis) al in
  if dt' != dt || al' != al then (sn, dt', al', loc) else n

and childrenInitName vis (s: specifier) (inn: init_name) : init_name =
  let (n, ie) = inn in
  let n' = visitCabsName vis NVar s n in
  let ie' = visitCabsInitExpression vis ie in
  if n' != n || ie' != ie then (n', ie') else inn

and childrenSingleName vis (k: nameKind) (sn: single_name) : single_name =
  let s, n = sn in
  let s' = visitCabsSpecifier vis s in
  let n' = visitCabsName vis k s' n in
  if s' != s || n' != n then (s', n') else sn

and visitCabsDefinition vis (d: definition) : definition list =
  doVisitList vis vis#vdef childrenDefinition d
and childrenDefinition vis d =
  match d with
    FUNDEF (spec,sn, b, l, lend) ->
      let sn' = childrenSingleName vis NFun sn in
      let b' = visitCabsBlock vis b in
      (* End the scope that was started by childrenFunctionName *)
      vis#vExitScope ();
      if sn' != sn || b' != b then FUNDEF (spec,sn', b', l, lend) else d

  | DECDEF (spec,(s, inl), l) ->
      let s' = visitCabsSpecifier vis s in
      let inl' = mapNoCopy (childrenInitName vis s') inl in
      if s' != s || inl' != inl then DECDEF (spec,(s', inl'), l) else d
  | TYPEDEF (ng, l) ->
      let ng' = childrenNameGroup vis NType ng in
      if ng' != ng then TYPEDEF (ng', l) else d
  | ONLYTYPEDEF (s, l) ->
      let s' = visitCabsSpecifier vis s in
      if s' != s then ONLYTYPEDEF (s', l) else d
  | GLOBASM _ -> d
  | PRAGMA (e, l) ->
      let e' = visitCabsExpression vis e in
      if e' != e then PRAGMA (e', l) else d
  | LINKAGE (n, l, dl) ->
      let dl' = mapNoCopyList (visitCabsDefinition vis) dl in
      if dl' != dl then LINKAGE (n, l, dl') else d
  | GLOBANNOT _ -> d
  | CUSTOM _ -> d

and visitCabsBlock vis (b: block) : block =
  doVisit vis vis#vblock childrenBlock b

and childrenBlock vis (b: block) : block =
  let _ = vis#vEnterScope () in
  let battrs' = mapNoCopyList (visitCabsAttribute vis) b.battrs in
  let bstmts' = mapNoCopyList (visitCabsStatement vis) b.bstmts in
  let _ = vis#vExitScope () in
  if battrs' != b.battrs || bstmts' != b.bstmts then
    { blabels = b.blabels; battrs = battrs'; bstmts = bstmts' }
  else
    b
and visitCabsStatement vis (s: statement) :  statement list =
  doVisitList vis vis#vstmt childrenStatement s
and childrenStatement vis s =
  let ve e = visitCabsExpression vis e in
  let vs l s = match visitCabsStatement vis s with
    [s'] -> s'
  | sl -> { s with stmt_node = BLOCK ({blabels = []; battrs = []; bstmts = sl }, l, l(*LRICEA*))}
  in
  match s.stmt_node with
    NOP _ -> s
  | COMPUTATION (e, l) ->
      let e' = ve e in
      if e' != e then {s with stmt_node = COMPUTATION (e', l)} else s
  | BLOCK (b, l, l') ->
      let b' = visitCabsBlock vis b in
      if b' != b then {s with stmt_node = BLOCK (b', l, l')} else s
  | SEQUENCE (s1, s2, l) ->
      let s1' = vs l s1 in
      let s2' = vs l s2 in
      if s1' != s1 || s2' != s2 then {s with stmt_node = SEQUENCE (s1', s2', l)} else s
  | IF (e, s1, s2, l) ->
      let e' = ve e in
      let s1' = vs l s1 in
      let s2' = vs l s2 in
      if e' != e || s1' != s1 || s2' != s2 then {s with stmt_node = IF (e', s1', s2', l)} else s
  | WHILE (a, e, s1, l) ->
      let e' = ve e in
      let s1' = vs l s1 in
      if e' != e || s1' != s1 then {s with stmt_node = WHILE (a, e', s1', l)} else s
  | DOWHILE (a, e, s1, l) ->
      let e' = ve e in
      let s1' = vs l s1 in
      if e' != e || s1' != s1 then {s with stmt_node = DOWHILE (a, e', s1', l)} else s
  | FOR (a, fc1, e2, e3, s4, l) ->
      let _ = vis#vEnterScope () in
      let fc1' =
        match fc1 with
          FC_EXP e1 ->
            let e1' = ve e1 in
            if e1' != e1 then FC_EXP e1' else fc1
        | FC_DECL d1 ->
            let d1' =
              match visitCabsDefinition vis d1 with
                [d1'] -> d1'
              | _ -> Kernel.fatal "visitCabs: for can have only one definition"
            in
            if d1' != d1 then FC_DECL d1' else fc1
      in
      let e2' = ve e2 in
      let e3' = ve e3 in
      let s4' = vs l s4 in
      let _ = vis#vExitScope () in
      if fc1' != fc1 || e2' != e2 || e3' != e3 || s4' != s4
      then {s with stmt_node = FOR (a, fc1', e2', e3', s4', l)} else s
  | BREAK _ | CONTINUE _ | GOTO _ -> s
  | RETURN (e, l) ->
      let e' = ve e in
      if e' != e then {s with stmt_node = RETURN (e', l)} else s
  | SWITCH (e, s1, l) ->
      let e' = ve e in
      let s1' = vs l s1 in
      if e' != e || s1' != s1 then {s with stmt_node = SWITCH (e', s1', l)} else s
  | CASE (e, s1, l) ->
      let e' = ve e in
      let s1' = vs l s1 in
      if e' != e || s1' != s1 then {s with stmt_node = CASE (e', s1', l)} else s
  | CASERANGE (e1, e2, s3, l) ->
      let e1' = ve e1 in
      let e2' = ve e2 in
      let s3' = vs l s3 in
      if e1' != e1 || e2' != e2 || s3' != s3 then
        {s with stmt_node = CASERANGE (e1', e2', s3', l)} else s
  | DEFAULT (s1, l) ->
      let s1' = vs l s1 in
      if s1' != s1 then {s with stmt_node = DEFAULT (s1', l)} else s
  | LABEL (n, s1, l) ->
      let s1' = vs l s1 in
      if s1' != s1 then {s with stmt_node = LABEL (n, s1', l)} else s
  | COMPGOTO (e, l) ->
      let e' = ve e in
      if e' != e then {s with stmt_node = COMPGOTO (e', l)} else s
  | DEFINITION d -> begin
      match visitCabsDefinition vis d with
          [d'] when d' == d -> s
        | [d'] -> {s with stmt_node = DEFINITION d' }
        | dl -> let l = get_definitionloc d in
          let dl' = List.map (fun d' -> {s with stmt_node = DEFINITION d'}) dl in
          {s with stmt_node = BLOCK ({blabels = []; battrs = []; bstmts = dl' }, l, l(*LRICEA*))}
    end
  | ASM (sl, b, details, l) ->
      let childrenIdentStringExp ((i,s, e) as input) =
        let e' = ve e in
        if e' != e then (i,s, e') else input
      in
      let details' = match details with
      | None -> details
      | Some { aoutputs = outl; ainputs = inl; aclobbers = clobs; alabels = labels } ->
	  let outl' = mapNoCopy childrenIdentStringExp outl in
	  let inl' = mapNoCopy childrenIdentStringExp inl in
	  if outl' == outl && inl' == inl then
	    details
	  else
	    Some { aoutputs = outl'; ainputs = inl'; aclobbers = clobs ; alabels = labels }
      in
      if details' != details then
        {s with stmt_node = ASM (sl, b, details', l)} else s
  | TRY_FINALLY (b1, b2, l) ->
      let b1' = visitCabsBlock vis b1 in
      let b2' = visitCabsBlock vis b2 in
      if b1' != b1 || b2' != b2 then {s with stmt_node = TRY_FINALLY(b1', b2', l)} else s
  | TRY_EXCEPT (b1, e, b2, l) ->
      let b1' = visitCabsBlock vis b1 in
      let e' = visitCabsExpression vis e in
      let b2' = visitCabsBlock vis b2 in
      if b1' != b1 || e' != e || b2' != b2 then
        {s with stmt_node = TRY_EXCEPT(b1', e', b2', l)} else s
  | THROW (e,l) ->
    let e' = optMapNoCopy (visitCabsExpression vis) e in
    if e != e' then { s with stmt_node = THROW(e',l) } else s
  | TRY_CATCH(t,l,loc) ->
    let visit_one_catch (v,s as c) =
      let v' = optMapNoCopy (childrenSingleName vis NVar) v in
      let s' = vs loc s in
      if v' != v || s' != s then (v,s) else c
    in
    let t' = vs loc t in
    let l' = mapNoCopy visit_one_catch l in
    if t' != t || l' != l then
      { s with stmt_node = TRY_CATCH(t',l',loc) }
    else s
  | CODE_ANNOT _ | CODE_SPEC _ -> s

and visitCabsExpression vis (e: expression) : expression =
  doVisit vis vis#vexpr childrenExpression e
and childrenExpression vis e =
  let ve e = visitCabsExpression vis e in
  match e.expr_node with
    NOTHING | LABELADDR _ -> e
  | UNARY (uo, e1) ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = UNARY (uo, e1')} else e
  | BINARY (bo, e1, e2) ->
      let e1' = ve e1 in
      let e2' = ve e2 in
      if e1' != e1 || e2' != e2 then
        { e with expr_node = BINARY (bo, e1', e2')} else e
  | QUESTION (e1, e2, e3) ->
      let e1' = ve e1 in
      let e2' = ve e2 in
      let e3' = ve e3 in
      if e1' != e1 || e2' != e2 || e3' != e3 then
        { e with expr_node = QUESTION (e1', e2', e3')} else e
  | CAST ((s, dt), ie) ->
      let s' = visitCabsSpecifier vis s in
      let dt' = visitCabsDeclType vis false dt in
      let ie' = visitCabsInitExpression vis ie in
      if s' != s || dt' != dt || ie' != ie then
        { e with expr_node = CAST ((s', dt'), ie')} else e
  | CALL (f, el, gl) ->
      let f' = ve f in
      let el' = mapNoCopy ve el in
      let gl' = mapNoCopy ve gl in
      if f' != f || el' != el then
        { e with expr_node = CALL (f', el',gl')} else e
  | COMMA el ->
      let el' = mapNoCopy ve el in
      if el' != el then { e with expr_node = COMMA (el') } else e
  | CONSTANT _ -> e
  | PAREN e1 ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = PAREN (e1') } else e
  | VARIABLE s ->
      let s' = vis#vvar s in
      if s' != s then { e with expr_node = VARIABLE s' } else e
  | EXPR_SIZEOF (e1) ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = EXPR_SIZEOF (e1') } else e
  | TYPE_SIZEOF (s, dt) ->
      let s' = visitCabsSpecifier vis s in
      let dt' = visitCabsDeclType vis false dt in
      if s' != s || dt' != dt then
        { e with expr_node = TYPE_SIZEOF (s' ,dt') } else e
  | EXPR_ALIGNOF (e1) ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = EXPR_ALIGNOF e1'} else e
  | TYPE_ALIGNOF (s, dt) ->
      let s' = visitCabsSpecifier vis s in
      let dt' = visitCabsDeclType vis false dt in
      if s' != s || dt' != dt then
        { e with expr_node = TYPE_ALIGNOF (s' ,dt')}
      else e
  | INDEX (e1, e2) ->
      let e1' = ve e1 in
      let e2' = ve e2 in
      if e1' != e1 || e2' != e2 then { e with expr_node = INDEX (e1', e2') }
      else e
  | MEMBEROF (e1, n) ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = MEMBEROF (e1', n)} else e
  | MEMBEROFPTR (e1, n) ->
      let e1' = ve e1 in
      if e1' != e1 then { e with expr_node = MEMBEROFPTR (e1', n) } else e
  | GNU_BODY b ->
      let b' = visitCabsBlock vis b in
      if b' != b then { e with expr_node = GNU_BODY b' } else e
  | EXPR_PATTERN _ -> e

and visitCabsInitExpression vis (ie: init_expression) : init_expression =
  doVisit vis vis#vinitexpr childrenInitExpression ie
and childrenInitExpression vis ie =
  let rec childrenInitWhat iw =
    match iw with
      NEXT_INIT -> iw
    | INFIELD_INIT (n, iw1) ->
        let iw1' = childrenInitWhat iw1 in
        if iw1' != iw1 then INFIELD_INIT (n, iw1') else iw
    | ATINDEX_INIT (e, iw1) ->
        let e' = visitCabsExpression vis e in
        let iw1' = childrenInitWhat iw1 in
        if e' != e || iw1' != iw1 then ATINDEX_INIT (e', iw1') else iw
    | ATINDEXRANGE_INIT (e1, e2) ->
        let e1' = visitCabsExpression vis e1 in
        let e2' = visitCabsExpression vis e2 in
        if e1' != e1 || e2' != e2 then ATINDEXRANGE_INIT (e1', e2') else iw
  in
  match ie with
    NO_INIT -> ie
  | SINGLE_INIT e ->
      let e' = visitCabsExpression vis e in
      if e' != e then SINGLE_INIT e' else ie
  | COMPOUND_INIT il ->
      let childrenOne ((iw, ie) as input) =
        let iw' = childrenInitWhat iw in
        let ie' = visitCabsInitExpression vis ie in
        if iw' != iw || ie' != ie then (iw', ie') else input
      in
      let il' = mapNoCopy childrenOne il in
      if il' != il then COMPOUND_INIT il' else ie


and visitCabsAttribute vis (a: attribute) : attribute list =
  doVisitList vis vis#vattr childrenAttribute a

and childrenAttribute vis ((n, el) as input) =
  let el' = mapNoCopy (visitCabsExpression vis) el in
  if el' != el then (n, el') else input

and visitCabsAttributes vis (al: attribute list) : attribute list =
  mapNoCopyList (visitCabsAttribute vis) al

let visitCabsFile (vis: cabsVisitor) ((fname, f): file) : file =
  (fname, mapNoCopyList (fun ((ghost,f) as glob) ->
                           let f' = visitCabsDefinition vis f in
                           match f' with
                             [f'] when f == f' -> [glob]
                           | _ -> List.map (fun f -> (ghost, f)) f'
                        ) f)

    (* end of file *)
