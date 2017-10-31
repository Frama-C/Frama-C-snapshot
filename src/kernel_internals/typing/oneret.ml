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

open Cil_types
open Cil
open Logic_const

let adjust_assigns_clause loc var code_annot =
  let change_result = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TResult _ -> ChangeTo (TVar var)
      | TVar _ | TMem _ -> DoChildren
  end
  in
  let change_term t = Cil.visitCilTerm change_result t in
  let module M = struct exception Found end in
  let check_var = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TVar v when Cil_datatype.Logic_var.equal var v -> raise M.Found
      | TVar _ | TResult _ | TMem _ -> DoChildren
  end
  in
  let contains_var l =
    try ignore (Cil.visitCilAssigns check_var (Writes l)); false
    with M.Found -> true
  in
  let change_from = function
    | FromAny -> FromAny
    | From l -> From (List.map Logic_const.refresh_identified_term l)
  in
  let adjust_lval (_,assigns as acc) (loc,from) =
    if Logic_utils.contains_result loc.it_content then begin
      true,
      (Logic_const.new_identified_term (change_term loc.it_content),
       change_from from)::assigns
    end else acc
  in
  let adjust_clause b =
    match b.b_assigns with
      | WritesAny -> ()
      | Writes l ->
          if not (contains_var l) then begin
            let (changed, a) = List.fold_left adjust_lval (false,l) l in
            let a =
              if changed then a 
              else 
                (Logic_const.new_identified_term (Logic_const.tvar ~loc var),
                 FromAny)
                :: a
            in
            b.b_assigns <- Writes a
          end
  in
  match code_annot with
    | AStmtSpec (_,s) -> List.iter adjust_clause s.spec_behavior
    | _ -> ()

type returns_clause =
  Cil_types.stmt * Cil_types.behavior * Cil_types.identified_predicate

type goto_annot =
  Cil_types.stmt * Cil_types.code_annotation

type callback = returns_clause -> goto_annot list -> unit

let collect_returns (ca : Cil_types.code_annotation) =
  match ca.annot_content with
  | AStmtSpec(_bhvs,spec) ->
    List.fold_left
      (fun acc bhv ->
         List.fold_left
           (fun acc (kind,predicate) ->
              match kind with
              | Returns -> (bhv,predicate) :: acc
              | _ -> acc
           ) acc bhv.b_post_cond
      ) [] spec.spec_behavior
  | _ -> []

let oneret ?(callback: callback option) (f: fundec) : unit =
  let fname = f.svar.vname in
  (* Get the return type *)
  let retTyp =
    match f.svar.vtype with
      TFun(rt, _, _, _) -> rt
    | _ ->
	Kernel.fatal "Function %s does not have a function type" f.svar.vname
  in
  (* Does it return anything ? *)
  let hasRet = match unrollType retTyp with TVoid _ -> false | _ -> true in

  (* Memoize the return result variable. Use only if hasRet *)
  let lastloc = ref Cil_datatype.Location.unknown in
  let getRetVar =
    let retVar : varinfo option ref = ref None in
    fun () ->
      match !retVar with
	Some rv -> rv
      | None -> begin
            let rv = makeLocalVar f "__retres" retTyp in (* don't collide *)
            retVar := Some rv;
            rv
      end
  in
  let convert_result p =
    let vis = object
      inherit Cil.nopCilVisitor
      method! vterm_lhost = function
        | TResult _ ->
          let v = getRetVar () in
          ChangeTo (TVar (cvar_to_lvar v))
        | TMem _ | TVar _ -> DoChildren
    end
    in visitCilPredicate vis p
  in
  let assert_of_returns ca =
    match ca.annot_content with
      | AAssert _ | AInvariant _ | AVariant _
      | AAssigns _ | AAllocation _ | APragma _ | AExtended _ -> ptrue
      | AStmtSpec (_bhvs,s) ->
        let res =
          List.fold_left
            (fun acc bhv ->
              pand
                (acc,
                 pimplies
                   (pands
                      (List.map
                         (fun p ->
                           pold ~loc:p.ip_content.pred_loc
                             (Logic_const.pred_of_id_pred p))
                         bhv.b_assumes),
                    pands
                      (List.fold_left
                         (fun acc (kind,p) ->
                           match kind with
                               Returns -> Logic_const.pred_of_id_pred p :: acc
                             | Normal | Exits | Breaks | Continues -> acc)
                         [ptrue] bhv.b_post_cond)
                   )))
            ptrue s.spec_behavior
        in convert_result res
  in
  (* Remember if we have introduced goto's *)
  let haveGoto = ref false in
  (* Memoize the return statement *)
  let retStmt : stmt ref = ref dummyStmt in
  let getRetStmt (_x: unit) : stmt =
    if !retStmt == dummyStmt then begin
      let sr =
        let getLastLoc () = (* CEA modified to have a good [!lastloc] *)
          let rec setLastLoc = function
            | [] -> ()
            | {skind=Block b} :: [] -> setLastLoc b.bstmts
            | {skind=UnspecifiedSequence seq}::[] ->
                setLastLoc (List.map (fun (x,_,_,_,_) -> x) seq)
            | {skind= _} as s :: [] -> lastloc := Cil_datatype.Stmt.loc s
            | {skind=_s} :: l -> setLastLoc l
          in
          setLastLoc f.sbody.bstmts; !lastloc
        in
        let loc = getLastLoc () in
      (* Must create a statement *)
        let rv =
          if hasRet then
            Some (new_exp ~loc (Lval(Var (getRetVar ()), NoOffset)))
          else None
      in
        mkStmt (Return (rv, loc))
      in retStmt := sr;
        sr
    end else
      !retStmt
  in
  (* Stack of predicates that must hold in case of returns
     (returns clause with \old transformed into \at(,L) for a suitable L).
     TODO: split that into behaviors and generates for foo,bar: assert instead
     of plain assert.
   *)
  let returns_stack :
    (Cil_types.predicate * Cil_types.stmt * Cil_types.code_annotation) Stack.t
    = Stack.create () in
  let popn n =
    try for _ = 1 to n do ignore (Stack.pop returns_stack) done
    with Stack.Empty -> assert false
  in
  let to_callback = Hashtbl.create 8 in
  let do_callback cb = Hashtbl.iter (fun _ (ca,gs) -> cb ca gs) to_callback in
  let register_goto (ca : returns_clause) (gc : goto_annot) =
    let (_,_, { ip_id }) = ca in
    let gs = try snd (Hashtbl.find to_callback ip_id) with Not_found -> [] in
    Hashtbl.replace to_callback ip_id (ca,gc::gs)
  in
  (* Now scan all the statements. Know if you are the main body of the
   * function and be prepared to add new statements at the end.
   * popstack indicates whether we should pop the stack after having analyzed
     current statement. It is an int since nothing in ACSL prevents from having
     multiple statement contracts on top of each other before finding an
     actual statement...
   *)
  let rec scanStmts acc (mainbody: bool) popstack = function
    | [] when mainbody -> (* We are at the end of the function. Now it is
                           * time to add the return statement *)
      let rs = getRetStmt () in
      if !haveGoto then
        rs.labels <- (Label("return_label", !lastloc, false)) :: rs.labels;
      List.rev (rs :: acc)

    | [] -> List.rev acc

    | [{skind=Return (Some ({enode = Lval(Var _,NoOffset)}), _l)} as s]
        when mainbody && not !haveGoto ->
      (* We're not changing the return into goto, so returns clause will still
         have effect.
       *)
      popn popstack;
      List.rev (s::acc)

    | ({skind=Return (retval, loc)} as s) :: rests ->
        Cil.CurrentLoc.set loc;
    (*
      ignore (E.log "Fixing return(%a) at %a\n"
      insert
      (match retval with None -> text "None"
      | Some e -> d_exp () e)
      d_loc l);
     *)
      if hasRet && retval = None then
        Kernel.fatal ~current:true
	  "Found return without value in function %s" fname;
      if not hasRet && retval <> None then
        Kernel.fatal ~current:true "Found return in subroutine %s" fname;
    (* Keep this statement because it might have labels. But change it to
     * an instruction that sets the return value (if any). *)
      s.skind <- begin
        match retval with
            Some rval -> Instr (Set((Var (getRetVar ()), NoOffset), rval, loc))
          | None -> Instr (Skip loc)
      end;
      let returns_assert = ref ptrue in
      Stack.iter
        (fun (p,_,_) -> returns_assert := pand ~loc (p, !returns_assert))
        returns_stack;
      (match retval with
       | Some _ ->
         let lvar = Cil.cvar_to_lvar (getRetVar()) in
         Stack.iter
           (fun (_,_,ca) -> adjust_assigns_clause loc lvar ca.annot_content)
           returns_stack
       | None -> () (* There's no \result: no need to adjust it *)
      );
      (* See if this is the last statement in function, and we don't
         have a statement contract above us. In that last case, it is best
         to keep a small block with a goto the actual return statement, so
         as to preserve the fact that we don't fall through the return
         statement. In particular, the following contract holds, and so should
         its normalization:

         /*@ returns \true; ensures \false; */
         returns 0;
      *)
      if mainbody && rests == [] && popstack = 0 then begin
        (* last statement, no contract, we can just fall through. *)
        scanStmts (s :: acc) mainbody 0 rests
      end else begin
      (* Add a Goto and put everything into a block on which
         the statement contract(s) will apply.
       *)
        let sgref = ref (getRetStmt ()) in
        let sg = mkStmt (Goto (sgref, loc)) in
        haveGoto := true;
        let b_stmts =
          match !returns_assert with
          | { pred_content = Ptrue } -> [s; sg]
          | p ->
            let a = Logic_const.new_code_annotation (AAssert ([],p)) in
            let sta = mkStmt (Instr (Code_annot (a,loc))) in
            if callback<>None then
              ( let gclause = sta , a in
                Stack.iter
                  (fun (_,str,ca) ->
                     List.iter
                       (fun (bhv,ret) -> register_goto (str,bhv,ret) gclause)
                       (collect_returns ca)
                  ) returns_stack ) ;
            [ s; sta; sg ]
        in
        let s = mkStmt (Block (mkBlock b_stmts)) in
        popn popstack;
        scanStmts (s :: acc) mainbody 0 rests
      end

    | ({skind=If(eb,t,e,l)} as s) :: rests ->
    (*CEA currentLoc := l;*)
      s.skind <- If(eb, scanBlock false t, scanBlock false e, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=Loop(a,b,l,lb1,lb2)} as s) :: rests ->
    (*CEA currentLoc := l;*)
      s.skind <- Loop(a,scanBlock false b, l,lb1,lb2);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=Switch(e, b, cases, l)} as s) :: rests ->
    (*CEA currentLoc := l;*)
      s.skind <- Switch(e, scanBlock false b, cases, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | [{skind=Block b} as s] when popstack = 0 ->
      (* if we have a statement contract just above (i.e popstack<>0),
         don't consider that we are in the main body.
         Depending on the semantics we want to give to ACSL contracts
         wrt control flow that jumps in the middle of a block over which
         a statement contract applies, this might lead to incorrect contract
         in the end. For now, it's safer to ensure that the goto
         is directed outside of such a block. *)
      s.skind <- Block (scanBlock mainbody b);
      popn popstack;
      List.rev (s::acc)
    | ({skind=Block b} as s) :: rests ->
      s.skind <- Block (scanBlock false b);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | [{skind = UnspecifiedSequence seq} as s] when popstack = 0 ->
      (* see above. *)
      s.skind <-
        UnspecifiedSequence
        (List.concat
           (List.map (fun (s,m,w,r,c) ->
             let res = scanStmts [] mainbody 0 [s] in
             (List.hd res,m,w,r,c)::
               (List.map (fun x -> x,[],[],[],[]) (List.tl res)))
              seq));
      popn popstack;
      List.rev (s::acc)
    | ({skind = UnspecifiedSequence seq} as s) :: rests ->
      s.skind <-
        UnspecifiedSequence
        (List.concat
           (List.map (fun (s,m,w,r,c) ->
             let res = scanStmts [] false 0 [s] in
             (List.hd res,m,w,r,c)::
               (List.map (fun x -> x,[],[],[],[]) (List.tl res)))
              seq));
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | {skind=Instr(Code_annot ({ annot_content = AStmtSpec _ } as ca,_))} as s
      :: rests ->
      let returns = assert_of_returns ca in
      let returns = Logic_utils.translate_old_label s returns in
      Stack.push (returns,s,ca) returns_stack;
      scanStmts (s::acc) mainbody (popstack + 1) rests
    | { skind = Instr (Code_annot _) } as s :: rests ->
      scanStmts (s::acc) mainbody popstack rests
    | { skind = TryCatch(t,c,l) } as s :: rests ->
      let scan_one_catch (e,b) = (e,scanBlock false b) in
      let t = scanBlock false t in
      let c = List.map scan_one_catch c in
      s.skind <- TryCatch(t,c,l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({skind=(Goto _ | Instr _ | Continue _ | Break _
                  | TryExcept _ | TryFinally _ | Throw _)} as s)
      :: rests ->
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests

  and scanBlock (mainbody: bool) (b: block) =
    { b with bstmts = scanStmts [] mainbody 0 b.bstmts;}

  in
  (*CEA since CurrentLoc isn't set
    ignore (visitCilBlock dummyVisitor f.sbody) ; *)(* sets CurrentLoc *)
  (*CEA so, [scanBlock] will set [lastloc] when necessary
    lastloc := !currentLoc ;  *) (* last location in the function *)
  f.sbody <- scanBlock true f.sbody ;
  Extlib.may do_callback callback

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
