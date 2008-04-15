(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA for more details about the license.                       *)
(*                                                                        *)
(**************************************************************************)

(* $Id: model.ml,v 1.89 2008/10/03 13:09:17 uid568 Exp $ *)

open Cil_types
open Cil
open Db
open Locations

type values = Value.state

let state_name = "security_status"

module Make(S : Lattice.S) = struct

  (** Extended Security Lattice *)
  module L = struct

    (* first component is the real security status,
       second component is the security status of the dependences. *)
    include
      Abstract_interp.Make_Lattice_Product(S)(S)(struct let collapse=false end)

    let filterAnnotations attr =
      let rec find acc annot = function
	| [] -> acc
	| x :: l when attributeName x = annot -> find (annot :: acc) annot l
	| _ :: l -> find acc annot l
      in
      List.fold_left (fun acc a -> find acc a attr) [] S.possible_annotations

    let attributes2state dft attr =
      S.annotations2state dft (filterAnnotations attr)

    let type_attributes2state dft ty = attributes2state dft (typeAttr ty)

    let defaultall = function
      | Base.Var(_info, _) | Base.Initialized_Var(_info, _) ->
	  (* TODO à voir: faux avec annotations sur type et non sur nom ? *)
	  (*attributes2state info.vattr bottom*)
	  inject S.variable S.bottom
      | Base.Null | Base.Cell_class _ | Base.String _  ->
	  inject S.constant S.bottom

    let default base _ _ = defaultall base

  end

  (** Leaks of the analysis *)
  module Leaks = struct

    type call =
	{ cloc : Cil_types.location; (** Position of the call *)
	  fname : string (** Name of the function *) }

    (* TODO (Flyspray #63):
     * - Use my own data structure for persistent stack ?
     * - Use Clist instead of list ? *)

    type leak =
	{ potential : bool; (** Is the leak only potential ? *)
	  loc : Cil_types.location option; (** Position of the leak *)
	  data : exp;       (** The unsecured expression *)
	  call_stack : call list (* Used as a persistent stack. *)
	    (** The call stack at the position of the leak *) }

    (* Used as a persistent stack. *)
    type t = leak list

    let empty = []

    let print_call_stack fmt stack =
      List.iter
	(fun c ->
	   Format.fprintf fmt
             "@\n\tFrom location %a (call to %s)." d_loc c.cloc c.fname)
	(List.rev stack)

    open Format
    let print_leak l =
      let fmt x =
	if l.potential then
	  "POTENTIAL security leak; %s of %a may be wrong." ^^ x
	else
	  "PROBABLE security leak; %s of %a is probably wrong." ^^ x
      in
      let loc = match l.loc with None -> assert false | Some loc -> loc in
      Cil.warnLoc loc (fmt "%a") state_name
        !Ast_printer.d_exp l.data print_call_stack l.call_stack

    let print_stat stat =
      log "Security state summary:";
      begin match stat with
      | 0, 0 -> log "The code is secure."
      | p, s ->
	  let some fmt x = log fmt x (if x > 1 then "s" else "") in
	  let log_p () = some "%d potential security leak%s." p in
	  if s > 0 then begin
	    if p > 0 then log_p ();
	    some "%d probable security leak%s." s
	  end else begin
	    assert (p > 0);
	    log_p ()
	    end
      end

    let nb_leaks = ref (0, 0) (* potential / sure *)

    let print results =
      nb_leaks := 0, 0;
      let f l =
	print_leak l;
	let p, s = !nb_leaks in
	if l.potential then nb_leaks := p+1, s else nb_leaks := p, s+1
      in
      List.iter f (List.rev results)

    let print_all results =
      print results;
      print_stat !nb_leaks

    let new_leak p loc exp =
      { potential = p; loc = loc; data = exp; call_stack = [] }

    let add_leak ~potential loc exp leaks = new_leak potential loc exp :: leaks

    let add_from_and_subst loc name subst ~from leaks =
      let call = { cloc = loc; fname = name } in
      let add l acc =
	let data, _ = Subst.expr ~trans:false l.data subst in
	{ l with call_stack = call :: l.call_stack; data = data } :: acc
      in
      (* keep the list in the same order *)
      List.fold_right add from leaks

    let rec is_included l1 l2 =
      match l1, l2 with
      | [], _ -> true
      | _ :: _, [] -> false
      | hd1 :: tl1, hd2 :: tl2 ->
	  (* [l1] and [l2] are sorted in the same order,
	     with first the newest element. *)
	  (hd1:leak) == hd2 && is_included tl1 tl2

    let combine ~old l =
      (* Remove double elements. *)
      let rec aux acc l1 l2 =
	match l1, l2 with
	| l, [] | [], l ->
	    List.rev l @ acc
	| hd1 :: tl1, hd2 :: tl2 ->
	    if (hd1:leak) == hd2 then
	      aux (hd1 :: acc) tl1 tl2
	    else
	      List.rev l2 @ List.rev l1 @ acc
      in
      aux [] (List.rev old) (List.rev l)

  end

  (** State of the analysis *)
  module State = struct

    (** Map of zone --> status *)
    module M = struct

      include Lmap_bitwise.Make_bitwise(L)

      (* Redefine an higher-level [add_binding] function. *)
      let add_binding map loc v =
	let z = valid_enumerate_bits loc in
	let exact = valid_cardinal_zero_or_one loc in
	add_binding ~exact map z v

    end

    type t =
	{ map: M.t;          (** status of a zone *)
	  leaks: Leaks.t;    (** found leaks      *)
	  subst: Subst.t;    (** Substitution of formals *) }

    let pretty fmt s = M.pretty fmt s.map

    let find_zone_status state z =
      try M.find state.map z with Not_found -> assert false

    let find_loc_status state l =
      find_zone_status state (valid_enumerate_bits l)

    let find_loc_status_with_deps ?(with_deps=true) state l =
      let s = find_loc_status state l in
      let exact = L.fst s in
      if with_deps then S.join exact (L.snd s) else exact

    (** Status of an expression. *)
    type exp_status =
      | Status of S.t        (** Concrete status *)
      | Location of location (** The same than the given location *)

    (** Debugging purpose *)
    let pretty_status _ = function
      | Status s -> log "[security] status %a" S.pretty s
      | Location l -> log "[security] location %a" Locations.pretty l

    let last_stmt = ref dummyStmt

    type call = { current_fun : Db_types.kernel_function; deps : S.t }

    let call_stack: call Stack.t = Stack.create ()

    let reset () =
      Stack.clear call_stack;
      last_stmt := dummyStmt;
      { map = M.empty;
	leaks = Leaks.empty;
	subst = Subst.empty }

    let lval_to_loc = !Value.lval_to_loc_state

    let rec status_of_deps values state =
      if S.use_ctrl_dependencies && !last_stmt != dummyStmt then begin
	let stmt = !last_stmt in
	let _, kf = Kernel_function.find_from_sid stmt.sid in
	let pdg = !Pdg.get kf in
(*	log "stmt %a@." d_stmt stmt;*)
	let nodes = !Pdg.find_simple_stmt_nodes pdg stmt in
	let deps =
	  match nodes with
	  | [] -> []
	  | n :: _ -> !Pdg.direct_ctrl_dpds pdg n
	in
(*	List.iter (fun n -> log "node %a@." !Pdg.pretty_node n) deps;*)
	let status_of_dep dep =
	  (* search the expression of the statement which really is the control
	     dependence and compute its status *)
	  match dep.skind with
	  | If(e, _, _, _) | Switch(e, _, _, _) ->
	      find_exp_status ~with_deps:false values state e
	  | Block _ | UnspecifiedSequence _
	  | Instr (Asm _) (* because of possible dummyStmt, see below *) ->
	      S.bottom
	  | _ ->
	      assert false
	in
	List.fold_left
	  (fun acc n ->
	     let s =
	       match !Pdg.node_key n with
               | PdgIndex.Key.Stmt s -> s
               | PdgIndex.Key.SigCallKey (_id,
                   (PdgIndex.Signature.In PdgIndex.Signature.InCtrl)) ->
                    dummyStmt (* TODO: Anne. *)
	       | PdgIndex.Key.SigKey _k -> dummyStmt (* TODO: huh, very ugly *)
	       | _ ->
                   Format.printf "what is this key : %a ???"
                     PdgIndex.Key.pretty (!Pdg.node_key n);
                   assert false
	     in
	     S.join acc (status_of_dep s))
	  (* continue the current dependencies computation *)
	  (try (Stack.top call_stack).deps with Stack.Empty -> S.bottom)
	  deps
      end else
	S.bottom

    and join_deps values state = S.join (status_of_deps values state)

    and find_loc_or_status ?(with_deps=true) values state e =
      let status s =
	Status
	  (if with_deps then S.join s (status_of_deps values state) else s)
      in
      match stripInfo e with
      | Info _ -> assert false
      | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ ->
	  status S.constant
      | Lval lv ->
	  let e, subst_occured = Subst.lval ~trans:true lv state.subst in
	  if subst_occured then
	    find_loc_or_status ~with_deps values state e
	  else
	    Location (lval_to_loc values lv)
      | AddrOf _lv | StartOf _lv ->
	  (* TODO: Flyspray #64 *)
	  CilE.warn_once "[security] address expression detected: suboptimical case in this version.";
	  Status S.top
      | CastE(ty, e) ->
	  let s = L.type_attributes2state S.bottom ty in
	  let ls = find_loc_or_status ~with_deps values state e in
	  if S.equal s S.bottom  then
	    ls
	  else
	    (match ls with
	     | Status _ ->
		 status s
	     | Location l ->
		 status (S.join s (L.snd (find_loc_status state l))))
      | SizeOfE e | AlignOfE e | UnOp(_, e, _) ->
	  find_loc_or_status ~with_deps values state e
      | BinOp((PlusPI | IndexPI | MinusPI | MinusPP), _e1, _e2, TPtr _) ->
	  (* TODO: Flyspray #64 *)
	  CilE.warn_once "[security] arithmetic pointer expression detected: suboptimital case in this version.";
	  Status S.top
      | BinOp((PlusPI | IndexPI | MinusPI | MinusPP), _e1, _e2, _) ->
	  assert false
      | BinOp(_, e1, e2, _) ->
	  let s1 = find_exp_status ~with_deps values state e1 in
	  let s2 = find_exp_status ~with_deps values state e2 in
	  status (S.join s1 s2)

    and find_exp_status ?(with_deps=true) values state e =
      match find_loc_or_status ~with_deps values state e with
      | Status s -> s
      | Location l -> find_loc_status_with_deps ~with_deps state l

    let is_included s1 s2 =
      (* TODO: nothing to do with subst & co ? *)
      M.is_included s1.map s2.map && Leaks.is_included s1.leaks s2.leaks

    let combine ~old s =
      (* TODO: nothing to do with subst & co ? *)
      { map = M.join old.map s.map;
	leaks = Leaks.combine ~old:old.leaks s.leaks;
	subst = s.subst }

    let change_loc_status values state loc status =
(*      log "change_loc_status: %a with %a@."
	      Locations.pretty loc pretty_status status;*)
      (match loc.loc with
       | Location_Bits.Top _ ->
	   CilE.warn_once "[security] analysis degenerated"
       | _ -> ());
      match status with
      | Status s ->
	  let s = L.inject s (status_of_deps values state) in
	  { state with map = M.add_binding state.map loc s }
      | Location l ->
	  let conv s =
	    let deps = S.join (status_of_deps values state) (L.snd s) in
	    L.inject (L.fst s) deps
	  in
	  (try
	     let f (b, s) = b, conv s in
	     { state with map = M.copy_paste ~f l loc state.map }
	   with Lmap_bitwise.Bitwise_cannot_copy ->
	     let s = find_loc_status state l in
	     { state with map = M.add_binding state.map loc (conv s) })

    let change_lval_status values state x status =
      let e, subst_occured = Subst.lval ~trans:true x state.subst in
      match find_loc_or_status values state e, subst_occured with
      | Location l, _ -> change_loc_status values state l status
      | Status _, true -> state
      | Status _, false -> assert false

    let change_lval_exp values state x e =
      let s = find_loc_or_status values state e in
      change_lval_status values state x s

    let clear_leaks state = { state with leaks = Leaks.empty }

    let push_call values kf args state =
      let deps, _ctx =
	status_of_deps values state,
	List.map (find_exp_status values state) args
      in
      Stack.push { current_fun = kf; deps = deps } call_stack;
(*      let get_status s = S.join deps (find_zone_status state s) in*)
      { state with leaks = Leaks.empty }
      (* TODO (flyspray #31):
	 le code ci-dessous n'est utilisé que dans la mémoization débranchée *)
(*      try
	Zone.fold_enum_by_base
	  (fun z acc -> get_status z :: acc)
	  (!Inputs.get_external kf)
	  ctx
	Zone.top
      with Zone.Error_Top ->
	assert false
	*)
    let pop_call kinstr fd args ~old state =
      let call = try Stack.pop call_stack with Stack.Empty -> assert false in
      match kinstr with
      | Kglobal -> state
      | Kstmt s ->
	  let diff_subst =
	    try
	      List.fold_left2
		(fun acc x y -> Subst.add x y acc)
		Subst.empty
		fd.sformals
		args
	    with Invalid_argument _ ->
	      Subst.empty
	  in
	  let leaks =
	    Leaks.add_from_and_subst
	      (Cilutil.get_stmtLoc s.skind)
	      (Kernel_function.get_name call.current_fun)
	      diff_subst
	      ~from:state.leaks
	      old.leaks
	  in
	  { state with leaks = leaks }

    let print_results state = Leaks.print_all state.leaks

    let add_leak ~potential state ki exp =
      let loc =
	match ki with
	| Kglobal -> None
	| Kstmt s -> Some (Cilutil.get_stmtLoc s.skind)
      in
      let leaks = Leaks.add_leak ~potential loc exp state.leaks in
      { state with leaks = leaks }

  end

  open State

  (** How to register variables. *)
  module Register = struct

    let change_loc_status values state loc status =
      change_loc_status values state loc (Status status)

    let rec typ state values loc = function
      | TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _
      | TEnum _ | TBuiltin_va_list _ ->
	  state
      | TNamed(info, _)  ->
	  typ state values loc info.ttype
      | TComp(info, _) as ty ->
	  List.fold_left (field values ty loc) state info.cfields

    and field values ty loc state f =
      (* TODO : à revoir *)
      let s = L.type_attributes2state S.variable f.ftype in
      Location_Bits.fold_bases
	(fun b state ->
	   let loc = loc_of_typoffset b ty (Field(f, NoOffset)) in
	   let state =  change_loc_status values state loc s in
	   typ state values loc f.ftype)
	loc.loc
	state

    let variable state values ?status v =
      let status =
	match status with
	| None -> S.variable
	| Some s -> s
      in
      let loc = loc_of_varinfo v in
      let state = change_loc_status values state loc status in
      typ state values loc v.vtype

    let globals state =
      let values = Value.globals_state () in
      Globals.Vars.fold (fun v _ s -> variable s values v) state

    let locals values f state =
      List.fold_left (fun s v -> variable s values v) state f.slocals

    let formals params args old_values state =
      try
	List.fold_left2
	  (fun (state, new_values) v a ->
	     let loc = loc_of_varinfo v in
	     { state with subst = Subst.add v a state.subst },
             Relations_type.Model.add_binding
               ~with_alarms:CilE.warn_none_mode ~exact:(valid_cardinal_zero_or_one loc)
               new_values
               loc
               (!Value.eval_expr ~with_alarms:CilE.warn_none_mode old_values a))
	  (state, old_values)
	  params
	  args
      with Invalid_argument _ ->
	CilE.warn_once
	  "Variadic call detected. Using only %d argument(s)."
	  (min (List.length args) (List.length params));
	state, old_values

    let clean kf state =
      let map =
	M.filter_base
	  (function
            (* Duplicate code from Base.is_formal_or_local,
               replacing Utils.is_formal_or_local by Db.is_formal_or_local,
               but no way to produce non mutually recursive module
               dependencies between Base and Db :-( *)
           | Base.Var(v, _) | Base.Initialized_Var(v, _) ->
               not (Kernel_function.is_formal_or_local v kf)
	   | Base.Null | Base.String _ | Base.Cell_class _ ->
	       assert false)
	  state.map
      in
      { state with map = map }

  end

  (** Logic model *)
  module Logic = struct

    let affect = change_lval_exp

    let return values state lv e =
      match lv with
      | None -> state
      | Some lv -> affect values state lv e

    let equal x y = S.equal x y
    let comparable x y = S.is_included x y || S.is_included y x
    let uncomparable x y = not (comparable x y)

    let cmp_of_rel x y = function
      | Rlt -> S.is_included x y && not (equal x y), uncomparable x y
      | Rgt -> S.is_included y x && not (equal x y), uncomparable x y
      | Rle -> S.is_included x y, uncomparable x y
      | Rge -> S.is_included y x, uncomparable x y
      | Req -> S.equal x y , uncomparable x y
      | Rneq -> uncomparable x y, S.equal x y

    let warn_todo () =
      CilE.warn_once
	"[security] this kind of specification is ignored with this lattice."

    let find_term_loc_or_status values state f l =
      match f.l_name, l with
      | a, [ y ] when a = state_name ->
	  let e = !Properties.Interp.term_to_exp y in
	  Some (find_loc_or_status values state e)
      | a, [] when List.mem a S.possible_annotations ->
	  Some (Status (S.annotations2state S.bottom [ a ]))
      | _, _ ->
	  None

    let rec requires values ki state p = match p.content with
      | Pand(p1, p2) ->
	  let state = requires values ki state p1 in
	  requires values ki state p2
      | (* [state(lval) op term] *)
	  Prel(rel,
	       { term_node = Tapp(f1, _, [ x ]) },
	       { term_node = Tapp(f2, _, l) })
	    when f1.l_name = state_name
	      ->
	  (match find_term_loc_or_status values state f2 l with
	   | None -> warn_todo (); state
	   | Some sly ->
	       let subst trans e = fst (Subst.expr ~trans e state.subst) in
	       let ex = !Properties.Interp.term_to_exp x in
	       let sx = find_exp_status values state (subst true ex) in
	       let sx =
		 if S.use_ctrl_dependencies then
		   join_deps values state sx
		 else
		   sx
	       in
	       let sy =
		 match sly with
		 | Status s -> s
		 | Location l -> find_loc_status_with_deps
		     ~with_deps:S.use_ctrl_dependencies
		       state
		       l
	       in
	       match cmp_of_rel sx sy rel with
	       | true, true -> assert false
	       | true, false -> state (* secure *)
	       | false, b ->
		   if Cmdline.Debug.get () > 0 then
		     log "[security] add security leak for %a"
		       !Ast_printer.d_exp (subst true ex);
		   let state =
		     add_leak ~potential:(not b) state ki (subst false ex)
		   in
		   if Cmdline.Security.PropagateAssertions.get () then
		     (* you have just emitted a leak corresponding to a
			propertie P, so you can assume that P is verified. Be
			careful to only perform over-approximation. *)
		     match rel with
		     | Req | Rle ->
			 (try
			    let x = !Properties.Interp.term_to_lval x in
			    change_lval_status values state x sly
			  with Invalid_argument _ ->
			    state)
		     | Rlt | Rgt | Rge | Rneq ->
			 state
		   else
		     state)
      | Ptrue -> state
      | Pfalse -> state
      | _ ->
	  (* Deal yet only with conjunction of relation. *)
	  warn_todo ();
	  state

    let rec ensures values state p = match p.content with
      | Pand(p1, p2) ->
	  let state = ensures values state p1 in
	  ensures values state p2
      | (* [state(x) = term] *)
	  Prel(Req,
	       { term_node = Tapp(f1, _, [ x ]) },
	       { term_node = Tapp(f2, _, l) })
	    when f1.l_name = state_name
	      ->
	  (match find_term_loc_or_status values state f2 l with
	   | None -> warn_todo (); state
	   | Some sy ->
	       (try
		  let x = !Properties.Interp.term_to_lval x in
		  change_lval_status values state x sy
		with Invalid_argument _ ->
		  CilE.warn_once
                    "%a is not a left value. Do not take the ensurement into account.@."
                    !Ast_printer.d_term x;
		  state))
      | Ptrue -> state
      | Pfalse -> state
      | _ ->
	  (* Deal yet only with conjunction of equalities. *)
	  warn_todo ();
	  state

  end

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
