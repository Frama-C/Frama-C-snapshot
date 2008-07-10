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

(* $Id: cilE.ml,v 1.57 2008/07/11 06:36:05 uid570 Exp $ *)
(* Cil extensions *)
open Cil_types
open Cil
open Db_types

let debug = false

let (warn_h:(string*Cil_types.location,unit) Hashtbl.t) = Hashtbl.create 1789

(*
let warn_once : ('a,Format.formatter, unit, unit) format4 -> 'a =
  function fmt ->
    let v = string_of_format fmt,!Cil.currentLoc in
    try
      Hashtbl.find warn_h v;
      Printf.kapr (fun _ _ -> Obj.magic ()) (fst v)
    with Not_found ->
      Hashtbl.add warn_h v ();
      Cil.warn fmt
*)

let add_predicate p =
  if not (Logic_env.PredicateInfo.mem p.p_name) then
    Logic_env.add_builtin_predicate p

let init_builtins () =
  add_predicate  {
    p_name = "pointer_comparable";
    p_profile = []; (* TODO: build this one somewhere else*)
    p_body = PReads []; (* TODO: give an explicit definition *)
    p_labels = [];
  };
  add_predicate {
    p_name = "result_finite_float";
    p_profile = []; (* TODO: build this one somewhere else*)
    p_body = PReads []; (* TODO: give an explicit definition *)
    p_labels = [];
  }

let () = Logic_env.Builtins.extend init_builtins

let warn_once fmt =
    let b = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer b in
    Format.kfprintf
      (function fmt ->
	Format.pp_print_flush fmt ();
	let fmt = Buffer.contents b in
	if not (Hashtbl.mem warn_h (fmt,!Cil.currentLoc))
	then begin
	    Hashtbl.add warn_h (fmt,!Cil.currentLoc) ();
	    Cil.warn "%s" fmt
	  end)
      bfmt fmt

let (log_h:(string,unit) Hashtbl.t) = Hashtbl.create 1789

let log_once fmt =
    let b = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer b in
    Format.kfprintf
      (function fmt ->
	Format.pp_print_flush fmt ();
	let fmt = Buffer.contents b in
	if not (Hashtbl.mem log_h fmt)
	then begin
	    Hashtbl.add log_h (fmt) ();
	    Cil.log "%s" fmt
	  end)
      bfmt fmt


let labels_table l =
  let lbl_tbl = Hashtbl.create 17 in
  let goto_changer = object
  inherit nopCilVisitor
  method vstmt s =
    List.iter (function
                 | Label (l,_,_) -> Hashtbl.add lbl_tbl l s
                 |  _ -> ())
      s.labels;
    DoChildren
  end
  in
  List.iter (fun x -> ignore (visitCilStmt goto_changer x)) l;
  lbl_tbl

let update_gotos sid_tbl block =
  let goto_changer = object
  inherit nopCilVisitor
  method vstmt s = match s.skind with
  | Goto(sref,loc) ->
      (try
(*      Format.printf "Found goto %d@\n" !sref.sid;*)
      let new_stmt = Cilutil.StmtMap.find !sref sid_tbl in
(*      Format.printf "Resolved goto %d@\n" !sref.sid;*)
      ChangeTo (mkStmt (Goto (ref new_stmt,loc)))
      with Not_found -> DoChildren)
  | _ -> DoChildren
  end
  in
  visitCilBlock goto_changer block

let compact_body fundec =
  let labels_moved = ref false in
  let stmt_map = ref Cilutil.StmtMap.empty in
  let add_labels s l old_stmt =
    stmt_map := Cilutil.StmtMap.add old_stmt s !stmt_map ;
    s.labels <- l @ s.labels;
    labels_moved := true
  in
  let docompact(s1,s2) = match s1.skind, s2.skind with
      (* remove skip *)
    | Instr (Skip _), _ when s1.labels = [] ->
        if debug then prerr_endline "[compact_body] skip/_";
        Some [s2]
    | _, Instr (Skip _) when s2.labels = [] ->
        if debug then prerr_endline "[compact_body] _/skip";
        Some [s1]
      (* collapse useless blocks *)
    | Block b1, Block b2 when b1.battrs = b2.battrs && (s2.labels = [] || b2.bstmts <> []) ->
        if debug then prerr_endline "[compact_body] blck/blck";
        if s2.labels <> [] then begin
          let fst_b2 = List.hd b2.bstmts in
          add_labels fst_b2 s2.labels s2
        end;
        b1.bstmts <- b1.bstmts@b2.bstmts;
        Some [s1]
    | Block b, _ when b.battrs = [] ->
        if debug then prerr_endline "[compact_body] blck/instr";
        b.bstmts <- b.bstmts@[s2];
        Some [s1]
    | _, Block b when b.battrs = [] && (s2.labels = [] || b.bstmts <> []) ->
        if debug then prerr_endline "[compact_body] instr/blck";
        if s2.labels <> [] then begin
          let fst_b = List.hd b.bstmts in
          add_labels fst_b s2.labels s2
        end;
        b.bstmts <- s1::b.bstmts;
        Some [s2]
    | _ -> None
  in
  let block = fundec.sbody in
    (* Je pense que peepHole2 ne fonctionne pas correctement.
    * Peut-être serait-il mieux de faire notre propre fonction...
    * en plus, ça permettrait de combiner peepHole1 + peepHole2.
    * Exemple : peepHole2 ( { { x = 1; skip; } return x; } )
    *         -> { { x = 1; skip; return x; } }
    * Anne. 12/07/2007. *)
  block.bstmts <- peepHole2 docompact block.bstmts;
  let block =
    if !labels_moved then update_gotos !stmt_map block
    else block
  in let rec get_stmts b = match b.bstmts with
    | ({skind=Block blk}) :: [] ->
        if debug then prerr_endline "[compact_body] block(block)";
        get_stmts blk
    | _ -> b.bstmts
  in fundec.sbody.bstmts <- get_stmts block


let current_stmt_tbl =
  let s = Stack.create () in
  Stack.push Kglobal s;
  s

let start_stmt ki =
  Stack.push ki current_stmt_tbl

let end_stmt () =
  try
    ignore (Stack.pop current_stmt_tbl)
  with Stack.Empty -> assert false

let current_stmt () =
  try
    Stack.top current_stmt_tbl
  with Stack.Empty -> assert false


type syntactic_context =
  | SyNone
  | SyBinOp of Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval

let syntactic_context = ref SyNone
let set_syntactic_context e =
 (* (match e with
  | SyBinOp (e1,e2) ->
      ignore
        (Cil.warn
           "New binary context: %a %a\n"
           Cil.d_exp e1
           Cil.d_exp e2)
  | SyUnOp e ->
      ignore
        (Cil.warn
           "New unary context: %a\n"
           Cil.d_exp e)
  | SyMem e ->
      ignore
        (Cil.warn
           "New mem context: %a\n"
           Cil.d_lval e)
  | SyNone ->       ignore
        (Cil.warn
           "New null context\n"));*)
  syntactic_context := e

let get_syntactic_context () = current_stmt (),!syntactic_context

type alarm_behavior = Aignore | Alog | Acall of (unit -> unit)
type warn_mode = {unspecified:alarm_behavior; others: alarm_behavior;
                  imprecision_tracing:alarm_behavior}

let warn_all_mode =
  {unspecified=Alog; others=Alog; imprecision_tracing=Alog}
let warn_none_mode =
  {unspecified=Aignore; others=Aignore; imprecision_tracing=Aignore}


let warn_div warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
	match get_syntactic_context () with
	| _,SyNone -> ()
	| _,SyUnOp _ | _,SyMem _ -> assert false
	| ki,SyBinOp ((Div|Mod),_,exp_d) ->
	    let lexpr = Logic_const.expr_to_term exp_d in
	    let annotation =
              Logic_const.new_code_annotation
		(AAssert ([],
			  Logic_const.unamed (Prel (Rneq,lexpr, lzero())))) in
	    if Alarms.register ki Alarms.Division_alarm annotation then
              Cil.warn
		"division by zero: %a" !Ast_printer.d_code_annotation annotation
	|_,SyBinOp (_,_,_) -> assert false
      end

let warn_shift warn_mode size =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,SyUnOp _ | _,SyMem _ -> assert false
    | ki,SyBinOp ((Shiftrt | Shiftlt),_,exp_d) ->
	let lexpr = Logic_const.expr_to_term exp_d in
	let annotation =
          Logic_const.new_code_annotation
            (AAssert
		([],
		 Logic_const.pand
                    (Logic_const.unamed (Prel (Rge,lexpr, lzero())),
                    Logic_const.unamed
                      (Prel (Rlt,lexpr, lconstant (Int64.of_int size))))))
	in
	if Alarms.register ki Alarms.Shift_alarm annotation then
          Cil.warn "invalid shift: %a" !Ast_printer.d_code_annotation annotation;
	()
    | _,SyBinOp(_,_,_) ->
	assert false
      end

let warn_mem warn_mode msg =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin

    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,SyBinOp _ | _,SyUnOp _ -> assert false
    | ki,SyMem lv_d ->
        let term = mkAddrOrStartOf lv_d in
        let lexpr = Logic_const.expr_to_tsets_elem term in
        let annotation =
          Logic_const.new_code_annotation
            (AAssert ([],
		      Logic_const.unamed (Pvalid (TSSingleton lexpr)))) in
        if Alarms.register ki Alarms.Memory_alarm annotation then
          Cil.warn
            "out of bounds %s. @[%a@]" msg !Ast_printer.d_code_annotation annotation;
        ( match lv_d with
          | Mem _,_ | _, (Index _ | Field _) -> ()
          | _ -> assert false)
      end

let warn_index warn_mode msg =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,SyMem _
    | _,SyUnOp _ -> assert false
    | ki ,SyBinOp (IndexPI,e1,e2) ->
        let lexpr = Logic_const.expr_to_term e1 in
        let rexpr = Logic_const.expr_to_term e2 in
        let p0 = Logic_const.prel(Rle, lzero(), lexpr) in
        let p1 = Logic_const.prel(Rlt, lexpr, rexpr) in
        let p = Logic_const.pand (p0,p1) in
        let annotation = Logic_const.new_code_annotation (AAssert ([],p)) in
        if Alarms.register ki Alarms.Memory_alarm annotation then
          Cil.warn
            "%s out of bounds index. @[%a@]"
            msg !Ast_printer.d_code_annotation annotation
    | _,SyBinOp(_,_,_) ->
        assert false
      end

let warn_mem_read warn_mode = warn_mem warn_mode "read"
let warn_mem_write warn_mode = warn_mem warn_mode "write"

let comparable_pointers t1 t2 =
  let pi = Logic_env.find_predicate "pointer_comparable" in
  Papp (pi, [], [t1;t2])

let warn_pointer_comparison warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,SyUnOp _ | _,SyMem _ -> assert false
  | ki,SyBinOp ((Eq|Ne|Ge|Le|Gt|Lt),exp_l,exp_r) ->
      let lexpr_l = Logic_const.expr_to_term exp_l in
      let lexpr_r = Logic_const.expr_to_term exp_r in
      let annotation =
        Logic_const.new_code_annotation
          (AAssert ([],
		    Logic_const.unamed (comparable_pointers lexpr_l lexpr_r))) in
      if Alarms.register ki Alarms.Pointer_compare_alarm annotation then
        ignore
          (Cil.warn
             "pointer comparison: %a" !Ast_printer.d_code_annotation annotation)
  | _,SyBinOp(_,_,_) ->
      assert false
end

let result_nan_infinite _op t1 t2 =
  let pi = Logic_env.find_predicate "result_finite_float" in
  Papp (pi, [], [t1;t2])

let warn_result_nan_infinite warn_mode =
  match warn_mode.others with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
  match get_syntactic_context () with
  | _,SyNone -> ()
  | _,SyUnOp _ | _,SyMem _ -> assert false
  | ki,SyBinOp (_op, exp_l, exp_r) ->
      let lexpr_l = Logic_const.expr_to_term exp_l in
      let lexpr_r = Logic_const.expr_to_term exp_r in
      let op = lexpr_l in (* TODO: use op *)
      let annotation =
        Logic_const.new_code_annotation
          (AAssert ([],
		    Logic_const.unamed (result_nan_infinite op lexpr_l lexpr_r))) in
      if Alarms.register ki Alarms.Result_is_nan_or_infinite_alarm annotation
      then
        ignore
          (Cil.warn "float operation: %a" !Ast_printer. d_code_annotation annotation)
end

let warn_uninitialized warn_mode =
  match warn_mode.unspecified with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,SyBinOp _ | _,SyUnOp _ -> assert false
    | _,SyMem lv_d ->
        warn_once "(TODO: emit a proper alarm) accessing uninitialized left-value: %a" d_lval lv_d
      end

let warn_escapingaddr warn_mode =
  match warn_mode.unspecified with
    Aignore -> ()
  | Acall f -> f()
  | Alog ->
      begin
    match get_syntactic_context () with
    | _,SyNone -> ()
    | _,SyBinOp _ | _,SyUnOp _ -> assert false
    | _,SyMem lv_d ->
        warn_once "(TODO: emit a proper alarm) accessing left-value that contains escaping addresses : %a" d_lval lv_d
      end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
