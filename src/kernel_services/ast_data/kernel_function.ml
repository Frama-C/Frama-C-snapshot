(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
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
open Cil_datatype

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

let dummy () =
  { fundec = Definition (Cil.emptyFunction "@dummy@", Location.unknown);
    spec = List.hd Funspec.reprs }

let get_vi kf = Ast_info.Function.get_vi kf.fundec
let get_id kf = (get_vi kf).vid
let get_name kf = (get_vi kf).vname

let get_location kf = match kf.fundec with
  | Definition (_, loc) -> loc
  | Declaration (_,vi,_, _) -> vi.vdecl

let get_type kf = (get_vi kf).vtype

let get_return_type kf = Cil.getReturnType (get_type kf)

let get_formals f = match f.fundec with
  | Definition(d, _) -> d.sformals
  | Declaration(_, _, None, _) -> []
  | Declaration(_,_,Some args,_) -> args

let get_locals f = match f.fundec with
  | Definition(d, _) -> d.slocals
  | Declaration(_, _, _, _) -> []

let get_statics f = match f.fundec with
  | Definition (d, _) ->
    let statics = ref [] in
    let local_statics_visitor =
      object
        inherit Cil.nopCilVisitor
        method! vblock b =
          statics := !statics @ b.bstatics;
          Cil.DoChildren
      end
    in
    ignore (Cil.visitCilBlock local_statics_visitor d.sbody);
    !statics
  | Declaration (_, _, _, _) -> []

exception No_Definition
let get_definition kf = match kf.fundec with
  | Definition (f,_) -> f
  | Declaration _ -> raise No_Definition

(* ************************************************************************* *)
(** {2 Kernel functions are comparable} *)
(* ************************************************************************* *)

include Cil_datatype.Kf

(* ************************************************************************* *)
(** {2 Searching} *)
(* ************************************************************************* *)

module Info = struct
  let name = "Kernel_function.DefiningKf"
  let dependencies = [Ast.self]
  let size = 64
end

module DefiningKf = Cil_state_builder.Varinfo_hashtbl (Cil_datatype.Kf) (Info)

let compute_defining_kf () =
  let compute_by_fun kf =
    let formals = get_formals kf
    and locals = get_locals kf in
    List.iter (fun var -> DefiningKf.replace var kf) formals;
    List.iter (fun var -> DefiningKf.replace var kf) locals;
  in
  Globals.Functions.iter compute_by_fun

let find_defining_kf vi =
  if vi.vglob
  then None
  else begin
    if not (DefiningKf.is_computed ()) then compute_defining_kf ();
    try Some (DefiningKf.find vi)
    with Not_found -> None
  end


module Kf =
  State_builder.Option_ref
    (Datatype.Int.Hashtbl.Make(Datatype.Triple(Kf)(Stmt)(Datatype.List(Block))))
    (struct
       let name = "Kernel_function.Kf"
       let dependencies = [ Ast.self ]
     end)

let self = Kf.self

let auxiliary_kf_stmt_state = Kf.self

let compute () =
  Kf.memo
    (fun () ->
       let p = Ast.get () in
       let h = Datatype.Int.Hashtbl.create 97 in
       let visitor = object(self)
         inherit Cil.nopCilVisitor
         val mutable current_kf = None
         val mutable opened_blocks = []
         method kf = match current_kf with None -> assert false | Some kf -> kf
         method! vblock b =
           opened_blocks <- b :: opened_blocks;
           Cil.ChangeDoChildrenPost
             (b,fun b -> opened_blocks <- List.tl opened_blocks; b)
	 method! vstmt s =
	   Datatype.Int.Hashtbl.add h s.sid (self#kf, s, opened_blocks);
	   Cil.DoChildren
	 method! vglob g =
	   begin match g with
	   | GFun (fd, _) ->
	       (try
                  let kf = Globals.Functions.get fd.svar in
	          current_kf <- Some kf;
                with Not_found ->
                  Kernel.fatal "No kernel function for function %a"
                    Cil_datatype.Varinfo.pretty fd.svar)
	   | _ ->
	       ()
	   end;
	   Cil.DoChildren
       end
       in
       Cil.visitCilFile (visitor :> Cil.cilVisitor) p;
       h)

let find_from_sid sid =
  let table = compute () in
  let kf, s, _ = Datatype.Int.Hashtbl.find table sid in
  s, kf

let find_englobing_kf stmt = snd (find_from_sid stmt.sid)

let () = Globals.find_englobing_kf := find_englobing_kf

let blocks_closed_by_edge_aux s1 s2 =
  let table = compute () in
  try
    let _,_,b1 = Datatype.Int.Hashtbl.find table s1.sid in
    let _,_,b2 = Datatype.Int.Hashtbl.find table s2.sid in
    let pp_block fmt b =
      Pretty_utils.pp_list ~sep:"@\n" Cil_printer.pp_block fmt b
    in
    Kernel.debug ~dkey:Kernel.dkey_kf_blocks
      "Blocks opened for stmt %a@\n%a@\nblocks opened for stmt %a@\n%a"
      Cil_printer.pp_stmt s1 pp_block b1 Cil_printer.pp_stmt s2 pp_block b2;
    let rec aux acc = function
        [] -> acc
      | inner_block::others ->
        if List.memq inner_block b2 then acc
        else aux (inner_block::acc) others
    in
    let res = aux [] b1 in
    Kernel.debug ~dkey:Kernel.dkey_kf_blocks "Result:@\n%a" pp_block res;
    res
  with Not_found ->
    (* Invalid statement, or incorrectly filled table 'Kf' *)
    Kernel.fatal "Unknown statement sid:%d or sid:%d" s1.sid s2.sid

let blocks_closed_by_edge s1 s2 =
  if not (List.exists (Stmt.equal s2) s1.succs) then
    raise (Invalid_argument "Kernel_function.blocks_closed_by_edge");
  blocks_closed_by_edge_aux s1 s2

let blocks_opened_by_edge s1 s2 =
  if not (List.exists (Stmt.equal s2) s1.succs) then
    raise (Invalid_argument "Kernel_function.blocks_opened_by_edge");
  blocks_closed_by_edge_aux s2 s1


let find_enclosing_block s =
  let table = compute () in
  let (_,_,b) = Datatype.Int.Hashtbl.find table s.sid in
  List.hd b

let () = Globals.find_enclosing_block:= find_enclosing_block

let find_all_enclosing_blocks s =
   let table = compute () in
  let (_,_,b) = Datatype.Int.Hashtbl.find table s.sid in b

let find_stmt_in_block b s =
  let has_stmt l =
    List.exists (fun (s',_,_,_,_) -> Cil_datatype.Stmt.equal s s') l
  in
  let rec aux = function
    | [] ->
      Kernel.fatal "statement %a is not inside block@\n%a"
        Cil_printer.pp_stmt s Cil_printer.pp_block b
    | s' :: _ when Cil_datatype.Stmt.equal s s' -> s'
    | { skind = UnspecifiedSequence l } as s':: _ when has_stmt l -> s'
    | _ :: l -> aux l
  in aux b.bstmts

let find_stmt_of_block outer inner =
  let rec is_stmt_of_block s =
    match s.skind with
    | Block b -> b == inner
    | If (_,b1,b2,_) -> b1 == inner || b2 == inner
    | Switch (_,b,_,_) -> b == inner
    | Loop (_,b,_,_,_) -> b == inner
    | UnspecifiedSequence l -> is_stmt_of_unspecified l
    | TryCatch (b, l, _) ->
      b == inner || List.exists (fun (_,b) -> b == inner) l
    | TryFinally (b1,b2,_) -> b1 == inner || b2 == inner
    | TryExcept (b1,_,b2,_) -> b1 == inner || b2 == inner
    | _ -> false
  and is_stmt_of_unspecified l =
    List.exists (fun (s,_,_,_,_) -> is_stmt_of_block s) l
  in
  try
    List.find is_stmt_of_block outer.bstmts
  with Not_found ->
    Kernel.fatal "inner block@\n%a@\nis not a direct child of outer block@\n%a"
      Cil_printer.pp_block inner Cil_printer.pp_block outer

let find_enclosing_stmt_in_block b s =
  let blocks = find_all_enclosing_blocks s in
  let rec aux prev l =
    match l, prev with
    | [], _ ->
      Kernel.fatal "statement %a is not part of block@\n%a"
        Cil_printer.pp_stmt s Cil_printer.pp_block b
    | b' :: _, None when b' == b -> find_stmt_in_block b s
    | b' :: _, Some prev when b' == b -> find_stmt_of_block b prev
    | b' :: l, _ -> aux (Some b') l
  in
  aux None blocks

let is_between b s1 s2 s3 =
  let s1 = find_enclosing_stmt_in_block b s1 in
  let s2 = find_enclosing_stmt_in_block b s2 in
  let s3 = find_enclosing_stmt_in_block b s3 in
  let rec aux has_s1 l =
    match l with
    | [] ->
      Kernel.fatal
        "Unexpected end of block while looking for %a"
        Cil_printer.pp_stmt s3
    | s :: _ when Cil_datatype.Stmt.equal s s3 -> false
    | s :: l when Cil_datatype.Stmt.equal s s1 -> aux true l
    | s :: _ when Cil_datatype.Stmt.equal s s2 -> has_s1
    | _ :: l -> aux has_s1 l
  in
  aux false b.bstmts

let common_block s1 s2 =
  let kf1 = find_englobing_kf s1 in
  let kf2 = find_englobing_kf s2 in
  if not (equal kf1 kf2) then
    Kernel.fatal
      "cannot find a common block for statements occurring \
       in two distinct functions";
  let b1 = find_all_enclosing_blocks s1 in
  let b2 = find_all_enclosing_blocks s2 in
  let rec aux last l1 l2 =
    match l1,l2 with
    | [], _ | _, [] -> last
    | b1 :: l1, b2 :: l2 when b1 == b2 -> aux b1 l1 l2
    | _ :: _, _ :: _ -> last
  in
  match List.rev b1, List.rev b2 with
  | [], _ | _, [] ->
    Kernel.fatal "Statement not contained in any block"
  | b1 :: l1, b2 :: l2 when b1 == b2 -> aux b1 l1 l2
  | _ :: _, _ :: _ ->
    Kernel.fatal
      "Statements do not share their function body as outermost common block"

let () = Globals.find_all_enclosing_blocks := find_all_enclosing_blocks

let stmt_in_loop kf stmt =
  let module Res = struct exception Found of bool end in
  let vis = object
    inherit Cil.nopCilVisitor
    val is_in_loop = Stack.create ()
    method! vstmt s =
      match s.skind with
        | Loop _ -> 
          Stack.push true is_in_loop;
          if Cil_datatype.Stmt.equal s stmt then raise (Res.Found true);
          Cil.DoChildrenPost (fun s -> ignore (Stack.pop is_in_loop); s)
        | _ when Cil_datatype.Stmt.equal s stmt ->
          raise (Res.Found (Stack.top is_in_loop))
        | _  -> Cil.DoChildren
    initializer Stack.push false is_in_loop
  end
  in
  try
    ignore
      (Cil.visitCilFunction vis (get_definition kf));
    false
  with
    | Res.Found f -> f
    | No_Definition -> false (* Not the good kf obviously. *)

let find_enclosing_loop kf stmt =
  let module Res = struct exception Found of Cil_types.stmt end in
  let vis = object
    inherit Cil.nopCilVisitor
    val loops = Stack.create ()
    method! vstmt s =
      match s.skind with
        | Loop _ ->
          Stack.push s loops;
          Cil.DoChildrenPost (fun s -> ignore (Stack.pop loops); s)
        | _ when Cil_datatype.Stmt.equal s stmt ->
          raise (Res.Found (Stack.top loops))
        | _ -> Cil.DoChildren
  end
  in
  try
    (match stmt.skind with
      | Loop _ -> stmt
      | _ ->
        ignore
          (Cil.visitCilFunction vis (get_definition kf));
        raise Not_found)
  with
    | No_Definition -> raise Not_found (* Not the good kf obviously. *)
    | Stack.Empty -> raise Not_found (* statement outside of a loop *)
    | Res.Found s -> s

exception Got_return of stmt
exception No_Statement

module ReturnCache =
  Cil_state_builder.Kernel_function_hashtbl
    (Cil_datatype.Stmt)
    (struct
      let name = "Kernel_function.ReturnCache"
      let dependencies = [Ast.self]
      let size = 43
    end)

let () = Ast.add_monotonic_state ReturnCache.self

let clear_sid_info () = Kf.clear (); ReturnCache.clear ()
let () = Cfg.clear_sid_info_ref := clear_sid_info

let find_return kf =
  try
    ReturnCache.find kf
  with Not_found ->
    let find_return fd =
      let visitor = object
        inherit Cil.nopCilVisitor
        method! vstmt s =
          match s.skind with
          | Return _ -> raise (Got_return s)
          | _ -> Cil.DoChildren
      end
      in
      try
        ignore (Cil.visitCilFunction (visitor :> Cil.cilVisitor) fd);
        Kernel.fatal "Function %a does not have a return statement" pretty kf
      with Got_return s ->
        ReturnCache.add kf s; s
    in
    try
      find_return (get_definition kf)
    with No_Definition -> raise No_Statement

let get_stmts kf =
  try (get_definition kf).sbody.bstmts with No_Definition | Not_found -> []

let find_first_stmt kf = match get_stmts kf with
  | [] -> raise No_Statement
  | s :: _ -> s

let () = Globals.find_first_stmt := find_first_stmt

let label_table kf =
  match kf.fundec with
  | Declaration _ -> Datatype.String.Map.empty
  | Definition (fundec,_) ->
    let label_finder = object(self)
      inherit Cil.nopCilVisitor
      val mutable labels = Datatype.String.Map.empty
      method all_labels = labels
      method new_label stmt lbl =
        match lbl with
        | Label (l,_,_) ->
          labels <- Datatype.String.Map.add l (ref stmt) labels
        | Case _ -> ()
        | Default _ ->
          (* Kept for compatibility with old implementation of find_label,
             but looks quite suspicious. *)
          labels <- Datatype.String.Map.add "default" (ref stmt) labels

      method! vstmt s =
        List.iter (self#new_label s) s.labels;
        Cil.DoChildren

      method! vexpr _ = Cil.SkipChildren
      method! vtype _ = Cil.SkipChildren
      method! vinst _ = Cil.SkipChildren
    end
    in
    ignore (Cil.visitCilFunction (label_finder:>Cil.cilVisitor) fundec);
    label_finder#all_labels

let find_all_labels kf =
  let labels = label_table kf in
  Datatype.String.(
    Map.fold (fun lab _ acc -> Set.add lab acc) labels Set.empty)

let find_label kf label =
  let labels = label_table kf in
  Datatype.String.Map.find label labels

let get_called fct = match fct.enode with
  | Lval (Var vkf, NoOffset) -> 
      (try Some (Globals.Functions.get vkf)
       with Not_found -> None)
  | _ -> None

(* ************************************************************************* *)
(** {2 CallSites} *)
(* ************************************************************************* *)

module CallSite = Datatype.Pair(Cil_datatype.Kf)(Stmt)
module CallSites = Cil_datatype.Kf.Hashtbl
module KfCallers =
  State_builder.Option_ref(CallSites.Make(Datatype.List(CallSite)))
    (struct
      let name = "Kernel_function.KfCallers"
      let dependencies = [ Ast.self ]
     end)

let called_kernel_function fct =
  match fct.enode with
    | Lval (Var vinfo,NoOffset) ->
        (try Some(Globals.Functions.get vinfo) with Not_found -> None)
    | _ -> None

class callsite_visitor hmap = object (self)
  inherit Cil.nopCilVisitor
  val mutable current_kf = None
  method private kf = match current_kf with None -> assert false | Some kf -> kf

  (* Go into functions *)
  method! vglob = function
    | GFun(fd,_) ->
        current_kf <- Some(Globals.Functions.get fd.svar) ;
        Cil.DoChildren
    | _ -> Cil.SkipChildren

  (* Inspect stmt calls *)
  method! vstmt stmt =
    let add_call callee =
      let sites = try CallSites.find hmap callee with Not_found -> [] in
      CallSites.replace hmap callee ((self#kf,stmt)::sites)
    in
    match stmt.skind with
      | Instr(Call(_,fct,_,_)) ->
        Extlib.may add_call (called_kernel_function fct); Cil.SkipChildren
      | Instr (Local_init (_, ConsInit(f,_,_),_)) ->
        add_call (Globals.Functions.get f); Cil.SkipChildren
      | Instr _ -> Cil.SkipChildren
      | _ -> Cil.DoChildren

  (* Skip many other things ... *)
  method! vexpr _ = Cil.SkipChildren
  method! vtype _ = Cil.SkipChildren
  method !vannotation _ = Cil.SkipChildren
  method !vcode_annot _ = Cil.SkipChildren
  method !vbehavior _ = Cil.SkipChildren

end

let compute_callsites () =
  let ast = Ast.get () in
  let hmap = CallSites.create 97 in
  let visitor = new callsite_visitor hmap in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) ast ;
  hmap

let find_syntactic_callsites kf =
  let table = KfCallers.memo compute_callsites in
  try CallSites.find table kf
  with Not_found -> []

let local_definition kf vi =
  let locals = get_locals kf in
  if not vi.vdefined ||
     not (List.exists (fun vi' -> Cil_datatype.Varinfo.equal vi vi') locals)
  then
    Kernel.fatal
      "%a is not a defined local variable of %a"
      Cil_datatype.Varinfo.pretty vi pretty kf;
  try
    List.find
      (fun s ->
         match s.skind with
         | Instr (Local_init (vi', _, _)) -> Cil_datatype.Varinfo.equal vi vi'
         | _ -> false)
      (get_definition kf).sallstmts
  with Not_found -> assert false (* cannot occur on well-formed AST. *)

let var_is_in_scope stmt vi =
  let blocks = find_all_enclosing_blocks stmt in
  let is_def_above b =
    let sdef = local_definition (find_englobing_kf stmt) vi in
    match b.bstmts with
    | [] -> assert false (* at least contains stmt *)
    | sfst :: _ ->
      (* if sfst is sdef (or an unspecified sequence containing sdef), it
         is necessarily above stmt. Otherwise, we can rely on is_between to
         check that. *)
      Cil_datatype.Stmt.equal sfst (find_enclosing_stmt_in_block b sdef) ||
      is_between b sfst sdef stmt
  in
  List.exists
    (fun b ->
       List.exists (Cil_datatype.Varinfo.equal vi) b.blocals &&
       (not vi.vdefined || is_def_above b)
    )
    blocks

(* ************************************************************************* *)
(** {2 Checkers} *)
(* ************************************************************************* *)

let is_definition kf =
  match kf.fundec with
  | Definition _ -> true
  | Declaration _ -> false

let is_entry_point kf =
  let main, _ = Globals.entry_point () in
  equal kf main

let returns_void kf =
  let result_type,_,_,_ = Cil.splitFunctionType (get_type kf) in
  match Cil.unrollType result_type with
  | TVoid _ -> true
  | _ -> false

(* ************************************************************************* *)
(** {2 Membership of variables} *)
(* ************************************************************************* *)

let is_formal v kf =  List.exists (fun vv -> v.vid = vv.vid) (get_formals kf)

let get_formal_position v kf =
  Extlib.find_index (fun vv -> v.vid = vv.vid) (get_formals kf)

let is_local v kf = match kf.fundec with
    | Definition(fd, _) -> Ast_info.Function.is_local v fd
    | Declaration _ -> false

let is_formal_or_local v kf =
  (not v.vglob) && (is_formal v kf || is_local v kf)

(* ************************************************************************* *)
(** {2 Collections} *)
(* ************************************************************************* *)

module Make_Table = State_builder.Hashtbl(Cil_datatype.Kf.Hashtbl)

module Hptset = struct
  include Hptset.Make
    (Cil_datatype.Kf)
    (struct let v = [ [ ] ] end)
    (struct let l = [ Ast.self ] end)
  let () = Ast.add_monotonic_state self
  let () = Ast.add_hook_on_update clear_caches
end

(* ************************************************************************* *)
(** {2 Setters} *)
(* ************************************************************************* *)

let register_stmt kf s b =
  let tbl = try Kf.get () with Not_found -> assert false in
  Datatype.Int.Hashtbl.add tbl s.sid (kf,s,b)

(* ************************************************************************* *)
(** {2 Memoized get_global} *)
(* ************************************************************************* *)

module Get_global =
  Make_Table
    (Cil_datatype.Global)
    (struct
      let name = "Kernel_function.get_global"
      let size = 8
      let dependencies = [ Globals.Functions.self ]
     end)

let compute_get_global () =
  Cil.iterGlobals
    (Ast.get ())
    (function
    | GFun({ svar = vi }, _) | GFunDecl(_, vi, _) as g
         when Ast.is_def_or_last_decl g ->
      let kf =
        try Globals.Functions.get vi
        with Not_found ->
          Kernel.fatal
            "[Kernel_function.compute_get_global] unknown function %a"
            Cil_datatype.Varinfo.pretty vi
      in
      Get_global.replace kf g
    | _ -> ())

let get_global =
  Get_global.memo
    (fun kf ->
      compute_get_global ();
      try Get_global.find kf
      with Not_found ->
        Kernel.fatal
          "[Kernel_function.get_global] unknown function %a"
          pretty kf)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
