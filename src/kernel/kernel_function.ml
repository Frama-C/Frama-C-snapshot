(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

open Extlib
open Cil_types
open Cil_datatype

(* ************************************************************************* *)
(** {2 Getters} *)
(* ************************************************************************* *)

let dummy () =
  { fundec =
      Definition (Cil.emptyFunction "@dummy@", Location.unknown);
    return_stmt = None;
    spec = Cil.empty_funspec ()}

let get_vi kf = Ast_info.Function.get_vi kf.fundec
let get_id kf = (get_vi kf).vid
let get_name kf = (get_vi kf).vname

let get_location kf = match kf.fundec with
  | Definition (_, loc) -> loc
  | Declaration (_,vi,_, _) -> vi.vdecl

let get_type kf = (get_vi kf).vtype

let get_return_type kf = Cil.getReturnType (get_type kf)

let get_global f = match f.fundec with
  | Definition (d, loc) -> GFun(d,loc)
  | Declaration (spec, vi, _, loc) -> GVarDecl(spec, vi,loc)

let get_formals f = match f.fundec with
  | Definition(d, _) -> d.sformals
  | Declaration(_, _, None, _) -> []
  | Declaration(_,_,Some args,_) -> args

let get_locals f = match f.fundec with
  | Definition(d, _) -> d.slocals
  | Declaration(_, _, _, _) -> []

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

module Kf =
  State_builder.Option_ref
    (Int_hashtbl.Make(Datatype.Triple(Kf)(Stmt)(Datatype.List(Block))))
    (struct
       let name = "KF"
       let dependencies = [ Ast.self ]
       let kind = `Internal
     end)

let self = Kf.self

let () = 
  State_dependency_graph.Static.add_dependencies
    ~from:Kf.self 
    [ Property_status.self ]

let clear_sid_info () = Kf.clear ()

let compute () =
  Kf.memo
    (fun () ->
       let p = Ast.get () in
       let h = Inthash.create 97 in
       let visitor = object(self)
         inherit Cil.nopCilVisitor
         val mutable current_kf = None
         val mutable opened_blocks = []
         method kf = match current_kf with None -> assert false | Some kf -> kf
         method vblock b =
           opened_blocks <- b :: opened_blocks;
           Cil.ChangeDoChildrenPost
             (b,fun b -> opened_blocks <- List.tl opened_blocks; b)
	 method vstmt s =
	   Inthash.add h s.sid (self#kf, s, opened_blocks);
	   Cil.DoChildren
	 method vglob g =
	   begin match g with
	   | GFun (fd, _) ->
	       let kf = Globals.Functions.get fd.svar in
	       current_kf <- Some kf;
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
  let kf, s, _ = Inthash.find table sid in
  s, kf

let () = Dataflow.stmt_of_sid := (fun sid -> fst (find_from_sid sid))

let find_englobing_kf stmt =
  snd (find_from_sid stmt.sid)

let blocks_closed_by_edge s1 s2 =
  if not (List.exists (Stmt.equal s2) s1.succs) then
    raise (Invalid_argument "Kernel_function.edge_exits_block");
  let table = compute () in
  try
  let _,_,b1 = Inthash.find table s1.sid in
  let _,_,b2 = Inthash.find table s2.sid in
  Kernel.debug ~level:2
    "Blocks opened for stmt %a@\n%a@\nblocks opened for stmt %a@\n%a"
    !Ast_printer.d_stmt s1 
    (Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep !Ast_printer.d_block) b1 
    !Ast_printer.d_stmt s2 
    (Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep !Ast_printer.d_block) b2;
  let rec aux acc = function
      [] -> acc
    | inner_block::others ->
        if List.memq inner_block b2 then acc
        else aux (inner_block::acc) others
  in aux [] b1
  with Not_found ->
    Format.eprintf "Unknown statements:@\n%d: %a@\n%d: %a@."
      s1.sid !Ast_printer.d_stmt s1 s2.sid !Ast_printer.d_stmt s2;
    raise Not_found

let find_enclosing_block s =
  let table = compute () in
  let (_,_,b) = Inthash.find table s.sid in
  List.hd b

let () = Globals.find_enclosing_block:= find_enclosing_block

let find_all_enclosing_blocks s =
   let table = compute () in
  let (_,_,b) = Inthash.find table s.sid in b

exception Got_return of stmt
exception No_Statement

let find_return kf =
  match kf.return_stmt with
  | None ->
      let find_return fd =
        let visitor = object
          inherit Cil.nopCilVisitor as super
          method vstmt s =
            match s.skind with
              | Return _ -> raise (Got_return s)
              | _ -> Cil.DoChildren
        end
        in
        try
          ignore (Cil.visitCilFunction (visitor :> Cil.cilVisitor) fd);
          assert false
        with Got_return s -> s
      in
      (try
         let ki = find_return (get_definition kf) in
         kf.return_stmt <- Some ki;
         ki
       with No_Definition ->
         raise No_Statement)
  | Some ki ->
      ki

let get_stmts kf =
  try (get_definition kf).sbody.bstmts with No_Definition | Not_found -> []

let find_first_stmt kf = match get_stmts kf with
  | [] -> raise No_Statement
  | s :: _ -> s

let () = Globals.find_first_stmt := find_first_stmt

exception Found_label of stmt ref
let find_label kf label =
  match kf.fundec with
  | Declaration _ -> raise Not_found
  | Definition (fundec,_) ->
      let label_finder = object
        inherit Cil.nopCilVisitor
        method vstmt s = begin
          if List.exists
            (fun lbl -> match lbl with
             | Label (s,_,_) -> s = label
             | Case _ -> false
             | Default _ -> label="default")
            s.labels then raise (Found_label (ref s));
          Cil.DoChildren
        end
        method vexpr _ = Cil.SkipChildren
        method vtype _ = Cil.SkipChildren
        method vinst _ = Cil.SkipChildren
      end
      in
      try
        ignore (Cil.visitCilFunction label_finder fundec);
        (* Ok: this is not a code label *)
        raise Not_found
      with Found_label s -> s

(* ************************************************************************* *)
(** {2 CallSites} *)
(* ************************************************************************* *)

module CallSite = Datatype.Pair(Cil_datatype.Kf)(Stmt)
module CallSites = Cil_datatype.Kf.Hashtbl
module KfCallers =
  State_builder.Option_ref(CallSites.Make(Datatype.List(CallSite)))
    (struct
      let name = "Kf.CallSites"
      let dependencies = [ Ast.self ]
      let kind = `Internal
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
  method vglob = function
    | GFun(fd,_) ->
        current_kf <- Some(Globals.Functions.get fd.svar) ;
        Cil.DoChildren
    | _ -> Cil.SkipChildren

  (* Inspect stmt calls *)
  method vstmt stmt =
    match stmt.skind with
      | Instr(Call(_,fct,_,_)) ->
          begin
            match called_kernel_function fct with
              | None -> Cil.SkipChildren
              | Some ckf ->
                  let sites = try CallSites.find hmap ckf with Not_found -> [] in
                  CallSites.replace hmap ckf ((self#kf,stmt)::sites) ;
                  Cil.SkipChildren
          end
      | Instr _ -> Cil.SkipChildren
      | _ -> Cil.DoChildren

  (* Skip many other things ... *)
  method vexpr _ = Cil.SkipChildren
  method vtype _ = Cil.SkipChildren

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

(* ************************************************************************* *)
(** {2 Checkers} *)
(* ************************************************************************* *)

let is_definition kf =
  match kf.fundec with
  | Definition _ -> true
  | Declaration _ -> false

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
(** {2 Specifications} *)
(* ************************************************************************* *)

let populate_spec = Extlib.mk_fun "Kernel_function.populate_spec"

let get_spec ?(populate=true) f =
  if is_definition f || not populate then
    f.spec
  else begin
    (* Do not overwrite an existing assigns clause*)
    !populate_spec f;
    (* Kernel.feedback 
       "Getting spec of %a: %a" pretty f !Ast_printer.d_funspec f.spec; *)
    f.spec
  end

let set_spec kf f =
  let get_ppts kf = Property.ip_of_spec kf Kglobal kf.spec in
  let old = get_ppts kf in
  kf.spec <- f kf.spec;
  Property_status.merge ~old (get_ppts kf)

let () = Globals.Functions.set_spec := set_spec

let postcondition kf k =
  Logic_const.pands
    (List.map
       (fun b -> Ast_info.behavior_postcondition b k)
       (get_spec kf).spec_behavior)

let precondition kf =
  Logic_const.pands
    (List.map
       (fun b -> Ast_info.behavior_precondition b)
       (get_spec kf).spec_behavior)

let code_annotations kf =
  try
    let def = get_definition kf in
    List.fold_left
      (fun acc stmt ->
         Annotations.single_fold_stmt
           (fun a acc -> (stmt, a) :: acc)
           stmt
           acc)
      []
      def.sallstmts
  with No_Definition ->
    []

let internal_function_behaviors kf =
  try
    let def = get_definition kf in
    List.fold_left
      (fun known_names stmt ->
         List.fold_left
           (fun known_names (_bhv,spec) ->
              (List.map (fun x -> x.b_name) spec.spec_behavior) @ known_names)
           known_names
           (Logic_utils.extract_contract
              (List.map
                 Annotations.get_code_annotation
                 (Annotations.get_all_annotations stmt))))
      []
      def.sallstmts
  with No_Definition -> 
    []

let spec_function_behaviors kf =
  List.map (fun x -> x.b_name) (get_spec kf).spec_behavior

let all_function_behaviors kf =
  (internal_function_behaviors kf) @ (spec_function_behaviors kf)

let fresh_behavior_name kf name =
  let existing_behaviors = all_function_behaviors kf in
  let rec aux i =
    let name = name ^ "_" ^ (string_of_int i) in
    if List.mem name existing_behaviors then aux (i+1)
    else name
  in
  if List.mem name existing_behaviors then aux 0 else name

(* ************************************************************************* *)
(** {2 Pretty printer} *)
(* ************************************************************************* *)

let pretty_name =
  Kernel.deprecated
    "Kernel_function.pretty"
    ~now:"Kernel_function.pretty"
    pretty

(* ************************************************************************* *)
(** {2 Collections} *)
(* ************************************************************************* *)

module Make_Table = State_builder.Hashtbl(Cil_datatype.Kf.Hashtbl)

module Hptset = struct
  let pretty_kf = pretty

  include Hptset.Make
  (Cil_datatype.Kf)
  (struct let v = [ [ ] ] end)
  (struct let l = [ Ast.self ] end)

  let pretty fmt = Pretty_utils.pp_iter iter pretty_kf fmt
end

(* ************************************************************************* *)
(** {2 Setters} *)
(* ************************************************************************* *)

let register_stmt kf s b =
  let tbl = try Kf.get () with Not_found -> assert false in
  Inthash.add tbl s.sid (kf,s,b)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
