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

(* $Id: globals.ml,v 1.26 2008/06/10 17:32:36 uid528 Exp $ *)

open Cil_types
open Db_types
open Cil
open Ast_info

(* ************************************************************************* *)
(** {2 Global variables} *)
(* ************************************************************************* *)

module Vars = struct

  include
    Kernel_computation.VarinfoHashtbl
    (Kernel_datatype.InitInfo)
    (struct
       let name = Project.Computation.Name.make "Globals.Vars"
       let dependencies = [ Cil_state.self ]
       let size = 17
     end)

  exception AlreadyExists of varinfo * initinfo

  let add vi info =
    ignore
      (memo
	 ~change:(fun info -> raise (AlreadyExists(vi, info)))
	 (fun _ -> info)
	 vi)

  let add_decl vi = add vi { init = None }

end

(* ************************************************************************* *)
(** {2 Functions} *)
(* ************************************************************************* *)

module Functions = struct

  module State =
    Kernel_computation.VarinfoHashtbl
      (Kernel_datatype.KernelFunction)
      (struct
	 let name = Project.Computation.Name.make "Functions"
	 let dependencies = [ Cil_state.self ]
	 let size = 17
       end)

  let init_kernel_function f spec =
    { fundec = f; return_stmt = None;
      spec = {spec with spec_variant = spec.spec_variant};
      stmts_graph = None
    }

  let register_declaration action spec v l =
    let args =
      try Some (getFormalsDecl v.vid)
      with Not_found ->
        try
          setFormalsDecl v.vid v.vtype;
          Some (getFormalsDecl v.vid)
        with Not_found ->
          None (* function with 0 arg. See
                  setFormalsDecl code for details *)
    in
    action
      (fun v -> init_kernel_function (Declaration(spec, v, args, l)) spec)
      v

  let add_declaration = register_declaration State.memo

  let replace_by_declaration =
    register_declaration (fun f v -> State.replace v (f v))

  let replace_by_definition spec f l =
    State.replace f.svar (init_kernel_function (Definition (f, l)) spec)

  let add f =
    match f with
    | Definition (n, _) ->
        if Cmdline.Debug.get () > 0 then
          Format.printf "Register %a with specification \"%a\"@\n"
            Ast_info.pretty_vname n.svar !Ast_printer.d_funspec n.sspec;
        (try
           let my_spec = (State.find n.svar).spec in
           Logic_const.merge_funspec n.sspec my_spec
         with Not_found ->
	   ());
        State.replace n.svar (init_kernel_function f n.sspec)
    | Declaration (spec, v,_,_) ->
	if Cmdline.Debug.get () > 0 then
          Format.printf "Register %a with specification \"%a\"@\n"
            Ast_info.pretty_vname v !Ast_printer.d_funspec spec;
	State.replace v (init_kernel_function f spec)

  let iter f = State.iter (fun _ -> f)
  let fold f = State.fold (fun _ -> f)
  let iter_on_fundecs f =
    iter
      (fun kf -> match kf.fundec with
       | Definition (fundec,_) -> f fundec
       | Declaration _ -> ())

  let get vi =
    if not (is_function_type vi) then raise Not_found;
    let add v = add_declaration (empty_funspec ()) v locUnknown in
    State.memo add vi

  (* Similar to [Cil.getGlobInit], except it registers the newly created
   * function
   *)
  let get_glob_init ?(main_name="main") (fl: file) =
    match fl.globinit with
      | Some f -> get f.svar
      | None ->
	  (* Create a function by calling [Cil.getGlobInit] and register it *)
	  let gif = getGlobInit ~main_name fl in
	  add (Definition(gif,locUnknown));
	  get gif.svar

  exception Found_kf of kernel_function

  let find_by_name fct_name =
    let f kf =
      if Function.get_name kf.fundec = fct_name then raise (Found_kf kf)
    in
    try
      iter f;
      raise Not_found
    with Found_kf kf ->
      kf

  let find_def_by_name fct_name =
    let f kf =
      if Function.is_definition kf.fundec
	&& Function.get_name kf.fundec = fct_name
      then
	raise (Found_kf kf)
    in
    try
      iter f;
      raise Not_found
    with Found_kf kf -> kf

  let find_englobing_kf ki =
    match ki with
    | Kglobal -> None
    | Kstmt s ->
	try
          iter
            (fun kf ->
               match kf.fundec with
               | Definition (fundec,_) ->
                   if List.exists
                     (fun sa -> sa.sid = s.sid)
                     fundec.sallstmts
                   then
                     raise (Found_kf kf)
               | Declaration _ -> ());
          None
	with Found_kf kf ->
          Some kf

end

(* ************************************************************************* *)
(** {2 Global annotations} *)
(* ************************************************************************* *)

module Annotations = struct

  module State =
    Computation.Ref
      (struct
	 include Project.Datatype.Imperative
	   (struct
	      type t =  (Cil_types.global_annotation * bool) list
	      let copy _ = assert false (* TODO *)
	    end)
	 let default = []
       end)
      (struct
	 let name = Project.Computation.Name.make "GlobalAnnotations"
	 let dependencies = [ Cil_state.self ]
       end)

  let self = State.self

  let get_all = State.get

  let add b annot =
    let l = State.get () in
    State.set ((annot, b) :: l)

  let add_user = add false
  let add_generated = add true

  let iter f = List.iter (fun (a, b) -> f a b) (State.get ())

  let replace_all f =
    let l = State.get () in
    State.set (List.map (fun (a, b) -> f a b) l)

end

(* ************************************************************************* *)
(** {2 Globals associated to filename} *)
(* ************************************************************************* *)

module FileIndex = struct

  module S =
    Computation.Hashtbl
      (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
      (Project.Datatype.Imperative
	 (struct
	    type t = global list
	    let copy _ = assert false (* TODO: deep copy *)
	  end))
      (struct
	 let name = Project.Computation.Name.make "FileIndex"
	 let dependencies = [ Cil_state.self ]
	 let size = 7
       end)

  let compute, _ =
    let compute () =
      iterGlobals
        (Cil_state.file ())
        (fun glob ->
	  let file = (fst (get_globalLoc glob)).Lexing.pos_fname in
	   let f = Filename.basename file in
	   if Cmdline.Debug.get () > 0 then
             Format.printf "Indexing in file %s the global in %s: %a@."
               f file !Ast_printer.d_global glob;
	   ignore
	     (S.memo ~change:(fun l -> glob :: l) (fun _ -> [ glob ]) f))
    in
    Computation.apply_once
      (Project.Computation.Name.make "FileIndex.compute")
      [ S.self ]
      compute

  let get_files () =
    compute ();
    S.fold (fun key _ keys ->  key :: keys) []

  let find ~filename =
    compute ();
    List.rev (S.find (Filename.basename filename))

  (** get all global variables as (varinfo, initinfo) list with only one
      occurence of a varinfo *)
  let get_globals ~filename =
    compute ();
    let varinfo_set =
      List.fold_right
        (fun glob acc ->
           let is_glob_varinfo x =
             if x.vglob then
               match x.vtype with
                 | TFun _ -> None
                 | _ -> Some x
             else
               None
           in let is_glob_var v = match v with
             | Cil_types.GVar (vi, _, _) ->
                 is_glob_varinfo vi
             | Cil_types.GVarDecl(_,vi, _) ->
                 is_glob_varinfo vi
             | _ -> None
           in match is_glob_var glob with
             | None -> acc
             | Some vi -> VarinfoSet.add vi acc)
        (S.find filename)
        VarinfoSet.empty
    in
    VarinfoSet.fold (fun vi acc -> (vi, Vars.find vi) :: acc) varinfo_set []

  let get_functions ~filename =
    compute ();
    let varinfo_set =
      List.fold_right
        (fun glob acc ->
           let is_func_varinfo x =
             if x.vglob then
               match x.vtype with
                 | TFun _ -> Some x
                 | _ -> None
             else
               None
           in let is_func v = match v with
             | Cil_types.GVarDecl(_,vi, _) ->
                 is_func_varinfo vi
             | Cil_types.GFun(fundec, _) -> Some (fundec.svar)
             | _ -> None
           in match is_func glob with
             | None -> acc
             | Some vi -> VarinfoSet.add vi acc)
        (S.find filename)
        VarinfoSet.empty
    in
    VarinfoSet.fold (fun vi acc -> Functions.get vi :: acc) varinfo_set []

  let kernel_function_of_local_var_or_param_varinfo x =
    compute ();
    let is_param = ref false in
    let pred vi =
      let pred symb = (x.Cil_types.vid = symb.Cil_types.vid)
      in match vi with
        | Cil_types.GFun (fundec, _) ->
            if List.exists pred fundec.Cil_types.slocals then true
            else if List.exists pred fundec.Cil_types.sformals then
	      (is_param := true; true)
            else false
        | _ -> false
    in
    let file = (fst x.Cil_types.vdecl).Lexing.pos_fname in
    match List.find pred (S.find file) with
    | Cil_types.GFun (fundec, _) ->
	Functions.get fundec.Cil_types.svar, !is_param
    | _ -> assert (false)

end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string

let entry_point () =
  let kf_name, lib = 
    Cmdline.MainFunction.get (),
    Cmdline.LibEntry.get ()
  in
  try Functions.find_def_by_name kf_name, lib
  with Not_found ->
    raise (No_such_entry_point
	     (Format.sprintf "Could not find entry point: %s" kf_name))

let set_entry_point name lib =
  let clear_from_entry_point () =
    let add s sel =
      Project.Selection.add s Kind.Only_Select_Dependencies sel
    in
    let selection = add Cmdline.MainFunction.self Project.Selection.empty in
    let selection = add Cmdline.LibEntry.self selection in
    Project.clear ~only:selection ()
  in

  let has_changed = lib <> Cmdline.LibEntry.get () ||
    name <> Cmdline.MainFunction.get ()
  in
  if has_changed then begin
  Cmdline.MainFunction.unsafe_set name;
  Cmdline.LibEntry.unsafe_set lib;
  clear_from_entry_point ()
  end

let has_entry_point () =
  try ignore (entry_point ()); true with No_such_entry_point _ -> false

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
