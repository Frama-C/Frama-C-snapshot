(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: globals.ml,v 1.37 2009-02-13 07:59:29 uid562 Exp $ *)

open Cil_types
open Cilutil
open Db_types
open Cil
open Ast_info

(* ************************************************************************* *)
(** {2 Global variables} *)
(* ************************************************************************* *)

(* redefinition from Kernel_function.ml *)
let get_formals f = match f.fundec with
  | Definition(d, _) -> d.sformals
  | Declaration(_, _, None, _) -> []
  | Declaration(_,_,Some args,_) -> args

let get_locals f = match f.fundec with
  | Definition(d, _) -> d.slocals
  | Declaration(_, _, _, _) -> []

module Vars = struct

  include
      Cil_computation.VarinfoHashtbl
      (Cil_datatype.InitInfo)
    (struct
       let name = "Globals.Vars"
       let dependencies = [ Ast.self ]
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

  let get_astinfo_ref : (Cil_types.varinfo -> string * localisation) ref =
    Extlib.mk_fun "get_astinfo_ref"

  exception Found of varinfo
  let find_from_astinfo name = function
    | VGlobal ->
	(try
	   iter (fun v _ -> if v.vname = name then raise (Found v));
	   invalid_arg ("[find_from_astinfo] global " ^ name ^ "not found")
	 with Found v ->
	   v)
    | VLocal kf ->
	(try
	   List.find (fun v -> v.vname = name) (get_locals kf)
	 with Not_found ->
	   invalid_arg ("[find_from_astinfo] local " ^ name ^ "not found"))
    | VFormal kf ->
	(try
	   List.find (fun v -> v.vname = name) (get_formals kf)
	 with Not_found ->
	   invalid_arg ("[find_from_astinfo] formal " ^ name ^ "not found"))

  let get_astinfo vi = !get_astinfo_ref vi

  let pp_varinfo p fmt v =
    let name, loc = get_astinfo v in
    let pp fmt =
      Format.fprintf fmt "Globals.Vars.find_from_astinfo %S %a" name
	(Type.pp Kernel_type.localisation Type.Call) loc
    in
    Type.par p Type.Call fmt pp

  let () = Type.register_pp Kernel_type.varinfo pp_varinfo

end

(* ************************************************************************* *)
(** {2 Functions} *)
(* ************************************************************************* *)

module Functions = struct

  module KF_Datatype = struct
    include Project.Datatype.Register
      (struct
	 type t = kernel_function
	 let rehash x =
           match x.fundec with
           | Definition _ | Declaration (_, _, None, _)-> x
           | Declaration (_, v, Some args, _) ->
               Cil.unsafeSetFormalsDecl v args;
	       x
	 let descr =
	   Unmarshal.Transform
	     (Unmarshal.Abstract,
	      fun o -> let x : t = Obj.obj o in Obj.repr (rehash x))
	 let copy _ = assert false (* TODO: deep copy *)
	 let name = "kernel_function"
       end)
    let id kf = Ast_info.Function.get_id kf.fundec
    let hash = id
    let equal = (==)
    let compare k1 k2 = Pervasives.compare (id k1) (id k2)
    let pretty fmt kf =
      Ast_info.pretty_vname fmt
	(Ast_info.Function.get_vi kf.fundec)
    let () = register_comparable ~hash ~equal ~compare ()
  end

  module State =
    Cil_computation.VarinfoHashtbl
      (KF_Datatype)
      (struct
	 let name = "Functions"
	 let dependencies = [ Ast.self ]
	 let size = 17
       end)

  let self = State.self

  let init_kernel_function f spec =
    { fundec = f; return_stmt = None;
      spec = {spec with spec_variant = spec.spec_variant};
      stmts_graph = None }

  let register_declaration action spec v l =
    let args =
      try Some (getFormalsDecl v)
      with Not_found ->
        try
          setFormalsDecl v v.vtype;
          Some (getFormalsDecl v)
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
	if Kernel.debug_atleast 1 then
	  Kernel.debug
	    "Register definition %a with specification \"%a\"@\n"
            Ast_info.pretty_vname n.svar !Ast_printer.d_funspec n.sspec ;
        (try
           let my_spec = (State.find n.svar).spec in
           Logic_utils.merge_funspec n.sspec my_spec
         with Not_found ->
	   ());
        State.replace n.svar (init_kernel_function f n.sspec);
	Parameters.MainFunction.set_possible_values
	  (n.svar.vname :: Parameters.MainFunction.get_possible_values ())
    | Declaration (spec, v,_,_) ->
	if Kernel.debug_atleast 1 then
          Kernel.debug
	    "Register declaration %a with specification \"%a\"@\n"
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
    let add v = add_declaration (empty_funspec ()) v Cilutil.locUnknown in
    State.memo add vi

  let get_params kf =
    match kf.fundec with
      | Definition(f,_loc) -> f.sformals
      | Declaration(_spec,_v,params,_loc) ->
	  match params with None -> [] | Some ls -> ls

  let get_vi kf =
    match kf.fundec with
      | Definition(f,_loc) -> f.svar
      | Declaration(_spec,v,_params,_loc) -> v

  let get_glob_init ?(main_name="main") (fl: file) =
    match fl.globinit with
    | Some f -> get f.svar
    | None ->
	(* Create a function by calling [Cil.getGlobInit] and register it *)
	let gif = getGlobInit ~main_name fl in
	add (Definition (gif, Cilutil.locUnknown));
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


  exception Found of kernel_function
  let get_astinfo vi =
    vi.vname,
    if vi.vglob then VGlobal
    else begin
      if vi.vformal then begin
	try
	  iter
	    (fun kf ->
	       if List.exists (fun v -> v.vname = vi.vname) (get_formals kf)
	       then raise (Found kf));
	  assert false
	with Found kf ->
	  VFormal kf
      end else begin
	try
	  iter
	    (fun kf ->
	       if List.exists (fun v -> v.vname = vi.vname) (get_locals kf)
	       then raise (Found kf));
	  assert false
	with Found kf ->
	  VLocal kf
      end
    end

  let () = Vars.get_astinfo_ref := get_astinfo

end

(* ************************************************************************* *)
(** {2 Global annotations} *)
(* ************************************************************************* *)

module Annotations = struct

  let name = "GlobalAnnotations"

  module State =
    Computation.Ref
      (struct
	 include Project.Datatype.Imperative
	   (struct
	      type t =  (Cil_types.global_annotation * bool) list
	      let copy _ = assert false (* TODO *)
	      let name = name
	    end)
	 let default () = []
       end)
      (struct
	 let name = name
	 let dependencies = [ Ast.self ]
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

  let name = "FileIndex"

  module S =
    Computation.Hashtbl
      (struct type t = string let hash = Hashtbl.hash let equal = (=) end)
      (Project.Datatype.Imperative
	 (struct
	    type t = string * (global list)
	    let copy _ = assert false (* TODO: deep copy *)
	    let name = name
	  end))
      (struct
	 let name = name
	 let dependencies = [ Ast.self ]
	 let size = 7
       end)

  let compute, self =
    let compute () =
      iterGlobals
        (Ast.get ())
        (fun glob ->
	  let file = (fst (Cilutil.get_globalLoc glob)).Lexing.pos_fname in
	   let f = Filename.basename file in
	   if Kernel.debug_atleast 1 then
             Kernel.debug "Indexing in file %s the global in %s@." f file;
	   ignore
	     (S.memo
		~change:(fun (f,l) -> f, glob:: l) (fun _ -> f,[ glob ]) file))
    in
    Computation.apply_once "FileIndex.compute" [ S.self ] compute

  let get_files () =
    compute ();
    S.fold (fun key _ keys ->  key :: keys) []

  let get_symbols ~filename =
    compute ();
    try
      S.find (Filename.basename filename)
    with Not_found -> S.find filename

  let find ~filename =
    let f,l = get_symbols ~filename
    in f, List.rev l

  let get_symbols ~filename =
    snd (get_symbols ~filename)

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
        (snd (S.find filename))
        VarinfoSet.empty
    in
      VarinfoSet.fold
	(fun vi acc -> (vi, Vars.find vi) :: acc) varinfo_set []

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
        (snd (S.find filename))
        VarinfoSet.empty
    in
      VarinfoSet.fold
	(fun vi acc -> Functions.get vi :: acc) varinfo_set []

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
    match List.find pred (snd (S.find file)) with
    | Cil_types.GFun (fundec, _) ->
	Functions.get fundec.Cil_types.svar, !is_param
    | _ -> assert (false)

end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string

let entry_point () =
  Ast.compute ();
  let kf_name, lib =
    Parameters.MainFunction.get (),
    Parameters.LibEntry.get ()
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
    let selection = add Parameters.MainFunction.self Project.Selection.empty in
    let selection = add Parameters.LibEntry.self selection in
    Project.clear ~only:selection ()
  in
  let has_changed =
    lib <> Parameters.LibEntry.get () || name <> Parameters.MainFunction.get ()
  in
  if has_changed then begin
    Parameters.MainFunction.unsafe_set name;
    Parameters.LibEntry.unsafe_set lib;
    clear_from_entry_point ()
  end

let has_entry_point () =
  try ignore (entry_point ()); true with No_such_entry_point _ -> false

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
