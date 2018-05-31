(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open Cil

(* ************************************************************************* *)
(** {2 Global variables} *)
(* ************************************************************************* *)

(* redefinition from Kernel_function.ml
   TODO: this should move to Ast_info
 *)
let get_formals f = match f.fundec with
  | Definition(d, _) -> d.sformals
  | Declaration(_, _, None, _) -> []
  | Declaration(_,_,Some args,_) -> args

let get_locals f = match f.fundec with
  | Definition(d, _) -> d.slocals
  | Declaration(_, _, _, _) -> []

let get_location kf = match kf.fundec with
  | Definition (_, loc) -> loc
  | Declaration (_,vi,_, _) -> vi.vdecl

let find_first_stmt = Extlib.mk_fun "Globals.find_first_stmt"

let find_enclosing_block = Extlib.mk_fun "Globals.find_enclosing_block"

let find_all_enclosing_blocks =
  Extlib.mk_fun "Globals.find_all_enclosing_blocks"

let find_englobing_kf = Extlib.mk_fun "Globals.find_englobing_kf"

module Vars = struct

  include Cil_state_builder.Varinfo_hashtbl
    (Initinfo)
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
           raise Not_found
         with Found v ->
           v)
    | VLocal kf ->
        List.find (fun v -> v.vname = name) (get_locals kf)
    | VFormal kf ->
        List.find (fun v -> v.vname = name) (get_formals kf)

  let get_astinfo vi = !get_astinfo_ref vi

  let pp_varinfo p fmt v =
    let name, loc = get_astinfo v in
    let pp fmt =
      Format.fprintf fmt "@[<hv 2>Globals.Vars.find_from_astinfo@;%S@;%a@]"
        name
        (Cil_datatype.Localisation.internal_pretty_code Type.Call) loc
    in
    Type.par p Type.Call fmt pp

  let () = Varinfo.internal_pretty_code_ref := pp_varinfo

  let iter_globals f l =
    let treat_global = function
      | GVar(vi,init,_) -> f vi init
      | GVarDecl (vi,_) ->
         (* If it is defined it will appear with the right init later *)
         if not vi.vdefined then f vi { init = None }
      | GType _ | GCompTag _ | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _
      | GFunDecl _ | GFun _ | GAsm _ | GPragma _ | GText _ | GAnnot _ -> ()
    in
    List.iter treat_global l

  let fold_globals f acc l =
    let treat_global acc = function
      | GVar(vi,init,_) -> f vi init acc
      | GVarDecl (vi,_) ->
         (* If it is defined it will appear with the right init later *)
         if vi.vdefined then acc else f vi { init = None } acc
      | GType _ | GCompTag _ | GCompTagDecl _ | GEnumTag _ | GEnumTagDecl _
      | GFunDecl _ | GFun _ | GAsm _ | GPragma _ | GText _ | GAnnot _ -> acc
    in
    List.fold_left treat_global acc l

  let iter_in_file_order f = iter_globals f (Ast.get ()).globals
  let fold_in_file_order f acc = fold_globals f acc (Ast.get ()).globals

  let iter_in_file_rev_order f = iter_globals f (List.rev (Ast.get ()).globals)
  let fold_in_file_rev_order f acc = 
    fold_globals f acc (List.rev (Ast.get ()).globals)

end

let () = Ast.add_linked_state Vars.self

(* ************************************************************************* *)
(** {2 Functions} *)
(* ************************************************************************* *)

module Functions = struct

  module State =
    Cil_state_builder.Varinfo_hashtbl
      (Cil_datatype.Kf)
      (struct
         let name = "Globals.Functions"
         let dependencies = [ Ast.self ]
         let size = 17
       end)

  let self = State.self

  (* Maintain an alphabetical ordering of the functions, so that
     iteration stays independent from vid numbering scheme. *)
  module VarinfoAlphaOrderSet = struct
    let compare_alpha x y =
      let res = String.compare x.vname y.vname in
      if res = 0 then Datatype.Int.compare x.vid y.vid
      else res

    module Elts = struct
      include Cil_datatype.Varinfo
      let compare = compare_alpha
    end
  end

  module Iterator = struct
    module State = State_builder.Ref
      (Datatype.String.Map.Make(VarinfoAlphaOrderSet.Elts))
      (struct
         let name = "Globals.FunctionsOrder.Iterator"
         let dependencies = [ State.self ]
         let default () = Datatype.String.Map.empty
       end)
    let add v =
      State.set (Datatype.String.Map.add v.vname v (State.get ()))
    let iter f =
      Datatype.String.Map.iter (fun _ v -> f v) (State.get ())
    let fold f acc =
      Datatype.String.Map.fold (fun _ v acc -> f v acc) (State.get ()) acc
  end

  module From_orig_name =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (Cil_datatype.Kf.Set)
      (struct
        let name = "Globals.Functions.From_orig_name"
        let dependencies = [ State.self ]
        let size = 17
      end)

  let get_from_orig_name s =
    try From_orig_name.find s with Not_found -> Cil_datatype.Kf.Set.empty

  let () =
    Parameter_customize.add_function_name_transformation get_from_orig_name

  let update_orig_name kf =
    let orig_name = (Ast_info.Function.get_vi kf.fundec).vorig_name in
    let set = get_from_orig_name orig_name in
    let set = Cil_datatype.Kf.Set.add kf set in
    From_orig_name.replace orig_name set

  let init_kernel_function f spec =
    { fundec = f; spec = spec }

  let fundec_of_decl spec v l =
    let args =
      try Some (getFormalsDecl v)
      with Not_found ->
        try
          setFormalsDecl v v.vtype;
          Some (getFormalsDecl v)
        with Not_found ->
          None (* function with 0 arg. See setFormalsDecl code for details *)
    in Declaration(spec, v, args, l)

  let register_declaration action spec v l =
    action (fun v -> init_kernel_function (fundec_of_decl spec v l) spec) v

  let add_declaration =
    register_declaration
      (fun f v ->
         Iterator.add v;
         let kf = State.memo f v in
         update_orig_name kf; kf)

  let update_kf kf fundec spec =
    (match kf.fundec, fundec with
      (* we never update a definition with a declaration (see bug 1914).
         If you really want to play this game, just mutate the kf in place and
         hope for the best.
       *)
      | Definition _, Declaration(_,v,_,_) when v.vdefined -> ()
      | _ -> kf.fundec <- fundec);
(*    Kernel.feedback "UPDATE Spec of function %a (%a)" 
      Cil_datatype.Kf.pretty kf Printer.pp_funspec spec;*)
    let loc = match kf.fundec with
      | Definition (_, loc) | Declaration (_, _, _, loc) -> loc 
    in
    Cil.CurrentLoc.set loc;
    Logic_utils.merge_funspec kf.spec spec

  let replace_by_declaration s v l=
(*    Kernel.feedback "replacing %a by decl" Cil_datatype.Varinfo.pretty v;*)
    if State.mem v then begin
      let fundec = fundec_of_decl s v l in
      let kf = State.find v in
      update_kf kf fundec s
    end else
      register_declaration
        (fun f v ->
           Iterator.add v;
           let res = f v in State.replace v res;
           update_orig_name res)
        s v l

  let replace_by_definition spec f l =
(*    Kernel.feedback "replacing %a" Cil_datatype.Varinfo.pretty f.svar;*)
    Iterator.add f.svar;
    if State.mem f.svar then
      update_kf (State.find f.svar) (Definition (f,l)) spec
    else begin
      let kf = init_kernel_function (Definition (f, l)) spec in
      State.replace f.svar  kf;
      update_orig_name kf;
    end;
    try ignore (Cil.getFormalsDecl f.svar)
    with Not_found -> Cil.unsafeSetFormalsDecl f.svar f.sformals

  let add f =
    match f with
    | Definition (n, l) ->
      Kernel.debug ~dkey:Kernel.dkey_globals
	"@[<hov 2>Register definition %a with specification@. \"%a\"@]"
        Varinfo.pretty n.svar Cil_printer.pp_funspec n.sspec ;
      replace_by_definition n.sspec n l;
    | Declaration (spec, v,_,l) ->
      Kernel.debug ~dkey:Kernel.dkey_globals
	"@[<hov 2>Register declaration %a with specification@ \"%a\"@]"
        Varinfo.pretty v Cil_printer.pp_funspec spec;
      replace_by_declaration spec v l

  let iter f = Iterator.iter (fun v -> f (State.find v))
  let fold f = Iterator.fold (fun v acc -> f (State.find v) acc)

  let iter_on_fundecs f =
    iter
      (fun kf -> match kf.fundec with
       | Definition (fundec,_) -> f fundec
       | Declaration _ -> ())

  let get vi =
    (*Kernel.feedback "get %a in %a" Cil_datatype.Varinfo.pretty vi
    Project.pretty (Project.current()); *)
    if not (Ast_info.is_function_type vi) then raise Not_found;
    let add v = 
      (* Builtins don't automatically get a kernel function (unless they
         are used explicitly), but might still be accessed after AST
         elaboration. Corresponding kf will be built according to needs.
         Other functions must exist in the table whatever happens.
      *)
      (*Kernel.feedback "adding empty fun for %a" 
        Cil_datatype.Varinfo.pretty vi; *)
      if Cil.is_special_builtin v.vname then
        add_declaration (empty_funspec ()) v v.vdecl
      else
        raise Not_found
    in
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

  let register kf =
    let vi = get_vi kf in
    let add _ = kf in
    let change old_kf =
      if old_kf != kf then
        Kernel.fatal
          "Trying to associate two distinct \
           kernel functions with same varinfo %a"
          Cil_datatype.Varinfo.pretty vi
      else old_kf
    in
    ignore (State.memo ~change add vi);
    Iterator.add vi

  let find_by_name fct_name =
    let vi = Datatype.String.Map.find fct_name (Iterator.State.get ()) in
    State.find vi

  let find_def_by_name fct_name =
    let vi = Datatype.String.Map.find fct_name (Iterator.State.get ()) in
    let res = State.find vi in
    if Ast_info.Function.is_definition res.fundec then
      res
    else
      raise Not_found

  let find_decl_by_name fct_name =
    let vi = Datatype.String.Map.find fct_name (Iterator.State.get ()) in
    let res = State.find vi in
    if Ast_info.Function.is_definition res.fundec then
      raise Not_found
    else
      res

  let () =
    Parameter_builder.find_kf_by_name := find_by_name;
    Parameter_builder.find_kf_def_by_name := find_def_by_name;
    Parameter_builder.find_kf_decl_by_name := find_decl_by_name;
    Parameter_customize.find_kf_by_name := find_by_name

  exception Found of kernel_function
  let get_astinfo vi =
    vi.vname,
    if vi.vglob then VGlobal
    else begin
      if vi.vformal then begin
        try
          iter
            (fun kf ->
               if List.exists (Cil_datatype.Varinfo.equal vi) (get_formals kf)
               then raise (Found kf));
          assert false
        with Found kf ->
          VFormal kf
      end else begin
        try
          iter
            (fun kf ->
               if List.exists (Cil_datatype.Varinfo.equal vi) (get_locals kf)
               then raise (Found kf));
          assert false
        with Found kf ->
          VLocal kf
      end
    end

  let () =
    Vars.get_astinfo_ref := get_astinfo;
    Ast.add_linked_state State.self;
    Ast.add_linked_state Iterator.State.self

  let category =
    let o = object
      method fold: 'a. (kernel_function -> 'a -> 'a) -> 'a -> 'a = fold
      method mem kf = State.mem (get_vi kf)
    end in
    Parameter_category.create "functions" Cil_datatype.Kf.ty
      ~register:true
      [ self ]
      o

  let generate_kf_category is_definition =
    let o = object
      method fold: 'a. (kernel_function -> 'a -> 'a) -> 'a -> 'a =
        fun f acc ->
          fold
            (fun kf acc -> match kf.fundec with
            | Definition _ -> if is_definition then f kf acc else acc
            | Declaration _ -> if is_definition then acc else f kf acc)
            acc
      method mem kf =
        State.mem (get_vi kf) &&
          (is_definition = Ast_info.Function.is_definition kf.fundec)
    end in
    Parameter_category.create "functions" Cil_datatype.Kf.ty
      ~register:true
      [ self ]
      o

  let def_category = generate_kf_category true
  let decl_category = generate_kf_category false

  let fundec_category =
    let o = object
      method fold: 'a. (fundec -> 'a -> 'a) -> 'a -> 'a =
        fun f acc ->
          fold
            (fun kf acc -> match kf.fundec with
            | Definition(fundec, _) -> f fundec acc
            | Declaration _ -> acc)
            acc
      method mem f = State.mem f.svar
    end in
    Parameter_category.create "functions" Cil_datatype.Fundec.ty
      ~register:true
      [ self ]
      o

  let string_category =
    let o = object
      method fold: 'a. (string -> 'a -> 'a) -> 'a -> 'a =
        fun f -> Iterator.fold (fun v acc -> f v.vname acc)
      method mem s = Datatype.String.Map.mem s (Iterator.State.get ())
    end in
    Parameter_category.create "functions" Datatype.string
      ~register:true
      [ self ]
      o

  let () =
    Parameter_builder.kf_category := (fun () -> category);
    Parameter_builder.kf_def_category := (fun () -> def_category);
    Parameter_builder.kf_decl_category := (fun () -> decl_category);
    Parameter_builder.kf_string_category := (fun () -> string_category);
    Parameter_builder.fundec_category := (fun () -> fundec_category)

end

(* ************************************************************************* *)
(** {2 Globals associated to filename} *)
(* ************************************************************************* *)

module FileIndex = struct

  module S =
    State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (Datatype.Pair(Datatype.String)(Datatype.List(Global)))
      (struct
         let name = "Globals.FileIndex"
         let dependencies = [ Ast.self ]
         let size = 7
       end)

  let compute, self =
    let compute () =
      iterGlobals
        (Ast.get ())
        (fun glob ->
          let f = (fst (Global.loc glob)).Lexing.pos_fname in
          Kernel.debug ~dkey:Kernel.dkey_globals "Indexing global in file %s@."
            (Filepath.pretty f);
          ignore
             (S.memo
                ~change:(fun (f,l) -> f, glob:: l) (fun _ -> f,[ glob ]) f))
    in
    State_builder.apply_once "Globals.FileIndex.compute" [ S.self ] compute

  let remove_global_annotations a =
    let f = (fst (Global_annotation.loc a)).Lexing.pos_fname in
    try 
      let _, l = S.find f in
      let l = 
	List.filter
	  (fun g -> match g with
	  | GAnnot(a', _) -> not (Global_annotation.equal a a')
	  | _ -> true)
	  l
      in
      S.replace f (f, l)
    with Not_found ->
      assert false

  let get_files () =
    compute ();
    S.fold (fun key _ keys ->  key :: keys) []

  let get_symbols ~filename =
    compute ();
    try S.find filename
    with Not_found ->
      (* ??? *)
      S.find (Filename.basename filename)

  let find ~filename =
    let f,l = get_symbols ~filename in
    f, List.rev l

  let get_symbols ~filename = snd (get_symbols ~filename)

 (** get all global variables as (varinfo, initinfo) list with only one
      occurrence of a varinfo *)
  let get_globals ~filename =
    compute ();
    let varinfo_set =
      let l =  try snd (S.find filename) with Not_found -> [] in
      List.fold_right
        (fun glob acc ->
           match glob with
             | Cil_types.GVar (vi, _, _) | Cil_types.GVarDecl(vi, _)
                 when vi.vglob -> Varinfo.Set.add vi acc
             | _ -> acc
        )
        l
        Varinfo.Set.empty
    in
    Varinfo.Set.fold (fun vi acc -> (vi, Vars.find vi) :: acc) varinfo_set []

  let get_global_annotations ~filename =
    compute ();
    let l = try snd (S.find filename) with Not_found -> [] in
    List.fold_right
      (fun glob acc -> match glob with
      | Cil_types.GAnnot(g, _) -> g :: acc
      | _ -> acc)
      l
      []

  let get_functions ?(declarations=false) ~filename =
    compute ();
    let varinfo_set =
      let l = try snd (S.find filename) with Not_found -> [] in
      List.fold_right
        (fun glob acc ->
           let is_func v = match v with
             | Cil_types.GFun(fundec, _) ->
                 Some (fundec.svar)
             | Cil_types.GFunDecl(_,x, _) ->
                 if declarations ||
                   (match (Functions.get x).fundec with
                     Definition _ -> false | Declaration _ -> true)
                 then Some x
                 else None
             | _ -> None
           in match is_func glob with
             | None -> acc
             | Some vi -> Varinfo.Set.add vi acc)
        l
        Varinfo.Set.empty
    in
    Varinfo.Set.fold
      (fun vi acc -> Functions.get vi :: acc)
      varinfo_set
      []

  let kernel_function_of_local_var_or_param_varinfo x =
    compute ();
    let is_param = ref false in
    let pred g =
      let pred symb = (x.Cil_types.vid = symb.Cil_types.vid)
      in match g with
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

module Syntactic_search = struct

  module Key =
    Datatype.Pair_with_collections
      (Datatype.String)(Cil_datatype.Syntactic_scope)
      (struct let module_name = "Globals.Datatype.Key" end)

  module Scope_info =
    State_builder.Hashtbl
      (Key.Hashtbl)(Datatype.Option(Cil_datatype.Varinfo))
      (struct
        let size = 137
        let dependencies = [ Ast.self; Vars.self; FileIndex.self ]
        let name = "Globals.Syntactic_search.Scope_info"
      end)

  let self = Scope_info.self

  let () = Ast.add_monotonic_state self

  let rec find_var (x,scope) =
    let has_name v = v.vorig_name = x in
    let global_has_name v =
      has_name v && v.vglob
      && not (Cil.hasAttribute Cabs2cil.fc_local_static v.vattr)
    in
    let module M = struct exception Found of varinfo end in
    match scope with
    | Program ->
      (try
         Vars.iter (fun v _ -> if global_has_name v then raise (M.Found v));
         None
       with M.Found v -> Some v)
    | Translation_unit file ->
      let symbols = FileIndex.get_globals file in
      (try Some (fst (List.find (fun x -> (global_has_name (fst x))) symbols))
       with Not_found -> find_in_scope x Program)
    | Block_scope stmt ->
      let blocks = !find_all_enclosing_blocks stmt in
      let find_in_block b =
        if List.exists has_name b.blocals then
          raise (M.Found (List.find has_name b.blocals))
        else if List.exists has_name b.bstatics then
          raise (M.Found (List.find has_name b.bstatics))
      in
      try
        List.iter find_in_block blocks;
        let kf = !find_englobing_kf stmt in
        let filename = (fst ( get_location kf)).Lexing.pos_fname in
        let formals = get_formals kf in
        if List.exists has_name formals then
          Some (List.find has_name formals)
        else
          find_in_scope x (Translation_unit filename)
      with M.Found v -> Some v

  and find_in_scope x scope = Scope_info.memo find_var (x,scope)

end

(* ************************************************************************* *)
(** {2 Types} *)
(* ************************************************************************* *)

module Types = struct

  module PairsExpTyp = Datatype.Pair(Cil_datatype.Exp)(Cil_datatype.Typ)

  (* Map from enum constant names to an expression containing the constant,
     and its type. *)
  module Enums = State_builder.Hashtbl(Datatype.String.Hashtbl)(PairsExpTyp)
    (struct
      let size = 137
      let dependencies = [Ast.self]
      let name = "Globals.Types.Enums"
     end)

  module Type_Name_Namespace =
    Datatype.Pair_with_collections
      (Datatype.String)(Logic_typing.Type_namespace)
      (struct let module_name = "Globals.Types.Typ_Name_Namespace" end)

  (* Maps from a type name and its namespace, to the Cil type. *)
  module Types =
    State_builder.Hashtbl(Type_Name_Namespace.Hashtbl)(Cil_datatype.Typ)
      (struct
        let size = 137
        let dependencies = [Ast.self]
        let name = "Globals.Types.Types"
       end)

  (* Maps a typename (with its namespace) to its corresponding global. *)
  module TypeNameToGlobal =
    State_builder.Hashtbl
      (Type_Name_Namespace.Hashtbl)
      (Cil_datatype.Global)
      (struct
        let name = "Globals.Types.TypeNameToGlobal"
        let size = 7
        let dependencies = [ Ast.self ]
      end)

  let resolve_types () =
    let aux_ei ei = (* for enums *)
      let exp = Cil.new_exp ~loc:ei.eiloc (Const (CEnum ei)) in
      Enums.replace ei.einame (exp, Cil.typeOf ei.eival)
    in
    let aux_glob g = match g with
      | GType (ti, _loc) ->
        let name_tag = (ti.tname, Logic_typing.Typedef) in
        Types.replace name_tag (TNamed (ti, []));
        TypeNameToGlobal.replace name_tag g

      | GEnumTag (ei, _loc) ->
        let name_tag = (ei.ename, Logic_typing.Enum) in
        Types.add name_tag (TEnum (ei, []));
        List.iter aux_ei ei.eitems;
        TypeNameToGlobal.replace name_tag g

      | GEnumTagDecl (ei, _) ->
        let name_tag = (ei.ename, Logic_typing.Enum) in
        Types.add name_tag (TEnum (ei, []));
        List.iter aux_ei ei.eitems

      | GCompTag (ci, _loc) ->
        let kind = Logic_typing.(if ci.cstruct then Struct else Union) in
        let name_tag = (ci.cname, kind) in
        Types.add name_tag (TComp (ci, Cil.empty_size_cache (), []));
        TypeNameToGlobal.replace name_tag g

      | GCompTagDecl (ci, _) ->
        let kind = Logic_typing.(if ci.cstruct then Struct else Union) in
        let name_tag = (ci.cname, kind) in
        Types.add name_tag (TComp (ci, Cil.empty_size_cache (), []))

      | _ -> ()
    in
    if not (Enums.is_computed ()) || not (Types.is_computed ()) then begin
      List.iter aux_glob (Ast.get ()).globals;
      Enums.mark_as_computed ();
      Types.mark_as_computed ();
      TypeNameToGlobal.mark_as_computed ()
    end

  let find_enum_tag x =
    resolve_types ();
    Enums.find x

  let find_type namespace s =
    resolve_types ();
    Types.find (s, namespace)

  let iter_types f =
    resolve_types ();
    Types.iter (fun (name, namespace) typ -> f name typ namespace)

  let global namespace s =
    resolve_types ();
    TypeNameToGlobal.find (s, namespace)

end

(* ************************************************************************* *)
(** {2 Entry point} *)
(* ************************************************************************* *)

exception No_such_entry_point of string

let entry_point () =
  Ast.compute ();
  let kf_name, lib =
    Kernel.MainFunction.get_plain_string (), Kernel.LibEntry.get ()
  in
  let fcts = Parameter_customize.get_c_ified_functions kf_name in
  if (Cil_datatype.Kf.Set.is_empty fcts) then
    raise
      (No_such_entry_point
         (Format.sprintf
            "cannot find entry point `%s'.@;\
               Please use option `-main' for specifying a valid entry point."
            kf_name))
  else begin
    if (Cil_datatype.Kf.Set.cardinal fcts > 1) then
      Kernel.warning
        "Ambiguous function name: %s; \
         choosing an arbitrary function whose name apply."
        kf_name;
    let kf = Cil_datatype.Kf.Set.choose fcts in
    kf, lib
  end

let set_entry_point name lib =
  let clear_from_entry_point () =
    let selection =
      State_selection.union
        (State_selection.with_dependencies Kernel.MainFunction.self)
        (State_selection.with_dependencies Kernel.LibEntry.self)
    in
    Project.clear ~selection ()
  in
  let has_changed =
    lib <> Kernel.LibEntry.get ()
    || name <> Kernel.MainFunction.get_plain_string ()
  in
  if has_changed then begin
    clear_from_entry_point ();
    Kernel.MainFunction.unsafe_set name;
    Kernel.LibEntry.unsafe_set lib;
  end

(* ************************************************************************* *)
(** {2 Global Comments} *)
(* ************************************************************************* *)

module Comments_global_cache =
  State_builder.Hashtbl
    (Cil_datatype.Global.Hashtbl)
    (Datatype.List(Datatype.String))
    (struct
      let name = "Globals.Comments_global_cache"
      let dependencies =
        [ Cabshelper.Comments.self; FileIndex.self ]
      let size = 17
     end)

module Comments_stmt_cache =
  State_builder.Hashtbl
    (Cil_datatype.Stmt.Hashtbl)
    (Datatype.List(Datatype.String))
    (struct
      let name = "Globals.Comments_stmt_cache"
      let dependencies = [ Cabshelper.Comments.self; FileIndex.self ]
      let size = 17
     end)

let get_comments_global g =
  let last_pos f =
    { Lexing.pos_fname = f;
      Lexing.pos_lnum = max_int;
      Lexing.pos_cnum = max_int;
      Lexing.pos_bol = max_int
    }
  in
  let add g =
    let my_loc = Cil_datatype.Global.loc g in
    let file = (fst my_loc).Lexing.pos_fname in
    let globs = FileIndex.get_symbols file in
    let globs = List.sort 
      (fun g1 g2 -> 
        Cil_datatype.Location.compare 
          (Cil_datatype.Global.loc g1)
          (Cil_datatype.Global.loc g2))
      globs
    in
    let rec find_prev l =
      match l with
        | [] -> 
          Kernel.fatal "Cannot find global %a in file %s"
            Cil_printer.pp_global g (Filepath.pretty file)
        | g' :: l when Cil_datatype.Global.equal g g' ->
            { Lexing.pos_fname = file;
              Lexing.pos_lnum = 1;
              Lexing.pos_cnum = 0;
              Lexing.pos_bol = 0; }, l = []
        | g' :: g'' :: l when Cil_datatype.Global.equal g'' g ->
          snd (Cil_datatype.Global.loc g'), l = []
        | _ :: l -> find_prev l
    in 
    let first, is_last = find_prev globs in
    match g with
        GFun (f,_) -> 
          let kf = Functions.get f.svar in
          let s = !find_first_stmt kf in
          let last = fst (Cil_datatype.Stmt.loc s) in
          let comments = Cabshelper.Comments.get (first,last) in
          if is_last then begin
            let first = snd my_loc in
            let last = last_pos file in
            comments @ (Cabshelper.Comments.get (first, last))
          end else comments
      | _ -> 
        let last = if is_last then last_pos file else snd my_loc in
        Cabshelper.Comments.get (first,last)
  in Comments_global_cache.memo add g

let get_comments_stmt s =
  let add s =
    let b = !find_enclosing_block s in
    let rec find_prev l =
      match l with
        | [] ->
          Kernel.fatal "Cannot find statement %d in its enclosing block" s.sid
        | s' :: _ when Cil_datatype.Stmt.equal s s' ->
          fst (Cil_datatype.Stmt.loc s')
        | s' :: s'' :: _ when Cil_datatype.Stmt.equal s'' s ->
          snd (Cil_datatype.Stmt.loc s')
        | { skind = UnspecifiedSequence l1} :: l2 ->
          find_prev ((List.map (fun (x,_,_,_,_) -> x) l1) @ l2)
        | _::l -> find_prev l
    in
    let first = find_prev b.bstmts in
    let last = snd (Cil_datatype.Stmt.loc s) in
    Cabshelper.Comments.get (first,last)
  in Comments_stmt_cache.memo add s

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
