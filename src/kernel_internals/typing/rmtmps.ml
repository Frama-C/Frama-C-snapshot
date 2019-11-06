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

let dkey = Kernel.dkey_rmtmps

open Extlib
open Cil_types
open Cil

(* Reachability of used data is stored in a table mapping [info] to [bool].
   Note that due to mutability, we need to use our own Hashtbl module which
   uses [Cil_datatype] equality functions. *)
type info =
  | Type of typeinfo
  | Enum of enuminfo
  | Comp of compinfo
  | Var of varinfo

module InfoHashtbl = Hashtbl.Make(struct
    type t = info
    let equal i1 i2 = match i1, i2 with
      | Type t1, Type t2 -> Cil_datatype.Typeinfo.equal t1 t2
      | Enum e1, Enum e2 -> Cil_datatype.Enuminfo.equal e1 e2
      | Comp c1, Comp c2 -> Cil_datatype.Compinfo.equal c1 c2
      | Var v1, Var v2 -> Cil_datatype.Varinfo.equal v1 v2
      | _, _ -> false
    let hash = function
      | Type t -> Cil_datatype.Typeinfo.hash t
      | Enum e -> Cil_datatype.Enuminfo.hash e
      | Comp c -> Cil_datatype.Compinfo.hash c
      | Var v -> Cil_datatype.Varinfo.hash v
  end)

let keepUnused = ref false

(* Possibly no longer used: *)
let rmUnusedInlines = ref false
let rmUnusedStatic = ref false

let is_reachable t r = try InfoHashtbl.find t r with Not_found -> false

let pp_info fmt = function
  | Type ti -> Format.fprintf fmt "%s" ti.tname
  | Enum ei -> Format.fprintf fmt "%s" ei.ename
  | Comp ci -> Format.fprintf fmt "%s" ci.cname
  | Var vi -> Format.fprintf fmt "%s" vi.vname


(***********************************************************************
 *
 *  Scanning and categorization of pragmas
 *
*)


(* collections of names of things to keep *)
type collection = (string, unit) Hashtbl.t
type keepers = {
  typedefs : collection;
  enums : collection;
  structs : collection;
  unions : collection;
  defines : collection;
}


(* rapid transfer of control when we find a malformed pragma *)
exception Bad_pragma

(* CIL and CCured define several pragmas which prevent removal of
 * various global varinfos.  Here we scan for those pragmas and build
 * up collections of the corresponding varinfos' names.
*)

let categorizePragmas ast =

  (* names of things which should be retained *)
  let keepers = {
    typedefs = Hashtbl.create 1;
    enums = Hashtbl.create 1;
    structs = Hashtbl.create 1;
    unions = Hashtbl.create 1;
    defines = Hashtbl.create 1
  } in

  (* populate these name collections in light of each pragma *)
  let considerPragma =

    let badPragma location pragma =
      Kernel.warning ~source:location "Invalid argument to pragma %s" pragma
    in

    function
    | GPragma (Attr ("cilnoremove" as directive, args), (location,_)) ->
      (* a very flexible pragma: can retain typedefs, enums,
       * structs, unions, or globals (functions or variables) *)
      begin
        let processArg arg =
          try
            match arg with
            | AStr specifier ->
              (* isolate and categorize one varinfo name *)
              let collection, name =
                (* Two words denotes a typedef, enum, struct, or
                 * union, as in "type foo" or "enum bar".  A
                 * single word denotes a global function or
                 * variable. *)
                let whitespace = Str.regexp "[ \t]+" in
                let words = Str.split whitespace specifier in
                match words with
                | ["type"; name] ->
                  keepers.typedefs, name
                | ["enum"; name] ->
                  keepers.enums, name
                | ["struct"; name] ->
                  keepers.structs, name
                | ["union"; name] ->
                  keepers.unions, name
                | [name] ->
                  keepers.defines, name
                | _ ->
                  raise Bad_pragma
              in
              Hashtbl.add collection name ()
            | _ ->
              raise Bad_pragma
          with Bad_pragma ->
            badPragma location directive
        in
        List.iter processArg args
      end
    | GFunDecl (_,v, _) -> begin
        (* Look for alias attributes, e.g. Linux modules *)
        match filterAttributes "alias" v.vattr with
        | [] -> ()  (* ordinary prototype. *)
        | [ Attr("alias", [AStr othername]) ] ->
          Hashtbl.add keepers.defines othername ()
        | _ ->
          Kernel.fatal ~current:true
            "Bad alias attribute at %a"
            Cil_printer.pp_location (CurrentLoc.get ())
      end
    | _ ->
      ()
  in
  iterGlobals ast considerPragma;
  keepers



(***********************************************************************
 *
 *  Root collection from pragmas
 *
*)


let isPragmaRoot keepers = function
  | GType ({tname = name}, _) ->
    Hashtbl.mem keepers.typedefs name
  | GEnumTag ({ename = name}, _)
  | GEnumTagDecl ({ename = name}, _) ->
    Hashtbl.mem keepers.enums name
  | GCompTag ({cname = name; cstruct = structure}, _)
  | GCompTagDecl ({cname = name; cstruct = structure}, _) ->
    let collection = if structure then keepers.structs else keepers.unions in
    Hashtbl.mem collection name
  | GVar ({vname = name; vattr = attrs}, _, _)
  | GVarDecl ({vname = name; vattr = attrs}, _)
  | GFunDecl (_,{vname = name; vattr = attrs}, _)
  | GFun ({svar = {vname = name; vattr = attrs}}, _) ->
    Hashtbl.mem keepers.defines name ||
    hasAttribute "used" attrs
  | _ ->
    false



(***********************************************************************
 *
 *  Common root collecting utilities
 *
*)
let hasExportingAttribute funvar =
  let isExportingAttribute = function
    | Attr ("constructor", []) -> true
    | Attr ("destructor", []) -> true
    | _ -> false
  in
  List.exists isExportingAttribute funvar.vattr

(***********************************************************************
 *
 *  Root collection from external linkage
 *
*)


(* Exported roots are those global varinfos which are visible to the
 * linker and dynamic loader.  For variables, this consists of
 * anything that is not "static".  For functions, this consists of:
 *
 * - functions bearing a "constructor" or "destructor" attribute
 * - functions declared extern but not inline
 * - functions declared neither inline nor static
 * - the function named "main"
 * gcc incorrectly (according to C99) makes inline functions visible to
 * the linker.  So we can only remove inline functions on MSVC.
*)

let isExportedRoot global =
  let name, result, reason = match global with
    | GVar ({vstorage = Static} as v, _, _) when
        Cil.hasAttribute "FC_BUILTIN" v.vattr ->
      v.vname, true, "FC_BUILTIN attribute"
    | GVar ({vstorage = Static; vname}, _, _) -> vname, false, "static variable"
    | GVar (v,_,_) ->
      v.vname, true, "non-static variable"
    | GFun ({svar = v}, _) -> begin
        if hasExportingAttribute v then
          v.vname,true, "constructor or destructor function"
        else if v.vstorage = Static then
          v.vname, not !rmUnusedStatic, "static function"
        else if v.vinline && v.vstorage != Extern
                && (Cil.msvcMode () || !rmUnusedInlines) then
          v.vname, false, "inline function"
        else
          v.vname, true, "other function"
      end
    | GFunDecl(_,v,_) when hasAttribute "alias" v.vattr ->
      v.vname, true, "has GCC alias attribute"
    | GFunDecl(_,v,_) | GVarDecl(v,_) when hasAttribute "FC_BUILTIN" v.vattr ->
      v.vname, true, "has FC_BUILTIN attribute"
    | GAnnot _ -> "", true, "global annotation"
    | GType (t, _) when
        Cil.hasAttribute "FC_BUILTIN" (Cil.typeAttr t.ttype) ->
      t.tname, true, "has FC_BUILTIN attribute"
    | GCompTag (c,_) | GCompTagDecl (c,_) when
        Cil.hasAttribute "FC_BUILTIN" c.cattr ->
      c.cname, true, "has FC_BUILTIN attribute"
    | GEnumTag (e, _) | GEnumTagDecl (e,_) when
        Cil.hasAttribute "FC_BUILTIN" e.eattr ->
      e.ename, true, "has FC_BUILTIN attribute"
    | _ ->
      (Format.asprintf "%a" Cil_types_debug.pp_global global), false,
      "neither fundef nor vardef nor annotation"
  in
  Kernel.debug
    ~dkey "isExportedRoot %s -> %B, %s"  name result reason;
  result



(***********************************************************************
 *
 *  Root collection for complete programs
 *
*)


(* Exported roots are "main()" and functions bearing a "constructor"
 * or "destructor" attribute.  These are the only things which must be
 * retained in a complete program.
*)

let isCompleteProgramRoot global =
  let result = match global with
    | GFun ({svar = {vname = "main"; vstorage = vstorage}}, _) ->
      vstorage <> Static
    | GFun (fundec, _)
      when hasExportingAttribute fundec.svar ->
      true
    | _ ->
      false
  in
  (*  trace (dprintf "complete program root -> %b for %a@!" result d_shortglobal global);*)
  result


(***********************************************************************
 *
 *  Transitive reachability closure from roots
 *
*)


(* This visitor recursively marks all reachable types and variables as used. *)
class markReachableVisitor
    (globalMap: (string, Cil_types.global) Hashtbl.t)
    (currentFunc: Cil_types.fundec option ref)
    (reachable_tbl: bool InfoHashtbl.t)
  = object (self)
    inherit nopCilVisitor

    method! vglob = function
      | GType (typeinfo, _) ->
        Kernel.debug ~dkey "marking reachable: type %s" typeinfo.tname;
        InfoHashtbl.replace reachable_tbl (Type typeinfo) true;
        DoChildren
      | GCompTag (compinfo, _)
      | GCompTagDecl (compinfo, _) ->
        Kernel.debug ~dkey "marking reachable: comp decl %s" compinfo.cname;
        InfoHashtbl.replace reachable_tbl (Comp compinfo) true;
        DoChildren
      | GEnumTag (enuminfo, _)
      | GEnumTagDecl (enuminfo, _) ->
        Kernel.debug ~dkey "marking reachable: enum decl %s" enuminfo.ename;
        InfoHashtbl.replace reachable_tbl (Enum enuminfo) true;
        DoChildren
      | GVar (varinfo, _, _)
      | GVarDecl (varinfo, _)
      | GFunDecl (_,varinfo, _)
      | GFun ({svar = varinfo}, _) ->
        if not (hasAttribute "FC_BUILTIN" varinfo.vattr) then
          begin
            Kernel.debug ~dkey "marking reachable: function %s" varinfo.vname;
            InfoHashtbl.replace reachable_tbl (Var varinfo) true;
          end;
        DoChildren
      | GAnnot _ -> DoChildren
      | _ ->
        SkipChildren

    method! vstmt s =
      match s.skind with
      | TryCatch(_,c,_) ->
        List.iter
          (fun (decl,_) ->
             match decl with
             | Catch_exn(v,l) ->
               (* treat all variables declared in exn clause as used. *)
               ignore (self#vvrbl v);
               List.iter (fun (v,_) -> ignore (self#vvrbl v)) l
             | Catch_all -> ())
          c;
        DoChildren
      | _ -> DoChildren

    method! vinst = function
      | Asm (_, tmpls, _, _) when Cil.msvcMode () ->
        (* If we have inline assembly on MSVC, we cannot tell which locals
         * are referenced. Keep them all *)
        (match !currentFunc with
           Some fd ->
           List.iter (fun v ->
               let vre = Str.regexp_string (Str.quote v.vname) in
               if List.exists (fun tmp ->
                   try ignore (Str.search_forward vre tmp 0); true
                   with Not_found -> false)
                   tmpls
               then
                 InfoHashtbl.replace reachable_tbl (Var v) true
             ) fd.slocals
         | _ -> assert false);
        DoChildren
      | _ -> DoChildren

    method! vvrbl v =
      if not (is_reachable reachable_tbl (Var v)) then
        begin
          let name = v.vname in
          if v.vglob then
            Kernel.debug ~dkey "marking transitive use: global %s (%d)" name v.vid
          else
            Kernel.debug ~dkey "marking transitive use: local %s (%d)" name v.vid;

          (* If this is a global, we need to keep everything used in its
           * definition and declarations. *)
          InfoHashtbl.replace reachable_tbl (Var v) true;
          if v.vglob then
            begin
              Kernel.debug ~dkey "descending: global %s (%d)" name v.vid;
              let descend global =
                ignore (visitCilGlobal (self :> cilVisitor) global)
              in
              let globals = Hashtbl.find_all globalMap name in
              List.iter descend globals
            end
        end;
      SkipChildren

    method private mark_enum e =
      if not (is_reachable reachable_tbl (Enum e)) then
        begin
          Kernel.debug ~dkey "marking transitive use: enum %s" e.ename;
          InfoHashtbl.replace reachable_tbl (Enum e) true;
          self#visitAttrs e.eattr;
          (* Must visit the value attributed to the enum constants *)
          ignore (visitCilEnumInfo (self:>cilVisitor) e);
        end
      else
        Kernel.debug ~dkey "not marking transitive use: enum %s" e.ename;

    method! vexpr e =
      match e.enode with
        Const (CEnum {eihost = ei}) -> self#mark_enum ei; DoChildren
      | _ -> DoChildren

    method! vterm_node t =
      match t with
        TConst (LEnum {eihost = ei}) -> self#mark_enum ei; DoChildren
      | _ -> DoChildren

    method private visitAttrs attrs =
      ignore (visitCilAttributes (self :> cilVisitor) attrs)

    method! vtype typ =
      (match typ with
       | TEnum(e, attrs) ->
         self#visitAttrs attrs;
         self#mark_enum e

       | TComp(c, _, attrs) ->
         let old = is_reachable reachable_tbl (Comp c) in
         if not old then
           begin
             Kernel.debug ~dkey "marking transitive use: compound %s"
               c.cname;
             InfoHashtbl.replace reachable_tbl (Comp c) true;

             (* to recurse, we must ask explicitly *)
             let recurse f = ignore (self#vtype f.ftype) in
             List.iter recurse c.cfields;
             self#visitAttrs attrs;
             self#visitAttrs c.cattr
           end;

       | TNamed(ti, attrs) ->
         let old = (is_reachable reachable_tbl (Type ti)) in
         if not old then
           begin
             Kernel.debug ~dkey "marking transitive use: typedef %s"
               ti.tname;
             InfoHashtbl.replace reachable_tbl (Type ti) true;
             (* recurse deeper into the type referred-to by the typedef *)
             (* to recurse, we must ask explicitly *)
             ignore (self#vtype ti.ttype);
             self#visitAttrs attrs
           end;

       | TVoid a | TInt (_,a) | TFloat (_,a) | TBuiltin_va_list a ->
         self#visitAttrs a
       | TPtr(ty,a) -> ignore (self#vtype ty); self#visitAttrs a
       | TArray(ty,sz, _, a) ->
         ignore (self#vtype ty); self#visitAttrs a;
         Extlib.may (ignore $ (visitCilExpr (self:>cilVisitor))) sz
       | TFun (ty, args,_,a) ->
         ignore (self#vtype ty);
         Extlib.may (List.iter (fun (_,ty,_) -> ignore (self#vtype ty))) args;
         self#visitAttrs a
      );
      SkipChildren

    method! vlogic_var_decl lv =
      Kernel.debug ~dkey "markReachable: found LOGIC VAR DECL for: %s (%d)\n" lv.lv_name lv.lv_id;
      DoChildren

    method! vlogic_var_use lv =
      Kernel.debug ~dkey "markReachable: found LOGIC VAR USE for: %s (%d)\n" lv.lv_name lv.lv_id;
      match lv.lv_origin with
      | None -> SkipChildren
      | Some v ->
        if not (is_reachable reachable_tbl (Var v)) then
          begin
            let name = v.vname in
            if v.vglob then
              Kernel.debug ~dkey "marking transitive use for logic var: global %s (%d)" name v.vid
            else
              Kernel.debug ~dkey "marking transitive use for logic var: local %s (%d)" name v.vid;

            (* If this is a global, we need to keep everything used in its
             * definition and declarations. *)
            InfoHashtbl.replace reachable_tbl (Var v) true;
            if v.vglob then
              begin
                Kernel.debug ~dkey "descending: global %s (%d)" name v.vid;
                let descend global =
                  ignore (visitCilGlobal (self :> cilVisitor) global)
                in
                let globals = Hashtbl.find_all globalMap name in
                List.iter descend globals
              end
          end;
        SkipChildren
  end


let markReachable isRoot ast reachable_tbl =
  (* build a mapping from global names back to their definitions &
   * declarations *)
  let globalMap = Hashtbl.create 137 in
  let considerGlobal global =
    match global with
    | GFun ({svar = info}, _)
    | GVar (info, _, _)
    | GFunDecl (_,info, _)
    | GVarDecl (info, _) ->
      Hashtbl.add globalMap info.vname global
    | _ ->
      ()
  in
  iterGlobals ast considerGlobal;

  let currentFunc = ref None in

  (* mark everything reachable from the global roots *)
  let visitor = new markReachableVisitor globalMap currentFunc reachable_tbl in
  let visitIfRoot global =
    if isRoot global then
      begin
        (* trace (dprintf "traversing root global: %a\n" d_shortglobal global);*)
        (match global with
           GFun(fd, _) -> currentFunc := Some fd
         | _ -> currentFunc := None);
        ignore (visitCilGlobal visitor global)
      end
    else
      (*      trace (dprintf "skipping non-root global: %a\n" d_shortglobal global)*)
      ()
  in
  iterGlobals ast visitIfRoot

(**********************************************************************
 *
 * Marking of referenced infos
 *
 **********************************************************************)

let global_type_and_name = function
  | GType (t, _) -> "type " ^ t.tname
  | GCompTag (c,_) -> "comp " ^ c.cname
  | GCompTagDecl (c,_) -> "comp decl " ^ c.cname
  | GEnumTag (e, _) -> "enum " ^ e.ename
  | GEnumTagDecl (e,_) -> "enum decl " ^ e.ename
  | GVarDecl(v,_) -> "var decl " ^ v.vname
  | GFunDecl(_,v,_) -> "fun decl " ^ v.vname
  | GVar (v, _, _) -> "var " ^ v.vname
  | GFun ({svar = v}, _) -> "fun " ^ v.vname
  | GAsm _ -> "<asm>"
  | GPragma _ -> "<pragma>"
  | GText _ -> "<text>"
  | GAnnot _ -> "<annot>"

class markReferencedVisitor = object
  inherit nopCilVisitor

  val dkey = Kernel.dkey_referenced

  val inside_exp : exp Stack.t = Stack.create ()
  val inside_typ : typ Stack.t = Stack.create ()

  method! vglob = function
    | GType (typeinfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: type %s" typeinfo.tname;
      typeinfo.treferenced <- true;
      DoChildren
    | GCompTag (compinfo, loc)
    | GCompTagDecl (compinfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: comp %s" compinfo.cname;
      compinfo.creferenced <- true;
      DoChildren
    | GEnumTag (enuminfo, loc)
    | GEnumTagDecl (enuminfo, loc) ->
      Kernel.debug ~source:(fst loc) ~dkey "referenced: enum %s" enuminfo.ename;
      enuminfo.ereferenced <- true;
      DoChildren
    | GVar (varinfo, _, loc)
    | GVarDecl (varinfo, loc)
    | GFunDecl (_,varinfo, loc)
    | GFun ({svar = varinfo}, loc) ->
      if not (hasAttribute "FC_BUILTIN" varinfo.vattr) then begin
        Kernel.debug ~dkey "referenced: var/fun %s@." varinfo.vname;
        Kernel.debug ~source:(fst loc) ~dkey "referenced: fun %s" varinfo.vname;
        varinfo.vreferenced <- true;
      end;
      DoChildren
    | GAnnot _ -> DoChildren
    | _ ->
      SkipChildren

  method! vtype = function
    | TNamed (ti, _) ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: type %s" ti.tname;
        ti.treferenced <- true;
      end;
      DoChildren
    | TComp (ci, _, _) ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: comp %s" ci.cname;
        ci.creferenced <- true;
      end;
      DoChildren
    | TEnum (ei, _) ->
      if not (Stack.is_empty inside_typ) then begin
        Kernel.debug ~current:true ~dkey "referenced: enum %s" ei.ename;
        ei.ereferenced <- true;
      end;
      DoChildren
    | TVoid _
    | TInt _
    | TFloat _
    | TPtr _
    | TArray _
    | TFun _
    | TBuiltin_va_list _ -> DoChildren

  method! vexpr e =
    match e.enode with
    | SizeOf t | AlignOf t | UnOp (_, _, t) | BinOp (_, _, _, t) ->
      Stack.push t inside_typ;
      DoChildrenPost (fun e -> ignore (Stack.pop inside_typ); e)
    | _ ->
      Stack.push e inside_exp;
      DoChildrenPost (fun e -> ignore (Stack.pop inside_exp); e)

  method! vvrbl v =
    if not (Stack.is_empty inside_exp) then begin
      Kernel.debug ~current:true ~dkey "referenced: var %s" v.vname;
      v.vreferenced <- true;
    end;
    SkipChildren

end

let markReferenced ast =
  Kernel.debug ~dkey "starting markReferenced (AST has %d globals)"
    (List.length ast.globals);
  visitCilFileSameGlobals (new markReferencedVisitor) ast;
  Kernel.debug ~dkey "finished markReferenced"

(**********************************************************************
 *
 * Marking and removing of unused labels
 *
 **********************************************************************)

(* We keep only one label, preferably one that was not introduced by CIL.
 * Scan a list of labels and return the data for the label that should be
 * kept, and the remaining filtered list of labels *)
let labelsToKeep is_removable ll =
  let rec loop sofar = function
      [] -> sofar, []
    | l :: rest ->
      let newlabel, keepl =
        match l with
        | Case _ | Default _ -> sofar, true
        | Label (ln, _, _) as lab -> begin
            match is_removable lab, sofar with
            | true, ("", _) ->
              (* keep this one only if we have no label so far *)
              (ln, lab), false
            | true, _ -> sofar, false
            | false, (_, lab') when is_removable lab' ->
              (* this is an original label; prefer it to temporary or
               * missing labels *)
              (ln, lab), false
            | false, _ -> sofar, false
          end
      in
      let newlabel', rest' = loop newlabel rest in
      newlabel', (if keepl then l :: rest' else rest')
  in
  loop ("", Label("", Cil_datatype.Location.unknown, false)) ll

class markUsedLabels is_removable (labelMap: (string, unit) Hashtbl.t) =
  let keep_label dest =
    let (ln, _), _ = labelsToKeep is_removable !dest.labels in
    if ln = "" then
      Kernel.fatal "Statement has no label:@\n%a" Cil_printer.pp_stmt !dest ;
    (* Mark it as used *)
    Hashtbl.replace labelMap ln ()
  in
  let keep_label_logic = function
    | FormalLabel _ | BuiltinLabel _ -> ()
    | StmtLabel dest -> keep_label dest
  in
  object
    inherit nopCilVisitor

    method! vstmt (s: stmt) =
      match s.skind with
        Goto (dest, _) -> keep_label dest; DoChildren
      | _ -> DoChildren

    method! vterm_node t =
      begin
        match t with
        | Tat (_,lab) -> keep_label_logic lab
        | Tapp(_,labs,_) ->
          List.iter keep_label_logic labs
        | _ -> ()
      end;
      DoChildren

    method! vpredicate_node t =
      begin
        match t with
        | Pat (_,lab) -> keep_label_logic lab
        | Papp(_,labs,_) ->
          List.iter keep_label_logic labs
        | _ -> ()
      end;
      DoChildren

    (* No need to go into expressions or types *)
    method! vexpr _ = SkipChildren
    method! vtype _ = SkipChildren
  end

class removeUnusedLabels is_removable (labelMap: (string, unit) Hashtbl.t) = object
  inherit nopCilVisitor

  method! vstmt (s: stmt) =
    let (ln, lab), lrest = labelsToKeep is_removable s.labels in
    s.labels <-
      (if ln <> "" &&
          (Hashtbl.mem labelMap ln || not (is_removable lab))
          (* keep user-provided labels *)
       then (* We had labels *)
         (lab :: lrest)
       else
         lrest);
    DoChildren

  (* No need to go into expressions or instructions *)
  method! vexpr _ = SkipChildren
  method! vinst _ = SkipChildren
  method! vtype _ = SkipChildren
end

(***********************************************************************
 *
 *  Removal of unused varinfos
 *
*)

let label_removable = function
    Label (_,_,user) -> not user
  | Case _ | Default _ -> false

let remove_unused_labels ?(is_removable=label_removable) func =
  (* We also want to remove unused labels. We do it all here, including
   * marking the used labels *)
  let usedLabels:(string, unit) Hashtbl.t = Hashtbl.create 13 in
  ignore
    (visitCilBlock (new markUsedLabels is_removable usedLabels) func.sbody);
  (* And now we scan again and we remove them *)
  ignore
    (visitCilBlock (new removeUnusedLabels is_removable usedLabels) func.sbody)

let removeUnmarked isRoot ast reachable_tbl =
  let removedLocals = ref [] in

  let filterGlobal global =
    match global with
    (* unused global types, variables, and functions are simply removed *)
    | GType (t, _) ->
      is_reachable reachable_tbl (Type t) ||
      Cil.hasAttribute "FC_BUILTIN" (Cil.typeAttr t.ttype)
      || isRoot global
    | GCompTag (c,_) | GCompTagDecl (c,_) ->
      is_reachable reachable_tbl (Comp c) ||
      Cil.hasAttribute "FC_BUILTIN" c.cattr || isRoot global
    | GEnumTag (e, _) | GEnumTagDecl (e,_) ->
      is_reachable reachable_tbl (Enum e) ||
      Cil.hasAttribute "FC_BUILTIN" e.eattr || isRoot global
    | GVar (v, _, _) ->
      is_reachable reachable_tbl (Var v) ||
      Cil.hasAttribute "FC_BUILTIN" v.vattr || isRoot global
    | GVarDecl (v, _)
    | GFunDecl (_,v, _)->
      is_reachable reachable_tbl (Var v) ||
      Cil.hasAttribute "FC_BUILTIN" v.vattr ||
      (if isRoot global then true else (Cil.removeFormalsDecl v; false))
    (* keep FC_BUILTIN, as some plug-ins might want to use them later
       for semi-legitimate reasons. *)
    | GFun (func, _) ->
      (* if some generated temp variables are useless, remove them.
         Keep variables that were already present in the code.
      *)
      let filterLocal local =
        if (local.vtemp || local.vstorage = Static) &&
           not (is_reachable reachable_tbl (Var local)) then
          begin
            (* along the way, record the interesting locals that were removed *)
            let name = local.vname in
            (Kernel.debug ~dkey "removing local: %s" name);
            removedLocals :=
              (func.svar.vname ^ "::" ^ name) :: !removedLocals;
            false
          end else true
      in
      func.slocals <- List.filter filterLocal func.slocals;
      let remove_blocals = object
        inherit Cil.nopCilVisitor
        method! vblock b =
          b.blocals <- List.filter filterLocal b.blocals;
          b.bstatics <- List.filter filterLocal b.bstatics;
          DoChildren
      end
      in
      ((is_reachable reachable_tbl (Var func.svar))
       || Cil.hasAttribute "FC_BUILTIN" func.svar.vattr
       || isRoot global) &&
      (ignore (visitCilBlock remove_blocals func.sbody);
       remove_unused_labels func;
       true)

    (* all other globals are retained *)
    | _ -> true
  in
  let keptGlobals, removedGlobals = List.partition filterGlobal ast.globals in
  ast.globals <- keptGlobals;
  if Kernel.is_debug_key_enabled dkey then
    List.iter (fun rg ->
        Kernel.debug ~dkey "removing global: %s" (global_type_and_name rg)
      ) removedGlobals;
  if Kernel.is_debug_key_enabled dkey then
    List.iter (fun rg ->
        begin
          match rg with
          | GFunDecl (_s, vi, _) ->
            begin
              try
                let kf = Globals.Functions.get vi in
                Kernel.debug ~dkey "GFunDecl: %a@." Kernel_function.pretty_code kf
              with Not_found ->
                Kernel.debug ~dkey "GFunDecl: not found for %a@." Printer.pp_varinfo vi;
            end
          | _ -> ()
        end;
        Kernel.debug ~dkey "kept global %s (%a)" (global_type_and_name rg) Printer.pp_global rg
      ) keptGlobals;
  !removedLocals


(***********************************************************************
 *
 *  Exported interface
 *
*)

let removeUnused ?(isRoot=isExportedRoot) ast =
  if not !keepUnused then
    begin
      Kernel.debug ~dkey "Removing unused" ;

      (* digest any pragmas that would create additional roots *)
      let keepers = categorizePragmas ast in

      let reachable_tbl = InfoHashtbl.create 43 in
      (* build up the root set *)
      let isRoot global =
        isPragmaRoot keepers global ||
        isRoot global
      in

      (* mark everything reachable from the global roots *)
      markReachable isRoot ast reachable_tbl;

      Kernel.debug ~dkey "reachable_tbl: %t"
        (fun fmt ->
           let elements =
             InfoHashtbl.fold (fun k v acc ->
                 Format.asprintf "%a:%B" pp_info k v :: acc)
               reachable_tbl []
           in
           Format.fprintf fmt "%a"
             (Pretty_utils.pp_list ~sep:"@\n" Format.pp_print_string) elements);

      markReferenced ast;

      (* take out the trash *)
      ignore (removeUnmarked isRoot ast reachable_tbl)
    end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
