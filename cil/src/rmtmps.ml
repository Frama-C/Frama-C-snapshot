(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

let level=666

open Cil_types
open Cil
module H = Hashtbl
module U = Cilutil

(* Set on the command-line: *)
let keepUnused = ref false
let rmUnusedInlines = ref false
let rmUnusedStatic = ref false
let rmEmptyInlines = ref true

(***********************************************************************
 *
 *  Clearing of "referenced" bits
 *
 *)


let clearReferencedBits file =
  let considerGlobal global =
    match global with
    | GType (info, _) ->
	(*trace (dprintf "clearing mark: %a\n" d_shortglobal global);*)
	info.treferenced <- false

    | GEnumTag (info, _)
    | GEnumTagDecl (info, _) ->
	(*trace (dprintf "clearing mark: %a\n" d_shortglobal global);*)
	info.ereferenced <- false

    | GCompTag (info, _)
    | GCompTagDecl (info, _) ->
	(*trace (dprintf "clearing mark: %a\n" d_shortglobal global);*)
	info.creferenced <- false

    | GVar ({vname = _name} as info, _, _)
    | GVarDecl (_,({vname = _name} as info), _) ->
	(*trace (dprintf "clearing mark: %a\n" d_shortglobal global);*)
	info.vreferenced <- false

    | GFun ({svar = info} as func, _) ->
	(*trace (dprintf "clearing mark: %a\n" d_shortglobal global);*)
	info.vreferenced <- false;
	let clearMark local =
	  (*trace (dprintf "clearing mark: local %s\n" local.vname);*)
	  local.vreferenced <- false
	in
	List.iter clearMark func.slocals

    | _ ->
	()
  in
  iterGlobals file considerGlobal


(***********************************************************************
 *
 *  Scanning and categorization of pragmas
 *
 *)


(* collections of names of things to keep *)
type collection = (string, unit) H.t
type keepers = {
    typedefs : collection;
    enums : collection;
    structs : collection;
    unions : collection;
    defines : collection;
  }


(* rapid transfer of control when we find a malformed pragma *)
exception Bad_pragma

let ccureddeepcopystring = "ccureddeepcopy"
(* Save this length so we don't recompute it each time. *)
let ccureddeepcopystring_length = String.length ccureddeepcopystring

(* CIL and CCured define several pragmas which prevent removal of
 * various global varinfos.  Here we scan for those pragmas and build
 * up collections of the corresponding varinfos' names.
 *)

let categorizePragmas file =

  (* names of things which should be retained *)
  let keepers = {
    typedefs = H.create 0;
    enums = H.create 0;
    structs = H.create 0;
    unions = H.create 0;
    defines = H.create 1
  } in

  (* populate these name collections in light of each pragma *)
  let considerPragma =

    let badPragma location pragma =
      Cilmsg.warning ~source:(source location) "Invalid argument to pragma %s" pragma
    in

    function
      | GPragma (Attr ("cilnoremove" as directive, args), location) ->
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
		    H.add collection name ()
		| _ ->
		    raise Bad_pragma
	      with Bad_pragma ->
		badPragma location directive
	    in
	    List.iter processArg args
	  end
      | GVarDecl (_,v, _) -> begin
          (* Look for alias attributes, e.g. Linux modules *)
          match filterAttributes "alias" v.vattr with
              [] -> ()  (* ordinary prototype. *)
            | [Attr("alias", [AStr othername])] ->
                H.add keepers.defines othername ()
            | _ ->
		(Cil.fatal "Bad alias attribute at %a" d_loc (CurrentLoc.get ()))
        end

      (*** Begin CCured-specific checks:  ***)
      (* these pragmas indirectly require that we keep the function named in
	  -- the first arguments of boxmodelof and ccuredwrapperof, and
	  -- the third argument of ccureddeepcopy*. *)
      | GPragma (Attr("ccuredwrapper" as directive, attribute :: _), location) ->
	  begin
	    match attribute with
	    | AStr name ->
		H.add keepers.defines name ()
	    | _ ->
		badPragma location directive
	  end
      | GPragma (Attr("ccuredvararg", _funcname :: (ASizeOf t) :: _), _location) ->
	  begin
	    match t with
	    | TComp(c,_,_) when c.cstruct -> (* struct *)
		H.add keepers.structs c.cname ()
	    | TComp(c,_,_) -> (* union *)
		H.add keepers.unions c.cname ()
	    | TNamed(ti,_) ->
		H.add keepers.typedefs ti.tname ()
	    | TEnum(ei, _) ->
		H.add keepers.enums ei.ename ()
	    | _ ->
		()
	  end
      | GPragma (Attr(directive, _ :: _ :: attribute :: _), location)
           when String.length directive > ccureddeepcopystring_length
	       && (Str.first_chars directive ccureddeepcopystring_length)
	           = ccureddeepcopystring ->
	  begin
	    match attribute with
	    | AStr name ->
		H.add keepers.defines name ()
	    | _ ->
		badPragma location directive
	  end
      (** end CCured-specific stuff **)
      |	_ ->
	  ()
  in
  iterGlobals file considerPragma;
  keepers



(***********************************************************************
 *
 *  Function body elimination from pragmas
 *
 *)


(* When performing global slicing, any functions not explicitly marked
 * as pragma roots are reduced to mere declarations.  This leaves one
 * with a reduced source file that still compiles to object code, but
 * which contains the bodies of only explicitly retained functions.
 *)

let amputateFunctionBodies keptGlobals file =
  let considerGlobal = function
    | GFun ({svar = {vname = name} as info}, location)
      when not (H.mem keptGlobals name) ->
	(Cilmsg.debug ~level "slicing: reducing to prototype: function %s\n" name);
	GVarDecl (empty_funspec(),info, location)
    | other ->
	other
  in
  mapGlobals file considerGlobal



(***********************************************************************
 *
 *  Root collection from pragmas
 *
 *)


let isPragmaRoot keepers = function
  | GType ({tname = name}, _) ->
      H.mem keepers.typedefs name
  | GEnumTag ({ename = name}, _)
  | GEnumTagDecl ({ename = name}, _) ->
      H.mem keepers.enums name
  | GCompTag ({cname = name; cstruct = structure}, _)
  | GCompTagDecl ({cname = name; cstruct = structure}, _) ->
      let collection = if structure then keepers.structs else keepers.unions in
      H.mem collection name
  | GVar ({vname = name; vattr = attrs}, _, _)
  | GVarDecl (_,{vname = name; vattr = attrs}, _)
  | GFun ({svar = {vname = name; vattr = attrs}}, _) ->
      H.mem keepers.defines name ||
      hasAttribute "used" attrs
  | _ ->
      false



(***********************************************************************
 *
 *  Common root collecting utilities
 *
 *)
(*TODO:remove
let traceRoot _reason _global =
(*  trace (dprintf "root (%s): %a@!" reason d_shortglobal global);*)
  true

let traceNonRoot _reason _global =
(*  trace (dprintf "non-root (%s): %a@!" reason d_shortglobal global);*)
  false
*)
let hasExportingAttribute funvar =
  let rec isExportingAttribute = function
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
  let result, _reason = match global with
  | GVar ({vstorage = Static}, _, _) ->
    false, "static variable"
  | GVar _ ->
    true, "non-static variable"
  | GFun ({svar = v}, _) -> begin
    if hasExportingAttribute v then
      true, "constructor or destructor function"
    else if v.vstorage = Static then
      not !rmUnusedStatic, "static function"
    else if v.vinline && v.vstorage != Extern
         && (theMachine.msvcMode || !rmUnusedInlines) then
      false, "inline function"
    else
      true, "other function"
  end
  | GVarDecl(_,v,_) when hasAttribute "alias" v.vattr ->
    true, "has GCC alias attribute"
  | GVarDecl(spec,v,_) when not (Cil.is_empty_funspec spec) ->
    v.vname="main", "main has formal spec"
  | GAnnot _ -> true, "global annotation"
  | _ ->
    false, "neither function nor variable nor annotation"
  in
  (*  trace (dprintf "isExportedRoot %a -> %b, %s@!"
      d_shortglobal global result reason);*)
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
    ((globalMap: (string, Cil_types.global) H.t),
     (currentFunc: Cil_types.fundec option ref)) = object (self)
  inherit nopCilVisitor

  method vglob = function
    | GType (typeinfo, _) ->
	typeinfo.treferenced <- true;
	DoChildren
    | GCompTag (compinfo, _)
    | GCompTagDecl (compinfo, _) ->
	compinfo.creferenced <- true;
	DoChildren
    | GEnumTag (enuminfo, _)
    | GEnumTagDecl (enuminfo, _) ->
	enuminfo.ereferenced <- true;
	DoChildren
    | GVar (varinfo, _, _)
    | GVarDecl (_,varinfo, _)
    | GFun ({svar = varinfo}, _) ->
	varinfo.vreferenced <- true;
	DoChildren
    | GAnnot _ -> DoChildren
    | _ ->
	SkipChildren

  method vinst = function
      Asm (_, tmpls, _, _, _, _) when theMachine.msvcMode ->
          (* If we have inline assembly on MSVC, we cannot tell which locals
           * are referenced. Keep thsem all *)
        (match !currentFunc with
          Some fd ->
            List.iter (fun v ->
              let vre = Str.regexp_string (Str.quote v.vname) in
              if List.exists (fun tmp ->
                try ignore (Str.search_forward vre tmp 0); true
                with Not_found -> false)
                  tmpls
              then
                v.vreferenced <- true) fd.slocals
        | _ -> assert false);
        DoChildren
    | Call (None,
            {enode = Lval(Var {vname = name; vinline = true}, NoOffset)},
            args,loc) ->
        let glob = Hashtbl.find globalMap name in
          begin
            match glob with
            GFun ({sbody = {bstmts = [] | [{skind = Return (None,_)}]}},_)
                ->
                  if false then
                  ChangeTo
                    [Asm ([],["nop"],[],List.map (fun e -> None,"q",e) args ,[],loc)]
                  else ChangeTo []
            | _ -> DoChildren
        end
    | _ -> DoChildren

  method vvrbl v =
    if not v.vreferenced then
      begin
	let name = v.vname in
	if v.vglob then
	  Cilmsg.debug ~level "marking transitive use: global %s" name
	else
	  Cilmsg.debug ~level "marking transitive use: local %s" name;

        (* If this is a global, we need to keep everything used in its
	 * definition and declarations. *)
	if v.vglob then
	  begin
	    Cilmsg.debug ~level "descending: global %s" name;
	    let descend global =
	      ignore (visitCilGlobal (self :> cilVisitor) global)
	    in
	    let globals = Hashtbl.find_all globalMap name in
	    List.iter descend globals
	  end
	else begin
	  v.vreferenced <- true;
	end
      end;
    SkipChildren

  method vexpr (e: exp) =
    match e.enode with
      Const (CEnum {eihost = ei}) -> ei.ereferenced <- true; DoChildren
    | _ -> DoChildren

  method vtype typ =
    let old : bool =
      let visitAttrs attrs =
	ignore (visitCilAttributes (self :> cilVisitor) attrs)
      in
      let visitType typ =
	ignore (visitCilType (self :> cilVisitor) typ)
      in
      match typ with
      | TEnum(e, attrs) ->
	  let old = e.ereferenced in
	  if not old then
	    begin
	      (Cilmsg.debug ~level "marking transitive use: enum %s\n" e.ename);
	      e.ereferenced <- true;
	      visitAttrs attrs;
	      visitAttrs e.eattr;
              (* Must visit the value attributed to the enum constants *)
              ignore (visitCilEnumInfo (self:>cilVisitor) e);
	    end;
	  old

      | TComp(c, _, attrs) ->
	  let old = c.creferenced in
          if not old then
            begin
	      (Cilmsg.debug ~level "marking transitive use: compound %s\n" c.cname);
	      c.creferenced <- true;

              (* to recurse, we must ask explicitly *)
	      let recurse f = visitType f.ftype in
	      List.iter recurse c.cfields;
	      visitAttrs attrs;
	      visitAttrs c.cattr
	    end;
	  old

      | TNamed(ti, attrs) ->
	  let old = ti.treferenced in
          if not old then
	    begin
	      (Cilmsg.debug ~level "marking transitive use: typedef %s\n" ti.tname);
	      ti.treferenced <- true;

	      (* recurse deeper into the type referred-to by the typedef *)
	      (* to recurse, we must ask explicitly *)
	      visitType ti.ttype;
	      visitAttrs attrs
	    end;
	  old

      | _ ->
          (* for anything else, just look inside it *)
	  false
    in
    if old then
      SkipChildren
    else
      DoChildren

end


let markReachable file isRoot =
  (* build a mapping from global names back to their definitions &
   * declarations *)
  let globalMap = Hashtbl.create 137 in
  let considerGlobal global =
    match global with
    | GFun ({svar = info}, _)
    | GVar (info, _, _)
    | GVarDecl (_,info, _) ->
	Hashtbl.add globalMap info.vname global
    | _ ->
	()
  in
  iterGlobals file considerGlobal;

  let currentFunc = ref None in

  (* mark everything reachable from the global roots *)
  let visitor = new markReachableVisitor (globalMap, currentFunc) in
  let visitIfRoot global =
    if isRoot global then
      begin
(*	trace (dprintf "traversing root global: %a\n" d_shortglobal global);*)
        (match global with
          GFun(fd, _) -> currentFunc := Some fd
        | _ -> currentFunc := None);
	ignore (visitCilGlobal visitor global)
      end
    else
(*      trace (dprintf "skipping non-root global: %a\n" d_shortglobal global)*)
      ()
  in
  iterGlobals file visitIfRoot


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

class markUsedLabels is_removable (labelMap: (string, unit) H.t) =
  let keep_label dest =
  let (ln, _), _ = labelsToKeep is_removable !dest.labels in
  if ln = "" then
    Cilmsg.fatal "Statement has no label:@\n%a" Cil.d_stmt !dest ;
  (* Mark it as used *)
  H.replace labelMap ln ()
in
let keep_label_logic =
  function LogicLabel _ -> () | StmtLabel dest -> keep_label dest
in
object
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    match s.skind with
      Goto (dest, _) -> keep_label dest; DoChildren
    | _ -> DoChildren

  method vterm_node t =
    begin
      match t with
      | Tat (_,lab) -> keep_label_logic lab
      | Tapp(_,labs,_) ->
          let labs = snd (List.split labs) in List.iter keep_label_logic labs
      | _ -> ()
    end;
    DoChildren

  method vpredicate t =
    begin
      match t with
      | Pat (_,lab) -> keep_label_logic lab
      | Papp(_,labs,_) ->
          let labs = snd (List.split labs) in List.iter keep_label_logic labs
      | _ -> ()
    end;
    DoChildren

   (* No need to go into expressions or types *)
  method vexpr _ = SkipChildren
  method vtype _ = SkipChildren
                                                        end

class removeUnusedLabels is_removable (labelMap: (string, unit) H.t) = object
  inherit nopCilVisitor

  method vstmt (s: stmt) =
    let (ln, lab), lrest = labelsToKeep is_removable s.labels in
    s.labels <-
       (if ln <> "" &&
          (H.mem labelMap ln || not (is_removable lab))
          (* keep user-provided labels *)
        then (* We had labels *)
         (lab :: lrest)
       else
         lrest);
    DoChildren

   (* No need to go into expressions or instructions *)
  method vexpr _ = SkipChildren
  method vinst _ = SkipChildren
  method vtype _ = SkipChildren
end

(***********************************************************************
 *
 *  Removal of unused varinfos
 *
 *)


(* regular expression matching names of uninteresting locals *)
let uninteresting =
  let names = [
    (* Cil.makeTempVar *)
    "__cil_tmp";

    (* sm: I don't know where it comes from but these show up all over. *)
    (* this doesn't seem to do what I wanted.. *)
    "iter";

    (* various macros in glibc's <bits/string2.h> *)
    "__result";
    "__s"; "__s1"; "__s2";
    "__s1_len"; "__s2_len";
    "__retval"; "__len";

    (* various macros in glibc's <ctype.h> *)
    "__c"; "__res";

    (* We remove the __malloc variables *)
  ] in

  (* optional alpha renaming *)
  let alpha = "\\(___[0-9]+\\)?" in

  let pattern = "\\(" ^ (String.concat "\\|" names) ^ "\\)" ^ alpha ^ "$" in
  Str.regexp pattern


let label_removable = function
    Label (_,_,user) -> not user
  | Case _ | Default _ -> false

let remove_unused_labels ?(is_removable=label_removable) func =
  (* We also want to remove unused labels. We do it all here, including
   * marking the used labels *)
  let usedLabels:(string, unit) H.t = H.create 13 in
  ignore
    (visitCilBlock (new markUsedLabels is_removable usedLabels) func.sbody);
  (* And now we scan again and we remove them *)
  ignore
    (visitCilBlock (new removeUnusedLabels is_removable usedLabels) func.sbody)

let removeUnmarked isRoot file =
  let removedLocals = ref [] in

  let filterGlobal global =
    (match global with
         (* unused global types, variables, and functions are simply removed *)
       | GType ({treferenced = false}, _)
       | GCompTag ({creferenced = false}, _)
       | GCompTagDecl ({creferenced = false}, _)
       | GEnumTag ({ereferenced = false}, _)
       | GEnumTagDecl ({ereferenced = false}, _)
       | GVar ({vreferenced = false}, _, _)
       | GFun ({svar = {vreferenced = false}}, _) ->
           (*	trace (dprintf "removing global: %a\n" d_shortglobal global);*)
	   false
       | GVarDecl (_,{vreferenced = false}, _) -> false

       (* retained functions may wish to discard some unused locals *)
       | GFun (func, _) ->
	   let rec filterLocal local =
	     if not local.vreferenced then
	       begin
	         (* along the way, record the interesting locals that were removed *)
	         let name = local.vname in
	         (Cilmsg.debug ~level "removing local: %s\n" name);
	         if not (Str.string_match uninteresting name 0) then
		   removedLocals := (func.svar.vname ^ "::" ^ name) :: !removedLocals;
	       end;
	     local.vreferenced
	   in
	   func.slocals <- List.filter filterLocal func.slocals;
           let remove_blocals = object
             inherit Cil.nopCilVisitor
             method vblock b =
               b.blocals <- List.filter filterLocal b.blocals;
               DoChildren
           end
           in
           ignore (visitCilBlock remove_blocals func.sbody);
           remove_unused_labels func;
	   true

    (* all other globals are retained *)
    | _ ->
(*	trace (dprintf "keeping global: %a\n" d_shortglobal global);*)
	true) || (isRoot global)
  in
  file.globals <- List.filter filterGlobal file.globals;
  !removedLocals


(***********************************************************************
 *
 *  Exported interface
 *
 *)


type rootsFilter = global -> bool

let isDefaultRoot = isExportedRoot

let rec removeUnusedTemps ?(isRoot : rootsFilter = isDefaultRoot) file =
  if not !keepUnused then
    begin
      Cilmsg.debug ~level "Removing unused temporaries" ;

      (* digest any pragmas that would create additional roots *)
      let keepers = categorizePragmas file in

      (* if slicing, remove the bodies of non-kept functions *)
      if !Cilglobopt.sliceGlobal then
	amputateFunctionBodies keepers.defines file;

      (* build up the root set *)
      let isRoot global =
	isPragmaRoot keepers global ||
	isRoot global
      in

      (* mark everything reachable from the global roots *)
      clearReferencedBits file;
      markReachable file isRoot;

      (* take out the trash *)
      let removedLocals = removeUnmarked isRoot file in

      (* print which original source variables were removed *)
      if false && removedLocals != [] then
	let count = List.length removedLocals in
	if count > 2000 then
	  (Cilmsg.warning "%d unused local variables removed" count)
	else
	  (Cilmsg.warning "%d unused local variables removed:@!%a"
	     count (Pretty_utils.pp_list ~sep:",@," Format.pp_print_string) removedLocals)
    end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
