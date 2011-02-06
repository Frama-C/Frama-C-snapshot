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

(* mergecil.ml *)
(* This module is responsible for merging multiple CIL source trees into
 * a single, coherent CIL tree which contains the union of all the
 * definitions in the source files.  It effectively acts like a linker,
 * but at the source code level instead of the object code level. *)


open Cil_types
open Cil
module H = Hashtbl
module A = Alpha
open Format

open Logic_const
open Logic_utils

let debugMerge = false
let debugInlines = false

let ignore_merge_conflicts = ref false

(* Try to merge structure with the same name. However, do not complain if
 * they are not the same *)
let mergeSynonyms = true


(** Whether to use path compression *)
let usePathCompression = false

(* Try to merge definitions of inline functions. They can appear in multiple
 * files and we would like them all to be the same. This can slow down the
 * merger an order of magnitude !!! *)
let mergeInlines = true

let mergeInlinesRepeat = mergeInlines && true

(* This may become an option of Frama-C. The default value has been changed
   to false after Boron to fix bts#524.
*)
let mergeInlinesWithAlphaConvert = mergeInlines && false


(* when true, merge duplicate definitions of externally-visible functions;
 * this uses a mechanism which is faster than the one for inline functions,
 * but only probabilistically accurate *)
let mergeGlobals = true


(* Return true if 's' starts with the prefix 'p' *)
let prefix p s =
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p



(* A name is identified by the index of the file in which it occurs (starting
 * at 0 with the first file) and by the actual name. We'll keep name spaces
 * separate *)

(* We define a data structure for the equivalence classes *)
type 'a node =
    { nname: string;   (* The actual name *)
      nfidx: int;      (* The file index *)
      ndata: 'a;       (* Data associated with the node *)
      mutable nloc: (location * int) option;
      (* location where defined and index within the file of the definition.
       * If None then it means that this node actually DOES NOT appear in the
       * given file. In rare occasions we need to talk in a given file about
       * types that are not defined in that file. This happens with undefined
       * structures but also due to cross-contamination of types in a few of
       * the cases of combineType (see the definition of combineTypes). We
       * try never to choose as representatives nodes without a definition.
       * We also choose as representative the one that appears earliest *)
      mutable nrep: 'a node;  (* A pointer to another node in its class (one
                               * closer to the representative). The nrep node
                               * is always in an earlier file, except for the
                               * case where a name is undefined in one file
                               * and defined in a later file. If this pointer
                               * points to the node itself then this is the
                               * representative.  *)
      mutable nmergedSyns: bool (* Whether we have merged the synonyms for
                                 * the node of this name *)
    }

let d_nloc fmt (lo: (location * int) option) =
  match lo with
    None -> Format.fprintf fmt "None"
  | Some (l, idx) -> Format.fprintf fmt "Some(%d at %a)" idx d_loc l

(* Make a node with a self loop. This is quite tricky. *)
let mkSelfNode (eq: (int * string, 'a node) H.t) (* The equivalence table *)
               (syn: (string, 'a node) H.t) (* The synonyms table *)
               (fidx: int) (name: string) (data: 'a)
               (l: (location * int) option) =
  let rec res = { nname = name; nfidx = fidx; ndata = data; nloc = l;
                  nrep  = res; nmergedSyns = false; }
  in
  H.add eq (fidx, name) res; (* Add it to the proper table *)
  (* mergeSynonyms is not active for anonymous types, probably because it is
     licit to have two distinct anonymous types in two different files
     (which should not be merged). However, for anonymous enums, they
     can, and are, in fact merged by CIL. Hence, we permit the merging of
     anonymous enums with the same base name *)
  if mergeSynonyms && (not (prefix "__anon" name) || prefix "__anonenum" name)
  then H.add syn name res;
  res

let debugFind = false

(* Find the representative with or without path compression *)
let rec find (pathcomp: bool) (nd: 'a node) =
  if debugFind then
    Cilmsg.debug "  find %s(%d)" nd.nname nd.nfidx ;
  if nd.nrep == nd then begin
    if debugFind then
      Cilmsg.debug "  = %s(%d)" nd.nname nd.nfidx ;
    nd
  end else begin
    let res = find pathcomp nd.nrep in
    if usePathCompression && pathcomp && nd.nrep != res then
      nd.nrep <- res; (* Compress the paths *)
    res
  end


(* Union two nodes and return the new representative. We prefer as the
 * representative a node defined earlier. We try not to use as
 * representatives nodes that are not defined in their files. We return a
 * function for undoing the union. Make sure that between the union and the
 * undo you do not do path compression *)
let union (nd1: 'a node) (nd2: 'a node) : 'a node * (unit -> unit) =
  (* Move to the representatives *)
  let nd1 = find true nd1 in
  let nd2 = find true nd2 in
  if nd1 == nd2 then begin
    (* It can happen that we are trying to union two nodes that are already
     * equivalent. This is because between the time we check that two nodes
     * are not already equivalent and the time we invoke the union operation
     * we check type isomorphism which might change the equivalence classes *)
(*
    ignore (warn "unioning already equivalent nodes for %s(%d)"
              nd1.nname nd1.nfidx);
*)
    nd1, fun x -> x
  end else begin
    let rep, norep = (* Choose the representative *)
      if (nd1.nloc != None) =  (nd2.nloc != None) then
        (* They have the same defined status. Choose the earliest *)
        if nd1.nfidx < nd2.nfidx then nd1, nd2
        else if nd1.nfidx > nd2.nfidx then nd2, nd1
        else (* In the same file. Choose the one with the earliest index *) begin
          match nd1.nloc, nd2.nloc with
            Some (_, didx1), Some (_, didx2) ->
              if didx1 < didx2 then nd1, nd2 else
              if didx1 > didx2 then nd2, nd1
              else begin
		Cil.warning
		  "Merging two elements (%s and %s) in the same file (%d) with the same idx (%d) within the file"
                  nd1.nname nd2.nname nd1.nfidx didx1 ;
                nd1, nd2
              end
          | _, _ -> (* both none. Does not matter which one we choose. Should
              * not happen though. *)
              (* sm: it does happen quite a bit when, e.g. merging STLport with
               * some client source; I'm disabling the warning since it supposedly
               * is harmless anyway, so is useless noise *)
              (* sm: re-enabling on claim it now will probably not happen *)
              Cil.warning
		"Merging two undefined elements in the same file: %s and %s"
		nd1.nname nd2.nname ;
              nd1, nd2
        end
      else (* One is defined, the other is not. Choose the defined one *)
        if nd1.nloc != None then nd1, nd2 else nd2, nd1
    in
    let oldrep = norep.nrep in
    norep.nrep <- rep;
    rep, (fun () -> norep.nrep <- oldrep)
  end
(*
let union (nd1: 'a node) (nd2: 'a node) : 'a node * (unit -> unit) =
  if nd1 == nd2 && nd1.nname = "!!!intEnumInfo!!!" then begin
    ignore (warn "unioning two identical nodes for %s(%d)"
              nd1.nname nd1.nfidx);
    nd1, fun x -> x
  end else
    union nd1 nd2
*)
(* Find the representative for a node and compress the paths in the process *)
let findReplacement
    (pathcomp: bool)
    (eq: (int * string, 'a node) H.t)
    (fidx: int)
    (name: string) : ('a * int) option =
  if debugFind then
    Cilmsg.debug "findReplacement for %s(%d)" name fidx ;
  try
    let nd = H.find eq (fidx, name) in
    if nd.nrep == nd then begin
      if debugFind then
        Cilmsg.debug "  is a representative" ;
      None (* No replacement if this is the representative of its class *)
    end else
      let rep = find pathcomp nd in
      if rep != rep.nrep then
        Cilmsg.abort "find does not return the representative" ;
      if debugFind then
        Cilmsg.debug "  RES = %s(%d)" rep.nname rep.nfidx ;
      Some (rep.ndata, rep.nfidx)
  with Not_found -> begin
    if debugFind then
      Cilmsg.debug "  not found in the map";
    None
  end

(* Make a node if one does not already exist. Otherwise return the
 * representative *)
let getNode    (eq: (int * string, 'a node) H.t)
               (syn: (string, 'a node) H.t)
               (fidx: int) (name: string) (data: 'a)
               (l: (location * int) option) =
  let debugGetNode = false in
  if debugGetNode then
    Cilmsg.debug "getNode(%s(%d), %a)" name fidx d_nloc l;
  try
    let res = H.find eq (fidx, name) in

    (match res.nloc, l with
      (* Maybe we have a better location now *)
      None, Some _ -> res.nloc <- l
    | Some (old_l, old_idx), Some (l, idx) ->
        if old_idx != idx  then
	  Cil.warning
	    "Duplicate definition of node %s(%d) at indices %d(%a) and %d(%a)"
            name fidx old_idx d_loc old_l idx d_loc l
    | _, _ -> ());
    if debugGetNode then Cilmsg.debug "  node already found";
    find false res (* No path compression *)
  with Not_found -> begin
    let res = mkSelfNode eq syn fidx name data l in
    if debugGetNode then Cilmsg.debug "   made a new one";
    res
  end



(* Dump a graph *)
let dumpGraph (what: string) (eq: (int * string, 'a node) H.t) : unit =
  Cilmsg.debug "Equivalence graph for %s is:" what;
  H.iter (fun (fidx, name) nd ->
            Cilmsg.debug "  %s(%d) %s-> "
              name fidx (if nd.nloc = None then "(undef)" else "");
            if nd.nrep == nd then
              Cilmsg.debug "*"
            else
              Cilmsg.debug " %s(%d)" nd.nrep.nname nd.nrep.nfidx
         ) eq




(* For each name space we define a set of equivalence classes *)
let vEq: (int * string, varinfo node) H.t = H.create 111 (* Vars *)
let sEq: (int * string, compinfo node) H.t = H.create 111 (* Struct + union *)
let eEq: (int * string, enuminfo node) H.t = H.create 111 (* Enums *)
let tEq: (int * string, typeinfo node) H.t = H.create 111 (* Type names*)
let iEq: (int * string, varinfo node) H.t = H.create 111 (* Inlines *)

let lfEq: (int * string, logic_info node) H.t = H.create 111 (* Logic functions *)
let ltEq: (int * string, logic_type_info node) H.t = H.create 111 (* Logic types *)
let lcEq: (int * string, logic_ctor_info node) H.t = H.create 111 (* Logic constructors *)

let laEq: (int * string, (string * global_annotation list) node) H.t = H.create 111
  (* Axiomatics *)
let llEq: (int * string, (string * (bool * logic_label list * string list *
                          predicate named * location)) node) H.t = H.create 111

exception NotHere
let translate table data get_info =
  let result =
    let result = ref None in
    try
      Hashtbl.iter
        (fun _ node ->
           if get_info node.ndata == get_info data then
             (result := Some node;
              raise Exit))
        table;
      raise NotHere
    with Exit ->
      match !result with
      | None -> raise NotHere
      | Some r -> r
  in
  if result == result.nrep then (* Name is already correct *) data
  else result.nrep.ndata

let translate_vinfo info =
  try
    let new_vi = translate vEq info (fun v -> v.vid) in
    (* Format.eprintf "TRANS %s(%d) to %s(%d)@\n"
      info.vname info.vid new_vi.vname new_vi.vid;*)
    new_vi
  with NotHere -> info
let translate_typinfo info =
  try translate tEq info (fun v -> v.tname) with NotHere -> info


(* Sometimes we want to merge synonyms. We keep some tables indexed by names.
 * Each name is mapped to multiple exntries *)
let vSyn: (string, varinfo node) H.t = H.create 111 (* Not actually used *)
let iSyn: (string, varinfo node) H.t = H.create 111 (* Inlines *)
let sSyn: (string, compinfo node) H.t = H.create 111
let eSyn: (string, enuminfo node) H.t = H.create 111
let tSyn: (string, typeinfo node) H.t = H.create 111
let lfSyn: (string, logic_info node) H.t = H.create 111
let ltSyn: (string, logic_type_info node) H.t = H.create 111
let lcSyn: (string, logic_ctor_info node) H.t = H.create 111
let laSyn: (string, (string * global_annotation list) node) H.t = H.create 111
let llSyn: (string, (string * (bool * logic_label list * string list *
                                 predicate named * location)) node) H.t = H.create 111

(** A global environment for variables. Put in here only the non-static
  * variables, indexed by their name.  *)
let vEnv : (string, varinfo node) H.t = H.create 111

(* A set of inline functions indexed by their printout ! *)
let inlineBodies : (string, varinfo node) H.t = H.create 111

(** A number of alpha conversion tables. We ought to keep one table for each
 * name space. Unfortunately, because of the way the C lexer works, type
 * names must be different from variable names!! We one alpha table both for
 * variables and types. *)
let vtAlpha : (string, location A.alphaTableData ref) H.t
    = H.create 57 (* Variables and
                   * types *)
let sAlpha : (string, location A.alphaTableData ref) H.t
    = H.create 57 (* Structures and
                   * unions have
                   * the same name
                   * space *)
let eAlpha : (string, location A.alphaTableData ref) H.t
    = H.create 57 (* Enumerations *)


(** Keep track, for all global function definitions, of the names of the formal
 * arguments. They might change during merging of function types if the
 * prototype occurs after the function definition and uses different names.
 * We'll restore the names at the end *)
let formalNames: (int * string, string list) H.t = H.create 111


(* Accumulate here the globals in the merged file *)
let theFileTypes = ref []
let theFile      = ref []

(*  we keep only one declaration for each function. The other ones are simply
    discarded, but we need to merge their spec. This is done at the end
    of the 2nd pass, to avoid going through theFile too many times.
 *)
let spec_to_merge = Hashtbl.create 59;;

(* renaming to be performed in spec found in declarations when there is
   a definition for the given function. Similar to spec_to_merge table.
 *)
let formals_renaming = Hashtbl.create 59;;

(* add 'g' to the merged file *)
let mergePushGlobal (g: global) : unit =
  pushGlobal g ~types:theFileTypes ~variables:theFile

let mergePushGlobals gl = List.iter mergePushGlobal gl

let add_to_merge_spec vi spec =
  let l =
    try Hashtbl.find spec_to_merge vi.vid
    with Not_found -> []
  in Hashtbl.replace spec_to_merge vi.vid (spec::l)

let add_alpha_renaming old_vi old_args new_args =
  try
    Hashtbl.add
      formals_renaming
      old_vi.vid
      (Cil.create_alpha_renaming old_args new_args)
  with Invalid_argument _ ->
    (* [old_args] and [new_args] haven't the same length.
       May occur at least when trying to merge incompatible declarations. *)
    ()

let mergeSpec vi_ref vi_disc spec =
  if not (Cil.is_empty_funspec spec) then
    let spec =
      try
	let alpha =
          Cil.create_alpha_renaming
            (Cil.getFormalsDecl vi_disc)
	    (Cil.getFormalsDecl vi_ref)
	in
	try Cil.visitCilFunspec alpha spec with Not_found -> assert false
      with Not_found ->
	spec
    in
    add_to_merge_spec vi_ref spec
  (* else no need keep empty specs *)

(* The index of the current file being scanned *)
let currentFidx = ref 0

let currentDeclIdx = ref 0 (* The index of the definition in a file. This is
                            * maintained both in pass 1 and in pass 2. Make
                            * sure you count the same things in both passes. *)
(* Keep here the file names *)
let fileNames : (int, string) H.t = H.create 113



(* Remember the composite types that we have already declared *)
let emittedCompDecls: (string, bool) H.t = H.create 113
(* Remember the variables also *)
let emittedVarDecls: (string, bool) H.t = H.create 113

(* also keep track of externally-visible function definitions;
 * name maps to declaration, location, and semantic checksum *)
let emittedFunDefn: (string, fundec * location * int) H.t = H.create 113
(* and same for variable definitions; name maps to GVar fields *)
let emittedVarDefn: (string, varinfo * init option * location) H.t = H.create 113

(** A mapping from the new names to the original names. Used in PASS2 when we
 * rename variables. *)
let originalVarNames: (string, string) H.t = H.create 113

(* Initialize the module *)
let init ?(all=true) () =
  H.clear sAlpha;
  H.clear eAlpha;
  H.clear vtAlpha;

  H.clear vEnv;

  if all then H.clear vEq;

  H.clear sEq;
  H.clear eEq;
  H.clear tEq;
  H.clear iEq;

  H.clear lfEq;
  H.clear ltEq;
  H.clear lcEq;
  H.clear laEq;
  H.clear llEq;

  H.clear vSyn;
  H.clear sSyn;
  H.clear eSyn;
  H.clear tSyn;
  H.clear iSyn;

  H.clear lfSyn;
  H.clear ltSyn;
  H.clear lcSyn;
  H.clear laSyn;
  H.clear llSyn;

  theFile := [];
  theFileTypes := [];

  H.clear formalNames;
  H.clear inlineBodies;

  currentFidx := 0;
  currentDeclIdx := 0;
  H.clear fileNames;

  H.clear emittedVarDecls;
  H.clear emittedCompDecls;

  H.clear emittedFunDefn;
  H.clear emittedVarDefn;

  H.clear originalVarNames;
  if all then Logic_env.prepare_tables ()

let rec global_annot_pass1 g = match g with
| Daxiomatic(id,decls,l) ->
    CurrentLoc.set l;
    ignore (getNode laEq laSyn !currentFidx id (id,decls)
              (Some (l,!currentDeclIdx)));
     List.iter global_annot_pass1 decls
| Dfun_or_pred (li,l) ->
    CurrentLoc.set l;
    (* FIXME: this is a copy of above, is it still correct for predicate ? *)
    let mynode = getNode lfEq lfSyn !currentFidx li.l_var_info.lv_name li None in
    (* NB: in case of mix decl/def it is the decl location that is taken. *)
    if mynode.nloc = None then
      ignore (getNode lfEq lfSyn !currentFidx li.l_var_info.lv_name li
               (Some (l, !currentDeclIdx)))

| Dtype_annot (pi,l) ->
    CurrentLoc.set l;
    ignore (getNode lfEq lfSyn !currentFidx pi.l_var_info.lv_name pi
              (Some (l, !currentDeclIdx)))
| Dinvariant (pi,l)  ->
    CurrentLoc.set l;
    ignore (getNode lfEq lfSyn !currentFidx pi.l_var_info.lv_name pi
              (Some (l, !currentDeclIdx)))
| Dtype (info,l) ->
    CurrentLoc.set l;
    ignore (getNode ltEq ltSyn !currentFidx info.lt_name info
              (Some (l, !currentDeclIdx)))

| Dlemma (n,is_ax,labs,typs,st,l) ->
    CurrentLoc.set l;
    ignore (getNode llEq llSyn !currentFidx n (n,(is_ax,labs,typs,st,l))
              (Some (l, !currentDeclIdx)))

(* Some enumerations have to be turned into an integer. We implement this by
 * introducing a special enumeration type which we'll recognize later to be
 * an integer *)
let intEnumInfo =
  let name = "!!!intEnumInfo!!!"
    (* invalid C name. Can't clash with anything. *)
  in
  { eorig_name = name;
    ename = name;
    eitems = [];
    eattr = [];
    ereferenced = false;
  }
(* And add it to the equivalence graph *)
let intEnumInfoNode =
  getNode eEq eSyn 0 intEnumInfo.ename intEnumInfo
                     (Some (Cil_datatype.Location.unknown, 0))

    (* Combine the types. Raises the Failure exception with an error message.
     * isdef says whether the new type is for a definition *)
type combineWhat =
    CombineFundef (* The new definition is for a function definition. The old
                   * is for a prototype *)
  | CombineFunarg (* Comparing a function argument type with an old prototype
                   * arg *)
  | CombineFunret (* Comparing the return of a function with that from an old
                   * prototype *)
  | CombineOther

let same_int64 e1 e2 =
  match (constFold true e1).enode, (constFold true e2).enode with
    | Const(CInt64(i, _, _)), Const(CInt64(i', _, _)) -> i = i'
    | _ -> false

let rec combineTypes (what: combineWhat)
    (oldfidx: int)  (oldt: typ)
    (fidx: int) (t: typ)  : typ =
  match oldt, t with
  | TVoid olda, TVoid a -> TVoid (addAttributes olda a)
  | TInt (oldik, olda), TInt (ik, a) ->
      let combineIK oldk k =
        if oldk == k
        then oldk
        else
          if bytesSizeOfInt oldk=bytesSizeOfInt k && isSigned oldk=isSigned k
          then
            (* the types contain the same sort of values but are not equal.
               For example on x86_16 machep unsigned short and unsigned int. *)
            if rank oldk<rank k then k else oldk
          else
            (* GCC allows a function definition to have a more precise integer
             * type than a prototype that says "int" *)
            if not theMachine.msvcMode && oldk = IInt && bitsSizeOf t <= 32
              && (what = CombineFunarg || what = CombineFunret)
            then
              k
            else (
              let msg =
                Pretty_utils.sfprintf
		  "(different integer types %a and %a)"
		  d_type oldt d_type t
              in
              raise (Failure msg))
      in
      TInt (combineIK oldik ik, addAttributes olda a)

  | TFloat (oldfk, olda), TFloat (fk, a) ->
      let combineFK oldk k =
        if oldk == k then oldk else
          (* GCC allows a function definition to have a more precise integer
           * type than a prototype that says "double" *)
          if not theMachine.msvcMode && oldk = FDouble && k = FFloat
            && (what = CombineFunarg || what = CombineFunret)
          then
            k
          else
            raise (Failure "(different floating point types)")
      in
      TFloat (combineFK oldfk fk, addAttributes olda a)

  | TEnum (oldei, olda), TEnum (ei, a) ->
      (* Matching enumerations always succeeds. But sometimes it maps both
       * enumerations to integers *)
      matchEnumInfo oldfidx oldei fidx ei;
      TEnum (oldei, addAttributes olda a)


  (* Strange one. But seems to be handled by GCC *)
  | TEnum (oldei, olda) , TInt(IInt, a) -> TEnum(oldei,
                                                 addAttributes olda a)

  (* Strange one. But seems to be handled by GCC. Warning. Here we are
   * leaking types from new to old  *)
  | TInt(IInt, olda), TEnum (ei, a) -> TEnum(ei, addAttributes olda a)

  | TComp (oldci, _, olda) , TComp (ci, _, a) ->
      matchCompInfo oldfidx oldci fidx ci;
      (* If we get here we were successful *)
      TComp (oldci, empty_size_cache (), addAttributes olda a)

  | TArray (oldbt, oldsz, _, olda), TArray (bt, sz, _, a) ->
      let combbt = combineTypes CombineOther oldfidx oldbt fidx bt in
      let combinesz =
        match oldsz, sz with
          None, Some _ -> sz
        | Some _, None -> oldsz
        | None, None -> oldsz
        | Some oldsz', Some sz' ->
            if same_int64 oldsz' sz' then oldsz else
              raise (Failure "(different array sizes)")
      in
      TArray (combbt, combinesz, empty_size_cache (), addAttributes olda a)

  | TPtr (oldbt, olda), TPtr (bt, a) ->
      TPtr (combineTypes CombineOther oldfidx oldbt fidx bt,
            addAttributes olda a)

  (* WARNING: In this case we are leaking types from new to old !! *)
  | TFun (_, _, _, [Attr("missingproto",_)]), TFun _ -> t


  | TFun _, TFun (_, _, _, [Attr("missingproto",_)]) -> oldt

  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
      let newrt =
        combineTypes
          (if what = CombineFundef then CombineFunret else CombineOther)
          oldfidx oldrt fidx rt
      in
      if oldva != va then
        raise (Failure "(diferent vararg specifiers)");
      (* If one does not have arguments, believe the one with the
       * arguments *)
      let newargs =
        if oldargs = None then args else
          if args = None then oldargs else
            let oldargslist = argsToList oldargs in
            let argslist = argsToList args in
            if List.length oldargslist <> List.length argslist then
              raise (Failure "(different number of arguments)")
            else begin
              (* Go over the arguments and update the old ones with the
               * adjusted types *)
              Some
		(List.map2
		   (fun (on, ot, oa) (an, at, aa) ->
                      let n = if an <> "" then an else on in
                      let t =
			combineTypes
			  (if what = CombineFundef then CombineFunarg
			   else CombineOther)
			  oldfidx ot fidx at
                      in
                      let a = addAttributes oa aa in
		      (n, t, a))
		   oldargslist argslist)
            end
      in
      TFun (newrt, newargs, oldva, addAttributes olda a)

  | TBuiltin_va_list olda, TBuiltin_va_list a ->
      TBuiltin_va_list (addAttributes olda a)

  | TNamed (oldt, olda), TNamed (t, a) ->
      matchTypeInfo oldfidx oldt fidx t;
      (* If we get here we were able to match *)
      TNamed(oldt, addAttributes olda a)

  (* Unroll first the new type *)
  | _, TNamed (t, a) ->
      let res = combineTypes what oldfidx oldt fidx t.ttype in
      typeAddAttributes a res

  (* And unroll the old type as well if necessary *)
  | TNamed (oldt, a), _ ->
      let res = combineTypes what oldfidx oldt.ttype fidx t in
      typeAddAttributes a res

  | _ -> (
      (* raise (Failure "(different type constructors)") *)
      let msg:string =
        Pretty_utils.sfprintf
          "(different type constructors: %a vs. %a)"
          d_type oldt  d_type t
      in
      raise (Failure msg))


(* Match two compinfos and throw a Failure if they do not match *)
and matchCompInfo (oldfidx: int) (oldci: compinfo)
    (fidx: int)    (ci: compinfo) : unit =
  if oldci.cstruct <> ci.cstruct then
    raise (Failure "(different struct/union types)");
  (* See if we have a mapping already *)
  (* Make the nodes if not already made. Actually return the
   * representatives *)
  let oldcinode = getNode sEq sSyn oldfidx oldci.cname oldci None in
  let    cinode = getNode sEq sSyn    fidx    ci.cname    ci None in
  if oldcinode == cinode then (* We already know they are the same *)
    ()
  else begin
    (* Replace with the representative data *)
    let oldci = oldcinode.ndata in
    let oldfidx = oldcinode.nfidx in
    let ci = cinode.ndata in
    let fidx = cinode.nfidx in

    let old_len = List.length oldci.cfields in
    let len = List.length ci.cfields in
    (* It is easy to catch here the case when the new structure is undefined
     * and the old one was defined. We just reuse the old *)
    (* More complicated is the case when the old one is not defined but the
     * new one is. We still reuse the old one and we'll take care of defining
     * it later with the new fields.
     * GN: 7/10/04, I could not find when is "later", so I added it below *)
    if len <> 0 && old_len <> 0 && old_len <> len then begin
      let curLoc = CurrentLoc.get () in     (* d_global blows this away.. *)
      CurrentLoc.set curLoc;
      let msg = Printf.sprintf
	"(different number of fields in %s and %s: %d != %d.)"
	oldci.cname ci.cname old_len len in
      raise (Failure msg)
    end;
    (* We check that they are defined in the same way. While doing this there
     * might be recursion and we have to watch for going into an infinite
     * loop. So we add the assumption that they are equal *)
    let newrep, undo = union oldcinode cinode in
    (* We check the fields but watch for Failure. We only do the check when
     * the lengths are the same. Due to the code above this the other
     * possibility is that one of the length is 0, in which case we reuse the
     * old compinfo. *)
    (* But what if the old one is the empty one ? *)
    if old_len = len then begin
      try
        List.iter2
          (fun oldf f ->
             if oldf.fbitfield <> f.fbitfield then
	       raise (Failure "(different bitfield info)");
             if oldf.fattr <> f.fattr then
	       raise (Failure "(different field attributes)");
             (* Make sure the types are compatible *)
             let newtype =
	       combineTypes CombineOther oldfidx oldf.ftype fidx f.ftype
             in
             (* Change the type in the representative *)
             oldf.ftype <- newtype)
          oldci.cfields ci.cfields
      with Failure reason ->
        (* Our assumption was wrong. Forget the isomorphism *)
        undo ();
        let msg =
          Pretty_utils.sfprintf
            "\n\tFailed assumption that %s and %s are isomorphic %s@?%a@?%a"
            (compFullName oldci) (compFullName ci) reason
            dn_global (GCompTag(oldci, Cil_datatype.Location.unknown))
            dn_global (GCompTag(ci, Cil_datatype.Location.unknown))
        in
        raise (Failure msg)
    end else begin
      (* We will reuse the old one. One of them is empty. If the old one is
       * empty, copy over the fields from the new one. Won't this result in
       * all sorts of undefined types??? *)
      if old_len = 0 then
        oldci.cfields <- ci.cfields;
    end;
    (* We get here when we succeeded checking that they are equal, or one of
     * them was empty *)
    newrep.ndata.cattr <- addAttributes oldci.cattr ci.cattr;
    ()
  end

(* Match two enuminfos and throw a Failure if they do not match *)
and matchEnumInfo (oldfidx: int) (oldei: enuminfo)
    (fidx: int)    (ei: enuminfo) : unit =
  (* Find the node for this enum, no path compression. *)
  let oldeinode = getNode eEq eSyn oldfidx oldei.ename oldei None in
  let einode    = getNode eEq eSyn fidx ei.ename ei None in
  if oldeinode == einode then (* We already know they are the same *)
    ()
  else begin
    (* Replace with the representative data *)
    let oldei = oldeinode.ndata in
    let ei = einode.ndata in
    (* Try to match them. But if you cannot just make them both integers *)
    try
      (* We do not have a mapping. They better be defined in the same way *)
      if List.length oldei.eitems <> List.length ei.eitems then
        raise (Failure "(different number of enumeration elements)");
      (* We check that they are defined in the same way. This is a fairly
       * conservative check. *)
      List.iter2
        (fun old_item item ->
           if old_item.einame <> item.einame then
             raise (Failure "(different names for enumeration items)");
           if not (same_int64 old_item.eival item.eival) then
             raise (Failure "(different values for enumeration items)"))
        oldei.eitems ei.eitems;
      (* Set the representative *)
      let newrep, _ = union oldeinode einode in
      (* We get here if the enumerations match *)
      newrep.ndata.eattr <- addAttributes oldei.eattr ei.eattr;
      ()
    with Failure _msg -> begin
      (* Get here if you cannot merge two enumeration nodes *)
      if oldeinode != intEnumInfoNode then begin
        let _ = union oldeinode intEnumInfoNode in ()
      end;
      if einode != intEnumInfoNode then begin
        let _ = union einode intEnumInfoNode in ()
      end;
    end
  end


(* Match two typeinfos and throw a Failure if they do not match *)
and matchTypeInfo (oldfidx: int) (oldti: typeinfo)
    (fidx: int)      (ti: typeinfo) : unit =
  if oldti.tname = "" || ti.tname = "" then
    Cilmsg.fatal "matchTypeInfo for anonymous type";
  (* Find the node for this enum, no path compression. *)
  let oldtnode = getNode tEq tSyn oldfidx oldti.tname oldti None in
  let    tnode = getNode tEq tSyn    fidx ti.tname    ti None in
  if oldtnode == tnode then (* We already know they are the same *)
    ()
  else begin
    (* Replace with the representative data *)
    let oldti = oldtnode.ndata in
    let oldfidx = oldtnode.nfidx in
    let ti = tnode.ndata in
    let fidx = tnode.nfidx in
    (* Check that they are the same *)
    (try
       ignore (combineTypes CombineOther oldfidx oldti.ttype fidx ti.ttype);
     with Failure reason -> begin
       let msg =
         Format.sprintf
           "Failed assumption that %s and %s are isomorphic %s"
           oldti.tname ti.tname reason
       in
       raise (Failure msg)
     end);
    let _ = union oldtnode tnode in
    ()
  end

let static_var_visitor = object
    inherit Cil.nopCilVisitor
    method vvrbl vi = if vi.vstorage = Static then raise Exit; DoChildren
  end

(*
let has_static_ref_predicate pred_info =
  try
    ignore (visitCilPredicateInfo static_var_visitor pred_info); false
  with Exit -> true
*)

let has_static_ref_logic_function lf_info =
  try
    ignore (visitCilLogicInfo static_var_visitor lf_info); false
  with Exit -> true

let matchLogicInfo oldfidx oldpi fidx pi =
  let oldtnode = getNode lfEq lfSyn oldfidx oldpi.l_var_info.lv_name oldpi None in
  let    tnode = getNode lfEq lfSyn fidx pi.l_var_info.lv_name pi None in
  if oldtnode == tnode then (* We already know they are the same *)
    ()
  else begin
    let oldpi = oldtnode.ndata in
    let oldfidx = oldtnode.nfidx in
    let pi = tnode.ndata in
    let fidx = tnode.nfidx in
    if Logic_utils.is_same_logic_info oldpi pi then begin
      if has_static_ref_logic_function oldpi then
        Cilmsg.abort
          "multiple inclusion of logic function %s referring to a static variable"
          oldpi.l_var_info.lv_name
      else  if oldfidx < fidx then
        tnode.nrep <- oldtnode.nrep
      else
        oldtnode.nrep <- tnode.nrep
    end else
      Cilmsg.abort "invalid multiple logic function declarations %s" pi.l_var_info.lv_name
  end

let matchLogicType oldfidx oldnode fidx node =
  let oldtnode =
    getNode ltEq ltSyn oldfidx oldnode.lt_name oldnode None in
  let    tnode = getNode ltEq ltSyn fidx oldnode.lt_name node None in
  if oldtnode == tnode then (* We already know they are the same *)
    ()
  else begin
    let oldinfo = oldtnode.ndata in
    let oldfidx = oldtnode.nfidx in
    let info = tnode.ndata in
    let fidx = tnode.nfidx in
    if Logic_utils.is_same_logic_type_info oldinfo info then begin
      if oldfidx < fidx then
        tnode.nrep <- oldtnode.nrep
      else
        oldtnode.nrep <- tnode.nrep
    end else
      Cil.error "invalid multiple logic type declarations %s" node.lt_name
  end

let matchLogicCtor oldfidx oldpi fidx pi =
  let oldtnode = getNode lcEq lcSyn oldfidx oldpi.ctor_name oldpi None in
  let    tnode = getNode lcEq lcSyn fidx pi.ctor_name pi None in
  if oldtnode == tnode then (* We already know they are the same *)
    ()
  else begin
    Cil.error "invalid multiple logic constructors declarations %s" pi.ctor_name
  end

let matchLogicAxiomatic oldfidx (oldid,_ as oldnode) fidx (id,_ as node) =
  let oldanode = getNode laEq laSyn oldfidx oldid oldnode None in
  let anode = getNode laEq laSyn fidx id node None in
  if oldanode == anode then
    ()
  else begin
    let (_,oldax) = oldanode.ndata in
    let oldaidx = oldanode.nfidx in
    let (_,ax) = anode.ndata in
    let aidx = anode.nfidx in
    if Logic_utils.is_same_axiomatic oldax ax then begin
      if oldaidx < aidx then
        anode.nrep <- oldanode.nrep
      else
        oldanode.nrep <- anode.nrep
    end else
      Cil.error "invalid multiple axiomatic declarations %s" id
  end

let matchLogicLemma oldfidx (oldid, _ as oldnode) fidx (id, _ as node) =
  let oldlnode = getNode llEq llSyn oldfidx oldid oldnode None in
  let lnode = getNode llEq llSyn fidx id node None in
  if oldlnode == lnode then ()
  else begin
    let (oldid,(oldax,oldlabs,oldtyps,oldst,oldloc)) = oldlnode.ndata in
    let oldfidx = oldlnode.nfidx in
    let (id,(ax,labs,typs,st,loc)) = lnode.ndata in
    let fidx = lnode.nfidx in
    if Logic_utils.is_same_global_annotation
      (Dlemma (oldid,oldax,oldlabs,oldtyps,oldst,oldloc))
      (Dlemma (id,ax,labs,typs,st,loc))
    then begin
      if oldfidx < fidx then
        lnode.nrep <- oldlnode.nrep
      else
        oldlnode.nrep <- lnode.nrep
    end else
      Cil.error "invalid multiple lemmas or axioms  declarations for %s" id
  end

(* Scan all files and do two things *)
(* 1. Initialize the alpha renaming tables with the names of the globals so
 * that when we come in the second pass to generate new names, we do not run
 * into conflicts.  *)
(* 2. For all declarations of globals unify their types. In the process
 * construct a set of equivalence classes on type names, structure and
 * enumeration tags  *)
(* 3. We clean the referenced flags *)

let rec oneFilePass1 (f:file) : unit =
  H.add fileNames !currentFidx f.fileName;
  Cilmsg.feedback ~level:2 "Pre-merging (%d) %s" !currentFidx f.fileName ;
  currentDeclIdx := 0;
  if f.globinitcalled || f.globinit <> None then
    Cil.warning "Merging file %s has global initializer" f.fileName;

  (* We scan each file and we look at all global varinfo. We see if globals
   * with the same name have been encountered before and we merge those types
   * *)
  let matchVarinfo (vi: varinfo) (l: location * int) =
    ignore (Alpha.registerAlphaName vtAlpha None vi.vname (CurrentLoc.get ()));
    (* Make a node for it and put it in vEq *)
    let vinode = mkSelfNode vEq vSyn !currentFidx vi.vname vi (Some l) in
    try
      let oldvinode = find true (H.find vEnv vi.vname) in
      let oldloc, _ =
        match oldvinode.nloc with
          None ->  (Cilmsg.fatal "old variable is undefined")
        | Some l -> l
      in
      let oldvi = oldvinode.ndata in
      (* There is an old definition. We must combine the types. Do this first
       * because it might fail *)
      let newtype =
        try
          combineTypes CombineOther
            oldvinode.nfidx oldvi.vtype
            !currentFidx vi.vtype;
        with (Failure reason) -> begin
          (* Go ahead when ignoring conflicts *)
          let f = if !ignore_merge_conflicts then Cil.warning else Cil.fatal in
          f "@[<hov>Incompatible@ declaration@ for@ %s@ (included from @ %s).@ \
Previous@ was@ at@ %a@ (include from@ %s)@ %s]"
            vi.vname
            (H.find fileNames !currentFidx)
            d_loc oldloc
            (H.find fileNames oldvinode.nfidx)
            reason;
          vi.vtype
        end
      in
      let newrep, _ = union oldvinode vinode in
      (* We do not want to turn non-"const" globals into "const" one. That
       * can happen if one file declares the variable a non-const while
       * others declare it as "const". *)
      if hasAttribute "const" (typeAttrs vi.vtype) !=
        hasAttribute "const" (typeAttrs oldvi.vtype) then begin
          newrep.ndata.vtype <- typeRemoveAttributes ["const"] newtype;
        end else begin
          newrep.ndata.vtype <- newtype;
        end;
      (* clean up the storage.  *)
      let newstorage =
        if vi.vstorage = oldvi.vstorage || vi.vstorage = Extern then
          oldvi.vstorage
        else if oldvi.vstorage = Extern then vi.vstorage
          (* Sometimes we turn the NoStorage specifier into Static for inline
           * functions *)
        else if oldvi.vstorage = Static &&
          vi.vstorage = NoStorage then Static
        else begin
          Cil.warning
	    "Inconsistent storage specification for %s. Now is %a and previous was %a at %a"
            vi.vname
            d_storage vi.vstorage d_storage oldvi.vstorage
            d_loc oldloc ;
	  vi.vstorage
        end
      in
      newrep.ndata.vstorage <- newstorage;
      newrep.ndata.vattr <- addAttributes oldvi.vattr vi.vattr
    with Not_found -> (* Not present in the previous files. Remember it for
                       * later  *)
      H.add vEnv vi.vname vinode
  in
  List.iter
    (function
       | GVarDecl (_,vi, l) | GVar (vi, _, l) ->
           CurrentLoc.set l;
           incr currentDeclIdx;
           vi.vreferenced <- false;
           if vi.vstorage <> Static then begin
             matchVarinfo vi (l, !currentDeclIdx);
           end

       | GFun (fdec, l) ->
           CurrentLoc.set l;
           incr currentDeclIdx;
           (* Save the names of the formal arguments *)
           let _, args, _, _ = splitFunctionTypeVI fdec.svar in
           H.add formalNames (!currentFidx, fdec.svar.vname)
             (List.map (fun (n,_,_) -> n) (argsToList args));
           fdec.svar.vreferenced <- false;
           (* Force inline functions to be static. *)
           (* GN: This turns out to be wrong. inline functions are external,
            * unless specified to be static. *)
           (*
             if fdec.svar.vinline && fdec.svar.vstorage = NoStorage then
             fdec.svar.vstorage <- Static;
           *)
           if fdec.svar.vstorage <> Static then begin
             matchVarinfo fdec.svar (l, !currentDeclIdx)
           end else begin
             if fdec.svar.vinline && mergeInlines then
               (* Just create the nodes for inline functions *)
               ignore (getNode iEq iSyn !currentFidx
                         fdec.svar.vname fdec.svar (Some (l, !currentDeclIdx)))
           end
             (* Make nodes for the defined type and structure tags *)
       | GType (t, l) ->
           incr currentDeclIdx;
           t.treferenced <- false;
           if t.tname <> "" then (* The empty names are just for introducing
                                  * undefined comp tags *)
             ignore (getNode tEq tSyn !currentFidx t.tname t
                       (Some (l, !currentDeclIdx)))
           else begin (* Go inside and clean the referenced flag for the
                       * declared tags *)
             match t.ttype with
               TComp (ci, _, _ ) ->
                 ci.creferenced <- false;
                 (* Create a node for it *)
                 ignore (getNode sEq sSyn !currentFidx ci.cname ci None)

             | TEnum (ei, _) ->
                 ei.ereferenced <- false;
                 ignore (getNode eEq eSyn !currentFidx ei.ename ei None);

             | _ ->  (Cilmsg.fatal "Anonymous Gtype is not TComp")
           end

       | GCompTag (ci, l) ->
           incr currentDeclIdx;
           ci.creferenced <- false;
           ignore (getNode sEq sSyn !currentFidx ci.cname ci
                     (Some (l, !currentDeclIdx)))
       | GEnumTag (ei, l) ->
           incr currentDeclIdx;
           ei.ereferenced <- false;
           ignore (getNode eEq eSyn !currentFidx ei.ename ei
                     (Some (l, !currentDeclIdx)))

       | GAnnot (gannot,l) ->
           CurrentLoc.set l;
           incr currentDeclIdx;
           global_annot_pass1 gannot
       | _ -> ())
    f.globals


(* Try to merge synonyms. Do not give an error if they fail to merge *)
let doMergeSynonyms
    (syn : (string, 'a node) H.t)
    (_eq : (int * string, 'a node) H.t)
    (compare : int -> 'a -> int -> 'a -> unit) (* A comparison function that
                                                * throws Failure if no match *)
    : unit =
  H.iter (fun n node ->
    if not node.nmergedSyns then begin
      (* find all the nodes for the same name *)
      let all = H.find_all syn n in
      let rec tryone (classes: 'a node list) (* A number of representatives
                                              * for this name *)
                     (nd: 'a node) : 'a node list (* Returns an expanded set
                                                   * of classes *) =
        nd.nmergedSyns <- true;
        (* Compare in turn with all the classes we have so far *)
        let rec compareWithClasses = function
            [] -> [nd](* No more classes. Add this as a new class *)
          | c :: restc ->
              try
                compare c.nfidx c.ndata  nd.nfidx nd.ndata;
                (* Success. Stop here the comparison *)
                c :: restc
              with Failure _ -> (* Failed. Try next class *)
                c :: (compareWithClasses restc)
        in
        compareWithClasses classes
      in
      (* Start with an empty set of classes for this name *)
      let _ = List.fold_left tryone [] all in
      ()
    end)
    syn


let matchInlines (oldfidx: int) (oldi: varinfo)
                 (fidx: int) (i: varinfo) =
  let oldinode = getNode iEq iSyn oldfidx oldi.vname oldi None in
  let    inode = getNode iEq iSyn    fidx    i.vname    i None in
  if oldinode != inode then begin
    (* Replace with the representative data *)
    let oldi = oldinode.ndata in
    let oldfidx = oldinode.nfidx in
    let i = inode.ndata in
    let fidx = inode.nfidx in
    (* There is an old definition. We must combine the types. Do this first
     * because it might fail *)
    oldi.vtype <-
       combineTypes CombineOther
         oldfidx oldi.vtype fidx i.vtype;
    (* We get here if we have success *)
    (* Combine the attributes as well *)
    oldi.vattr <- addAttributes oldi.vattr i.vattr
    (* Do not union them yet because we do not know that they are the same.
     * We have checked only the types so far *)
  end

(************************************************************
 *
 *  PASS 2
 *
 *
 ************************************************************)


(** Keep track of the functions we have used already in the file. We need
  * this to avoid removing an inline function that has been used already.
  * This can only occur if the inline function is defined after it is used
  * already; a bad style anyway *)
let varUsedAlready: (string, unit) H.t = H.create 111

(** A visitor that renames uses of variables and types *)
class renameVisitorClass =
let rename_associated_logic_var lv =
    match lv.lv_origin with
        None -> DoChildren
      | Some vi ->
          if not vi.vglob then DoChildren
          else begin
            match findReplacement true vEq !currentFidx vi.vname with
                None -> DoChildren
              | Some (vi',_) ->
                  vi'.vreferenced <- true;
                  if vi == vi' then DoChildren (* replacement was done already*)
                  else begin
                    (match vi'.vlogic_var_assoc with
                        None ->
                          vi'.vlogic_var_assoc <- Some lv; DoChildren
                       | Some lv' -> ChangeTo lv')
                  end
          end
in
let find_enumitem_replacement ei =
  match findReplacement true eEq !currentFidx ei.eihost.ename with
      None -> None
    | Some (enum,_) ->
        if enum == intEnumInfo then begin
          (* Two different enums have been merged into an int type.
             Switch to an integer constant. *)
          match (constFold true ei.eival).enode with
            | Const c -> Some c
            | _ -> fatal "non constant value for an enum item"
        end else begin
          (* Merged with an isomorphic type. Find the appropriate enumitem *)
          let n = Extlib.find_index (fun e -> e.einame = ei.einame)
            ei.eihost.eitems in
          let ei' = List.nth enum.eitems n in
          assert (same_int64 ei.eival ei'.eival);
          Some (CEnum ei')
        end
in
object (self)
  inherit nopCilVisitor

      (* This is either a global variable which we took care of, or a local
       * variable. Must do its type and attributes. *)
  method vvdec (_vi: varinfo) = DoChildren

      (* This is a variable use. See if we must change it *)
  method vvrbl (vi: varinfo) : varinfo visitAction =
    if not vi.vglob then DoChildren else
    if vi.vreferenced then begin
      H.add varUsedAlready vi.vname ();
      DoChildren
    end else begin
      match findReplacement true vEq !currentFidx vi.vname with
        None -> DoChildren
      | Some (vi', oldfidx) ->
          if debugMerge then
              (Cilmsg.debug "Renaming use of var %s(%d) to %s(%d)"
                        vi.vname
                        !currentFidx vi'.vname oldfidx);
          vi'.vreferenced <- true;
          H.add varUsedAlready vi'.vname ();
          ChangeTo vi'
    end

  method vlogic_var_decl lv = rename_associated_logic_var lv

  method vlogic_var_use lv = rename_associated_logic_var lv

  method vlogic_info_use li =
    match findReplacement true lfEq !currentFidx li.l_var_info.lv_name with
        None -> if debugMerge then
          (Cilmsg.debug "Using logic function %s(%a)(%d)"
             li.l_var_info.lv_name
	     (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
	     (List.map (fun v -> v.lv_type) li.l_profile)
             !currentFidx);
          DoChildren
      | Some(li',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic function %s(%a)(%d) to %s(%a)(%d)"
                      li.l_var_info.lv_name
		      (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
		      (List.map (fun v -> v.lv_type) li.l_profile)
                      !currentFidx
		      li'.l_var_info.lv_name
		      (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
		      (List.map (fun v -> v.lv_type) li'.l_profile)
		      oldfidx);
          ChangeTo li'

  method vlogic_info_decl li =
    match findReplacement true lfEq !currentFidx li.l_var_info.lv_name with
        None ->
          if debugMerge then
            (Cilmsg.debug "Using logic function %s(%a)(%d)"
                      li.l_var_info.lv_name
		      (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
		      (List.map (fun v -> v.lv_type) li.l_profile)
                      !currentFidx);
          DoChildren
      | Some(li',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic function %s(%a)(%d) to %s(%a)(%d)"
                      li.l_var_info.lv_name
                      (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
		      (List.map (fun v -> v.lv_type) li.l_profile)
                      !currentFidx
		      li'.l_var_info.lv_name
		      (Pretty_utils.pp_list ~sep:",@ " d_logic_type)
		      (List.map (fun v -> v.lv_type) li'.l_profile)
                      oldfidx);
          ChangeTo li'

  method vlogic_type_info_use lt =
    match findReplacement true ltEq !currentFidx lt.lt_name with
        None -> if debugMerge then
            (Cilmsg.debug "Using logic type %s(%d)"
                      lt.lt_name
                      !currentFidx);
          DoChildren
      | Some(lt',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic type %s(%d) to %s(%d)"
                      lt.lt_name
                      !currentFidx lt'.lt_name oldfidx);
          ChangeTo lt'

  method vlogic_type_info_decl lt =
    match findReplacement true ltEq !currentFidx lt.lt_name with
        None ->
          if debugMerge then
            (Cilmsg.debug "Using logic type %s(%d)"
                      lt.lt_name
                      !currentFidx);
          DoChildren
      | Some(lt',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic function %s(%d) to %s(%d)"
                      lt.lt_name
                      !currentFidx lt'.lt_name oldfidx);
          ChangeTo lt'

  method vlogic_ctor_info_use lc =
    match findReplacement true lcEq !currentFidx lc.ctor_name with
        None -> if debugMerge then
            (Cilmsg.debug "Using logic constructor %s(%d)"
                      lc.ctor_name
                      !currentFidx);
          DoChildren
      | Some(lc',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic type %s(%d) to %s(%d)"
                      lc.ctor_name
                      !currentFidx lc'.ctor_name oldfidx);
          ChangeTo lc'

  method vlogic_ctor_info_decl lc =
    match findReplacement true lcEq !currentFidx lc.ctor_name with
        None ->
          if debugMerge then
            (Cilmsg.debug "Using logic constructor %s(%d)"
                      lc.ctor_name
                      !currentFidx);
          DoChildren
      | Some(lc',oldfidx) ->
          if debugMerge then
            (Cilmsg.debug "Renaming use of logic function %s(%d) to %s(%d)"
                      lc.ctor_name
                      !currentFidx lc'.ctor_name oldfidx);
          ChangeTo lc'

        (* The use of a type. Change only those types whose underlying info
         * is not a root. *)
  method vtype (t: typ) =
    match t with
      TComp (ci, _, a) when not ci.creferenced -> begin
        match findReplacement true sEq !currentFidx ci.cname with
          None -> DoChildren
        | Some (ci', oldfidx) ->
            if debugMerge then
              (Cilmsg.debug "Renaming use of %s(%d) to %s(%d)"
                        ci.cname !currentFidx ci'.cname oldfidx);
            ChangeTo (TComp (ci', empty_size_cache (), visitCilAttributes (self :> cilVisitor) a))
      end
    | TEnum (ei, a) when not ei.ereferenced -> begin
        match findReplacement true eEq !currentFidx ei.ename with
          None -> DoChildren
        | Some (ei', _) ->
            if ei' == intEnumInfo then
              (* This is actually our friend intEnumInfo *)
              ChangeTo (TInt(IInt, visitCilAttributes (self :> cilVisitor) a))
            else
              ChangeTo (TEnum (ei', visitCilAttributes (self :> cilVisitor) a))
      end

    | TNamed (ti, a) when not ti.treferenced -> begin
        match findReplacement true tEq !currentFidx ti.tname with
          None -> DoChildren
        | Some (ti', _) ->
            ChangeTo (TNamed (ti', visitCilAttributes (self :> cilVisitor) a))
    end

    | _ -> DoChildren

  method vexpr e =
    match e.enode with
      | Const (CEnum ei) ->
          (match find_enumitem_replacement ei with
               None -> DoChildren
             | Some c ->
                 ChangeTo { e with enode = Const c })
      | _ -> DoChildren

  method vterm e =
    match e.term_node with
      | TConst(CEnum ei) ->
          (match find_enumitem_replacement ei with
               None -> DoChildren
             | Some c ->
                 let t = visitCilLogicType (self:>cilVisitor) e.term_type in
                 ChangeTo
                   { e with
                       term_node = TConst c;
                       term_type = t
                   })
      | _ -> DoChildren

  (* The Field offset might need to be changed to use new compinfo *)
  method voffs = function
      Field (f, o) -> begin
        (* See if the compinfo was changed *)
        if f.fcomp.creferenced then
          DoChildren
        else begin
          match findReplacement true sEq !currentFidx f.fcomp.cname with
            None -> DoChildren (* We did not replace it *)
          | Some (ci', _oldfidx) -> begin
              (* First, find out the index of the original field *)
              let rec indexOf (i: int) = function
                  [] -> Cilmsg.fatal "Cannot find field %s in %s"
                    f.fname (compFullName f.fcomp)
                | f' :: _ when f' == f -> i
                | _ :: rest -> indexOf (i + 1) rest
              in
              let index = indexOf 0 f.fcomp.cfields in
              if List.length ci'.cfields <= index then
                 Cilmsg.fatal "Too few fields in replacement %s for %s"
                   (compFullName ci')
                   (compFullName f.fcomp);
              let f' = List.nth ci'.cfields index in
              ChangeDoChildrenPost (Field (f', o), fun x -> x)
          end
        end
      end
    | _ -> DoChildren

  method vterm_offset = function
      TField (f, o) -> begin
        (* See if the compinfo was changed *)
        if f.fcomp.creferenced then
          DoChildren
        else begin
          match findReplacement true sEq !currentFidx f.fcomp.cname with
            None -> DoChildren (* We did not replace it *)
          | Some (ci', _oldfidx) -> begin
              (* First, find out the index of the original field *)
              let rec indexOf (i: int) = function
                  [] -> Cilmsg.fatal "Cannot find field %s in %s"
                    f.fname (compFullName f.fcomp)
                | f' :: _ when f' == f -> i
                | _ :: rest -> indexOf (i + 1) rest
              in
              let index = indexOf 0 f.fcomp.cfields in
              if List.length ci'.cfields <= index then
                 Cilmsg.fatal "Too few fields in replacement %s for %s"
                   (compFullName ci')
                   (compFullName f.fcomp);
              let f' = List.nth ci'.cfields index in
              ChangeDoChildrenPost (TField (f', o), fun x -> x)
          end
        end
      end
    | _ -> DoChildren

  method vinitoffs o =
    (self#voffs o)      (* treat initializer offsets same as lvalue offsets *)

end

let renameVisitor = new renameVisitorClass


(** A visitor that renames uses of inline functions that were discovered in
 * pass 2 to be used before they are defined. This is like the renameVisitor
 * except it only looks at the variables (thus it is a bit more efficient)
 * and it also renames forward declarations of the inlines to be removed. *)

class renameInlineVisitorClass = object
  inherit nopCilVisitor

      (* This is a variable use. See if we must change it *)
  method vvrbl (vi: varinfo) : varinfo visitAction =
    if not vi.vglob then DoChildren else
    if vi.vreferenced then begin (* Already renamed *)
      DoChildren
    end else begin
      match findReplacement true vEq !currentFidx vi.vname with
        None -> DoChildren
      | Some (vi', oldfidx) ->
          if debugMerge then
              Cilmsg.debug "Renaming var %s(%d) to %s(%d)"
                vi.vname
                !currentFidx vi'.vname oldfidx;
          vi'.vreferenced <- true;
          ChangeTo vi'
    end

  (* And rename some declarations of inlines to remove. We cannot drop this
   * declaration (see small1/combineinline6) *)
  method vglob = function
      GVarDecl(spec,vi, l) when vi.vinline -> begin
        (* Get the original name *)
        let origname =
          try H.find originalVarNames vi.vname
          with Not_found -> vi.vname
        in
        (* Now see if this must be replaced *)
        match findReplacement true vEq !currentFidx origname with
          None -> DoChildren
        | Some (vi', _) ->
            (*TODO: visit the spec to change references to formals *)
            ChangeTo [GVarDecl (spec,vi', l)]
      end
    | _ -> DoChildren

end
let renameInlinesVisitor = new renameInlineVisitorClass

let collect_type_vars l =
  let tvars = ref [] in
  let vis = object
    inherit Cil.nopCilVisitor
    method vlogic_type t =
      match t with
          Lvar v when not (List.mem v !tvars) -> tvars:=v::!tvars; DoChildren
        | _ -> DoChildren
  end
  in
  List.iter (fun x -> ignore(visitCilLogicType vis x.lv_type)) l;
  !tvars

let rec logic_annot_pass2 ~in_axiomatic g a = match a with

| Dfun_or_pred (li,l) ->
    begin
      CurrentLoc.set l;
      match findReplacement true lfEq !currentFidx li.l_var_info.lv_name with
	| None ->
	    if not in_axiomatic then
              mergePushGlobals (visitCilGlobal renameVisitor g);
            Logic_utils.add_logic_function li;
	| Some _ -> ()
	    (* FIXME: should we perform same actions
	       as the case Dlogic_reads above ? *)
    end
| Dtype (t,l) ->
    begin
      CurrentLoc.set l;
      match findReplacement true ltEq !currentFidx t.lt_name with
      | None ->
	  if not in_axiomatic then
            mergePushGlobals (visitCilGlobal renameVisitor g);
          Logic_env.add_logic_type
            t.lt_name (H.find ltEq (!currentFidx,t.lt_name)).ndata
      | Some _ -> ()
    end
| Dinvariant ({l_var_info = {lv_name = n}},l) ->
    begin
      CurrentLoc.set l;
      match findReplacement true lfEq !currentFidx n with
      | None ->
	  assert (not in_axiomatic);
          mergePushGlobals (visitCilGlobal renameVisitor g);
          Logic_utils.add_logic_function (H.find lfEq (!currentFidx,n)).ndata
      | Some _ -> ()
    end
| Dtype_annot (n,l) ->
    begin
      CurrentLoc.set l;
      match findReplacement true lfEq !currentFidx n.l_var_info.lv_name with
      | None ->
          let g = visitCilGlobal renameVisitor g in
	  if not in_axiomatic then
            mergePushGlobals g;
          Logic_utils.add_logic_function
            (H.find lfEq (!currentFidx,n.l_var_info.lv_name)).ndata
      | Some _ -> ()
    end
| Dlemma (n,_,_,_,_,l) ->
    begin
      CurrentLoc.set l;
      match findReplacement true llEq !currentFidx n with
          None ->
            if not in_axiomatic then
              mergePushGlobals (visitCilGlobal renameVisitor g)
        | Some _ -> ()
    end
| Daxiomatic(n,l,loc) ->
    begin
      CurrentLoc.set loc;
      match findReplacement true laEq !currentFidx n with
          None ->
            assert (not in_axiomatic);
            mergePushGlobals (visitCilGlobal renameVisitor g);
            List.iter (logic_annot_pass2 ~in_axiomatic:true g) l
        | Some _ -> ()
    end

let global_annot_pass2 g a = logic_annot_pass2 ~in_axiomatic:false g a

(* sm: First attempt at a semantic checksum for function bodies.
 * Ideally, two function's checksums would be equal only when their
 * bodies were provably equivalent; but I'm using a much simpler and
 * less accurate heuristic here.  It should be good enough for the
 * purpose I have in mind, which is doing duplicate removal of
 * multiply-instantiated template functions. *)
let functionChecksum (dec: fundec) : int =
begin
  (* checksum the structure of the statements (only) *)
  let rec stmtListSum (lst : stmt list) : int =
    (List.fold_left (fun acc s -> acc + (stmtSum s)) 0 lst)
  and stmtSum (s: stmt) : int =
    (* strategy is to just throw a lot of prime numbers into the
     * computation in hopes of avoiding accidental collision.. *)
    match s.skind with
    | UnspecifiedSequence seq ->
        131*(stmtListSum (List.map (fun (x,_,_,_,_) -> x) seq)) + 127
    | Instr _ -> 13 + 67
    | Return(_) -> 17
    | Goto(_) -> 19
    | Break(_) -> 23
    | Continue(_) -> 29
    | If(_,b1,b2,_) -> 31 + 37*(stmtListSum b1.bstmts)
                          + 41*(stmtListSum b2.bstmts)
    | Switch(_,b,_,_) -> 43 + 47*(stmtListSum b.bstmts)
                            (* don't look at stmt list b/c is not part of tree *)
    | Loop(_,b,_,_,_) -> 49 + 53*(stmtListSum b.bstmts)
    | Block(b) -> 59 + 61*(stmtListSum b.bstmts)
    | TryExcept (b, (_, _), h, _) ->
        67 + 83*(stmtListSum b.bstmts) + 97*(stmtListSum h.bstmts)
    | TryFinally (b, h, _) ->
        103 + 113*(stmtListSum b.bstmts) + 119*(stmtListSum h.bstmts)
  in

  (* disabled 2nd and 3rd measure because they appear to get different
   * values, for the same code, depending on whether the code was just
   * parsed into CIL or had previously been parsed into CIL, printed
   * out, then re-parsed into CIL *)
  let a,b,c,d,e =
    (List.length dec.sformals),        (* # formals *)
    0 (*(List.length dec.slocals)*),         (* # locals *)
    0 (*dec.smaxid*),                        (* estimate of internal statement count *)
    (List.length dec.sbody.bstmts),    (* number of statements at outer level *)
    (stmtListSum dec.sbody.bstmts) in  (* checksum of statement structure *)
  2*a + 3*b + 5*c + 7*d + 11*e
end


(* sm: equality for initializers, etc.; this is like '=', except
 * when we reach shared pieces (like references into the type
 * structure), we use '==', to prevent circularity *)
(* update: that's no good; I'm using this to find things which
 * are equal but from different CIL trees, so nothing will ever
 * be '=='.. as a hack I'll just change those places to 'true',
 * so these functions are not now checking proper equality..
 * places where equality is not complete are marked "INC" *)
let rec equalInits (x: init) (y: init) : bool =
begin
  match x,y with
  | SingleInit(xe), SingleInit(ye) -> (equalExps xe ye)
  | CompoundInit(_xt, xoil), CompoundInit(_yt, yoil) ->
      (*(xt == yt) &&*)  (* INC *)       (* types need to be identically equal *)
      let rec equalLists xoil yoil : bool =
        match xoil,yoil with
        | ((xo,xi) :: xrest), ((yo,yi) :: yrest) ->
            (equalOffsets xo yo) &&
            (equalInits xi yi) &&
            (equalLists xrest yrest)
        | [], [] -> true
        | _, _ -> false
      in
      (equalLists xoil yoil)
  | _, _ -> false
end

and equalOffsets (x: offset) (y: offset) : bool =
begin
  match x,y with
  | NoOffset, NoOffset -> true
  | Field(xfi,xo), Field(yfi,yo) ->
      (xfi.fname = yfi.fname) &&     (* INC: same fieldinfo name.. *)
      (equalOffsets xo yo)
  | Index(xe,xo), Index(ye,yo) ->
      (equalExps xe ye) &&
      (equalOffsets xo yo)
  | _,_ -> false
end

and equalExps (x: exp) (y: exp) : bool =
begin
  match x.enode,y.enode with
  | Const(xc), Const(yc) ->        xc = yc   ||    (* safe to use '=' on literals *)
    (
      (* CIL changes (unsigned)0 into 0U during printing.. *)
      match xc,yc with
      | CInt64(xv,_,_),CInt64(yv,_,_) ->
          (Int64.to_int xv) = 0   &&     (* ok if they're both 0 *)
          (Int64.to_int yv) = 0
      | _,_ -> false
    )
  | Lval(xl), Lval(yl) ->          (equalLvals xl yl)
  | SizeOf(_xt), SizeOf(_yt) ->      true (*INC: xt == yt*)  (* identical types *)
  | SizeOfE(xe), SizeOfE(ye) ->    (equalExps xe ye)
  | AlignOf(_xt), AlignOf(_yt) ->    true (*INC: xt == yt*)
  | AlignOfE(xe), AlignOfE(ye) ->  (equalExps xe ye)
  | UnOp(xop,xe,_xt), UnOp(yop,ye,_yt) ->
      xop = yop &&
      (equalExps xe ye) &&
      true  (*INC: xt == yt*)
  | BinOp(xop,xe1,xe2,_xt), BinOp(yop,ye1,ye2,_yt) ->
      xop = yop &&
      (equalExps xe1 ye1) &&
      (equalExps xe2 ye2) &&
      true  (*INC: xt == yt*)
  | CastE(_xt,xe), CastE(_yt,ye) ->
      (*INC: xt == yt &&*)
      (equalExps xe ye)
  | AddrOf(xl), AddrOf(yl) ->      (equalLvals xl yl)
  | StartOf(xl), StartOf(yl) ->    (equalLvals xl yl)

  (* initializers that go through CIL multiple times sometimes lose casts they
   * had the first time; so allow a different of a cast *)
  | CastE(_xt,xe),_ ->
      (equalExps xe y)
  | _, CastE(_yt,ye) ->
      (equalExps x ye)

  | _,_ -> false
end

and equalLvals (x: lval) (y: lval) : bool =
begin
  match x,y with
  | (Var _xv,xo), (Var _yv,yo) ->
      (* I tried, I really did.. the problem is I see these names
       * before merging collapses them, so __T123 != __T456,
       * so whatever *)
      (*(xv.vname = vy.vname) &&      (* INC: same varinfo names.. *)*)
      (equalOffsets xo yo)

  | (Mem(xe),xo), (Mem(ye),yo) ->
      (equalExps xe ye) &&
      (equalOffsets xo yo)
  | _,_ -> false
end

let equalInitOpts (x: init option) (y: init option) : bool =
begin
  match x,y with
  | None,None -> true
  | Some(xi), Some(yi) -> (equalInits xi yi)
  | _,_ -> false
end


  (* Now we go once more through the file and we rename the globals that we
   * keep. We also scan the entire body and we replace references to the
   * representative types or variables. We set the referenced flags once we
   * have replaced the names. *)
let oneFilePass2 (f: file) =
  Cilmsg.feedback ~level:2 "Final merging phase: %s" f.fileName;
  currentDeclIdx := 0; (* Even though we don't need it anymore *)
  H.clear varUsedAlready;
  H.clear originalVarNames;
  (* If we find inline functions that are used before being defined, and thus
   * before knowing that we can throw them away, then we mark this flag so
   * that we can make another pass over the file *)
  let repeatPass2 = ref false in

  (* set to true if we need to make an additional path for changing tentative
     definition into plain declaration because a real definition has been found.
   *)
  let replaceTentativeDefn = ref false in

  (* Keep a pointer to the contents of the file so far *)
  let savedTheFile = !theFile in

  let processOneGlobal (g: global) : unit =
    (* Process a varinfo. Reuse an old one, or rename it if necessary *)
    let processVarinfo (vi: varinfo) (vloc: location) : varinfo =
      if vi.vreferenced then
        vi (* Already done *)
      else begin
        (* Maybe it is static. Rename it then *)
        if vi.vstorage = Static then begin
          let newName, _ =
	    A.newAlphaName vtAlpha None vi.vname (CurrentLoc.get ())
	  in
          let formals_decl =
            try ignore (Cil.getFormalsDecl vi); true
            with Not_found -> false
          in
	  (* Remember the original name *)
          H.add originalVarNames newName vi.vname;
          if debugMerge then Cilmsg.debug "renaming %s at %a to %s"
                                vi.vname
                                d_loc vloc
                                newName;
          vi.vname <- newName;
          vi.vreferenced <- true;
          Cil_const.set_vid vi;
          if formals_decl then Cil.setFormalsDecl vi vi.vtype;
	  vi
        end else begin
          (* Find the representative *)
          match findReplacement true vEq !currentFidx vi.vname with
            None -> vi (* This is the representative *)
          | Some (vi', _) -> (* Reuse some previous one *)
              vi'.vreferenced <- true; (* Mark it as done already *)
              vi'.vaddrof <- vi.vaddrof || vi'.vaddrof;
              vi'
        end
      end
    in
    try
      match g with
      | GVarDecl (spec,vi, l) as g ->
          CurrentLoc.set l;
          incr currentDeclIdx;
          let vi' = processVarinfo vi l in
          let spec' = visitCilFunspec renameVisitor spec in
          if vi != vi' then begin
            (* Drop the decl, keep the spec *)
            mergeSpec vi' vi spec' end
          else if H.mem emittedVarDecls vi'.vname then begin
            mergeSpec vi' vi spec'
          end else begin
            H.add emittedVarDecls vi'.vname true; (* Remember that we emitted
                                                   * it  *)
            mergePushGlobals (visitCilGlobal renameVisitor g)
          end

      | GVar (vi, init, l) ->
          CurrentLoc.set l;
          incr currentDeclIdx;
          let vi' = processVarinfo vi l in
          (* We must keep this definition even if we reuse this varinfo,
           * because maybe the previous one was a declaration *)
          H.add emittedVarDecls vi.vname true; (* Remember that we emitted it*)

          let emitIt:bool = (not mergeGlobals) ||
            try
              let _prevVar, prevInitOpt, prevLoc =
                (H.find emittedVarDefn vi'.vname) in
              (* previously defined; same initializer? *)
              if (equalInitOpts prevInitOpt init.init)
                || (init.init = None) then (
                  false  (* do not emit *)
		)
              else if prevInitOpt = None then (
                (* The previous occurence was only a tentative defn. Now,
                   we have a real one. Set the correct value in the table,
                   and tell that we need to change the previous into a GVarDecl
                 *)
                H.replace emittedVarDefn vi'.vname(vi',init.init,l);
                replaceTentativeDefn:=true;
                true
              )
              else (
                (* Both GVars have initializers. *)
                Cil.error "global var %s at %a has different initializer than %a"
                  vi'.vname
                  d_loc l  d_loc prevLoc;
                false
              )
            with Not_found -> (
              (* no previous definition *)
              (H.add emittedVarDefn vi'.vname (vi', init.init, l));
              true     (* emit it *)
            )
          in

          if emitIt then
            mergePushGlobals (visitCilGlobal renameVisitor (GVar(vi', init, l)))

      | GFun (fdec, l) as g ->
          CurrentLoc.set l;
          incr currentDeclIdx;
          (* We apply the renaming *)
          let vi = processVarinfo fdec.svar l in
          if fdec.svar != vi then begin
	    (try add_alpha_renaming vi (Cil.getFormalsDecl vi) fdec.sformals
	     with Not_found -> ());
            fdec.svar <- vi
          end;
          (* Get the original name. *)
          let origname =
            try H.find originalVarNames fdec.svar.vname
            with Not_found -> fdec.svar.vname
          in
          (* Go in there and rename everything as needed *)
          let fdec' =
	    match visitCilGlobal renameVisitor g with
            | [ GFun(fdec', _) ] -> fdec'
            | _ -> Cilmsg.fatal "renameVisitor for GFun returned something else"
          in
          let g' = GFun(fdec', l) in
          (* Now restore the parameter names *)
          let _, args, _, _ = splitFunctionTypeVI fdec'.svar in
          let oldnames, foundthem =
            try H.find formalNames (!currentFidx, origname), true
            with Not_found -> begin
              Cil.warnOpt "Cannot find %s in formalNames" origname ;
              [], false
            end
          in
          if foundthem then begin
            let _argl = argsToList args in
            if List.length oldnames <> List.length fdec.sformals then
              Cil.fatal "After merging the function has different arguments";
            List.iter2
              (fun oldn a -> if oldn <> "" then a.vname <- oldn)
              oldnames fdec.sformals;
            (* Reflect them in the type *)
            setFormals fdec fdec.sformals
          end;
          (** See if we can remove this inline function *)
          if fdec'.svar.vinline && mergeInlines then begin
            let printout =
              (* Temporarily turn of printing of lines *)
              let oldprintln = miscState.lineDirectiveStyle in
              miscState.lineDirectiveStyle <- None;
              (* Temporarily set the name to all functions in the same way *)
              let newname = fdec'.svar.vname in
              (* If we must do alpha conversion then temporarily set the
               * names of the function, local variables and formals in a
	       * standard way *)
              if mergeInlinesWithAlphaConvert then
		fdec'.svar.vname <- "@@alphaname@@";
              let nameId = ref 0 in
              let oldNames : string list ref = ref [] in
              let renameOne (v: varinfo) =
                oldNames := v.vname :: !oldNames;
                incr nameId;
                v.vname <- "___alpha" ^ string_of_int !nameId
              in
              let undoRenameOne (v: varinfo) =
                match !oldNames with
                  n :: rest ->
                    oldNames := rest;
                    v.vname <- n
                | _ ->  Cilmsg.fatal "undoRenameOne"
              in
              (* Remember the original type *)
              let origType = fdec'.svar.vtype in
              if mergeInlinesWithAlphaConvert then begin
                (* Rename the formals *)
                List.iter renameOne fdec'.sformals;
                (* Reflect in the type *)
                setFormals fdec' fdec'.sformals;
                (* Now do the locals *)
                List.iter renameOne fdec'.slocals
              end;
              (* Now print it *)
              let res = Pretty_utils.sfprintf "%a" d_global g' in
              miscState.lineDirectiveStyle <- oldprintln;
              fdec'.svar.vname <- newname;
              if mergeInlinesWithAlphaConvert then begin
                (* Do the locals in reverse order *)
                List.iter undoRenameOne (List.rev fdec'.slocals);
                (* Do the formals in reverse order *)
                List.iter undoRenameOne (List.rev fdec'.sformals);
                (* Restore the type *)
                fdec'.svar.vtype <- origType;
              end;
              res
            in
            (* Make a node for this inline function using the original
               name. *)
            let inode =
              getNode vEq vSyn !currentFidx origname fdec'.svar
                (Some (l, !currentDeclIdx))
            in
            if debugInlines then begin
              Cilmsg.debug "getNode %s(%d) with loc=%a. declidx=%d"
                inode.nname inode.nfidx
                d_nloc inode.nloc
                !currentDeclIdx;
              Cilmsg.debug
                "Looking for previous definition of inline %s(%d)"
                origname !currentFidx;
            end;
            try
              let oldinode = H.find inlineBodies printout in
              if debugInlines then
                Cilmsg.debug "  Matches %s(%d)"
                  oldinode.nname oldinode.nfidx;
              (* There is some other inline function with the same printout.
               * We should reuse this, but watch for the case when the inline
               * was already used. *)
              if H.mem varUsedAlready fdec'.svar.vname then begin
                if mergeInlinesRepeat then begin
                  repeatPass2 := true
                end else begin
                  Cil.warning
		    "Inline function %s because it is used before it is defined"
		    fdec'.svar.vname;
                  raise Not_found
                end
              end;
              let _ = union oldinode inode in
              (* Clean up the vreferenced bit in the new inline, so that we
               * can rename it. Reset the name to the original one so that
               * we can find the replacement name. *)
              fdec'.svar.vreferenced <- false;
              fdec'.svar.vname <- origname;
              () (* Drop this definition *)
            with Not_found -> begin
              if debugInlines then Cilmsg.debug " Not found";
              H.add inlineBodies printout inode;
              mergePushGlobal g'
            end
          end else begin
            (* either the function is not inline, or we're not attempting to
             * merge inlines *)
            if mergeGlobals
	      && not fdec'.svar.vinline
	      && fdec'.svar.vstorage <> Static
	    then begin
              (* sm: this is a non-inline, non-static function.  I want to
               * consider dropping it if a same-named function has already
               * been put into the merged file *)
              let curSum = (functionChecksum fdec') in
              try
                let _prevFun, prevLoc, prevSum =
                  (H.find emittedFunDefn fdec'.svar.vname) in
                (* previous was found *)
                if (curSum = prevSum) then
                  Cil.warning
                    "dropping duplicate def'n of func %s at %a in favor of that at %a"
                    fdec'.svar.vname
                    d_loc l  d_loc prevLoc
                else begin
                  (* the checksums differ, so print a warning but keep the
                   * older one to avoid a link error later.  I think this is
		   * a reasonable approximation of what ld does. *)
                  Cil.warning
		    "def'n of func %s at %a (sum %d) conflicts with the one at %a (sum %d); keeping the one at %a."
                    fdec'.svar.vname
                    d_loc l  curSum  d_loc prevLoc
		    prevSum d_loc prevLoc
                end
              with Not_found -> begin
                (* there was no previous definition *)
                (mergePushGlobal g');
                (H.add emittedFunDefn fdec'.svar.vname (fdec', l, curSum))
              end
            end else begin
              (* not attempting to merge global functions, or it was static
               * or inline *)
              mergePushGlobal g'
            end;
          end

      | GCompTag (ci, l) as g -> begin
          CurrentLoc.set l;
          incr currentDeclIdx;
          if ci.creferenced then
            ()
          else begin
            match findReplacement true sEq !currentFidx ci.cname with
              None ->
                (* A new one, we must rename it and keep the definition *)
                (* Make sure this is root *)
                (try
                   let nd = H.find sEq (!currentFidx, ci.cname) in
                   if nd.nrep != nd then
                     Cilmsg.fatal "Setting creferenced for struct %s which is \
                         not root!"
                       ci.cname;
                 with Not_found -> begin
                   Cilmsg.fatal "Setting creferenced for struct %s which is not \
                       in the sEq!"
                     ci.cname;
                 end);
                let newname, _ =
                  A.newAlphaName sAlpha None ci.cname (CurrentLoc.get ()) in
                ci.cname <- newname;
                ci.creferenced <- true;
                ci.ckey <- H.hash (compFullName ci);
                (* Now we should visit the fields as well *)
                H.add emittedCompDecls ci.cname true; (* Remember that we
                                                       * emitted it  *)
                mergePushGlobals (visitCilGlobal renameVisitor g)
            | Some (_oldci, _oldfidx) -> begin
                (* We are not the representative. Drop this declaration
                 * because we'll not be using it. *)
                ()
              end
          end
	end
      | GEnumTag (ei, l) as g -> begin
          CurrentLoc.set l;
          incr currentDeclIdx;
          if ei.ereferenced then
            ()
          else begin
            match findReplacement true eEq !currentFidx ei.ename with
              None -> (* We must rename it *)
                let newname, _ =
                  A.newAlphaName eAlpha None ei.ename (CurrentLoc.get ()) in
                ei.ename <- newname;
                ei.ereferenced <- true;
                (* And we must rename the items to using the same name space
                 * as the variables *)
                List.iter
                  (fun item ->
                     let newname,_= A.newAlphaName vtAlpha None item.einame
                       item.eiloc
                     in item.einame <- newname)
                  ei.eitems;
                mergePushGlobals (visitCilGlobal renameVisitor g);
            | Some (_ei', _) -> (* Drop this since we are reusing it from
                                 * before *)
                ()
          end
	end
      | GCompTagDecl (ci, l) -> begin
          CurrentLoc.set l; (* This is here just to introduce an undefined
                             * structure. But maybe the structure was defined
                             * already.  *)
          (* Do not increment currentDeclIdx because it is not incremented in
           * pass 1*)
          if H.mem emittedCompDecls ci.cname then
            () (* It was already declared *)
          else begin
            H.add emittedCompDecls ci.cname true;
            (* Keep it as a declaration *)
            mergePushGlobal g;
          end
	end

      | GEnumTagDecl (_ei, l) ->
          CurrentLoc.set l;
          (* Do not increment currentDeclIdx because it is not incremented in
           * pass 1*)
          (* Keep it as a declaration *)
          mergePushGlobal g


      | GType (ti, l) as g -> begin
          CurrentLoc.set l;
          incr currentDeclIdx;
          if ti.treferenced then
            ()
          else begin
            match findReplacement true tEq !currentFidx ti.tname with
              None -> (* We must rename it and keep it *)
                let newname, _ =
                  A.newAlphaName vtAlpha None ti.tname (CurrentLoc.get ()) in
                ti.tname <- newname;
                ti.treferenced <- true;
                mergePushGlobals (visitCilGlobal renameVisitor g);
            | Some (_ti', _) ->(* Drop this since we are reusing it from
				* before *)
                ()
          end
        end
      | GAnnot (a, l) as g ->
          CurrentLoc.set l;
          incr currentDeclIdx;
          global_annot_pass2 g a
      | g -> mergePushGlobals (visitCilGlobal renameVisitor g)
    with e -> begin
      Cilmsg.debug "error when merging global %a: %s" d_global g
	(Printexc.to_string e);
      mergePushGlobal (GText (Pretty_utils.sfprintf "/* error at %t:" d_thisloc));
      mergePushGlobal g;
      mergePushGlobal (GText ("*************** end of error*/"));
      raise e
    end
  in
  (* Now do the real PASS 2 *)
  List.iter processOneGlobal f.globals;
  (* Replace tentative definition by a declaration when we found a real
     definition somewhere else *)
  if !replaceTentativeDefn then begin
    (* Stay tail-recursive, the list of globals can be huge. *)
    theFile :=
      List.rev
        (List.rev_map
           (function
                GVar(vi,{init=None},loc) as g ->
                  (try let (_,real_init,_) = H.find emittedVarDefn vi.vname
                   in (match real_init with
                           None -> g
                         | Some _ -> GVarDecl(empty_funspec(),vi,loc))
                   with Not_found -> g)
              | g -> g)
           !theFile)
       end;
  (* See if we must re-visit the globals in this file because an inline that
   * is being removed was used before we saw the definition and we decided to
   * remove it *)
  if mergeInlinesRepeat && !repeatPass2 then begin
    Cilmsg.feedback "Repeat final merging phase: %s" f.fileName;
    (* We are going to rescan the globals we have added while processing this
     * file. *)
    let theseGlobals : global list ref = ref [] in
    (* Scan a list of globals until we hit a given tail *)
    let rec scanUntil (tail: 'a list) (l: 'a list) =
      if tail == l then ()
      else
        match l with
        | [] ->  Cilmsg.fatal "mergecil: scanUntil could not find the marker"
        | g :: rest ->
            theseGlobals := g :: !theseGlobals;
            scanUntil tail rest
    in
    (* Collect in theseGlobals all the globals from this file *)
    theseGlobals := [];
    scanUntil savedTheFile !theFile;
    (* Now reprocess them *)
    theFile := savedTheFile;
    List.iter (fun g ->
                 theFile := (visitCilGlobal renameInlinesVisitor g) @ !theFile)
      !theseGlobals;
    (* Now check if we have inlines that we could not remove
       H.iter (fun name _ ->
       if not (H.mem inlinesRemoved name) then
       ignore (warn "Could not remove inline %s. I have no idea why!\n"
       name))
       inlinesToRemove *)
  end


let merge_specs orig to_merge =
  let initial = { orig with spec_behavior = orig.spec_behavior } in
  let merge_one_spec spec =
    if is_same_spec initial spec then ()
    else Logic_utils.merge_funspec orig spec
  in
  List.iter merge_one_spec to_merge

let global_merge_spec g =
match g with
  | GFun(fdec,_) ->
      (try
         let specs =
           Hashtbl.find spec_to_merge fdec.svar.vid
         in
         merge_specs fdec.sspec specs
       with Not_found -> ())
  | GVarDecl(spec,v,_) ->
      let rename spec =
        try
          let alpha = Hashtbl.find formals_renaming v.vid in
          ignore (visitCilFunspec alpha spec)
        with Not_found -> ()
      in
      (try
         let specs =
           Hashtbl.find spec_to_merge v.vid
         in
         merge_specs spec specs;
         rename spec
       with Not_found -> rename spec
      )
  | _ -> ()

let merge (files: file list) (newname: string) : file =
  init ();

  Cilmsg.push_errors ();

  (* Make the first pass over the files *)
  currentFidx := 0;
  List.iter (fun f -> oneFilePass1 f; incr currentFidx) files;

  (* Now maybe try to force synonyms to be equal *)
  if mergeSynonyms then begin
    doMergeSynonyms sSyn sEq matchCompInfo;
    doMergeSynonyms eSyn eEq matchEnumInfo;
    doMergeSynonyms tSyn tEq matchTypeInfo;

    doMergeSynonyms lfSyn lfEq matchLogicInfo;
    doMergeSynonyms ltSyn ltEq matchLogicType;
    doMergeSynonyms lcSyn lcEq matchLogicCtor;
    doMergeSynonyms laSyn laEq matchLogicAxiomatic;
    doMergeSynonyms llSyn llEq matchLogicLemma;

    if mergeInlines then begin
      (* Copy all the nodes from the iEq to vEq as well. This is needed
       * because vEq will be used for variable renaming *)
      H.iter (fun k n -> H.add vEq k n) iEq;
      doMergeSynonyms iSyn iEq matchInlines;
    end
  end;

  (* Now maybe dump the graph *)
  if debugMerge then begin
    dumpGraph "type" tEq;
    dumpGraph "struct and union" sEq;
    dumpGraph "enum" eEq;
    dumpGraph "variable" vEq;
    if mergeInlines then dumpGraph "inline" iEq;
  end;
  (* Make the second pass over the files. This is when we start rewriting the
   * file *)
  currentFidx := 0;
  List.iter (fun f -> oneFilePass2 f; incr currentFidx) files;

  (* Now reverse the result and return the resulting file *)
  let rec revonto acc = function
      [] -> acc
    | x :: t ->
        revonto (x :: acc) t
  in
  let res =
    { fileName = newname;
      globals  = revonto (revonto [] !theFile) !theFileTypes;
      globinit = None;
      globinitcalled = false } in
  List.iter global_merge_spec res.globals;
  init ~all:false (); (* Make the GC happy BUT KEEP some tables *)
  (* We have made many renaming changes and sometimes we have just guessed a
   * name wrong. Make sure now that the local names are unique. *)
  uniqueVarNames res;
  let res =
    if Cilmsg.had_errors () then
      begin
        Cilmsg.error "Error during linking@." ;
        { fileName = newname;
	  globals = [];
	  globinit = None;
	  globinitcalled = false }
      end
    else
      res
  in
  Cilmsg.pop_errors ();
  res

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
