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

(* mergecil.ml *)
(* This module is responsible for merging multiple CIL source trees into
 * a single, coherent CIL tree which contains the union of all the
 * definitions in the source files.  It effectively acts like a linker,
 * but at the source code level instead of the object code level. *)

open Extlib
open Cil_types
open Cil
module H = Hashtbl
module A = Alpha

open Logic_utils

let dkey = Kernel.register_category "mergecil"

let debugInlines = false

(* Try to merge structure with the same name. However, do not complain if
 * they are not the same *)
let mergeSynonyms = true


(** Whether to use path compression *)
let usePathCompression = true

(* Try to merge definitions of inline functions. They can appear in multiple
 * files and we would like them all to be the same. This can slow down the
 * merger an order of magnitude !!! *)
let mergeInlines = true

let mergeInlinesRepeat = mergeInlines && true

(* The default value has been changed to false after Boron to fix bts#524.
   But this behavior is very convenient to parse the Linux kernel. *)
let mergeInlinesWithAlphaConvert () = 
  mergeInlines && Kernel.AgressiveMerging.get ()


(* when true, merge duplicate definitions of externally-visible functions;
 * this uses a mechanism which is faster than the one for inline functions,
 * but only probabilistically accurate *)
let mergeGlobals = true


(* Return true if 's' starts with the prefix 'p' *)
let prefix p s =
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p

let d_nloc fmt (lo: (location * int) option) =
  match lo with
    None -> Format.fprintf fmt "None"
  | Some (l, idx) -> 
    Format.fprintf fmt "Some(%d at %a)" idx Cil_printer.pp_location l

type ('a, 'b) node =
    { nname: 'a;   (* The actual name *)
      nfidx: int;      (* The file index *)
      ndata: 'b;       (* Data associated with the node *)
      mutable nloc: (location * int) option;
      (* location where defined and index within the file of the definition.
       * If None then it means that this node actually DOES NOT appear in the
       * given file. In rare occasions we need to talk in a given file about
       * types that are not defined in that file. This happens with undefined
       * structures but also due to cross-contamination of types in a few of
       * the cases of combineType (see the definition of combineTypes). We
       * try never to choose as representatives nodes without a definition.
       * We also choose as representative the one that appears earliest *)
      mutable nrep: ('a, 'b) node;  
      (* A pointer to another node in its class (one
       * closer to the representative). The nrep node
       * is always in an earlier file, except for the
       * case where a name is undefined in one file
       * and defined in a later file. If this pointer
       * points to the node itself then this is the
       * representative.  *)
      mutable nmergedSyns: bool (* Whether we have merged the synonyms for
                                 * the node of this name *)
    }

module Merging
  (H: 
    sig
      include Hashtbl.HashedType
      val merge_synonym: t -> bool (* whether this name should be considered
                                      for merging or not.
                                    *)
      val compare: t -> t -> int
      val output: Format.formatter -> t -> unit
    end
    ): 
sig
type 'a eq_table
type 'a syn_table
val create_eq_table: int -> 'a eq_table
val find_eq_table: 'a eq_table -> (int * H.t) -> (H.t, 'a) node
val add_eq_table: 'a eq_table -> (int * H.t) -> (H.t,'a) node -> unit
val iter_eq_table: 
  ((int * H.t) -> (H.t,'a) node -> unit) -> 'a eq_table -> unit
val clear_eq: 'a eq_table -> unit
val create_syn_table: int -> 'a syn_table
val clear_syn: 'a syn_table -> unit
val mkSelfNode:
  'a eq_table -> 'a syn_table -> int -> H.t -> 'a ->
  (location * int) option -> (H.t, 'a) node
val find: bool -> (H.t, 'a) node -> (H.t, 'a) node
val union: (H.t, 'a) node -> (H.t,'a) node -> (H.t, 'a) node * (unit -> unit) 
val findReplacement:
  bool -> 'a eq_table -> int -> H.t -> ('a * int) option
val getNode: 'a eq_table -> 'a syn_table -> int -> 
  H.t -> 'a -> (location * int) option -> (H.t, 'a) node
(* [doMergeSynonyms eq compare] 
   tries to merge synonyms. Do not give an error if they fail to merge
   compare is a comparison function that throws Failure if no match *)
val doMergeSynonyms: 'a syn_table -> (int -> 'a -> int -> 'a -> unit) -> unit
val dumpGraph: string -> 'a eq_table -> unit
end
=
struct
  module Elts =
    struct
      type t = int * H.t
      let hash (d,x) = 19 * d + H.hash x
      let equal (d1,x1) (d2,x2) = d1 = d2 && H.equal x1 x2
      let compare (d1,x1) (d2,x2) =
        let res = compare d1 d2 in
        if res = 0 then H.compare x1 x2 else res
    end

(* Find the representative for a node and compress the paths in the process *)
module Heq = Hashtbl.Make (Elts)

module Iter_sorted = FCMap.Make(Elts)

module Hsyn = Hashtbl.Make(H)

type 'a eq_table = (H.t,'a) node Heq.t
type 'a syn_table = (H.t,'a) node Hsyn.t

let create_eq_table x = Heq.create x
let create_syn_table x = Hsyn.create x

let clear_eq = Heq.clear
let clear_syn = Hsyn.clear

let find_eq_table = Heq.find

let add_eq_table = Heq.add

let iter_eq_table f t =
  let sorted = Heq.fold Iter_sorted.add t Iter_sorted.empty in
  Iter_sorted.iter f sorted

(* Make a node with a self loop. This is quite tricky. *)
let mkSelfNode eq syn fidx name data l =
  let rec res = { nname = name; nfidx = fidx; ndata = data; nloc = l;
                  nrep  = res; nmergedSyns = false; }
  in
  Heq.add eq (fidx, name) res; (* Add it to the proper table *)
  (* mergeSynonyms is not active for anonymous types, probably because it is
     licit to have two distinct anonymous types in two different files
     (which should not be merged). However, for anonymous enums, they
     can, and are, in fact merged by CIL. Hence, we permit the merging of
     anonymous enums with the same base name *)
  if mergeSynonyms && H.merge_synonym name
  then Hsyn.add syn name res;
  res

(* Find the representative with or without path compression *)
let rec find pathcomp nd =
  let dkey = Kernel.register_category "mergecil:find" in
  Kernel.debug ~dkey "find %a(%d)" H.output nd.nname nd.nfidx ;
  if nd.nrep == nd then begin
    Kernel.debug ~dkey "= %a(%d)" H.output nd.nname nd.nfidx ;
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
let union nd1 nd2 =
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
        else (* In the same file. Choose the one with the earliest index *)
          begin
            match nd1.nloc, nd2.nloc with
                Some (_, didx1), Some (_, didx2) ->
                  if didx1 < didx2 then nd1, nd2 else
                    if didx1 > didx2 then nd2, nd1
                    else begin
		      Kernel.warning
		        "Merging two elements (%a and %a) \
                         in the same file (%d) \
                         with the same idx (%d) within the file"
                        H.output nd1.nname H.output nd2.nname nd1.nfidx didx1 ;
                      nd1, nd2
                    end
              | _, _ ->
	    (* both none. Does not matter which one we choose. Should not happen
	       though. *)
            (* sm: it does happen quite a bit when, e.g. merging STLport with
               some client source; I'm disabling the warning since it supposedly
               is harmless anyway, so is useless noise *)
            (* sm: re-enabling on claim it now will probably not happen *)
                Kernel.warning ~current:true
	          "Merging two undefined elements in the same file: %a and %a"
	          H.output nd1.nname H.output nd2.nname ;
                nd1, nd2
          end
        else (* One is defined, the other is not. Choose the defined one *)
          if nd1.nloc != None then nd1, nd2 else nd2, nd1
    in
    let oldrep = norep.nrep in
    norep.nrep <- rep;
    rep, (fun () -> norep.nrep <- oldrep)
  end

let findReplacement pathcomp eq fidx name =
  let dkey = Kernel.register_category "mergecil:find" in
    Kernel.debug ~dkey "findReplacement for %a(%d)" H.output name fidx;
  try
    let nd = Heq.find eq (fidx, name) in
    if nd.nrep == nd then begin
      Kernel.debug ~dkey "is a representative";
      None (* No replacement if this is the representative of its class *)
    end else
      let rep = find pathcomp nd in
      if rep != rep.nrep then
        Kernel.abort "find does not return the representative" ;
      Kernel.debug ~dkey "RES = %a(%d)" H.output rep.nname rep.nfidx;
      Some (rep.ndata, rep.nfidx)
  with Not_found -> begin
    Kernel.debug ~dkey "not found in the map";
    None
  end

(* Make a node if one does not already exist. Otherwise return the
 * representative *)
let getNode eq syn fidx name data l =
  let dkey = Kernel.register_category "mergecil:getNode" in
  let level = 2 in
  Kernel.debug ~dkey ~level "getNode(%a(%d), %a)" H.output name fidx d_nloc l;
  try
    let res = Heq.find eq (fidx, name) in

    (match res.nloc, l with
      (* Maybe we have a better location now *)
      None, Some _ -> res.nloc <- l
    | Some (old_l, old_idx), Some (l, idx) ->
        if old_idx != idx  then
	  Kernel.warning ~current:true
	    "Duplicate definition of node %a(%d) at indices %d(%a) and %d(%a)"
            H.output name fidx old_idx 
	    Cil_printer.pp_location old_l idx 
	    Cil_printer.pp_location l
    | _, _ -> ());
    Kernel.debug ~dkey ~level "  node already found";
    find false res (* No path compression *)
  with Not_found -> begin
    let res = mkSelfNode eq syn fidx name data l in
    Kernel.debug ~dkey ~level "   made a new one";
    res
  end

let doMergeSynonyms syn compare =
  Hsyn.iter 
    (fun n node ->
      if not node.nmergedSyns then begin
      (* find all the nodes for the same name *)
        let all = Hsyn.find_all syn n in
        (* classes are a list of representative for the nd name.
           We'll select an appropriate one according to the comparison 
           function. *)
        let tryone classes nd =
          nd.nmergedSyns <- true;
        (* Compare in turn with all the classes we have so far *)
          let rec compareWithClasses = function
            | [] -> [nd] (* No more classes. Add this as a new class *)
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

(* Dump a graph. No need to use ~dkey, this function is never called unless
   we are in proper debug mode. *)
let dumpGraph what eq : unit =
  Kernel.debug "Equivalence graph for %s is:" what;
  iter_eq_table
    (fun (fidx, name) nd ->
      Kernel.debug "  %a(%d) %s-> "
        H.output name fidx (if nd.nloc = None then "(undef)" else "");
      if nd.nrep == nd then
        Kernel.debug "*"
      else
        Kernel.debug " %a(%d)" H.output nd.nrep.nname nd.nrep.nfidx
    ) eq

end

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

let aeAlpha = H.create 57 (* Anonymous enums. *)

(* The original mergecil uses plain old Hashtbl for everything. *)
module PlainMerging = 
  Merging
    (struct
      type t = string
      let hash = Hashtbl.hash
      let equal = (=)
      let compare = compare
      let merge_synonym name = not (prefix "__anon" name)
      let output = Format.pp_print_string
     end)

module VolatileMerging =
  Merging
    (struct
        type t = identified_term list
        let hash = function
	  | [] -> 0
          | h::_ -> Logic_utils.hash_term h.it_content
        let equal = Logic_utils.is_same_list Logic_utils.is_same_identified_term
        let compare =
          Extlib.list_compare 
            (fun t1 t2 -> Logic_utils.compare_term t1.it_content t2.it_content)

        let merge_synonym _ = true
        let output fmt x = 
	  Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_identified_term fmt x
     end)

let hash_type t =
  let rec aux acc depth = function
    | TVoid _ -> acc
    | TInt (ikind,_) -> 3 * acc + Hashtbl.hash ikind
    | TFloat (fkind,_) -> 5 * acc + Hashtbl.hash fkind
    | TPtr(t,_) when depth < 5 -> aux (7*acc) (depth+1) t
    | TPtr _ -> 7 * acc
    | TArray (t,_,_,_) when depth < 5 -> aux (9*acc) (depth+1) t
    | TArray _ -> 9 * acc
    | TFun (r,_,_,_) when depth < 5 -> aux (11*acc) (depth+1) r
    | TFun _ -> 11 * acc
    | TNamed (t,_) -> 13 * acc + Hashtbl.hash t.tname
    | TComp(c,_,_) ->
      let mul = if c.cstruct then 17 else 19 in
      mul * acc + Hashtbl.hash c.cname
    | TEnum (e,_) -> 23 * acc + Hashtbl.hash e.ename
    | TBuiltin_va_list _ -> 29 * acc
  in
  aux 117 0 t

module ModelMerging =
  Merging
    (struct
      type t = string * typ
      let hash (s,t) =
        Datatype.String.hash s + 3 * hash_type t
      let equal (s1,t1 : t) (s2,t2) =
        s1 = s2 && Cil_datatype.TypByName.equal t1 t2
      let compare (s1,t1) (s2, t2) =
        let res = String.compare s1 s2 in
        if res = 0 then Cil_datatype.TypByName.compare t1 t2 else res
      let merge_synonym _ = true
      let output fmt (s,t) =
        Format.fprintf fmt "model@ %a@ { %s }" Cil_printer.pp_typ t s
     end)

let same_int64 e1 e2 =
  match (constFold true e1).enode, (constFold true e2).enode with
    | Const(CInt64(i, _, _)), Const(CInt64(i', _, _)) -> 
      Integer.equal i i'
    | _ -> false

let compare_int e1 e2 =
  match (constFold true e1), (constFold true e2) with
  | {enode = Const(CInt64(i, _, _))}, {enode = Const(CInt64(i', _, _))} -> 
      Integer.compare i i'
  | e1,e2 -> Cil_datatype.Exp.compare e1 e2
    (* not strictly accurate, but should do the trick anyway *)

let have_same_enum_items oldei ei =
  if List.length oldei.eitems <> List.length ei.eitems then
    raise (Failure "different number of enumeration elements");
  (* We check that they are defined in the same way. This is a fairly
   * conservative check. *)
  List.iter2
    (fun old_item item ->
      if old_item.einame <> item.einame then
        raise (Failure 
                 "different names for enumeration items");
      if not (same_int64 old_item.eival item.eival) then
        raise (Failure "different values for enumeration items"))
    oldei.eitems ei.eitems

let compare_enum_item e1 e2 =
  let res = String.compare e1.einame e2.einame in
  if res = 0 then compare_int e1.eival e2.eival else res

let same_enum_items oldei ei =
  try have_same_enum_items oldei ei; true
  with Failure _ -> false

let is_anonymous_enum e = prefix "__anonenum" e.ename

module EnumMerging =
  Merging
    (struct
      type t = enuminfo
      let hash s = Datatype.String.hash s.ename
      let equal e1 e2 =
        (is_anonymous_enum e1 && is_anonymous_enum e2 &&
           (same_enum_items e1 e2 || 
              (e1.ename = e2.ename &&
                  (e2.ename <- 
                    fst 
                    (A.newAlphaName
                       aeAlpha e2.ename Cil_datatype.Location.unknown);
                   false))
           ))
        || e1.ename = e2.ename
      let compare e1 e2 =
        if is_anonymous_enum e1 then
          if is_anonymous_enum e2 then
            Extlib.list_compare compare_enum_item e1.eitems e2.eitems
          else -1
        else if is_anonymous_enum e2 then 1
        else String.compare e1.ename e2.ename
      let merge_synonym _ = true
      let output fmt e =
        Cil_printer.pp_global fmt (GEnumTag (e, Cil_datatype.Location.unknown))
     end)

open PlainMerging

(* For each name space we define a set of equivalence classes *)
let vEq = PlainMerging.create_eq_table 111 (* Vars *)
let sEq = PlainMerging.create_eq_table 111 (* Struct + union *)
let eEq = EnumMerging.create_eq_table 111 (* Enums *)
let tEq = PlainMerging.create_eq_table 111 (* Type names*)
let iEq = PlainMerging.create_eq_table 111 (* Inlines *)

let lfEq = PlainMerging.create_eq_table 111 (* Logic functions *)
let ltEq = PlainMerging.create_eq_table 111 (* Logic types *)
let lcEq = PlainMerging.create_eq_table 111 (* Logic constructors *)

let laEq = PlainMerging.create_eq_table 111 (* Axiomatics *)
let llEq = PlainMerging.create_eq_table 111 (* Lemmas *)
let lcusEq = PlainMerging.create_eq_table 111 (* Custom *)

let lvEq = VolatileMerging.create_eq_table 111
let mfEq = ModelMerging.create_eq_table 111

(* Sometimes we want to merge synonyms. We keep some tables indexed by names.
 * Each name is mapped to multiple exntries *)
let vSyn = PlainMerging.create_syn_table 111
let iSyn = PlainMerging.create_syn_table 111
let sSyn = PlainMerging.create_syn_table 111
let eSyn = EnumMerging.create_syn_table 111
let tSyn = PlainMerging.create_syn_table 111
let lfSyn = PlainMerging.create_syn_table 111
let ltSyn = PlainMerging.create_syn_table 111
let lcSyn = PlainMerging.create_syn_table 111
let laSyn = PlainMerging.create_syn_table 111
let llSyn = PlainMerging.create_syn_table 111
let lcusSyn = PlainMerging.create_syn_table 111
let lvSyn = VolatileMerging.create_syn_table 111
let mfSyn = ModelMerging.create_syn_table 111

(** A global environment for variables. Put in here only the non-static
  * variables, indexed by their name.  *)
let vEnv : (string, (string, varinfo) node) H.t = H.create 111

(* A set of inline functions indexed by their printout ! *)
let inlineBodies : (string, (string, varinfo) node) H.t = H.create 111

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
let spec_to_merge = Cil_datatype.Varinfo.Hashtbl.create 59;;

(* renaming to be performed in spec found in declarations when there is
   a definition for the given function. Similar to spec_to_merge table.
 *)
let formals_renaming = Cil_datatype.Varinfo.Hashtbl.create 59;;

(* add 'g' to the merged file *)
let mergePushGlobal (g: global) : unit =
  pushGlobal g ~types:theFileTypes ~variables:theFile

let mergePushGlobals gl = List.iter mergePushGlobal gl

let add_to_merge_spec vi spec =
  let l =
    try Cil_datatype.Varinfo.Hashtbl.find spec_to_merge vi
    with Not_found -> []
  in Cil_datatype.Varinfo.Hashtbl.replace spec_to_merge vi (spec::l)

let add_alpha_renaming old_vi old_args new_args =
  try
    Cil_datatype.Varinfo.Hashtbl.add formals_renaming old_vi
      (Cil.create_alpha_renaming old_args new_args)
  with Invalid_argument _ ->
    (* [old_args] and [new_args] haven't the same length.
       May occur at least when trying to merge incompatible declarations. *)
    ()

let mergeSpec vi_ref vi_disc spec =
  if not (Cil.is_empty_funspec spec) then begin
    let spec =
      try
        let my_vars = Cil.getFormalsDecl vi_disc in
        let to_rename = Cil.getFormalsDecl vi_ref in
        Kernel.debug ~dkey "Renaming arguments: %a -> %a"
          (Pretty_utils.pp_list ~sep:",@ " Cil_datatype.Varinfo.pretty)
          my_vars
          (Pretty_utils.pp_list ~sep:",@ " Cil_datatype.Varinfo.pretty)
          to_rename;
	let alpha = Cil.create_alpha_renaming my_vars to_rename in
        Kernel.debug ~dkey
          "Renaming spec of function %a" Cil_datatype.Varinfo.pretty vi_disc;
        Kernel.debug  ~dkey
          "original spec is %a" Cil_printer.pp_funspec spec;
	try
          let res = Cil.visitCilFunspec alpha spec in
          Kernel.debug ~dkey "renamed spec is %a" Cil_printer.pp_funspec spec;
          res
        with Not_found -> assert false
      with Not_found -> spec
    in
    let spec =
      try 
        let alpha = Cil_datatype.Varinfo.Hashtbl.find formals_renaming vi_ref in
        let res = Cil.visitCilFunspec alpha spec in
        Kernel.debug ~dkey "renamed spec with definition's formals is %a"
          Cil_printer.pp_funspec res;
        res
      with Not_found -> spec
    in
    add_to_merge_spec vi_ref spec
end (* else no need to keep empty specs *)

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

  if all then PlainMerging.clear_eq vEq;

  PlainMerging.clear_eq sEq;
  EnumMerging.clear_eq eEq;
  PlainMerging.clear_eq tEq;
  PlainMerging.clear_eq iEq;

  PlainMerging.clear_eq lfEq;
  PlainMerging.clear_eq ltEq;
  PlainMerging.clear_eq lcEq;
  PlainMerging.clear_eq laEq;
  PlainMerging.clear_eq llEq;
  VolatileMerging.clear_eq lvEq;
  ModelMerging.clear_eq mfEq;

  PlainMerging.clear_syn vSyn;
  PlainMerging.clear_syn sSyn;
  EnumMerging.clear_syn eSyn;
  PlainMerging.clear_syn tSyn;
  PlainMerging.clear_syn iSyn;

  PlainMerging.clear_syn lfSyn;
  PlainMerging.clear_syn ltSyn;
  PlainMerging.clear_syn lcSyn;
  PlainMerging.clear_syn laSyn;
  PlainMerging.clear_syn llSyn;
  VolatileMerging.clear_syn lvSyn;
  ModelMerging.clear_syn mfSyn;

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
  | Dvolatile(id,rvi,wvi,loc) ->
    CurrentLoc.set loc;
    ignore (VolatileMerging.getNode 
              lvEq lvSyn !currentFidx id (id,(rvi,wvi,loc))
              (Some (loc,!currentDeclIdx)))
  | Daxiomatic(id,decls,l) ->
    CurrentLoc.set l;
    ignore (PlainMerging.getNode laEq laSyn !currentFidx id (id,decls)
              (Some (l,!currentDeclIdx)));
     List.iter global_annot_pass1 decls
  | Dfun_or_pred (li,l) ->
    CurrentLoc.set l;
    let mynode = 
      PlainMerging.getNode 
        lfEq lfSyn !currentFidx li.l_var_info.lv_name li None 
    in
    (* NB: in case of mix decl/def it is the decl location that is taken. *)
    if mynode.nloc = None then
      ignore 
        (PlainMerging.getNode lfEq lfSyn !currentFidx li.l_var_info.lv_name li
           (Some (l, !currentDeclIdx)))
  | Dtype_annot (pi,l) ->
    CurrentLoc.set l;
    ignore (PlainMerging.getNode 
              lfEq lfSyn !currentFidx pi.l_var_info.lv_name pi
              (Some (l, !currentDeclIdx)))
  | Dmodel_annot (mfi,l) -> 
    CurrentLoc.set l;
    ignore (ModelMerging.getNode 
              mfEq mfSyn !currentFidx (mfi.mi_name,mfi.mi_base_type) mfi
              (Some (l, !currentDeclIdx)))
  | Dcustom_annot (c, n, l) -> 
    Format.eprintf "Mergecil : custom@.";
    CurrentLoc.set l;
    ignore (PlainMerging.getNode
              lcusEq lcusSyn !currentFidx n (n,(c,l))
              (Some (l, !currentDeclIdx)))
  | Dinvariant (pi,l)  ->
    CurrentLoc.set l;
    ignore (PlainMerging.getNode 
              lfEq lfSyn !currentFidx pi.l_var_info.lv_name pi
              (Some (l, !currentDeclIdx)))
  | Dtype (info,l) ->
    CurrentLoc.set l;
    ignore (PlainMerging.getNode ltEq ltSyn !currentFidx info.lt_name info
              (Some (l, !currentDeclIdx)))

  | Dlemma (n,is_ax,labs,typs,st,l) ->
    CurrentLoc.set l;
    ignore (PlainMerging.getNode
              llEq llSyn !currentFidx n (n,(is_ax,labs,typs,st,l))
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
    ekind = IInt;
  }
(* And add it to the equivalence graph *)
let intEnumInfoNode =
  EnumMerging.getNode eEq eSyn 0 intEnumInfo intEnumInfo
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
		"different integer types %a and %a"
		Cil_printer.pp_typ oldt Cil_printer.pp_typ t
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
          raise (Failure "different floating point types")
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
          raise (Failure "different array sizes")
    in
    TArray (combbt, combinesz, empty_size_cache (), addAttributes olda a)

  | TPtr (oldbt, olda), TPtr (bt, a) ->
    TPtr (combineTypes CombineOther oldfidx oldbt fidx bt,
          addAttributes olda a)

  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
    let newrt =
      combineTypes
        (if what = CombineFundef then CombineFunret else CombineOther)
        oldfidx oldrt fidx rt
    in
    if oldva != va then
      raise (Failure "different vararg specifiers");
      (* If one does not have arguments, believe the one with the
       * arguments *)
    let newargs =
      if oldargs = None then args else
        if args = None then oldargs else
          let oldargslist = argsToList oldargs in
          let argslist = argsToList args in
          if List.length oldargslist <> List.length argslist then
            raise (Failure "different number of arguments")
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
    let olda =
      if Cil.hasAttribute "missingproto" a then olda
      else Cil.dropAttribute "missingproto" olda
    in
    let a =
      if Cil.hasAttribute "missingproto" olda then a
      else Cil.dropAttribute "missingproto" a
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
      (* raise (Failure "different type constructors") *)
    let msg:string =
      Pretty_utils.sfprintf
        "different type constructors: %a vs. %a"
        Cil_printer.pp_typ oldt Cil_printer.pp_typ t
    in
    raise (Failure msg))


(* Match two compinfos and throw a Failure if they do not match *)
and matchCompInfo (oldfidx: int) (oldci: compinfo)
    (fidx: int)    (ci: compinfo) : unit =
  let cstruct = oldci.cstruct in
  if cstruct <> ci.cstruct then
    raise (Failure "different struct/union types");
  (* See if we have a mapping already *)
  (* Make the nodes if not already made. Actually return the
   * representatives *)
  let oldcinode = 
    PlainMerging.getNode sEq sSyn oldfidx oldci.cname oldci None 
  in
  let cinode = PlainMerging.getNode sEq sSyn fidx ci.cname ci None in
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
      let aggregate_name = if cstruct then "struct" else "union" in
      let msg = Printf.sprintf
	"different number of fields in %s %s and %s %s: %d != %d."
	aggregate_name oldci.cname aggregate_name ci.cname 
	old_len len 
      in
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
	      raise (Failure "different bitfield info");
            if not (Cil_datatype.Attributes.equal oldf.fattr f.fattr) then
	      raise (Failure "different field attributes");
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
        let fields_old = 
          Pretty_utils.sfprintf "%a"
	    Cil_printer.pp_global
	    (GCompTag(oldci, Cil_datatype.Location.unknown)) 
	in
	let fields =
          Pretty_utils.sfprintf "%a"
            Cil_printer.pp_global (GCompTag(ci, Cil_datatype.Location.unknown))
	in
	let fullname_old = compFullName oldci in 
	let fullname = compFullName ci in
        let msg =
	  match fullname_old = fullname,
	    fields_old = fields (* Could also use a special comparison *)
	  with
	    true, true ->
	      Pretty_utils.sfprintf
		"Definitions of %s are not isomorphic. Reason follows:@\n@?%s"
		fullname_old reason
	  | false, true ->
	      Pretty_utils.sfprintf
		"%s and %s are not isomorphic. Reason follows:@\n@?%s"
		fullname_old fullname reason
	  | true, false ->
	      Pretty_utils.sfprintf
		"Definitions of %s are not isomorphic. \
                 Reason follows:@\n@?%s@\n@?%s@?%s"
		fullname_old reason
		fields_old fields
	  | false, false ->
	      Pretty_utils.sfprintf
		"%s and %s are not isomorphic. Reason follows:@\n@?%s@\n@?%s@?%s"
		fullname_old fullname reason
		fields_old fields


        in
        raise (Failure msg)
    end else begin
      (* We will reuse the old one. One of them is empty. If the old one is
       * empty, copy over the fields from the new one. Won't this result in
       * all sorts of undefined types??? *)
      if old_len = 0 then oldci.cfields <- ci.cfields;
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
  let oldeinode = EnumMerging.getNode eEq eSyn oldfidx oldei oldei None 
  in
  let einode = EnumMerging.getNode eEq eSyn fidx ei ei None in
  if oldeinode == einode then (* We already know they are the same *)
    ()
  else begin
    (* Replace with the representative data *)
    let oldei = oldeinode.ndata in
    let ei = einode.ndata in
    (* Try to match them. But if you cannot just make them both integers *)
    try
      have_same_enum_items oldei ei;
      (* Set the representative *)
      let newrep, _ = EnumMerging.union oldeinode einode in
      (* We get here if the enumerations match *)
      newrep.ndata.eattr <- addAttributes oldei.eattr ei.eattr;
      ()
    with Failure msg -> begin
      let pp_items = Pretty_utils.pp_list ~pre:"{" ~suf:"}" ~sep:",@ "
        (fun fmt item -> 
	  Format.fprintf fmt "%s=%a" item.eiorig_name 
	    Cil_printer.pp_exp item.eival)
      in
      if oldeinode != intEnumInfoNode && einode != intEnumInfoNode then
        Kernel.warning
          "@[merging definitions of enum %s using int type@ (%s);@ items %a and@ %a@]" 
          oldei.ename msg
          pp_items oldei.eitems pp_items ei.eitems;
      (* Get here if you cannot merge two enumeration nodes *)
      if oldeinode != intEnumInfoNode then begin
        let _ = EnumMerging.union oldeinode intEnumInfoNode in ()
      end;
      if einode != intEnumInfoNode then begin
        let _ = EnumMerging.union einode intEnumInfoNode in ()
      end;
    end
  end


(* Match two typeinfos and throw a Failure if they do not match *)
and matchTypeInfo (oldfidx: int) (oldti: typeinfo)
    (fidx: int)      (ti: typeinfo) : unit =
  if oldti.tname = "" || ti.tname = "" then
    Kernel.fatal "matchTypeInfo for anonymous type";
  (* Find the node for this enum, no path compression. *)
  let oldtnode = PlainMerging.getNode tEq tSyn oldfidx oldti.tname oldti None in
  let tnode = PlainMerging.getNode tEq tSyn    fidx ti.tname    ti None in
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
	 let oldname = oldti.tname in
	 let name = ti.tname in
	 if oldname = name
	 then
           Format.sprintf
             "Definitions of type %s are not isomorphic. \
              Reason follows:@\n@?%s"
             oldname reason
	 else
	   Format.sprintf
             "Types %s and %s are not isomorphic. Reason follows:@\n@?%s"
             oldname name reason
       in
       raise (Failure msg)
     end);
    let _ = union oldtnode tnode in
    ()
  end

let static_var_visitor = object
    inherit Cil.nopCilVisitor
    method! vvrbl vi = if vi.vstorage = Static then raise Exit; DoChildren
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
  let oldtnode = 
    PlainMerging.getNode lfEq lfSyn oldfidx oldpi.l_var_info.lv_name oldpi None
  in
  let tnode = 
    PlainMerging.getNode lfEq lfSyn fidx pi.l_var_info.lv_name pi None 
  in
  if oldtnode == tnode then (* We already know they are the same *)
    ()
  else begin
    let oldpi = oldtnode.ndata in
    let oldfidx = oldtnode.nfidx in
    let pi = tnode.ndata in
    let fidx = tnode.nfidx in
    if Logic_utils.is_same_logic_info oldpi pi then begin
      if has_static_ref_logic_function oldpi then
        Kernel.abort
          "multiple inclusion of logic function %s referring to a static variable"
          oldpi.l_var_info.lv_name
      else  if oldfidx < fidx then
        tnode.nrep <- oldtnode.nrep
      else
        oldtnode.nrep <- tnode.nrep
    end else
      Kernel.abort "invalid multiple logic function declarations %s" pi.l_var_info.lv_name
  end

let matchLogicType oldfidx oldnode fidx node =
  let oldtnode =
    PlainMerging.getNode ltEq ltSyn oldfidx oldnode.lt_name oldnode None 
  in
  let tnode = PlainMerging.getNode ltEq ltSyn fidx oldnode.lt_name node None in
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
      Kernel.error ~current:true
	"invalid multiple logic type declarations %s" node.lt_name
  end

let matchLogicCtor oldfidx oldpi fidx pi =
  let oldtnode = 
    PlainMerging.getNode lcEq lcSyn oldfidx oldpi.ctor_name oldpi None 
  in
  let tnode = PlainMerging.getNode lcEq lcSyn fidx pi.ctor_name pi None in
  if oldtnode != tnode then
    Kernel.error ~current:true
      "invalid multiple logic constructors declarations %s" pi.ctor_name

let matchLogicAxiomatic oldfidx (oldid,_ as oldnode) fidx (id,_ as node) =
  let oldanode = PlainMerging.getNode laEq laSyn oldfidx oldid oldnode None in
  let anode = PlainMerging.getNode laEq laSyn fidx id node None in
  if oldanode != anode then begin
    let _, oldax = oldanode.ndata in
    let oldaidx = oldanode.nfidx in
    let _, ax = anode.ndata in
    let aidx = anode.nfidx in
    if Logic_utils.is_same_axiomatic oldax ax then begin
      if oldaidx < aidx then
        anode.nrep <- oldanode.nrep
      else
        oldanode.nrep <- anode.nrep
    end else
      Kernel.error ~current:true
	"invalid multiple axiomatic declarations %s" id
  end

let matchLogicLemma oldfidx (oldid, _ as oldnode) fidx (id, _ as node) =
  let oldlnode = PlainMerging.getNode llEq llSyn oldfidx oldid oldnode None in
  let lnode = PlainMerging.getNode llEq llSyn fidx id node None in
  if oldlnode != lnode then begin
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
      Kernel.error ~current:true
	"invalid multiple lemmas or axioms  declarations for %s" id
  end

let matchVolatileClause oldfidx (oldid,_ as oldnode) fidx (id,_ as node) =
  let oldlnode = 
    VolatileMerging.getNode lvEq lvSyn oldfidx oldid oldnode None 
  in
  let lnode = VolatileMerging.getNode lvEq lvSyn fidx id node None in
  if oldlnode != lnode then begin
    let (oldid,(oldr,oldw,oldloc)) = oldlnode.ndata in
    let oldfidx = oldlnode.nfidx in
    let (id,(r,w,loc)) = lnode.ndata in
    let fidx = lnode.nfidx in
    if Logic_utils.is_same_global_annotation
      (Dvolatile (oldid,oldr,oldw,oldloc)) (Dvolatile (id,r,w,loc))
    then begin
      if oldfidx < fidx then
        lnode.nrep <- oldlnode.nrep
      else
        oldlnode.nrep <- lnode.nrep
    end else
      Kernel.error ~current:true
        "invalid multiple volatile clauses for locations %a"
        (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_identified_term) id
  end

let matchModelField 
    oldfidx ({ mi_name = oldname; mi_base_type = oldtyp } as oldnode) 
    fidx ({mi_name = name; mi_base_type = typ } as node)
    =
  let oldlnode =
    ModelMerging.getNode mfEq mfSyn oldfidx (oldname,oldtyp) oldnode None
  in
  let lnode = ModelMerging.getNode mfEq mfSyn fidx (name,typ) node None in
  if oldlnode != lnode then begin
    let oldmf = oldlnode.ndata in
    let oldfidx = oldlnode.nfidx in
    let mf = lnode.ndata in
    let fidx = oldlnode.nfidx in
    if Logic_utils.is_same_type oldmf.mi_field_type mf.mi_field_type then
      begin
        if oldfidx < fidx then
          lnode.nrep <- oldlnode.nrep
        else
          oldlnode.nrep <- lnode.nrep
      end
    else
      Kernel.error ~current:true 
        "Model field %s of type %a is declared with different logic type: \
         %a and %a"
        mf.mi_name Cil_printer.pp_typ mf.mi_base_type
        Cil_printer.pp_logic_type mf.mi_field_type 
	Cil_printer.pp_logic_type oldmf.mi_field_type
  end

(* Scan all files and do two things *)
(* 1. Initialize the alpha renaming tables with the names of the globals so
 * that when we come in the second pass to generate new names, we do not run
 * into conflicts.  *)
(* 2. For all declarations of globals unify their types. In the process
 * construct a set of equivalence classes on type names, structure and
 * enumeration tags  *)
(* 3. We clean the referenced flags *)

let oneFilePass1 (f:file) : unit =
  H.add fileNames !currentFidx f.fileName;
  Kernel.feedback ~level:2 "Pre-merging (%d) %s" !currentFidx f.fileName ;
  currentDeclIdx := 0;
  if f.globinitcalled || f.globinit <> None then
    Kernel.warning ~current:true
      "Merging file %s has global initializer" f.fileName;

  (* We scan each file and we look at all global varinfo. We see if globals
   * with the same name have been encountered before and we merge those types
   * *)
  let matchVarinfo (vi: varinfo) (loc, _ as l) =
    ignore (Alpha.registerAlphaName vtAlpha vi.vname (CurrentLoc.get ()));
    (* Make a node for it and put it in vEq *)
    let vinode = 
      PlainMerging.mkSelfNode vEq vSyn !currentFidx vi.vname vi (Some l) 
    in
    try
      let oldvinode = PlainMerging.find true (H.find vEnv vi.vname) in
      let oldloc, _ =
        match oldvinode.nloc with
          None ->  (Kernel.fatal "old variable is undefined")
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
          Kernel.abort 
            "@[<hov>Incompatible declaration for %s:@ %s@\n\
             First declaration was at  %a@\n\
             Current declaration is at %a"
            vi.vname reason
            Cil_printer.pp_location oldloc
            Cil_printer.pp_location loc
        end
      in
      let newrep, _ = union oldvinode vinode in
      (* We do not want to turn non-"const" globals into "const" one. That
       * can happen if one file declares the variable a non-const while
       * others declare it as "const". *)
      if hasAttribute "const" (typeAttrs vi.vtype) !=
        hasAttribute "const" (typeAttrs oldvi.vtype) then begin
          Cil.update_var_type
            newrep.ndata (typeRemoveAttributes ["const"] newtype);
        end else Cil.update_var_type newrep.ndata newtype;
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
          Kernel.warning ~current:true
	    "Inconsistent storage specification for %s. \
Now is %a and previous was %a at %a"
            vi.vname
            Cil_printer.pp_storage vi.vstorage 
	    Cil_printer.pp_storage oldvi.vstorage
            Cil_printer.pp_location oldloc ;
	  vi.vstorage
        end
      in
      newrep.ndata.vstorage <- newstorage;
      newrep.ndata.vattr <- addAttributes oldvi.vattr vi.vattr
    with Not_found ->
      (* Not present in the previous files. Remember it for later  *)
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
               ignore (PlainMerging.getNode iEq iSyn !currentFidx
                         fdec.svar.vname fdec.svar (Some (l, !currentDeclIdx)))
           end
             (* Make nodes for the defined type and structure tags *)
       | GType (t, l) ->
           incr currentDeclIdx;
           t.treferenced <- false;
           if t.tname <> "" then (* The empty names are just for introducing
                                  * undefined comp tags *)
             ignore (PlainMerging.getNode tEq tSyn !currentFidx t.tname t
                       (Some (l, !currentDeclIdx)))
           else begin (* Go inside and clean the referenced flag for the
                       * declared tags *)
             match t.ttype with
               TComp (ci, _, _ ) ->
                 ci.creferenced <- false;
                 (* Create a node for it *)
                 ignore 
                   (PlainMerging.getNode sEq sSyn !currentFidx ci.cname ci None)

             | TEnum (ei, _) ->
                 ei.ereferenced <- false;
                 ignore 
                   (EnumMerging.getNode eEq eSyn !currentFidx ei ei None)

             | _ ->  (Kernel.fatal "Anonymous Gtype is not TComp")
           end

       | GCompTag (ci, l) ->
           incr currentDeclIdx;
           ci.creferenced <- false;
           ignore (PlainMerging.getNode sEq sSyn !currentFidx ci.cname ci
                     (Some (l, !currentDeclIdx)))
       | GCompTagDecl (ci,_) -> ci.creferenced <- false
       | GEnumTagDecl (ei,_) -> ei.ereferenced <- false
       | GEnumTag (ei, l) ->
           incr currentDeclIdx;
           ignore (A.newAlphaName aeAlpha ei.ename l);
           ei.ereferenced <- false;
           ignore 
             (EnumMerging.getNode eEq eSyn !currentFidx ei ei
                (Some (l, !currentDeclIdx)))
       | GAnnot (gannot,l) ->
           CurrentLoc.set l;
           incr currentDeclIdx;
           global_annot_pass1 gannot
       | GText _ | GPragma _ | GAsm _ -> ())
    f.globals

let matchInlines (oldfidx: int) (oldi: varinfo)
                 (fidx: int) (i: varinfo) =
  let oldinode = PlainMerging.getNode iEq iSyn oldfidx oldi.vname oldi None in
  let    inode = PlainMerging.getNode iEq iSyn    fidx    i.vname    i None in
  if oldinode != inode then begin
    (* Replace with the representative data *)
    let oldi = oldinode.ndata in
    let oldfidx = oldinode.nfidx in
    let i = inode.ndata in
    let fidx = inode.nfidx in
    (* There is an old definition. We must combine the types. Do this first
     * because it might fail *)
    Cil.update_var_type
      oldi (combineTypes CombineOther oldfidx oldi.vtype fidx i.vtype);
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

let pp_profiles fmt li =
  Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_logic_type
    fmt
    (List.map (fun v -> v.lv_type) li.l_profile)

(** A visitor that renames uses of variables and types *)
class renameVisitorClass =
let rename_associated_logic_var lv =
    match lv.lv_origin with
        None ->
          (match PlainMerging.findReplacement true lfEq !currentFidx lv.lv_name
           with
             | None -> DoChildren
             | Some (li,_) ->
               let lv' = li.l_var_info in
               if lv == lv' then DoChildren (* Replacement already done... *)
               else ChangeTo lv')
      | Some vi ->
          if not vi.vglob then DoChildren
          else begin
            match PlainMerging.findReplacement true vEq !currentFidx vi.vname 
            with
              | None -> DoChildren
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
  match EnumMerging.findReplacement true eEq !currentFidx ei.eihost with
      None -> None
    | Some (enum,_) ->
        if enum == intEnumInfo then begin
          (* Two different enums have been merged into an int type.
             Switch to an integer constant. *)
          match (constFold true ei.eival).enode with
          | Const c -> Some c
          | _ ->
	    Kernel.fatal ~current:true "non constant value for an enum item"
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
  method! vvdec (_vi: varinfo) = DoChildren

      (* This is a variable use. See if we must change it *)
  method! vvrbl (vi: varinfo) : varinfo visitAction =
    if not vi.vglob then DoChildren else
    if vi.vreferenced then begin
      H.add varUsedAlready vi.vname ();
      DoChildren
    end else begin
      match PlainMerging.findReplacement true vEq !currentFidx vi.vname with
        None -> DoChildren
      | Some (vi', oldfidx) ->
          Kernel.debug ~dkey "Renaming use of var %s(%d) to %s(%d)"
            vi.vname !currentFidx vi'.vname oldfidx;
          vi'.vreferenced <- true;
          H.add varUsedAlready vi'.vname ();
          ChangeTo vi'
    end

  method! vlogic_var_decl lv = rename_associated_logic_var lv

  method! vlogic_var_use lv = rename_associated_logic_var lv

  method! vlogic_info_use li =
    match 
      PlainMerging.findReplacement true lfEq !currentFidx li.l_var_info.lv_name
    with
        None ->
          Kernel.debug ~level:2 ~dkey "Using logic function %s(%a)(%d)"
             li.l_var_info.lv_name
	     (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_logic_type)
	     (List.map (fun v -> v.lv_type) li.l_profile)
             !currentFidx;
          DoChildren
      | Some(li',oldfidx) ->
          Kernel.debug ~dkey 
            "Renaming use of logic function %s(%a)(%d) to %s(%a)(%d)"
            li.l_var_info.lv_name pp_profiles li !currentFidx 
            li'.l_var_info.lv_name pp_profiles li' oldfidx;
        ChangeTo li'

  method! vlogic_info_decl li =
    match 
      PlainMerging.findReplacement 
        true lfEq !currentFidx li.l_var_info.lv_name 
    with
        None ->
          Kernel.debug ~level:2 ~dkey "Using logic function %s(%a)(%d)"
            li.l_var_info.lv_name pp_profiles li !currentFidx;
          DoChildren
      | Some(li',oldfidx) ->
          Kernel.debug ~level:2 ~dkey 
            "Renaming use of logic function %s(%a)(%d) to %s(%a)(%d)"
            li.l_var_info.lv_name pp_profiles li !currentFidx
            li'.l_var_info.lv_name pp_profiles li' oldfidx;
          ChangeTo li'

  method! vlogic_type_info_use lt =
    match PlainMerging.findReplacement true ltEq !currentFidx lt.lt_name with
        None ->
          Kernel.debug ~level:2 ~dkey
            "Using logic type %s(%d)" lt.lt_name !currentFidx;
          DoChildren
      | Some(lt',oldfidx) ->
        Kernel.debug ~dkey "Renaming use of logic type %s(%d) to %s(%d)"
          lt.lt_name !currentFidx lt'.lt_name oldfidx;
        ChangeTo lt'

  method! vlogic_type_info_decl lt =
    match PlainMerging.findReplacement true ltEq !currentFidx lt.lt_name with
      | None ->
          Kernel.debug ~level:2 ~dkey
            "Using logic type %s(%d)" lt.lt_name !currentFidx;
          DoChildren
      | Some(lt',oldfidx) ->
          Kernel.debug ~dkey "Renaming use of logic function %s(%d) to %s(%d)"
            lt.lt_name !currentFidx lt'.lt_name oldfidx;
          ChangeTo lt'

  method! vlogic_ctor_info_use lc =
    match PlainMerging.findReplacement true lcEq !currentFidx lc.ctor_name with
        None ->
          Kernel.debug ~level:2 ~dkey "Using logic constructor %s(%d)"
            lc.ctor_name !currentFidx;
          DoChildren
      | Some(lc',oldfidx) ->
          Kernel.debug ~dkey "Renaming use of logic type %s(%d) to %s(%d)"
            lc.ctor_name !currentFidx lc'.ctor_name oldfidx;
          ChangeTo lc'

  method! vlogic_ctor_info_decl lc =
    match PlainMerging.findReplacement true lcEq !currentFidx lc.ctor_name with
        None ->
          Kernel.debug ~dkey ~level:2 "Using logic constructor %s(%d)"
            lc.ctor_name !currentFidx;
          DoChildren
      | Some(lc',oldfidx) ->
          Kernel.debug ~dkey ~level:2
            "Renaming use of logic function %s(%d) to %s(%d)"
            lc.ctor_name !currentFidx lc'.ctor_name oldfidx;
          ChangeTo lc'

        (* The use of a type. Change only those types whose underlying info
         * is not a root. *)
  method! vtype (t: typ) =
    match t with
      TComp (ci, _, a) when not ci.creferenced -> begin
        match PlainMerging.findReplacement true sEq !currentFidx ci.cname with
          None ->
             Kernel.debug ~dkey "No renaming needed %s(%d)"
               ci.cname !currentFidx;
            DoChildren
        | Some (ci', oldfidx) ->
            Kernel.debug ~dkey "Renaming use of %s(%d) to %s(%d)"
              ci.cname !currentFidx ci'.cname oldfidx;
            ChangeTo (TComp (ci',
                             empty_size_cache (),
                             visitCilAttributes (self :> cilVisitor) a))
      end
      | TComp(ci,_,_) -> 
        Kernel.debug ~dkey "%s(%d) referenced. No change" ci.cname !currentFidx;
        DoChildren
    | TEnum (ei, a) when not ei.ereferenced -> begin
        match EnumMerging.findReplacement true eEq !currentFidx ei with
          None -> DoChildren
        | Some (ei', _) ->
            if ei' == intEnumInfo then
              (* This is actually our friend intEnumInfo *)
              ChangeTo (TInt(IInt, visitCilAttributes (self :> cilVisitor) a))
            else
              ChangeTo (TEnum (ei', visitCilAttributes (self :> cilVisitor) a))
      end

    | TNamed (ti, a) when not ti.treferenced -> begin
        match PlainMerging.findReplacement true tEq !currentFidx ti.tname with
          None -> DoChildren
        | Some (ti', _) ->
            ChangeTo (TNamed (ti', visitCilAttributes (self :> cilVisitor) a))
    end

    | _ -> DoChildren

  method! vexpr e =
    match e.enode with
      | Const (CEnum ei) ->
          (match find_enumitem_replacement ei with
               None -> DoChildren
             | Some c ->
                 ChangeTo { e with enode = Const c })
      | CastE _ -> 
        (* Maybe the cast is no longer necessary if an enum has been replaced
           by an integer type. *)
        let post_action e = match e.enode with
        | CastE(typ,exp) when
            Cil_datatype.TypByName.equal (typeOf exp) typ ->
          exp
        | _ -> e
        in
        ChangeDoChildrenPost (e,post_action)
      | _ -> DoChildren

  method! vterm e =
    match e.term_node with
      | TConst(LEnum ei) ->
          (match find_enumitem_replacement ei with
               None -> DoChildren
             | Some c ->
                 let t = visitCilLogicType (self:>cilVisitor) e.term_type in
                 ChangeTo
                   { e with
                       term_node = TConst (Logic_utils.constant_to_lconstant c);
                       term_type = t
                   })
      | _ -> DoChildren

  (* The Field offset might need to be changed to use new compinfo *)
  method! voffs = function
      Field (f, o) -> begin
        (* See if the compinfo was changed *)
        if f.fcomp.creferenced then
          DoChildren
        else begin
          match 
            PlainMerging.findReplacement true sEq !currentFidx f.fcomp.cname 
          with
            None -> DoChildren (* We did not replace it *)
          | Some (ci', _oldfidx) -> begin
              (* First, find out the index of the original field *)
              let rec indexOf (i: int) = function
                  [] -> Kernel.fatal "Cannot find field %s in %s"
                    f.fname (compFullName f.fcomp)
                | f' :: _ when f' == f -> i
                | _ :: rest -> indexOf (i + 1) rest
              in
              let index = indexOf 0 f.fcomp.cfields in
              if List.length ci'.cfields <= index then
                 Kernel.fatal "Too few fields in replacement %s for %s"
                   (compFullName ci')
                   (compFullName f.fcomp);
              let f' = List.nth ci'.cfields index in
              ChangeDoChildrenPost (Field (f', o), fun x -> x)
          end
        end
      end
    | _ -> DoChildren

  method! vterm_offset = function
      TField (f, o) -> begin
        (* See if the compinfo was changed *)
        if f.fcomp.creferenced then
          DoChildren
        else begin
          match 
            PlainMerging.findReplacement true sEq !currentFidx f.fcomp.cname 
          with
            None -> DoChildren (* We did not replace it *)
          | Some (ci', _oldfidx) -> begin
              (* First, find out the index of the original field *)
              let rec indexOf (i: int) = function
                  [] -> Kernel.fatal "Cannot find field %s in %s"
                    f.fname (compFullName f.fcomp)
                | f' :: _ when f' == f -> i
                | _ :: rest -> indexOf (i + 1) rest
              in
              let index = indexOf 0 f.fcomp.cfields in
              if List.length ci'.cfields <= index then
                 Kernel.fatal "Too few fields in replacement %s for %s"
                   (compFullName ci')
                   (compFullName f.fcomp);
              let f' = List.nth ci'.cfields index in
              ChangeDoChildrenPost (TField (f', o), fun x -> x)
          end
        end
      end
    | TModel(f,o) ->
        (match 
            ModelMerging.findReplacement
              true mfEq !currentFidx (f.mi_name, f.mi_base_type)
         with
           | None -> 
               (* We might have changed the field before choosing it as
                  representative. Check that. *)
               let f' =
                 (ModelMerging.find_eq_table
                    mfEq (!currentFidx,(f.mi_name, f.mi_base_type))).ndata
               in 
               if f == f' then DoChildren (* already the representative. *)
               else ChangeDoChildrenPost (TModel(f',o),fun x -> x)
           | Some (f',_) ->
               ChangeDoChildrenPost (TModel(f',o), fun x -> x))

    | _ -> DoChildren

  method! vinitoffs o =
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
  method! vvrbl (vi: varinfo) : varinfo visitAction =
    if not vi.vglob then DoChildren else
    if vi.vreferenced then begin (* Already renamed *)
      DoChildren
    end else begin
      match PlainMerging.findReplacement true vEq !currentFidx vi.vname with
        None -> DoChildren
      | Some (vi', oldfidx) ->
          Kernel.debug ~dkey "Renaming var %s(%d) to %s(%d)"
            vi.vname !currentFidx vi'.vname oldfidx;
          vi'.vreferenced <- true;
          ChangeTo vi'
    end

  (* And rename some declarations of inlines to remove. We cannot drop this
   * declaration (see small1/combineinline6) *)
  method! vglob = function
      GVarDecl(spec,vi, l) when vi.vinline -> begin
        (* Get the original name *)
        let origname =
          try H.find originalVarNames vi.vname
          with Not_found -> vi.vname
        in
        (* Now see if this must be replaced *)
        match PlainMerging.findReplacement true vEq !currentFidx origname with
          None -> DoChildren
        | Some (vi', _) ->
            (*TODO: visit the spec to change references to formals *)
            ChangeTo [GVarDecl (spec,vi', l)]
      end
    | _ -> DoChildren

end
let renameInlinesVisitor = new renameInlineVisitorClass

let rec logic_annot_pass2 ~in_axiomatic g a = 
  match a with
    | Dfun_or_pred (li,l) ->
      begin
        CurrentLoc.set l;
        match 
          PlainMerging.findReplacement 
            true lfEq !currentFidx li.l_var_info.lv_name 
        with
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
        match PlainMerging.findReplacement true ltEq !currentFidx t.lt_name with
          | None ->
	    if not in_axiomatic then
              mergePushGlobals (visitCilGlobal renameVisitor g);
            Logic_env.add_logic_type
              t.lt_name 
              (PlainMerging.find_eq_table ltEq (!currentFidx,t.lt_name)).ndata
          | Some _ -> ()
      end
    | Dinvariant ({l_var_info = {lv_name = n}},l) ->
      begin
        CurrentLoc.set l;
        match PlainMerging.findReplacement true lfEq !currentFidx n with
          | None ->
	    assert (not in_axiomatic);
            mergePushGlobals (visitCilGlobal renameVisitor g);
            Logic_utils.add_logic_function 
              (PlainMerging.find_eq_table lfEq (!currentFidx,n)).ndata
          | Some _ -> ()
      end
    | Dtype_annot (n,l) ->
      begin
        CurrentLoc.set l;
        match 
          PlainMerging.findReplacement 
            true lfEq !currentFidx n.l_var_info.lv_name 
        with
          | None ->
            let g = visitCilGlobal renameVisitor g in
	    if not in_axiomatic then
              mergePushGlobals g;
            Logic_utils.add_logic_function
              (PlainMerging.find_eq_table 
                 lfEq (!currentFidx,n.l_var_info.lv_name)).ndata
          | Some _ -> ()
      end
    | Dmodel_annot (mf,l) -> 
      begin
        CurrentLoc.set l;
        match 
          ModelMerging.findReplacement 
            true mfEq !currentFidx (mf.mi_name,mf.mi_base_type)
        with
          | None ->
              let mf' = visitCilModelInfo renameVisitor mf in
              if mf' != mf then begin
                let my_node =
                  ModelMerging.find_eq_table 
                    mfEq (!currentFidx,(mf'.mi_name,mf'.mi_base_type))
                in
                (* Adds a new representative. Do not replace directly
                   my_node, as there might be some pointers to it from
                   other files.
                *)
                let my_node' = { my_node with ndata = mf' } in
                my_node.nrep <- my_node'; (* my_node' represents my_node *)
                my_node'.nrep <- my_node';
                  (* my_node' is the canonical representative. *)
                ModelMerging.add_eq_table
                  mfEq 
                  (!currentFidx,(mf'.mi_name,mf'.mi_base_type))
                  my_node';
              end;
	      if not in_axiomatic then
                mergePushGlobals [GAnnot (Dmodel_annot(mf',l),l)];
              Logic_env.add_model_field
                (ModelMerging.find_eq_table
                   mfEq (!currentFidx,(mf'.mi_name,mf'.mi_base_type))).ndata;
          | Some _ -> ()
      end
    | Dcustom_annot (_c, n, l) -> 
      begin
        CurrentLoc.set l;
        match 
          PlainMerging.findReplacement 
            true lcusEq !currentFidx n
        with
          | None ->
            let g = visitCilGlobal renameVisitor g in
	    if not in_axiomatic then
              mergePushGlobals g
          | Some _ -> ()
      end
    | Dlemma (n,_,_,_,_,l) ->
      begin
        CurrentLoc.set l;
        match PlainMerging.findReplacement true llEq !currentFidx n with
            None ->
              if not in_axiomatic then
                mergePushGlobals (visitCilGlobal renameVisitor g)
          | Some _ -> ()
      end
    | Dvolatile(vi,_,_,loc) ->
      (CurrentLoc.set loc;
       match VolatileMerging.findReplacement true lvEq !currentFidx vi with
           None -> mergePushGlobals (visitCilGlobal renameVisitor g)
         | Some _ -> ())
    | Daxiomatic(n,l,loc) ->
      begin
        CurrentLoc.set loc;
        match PlainMerging.findReplacement true laEq !currentFidx n with
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
  | Const(xc), Const(yc) ->
      Cil.compareConstant xc yc  ||
    ((* CIL changes (unsigned)0 into 0U during printing.. *)
      match xc,yc with
      | CInt64(xv,_,_),CInt64(yv,_,_) ->
          (Integer.equal xv Integer.zero) 
        && (* ok if they're both 0 *)
            (Integer.equal yv Integer.zero) 
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
  Kernel.feedback ~level:2 "Final merging phase: %s" f.fileName;
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
	    A.newAlphaName vtAlpha vi.vname (CurrentLoc.get ())
	  in
          let formals_decl =
            try Some (Cil.getFormalsDecl vi)
            with Not_found -> None
          in
	  (* Remember the original name *)
          H.add originalVarNames newName vi.vname;
          Kernel.debug ~dkey "renaming %s at %a to %s"
            vi.vname Cil_printer.pp_location vloc newName;
          vi.vname <- newName;
          vi.vreferenced <- true;
          Cil_const.set_vid vi;
          (match formals_decl with
            | Some formals -> Cil.unsafeSetFormalsDecl vi formals
            | None -> ());
	  vi
        end else begin
          (* Find the representative *)
          match PlainMerging.findReplacement true vEq !currentFidx vi.vname with
            None -> vi (* This is the representative *)
          | Some (vi', _) -> (* Reuse some previous one *)
              vi'.vreferenced <- true; (* Mark it as done already *)
              vi'.vaddrof <- vi.vaddrof || vi'.vaddrof;
              vi'.vdefined <- vi.vdefined || vi'.vdefined;
              if Extlib.xor vi'.vghost vi.vghost then
                Kernel.abort
                  "Cannot merge: Global %a has both ghost and non-ghost status"
                  Cil_printer.pp_varinfo vi';
              (* If vi has a logic binding, add one to
                 the representative if needed. *)
              (match vi'.vlogic_var_assoc, vi.vlogic_var_assoc with
                | _, None -> ()
                | Some _, _ -> ()
                | None, Some _ -> ignore (Cil.cvar_to_lvar vi'));
              vi'
        end
      end
    in
    match g with
      | GVarDecl (spec,vi, l) as g ->
          CurrentLoc.set l;
          incr currentDeclIdx;
          let vi' = processVarinfo vi l in
          let spec' = visitCilFunspec renameVisitor spec in
          if vi != vi' then begin
            (* Drop the decl, keep the spec *)
            mergeSpec vi' vi spec';
            (try
              (* if the reference varinfo already has formals, everything
                 is renamed accordingly. *)
               ignore (Cil.getFormalsDecl vi')
             with Not_found ->
              (* Otherwise, if we have formals here, register them with
                 the reference varinfo *)
               try
                 let my_formals = Cil.getFormalsDecl vi in
                 Cil.unsafeSetFormalsDecl vi' my_formals
               with Not_found -> ()
               (* Neither decl has formals. Do nothing. *));
            Cil.removeFormalsDecl vi
          end
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
                Kernel.error ~current:true
		  "global var %s at %a has different initializer than %a"
                  vi'.vname
                  Cil_printer.pp_location l Cil_printer.pp_location prevLoc;
                false
              )
            with Not_found -> begin
              (* no previous definition *)
              H.add emittedVarDefn vi'.vname (vi', init.init, l);
              true (* emit it *)
            end
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
            | _ -> Kernel.fatal "renameVisitor for GFun returned something else"
          in
          let g' = GFun(fdec', l) in
          (* Now restore the parameter names *)
          let _, args, _, _ = splitFunctionTypeVI fdec'.svar in
          let oldnames, foundthem =
            try H.find formalNames (!currentFidx, origname), true
            with Not_found -> begin
              [], false
            end
          in
          if foundthem then begin
            let _argl = argsToList args in
            if List.length oldnames <> List.length fdec.sformals then
              Kernel.fatal ~current:true
		"After merging the function has different arguments";
            List.iter2
              (fun oldn a -> if oldn <> "" then a.vname <- oldn)
              oldnames fdec.sformals;
            (* Reflect them in the type *)
            setFormals fdec fdec.sformals
          end;
          (** See if we can remove this inline function *)
          if fdec'.svar.vinline && mergeInlines then begin
	    let mergeInlinesWithAlphaConvert = mergeInlinesWithAlphaConvert () 
	    in
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
                | _ ->  Kernel.fatal "undoRenameOne"
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
              let res = Pretty_utils.sfprintf "%a" Cil_printer.pp_global g' in
              miscState.lineDirectiveStyle <- oldprintln;
              fdec'.svar.vname <- newname;
              if mergeInlinesWithAlphaConvert then begin
                (* Do the locals in reverse order *)
                List.iter undoRenameOne (List.rev fdec'.slocals);
                (* Do the formals in reverse order *)
                List.iter undoRenameOne (List.rev fdec'.sformals);
                (* Restore the type *)
                Cil.update_var_type fdec'.svar origType;
              end;
              res
            in
            (* Make a node for this inline function using the original
               name. *)
            let inode =
              PlainMerging.getNode vEq vSyn !currentFidx origname fdec'.svar
                (Some (l, !currentDeclIdx))
            in
            if debugInlines then begin
              Kernel.debug "getNode %s(%d) with loc=%a. declidx=%d"
                inode.nname inode.nfidx
                d_nloc inode.nloc
                !currentDeclIdx;
              Kernel.debug
                "Looking for previous definition of inline %s(%d)"
                origname !currentFidx;
            end;
            try
              let oldinode = H.find inlineBodies printout in
              if debugInlines then
                Kernel.debug "  Matches %s(%d)"
                  oldinode.nname oldinode.nfidx;
              (* There is some other inline function with the same printout.
               * We should reuse this, but watch for the case when the inline
               * was already used. *)
              if H.mem varUsedAlready fdec'.svar.vname then begin
                if mergeInlinesRepeat then begin
                  repeatPass2 := true
                end else begin
                  Kernel.warning ~current:true
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
              if debugInlines then Kernel.debug " Not found";
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
                  Kernel.warning ~current:true
                    "dropping duplicate def'n of func %s at %a in favor of \
 that at %a"
                    fdec'.svar.vname
                    Cil_printer.pp_location l Cil_printer.pp_location prevLoc
                else begin
                  (* the checksums differ, so print a warning but keep the
                   * older one to avoid a link error later.  I think this is
		   * a reasonable approximation of what ld does. *)
                  Kernel.warning ~current:true
		    "def'n of func %s at %a (sum %d) conflicts with the one \
 at %a (sum %d); keeping the one at %a."
                    fdec'.svar.vname
                    Cil_printer.pp_location l 
		    curSum
		    Cil_printer.pp_location prevLoc
		    prevSum Cil_printer.pp_location prevLoc
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
            match 
              PlainMerging.findReplacement true sEq !currentFidx ci.cname 
            with
              None ->
                (* A new one, we must rename it and keep the definition *)
                (* Make sure this is root *)
                (try
                   let nd = 
                     PlainMerging.find_eq_table sEq (!currentFidx, ci.cname) 
                   in
                   if nd.nrep != nd then
                     Kernel.fatal "Setting creferenced for struct %s which is \
                         not root!"
                       ci.cname;
                 with Not_found -> begin
                   Kernel.fatal "Setting creferenced for struct %s which \
                                 is not in the sEq!"
                     ci.cname;
                 end);
                let newname, _ =
                  A.newAlphaName sAlpha ci.cname (CurrentLoc.get ()) in
                ci.cname <- newname;
                ci.creferenced <- true;
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
            match 
              EnumMerging.findReplacement true eEq !currentFidx ei
            with
              None -> (* We must rename it *)
                let newname, _ =
                  A.newAlphaName eAlpha ei.ename (CurrentLoc.get ())
                in
                ei.ename <- newname;
                ei.ereferenced <- true;
                (* And we must rename the items to using the same name space
                 * as the variables *)
                List.iter
                  (fun item ->
                     let newname,_ =
                       A.newAlphaName vtAlpha item.einame item.eiloc
                     in
                     item.einame <- newname)
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
            match 
              PlainMerging.findReplacement true tEq !currentFidx ti.tname 
            with
              None -> (* We must rename it and keep it *)
                let newname, _ =
                  A.newAlphaName vtAlpha ti.tname (CurrentLoc.get ())
                in
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
    Kernel.feedback "Repeat final merging phase: %s" f.fileName;
    (* We are going to rescan the globals we have added while processing this
     * file. *)
    let theseGlobals : global list ref = ref [] in
    (* Scan a list of globals until we hit a given tail *)
    let rec scanUntil (tail: 'a list) (l: 'a list) =
      if tail == l then ()
      else
        match l with
        | [] ->  Kernel.fatal "mergecil: scanUntil could not find the marker"
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
  Kernel.debug ~dkey "Merging global %a" Cil_printer.pp_global g;
  match g with
  | GFun(fdec,loc) ->
    (try
       Kernel.debug ~dkey
         "Merging global definition %a" Cil_printer.pp_global g;
       let specs = Cil_datatype.Varinfo.Hashtbl.find spec_to_merge fdec.svar in
       List.iter
	 (fun s -> 
	   Kernel.debug ~dkey "Found spec to merge %a" Cil_printer.pp_funspec s)
         specs;
       Kernel.debug ~dkey "Merging with %a" Cil_printer.pp_funspec fdec.sspec ;
       Cil.CurrentLoc.set loc;
       merge_specs fdec.sspec specs
     with Not_found -> 
       Kernel.debug ~dkey "No spec_to_merge")
  | GVarDecl(spec,v,loc) ->
    Kernel.debug ~dkey "Merging global declaration %a" Cil_printer.pp_global g;
    let rename spec =
      try
        let alpha = Cil_datatype.Varinfo.Hashtbl.find formals_renaming v in
        ignore (visitCilFunspec alpha spec)
      with Not_found -> ()
    in
    (try
       let specs = Cil_datatype.Varinfo.Hashtbl.find spec_to_merge v in
       List.iter
	 (fun s -> 
	   Kernel.debug ~dkey "Found spec to merge %a" Cil_printer.pp_funspec s)
         specs;
       Kernel.debug "Renaming %a" Cil_printer.pp_funspec spec ;
       rename spec;
       (* The registered specs might also need renaming up to 
          definition's formals instead of declaration's ones. *)
       List.iter rename specs;
       Kernel.debug ~dkey "Renamed to %a" Cil_printer.pp_funspec spec;
       Cil.CurrentLoc.set loc;
       merge_specs spec specs;
       Kernel.debug ~dkey "Merged into %a" Cil_printer.pp_funspec spec ;
     with Not_found -> 
       Kernel.debug ~dkey "No spec_to_merge for declaration" ;
       rename spec;
       Kernel.debug ~dkey "Renamed to %a" Cil_printer.pp_funspec spec ;
    )
  | _ -> ()

let find_decls g =
  let c_res = ref Cil_datatype.Varinfo.Set.empty in
  let res = ref Cil_datatype.Logic_var.Set.empty in
  let visit =
object(self)
      inherit Cil.nopCilVisitor
      method! vvdec v =
        c_res:=Cil_datatype.Varinfo.Set.add v !c_res; DoChildren
      method! vlogic_var_decl lv =
        res := Cil_datatype.Logic_var.Set.add lv !res;
        SkipChildren
      method! vspec _ = SkipChildren
      method! vfunc f = 
        ignore (self#vvdec f.svar);
        Extlib.may (ignore $ self#vlogic_var_decl) f.svar.vlogic_var_assoc;
        SkipChildren
    end
  in 
  ignore (visitCilGlobal visit g); !c_res, !res

let used_vars g =
  let res = ref Cil_datatype.Logic_var.Set.empty in
  let locals = ref Cil_datatype.Logic_var.Set.empty in
  let visit =
    object
      inherit Cil.nopCilVisitor
      method! vlogic_var_decl lv =
        locals := Cil_datatype.Logic_var.Set.add lv !locals;
        SkipChildren
      method! vlogic_var_use lv =
        if not (Cil_datatype.Logic_var.Set.mem lv !locals) 
          && not (Logic_env.is_builtin_logic_function lv.lv_name)
          && not (lv.lv_name = "\\exit_status")
        then 
          begin
            res:=Cil_datatype.Logic_var.Set.add lv !res
          end;
        SkipChildren
    end
  in
  ignore (visitCilGlobal visit g); !res

let print_missing fmt to_declare =
  let print_one_binding fmt s =
    Cil_datatype.Logic_var.Set.iter 
      (fun x -> Format.fprintf fmt "%a;@ " Cil_printer.pp_logic_var x) s
  in
  let print_entry fmt v (_,s) =
    Format.fprintf fmt "@[%a:@[%a@]@]@\n" 
      Cil_printer.pp_varinfo v print_one_binding s
  in
  Cil_datatype.Varinfo.Map.iter (print_entry fmt) to_declare
      

let move_spec globals =
  let all_declared known v (g,missing) (can_declare,to_declare) =
    let missing = Cil_datatype.Logic_var.Set.diff missing known in
    if Cil_datatype.Logic_var.Set.is_empty missing then
      (g::can_declare,to_declare)
    else
      (can_declare, Cil_datatype.Varinfo.Map.add v (g,missing) to_declare)
  in
  let aux (res,c_known,known,to_declare) g =
    let my_c_decls, my_decls = find_decls g in
    let known = Cil_datatype.Logic_var.Set.union my_decls known in
    let can_declare, to_declare =
      Cil_datatype.Varinfo.Map.fold (all_declared known) to_declare
        ([],Cil_datatype.Varinfo.Map.empty)
    in
    let res, to_declare =
      match g with
          GVarDecl (_,v,l) ->
            let needs = used_vars g in
            let missing = Cil_datatype.Logic_var.Set.diff needs known in
            if Cil_datatype.Logic_var.Set.is_empty missing then
              g::res, to_declare
            else 
              (GVarDecl(Cil.empty_funspec (),v,l)::res,
               Cil_datatype.Varinfo.Map.add v (g,missing) to_declare)
        | GFun (f,l) ->
          let needs = used_vars g in
          let missing = Cil_datatype.Logic_var.Set.diff needs known in
          if Cil_datatype.Logic_var.Set.is_empty missing then g::res,to_declare
          else
	    let res =
	      if Cil_datatype.Varinfo.Set.mem f.svar c_known then
		res 
	      else
		GVarDecl(Cil.empty_funspec (),f.svar,l)::res
	    in
	    res, Cil_datatype.Varinfo.Map.add f.svar (g,missing) to_declare
        | _ -> (g::res,to_declare)
    in
    let c_known = Cil_datatype.Varinfo.Set.union my_c_decls c_known in
    (can_declare @ res, c_known, known, to_declare)
  in
  let (res,_,_,to_declare) = 
    List.fold_left 
      aux 
      ([],
       Cil_datatype.Varinfo.Set.empty,
       Cil_datatype.Logic_var.Set.empty,
       Cil_datatype.Varinfo.Map.empty)
      globals
  in 
  assert
    (Kernel.verify (Cil_datatype.Varinfo.Map.is_empty to_declare)
       "Some globals contain dangling references after link:@\n%a"
       print_missing to_declare);
  List.rev res

let merge (files: file list) (newname: string) : file =
  init ();

  Cilmsg.push_errors ();

  (* Make the first pass over the files *)
  currentFidx := 0;
  List.iter (fun f -> oneFilePass1 f; incr currentFidx) files;

  (* Now maybe try to force synonyms to be equal *)
  if mergeSynonyms then begin
    doMergeSynonyms sSyn matchCompInfo;
    EnumMerging.doMergeSynonyms eSyn matchEnumInfo;
    doMergeSynonyms tSyn matchTypeInfo;

    doMergeSynonyms lfSyn matchLogicInfo;
    doMergeSynonyms ltSyn matchLogicType;
    doMergeSynonyms lcSyn matchLogicCtor;
    doMergeSynonyms laSyn matchLogicAxiomatic;
    doMergeSynonyms llSyn matchLogicLemma;
    VolatileMerging.doMergeSynonyms lvSyn matchVolatileClause;
    ModelMerging.doMergeSynonyms mfSyn matchModelField;
    if mergeInlines then begin
      (* Copy all the nodes from the iEq to vEq as well. This is needed
       * because vEq will be used for variable renaming *)
      PlainMerging.iter_eq_table 
        (fun k n -> PlainMerging.add_eq_table vEq k n) iEq;
      doMergeSynonyms iSyn matchInlines;
    end
  end;

  (* Now maybe dump the graph *)
  if false then begin
    dumpGraph "type" tEq;
    dumpGraph "struct and union" sEq;
    EnumMerging.dumpGraph "enum" eEq;
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
  let globals = move_spec res.globals in
  res.globals <- globals;
  init ~all:false (); (* Make the GC happy BUT KEEP some tables *)
  (* We have made many renaming changes and sometimes we have just guessed a
   * name wrong. Make sure now that the local names are unique. *)
  uniqueVarNames res;
  let res =
    if Cilmsg.had_errors () then
      begin
        Kernel.error "Error during linking@." ;
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
