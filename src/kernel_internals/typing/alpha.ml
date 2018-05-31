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

module H = Hashtbl

(*** Alpha conversion ***)
let alphaSeparator = '_'

(** For each prefix we remember the last integer suffix that has been used
    (to start searching for a fresh name) and the list 
 * of suffixes, each with some data associated with the newAlphaName that 
 * created the suffix. *)
type 'a alphaTableData = Integer.t * (Integer.t * 'a) list

type 'a undoAlphaElement = 
    AlphaChangedSuffix of 'a alphaTableData ref * 'a alphaTableData (* The 
                                             * reference that was changed and 
                                             * the old suffix *)
  | AlphaAddedSuffix of string * string  (* We added this new entry to the 
                                          * table *)

type 'a alphaTable = (string, (string, 'a alphaTableData ref) H.t) H.t

(* specify a behavior for renaming *)
type rename_mode =
  | Incr_last_suffix
     (* increment the last suffix in the original id 
        (adding _nnn if no suffix exists in the original id) *)
  | Add_new_suffix
      (* systematically adds a _nnn suffix even if the original name
         ends with _mmm *)

let has_generated_prefix n prefix =
  let prefix_length = String.length prefix in
  let real_name =
    if String.contains n ' ' then begin
      let i = String.rindex n ' ' in
      String.sub n (i+1) (String.length n - i - 1)
    end else n
  in
  String.length real_name >= prefix_length &&
    String.sub real_name 0 prefix_length = prefix

let generated_prefixes = [ "__anon"; "__constr_expr" ]

let is_generated_name n =
  List.exists (has_generated_prefix n) generated_prefixes

(* Strip the suffix. Return the prefix, the suffix (including the separator 
 * but not the numeric value, possibly empty), and the 
 * numeric value of the suffix (possibly -1 if missing) *) 
let splitNameForAlpha ~(lookupname: string) = 
  let len = String.length lookupname in
  (* Search backward for the numeric suffix. Return the first digit of the 
   * suffix. Returns len if no numeric suffix *)
  let rec skipSuffix seen_sep last_sep (i: int) =
    if i = -1 then last_sep else
    let c = lookupname.[i] in
    (* we might start to use Str at some point. *)
    if (Char.compare '0' c <= 0 && Char.compare c '9' <= 0) then
      skipSuffix false last_sep (i - 1)
    else if c = alphaSeparator then
      if not seen_sep then
         (* check whether we are in the middle of a multi-suffix ident 
            e.g. x_0_2, where the prefix would be x. *)
        skipSuffix true i (i-1)
      else (* we have something like x__0. Consider x_ as the prefix. *)
        i+1
    else (* we have something like x1234_0. Consider x1234 as the prefix *)
      last_sep
  in
  (* we start as if the next char of the identifier was _, so that
     x123_ is seen as a prefix.
   *)
  let startSuffix = skipSuffix true len (len - 1) in
  if startSuffix >= len
  then
    (lookupname, "")  (* No valid suffix in the name *)
  else begin
    (String.sub lookupname 0 startSuffix,
     String.sub lookupname startSuffix (len - startSuffix))
  end

let make_suffix n = (String.make 1 alphaSeparator) ^ (Integer.to_string n)

let make_full_suffix infix n = infix ^ make_suffix n

(* find an unused suffix in l greater than or equal to min, knowing that all
   elements of l are less than or equal to max.
   returns the new suffix and a new bound to max in case the new suffix is
   greater than max.
 *)
let find_unused_suffix min infix sibling l =
  let rec aux v =
    if List.exists (fun (n,_) -> Integer.equal n v) l
      || H.mem sibling (make_full_suffix infix v)
    then begin
      Kernel.debug ~dkey:Kernel.dkey_alpha
        "%s is already taken" (make_full_suffix infix v);
      aux (Integer.succ v)
    end else v
  in aux min

let get_suffix_idx rename_mode infix =
  match rename_mode with
    | Add_new_suffix -> infix, Integer.minus_one
    | Incr_last_suffix when infix = "" -> infix, Integer.minus_one
    | Incr_last_suffix ->
      (* by construction there is at least one alphaSeparator in the infix *)
      let idx = String.rindex infix alphaSeparator in
      String.sub infix 0 idx,
      Integer.of_string
        (String.sub infix (idx + 1) (String.length infix - idx - 1))

(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name by stripping a suffix consisting of 
 * the alphaSeparator followed by only digits), followed by alphaSeparator 
 * and then by a positive integer suffix. The first argument is a table 
 * mapping name prefixes to the largest suffix used so far for that 
 * prefix. The largest suffix is one when only the version without suffix has 
 * been used. *)

let alphaWorker      ~(alphaTable: 'a alphaTable)
                     ?undolist
                     ~(lookupname: string) ~(data:'a)
                     (make_new: bool) : string * 'a = 
  let prefix, infix = splitNameForAlpha ~lookupname in
  let rename_mode =
    if is_generated_name prefix then Incr_last_suffix else Add_new_suffix
  in
  let infix, curr_idx = get_suffix_idx rename_mode infix in
  Kernel.debug ~dkey:Kernel.dkey_alpha
    "Alpha worker: lookupname=%s prefix=%s infix=%s index=%s create=%B."
    lookupname prefix infix (Integer.to_string curr_idx) make_new;
  let newname, (olddata: 'a) =
    try
      let infixes = H.find alphaTable prefix in
      let rc = H.find infixes infix in
      let min, suffixes = !rc in
      (* We have seen this prefix *)
      Kernel.debug ~dkey:Kernel.dkey_alpha "Old min %s. Old suffixes: @[%a@]"
	(Integer.to_string min)
        (Pretty_utils.pp_list
           (fun fmt (s,_) -> Format.fprintf fmt "%s" (Integer.to_string s)))
        suffixes;
      (* Save the undo info *)
      (match undolist with
        Some l -> l := AlphaChangedSuffix (rc, !rc) :: !l
      | _ -> ());
      let newname, newmin, (olddata: 'a), newsuffixes =
        match
          List.filter (fun (n, _) -> Integer.equal n curr_idx) suffixes
        with
          | [] -> (* never seen this index before *)
            lookupname, min, data, (curr_idx, data) :: suffixes
          | [(_, l) ] ->
              (* We have seen this exact suffix before *)
              (* In Incr_last_suffix mode, we do not take curr_idx into account,
                 but select the first available index available *)
              if make_new then begin
                let newmin =
                  find_unused_suffix (Integer.succ min) infix infixes suffixes
                in
                let newsuffix = make_suffix newmin in
                let base =
                  if is_generated_name prefix then prefix else lookupname
                in
                H.add
                  infixes newsuffix
                  (ref (Integer.minus_one, [(Integer.minus_one, data)]));
                (match undolist with
                  | Some l -> l:= AlphaAddedSuffix (prefix,newsuffix)::!l
                  | None -> ());
                base ^ newsuffix, newmin, l, (newmin, data) :: suffixes
              end else lookupname, min, data, suffixes
          |  _ -> (Kernel.fatal "Cil.alphaWorker")
      in
      rc := (newmin, newsuffixes);
      newname, olddata
    with Not_found -> begin (* First variable with this prefix *)
      (match undolist with 
        Some l -> l := AlphaAddedSuffix (prefix,infix) :: !l
      | _ -> ());
      let infixes =
        try H.find alphaTable prefix
        with Not_found ->
          let h = H.create 3 in H.add alphaTable prefix h; h
      in
      H.add infixes infix
        (ref (Integer.minus_one, [ (curr_idx, data) ]));
      Kernel.debug ~dkey:Kernel.dkey_alpha " First seen. ";
      lookupname, data  (* Return the original name *)
    end
  in
  Kernel.debug ~dkey:Kernel.dkey_alpha "Res=: %s" newname;
  newname, olddata

    
let newAlphaName ~alphaTable ?undolist ~lookupname ~data =
  alphaWorker ~alphaTable ?undolist ~lookupname ~data true

(** Just register the name so that we will not use in the future *)
let registerAlphaName ~alphaTable ?undolist ~lookupname ~data =
  ignore (alphaWorker ~alphaTable ?undolist ~lookupname ~data false)

let getAlphaPrefix ~lookupname = splitNameForAlpha ~lookupname

(* Undoes the changes as specified by the undolist *)
let undoAlphaChanges ~alphaTable ~undolist = 
  List.iter
    (function 
        AlphaChangedSuffix (where, old) -> 
          where := old
      | AlphaAddedSuffix (prefix, infix) ->
        Kernel.debug ~dkey:Kernel.dkey_alpha_undo
          "Removing %s%s from alpha table\n" prefix infix;
        try
          let infixes = H.find alphaTable prefix in
          H.remove infixes infix;
          if H.length infixes = 0 then H.remove alphaTable prefix
        with Not_found ->
          Kernel.warning
            "prefix %s has no entry in the table. Inconsistent undo list"
            prefix)
    undolist

