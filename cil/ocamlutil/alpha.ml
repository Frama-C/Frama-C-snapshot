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

let debugAlpha (_prefix: string) = false
(*** Alpha conversion ***)
let alphaSeparator = "_"
let alphaSeparatorLen = String.length alphaSeparator

(** For each prefix we remember the next integer suffix to use and the list 
 * of suffixes, each with some data assciated with the newAlphaName that 
 * created the suffix. *)
type 'a alphaTableData = Big_int.big_int * (string * 'a) list

type 'a undoAlphaElement = 
    AlphaChangedSuffix of 'a alphaTableData ref * 'a alphaTableData (* The 
                                             * reference that was changed and 
                                             * the old suffix *)
  | AlphaAddedSuffix of string          (* We added this new entry to the 
                                         * table *)

(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name by stripping a suffix consisting of 
 * the alphaSeparator followed by only digits), followed by alphaSeparator 
 * and then by a positive integer suffix. The first argument is a table 
 * mapping name prefixes to the largest suffix used so far for that 
 * prefix. The largest suffix is one when only the version without suffix has 
 * been used. *)
let rec newAlphaName ~(alphaTable: (string, 'a alphaTableData ref) H.t)
                     ?undolist
                     ~(lookupname: string) 
                     ~(data: 'a) : string * 'a = 
  alphaWorker ~alphaTable:alphaTable ?undolist
              ~lookupname:lookupname ~data:data true
  

(** Just register the name so that we will not use in the future *)
and registerAlphaName ~(alphaTable: (string, 'a alphaTableData ref) H.t)
                      ?undolist
                      ~(lookupname: string) 
                      ~(data: 'a) : unit = 
  ignore (alphaWorker ~alphaTable:alphaTable ?undolist
                      ~lookupname:lookupname ~data:data false)


and alphaWorker      ~(alphaTable: (string, 'a alphaTableData ref) H.t)
                     ?undolist
                     ~(lookupname: string) ~(data:'a)
                     (make_new: bool) : string * 'a = 
  let prefix, suffix, (numsuffix: Big_int.big_int) =
    splitNameForAlpha ~lookupname
  in
  if debugAlpha prefix then
    (Kernel.debug "Alpha worker: prefix=%s suffix=%s (%s) create=%b. " 
              prefix suffix (Big_int.string_of_big_int numsuffix) make_new);
  let newname, (olddata: 'a) = 
    try
      let rc = H.find alphaTable prefix in
      let max, suffixes = !rc in 
      (* We have seen this prefix *)
      if debugAlpha prefix then
        Kernel.debug " Old max %s. Old suffixes: @[%a@]" 
	  (Big_int.string_of_big_int max)
          (Pretty_utils.pp_list (fun fmt (s,_) -> Format.fprintf fmt "%s" s)) suffixes ;
      (* Save the undo info *)
      (match undolist with 
        Some l -> l := AlphaChangedSuffix (rc, !rc) :: !l
      | _ -> ());

      let newmax, newsuffix, (olddata: 'a), newsuffixes = 
        if Big_int.gt_big_int numsuffix max then begin 
          (* Clearly we have not seen it *)
          numsuffix, suffix, data,
          (suffix, data) :: suffixes 
        end else begin 
          match List.filter (fun (n, _) -> n = suffix) suffixes with 
            [] -> (* Not found *)
              max, suffix, data, (suffix, data) :: suffixes
          | [(_, l) ] -> 
              (* We have seen this exact suffix before *)
              if make_new then 
                let newsuffix = 
                  alphaSeparator ^ (Big_int.string_of_big_int (Big_int.succ_big_int max )) 
                in
               Big_int.succ_big_int max, newsuffix, l, (newsuffix, data) :: suffixes
              else
                max, suffix, data, suffixes
          |  _ -> (Kernel.fatal "Cil.alphaWorker")
        end
      in
      rc := (newmax, newsuffixes);
      prefix ^ newsuffix, olddata
    with Not_found -> begin (* First variable with this prefix *)
      (match undolist with 
        Some l -> l := AlphaAddedSuffix prefix :: !l
      | _ -> ());
      H.add alphaTable prefix (ref (numsuffix, [ (suffix, data) ]));
      if debugAlpha prefix then (Kernel.debug " First seen. ");
      lookupname, data  (* Return the original name *)
    end
  in
  if debugAlpha prefix then
    (Kernel.debug " Res=: %s \n" newname (* d_loc oldloc *));
  newname, olddata

(* Strip the suffix. Return the prefix, the suffix (including the separator 
 * and the numeric value, possibly empty), and the 
 * numeric value of the suffix (possibly -1 if missing) *) 
and splitNameForAlpha ~(lookupname: string) : (string * string * Big_int.big_int) = 
  let len = String.length lookupname in
  (* Search backward for the numeric suffix. Return the first digit of the 
   * suffix. Returns len if no numeric suffix *)
  let rec skipSuffix (i: int) = 
    if i = -1 then -1 else 
    let c = Char.code (String.get lookupname i) - Char.code '0' in
    if c >= 0 && c <= 9 then 
      skipSuffix (i - 1)
    else (i + 1)
  in
  let startSuffix = skipSuffix (len - 1) in

  if startSuffix >= len (* No digits at all at the end *) ||
     startSuffix <= alphaSeparatorLen     (* Not enough room for a prefix and 
                                           * the separator before suffix *) ||
     (* Suffix starts with a 0 and has more characters after that *) 
     (startSuffix < len - 1 && String.get lookupname startSuffix = '0')  ||
     alphaSeparator <> String.sub lookupname 
                                 (startSuffix - alphaSeparatorLen)  
                                 alphaSeparatorLen 
  then
    (lookupname, "", (Big_int.minus_big_int Big_int.unit_big_int))  (* No valid suffix in the name *)
  else
    (String.sub lookupname 0 (startSuffix - alphaSeparatorLen), 
     String.sub lookupname (startSuffix - alphaSeparatorLen) 
                           (len - startSuffix + alphaSeparatorLen),
     Big_int.big_int_of_string (String.sub lookupname startSuffix (len - startSuffix)))
    

let getAlphaPrefix ~(lookupname:string) : string = 
  let p, _, _ = splitNameForAlpha ~lookupname:lookupname in
  p
      
(* Undoes the changes as specified by the undolist *)
let undoAlphaChanges ~(alphaTable: (string, 'a alphaTableData ref) H.t) 
                     ~(undolist: 'a undoAlphaElement list) = 
  List.iter
    (function 
        AlphaChangedSuffix (where, old) -> 
          where := old
      | AlphaAddedSuffix name -> 
          if debugAlpha name then 
            (Kernel.debug "Removing %s from alpha table\n" name);
          H.remove alphaTable name)
    undolist

let docAlphaTable fmt (alphaTable: (string, 'a alphaTableData ref) H.t) = 
  let acc = ref [] in
  H.iter (fun k d -> acc := (k, !d) :: !acc) alphaTable;
  Pretty_utils.pp_list ~sep:"@\n" 
    (fun fmt (k, (d, _)) -> 
       Format.fprintf fmt "  %s -> %s" k (Big_int.string_of_big_int d))
    fmt !acc

