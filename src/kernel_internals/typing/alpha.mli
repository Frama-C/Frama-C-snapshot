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

(** Alpha conversion. *)

(** This is the type of the elements that are recorded by the alpha 
 * conversion functions in order to be able to undo changes to the tables 
 * they modify. Useful for implementing 
 * scoping *)
type 'a undoAlphaElement

(** This is the type of the elements of the alpha renaming table. These 
 * elements can carry some data associated with each occurrence of the name. *)
type 'a alphaTableData


(** Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name by stripping a suffix consisting of _ 
 * followed by only digits), followed by a special separator and then by a 
 * positive integer suffix. The first argument is a table mapping name 
 * prefixes to some data that specifies what suffixes have been used and how 
 * to create the new one. This function updates the table with the new 
 * largest suffix generated. The "undolist" argument, when present, will be 
 * used by the function to record information that can be used by 
 * {!Alpha.undoAlphaChanges} to undo those changes. Note that the undo 
 * information will be in reverse order in which the action occurred. Returns 
 * the new name and, if different from the lookupname, the location of the 
 * previous occurrence. This function knows about the location implicitly 
 * from the [(Cil.CurrentLoc.get ())]. *)
val newAlphaName: alphaTable:(string, 'a alphaTableData ref) Hashtbl.t ->
                  ?undolist: 'a undoAlphaElement list ref ->
                  lookupname:string -> data:'a -> string * 'a


(** Register a name with an alpha conversion table to ensure that when later 
  * we call newAlphaName we do not end up generating this one *)
val registerAlphaName: alphaTable:(string, 'a alphaTableData ref) Hashtbl.t -> 
                       ?undolist: 'a undoAlphaElement list ref ->
                       lookupname:string -> data:'a -> unit

(** Split the name in preparation for newAlphaName. The prefix returned is 
    used to index into the hashtable. The next result value is a separator 
    (either empty or the separator chosen to separate the original name from 
     the index)  *)
val docAlphaTable: Format.formatter -> 
                  (string, 'a alphaTableData ref) Hashtbl.t -> unit


val getAlphaPrefix: lookupname:string -> string

(** Undo the changes to a table *)
val undoAlphaChanges: alphaTable:(string, 'a alphaTableData ref) Hashtbl.t -> 
                      undolist:'a undoAlphaElement list -> unit
