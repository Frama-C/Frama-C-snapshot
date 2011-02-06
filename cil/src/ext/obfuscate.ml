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

open Cil
open Cil_types
open Cil_datatype

class renamer prefix dictionary = object
  val prefix = prefix
  val mutable index = 0

  method fresh (name:string) =
    index <- index + 1;
    let fresh = prefix ^ string_of_int index in
    Hashtbl.add dictionary fresh name;
    fresh
end

class obfuscateVisitor dictionary = object
  inherit nopCilVisitor
  val var  = new renamer "G" dictionary
  val field = new renamer "M" dictionary
  val typ = new renamer "T" dictionary
  val enum = new renamer "E" dictionary
  val local = new renamer "V" dictionary
  val functions = new renamer "F" dictionary
  val formals = new renamer "f" dictionary

  val varinfos_visited = Varinfo.Hashtbl.create 17

  method vglob global =
    begin match global with
    | GType (ty,_) -> ty.tname <- typ#fresh ty.tname
    | _ -> ()
    end;
    DoChildren

  method vcompinfo ci =  ci.cname <- typ#fresh ci.cname; DoChildren

  method vfieldinfo fi = fi.fname <- field#fresh fi.fname; DoChildren

  method venuminfo ei = ei.ename <- typ#fresh ei.ename; DoChildren

  method venumitem ei = ei.einame <- enum#fresh ei.einame; DoChildren

  method vvdec vi =
    (* Varinfo can be visited (and obfuscated) more than once:
       functions for their declaration and definition, variables
       as parts of the type of the function, and in the body of
       the function declaration, etc. Thus we make sure that the
       obfuscator does not visit them twice *)
    try
      Varinfo.Hashtbl.find varinfos_visited vi;
      SkipChildren
    with Not_found ->
      if isFunctionType vi.vtype then begin
	if vi.vname <> "main" then vi.vname <- functions#fresh vi.vname
      end else
        vi.vname <-
          if vi.vglob then var#fresh vi.vname
          else if vi.vformal then formals#fresh vi.vname
          else local#fresh vi.vname;
      Varinfo.Hashtbl.add varinfos_visited vi ();
      DoChildren
end

let obfuscate file =
  let dictionary = Hashtbl.create 7 in
  let v = new obfuscateVisitor dictionary in
  visitCilFileSameGlobals (v:>cilVisitor) file;
  dictionary

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
