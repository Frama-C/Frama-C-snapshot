(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
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
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

open Cil
open Cil_types

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

  method vglob global =
    begin match global with
    | GType (ty,_) ->
        ty.tname <- typ#fresh ty.tname;

    | GVarDecl (_spec, ({vtype = TFun (t,Some l, b,att)} as vi),_) ->
        let fresh_l = List.map (fun (n,t,a) -> (var#fresh n,t,a)) l in
        vi.vtype <- TFun (t,Some fresh_l, b,att)
    | _ -> ()
    end;
    DoChildren

  method vcompinfo ci =  ci.cname <- typ#fresh ci.cname; DoChildren

  method vfieldinfo fi = fi.fname <- field#fresh fi.fname; DoChildren

  method venuminfo ei = ei.ename <- typ#fresh ei.ename; DoChildren

  method venumitem ei = ei.einame <- enum#fresh ei.einame; DoChildren

  method vvdec vi =
    if isFunctionType vi.vtype then
      begin if vi.vname <> "main" then vi.vname <- functions#fresh vi.vname end
    else
      vi.vname <-
        if vi.vglob then var#fresh vi.vname
        else local#fresh vi.vname;
    DoChildren
end

let obfuscate file =
  let dictionary = Hashtbl.create 7 in
  let v = new obfuscateVisitor dictionary in
  visitCilFile (v:>cilVisitor) file;
  dictionary
