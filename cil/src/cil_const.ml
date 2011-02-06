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

open Cilutil
open Format
open Cil_types

module CurrentLoc =
  State_builder.Ref
    (Cil_datatype.Location)
    (struct
       let dependencies = []
       let name = "CurrentLoc"
       let kind = `Internal
       let default () = Cil_datatype.Location.unknown
     end)

let voidType = TVoid([])

let d_loc fmt loc =
  fprintf fmt "%s:%d" (fst loc).Lexing.pos_fname (fst loc).Lexing.pos_lnum

let d_thisloc (fmt: formatter) : unit = d_loc fmt (CurrentLoc.get ())

(*
let generic_bug s fstring =
  let f fmt =
    E.hadErrors := true;
    kfprintf (fun _ -> E.showContext (); raise E.Error) fmt (fstring ^^ "@]@.")
  in
  kfprintf f err_formatter "@[%t: %s: " d_thisloc s
*)

let error fstring = Cilmsg.abort ~current:true fstring
let fatal fstring = Cilmsg.fatal ~current:true fstring

module Build_Counter(Name:sig val name:string end) : sig
  val next: unit -> int
  val reset: unit -> unit
  val get: unit -> int
  val self: State.t
end = struct
  include State_builder.Zero_ref
    (struct
       let dependencies = []
       let name = Name.name
       let kind = `Internal
     end)
  let next () =
    let n = get () in
    if n = -1 then fatal "Too many values for counter %s." Name.name;
    set (succ n);
    get ()
  let reset = clear
end


module Vid = Build_Counter(struct let name = "vid" end)

let set_vid v =
  let n = Vid.next () in
  v.vid <- n

let copy_with_new_vid v =
  let n = Vid.next () in
  { v with vid = n }
;;

let new_raw_id = Vid.next

let make_logic_var x typ =
  {lv_name = x; lv_id = new_raw_id(); lv_type = typ; lv_origin = None }

let make_logic_info x =
  { l_var_info = make_logic_var x (Ctype voidType);
      (* we should put the right type when fields
	 l_profile, l_type will be factorized *)
    l_type = None;
    l_tparams = [];
    l_labels = [];
    l_profile = [];
    l_body = LBnone;
  }

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
