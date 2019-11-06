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

open Cil_types

module CurrentLoc =
  State_builder.Ref
    (Cil_datatype.Location)
    (struct
       let dependencies = []
       let name = "CurrentLoc"
       let default () = Cil_datatype.Location.unknown
     end)

let voidType = TVoid([])

module Vid = State_builder.SharedCounter(struct let name = "vid_counter" end)
module Sid = State_builder.SharedCounter(struct let name = "sid" end)
module Eid = State_builder.SharedCounter(struct let name = "eid" end)

let set_vid v =
  let n = Vid.next () in
  v.vid <- n

let copy_with_new_vid v =
  let n = Vid.next () in
  let new_v = { v with vid = n } in
  (match v.vlogic_var_assoc with
    | None -> ()
    | Some lv ->
      let new_lv = { lv with lv_id = n } in
      new_v.vlogic_var_assoc <- Some new_lv;
      new_lv.lv_origin <- Some new_v);
  new_v

let change_varinfo_name vi name =
  vi.vname <- name;
  match vi.vlogic_var_assoc with
    | None -> ()
    | Some lv -> lv.lv_name <- name

let new_raw_id = Vid.next

(* The next compindo identifier to use. Counts up. *)
 let nextCompinfoKey =
   let module M =
         State_builder.SharedCounter(struct let name = "compinfokey" end)
   in
   M.next

(** Creates a (potentially recursive) composite type. Make sure you add a
  * GTag for it to the file! **)
let mkCompInfo
    (isstruct: bool)
    (n: string)
    ?(norig=n)
    (* fspec is a function that when given a forward
       * representation of the structure type constructs the type of
       * the fields. The function can ignore this argument if not
       * constructing a recursive type.  *)
    (mkfspec: compinfo -> (string * typ * int option * attribute list *
			   location) list)
    (a: attribute list) : compinfo =

  (* make a new name for anonymous structs *)
  if n = "" then Kernel.fatal "mkCompInfo: missing structure name\n" ;
  (* Make a new self cell and a forward reference *)
  let comp =
    { cstruct = isstruct;
      corig_name = norig;
      cname = n;
      ckey = nextCompinfoKey ();
      cfields = []; (* fields will be added afterwards. *)
      cattr = a;
      creferenced = false;
      (* Make this compinfo undefined by default *)
      cdefined = false; }
  in
  let flds =
    List.map (fun (fn, ft, fb, fa, fl) ->
	{ fcomp = comp;
	  ftype = ft;
	  forig_name = fn;
	  fname = fn;
	  fbitfield = fb;
	  fattr = fa;
	  floc = fl;
	  faddrof = false;
	  fsize_in_bits = None;
	  foffset_in_bits = None;
	  fpadding_in_bits = None;
	}) (mkfspec comp) in
  comp.cfields <- flds;
  if flds <> [] then comp.cdefined <- true;
  comp

(** Make a copy of a compinfo, changing the name and the key *)
let copyCompInfo ?(fresh=true) ci cname =
  let ckey = if fresh then nextCompinfoKey () else ci.ckey in
  let ci' = { ci with cname; ckey } in
  (* Copy the fields and set the new pointers to parents *)
  ci'.cfields <- List.map (fun f -> {f with fcomp = ci'}) ci'.cfields;
  ci'


let make_logic_var_kind x kind typ =
  {lv_name = x; lv_id = new_raw_id(); lv_type = typ; lv_kind = kind; 
   lv_origin = None; lv_attr = [] }

let make_logic_var_global x t = make_logic_var_kind x LVGlobal t
let make_logic_var_formal x t = make_logic_var_kind x LVFormal t
let make_logic_var_quant x t = make_logic_var_kind x LVQuant t
let make_logic_var_local x t = make_logic_var_kind x LVLocal t

let make_logic_var =
  Kernel.deprecated "Cil_const.make_logic_var" 
    ~now:"Use one of Cil_const.make_logic_var_* to indicate \
          the origin of the variable"
    make_logic_var_quant

let make_logic_info k x =
  { l_var_info = make_logic_var_kind x k (Ctype voidType);
      (* we should put the right type when fields
	 l_profile, l_type will be factorized *)
    l_type = None;
    l_tparams = [];
    l_labels = [];
    l_profile = [];
    l_body = LBnone;
  }

let make_logic_info_local = make_logic_info LVLocal
let make_logic_info = make_logic_info LVGlobal

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
