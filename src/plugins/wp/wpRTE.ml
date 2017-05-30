(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

let dkey = Wp_parameters.register_category "rte"

type t = {
  name : string ;
  kernel : (unit -> bool) ;
  rtegen : string ;
  cint : bool ;
  status : (unit -> Db.RteGen.status_accessor) ref ;
}

let option name =
  try name = "" || Dynamic.Parameter.Bool.get name ()
  with _ -> false

let status db kf =
  try
    (* Absolutely forbidden to use 'set' from Db.RteGen :
       this disables the generation of the associated RTE. *)
    let (_,_,get) = (!db) () in get kf
  with Failure _ ->
    Wp_parameters.warning ~once:true
      "Missing RTE plug-in: can not generate conditions" ;
    false

let is_precond_generated = status Db.RteGen.get_precond_status

let always _ = true

let configure ~update ~generate kf cint rte =
  if not rte.cint || rte.kernel () then
    begin
      (* need RTE guard, but kernel option is set *)
      if not (status rte.status kf) then
        begin
          if option rte.rtegen then
            let msg = if generate then "generate" else "missing" in
            Wp_parameters.debug ~dkey "function %a: %s rte for %s"
              Kernel_function.pretty kf msg rte.name ;
          else
            Wp_parameters.warning ~once:true ~current:false
              "-wp-rte can annotate %s because %s is not set"
              rte.name rte.rtegen ;
          update := true ;
        end
    end
  else if generate then
    match cint with
    | Cint.Machine -> () (* RTE has been set *)
    | Cint.Natural ->
        Wp_parameters.warning ~once:true ~current:false
          "-wp-rte and model nat require kernel to warn against %s" rte.name

let generator =
  [
    { name = "memory access" ;
      kernel = always ; rtegen = "-rte-mem" ; cint = false ;
      status = Db.RteGen.get_memAccess_status } ;
    { name = "division by zero" ;
      kernel = always ; rtegen = "-rte-div" ; cint = false ;
      status = Db.RteGen.get_divMod_status } ;
    { name = "signed overflow" ; cint = true ;
      kernel = Kernel.SignedOverflow.get ; rtegen = "" ;
      status = Db.RteGen.get_signedOv_status } ;
    { name = "unsigned overflow" ; cint = true ;
      kernel = Kernel.UnsignedOverflow.get ; rtegen = "" ;
      status = Db.RteGen.get_unsignedOv_status } ;
    { name = "signed downcast" ; cint = true ; rtegen = "" ;
      kernel = Kernel.SignedDowncast.get ;
      status = Db.RteGen.get_signed_downCast_status } ;
    { name = "unsigned downcast" ; cint = true ; rtegen = "" ;
      kernel = Kernel.UnsignedDowncast.get ;
      status = Db.RteGen.get_unsignedDownCast_status } ;
  ]

let generate kf model =
  let update = ref false in
  let cint = Model.with_model model Cint.current () in
  List.iter (configure ~update ~generate:true kf cint) generator ;
  if !update then !Db.RteGen.annotate_kf kf

let missing_guards kf model =
  let update = ref false in
  let cint = Model.with_model model Cint.current () in
  List.iter (configure ~update ~generate:false kf cint) generator ;
  !update
