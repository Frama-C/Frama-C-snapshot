(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Warning Manager                                                    --- *)
(* -------------------------------------------------------------------------- *)

module SELF =
struct

  type t = {
    wrn_loc : Lexing.position ;
    wrn_severe : bool ;
    wrn_source : string ;
    wrn_reason : string ;
    wrn_effect : string ;
  }
      
  let compare w1 w2 =
    if w1 == w2 then 0 else
      let f1 = w1.wrn_loc.Lexing.pos_fname in
      let f2 = w2.wrn_loc.Lexing.pos_fname in
      let fc = String.compare f1 f2 in
      if fc <> 0 then fc else
	let l1 = w1.wrn_loc.Lexing.pos_lnum in
	let l2 = w2.wrn_loc.Lexing.pos_lnum in
	let lc = l1 - l2 in
	if lc <> 0 then lc else
	  match w1.wrn_severe , w2.wrn_severe with
	    | true , false -> (-1)
	    | false , true -> 1
	    | _ -> Pervasives.compare w1 w2
		
end

include SELF
module Map = Map.Make(SELF)
module Set = Set.Make(SELF)	      

let severe s = Set.exists (fun w -> w.wrn_severe) s

let pretty fmt w =
  begin
    Format.fprintf fmt
      "@[<v 0>%s:%d: warning from %s:@\n"
      w.wrn_loc.Lexing.pos_fname
      w.wrn_loc.Lexing.pos_lnum
      w.wrn_source ;
    if w.wrn_severe then
      Format.fprintf fmt " - Warning: %s, looking for context inconsistency"
        w.wrn_effect
    else
      Format.fprintf fmt " - Warning: %s" w.wrn_effect ;
    Format.fprintf fmt "@\n   Reason: %s@]" w.wrn_reason ;
  end

type collector = {
  default : string ;
  mutable warnings : Set.t ;
}

let collector : collector Context.value = Context.create "Warning"
let default () = (Context.get collector).default

(* -------------------------------------------------------------------------- *)
(* --- Contextual Errors                                                  --- *)
(* -------------------------------------------------------------------------- *)

exception Error of string * string (* source , reason *)

let error ?(source="wp") text = 
  let buffer = Buffer.create 120 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       let text = Buffer.contents buffer in
       if Context.defined collector then
	 raise (Error (source,text))
       else
	 Wp_parameters.fatal ~current:true "%s" text
    ) (Format.formatter_of_buffer buffer) text


(* -------------------------------------------------------------------------- *)
(* --- Contextual Errors                                                  --- *)
(* -------------------------------------------------------------------------- *)

type context = collector option
let context ?(source="wp") () = 
  Context.push collector { default = source ; warnings = Set.empty }

let flush old =
  let c = Context.get collector in
  Context.pop collector old ; c.warnings

let add w =
  Wp_parameters.warning ~source:w.wrn_loc "%s" w.wrn_reason ~once:true ;
  let c = Context.get collector in
  c.warnings <- Set.add w c.warnings

let emit ?(severe=false) ?source ~effect message =
  let source = match source with Some s -> s | None -> default () in
  let buffer = Buffer.create 80 in
  Format.kfprintf 
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       let text = Buffer.contents buffer in
       let loc = Cil_const.CurrentLoc.get () in
       add { 
	 wrn_loc = fst loc ; 
	 wrn_severe = severe ; 
	 wrn_source = source ;
	 wrn_effect = effect ; 
	 wrn_reason = text ;
       })
    (Format.formatter_of_buffer buffer)
    message

let handle ?(severe=false) ~effect ~handler cc x =
  try cc x
  with Error(source,reason) ->
    if Context.defined collector then
      ( emit ~severe ~source ~effect "%s" reason ; handler x )
    else
      if source <> "wp" then
	Wp_parameters.fatal ~current:true "[%s] %s" source reason
      else
	Wp_parameters.fatal ~current:true "%s" reason

type 'a outcome =
  | Result of Set.t * 'a
  | Failed of Set.t

let catch ?source ?(severe=true) ~effect cc x =
  let wrn = context ?source () in
  try let y = cc x in Result(flush wrn,y) (* DO NOT inline this let *)
  with Error(source,reason) ->
    emit ~severe ~source ~effect "%s" reason ;
    Failed (flush wrn)
