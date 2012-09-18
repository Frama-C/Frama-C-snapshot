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
(* --- Fast Report for WP                                                 --- *)
(* -------------------------------------------------------------------------- *)

let ladder = [| 1.0 ; 2.0 ; 3.0 ; 5.0 ; 10.0 ; 15.0 ; 
		20.0 ; 30.0 ; 40.0 ;
		60.0 ; 90.0 ; 120.0 ; 180.0 ;     (* 1', 1'30, 2', 3' *)  
		300.0 ; 600.0 ; 900.0 ; 1800.0 ; (* 5', 10', 15', 30' *)
		3600.0 |]                  (* 1h *)

(* -------------------------------------------------------------------------- *)
(* --- Statistics                                                         --- *)
(* -------------------------------------------------------------------------- *)

type coverage = {
  mutable covered : Property.Set.t ;
  mutable proved : Property.Set.t ;
}

type stats = {
  mutable total : int ;
  mutable valid : int ;
  mutable steps : int ;
  mutable time : float ;
}

let stats () = { total=0 ; valid=0 ; time=0.0 ; steps=0 }
let coverage () = { covered = Property.Set.empty ; proved = Property.Set.empty }

let add_cover s g =
  begin
    let ok,p = Wpo.get_proof g in
    s.covered <- Property.Set.add p s.covered ;
    if ok then s.proved <- Property.Set.add p s.proved ;
  end

let add_vc s ok st tm =
  begin
    s.total <- succ s.total ;
    if ok then s.valid <- succ s.valid ;
    if tm > s.time then s.time <- tm ;
    if st > s.steps then s.steps <- st ;
  end

type pstats = {
  main : stats ;
  coverage : coverage ;
  prover : (VCS.prover,stats) Hashtbl.t ;
}

let pstats () = {
  main = stats () ;
  coverage = coverage () ;
  prover = Hashtbl.create 7 ;
}

type entry = 
  | Lem of string
  | Fun of Kernel_function.t

module Smap = Map.Make
  (struct
     type t = entry
     let compare s1 s2 =
       match s1 , s2 with
	 | Lem a , Lem b -> String.compare a b
	 | Lem _ , Fun _ -> (-1)
	 | Fun _ , Lem _ -> 1
	 | Fun f , Fun g -> Kernel_function.compare f g
   end)

type fcstat = {
  mutable smap : pstats Smap.t ;
  global : pstats ;
}

(* -------------------------------------------------------------------------- *)
(* --- Computing Statistics                                               --- *)
(* -------------------------------------------------------------------------- *)

let get_section fc s =
  try Smap.find s fc.smap
  with Not_found ->
    let fs = pstats () in
    fc.smap <- Smap.add s fs fc.smap ; fs

let get_prover fs prover =
  try Hashtbl.find fs.prover prover
  with Not_found ->
    let s = stats () in
    Hashtbl.add fs.prover prover s ; s

let add_goal fc wpo =
  begin
    let section = match Wpo.get_index wpo with
      | Wpo.Lemma a -> Lem a
      | Wpo.Function(kf,_) -> Fun kf
    in
    let f = get_section fc section in
    let ok = ref false in
    let tm = ref 0.0 in
    let sm = ref 0 in
    List.iter
      (fun (p,r) ->
	 let fp = get_prover f p in
	 let gp = get_prover fc.global p in
	 let rok = Wpo.is_valid r in
	 let st = Wpo.get_steps r in
	 let tc = Wpo.get_time r in
	 add_vc fp rok st tc ; 
	 add_vc gp rok st tc ;
	 if rok then ok := true ;
	 if tc > !tm then tm := tc ;
	 if st > !sm then sm := st ;
      ) (Wpo.get_results wpo) ;
    add_vc f.main !ok !sm !tm ;
    add_vc fc.global.main !ok !sm !tm ;
    add_cover f.coverage wpo ;
    add_cover fc.global.coverage wpo ;
  end

let fcstat () = 
  let fcstat : fcstat = {
    global = pstats () ;
    smap = Smap.empty ;
  } in
  Wpo.iter ~on_goal:(add_goal fcstat) () ;
  fcstat

(* -------------------------------------------------------------------------- *)
(* --- Rendering Numbers                                                  --- *)
(* -------------------------------------------------------------------------- *)

type config = {
  mutable funct : string ;
  mutable lemma : string ;
  mutable console : bool ;
  mutable zero : string ;
}

let percent config fmt s = 
  if s.total = 0 
  then 
    if config.console
    then Format.fprintf fmt "%4s" config.zero
    else Format.pp_print_string fmt config.zero
  else
    if s.total = s.valid then
      Format.pp_print_string fmt (if config.console then " 100" else "100")
    else
      let ratio = float_of_int s.valid /. float_of_int s.total in
      Format.fprintf fmt "%4.1f" (100.0 *. ratio)
    
let number config fmt k = 
  if config.console then
    if k = 0
    then Format.fprintf fmt "%4s" config.zero
    else Format.fprintf fmt "%4d" k
  else
    if k = 0 
    then Format.pp_print_string fmt config.zero
    else Format.pp_print_int fmt k

let properties config fmt (s:coverage) = function
  | "" -> percent config fmt
      { 
	total = Property.Set.cardinal s.covered ;
	valid = Property.Set.cardinal s.proved ;
	steps = 0 ; time = 0.0 ; (* dummies *)
      }
  | "total" -> number config fmt (Property.Set.cardinal s.covered)
  | "valid" -> number config fmt (Property.Set.cardinal s.proved)
  | "failed" -> number config fmt (Property.Set.cardinal s.covered - Property.Set.cardinal s.proved)
  | _ -> raise Exit

let stat config fmt s = function
  | "total" -> number config fmt s.total
  | "valid" | "" -> number config fmt s.valid
  | "failed" -> number config fmt (s.total - s.valid)
  | "success" -> percent config fmt s
  | "time" -> Rformat.pp_time_range ladder fmt s.time
  | "steps" -> if s.steps > 0 then Format.fprintf fmt "(%d)" s.steps
  | _ -> raise Exit

let pstats config fmt s cmd arg =
  match cmd with
    | "success" -> percent config fmt s.main
    | "prop" -> properties config fmt s.coverage arg
    | "total" -> number config fmt s.main.total
    | "valid" -> number config fmt s.main.valid
    | "failed" -> number config fmt (s.main.total - s.main.valid)
    | "time" -> Rformat.pp_time_range ladder fmt s.main.time
    | "steps" -> if s.main.steps > 0 then Format.fprintf fmt "(%d)" s.main.steps
    | "wp" -> stat config fmt (get_prover s VCS.WP) arg
    | "alt-ergo" | "ergo" -> stat config fmt (get_prover s VCS.AltErgo) arg
    | "coq" -> stat config fmt (get_prover s VCS.Coq) arg
    | "z3" -> stat config fmt (get_prover s (VCS.Why "z3")) arg
    | "simplify" -> stat config fmt (get_prover s (VCS.Why "simplify")) arg
    | "vampire" -> stat config fmt (get_prover s (VCS.Why "vampire")) arg
    | "zenon" -> stat config fmt (get_prover s (VCS.Why "zenon")) arg
    | "cvc3" -> stat config fmt (get_prover s (VCS.Why "cvc3")) arg
    | "yices" -> stat config fmt (get_prover s (VCS.Why "yices")) arg
    | _ -> raise Exit

(* -------------------------------------------------------------------------- *)
(* --- Rformat Environments                                               --- *)
(* -------------------------------------------------------------------------- *)

let env_section config s f fmt cmd arg =
  try
    match cmd with
      | "function" | "name" -> 
	  begin match s with
	    | Lem a -> 
		Format.fprintf fmt "%s%s" config.lemma a
	    | Fun kf -> 
		let f = Kernel_function.get_name kf in
		Format.fprintf fmt "%s%s" config.funct f
	  end
      | _ -> 
	  pstats config fmt f cmd arg
  with Exit ->
    if arg="" 
    then Wp_parameters.error "Unknown function-format '%%%s'" cmd 
    else Wp_parameters.error "Unknown function-format '%%%s:%s'" cmd arg


let env_global config global fmt cmd arg =
  try
    pstats config fmt global cmd arg
  with Exit ->
    if arg="" 
    then Wp_parameters.error "Unknown global-format '%%%s'" cmd 
    else Wp_parameters.error "Unknown global-format '%%%s:%s'" cmd arg

(* -------------------------------------------------------------------------- *)
(* --- Statistics Printing                                                --- *)
(* -------------------------------------------------------------------------- *)

let print fcstat config head fcts tail fmt =
  begin
    if head <> "" then
      Rformat.pretty (env_global config fcstat.global) fmt head ;
    if fcts <> "" then
      begin
	Smap.iter
	  (fun s f ->
	     Rformat.pretty (env_section config s f) fmt fcts ;
	  ) fcstat.smap
      end ;
    if tail <> "" then
      Rformat.pretty (env_global config fcstat.global) fmt tail ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Report Printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

type section = HEAD | FCTS | TAIL | END

let export fcstat specfile =
  let config = {
    console = false ; 
    zero = "-" ;
    funct = "" ;
    lemma = "(Lem) " ;
  } in
  let file = ref None in
  let head = Buffer.create 128 in
  let fcts = Buffer.create 128 in
  let tail = Buffer.create 128 in
  let section = ref HEAD in
  begin
    let cin = open_in specfile in
    try
      while true do
	let line = input_line cin in
	match Rformat.command line with
	  | Rformat.ARG("LEMPREFIX",f) -> config.lemma <- f
	  | Rformat.ARG("FUNPREFIX",f) -> config.funct <- f 
	  | Rformat.ARG("ZERO",z) -> config.zero <- z
	  | Rformat.ARG("FILE",f) -> file := Some f
	  | Rformat.ARG("SUFFIX",e) -> 
	      let basename = Wp_parameters.ReportName.get () in
	      let filename = basename ^ e in
	      file := Some filename
	  | Rformat.CMD "CONSOLE" -> config.console <- true
	  | Rformat.CMD "HEAD" -> section := HEAD
	  | Rformat.CMD "FUNCTION" -> section := FCTS
	  | Rformat.CMD "TAIL" -> section := TAIL
	  | Rformat.CMD "END" -> section := END
	  | Rformat.CMD a | Rformat.ARG(a,_) -> 
	      Wp_parameters.error "Report '%s': unknown command '%s'" specfile a
	  | Rformat.TEXT ->
	      if !section <> END then
		let text = match !section with HEAD -> head | TAIL|END -> tail | FCTS -> fcts in
		Buffer.add_string text line ;
		Buffer.add_char text '\n' ;
      done
    with 
      | End_of_file -> close_in cin
      | err -> close_in cin ; raise err
  end ;
  match !file with
    | None ->
	Log.print_on_output
	  (print fcstat config
	     (Buffer.contents head)
	     (Buffer.contents fcts)
	     (Buffer.contents tail))
    | Some report ->
	Wp_parameters.feedback "Report '%s'" report ;
	let cout = open_out report in
	let fout = Format.formatter_of_out_channel cout in
	try
	  print fcstat config
	    (Buffer.contents head)
	    (Buffer.contents fcts)
	    (Buffer.contents tail) 
	    fout ;
	  Format.pp_print_flush fout () ;
	  close_out cout ;
	with err ->
	  Format.pp_print_flush fout () ;
	  close_out cout ;
	  raise err

(* -------------------------------------------------------------------------- *)
