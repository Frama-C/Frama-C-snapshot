(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Fast Report for WP                                                 --- *)
(* -------------------------------------------------------------------------- *)

let ladder = [| 1.0 ; 2.0 ; 3.0 ; 5.0 ; 10.0 ; 15.0 ; 
		20.0 ; 30.0 ; 40.0 ;
		60.0 ; 90.0 ; 120.0 ; 180.0 ;    (* 1', 1'30, 2', 3' *)  
		300.0 ; 600.0 ; 900.0 ; 1800.0 ; (* 5', 10', 15', 30' *)
		3600.0 |]                        (* 1h *)

(* -------------------------------------------------------------------------- *)
(* --- Statistics                                                         --- *)
(* -------------------------------------------------------------------------- *)

type res = VALID | UNSUCCESS | INCONCLUSIVE | NORESULT

let result (r:VCS.result) = match r.VCS.verdict with
  | VCS.NoResult | VCS.Computing _ -> NORESULT
  | VCS.Failed -> INCONCLUSIVE
  | VCS.Invalid | VCS.Unknown | VCS.Timeout | VCS.Stepout -> UNSUCCESS
  | VCS.Valid -> VALID
      
let best_result a b = match a,b with
  | NORESULT,c | c,NORESULT -> c
  | VALID,_ | _,VALID -> VALID
  | UNSUCCESS,_ | _,UNSUCCESS -> UNSUCCESS
  | INCONCLUSIVE,INCONCLUSIVE -> INCONCLUSIVE

type stats = {
  mutable valid : int ; (* Result is Valid *)
  mutable unsuccess : int ; (* verdict is NoResult, Unknown, Timeout, or Stepout, Invalid *)
  mutable inconclusive : int ; (* verdict is Failed *)
  mutable total : int ; (* valid + unsuccess + inconclusive *)
  mutable steps : int ;
  mutable time : float ;
}

let stats () = { total=0 ; valid=0 ; unsuccess=0 ; inconclusive=0 ; steps=0 ; time=0.0 }

let add_stat (r:res) (st:int) (tm:float) (s:stats) =
  begin
    s.total <- succ s.total ;
    match r with
      | VALID -> 
	  if tm > s.time then s.time <- tm ;
	  if st > s.steps then s.steps <- st ;
	  s.valid <- succ s.valid
      | NORESULT | UNSUCCESS -> s.unsuccess <- succ s.unsuccess
      | INCONCLUSIVE -> s.inconclusive <- succ s.inconclusive
  end

let add_qedstat (ts:float) (s:stats) =
  if ts > s.time then s.time <- ts

(* -------------------------------------------------------------------------- *)
(* --- Stats by Prover                                                    --- *)
(* -------------------------------------------------------------------------- *)

type pstats = {
  main : stats ;
  prover : (VCS.prover,stats) Hashtbl.t ;
}

let pstats () = {
  main = stats () ;
  prover = Hashtbl.create 7 ;
}

let get_prover fs prover =
  try Hashtbl.find fs.prover prover
  with Not_found ->
    let s = stats () in
    Hashtbl.add fs.prover prover s ; s

let add_results (plist:pstats list) (wpo:Wpo.t) =
  let ok = ref NORESULT in
  let tm = ref 0.0 in
  let sm = ref 0 in
  List.iter
    (fun (p,r) ->
       let re = result r in
       let st = Wpo.get_steps r in
       let tc = Wpo.get_time r in
       let ts = r.VCS.solver_time in
       if re <> NORESULT then
	 begin
	   List.iter 
	     (fun fs -> add_stat re st tc (get_prover fs p))
	     plist ;
	   if p <> VCS.Qed && ts > 0.0 then
	     List.iter 
	       (fun fs -> add_qedstat ts (get_prover fs VCS.Qed))
	       plist ;
	 end ;
       ok := best_result !ok re ;
       if tc > !tm then tm := tc ;
       if st > !sm then sm := st ;
    ) (Wpo.get_results wpo) ;
  List.iter (fun fs -> add_stat !ok !sm !tm fs.main) plist

(* -------------------------------------------------------------------------- *)
(* --- Stats by Section                                                   --- *)
(* -------------------------------------------------------------------------- *)

type coverage = {
  mutable covered : Property.Set.t ;
  mutable proved : Property.Set.t ;
}

let coverage () = { covered = Property.Set.empty ; proved = Property.Set.empty }

let add_cover (s:coverage) ok p =
  begin
    s.covered <- Property.Set.add p s.covered ;
    if ok then s.proved <- Property.Set.add p s.proved ;
  end

type dstats = {
  dstats : pstats ;
  dcoverage : coverage ;
  mutable dmap : pstats Property.Map.t ;
}

let dstats () = {
  dstats = pstats () ;
  dcoverage = coverage () ;
  dmap = Property.Map.empty ;
}

(* -------------------------------------------------------------------------- *)
(* --- Stats WP                                                           --- *)
(* -------------------------------------------------------------------------- *)

type entry = 
  | Global of string (* [JS 2012/11/27] unused *)
  | Axiom of string
  | Fun of Kernel_function.t

let decode_chapter= function
  | Global _-> "global"
  | Axiom _ -> "axiomatic"
  | Fun _   -> "function"

module Smap = FCMap.Make
  (struct
     type t = entry
     let compare s1 s2 =
       match s1 , s2 with
	 | Global a, Global b -> String.compare a b
	 | Global _, _ -> (-1)
	 | _ , Global _ -> 1
	 | Axiom a , Axiom b -> String.compare a b
	 | Axiom _ , Fun _ -> (-1)
	 | Fun _ , Axiom _ -> 1
	 | Fun f , Fun g -> Kernel_function.compare f g
   end)

type fcstat = {
  global : pstats ;
  gcoverage : coverage ;
  mutable dsmap : dstats Smap.t ;
}

(* -------------------------------------------------------------------------- *)
(* --- Computing Statistics                                               --- *)
(* -------------------------------------------------------------------------- *)

let get_section gs s =
  try Smap.find s gs.dsmap
  with Not_found ->
    let ds = dstats () in
    gs.dsmap <- Smap.add s ds gs.dsmap ; ds

let get_property ds p =
  try Property.Map.find p ds.dmap
  with Not_found ->
    let ps = pstats () in
    ds.dmap <- Property.Map.add p ps ds.dmap ; ps

let add_goal (gs:fcstat) wpo =
  begin
    let section = match Wpo.get_index wpo with
      | Wpo.Axiomatic None -> Axiom ""
      | Wpo.Axiomatic (Some a) -> Axiom a
      | Wpo.Function(kf,_) -> Fun kf
    in
    let ds : dstats = get_section gs section in
    let (ok,prop) = Wpo.get_proof wpo in
    let ps : pstats = get_property ds prop in  
    add_results [gs.global ; ds.dstats ; ps] wpo ;
    add_cover gs.gcoverage ok prop ;
    add_cover ds.dcoverage ok prop ;
  end

let fcstat () = 
  let fcstat : fcstat = {
    global = pstats () ;
    gcoverage = coverage () ;
    dsmap = Smap.empty ;
  } in
  Wpo.iter ~on_goal:(add_goal fcstat) () ;
  fcstat

(* -------------------------------------------------------------------------- *)
(* --- Iteration on Stats                                                 --- *)
(* -------------------------------------------------------------------------- *)

type istat = {
  fcstat: fcstat;
  chapters : (string * (entry * dstats) list) list;
}

(** start chapter stats *)    
let start_stat4chap fcstat =
  let chapter = ref "" in
  let decode_chapter e =
    let code = decode_chapter e in
    let is_new_code = (code <> !chapter) in
      if is_new_code then
	chapter := code;
      is_new_code
  in 
  let close_chapter (na,ca,ga) =
    if ca = [] then !chapter,[],ga
    else !chapter,[],((na,List.rev ca)::ga) 
  in
  let (_,_,ga) = 
    let acc = 
      Smap.fold
	(fun entry ds acc ->
	   let is_new_chapter = decode_chapter entry in
	   let (na,ca,ga) = if is_new_chapter
           then close_chapter acc
           else acc in
	     na,((entry,ds)::ca),ga
	) fcstat.dsmap ("",[],[])
    in if !chapter <> "" then close_chapter acc
      else acc
  in if ga = [] then None
    else Some { fcstat = fcstat;
		chapters = List.rev ga;
	      }

(** next chapters stats *)
let next_stat4chap istat = 
  match istat.chapters with
   | ([] | _::[]) -> None
   | _::l -> Some { istat with chapters = l }
	
type cistat = {
  cfcstat: fcstat;
  chapter : string; 
  sections : (entry * dstats) list;
}

(** start section stats of a chapter*)
let start_stat4sect istat = 
  match istat.chapters with
  | [] -> None
  | (c,s)::_ -> Some { cfcstat = istat.fcstat;
		       chapter = c; 
		       sections = s;
		     }
	
(** next section stats *)
let next_stat4sect cistat = 
  match cistat.sections with
  | ([] | _::[]) -> None
  | _::l -> Some { cistat with sections = l }
  
type sistat = {
  sfcstat: fcstat;
  schapter : string ;
  section : (entry * dstats);
  properties : (Property.t * pstats) list;
}

(** start property stats of a section *)
let start_stat4prop cistat = 
  match cistat.sections with
  | [] -> None
  | ((_,ds) as s)::_ -> 
      Some { sfcstat = cistat.cfcstat;
	     schapter = cistat.chapter;
	     section = s;
	     properties = List.rev (Property.Map.fold 
                                      (fun p ps acc -> (p,ps)::acc) ds.dmap []);
	   }

(** next property stats *)
let next_stat4prop sistat = 
  match sistat.properties with
  | ([] | _::[]) -> None
  | _::l -> Some { sfcstat = sistat.sfcstat;
		   schapter = sistat.schapter;
		   section = sistat.section;
		   properties = l;
		   }

(** generic iterator *)
let iter_stat ?first ?sep ?last ~from start next=
  if first<>None || sep<>None || last <> None then
    let items = ref (start from) in
    if !items <> None then
    begin
      let apply v = function
	| None -> ()
	| Some app -> app v 
      in
      let next app = 
        let item = (Extlib.the !items) in
        apply item app;
        items := next item
      in 
      next first;
      if sep<>None || last <> None then
      begin
        while !items <> None do
          next sep;
        done;
        apply () last;
     end
   end

(* -------------------------------------------------------------------------- *)
(* --- Rendering Numbers                                                  --- *)
(* -------------------------------------------------------------------------- *)

type config = {
  mutable status_passed : string ;
  mutable status_failed : string ;
  mutable status_inconclusive : string ;
  mutable status_untried : string ;

  mutable global_prefix : string ;
  mutable lemma_prefix : string ;
  mutable axiomatic_prefix : string ;
  mutable function_prefix : string ;
  mutable property_prefix : string ;

  mutable global_section: string ;
  mutable axiomatic_section: string ;
  mutable function_section : string ;
  mutable console : bool ;
  mutable zero : string ;
}

let pp_zero ~config fmt =
  if config.console
  then Format.fprintf fmt "%4s" config.zero
  else Format.pp_print_string fmt config.zero

let percent ~config fmt number total = 
  if total <= 0 || number < 0
  then pp_zero ~config fmt
  else
    if number >= total then
      Format.pp_print_string fmt (if config.console then " 100" else "100")
    else
      let ratio = float_of_int number /. float_of_int total in
      Format.fprintf fmt "%4.1f" (100.0 *. ratio)
	
let number ~config fmt k = 
  if k = 0
  then pp_zero ~config fmt
  else 
    if config.console 
    then Format.fprintf fmt "%4d" k
    else Format.pp_print_int fmt k

let properties ~config fmt (s:coverage) = function
  | "" -> percent config fmt (Property.Set.cardinal s.proved) (Property.Set.cardinal s.covered)
  | "total" -> number config fmt (Property.Set.cardinal s.covered)
  | "valid" -> number config fmt (Property.Set.cardinal s.proved)
  | "failed" -> number config fmt (Property.Set.cardinal s.covered - Property.Set.cardinal s.proved)
  | _ -> raise Exit

let stat ~config fmt s = function
  | "success" -> percent config fmt s.valid s.total
  | "total" -> number config fmt s.total
  | "valid" | "" -> number config fmt s.valid
  | "failed" -> number config fmt (s.unsuccess + s.inconclusive)
  | "status" -> 
      let msg = 
	if s.inconclusive > 0 then config.status_inconclusive else
	  if s.unsuccess > 0 then config.status_failed else
	    if s.valid >= s.total then config.status_passed else
	      config.status_untried
      in Format.pp_print_string fmt msg
  | "inconclusive" -> number config fmt s.inconclusive
  | "unsuccess" -> number config fmt s.unsuccess
  | "time" -> 
      if s.time > 0.0 then
	Rformat.pp_time_range ladder fmt s.time
  | "perf" -> 
      if s.time > Rformat.epsilon then 
	Format.fprintf fmt "(%a)" Rformat.pp_time s.time
  | "steps" -> 
      if s.steps > 0 then Format.fprintf fmt "(%d)" s.steps
  | _ -> raise Exit

let pstats ~config fmt s cmd arg =
  match cmd with
    | "wp" | "qed" -> stat ~config fmt (get_prover s VCS.Qed) arg
    | "alt-ergo" | "ergo" -> stat ~config fmt (get_prover s VCS.AltErgo) arg
    | "coq" -> stat ~config fmt (get_prover s VCS.Coq) arg
    | "z3" -> stat ~config fmt (get_prover s (VCS.Why3 "z3")) arg
    | "gappa" -> stat ~config fmt (get_prover s (VCS.Why3 "gappa")) arg
    | "simplify" -> stat ~config fmt (get_prover s (VCS.Why3 "simplify")) arg
    | "vampire" -> stat ~config fmt (get_prover s (VCS.Why3 "vampire")) arg
    | "zenon" -> stat ~config fmt (get_prover s (VCS.Why3 "zenon")) arg
    | "cvc3" -> stat ~config fmt (get_prover s (VCS.Why3 "cvc3")) arg
    | "yices" -> stat ~config fmt (get_prover s (VCS.Why3 "yices")) arg
    | _ -> stat ~config fmt s.main cmd

let pcstats ~config fmt (s,c) cmd arg =
  match cmd with
    | "prop" -> properties ~config fmt c arg
    | _ -> pstats ~config fmt s cmd arg

(* -------------------------------------------------------------------------- *)
(* --- Rformat Environments                                               --- *)
(* -------------------------------------------------------------------------- *)

let env_toplevel ~config gstat fmt cmd arg =
  try
    pcstats config fmt (gstat.global, gstat.gcoverage) cmd arg
  with Exit ->
    if arg="" 
    then Wp_parameters.error ~once:true "Unknown toplevel-format '%%%s'" cmd 
    else Wp_parameters.error ~once:true "Unknown toplevel-format '%%%s:%s'" cmd arg

let env_chapter chapter_name fmt cmd arg =
  try
    match cmd with
      | "chapter" | "name"  ->
	  Format.pp_print_string fmt chapter_name	      
      | _ -> raise Exit
  with Exit ->
    if arg="" 
    then Wp_parameters.error ~once:true "Unknown chapter-format '%%%s'" cmd 
    else Wp_parameters.error ~once:true "Unknown chapter-format '%%%s:%s'" cmd arg
		       
let env_section ~config ~name sstat fmt cmd arg =
  try
    let entry,ds = match sstat.sections with
      | section_item::_others -> section_item
      | _ -> raise Exit
    in match cmd with
      | "chapter" -> 
	  let chapter = match entry with
	    | Global _ -> config.global_section
	    | Axiom _ -> config.axiomatic_section
	    | Fun _ ->  config.function_section
	  in Format.pp_print_string fmt chapter
      | "name" | "section" | "global" | "axiomatic" | "function" -> 
	  if cmd <> "name" &&  cmd <> "section" && name <> cmd then
	    Wp_parameters.error "Invalid section-format '%%%s' inside a section %s" cmd name;
	  let prefix,name = match entry with
	    | Global a->  config.global_prefix, a
	    | Axiom "" -> config.lemma_prefix,""
	    | Axiom a -> config.axiomatic_prefix,a
	    | Fun kf -> config.function_prefix, ( Kernel_function.get_name kf)
	  in Format.fprintf fmt "%s%s" prefix name
      | _ -> 
	  pcstats config fmt (ds.dstats, ds.dcoverage) cmd arg
  with Exit ->
    if arg="" 
    then Wp_parameters.error ~once:true "Unknown section-format '%%%s'" cmd 
    else Wp_parameters.error ~once:true "Unknown section-format '%%%s:%s'" cmd arg

let env_property ~config ~name pstat fmt cmd arg =
  try
    let entry = fst pstat.section in
    let p,stat = match pstat.properties with
      | property_item::_others -> property_item
      | _ -> raise Exit
    in match cmd with
      | "chapter" -> 
	  let chapter = match entry with
	    | Global _ -> config.global_section
	    | Axiom _ -> config.axiomatic_section
	    | Fun _ ->  config.function_section
	  in Format.pp_print_string fmt chapter
      | "section" | "global" | "axiomatic" | "function" -> 
 	  if cmd <> "section" && name <> cmd then
	    Wp_parameters.error "Invalid property-format '%%%s' inside a section %s" cmd name;
	  let prefix,name = match entry with
	    | Global a->  config.global_prefix, a
 	    | Axiom "" -> config.lemma_prefix,""
	    | Axiom a -> config.axiomatic_prefix,a
 	    | Fun kf -> config.function_prefix, ( Kernel_function.get_name kf)
 	  in Format.fprintf fmt "%s%s" prefix name
      | "name" -> 
	  Format.fprintf fmt "%s%s" config.property_prefix 
	    (Property.Names.get_prop_name_id p)
      | "property" ->
	  Description.pp_local fmt p
      | _ -> 
	  pstats config fmt stat cmd arg
  with Exit ->
    if arg="" 
    then Wp_parameters.error ~once:true "Unknown property-format '%%%s'" cmd 
    else Wp_parameters.error ~once:true "Unknown property-format '%%%s:%s'" cmd arg

(* -------------------------------------------------------------------------- *)
(* --- Statistics Printing                                                --- *)
(* -------------------------------------------------------------------------- *)

let print_property (pstat:sistat) ~config ~name ~prop fmt =
  Rformat.pretty (env_property ~config ~name pstat) fmt prop

let print_section (sstat:cistat) ~config ~name ~sect ~prop fmt =
  if sect <> "" then
    Rformat.pretty (env_section ~config ~name sstat) fmt sect ;
  if prop <> "" then
    let print_property pstat = print_property pstat ~config ~name ~prop fmt
    in iter_stat ~first:print_property ~sep:print_property ~from:sstat start_stat4prop next_stat4prop

let print_chapter (cstat:istat) ~config ~chap ~sect ~glob ~axio ~func ~prop fmt =
  let chapter_item = match cstat.chapters with
    | chapter_item::_others -> chapter_item
    | _ -> raise Exit
  in let section_name = fst chapter_item
  in let section,chapter_name = match section_name with
    | "global"    -> glob,config.global_section
    | "axiomatic" -> axio,config.axiomatic_section
    | "function"  -> func,config.function_section
    | _ -> sect,""
  in let section,section_name = if section <> "" then section,section_name else sect,""
  in
  if chap <> "" then
    Rformat.pretty (env_chapter chapter_name) fmt chap ;
  if section <> "" || prop <> "" then
    let print_section sstat = print_section sstat ~config ~name:section_name ~sect:section ~prop fmt
    in iter_stat ~first:print_section ~sep:print_section ~from:cstat start_stat4sect next_stat4sect

let print gstat ~config ~head ~tail ~chap ~sect ~glob ~axio ~func ~prop fmt =
  begin
    if head <> "" then
      Rformat.pretty (env_toplevel ~config gstat) fmt head ;
    if chap <> "" || sect <> "" || glob <> "" || axio <> "" || func <> "" || prop <> "" then
      let print_chapter cstat = print_chapter cstat ~config ~chap ~sect ~glob ~axio ~func ~prop fmt
      in iter_stat ~first:print_chapter ~sep:print_chapter ~from:gstat start_stat4chap next_stat4chap ;
    if tail <> "" then
      Rformat.pretty (env_toplevel ~config gstat) fmt tail ;
  end
  
(* -------------------------------------------------------------------------- *)
(* --- Report Printing                                                    --- *)
(* -------------------------------------------------------------------------- *)

type section = END | HEAD | TAIL
	     | CHAPTER
             | SECTION | GLOB_SECTION | AXIO_SECTION | FUNC_SECTION
             | PROPERTY

let export gstat specfile =
  let config = {
    console = false ; 
    zero = "-" ;
    
    status_passed = "  Ok  " ;
    status_failed = "Failed" ;
    status_inconclusive = "*Bug**" ;
    status_untried = "     " ;

    lemma_prefix = "Lemma " ;
    global_prefix = "(Global) " ;
    axiomatic_prefix = "Axiomatic " ;
    function_prefix = "" ;
    property_prefix = "" ;

    global_section = "Globals" ;
    axiomatic_section = "Axiomatics" ;
    function_section = "Functions" ;
 
  } in
  let head = Buffer.create 64 in
  let tail = Buffer.create 64 in
  let chap = Buffer.create 64 in (* chapter *)
  let sect = Buffer.create 64 in (* default section *)
  let glob = Buffer.create 64 in (* section *)
  let axio = Buffer.create 64 in (* section *)
  let func = Buffer.create 64 in (* section *)
  let sect_prop = Buffer.create 64 in (* default sub-section *)
  let file = ref None in
  let section = ref HEAD in
  begin
    let cin = open_in specfile in
    try
      while true do
	let line = input_line cin in
	match Rformat.command line with
	  | Rformat.ARG("AXIOMATIC_PREFIX",f) -> config.axiomatic_prefix <- f
	  | Rformat.ARG("FUNCTION_PREFIX",f) -> config.function_prefix <- f 
	  | Rformat.ARG("PROPERTY_PREFIX",f) -> config.property_prefix <- f	     
	  | Rformat.ARG("LEMMA_PREFIX",f) -> config.lemma_prefix <- f 
 
	  | Rformat.ARG("GLOBAL_SECTION",f) -> config.global_section <- f
	  | Rformat.ARG("AXIOMATIC_SECTION",f) -> config.axiomatic_section <- f
	  | Rformat.ARG("FUNCTION_SECTION",f) -> config.function_section <- f 

	  | Rformat.ARG("PASSED",s) -> config.status_passed <- s
	  | Rformat.ARG("FAILED",s) -> config.status_failed <- s
	  | Rformat.ARG("INCONCLUSIVE",s) -> config.status_inconclusive <- s
	  | Rformat.ARG("UNTRIED",s) -> config.status_untried <- s

	  | Rformat.ARG("ZERO",z) -> config.zero <- z
	  | Rformat.ARG("FILE",f) -> file := Some f
	  | Rformat.ARG("SUFFIX",e) -> 
	      let basename = Wp_parameters.ReportName.get () in
	      let filename = basename ^ e in
	      file := Some filename
	  | Rformat.CMD "CONSOLE" -> config.console <- true

	  | Rformat.CMD "END" -> section := END
	  | Rformat.CMD "HEAD" -> section := HEAD
	  | Rformat.CMD "TAIL" -> section := TAIL

	  | Rformat.CMD "CHAPTER" -> section := CHAPTER

	  | Rformat.CMD "SECTION" -> section := SECTION
	  | Rformat.CMD "GLOBAL" -> section := GLOB_SECTION
	  | Rformat.CMD "AXIOMATIC" -> section := AXIO_SECTION
	  | Rformat.CMD "FUNCTION" -> section := FUNC_SECTION

	  | Rformat.CMD "PROPERTY" -> section := PROPERTY

	  | Rformat.CMD a | Rformat.ARG(a,_) -> 
	      Wp_parameters.error "Report '%s': unknown command '%s'" specfile a
	  | Rformat.TEXT ->
	      if !section <> END then
		let text = match !section with
		  | HEAD      -> head
                  | CHAPTER   -> chap
		  | SECTION   -> sect
		  | GLOB_SECTION -> glob
		  | AXIO_SECTION -> axio
		  | FUNC_SECTION -> func
		  | PROPERTY -> sect_prop
		  | TAIL|END  -> tail 
		  in
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
	  (print gstat ~config
	     ~head:(Buffer.contents head) ~tail:(Buffer.contents tail)
	     ~chap:(Buffer.contents chap)
	     ~sect:(Buffer.contents sect)
	     ~glob:(Buffer.contents glob)
	     ~axio:(Buffer.contents axio)
	     ~func:(Buffer.contents func)
	     ~prop:(Buffer.contents sect_prop))
    | Some report ->
	Wp_parameters.feedback "Report '%s'" report ;
	let cout = open_out report in
	let fout = Format.formatter_of_out_channel cout in
	try
	  print gstat ~config
	    ~head:(Buffer.contents head) ~tail:(Buffer.contents tail) 
	    ~chap:(Buffer.contents chap)
	    ~sect:(Buffer.contents sect)
	    ~glob:(Buffer.contents glob)
	    ~axio:(Buffer.contents axio)
	    ~func:(Buffer.contents func)
	    ~prop:(Buffer.contents sect_prop)
	    fout ;
	  Format.pp_print_flush fout () ;
	  close_out cout ;
	with err ->
	  Format.pp_print_flush fout () ;
	  close_out cout ;
	  raise err

(* -------------------------------------------------------------------------- *)
