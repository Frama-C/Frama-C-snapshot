(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)


type goal = {
  goal_expl : Logic_decl.vc_expl;
  goal_file : string;
  sub_goal : goal list;
  proof : (string*string*string*string*string) list ;
  mutable goal_tags : (string*string) list; 
}

type lemma = {
  lemma_name : string;
  lemma_loc : Loc.floc;
  lemma_goal : goal;
  mutable lemma_tags : (string*string) list; 
}

type behavior = {
  behavior_name : string;
  behavior_loc : Loc.floc;
  mutable behavior_goals : goal list;
  mutable behavior_tags : (string*string) list;
}

type funct = {
  function_name : string;
  function_loc : Loc.floc;
  mutable function_behaviors : behavior list;
  mutable function_tags : (string*string) list;
}
  

type t = {
  project_name : string;
  mutable project_context_file : string;
  mutable project_lemmas : lemma list;
  mutable project_functions : funct list;
}

let create n =
  {
    project_name = n;
    project_context_file = "";
    project_lemmas = [];
    project_functions = [];
  }

let set_project_context_file p f =
  p.project_context_file <- f

let add_lemma p n e f =
  let g = {
    goal_expl = e; 
    goal_file = f;
    sub_goal = [];
    proof = [];
    goal_tags = [];
  }
  in
  let l = {
    lemma_name = n;
    lemma_loc = Loc.dummy_floc;
    lemma_goal = g;
    lemma_tags = [];
  }
  in p.project_lemmas <- l :: p.project_lemmas;
  l

let add_goal b e f =
  let g = {
    goal_expl = e; 
    goal_file = f;
    sub_goal = [];
    proof = [];
    goal_tags = [];
  }
  in
  b.behavior_goals <- g :: b.behavior_goals;
  g

let add_behavior f be floc =
  let b = {
    behavior_name = be;
    behavior_loc = floc;
    behavior_goals = [];
    behavior_tags = [];
  }
  in
  f.function_behaviors <- b :: f.function_behaviors;
  b

let add_function p fname floc =
  let f = {
    function_name = fname;
    function_loc = floc;
    function_behaviors = [];
    function_tags = [];
  }
  in
  p.project_functions <- f :: p.project_functions;
  f


(* toggle *)

let toggle_lemma l = l.lemma_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value)) l.lemma_tags

let toggle_function f = f.function_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value)) f.function_tags

let toggle_behavior b = b.behavior_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value))  b.behavior_tags

let toggle_goal g = g.goal_tags <- 
  List.map (fun (key,value) ->  
	      if key = "ww_open" then 
		if value = "true" then (key,"false") 
		else (key,"true") 
	      else
		(key,value))  g.goal_tags

(* saving *)

open Format
open Logic
open Logic_decl

let pr_goal fmt g =
  fprintf fmt "    <goal why_file=\"%s\">@." g.goal_file;
  fprintf fmt "      <location %a/>@."
    (fun fmt -> Explain.raw_loc ~quote:true fmt) (g.goal_expl.vc_loc);
  fprintf fmt "      <explain %a/>@."
    (fun fmt -> Explain.print ~quote:true fmt) (g.goal_expl);
  fprintf fmt "    </goal>@."

let save p file =
  let ch = open_out (file ^ ".wpr") in
  let fpr = formatter_of_out_channel ch in
  fprintf fpr "<project name=\"%s\" context=\"%s\">@." 
    p.project_name p.project_context_file;
  List.iter
    (fun l -> (* name (floc,loc,expl,fpo) -> *)
       fprintf fpr "  <lemma name=\"%s\">@." l.lemma_name;
       fprintf fpr "    <location %a/>@." 
	 (fun fmt -> Explain.raw_loc ~quote:true fmt) l.lemma_loc;
       pr_goal fpr l.lemma_goal;
    fprintf fpr "  </lemma>@.")
    p.project_lemmas;
  List.iter
    (fun f -> (* name (floc,behs) -> *)
       fprintf fpr "  <function name=\"%s\">@."  f.function_name;
       fprintf fpr "    <location %a/>@." 
	 (fun fmt -> Explain.raw_loc ~quote:true fmt) f.function_loc;
       List.iter
	 (fun b -> (*vcs -> *)
	  fprintf fpr "    <behavior name=\"%s\">@." b.behavior_name;
	    List.iter (pr_goal fpr) b.behavior_goals;
	      fprintf fpr "    </behavior>@." )
	 f.function_behaviors;
    fprintf fpr "  </function>@.")
    p.project_functions;
  fprintf fpr "</project>@.";
  close_out ch
      
(* loading *)

let get_string_attr a e =
  try
    match List.assoc a e.Xml.attributes with
      | Rc.RCstring s -> s
      | Rc.RCident n -> n
      | _ -> raise Not_found
  with Not_found -> 
    eprintf "Warning: attribute `%s' not found@." a;
    ""
  

let get_name_attr = get_string_attr "name"

let get_int_attr a e =
  match List.assoc a e.Xml.attributes with
    | Rc.RCstring s -> int_of_string s
    | Rc.RCint n -> n
    | _ -> raise Not_found

let get_bool_attr a e =
  try
    match List.assoc a e.Xml.attributes with
      | Rc.RCstring "true" -> true
      | Rc.RCstring "false" -> false
      | Rc.RCint n -> n <> 0
      | Rc.RCbool b -> b
      | _ -> false
  with Not_found -> false

let get_tags l = 
  let ww_open = ref false in
  let rec get_tag tags l =
    match l with 
      | e::l when e.Xml.name = "tag" -> 
	  let key = get_string_attr "key" e in
	  let value = get_string_attr "value" e in
	  if key = "ww_open" then ww_open := true else ();
	  get_tag ((key,value)::tags) l
      | _ -> tags,l
  in
  let (tags,l) = get_tag [] l in
  if !ww_open then  tags,l else (("ww_open","false")::tags,l)

let get_loc e =
  let file = get_string_attr "file" e in
  let line = get_int_attr "line" e in
  let be = get_int_attr "begin" e in
  let en = get_int_attr "end" e in
  (file,line,be,en)

let get_kind e =
  let k = get_string_attr "kind" e in
  match k with
    | "Lemma" -> Logic_decl.EKLemma
    | "Other" -> Logic_decl.EKOther (get_string_attr "text" e)
    | "Absurd" -> Logic_decl.EKAbsurd 
    | "Assert" -> Logic_decl.EKAssert 
    | "Pre" -> Logic_decl.EKPre (get_string_attr "text" e)
    | "Post" -> Logic_decl.EKPost 
    | "WfRel" -> Logic_decl.EKWfRel
    | "VarDecr" -> Logic_decl.EKVarDecr 
    | "LoopInvInit" -> 
	let f =
	  try get_string_attr "formula" e
	  with Not_found -> ""
	in
	Logic_decl.EKLoopInvInit f
    | "LoopInvPreserv" -> 
	let f =
	  try get_string_attr "formula" e
	  with Not_found -> ""
	in
	Logic_decl.EKLoopInvPreserv f
    | _ -> Logic_decl.EKOther ("Project: unrecognized kind " ^ k)


let get_proof elements =
  let rec get_proofs proofs l =
    match l with 
      | e::l when e.Xml.name = "proof" -> 
	  let prover = get_string_attr "prover" e in
	  let status = get_string_attr "status" e in
	  let timelimit = 
	    try get_string_attr "timelimit" e with Not_found -> "no timeout"
	  in
	  let date =  
	    try get_string_attr "date" e  with Not_found -> "no date" 
	  in
	  let scriptfile = 
	    try get_string_attr "scriptfile" e with Not_found -> "no script" 
	  in
	  get_proofs ((prover,status,timelimit,date,scriptfile)::proofs) l
      | _ -> proofs,l
  in
  (get_proofs [] elements)

let rec get_goal lf beh e =
  match e.Xml.name with
    | "goal" ->
	let (tags, elements) = get_tags e.Xml.elements in
	let loc = List.hd elements in
	let elements = List.tl elements in
	let loc = get_loc loc in
	let k = get_kind (List.hd elements) in 
	let expl =
	  {
	    Logic_decl.lemma_or_fun_name = lf;
	    Logic_decl.behavior = beh;
	    Logic_decl.vc_loc = loc;
	    Logic_decl.vc_kind = k;
	  }
	in
	let (proofs,elements) = get_proof (List.tl elements) in
	let sub_goals =  List.map (get_goal lf beh) elements in
	{ goal_expl = expl;
	  goal_file = get_string_attr "why_file" e;
	  sub_goal = sub_goals;
	  proof = proofs;
	  goal_tags = tags;
	}
    | _ -> assert false
	
let get_behavior lf loc e  =
  match e.Xml.name with
    | "behavior" ->
	let n = get_name_attr e in
	let (tags,elements) = get_tags e.Xml.elements in
	{ behavior_name = n;
	  behavior_loc = loc;
	  behavior_goals = List.map (get_goal lf n) elements;
	  behavior_tags = tags;
	}
    | _ -> assert false
	
let lemmas = ref []

let functions = ref []

let get_lemma_or_function e =
  match e.Xml.name with
    | "lemma" ->
	let n = get_name_attr e in
	let (tags,elements) = get_tags e.Xml.elements in	
	let loc = get_loc (List.hd elements) in
	let g = List.map (get_goal n "")  (List.tl elements) in
	begin
	  match g with
	    |	[g] ->
		  let l = 
		    { lemma_name = n;
		      lemma_loc = loc;
		      lemma_goal = g;
		      lemma_tags = tags;
		    }
		  in
		  lemmas := l :: !lemmas
	    | _ -> failwith "lemma should have exactly one goal element"
	end
    | "function" ->
	let n = get_name_attr e in
	let (tags,elements) = get_tags e.Xml.elements in
	let loc = get_loc (List.hd elements) in
	let behs = List.map (get_behavior n loc)  (List.tl elements) in
	let f =
	  { function_name = n;
	    function_loc = loc;
	    function_behaviors = behs;
	    function_tags = tags;
	  }
	in
	functions := f :: !functions
    | "tags" -> ()
    | _ -> assert false

(* read XML file *)
let load f =
  let xml = Xml.from_file f in
  match xml with
    | [p] when p.Xml.name = "project" ->
	let name = get_name_attr p in 
(*
	if name <> f then
	  eprintf "Warning! project name `%s' does match file name `%s'@." 
	    !project_name f;
*)
	lemmas := [];
	functions := [];
	List.iter get_lemma_or_function p.Xml.elements;
	{ project_name = name;
	  project_context_file = get_string_attr "context" p; 
	  project_lemmas = !lemmas;
	  project_functions = !functions;
	}
	
    | _ -> failwith "unique <project> element expected"


