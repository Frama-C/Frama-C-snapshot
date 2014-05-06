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

(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Format

module S = Datatype.String
module Xml = Why3_xml

type goal =
  {
    goal_name : string;
    goal_parent : theory;
    mutable goal_verified : bool;
  }


and theory =
  {
    theory_name : string;
    theory_parent : file;
    theory_goals : goal Datatype.String.Hashtbl.t;
    mutable theory_verified : bool;
    }

and file =
    {
      file_name : string;
      file_format : string option;
      file_parent : session;
      file_theories: theory Datatype.String.Hashtbl.t;
      (** Not mutated after the creation *)
      mutable file_verified : bool;
    }

and session =
    { session_files : file Datatype.String.Hashtbl.t;
      session_dir   : string;
    }

(** 2 Create a session *)
let db_filename = "why3session.xml"
let session_dir_for_save = ref "."

let empty_session dir =
  { session_files = S.Hashtbl.create 3;
    session_dir   = dir;
  }


(* [raw_add_goal parent name expl sum t] adds a goal to the given parent
   DOES NOT record the new goal in its parent, thus this should not be exported
*)
let raw_add_no_task parent name =
  let goal = { goal_name = name;
               goal_parent = parent;
               goal_verified = false;
             }
  in
  S.Hashtbl.replace parent.theory_goals name goal;
  goal

let raw_add_theory mfile thname =
  let mth = { theory_name = thname;
              theory_parent = mfile;
              theory_goals = S.Hashtbl.create 10;
              theory_verified = false;
            }
  in
  S.Hashtbl.replace mfile.file_theories thname mth;
  mth

let raw_add_file session f fmt =
  let mfile = { file_name = f;
                file_format = fmt;
                file_theories = S.Hashtbl.create 10;
                file_verified = false;
                file_parent  = session;
              }
  in
  S.Hashtbl.replace session.session_files f mfile;
  mfile


(****************************)
(*     session opening      *)
(****************************)
exception LoadError

let bool_attribute field r def =
  try
    match List.assoc field r.Xml.attributes with
      | "true" -> true
      | "false" -> false
      | _ -> assert false
  with Not_found -> def

let int_attribute_def field r def =
  try
    int_of_string (List.assoc field r.Xml.attributes)
  with Not_found | Invalid_argument _ -> def

let int_attribute field r =
  try
    int_of_string (List.assoc field r.Xml.attributes)
  with Not_found | Invalid_argument _ ->
    Wp_parameters.failure
      "[Why3ide] missing required attribute '%s' from element '%s'@."
      field r.Xml.name;
    raise LoadError

let string_attribute_def field r def=
  try
    List.assoc field r.Xml.attributes
  with Not_found -> def

let string_attribute field r =
  try
    List.assoc field r.Xml.attributes
  with Not_found ->
    Wp_parameters.failure
      "[Why3ide] missing required attribute '%s' from element '%s'@."
      field r.Xml.name;
    raise LoadError

let load_option attr g =
  try Some (List.assoc attr g.Xml.attributes)
  with Not_found -> None

let load_ident elt =
  let name = string_attribute "name" elt in
  name

let rec load_goal parent g =
  match g.Xml.name with
    | "goal" ->
        let gname = load_ident g in
        let verified = bool_attribute "proved" g false in
        let mg =
          raw_add_no_task parent gname
        in
        mg.goal_verified <- verified
    | "label" -> ()
    | s ->
	Wp_parameters.debug
          "[Why3ide] Session.load_goal: unexpected element '%s'@." s

let load_theory mf th =
  match th.Xml.name with
    | "theory" ->
        let thname = load_ident th in
        let verified = bool_attribute "verified" th false in
        let mth = raw_add_theory mf thname in
        List.iter (load_goal mth) th.Xml.elements;
        mth.theory_verified <- verified
    | s ->
	Wp_parameters.debug
          "[Why3ide] Session.load_theory: unexpected element '%s'@." s
	  
let load_file session f =
  match f.Xml.name with
    | "file" ->
        let fn = string_attribute "name" f in
        let fmt = load_option "format" f in
        let verified = bool_attribute "verified" f false in
        let mf = raw_add_file session fn fmt in
        List.iter (load_theory mf) f.Xml.elements;
        mf.file_verified <- verified
    | "prover" -> ()
    | s ->
	Wp_parameters.debug
          "[Why3ide] Session.load_file: unexpected element '%s'@." s
	  
let load_session session xml =
  match xml.Xml.name with
    | "why3session" ->
	(* dprintf debug "[Info] load_session: shape version is %d@\n"
           shape_version; *)
	(* just to keep the old_provers somewhere *)
	List.iter (load_file session) xml.Xml.elements;
	(* dprintf debug "[Info] load_session: done@\n" *)
    | s ->
	Wp_parameters.debug
          "[Why3ide] Session.load_session: unexpected element '%s'@." s
	  
type notask = unit
let read_session dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    begin
      Wp_parameters.error
        "[Why3ide] %s is not an existing directory" dir;
      raise LoadError
    end;
  let xml_filename = Filename.concat dir db_filename in
  let session = empty_session dir in
  (** If the xml is present we read it, otherwise we consider it empty *)
  if Sys.file_exists xml_filename then begin
    try
      let xml = Xml.from_file xml_filename in
      try
        load_session session xml.Xml.content;
      with Sys_error msg ->
        failwith ("Open session: sys error " ^ msg)
    with
      | Sys_error _msg ->
	  (* xml does not exist yet *)
          Wp_parameters.failure
            "[Why3ide] Can't open %s" xml_filename
      | Xml.Parse_error s ->
          Wp_parameters.failure
            "[Why3ide] XML database corrupted, ignored (%s)@." s;
	  (* failwith
             ("Open session: XML database corrupted (%s)@." ^ s) *)
          raise LoadError
  end;
  session
    
