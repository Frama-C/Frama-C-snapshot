(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* Registry for ACSL Builtins                                             --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Qed
open Lang

type category = Lang.lfun Qed.Logic.category

type builtin =
  | ACSLDEF
  | LFUN of lfun
  | HACK of (F.term list -> F.term)

type kind =
  | Z (* integer *)
  | R (* real *)
  | I of Ctypes.c_int
  | F of Ctypes.c_float
  | A (* abstract data *)

(* [LC] kinds can be compared by Stdlib.compare *)

let okind = function
  | C_int i -> I i
  | C_float f -> F f
  | _ -> A

let ckind typ = okind (object_of typ)

let skind = function
  | I _ | Z -> Logic.Sint
  | F _ | R -> Logic.Sreal
  | A -> Logic.Sdata

let rec lkind t =
  match Logic_utils.unroll_type ~unroll_typedef:false t with
  | Ctype ty -> ckind ty
  | Ltype({lt_name="set"},[t]) -> lkind t
  | Lreal -> R
  | Linteger -> Z
  | Ltype _ | Larrow _ | Lvar _ -> A

let kind_of_tau = function
  | Qed.Logic.Int -> Z
  | Qed.Logic.Real -> R
  | _ -> A

let pp_kind fmt = function
  | I i -> Ctypes.pp_int fmt i
  | F f -> Ctypes.pp_float fmt f
  | Z -> Format.pp_print_string fmt "int"
  | R -> Format.pp_print_string fmt "real"
  | A -> Format.pp_print_string fmt "_"

let pp_kinds fmt = function
  | [] -> ()
  | t::ts ->
      Format.fprintf fmt "(%a" pp_kind t ;
      List.iter (fun t -> Format.fprintf fmt ",%a" pp_kind t) ts ;
      Format.fprintf fmt ")"

let pp_libs fmt = function
  | [] -> ()
  | t::ts ->
      Format.fprintf fmt ": %s" t ;
      List.iter (fun t -> Format.fprintf fmt ",%s" t) ts

let pp_link fmt = function
  | ACSLDEF -> Format.pp_print_string fmt "(ACSL)"
  | HACK _ -> Format.pp_print_string fmt "(HACK)"
  | LFUN f -> Fun.pretty fmt f

(* -------------------------------------------------------------------------- *)
(* --- Driver & Lookup & Registry                                         --- *)
(* -------------------------------------------------------------------------- *)

type sigfun = kind list * builtin

type driver = {
  driverid : string;
  description : string;
  includes : string list;
  hlogic : (string , sigfun list) Hashtbl.t;
  hdeps : (string, string list) Hashtbl.t;
  hoptions :
    (string (* library *) * string (* group *) * string (* name *), string list)
      Hashtbl.t
}

let id d = d.driverid
let descr d = d.description
let is_default d = (d.driverid = "")
let compare d d' = String.compare d.driverid d'.driverid

let driver = Context.create "driver"
let cdriver () = Context.get driver

let lookup_driver name kinds =
  try
    let sigs = Hashtbl.find (cdriver ()).hlogic name in
    try List.assoc kinds sigs
    with Not_found ->
      Wp_parameters.feedback ~once:true
        "Use -wp-msg-key 'driver' for debugging drivers" ;
      if kinds=[]
      then Warning.error "Builtin %s undefined as a constant" name
      else Warning.error "Builtin %s undefined with signature %a" name
          pp_kinds kinds
  with Not_found ->
    if name.[0] == '\\' then
      Warning.error "Builtin %s%a not defined" name pp_kinds kinds
    else
      ACSLDEF

let hacks = Hashtbl.create 8
let hack name phi = Hashtbl.replace hacks name phi

let lookup name kinds =
  try
    let hack = Hashtbl.find hacks name in
    let compute es =
      try hack es with Not_found ->
      match lookup_driver name kinds with
      | ACSLDEF | HACK _ -> Warning.error "No fallback for hacked '%s'" name
      | LFUN p -> F.e_fun p es
    in HACK compute
  with Not_found -> lookup_driver name kinds

let register ?source name kinds link =
  let sigs = try Hashtbl.find (cdriver ()).hlogic name with Not_found -> [] in
  if List.exists (fun (s,_) -> s = kinds) sigs then
    Wp_parameters.warning ?source "Redefinition of logic %s%a"
      name pp_kinds kinds ;
  let entry = (kinds,link) in
  Hashtbl.add (cdriver ()).hlogic name (entry::sigs)

let iter_table f =
  let items = ref [] in
  Hashtbl.iter
    (fun a sigs -> List.iter (fun (ks,lnk) -> items := (a,ks,lnk)::!items) sigs)
    (cdriver ()).hlogic ;
  List.iter f (List.sort Transitioning.Stdlib.compare !items)

let iter_libs f =
  let items = ref [] in
  Hashtbl.iter
    (fun a libs -> items := (a,libs) :: !items)
    (cdriver ()).hdeps ;
  List.iter f (List.sort Transitioning.Stdlib.compare !items)

let dump () =
  Log.print_on_output
    begin fun fmt ->
      Format.fprintf fmt "Builtins:@\n" ;
      iter_libs
        (fun (name,libs) -> Format.fprintf fmt " * Library %s%a@\n"
            name pp_libs libs) ;
      iter_table
        (fun (name,k,lnk) -> Format.fprintf fmt " * Logic %s%a = %a@\n"
            name pp_kinds k pp_link lnk) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Implemented Builtins                                               --- *)
(* -------------------------------------------------------------------------- *)

let logic phi =
  lookup phi.l_var_info.lv_name
    (List.map (fun v -> lkind v.lv_type) phi.l_profile)

let ctor phi =
  lookup phi.ctor_name (List.map lkind phi.ctor_params)

let constant name =
  lookup name []

(* -------------------------------------------------------------------------- *)
(* --- Declaration of Builtins                                            --- *)
(* -------------------------------------------------------------------------- *)

let dependencies lib =
  Hashtbl.find (cdriver ()).hdeps lib

let add_library lib deps =
  let others = try dependencies lib with Not_found -> [] in
  Hashtbl.add (cdriver ()).hdeps lib (others @ deps)

let add_alias ~source name kinds ~alias () =
  register ~source name kinds (lookup alias kinds)

let add_logic ~source result name kinds ~library ?category ~link () =
  let sort = skind result in
  let params = List.map skind kinds in
  let lfun = Lang.extern_s ~library ?category ~sort ~params ~link name in
  register ~source name kinds (LFUN lfun)

let add_predicate ~source name kinds ~library ~link () =
  let params = List.map skind kinds in
  let lfun = Lang.extern_fp ~library ~params ~link link.altergo in
  register ~source name kinds (LFUN lfun)

let add_ctor ~source name kinds ~library ~link () =
  let category = Logic.Constructor in
  let params = List.map skind kinds in
  let lfun = Lang.extern_s ~library ~category ~params ~link name in
  register ~source name kinds (LFUN lfun)

let add_type ~source name ~library ?(link=Lang.infoprover name) () =
  if Lang.mem_builtin_type ~name then
    Wp_parameters.warning ~source "Redefinition of type '%s'" name ;
  Lang.set_builtin_type ~name ~library ~link

type sanitizer = driver_dir:string -> string -> string
let sanitizers : ( string * string , sanitizer ) Hashtbl.t = Hashtbl.create 10

exception Unknown_option of string * string

let sanitize ~driver_dir group name v =
  try
    (Hashtbl.find sanitizers (group,name)) ~driver_dir v
  with Not_found -> raise (Unknown_option(group,name))

type doption = string * string

let create_option ~sanitizer group name =
  let option = (group,name) in
  Hashtbl.replace sanitizers option sanitizer ;
  option

let get_option (group,name) ~library =
  try Hashtbl.find (cdriver ()).hoptions (library,group,name)
  with Not_found -> []

let set_option ~driver_dir group name ~library value =
  let value = sanitize ~driver_dir group name value in
  Hashtbl.replace (cdriver ()).hoptions (library,group,name) [value]

let add_option ~driver_dir group name ~library value =
  let value = sanitize ~driver_dir group name value in
  let l = get_option (group,name) ~library in
  Hashtbl.replace (cdriver ()).hoptions (library,group,name) (l @ [value])


(** Includes *)

let find_lib file =
  if Sys.file_exists file then file else
    let rec lookup file = function
      | [] -> Wp_parameters.abort "File '%s' not found" file
      | dir::dirs ->
          let path = Printf.sprintf "%s/%s" dir file in
          if Sys.file_exists path then path else lookup file dirs
    in
    lookup file (cdriver ()).includes

(* -------------------------------------------------------------------------- *)
(* --- Implemented Builtins                                               --- *)
(* -------------------------------------------------------------------------- *)

let builtin_driver = {
  driverid = "builtin driver";
  description = "builtin driver";
  includes = [];
  hlogic = Hashtbl.create 131;
  hdeps  = Hashtbl.create 31;
  hoptions = Hashtbl.create 131;
}

let add_builtin name kinds lfun =
  let phi = LFUN lfun in
  if Context.defined driver then
    register name kinds phi
  else
    Context.bind driver builtin_driver (register name kinds) phi

let create ~id ?(descr=id) ?(includes=[]) () =
  {
    driverid = id ;
    description = descr ;
    includes = includes @ builtin_driver.includes ;
    hlogic = Hashtbl.copy builtin_driver.hlogic ;
    hdeps  = Hashtbl.copy builtin_driver.hdeps ;
    hoptions = Hashtbl.copy builtin_driver.hoptions ;
  }

let init ~id ?descr ?includes () =
  Context.set driver (create ~id ?descr ?includes ())

(* -------------------------------------------------------------------------- *)
