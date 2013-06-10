(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
module W = Wp_parameters

type category = Lang.lfun Qed.Logic.category

type builtin =
  | ACSLDEF
  | LFUN of lfun
  | CONST of F.term

type kind = 
  | Z (* integer *)
  | R (* real *)
  | I of Ctypes.c_int
  | F of Ctypes.c_float
  | A (* abstract data *)

(* [LC] kinds can be compared by Pervasives.compare *)

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
  match Logic_utils.unroll_type t with
    | Ctype ty -> ckind ty
    | Ltype({lt_name="set"},[t]) -> lkind t
    | Lreal -> R
    | Linteger -> Z
    | Ltype _ | Larrow _ | Lvar _ -> A

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
  | CONST e -> F.pp_term fmt e
  | LFUN f -> Fun.pretty fmt f

(* -------------------------------------------------------------------------- *)
(* --- Lookup & Registry                                                  --- *)
(* -------------------------------------------------------------------------- *)

type sigfun = kind list * builtin

let hlogic : (string , sigfun list) Hashtbl.t = Hashtbl.create 131
let symbol : (string , lfun) Hashtbl.t = Hashtbl.create 131
let hlibs = Hashtbl.create 31

let chop_backslash name =
  if name.[0] == '\\' then String.sub name 1 (String.length name - 1) else name

let lookup name kinds =
  try 
    let sigs = Hashtbl.find hlogic name in
    try List.assoc kinds sigs
    with Not_found ->
      Wp_parameters.feedback ~once:true 
	"Use -wp-logs 'driver' for debugging drivers" ;
      if kinds=[] 
      then W.error ~current:true "Builtin %s undefined as a constant" name
      else W.error ~current:true "Builtin %s undefined with signature %a" name
	pp_kinds kinds ;
      ACSLDEF
  with Not_found ->
    if name.[0] == '\\' then 
      W.error "Builtin %s%a not defined" name pp_kinds kinds ;
    ACSLDEF

let register name kinds link =
  let sigs = try Hashtbl.find hlogic name with Not_found -> [] in
  begin
    if List.exists (fun (s,_) -> s = kinds) sigs then
      let msg = Pretty_utils.sfprintf "Builtin %s%a already defined" name 
	pp_kinds kinds 
      in failwith msg ;
  end ;
  let entry = (kinds,link) in
  Hashtbl.add hlogic name (entry::sigs) ;
  match link with LFUN f -> Hashtbl.add symbol (Fun.id f) f | _ -> ()

let symbol = Hashtbl.find symbol

let iter_table f =
  let items = ref [] in
  Hashtbl.iter
    (fun a sigs -> List.iter (fun (ks,lnk) -> items := (a,ks,lnk)::!items) sigs)
    hlogic ;
  List.iter f (List.sort Pervasives.compare !items)

let iter_libs f =
  let items = ref [] in
  Hashtbl.iter
    (fun a libs -> items := (a,libs) :: !items)
    hlibs ;
  List.iter f (List.sort Pervasives.compare !items)

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
  
(* -------------------------------------------------------------------------- *)
(* --- Declaration of Builtins                                            --- *)
(* -------------------------------------------------------------------------- *)

let dependencies lib = Hashtbl.find hlibs lib

let add_library lib deps = 
  Hashtbl.add hlibs lib deps

let add_logic result name kinds ~theory ?category ?balance 
    ?(link=chop_backslash name) () =
  let result = skind result in
  let params = List.map skind kinds in
  let lfun = Lang.extern_s ~theory ?category ?balance ~result ~params link in
  register name kinds (LFUN lfun)
    
let add_predicate name kinds ~theory ?(link=chop_backslash name) () =
  let params = List.map skind kinds in
  let lfun = Lang.extern_fp ~theory ~params link in
  register name kinds (LFUN lfun)

let add_ctor name kinds ~theory ?(link=name) () =
  let category = Logic.Constructor in
  let params = List.map skind kinds in
  let lfun = Lang.extern_s ~theory ~category ~params ~result:Logic.Sdata link in
  register name kinds (LFUN lfun)

let add_const name value =
  register name [] (CONST value)

let add_type name ~theory ?(link=name) () =
  Lang.builtin ~name ~theory ~link

(* -------------------------------------------------------------------------- *)
(* --- Abs,Min,Max algebraic properties                                   --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic

let minmax = 
  Operator {
    inversible = false ;
    commutative = true ;
    associative = true ;
    idempotent = true ;
    neutral = E_none ;
    absorbant = E_none ;
  }

(* -------------------------------------------------------------------------- *)
(* --- Implemented Builtins                                               --- *)
(* -------------------------------------------------------------------------- *)

let () =
  begin

    add_const "\\true" F.e_true ;
    add_const "\\false" F.e_false ;

    let theory = "cmath" in
    add_logic Z "\\abs" [ Z ] ~theory ~link:"abs_int" () ;
    add_logic R "\\abs" [ R ] ~theory ~link:"abs_real" () ;
    add_logic Z "\\max" [ Z;Z ] ~theory ~link:"max_int" ~category:minmax () ;
    add_logic Z "\\min" [ Z;Z ] ~theory ~link:"min_int" ~category:minmax () ;
    add_logic R "\\max" [ R;R ] ~theory ~link:"max_real" () ;
    add_logic R "\\min" [ R;R ] ~theory ~link:"min_real" () ;

    let theory = "cfloat" in
    add_type "rounding_mode" ~theory () ;
    add_ctor "Up" [] ~theory () ;
    add_ctor "Down" [] ~theory () ;
    add_ctor "ToZero" [] ~theory () ;
    add_ctor "NearestAway" [] ~theory ~link:"NearestTiesToAway" () ;
    add_ctor "NearestEven" [] ~theory ~link:"NearestTiesToEven" () ;
    add_predicate "\\is_finite" [ F Float32 ] ~theory ~link:"is_finite32" () ;
    add_predicate "\\is_finite" [ F Float64 ] ~theory ~link:"is_finite64" () ;
    add_logic A "\\round_float" [ A; R ] ~theory () ;
    add_logic A "\\round_double" [ A ; R ] ~theory () ;

  end
