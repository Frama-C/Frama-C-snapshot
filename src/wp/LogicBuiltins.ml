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
(* Registry for ACSL Builtins                                             --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Qed
open Lang

type builtin =
  | USER
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

let _tkind t = lkind t.term_type

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

(* -------------------------------------------------------------------------- *)
(* --- Lookup & Registry                                                  --- *)
(* -------------------------------------------------------------------------- *)

let lookup (hmap : (string * kind list , builtin) Hashtbl.t) name kinds =
  try Hashtbl.find hmap (name,kinds)
  with Not_found ->
    if name.[0] == '\\' then begin
      Wp_parameters.error "Builtin %s%a not defined" name pp_kinds kinds ;
    end;
    USER

let register hmap name kinds entry =
  let key = ("\\" ^ name) , kinds in
  if Hashtbl.mem hmap key then
    Wp_parameters.fatal "Builtin \\%s%a already defined" name pp_kinds kinds ;
  Hashtbl.add hmap key entry

let hlogic = Hashtbl.create 131
let hctors = Hashtbl.create 131 

let dump () =
  Log.print_on_output 
    begin fun fmt ->
      Format.fprintf fmt "Builtins:@\n" ;
      Hashtbl.iter 
	(fun (name,k) _ -> Format.fprintf fmt " * Logic %s%a@\n" name pp_kinds k) 
	hlogic ;
      Hashtbl.iter 
	(fun (name,k) _ -> Format.fprintf fmt " * Constructor %s%a@\n" name pp_kinds k) 
	hctors ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Implemented Builtins                                               --- *)
(* -------------------------------------------------------------------------- *)
  
let logic phi = 
  lookup hlogic phi.l_var_info.lv_name 
    (List.map (fun v -> lkind v.lv_type) phi.l_profile)

let ctor phi = 
  lookup hctors phi.ctor_name (List.map lkind phi.ctor_params)
  
(* -------------------------------------------------------------------------- *)
(* --- Declaration of Builtins                                            --- *)
(* -------------------------------------------------------------------------- *)
  
let add_logic result name kinds ~theory ?(link=name) 
    ?(category=Logic.Function) () =
  let sort = skind result in
  let lfun = Lang.extern_s ~theory ~category ~sort link in
  register hlogic name kinds (LFUN lfun)
    
let add_predicate name kinds ~theory ?(link=name) () =
  let lfun = Lang.extern_fp ~theory link in
  register hlogic name kinds (LFUN lfun)
    
let add_ctor name kinds ~theory ?(link=name) () =
  let category = Logic.Constructor in
  let lfun = Lang.extern_s ~theory ~category ~sort:Logic.Sdata link in
  register hctors name kinds (LFUN lfun)

let add_const name value =
  register hctors name [] (CONST value)

let add_type name ~theory ?(link=name) () =
  Lang.builtin ~name ~theory ~link

(* -------------------------------------------------------------------------- *)
(* --- Implemented Builtins                                               --- *)
(* -------------------------------------------------------------------------- *)

let () =
  begin

    add_const "true" F.e_true ;
    add_const "false" F.e_false ;

    add_logic Z "abs" [ Z ]  ~theory:"cmath" ~link:"abs_int" () ;
    add_logic R "abs" [ R ] ~theory:"cmath" ~link:"abs_real" () ;
    add_logic Z "max" [ Z;Z ]  ~theory:"cmath" ~link:"max_int" () ;
    add_logic Z "max" [ R;R ]  ~theory:"cmath" ~link:"max_real" () ;
    add_logic Z "min" [ Z;Z ]  ~theory:"cmath" ~link:"min_int" () ;
    add_logic Z "min" [ R;R ]  ~theory:"cmath" ~link:"min_real" () ;

    add_type "sign" ~theory:"cfloat" () ;
    add_ctor "Positive" [] ~theory:"cfloat" () ;
    add_ctor "Negative" [] ~theory:"cfloat" () ;

    add_type "float_format" ~theory:"cfloat" () ;
    add_ctor "Single" [] ~theory:"cfloat" () ;
    add_ctor "Double" [] ~theory:"cfloat" () ;
    add_ctor "Quad" [] ~theory:"cfloat" () ;

    add_type "rounding_mode" ~theory:"cfloat" () ;
    add_ctor "Up" [] ~theory:"cfloat" () ;
    add_ctor "Down" [] ~theory:"cfloat" () ;
    add_ctor "ToZero" [] ~theory:"cfloat" () ;
    add_ctor "NearestAway" [] ~theory:"cfloat" () ;
    add_ctor "NearestEven" [] ~theory:"cfloat" () ;

    add_predicate "is_finite" [ F Float32 ] ~theory:"cfloat" ~link:"is_finite32" () ;
    add_predicate "is_finite" [ F Float64 ] ~theory:"cfloat" ~link:"is_finite64" () ;

    add_logic A "round_float" [ A; R ] ~theory:"cfloat" () ;
    add_logic A "round_double" [ A ; R ] ~theory:"cfloat" () ;

  end
