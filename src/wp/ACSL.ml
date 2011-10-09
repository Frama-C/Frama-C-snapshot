(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
(* --- Custom Registries                                                  --- *)
(* -------------------------------------------------------------------------- *)

module type Compiler =
sig
  type key
  type data
  val name : string
  val reprs : data list
  val compile : key -> data
end

module Register(K : Datatype.Hashtbl)(C : Compiler with type key = K.key) =
struct

  type value = Data of C.data | Error of exn | Locked

  module D : Datatype.S with type t = value =
    Datatype.Make
      (struct
	 include Datatype.Undefined
	 type t = value
	 let name = "Wp.Data." ^ C.name
	 let reprs = Locked :: Error Exit :: List.map (fun x -> Data x) C.reprs
       end)

  module H = State_builder.Hashtbl(K)(D)
    (struct
       let name = "Wp.ACSL." ^ C.name
       let dependencies = [Ast.self]
       let kind = `Tuning
       let size = 231
     end)

  let get_value x =
    try H.find x
    with Not_found ->
      H.replace x Locked ;
      let value = try Data(C.compile x) with exn -> Error exn in
      H.replace x value ; value

  let obtain x =
    match get_value x with
      | Locked -> Wp_parameters.fatal "Cyclic compilation (%a)" K.Key.pretty x
      | Error exn -> raise exn
      | Data y -> y

end

(* -------------------------------------------------------------------------- *)
(* --- ACSL Definitions                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Cil_types
open Cil_datatype

open LogicId
open LogicTau
open LogicLang
open LogicDef

let dtau_ctype : (typ -> tau) ref = ref (fun _ -> assert false)
let dhas_ctype : (typ -> term -> pred) ref = ref (fun _ _ -> assert false)
let deq_ctype : (typ -> term -> term -> pred) ref = ref (fun _ _ _ -> assert false)

(* -------------------------------------------------------------------------- *)
(* --- Array with Dimension                                               --- *)
(* -------------------------------------------------------------------------- *)

module Tarray =
  Datatype.Make_with_collections
    (struct
       include Datatype.Undefined
       type t = typ * int option list
       let name = "Wp.ACSL.Tarray"
       let reprs = [TVoid[],[]]
       let pp_hash_dim h = function 
	 | None -> h * 731 
	 | Some d -> h * 731 + d * 27 
       let hash (te,dims) =
	 List.fold_left pp_hash_dim (Typ.hash te) dims
       let equal (t1,d1) (t2,d2) =
	 Pervasives.(=) d1 d2 && Typ.equal t1 t2
       let compare _ _ = 0 (* incorrect but never used *)
       let pp_dim fmt = function
	 | None -> Format.pp_print_string fmt "[]"
	 | Some d -> Format.fprintf fmt "[%d]" d
       let pretty fmt (te,dims) =
	 Typ.pretty fmt te ; List.iter (pp_dim fmt) dims
     end)

(* Find the head-dimension of an array *)
let tarray_dim = function
  | None -> None
  | Some aflat ->
      try Some(Int64.to_int aflat.arr_size)
      with _ -> None

(* t-array extraction *)
let rec tarray_of_typ t ds =
  match object_of t with
    | C_array info -> tarray_of_arrayinfo info ds
    | _ -> t,ds

and tarray_of_arrayinfo info ds =
  let d = tarray_dim info.arr_flat in
  tarray_of_typ info.arr_element (d::ds)

let rec tau_of_tarray tau = function
  | [] -> tau
  | _::ds -> tau_of_tarray (Array(Integer,tau)) ds

let tarray_elt fmt te =
  match object_of te with
    | C_int i -> pp_int fmt i
    | C_float f -> pp_float fmt f
    | C_comp c -> Format.pp_print_string fmt c.cname
    | C_array _ -> Wp_parameters.fatal "incomplete array-dimension"
    | C_pointer _ -> Wp_parameters.fatal "pointer array constraint"

let tarray_name te ds =
  Pretty_utils.sfprintf "%a%t" tarray_elt te
    (fun fmt ->
       List.iter
	 (function
	    | None -> Format.fprintf fmt "_arr"
	    | Some n -> Format.fprintf fmt "_a%d" n
	 ) ds)

let in_range i d p =
  match d with
    | None -> p
    | Some n -> 
	let k = e_var i in
	p_goal [ p_icmp Cleq e_zero k ; p_icmp Clt k (e_int n) ] p

module Darray =
  Datatype.Make_with_collections
    (struct
       include Datatype.Undefined
       type t = typ * int
       let name = "Wp.ACSL.Darray"
       let reprs = [TVoid[],0]
       let hash (te,dims) = 31*(Typ.hash te) + dims
       let equal (t1,d1) (t2,d2) =
	 Pervasives.(=) d1 d2 && Typ.equal t1 t2
       let compare _ _ = 0 (* incorrect but never used *)
       let pretty fmt (te,dims) =
	 Typ.pretty fmt te ; 
	 for i=1 to dims do Format.pp_print_string fmt "[]" done
     end)

let rec tau_of_darray te n =
  if n > 0 then Array(Integer, tau_of_darray te (pred n)) else te

let darray_name te n = Pretty_utils.sfprintf "%a_d%d" tarray_elt te n

let rec darray_of_arrayinfo info n =
  let te = info.arr_element in
  match object_of te with
    | C_int _ | C_float _ | C_pointer _ | C_comp _ -> te , n
    | C_array arr -> darray_of_arrayinfo arr (succ n)
	  
(* -------------------------------------------------------------------------- *)
(* --- C-Comp                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Fmap = Fieldinfo.Map
module Record = Register(Compinfo.Hashtbl)
  (struct

     type key = compinfo
     type data = id * field Fmap.t

     let name = "Record"
     let reprs = [LogicId.dummy,Fmap.empty]

     let compile comp =
       let prefix = if comp.cstruct then "S_" else "U_" in
       let record_id = LogicId.create (prefix ^ comp.cname) in
       let record_fields = ref [] in
       let compile_fields = ref Fmap.empty in
       List.iter
	 (fun f ->
	    let field = { 
	      LogicTau.f_name = LogicId.create ("F_" ^ f.fname) ; 
	      LogicTau.f_type = !dtau_ctype f.ftype ;
	      LogicTau.f_record = record_id ;
	    } in
	    record_fields := field :: !record_fields ;
	    compile_fields := Fmap.add f field !compile_fields ;
	 ) comp.cfields ;
       let descr = {
	 t_source = fst (List.hd comp.cfields).floc ;
	 t_short = (if comp.cstruct then "struct " else "union ") ^ comp.corig_name ;
	 t_descr = "" ;
       } in
       LogicDef.declare {
	 d_name = record_id ;
	 d_item = RECORD (List.rev !record_fields) ;
	 d_descr = descr ;
       } ;
       (record_id , !compile_fields)

   end)

let record_of comp = fst (Record.obtain comp)
let field_of finfo = Fmap.find finfo (snd (Record.obtain finfo.fcomp))

(* -------------------------------------------------------------------------- *)
(* --- ADT-Types                                                          --- *)
(* -------------------------------------------------------------------------- *)

module ADT = Register(Logic_type_info.Hashtbl)
  (struct
     type key = logic_type_info
     type data = id
     let name = "ADT"
     let reprs = [LogicId.dummy]
     let compile lt = 
       let tid = LogicId.create ("T_" ^ lt.lt_name) in
       LogicDef.declare {
	 d_name = tid ;
	 d_item = TYPE (List.length lt.lt_params) ;
	 d_descr = {
	   t_source = Lexing.dummy_pos ;
	   t_short = Printf.sprintf "logic type %s" lt.lt_name ;
	   t_descr = "" ;
	 } ;
       } ; tid
   end)

(* -------------------------------------------------------------------------- *)
(* --- Types                                                              --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes

let rec tau_of_ctype t = 
  tau_of_object (Ctypes.object_of t)

and tau_of_object = function
  | C_int _ -> Integer
  | C_float _ -> Real
  | C_pointer _ -> Pointer
  | C_comp c -> Record (record_of c)
  | C_array a -> Array(Integer,tau_of_ctype a.arr_element)
  
and tau_of_logic_type = function
  | Ctype c -> tau_of_ctype c
  | Linteger ->  Integer
  | Lreal ->  Real
  | Ltype( d , [] ) when d.lt_name = Utf8_logic.boolean -> Boolean
  | Ltype( {lt_name="set"} , [t] ) -> Set (tau_of_logic_type t)
  | Ltype( lt , ts) -> ADT( ADT.obtain lt , List.map tau_of_logic_type ts )
  | Lvar _ -> Wp_parameters.not_yet_implemented "logic type variables"
  | Larrow _ -> Wp_parameters.not_yet_implemented "type of logic function"

let () = dtau_ctype := tau_of_ctype

(* -------------------------------------------------------------------------- *)
(* --- Is Int                                                             --- *)
(* -------------------------------------------------------------------------- *)

let lib_is_int = List.map
  (fun i -> i , LogicId.library (Pretty_utils.sfprintf "is_%a" pp_int i))
  Ctypes.c_int_all

let lib_to_int = List.map
  (fun i -> i , LogicId.library (Pretty_utils.sfprintf "to_%a" pp_int i))
  Ctypes.c_int_all

let is_int i = List.assoc i lib_is_int
let to_int i = List.assoc i lib_to_int

(* -------------------------------------------------------------------------- *)
(* --- Is Comp                                                            --- *)
(* -------------------------------------------------------------------------- *)

module IsComp = Register(Compinfo.Hashtbl)
  (struct
     type key = compinfo
     type data = id
     let name = "IsComp"
     let reprs = [LogicId.dummy]
     let compile compinfo =
       let record = record_of compinfo in
       let pid = LogicId.create ("Is_" ^ compinfo.cname) in
       let pool = LogicLang.pool () in
       let r_var = LogicLang.fresh pool "r" (Record record) in
       let r_val = e_var r_var in
       let has_ftype f = 
	 !dhas_ctype f.Cil_types.ftype (e_getfield r_val (field_of f))
       in
       let condition = p_conj (List.map has_ftype compinfo.cfields) in
       LogicDef.declare {
	 d_name = pid ;
	 d_item = PREDICATE([r_var],Some condition) ;
	 d_descr = {
	   t_source = Lexing.dummy_pos ;
	   t_short = "subtype " ^ compinfo.corig_name ;
	   t_descr = "" ;
	 } ;
       } ; pid
   end)

(* -------------------------------------------------------------------------- *)
(* --- Is Array                                                           --- *)
(* -------------------------------------------------------------------------- *)
	
let rec is_darray pool te dims a =
  if dims > 0 then
    let i = LogicLang.fresh pool "i" Integer in
    let a_i = e_access a (e_var i) in
    p_forall i (is_darray pool te (pred dims) a_i)
  else
    !dhas_ctype te a
	  
module IsArray = Register(Darray.Hashtbl)
  (struct
     type key = Darray.t
     type data = id
     let name = "IsArray"
     let reprs = [LogicId.dummy]
     let compile (te,dims) =
       let tau = tau_of_ctype te in
       let tau_dim = tau_of_darray tau dims in
       let aid = LogicId.create ("Is_" ^ darray_name te dims) in 
       let pool = LogicLang.pool () in
       let a = LogicLang.fresh pool "a" tau_dim in
       let condition = is_darray pool te dims (e_var a) in
       LogicDef.declare {
	 d_name = aid ;
	 d_item = PREDICATE([a],Some condition) ;
	 d_descr = {
	   t_source = Lexing.dummy_pos ;
	   t_short = "subtype of array" ;
	   t_descr = "" ;
	 } ;
       } ; aid
   end)

(* -------------------------------------------------------------------------- *)
(* --- ACSL-Types                                                         --- *)
(* -------------------------------------------------------------------------- *)
	
let rec has_ctype typ term = has_object (object_of typ) term
and has_object obj term =
  match obj with
    | C_float _ | C_pointer _ -> p_true
    | C_int i -> p_call (is_int i) [term]
    | C_comp c -> p_call (IsComp.obtain c) [term]
    | C_array info -> 
	let ta = darray_of_arrayinfo info 1 in
	match object_of (fst ta) with
	  | C_float _ | C_pointer _ -> p_true
	  | C_array _ -> Wp_parameters.fatal "non canonical array-dimension"
	  | C_int _ | C_comp _ -> 
	      p_call (IsArray.obtain ta) [term]

let () = dhas_ctype := has_ctype

(* -------------------------------------------------------------------------- *)
(* --- Eq-Comp                                                            --- *)
(* -------------------------------------------------------------------------- *)

module EqComp = Register(Compinfo.Hashtbl)
  (struct
     type key = compinfo
     type data = id
     let reprs = [LogicId.dummy]
     let name = "EqComp"
     let compile compinfo =
       let record = record_of compinfo in
       let cid = LogicId.create ("Eq_" ^ compinfo.cname) in
       let pool = LogicLang.pool () in
       let a = LogicLang.fresh pool "a" (Record record) in
       let b = LogicLang.fresh pool "b" (Record record) in
       let a_val = e_var a in
       let b_val = e_var b in
       let eq_field f =
	 let fd = field_of f in
	 !deq_ctype f.Cil_types.ftype (e_getfield a_val fd) (e_getfield b_val fd)
       in
       let condition = p_conj (List.map eq_field compinfo.cfields) in
       LogicDef.declare {
	 d_name = cid ;
	 d_item = PREDICATE([a;b],Some condition) ;
	 d_descr = {
	   t_source = Lexing.dummy_pos ;
	   t_short = "equality for " ^ compinfo.corig_name ;
	   t_descr = "" ;
	 } ;
       } ; cid
   end)

(* -------------------------------------------------------------------------- *)
(* --- Eq-Array                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec eq_tarray pool te dims ta tb =
  match dims with
    | [] -> !deq_ctype te ta tb
    | d::ds ->
	let i = LogicLang.fresh pool "i" Integer in
	let ta_i = e_access ta (e_var i) in
	let tb_i = e_access tb (e_var i) in
	p_forall i
	  (in_range i d (eq_tarray pool te ds ta_i tb_i))

module EqArray = Register(Tarray.Hashtbl)
  (struct
     type key = Tarray.t
     type data = id
     let reprs = [LogicId.dummy]
     let name = "EqArray"
     let compile (te,dims) =
       let tau = tau_of_ctype te in
       let tau_dim = tau_of_tarray tau dims in
       let aid = LogicId.create ("Eq_" ^ tarray_name te dims) in 
       let pool = LogicLang.pool () in
       let a = LogicLang.fresh pool "a" tau_dim in
       let b = LogicLang.fresh pool "b" tau_dim in
       let condition = eq_tarray pool te dims (e_var a) (e_var b) in
       LogicDef.declare {
	 d_name = aid ;
	 d_item = PREDICATE([a],Some condition) ;
	 d_descr = {
	   t_source = Lexing.dummy_pos ;
	   t_short = "equality for array" ;
	   t_descr = "" ;
	 } ;
       } ; aid
   end)

(* -------------------------------------------------------------------------- *)
(* --- ACSL-Equality                                                      --- *)
(* -------------------------------------------------------------------------- *)

let rec eq_ctype typ t1 t2 = eq_object (object_of typ) t1 t2

and eq_object obj t1 t2 =
  match obj with
    | C_int _ -> p_icmp Ceq t1 t2
    | C_float _ -> p_rcmp Ceq t1 t2
    | C_comp c -> p_call (EqComp.obtain c) [t1;t2]
    | C_array a -> p_call (EqArray.obtain (tarray_of_arrayinfo a [])) [t1;t2]
    | C_pointer _ -> p_equal t1 t2

and eq_logic_type lt t1 t2 =
  match lt with
    | Ctype typ -> eq_ctype typ t1 t2
    | _ -> p_equal t1 t2 (* Stronger for Set, etc. *)

let () = deq_ctype := eq_ctype

(* -------------------------------------------------------------------------- *)
