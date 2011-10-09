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
(* --- Translation of Expressions                                         --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Cil_types
open Cil_datatype
module WpLog = Wp_parameters


let dkey = "translate_exp" (* debugging key*)

let debug = Wp_parameters.debug ~dkey 


module Create (M:Mvalues.S)  =
struct

  module F = M.F
  module A = M.A
  module R = M.R

  (*------------------------------------------------------------------------ *)
  (*--- Utilities                                                        --- *)
  (*------------------------------------------------------------------------ *)

  let c_int_of_typ t =
    match object_of t with
      | C_int i -> i
      | _ -> WpLog.fatal "non-integer offset"


  let int_of_value = function
    | M.V_int(_,t) -> t
    | v -> WpLog.fatal "[int_of_value] of non integer value %a" M.pp_value v

  let float_of_value = function
    | M.V_float(_,t) -> t
    | v -> WpLog.fatal "[float_of_value] of non float value %a" M.pp_value v

  let loc_of_value = function
    | M.V_pointer(_,loc) -> loc
    | M.V_array _ as v -> 
	debug "[loc_of_value] ARRAY %a" M.pp_value v ; 
	WpLog.fatal "[loc_of_value] ARRAY %a" M.pp_value v
    | v -> debug "[loc_of_value] %a" M.pp_value v ; 
	WpLog.fatal "[loc_of_value] %a" M.pp_value v

  let value_of_integer i z = M.V_int(i,z)
  let value_of_boolean b = value_of_integer (Ctypes.c_bool()) (F.e_bool b)
  let boolean_of_loc p = F.e_not (M.is_null p)
  let boolean_of_integer z = F.e_icmp Formula.Cneq z F.i_zero
  let boolean_of_int  v = boolean_of_integer v
  let boolean_of_float v = F.e_rcmp Formula.Cneq v F.r_zero

  let boolean_of_value = function
    | M.V_int(_,t) -> boolean_of_int t
    | M.V_float(_,t) -> boolean_of_float t
    | M.V_pointer(_,loc) -> boolean_of_loc loc
    | v -> WpLog.fatal "[loc_of_value] %a"M.pp_value v

  let prop_of_loc p = F.p_not (F.p_bool (M.is_null p))
  let prop_of_integer z = F.p_icmp Formula.Cneq z F.i_zero
  let prop_of_int z = prop_of_integer  z
  let prop_of_float r = F.p_rcmp Formula.Cneq r F.r_zero

  let prop_of_value = function
    | M.V_int(_,t) -> prop_of_int t
    | M.V_float(_,t) -> prop_of_float t
    | M.V_pointer(_,loc) -> prop_of_loc loc
    | v -> WpLog.fatal "[prop_of_value] %a" M.pp_value v

  let not_of_loc p = F.p_bool (M.is_null p)
  let not_of_integer z = F.p_icmp Formula.Ceq z F.i_zero
  let not_of_int z = not_of_integer z
  let not_of_float r = F.p_rcmp Formula.Ceq r F.r_zero

  let not_of_value = function
    | M.V_int(_,t) -> not_of_int t
    | M.V_float(_,t) -> not_of_float t
    | M.V_pointer(_,loc) -> not_of_loc loc
    | v -> WpLog.fatal "[not_of_value] %a" M.pp_value v

  (* ----------------------------------------------------------------------- *)
  (* --- Deep Recursion over Expressions, Conditions and binary operators -- *)
  (* ----------------------------------------------------------------------- *)

  let expr_rec = ref (fun _ _ -> assert false)
  let cond_rec = ref (fun _ _ -> assert false)

  let prop_rec = ref (fun _ _ -> assert false)

  (* [expr_int mem ie ir e] interprets an expression [e] in the memory
     [mem] as an integer of size [ie] and converts this integer as an
     integer of size [ir]. *)
  let expr_int mem ie ir e =
    F.i_convert ie ir (int_of_value (!expr_rec mem e))

  (* [expr_float mem fe fr e] interprets an expression [e] in the
     memory [mem] as a float of size [fe] and converts this float as a
     float of size [fr]. *)
  let expr_float mem _fe _fr e =
    (float_of_value (!expr_rec mem e))

  (* ----------------------------------------------------------------------- *)
  (* --- Casts                                                           --- *)
  (* ----------------------------------------------------------------------- *)

  (*[expr_cast tyv tyr v] converts a value [v] of type [tyv] to type
    [tyr].*)
  let expr_cast tyv tyr v =
    if Typ.equal tyv tyr then v else
    match object_of tyv, object_of tyr with
      | C_int i1 , C_int i2 ->
          M.V_int(i2,F.i_convert i1 i2 (int_of_value v))
      | C_float _f1 , C_float f2 ->
          M.V_float(f2,float_of_value v)
      | C_int _ , C_float f2 ->
          let z = int_of_value v in
          let r = F.real_of_integer z in
          M.V_float(f2,r)
      | C_float _ , C_int i2 ->
          let r = float_of_value v in
          let z = F.integer_of_real r in
          M.V_int(i2,M.F.modulo i2 z)
            (* TODO : specify non-modulo Cf. ISO-C 6.3.1.4 *)
      | C_pointer t1 , C_pointer t2 ->
	  M.V_pointer
	    (Ctypes.object_of t2,
	     M.cast_loc_to_loc t1 t2 (loc_of_value v))
      | C_pointer t1 , C_int i2 ->
          M.V_int (i2,M.cast_loc_to_int t1 (loc_of_value v)i2)
      | C_int i1 , C_pointer t2 ->
          M.V_pointer
	    (Ctypes.object_of t2, 
	     M.cast_int_to_loc i1 (int_of_value v) t2)
      | a,b ->
          WpLog.not_yet_implemented "cast from %a to %a"
            pp_object a pp_object b

  let prop_cast tyv tyr v =
    match object_of tyv , object_of tyr with
      | C_int i1 , C_int i2 ->
          prop_of_int (F.i_convert i1 i2 (int_of_value v))
      | C_float _f1 , C_float _f2 ->
          prop_of_float (float_of_value v)
      | C_int _i1 , C_float _ ->
          let z = int_of_value v in
          let r = F.real_of_integer z in
          prop_of_float r
      | C_float _ , C_int i2 ->
          let r = float_of_value v in
          let z = F.integer_of_real r in
          prop_of_int (F.modulo i2 z)
            (* TODO : specify non-modulo Cf. ISO-C 6.3.1.4 *)
      | C_pointer t1 , C_pointer t2 ->
          prop_of_loc (M.cast_loc_to_loc t1 t2 (loc_of_value v))
      | C_pointer t1 , C_int i2 ->
          prop_of_int (M.cast_loc_to_int t1 (loc_of_value v) i2)
      | C_int i1 , C_pointer t2 ->
          prop_of_loc (M.cast_int_to_loc i1 (int_of_value v) t2)
      | a,b ->
          WpLog.not_yet_implemented "cast from %a to %a"
            pp_object a pp_object b



  (* ----------------------------------------------------------------------- *)
  (* --- Constants                                                       --- *)
  (* ----------------------------------------------------------------------- *)

  (* [expr_const m c] interprets a constant [c] in memory [m].*)
  let expr_const mem = function
    | CInt64(k,ik,_) ->
        M.V_int(Ctypes.c_int ik,F.e_icst (My_bigint.to_string k))
    | CChr c ->
        M.V_int(Ctypes.c_char (),F.e_icst (Int64.to_string (Ctypes.char c)))
    | CReal(f,fk,_)  ->
        M.V_float(Ctypes.c_float fk, F.e_float f)
    | CEnum e ->
        !expr_rec mem e.eival
    | CWStr _        ->
        WpLog.not_yet_implemented "wide character string constant"
    | CStr s         ->
        WpLog.not_yet_implemented "character string constant (%S)" s

  let prop_const mem = function
    | CInt64(k,_,_) ->
        if My_bigint.equal k My_bigint.zero then F.p_false else F.p_true
    | CChr c ->
        if c ='0' then F.p_false else F.p_true
    | CReal(f,_,_) ->
        if f = 0.0 then F.p_false else F.p_true
    | CEnum e ->
        !prop_rec mem e.eival
    | CWStr _ -> F.p_false (* pointer to constant string is non-null *)
    | CStr  _ -> F.p_false (* pointer to constant string is non-null *)

  (* ----------------------------------------------------------------------- *)
  (* --- Address                                                         --- *)
  (* ----------------------------------------------------------------------- *)

  (* [shift_loc mem l typ_l path] interprets [path] in memory [mem] from
     the location [l] of type [typ_l]. *)
  let rec shift_loc (mem:M.mem) l typ_l = function
    | NoOffset -> l
    | Field(f,next) ->
        shift_loc mem (M.field l f) f.ftype next
    | Index(e,next) ->
        let v = !expr_rec mem e in
        let k = int_of_value v in
        let typ_elt = Cil.typeOf_array_elem typ_l in
        shift_loc mem (M.index l (Ctypes.object_of typ_elt) k) typ_elt next

  let typeOf_array_elem = function
    | C_array arr -> object_of arr.arr_element
    | t -> WpLog.fatal 
	"[typeOf_array_elem] of non array type %a" Ctypes.pp_object t

  (* [addr mem l] interprets the left-value [l] as memory path
     (address) in the memory of [mem].*)
  let addr mem l =
    match l with
      | (Var x,off) ->
          let te = x.vtype in
          shift_loc mem (M.cvar mem x) te off
      | (Mem e,off) -> 
          let te = Cil.typeOf e in
          let tl = Cil.typeOf_pointed te in
          let loc = loc_of_value (!expr_rec mem e) in
          shift_loc mem loc tl off

  (* [startof mem t_elt lv] compute the location of l-value [lv], casted
     as pointer to an object of type [t] *)
  let startof mem t_elt lv =
    M.startof (addr mem lv) t_elt

  (* ---------------------------------------------------------------------- *)
  (* --- Binary Expressions                                             --- *)
  (* ---------------------------------------------------------------------- *)

  let int_operator iota = function
    | PlusA  -> A.i_op iota Formula.Iadd
    | MinusA -> A.i_op iota Formula.Isub
    | Mult   -> A.i_op iota Formula.Imul
    | Div    -> A.i_op iota Formula.Idiv
    | Mod    -> A.i_op iota Formula.Imod
    | BAnd   -> A.bits_and iota
    | BXor   -> A.bits_xor iota
    | BOr    -> A.bits_or iota
    | Shiftlt -> A.bits_lshift iota
    | Shiftrt -> A.bits_rshift iota
    | _ -> WpLog.fatal "[int_operator] non integer operator"

  let float_operator phi = function
    | PlusA  -> R.f_op phi Formula.Radd
    | MinusA -> R.f_op phi Formula.Rsub
    | Mult   -> R.f_op phi Formula.Rmul
    | Div    -> R.f_op phi Formula.Rdiv
    | _ -> WpLog.fatal "[float_operator] non float operator"

  let icmp_operator iota op x y =
    match op with
      | Eq -> A.i_cmp iota Formula.Ceq x y
      | Ne -> A.i_cmp iota Formula.Cneq x y
      | Lt -> A.i_cmp iota Formula.Clt x y
      | Le -> A.i_cmp iota Formula.Cleq x y
      | Ge -> A.i_cmp iota Formula.Cleq y x
      | Gt -> A.i_cmp iota Formula.Clt y x
      | _ -> WpLog.fatal "[icmp_operator] non integer comparator"

  let prop_icmp op x y =
    match op with
      | Eq -> F.p_icmp Formula.Ceq x y
      | Ne -> F.p_icmp Formula.Cneq x y
      | Lt -> F.p_icmp Formula.Clt x y
      | Le -> F.p_icmp Formula.Cleq x y
      | Ge -> F.p_icmp Formula.Cleq y x
      | Gt -> F.p_icmp Formula.Clt y x
      | _ -> WpLog.fatal "[prop_icmp] non integer relation"

  let fcmp_operator phi op x y =
    match op with
      | Eq -> R.f_cmp phi Formula.Ceq x y
      | Ne -> R.f_cmp phi Formula.Cneq x y
      | Lt -> R.f_cmp phi Formula.Clt x y
      | Le -> R.f_cmp phi Formula.Cleq x y
      | Ge -> R.f_cmp phi Formula.Cleq y x
      | Gt -> R.f_cmp phi Formula.Clt y x
      | _ -> WpLog.fatal "[fcmp_operator] non float comparator"

  let prop_rcmp op x y =
    match op with
      | Eq -> F.p_rcmp Formula.Ceq x y
      | Ne -> F.p_rcmp Formula.Cneq x y
      | Lt -> F.p_rcmp Formula.Clt x y
      | Le -> F.p_rcmp Formula.Cleq x y
      | Ge -> F.p_rcmp Formula.Cleq y x
      | Gt -> F.p_rcmp Formula.Clt y x
      | _ -> WpLog.fatal "[prop_rcmp] non real relation"


  let pcmp_operator op x y =
    match op with
      | Eq -> M.equal_loc_bool x y
      | Ne -> F.e_not ( M.equal_loc_bool x y)
      | Lt -> M.lt_loc_bool x y
      | Le -> M.le_loc_bool x y
      | Ge -> M.le_loc_bool y x
      | Gt -> M.lt_loc_bool y x
      | _ -> WpLog.fatal "[pcmp_operator] non comparator"


   let pcmp_rel op x y =
    match op with
      | Eq -> M.equal_loc x y
      | Ne -> F.p_not (M.equal_loc x y)
      | Lt -> M.lt_loc x y
      | Le -> M.le_loc x y
      | Ge -> M.le_loc y x
      | Gt -> M.lt_loc y x
      | _ -> WpLog.fatal "[pcmp_rel] non relation"

  (* special pointer arithmetic interpretation of Zero. *)
  let expr_rec_spec_null mem e1 =
    if Cil.isZero e1 then
      let t = Ctypes.object_of (Cil.typeOf e1) in
      M.V_pointer(t, M.null) else (!expr_rec mem e1)

  (*Interpretation of pointer comparisons. *)
  let expr_cond_cmp_ptr mem cmpop e1 e2 =
    let t1 = loc_of_value (expr_rec_spec_null mem e1) in
    let t2 = loc_of_value (expr_rec_spec_null mem e2) in
    pcmp_operator cmpop t1 t2

  let prop_cmp_ptr mem cmpop e1 e2 =
    let t1 = loc_of_value (expr_rec_spec_null mem e1) in
    let t2 = loc_of_value (expr_rec_spec_null mem e2) in
    pcmp_rel cmpop t1 t2

  (*Interpretation of arithmetic comparisons. *)
  let expr_cond_cmp_arith mem cmpop ct1 e1 ct2 e2 =
    let ctr = Ctypes.promote ct1 ct2 in
    begin
      match ctr , ct1 , ct2 with
        | C_int ir , C_int i1 , C_int i2 ->
            let t1 = expr_int mem i1 ir e1 in
            let t2 = expr_int mem i2 ir e2 in
            icmp_operator ir cmpop t1 t2
        | C_float fr, C_float f1, C_float f2 ->
            let t1 = expr_float mem f1 fr e1 in
            let t2 = expr_float mem f2 fr e2 in
            fcmp_operator fr cmpop t1 t2
        | _ -> WpLog.fatal "[expr_cond_cmp_arith] non arithmetics comparison"
    end

  (* [expr_cond_cmp mem cmpop t1 e1 t2 e2] returns the interpreation
     of the comparison [cmpop] of expression [e1] of type [t1] and
     expression [e2] of type [t2] in memory [mem]. *)
  let expr_cond_cmp  mem cmpop t1 e1 t2 e2 =
    let ct1 = Ctypes.object_of t1 in
    let ct2 = Ctypes.object_of t2 in
    begin
      match ct1,ct2 with
        |  C_pointer _,C_pointer _ -> expr_cond_cmp_ptr mem cmpop e1 e2
        | _ -> expr_cond_cmp_arith mem cmpop ct1  e1 ct2 e2
    end



  let prop_cmp mem cmpop t1 e1 t2 e2 =
    let ct1 = Ctypes.object_of t1 in
    let ct2 = Ctypes.object_of t2 in
    match ct1,ct2 with
      |  C_pointer _,C_pointer _ ->
           (prop_cmp_ptr mem cmpop  e1 e2)
      | _ ->
          let ctr = Ctypes.promote ct1 ct2 in
          begin
            match ctr , ct1 , ct2 with
              | C_int ir , C_int i1 , C_int i2 ->
                  let t1 = expr_int mem i1 ir e1 in
                  let t2 = expr_int mem i2 ir e2 in
                  prop_icmp cmpop t1 t2
              | C_float fr, C_float f1, C_float f2 ->
                  let t1 = expr_float mem f1 fr e1 in
                  let t2 = expr_float mem f2 fr e2 in
                  prop_rcmp cmpop t1 t2
              | _ -> WpLog.fatal "[prop_cmp] non arithmetic relation"
          end

  (* Interpretation of integer arithmetics. *)
  let expr_int_operator mem ir binop i1 e1 i2 e2 =
    let t1 = expr_int mem i1 ir e1 in
    let t2 = expr_int mem i2 ir e2 in
    int_operator ir binop t1 t2


  (* Interpretation of float arithmetics *)
  let expr_float_operator mem fr binop f1 e1 f2 e2 =
    let t1 = expr_float mem f1 fr e1 in
    let t2 = expr_float mem f2 fr e2 in
    float_operator fr binop t1 t2


  (* [expr_binop mem binop tr e1 t1 e2 t2] interprets the binary
     operation [binop] as a value of type [tr] of expression [e1] of
     type [t1] and expression [e2] of type [t2].*)
  let expr_binop mem binop tr e1 t1 e2 t2 =
    match binop with
      | IndexPI | PlusPI ->
          let ty = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let idx = int_of_value (!expr_rec mem e2) in
          M.V_pointer(ty,M.shift loc ty idx)

      | MinusPI ->
          let ty = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let neg_idx = int_of_value (!expr_rec mem e2) in
          let idx =  F.e_ineg neg_idx in
          M.V_pointer(ty,M.shift loc ty idx)

      | MinusPP ->
          let iota = c_int_of_typ tr in
          value_of_integer iota
            (M.minus_loc
               (loc_of_value (!expr_rec mem e1))
               (loc_of_value (!expr_rec mem e2)))

      | (Eq | Ne | Ge | Le | Gt | Lt) ->
          value_of_boolean (expr_cond_cmp mem binop t1 e1 t2 e2)

      | PlusA | MinusA | Mult | Div | Mod
      | BAnd | BXor | BOr | Shiftlt | Shiftrt ->
          let ct1 = Ctypes.object_of t1 in
          let ct2 = Ctypes.object_of t2 in
          let ctr = Ctypes.object_of tr in
          begin
            match ctr , ct1 , ct2 with
              | C_int ir , C_int i1 , C_int i2 ->
                  M.V_int(ir,expr_int_operator mem ir binop i1 e1 i2 e2)
              | C_float fr, C_float f1, C_float f2 ->
                  M.V_float(fr,expr_float_operator mem fr binop f1 e1 f2 e2)
              | _ -> WpLog.fatal "non arithmetics arguments"
          end

      | LAnd ->
          value_of_boolean (F.e_and (!cond_rec mem e1) (!cond_rec mem e2))
      | LOr ->
          value_of_boolean (F.e_or  (!cond_rec mem e1) (!cond_rec mem e2))

  (* [cond_binop mem binop tr e1 t1 e2 t2] interprets the binary
     operation [binop] as a boolean of expression [e1] of type [t1]
     and expression [e2] of type [t2].*)
  let cond_binop mem binop tr e1 t1 e2 t2 =
    match binop with
      | IndexPI | PlusPI ->
          let te = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let idx = int_of_value (!expr_rec mem e2) in
          boolean_of_loc (M.shift loc te idx)

      | MinusPI ->
          let te = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let neg_idx = int_of_value (!expr_rec mem e2) in
          let idx =  F.e_ineg neg_idx in
          boolean_of_loc (M.shift loc te idx)

      | MinusPP ->
          boolean_of_integer
            (M.minus_loc
               (loc_of_value (!expr_rec mem e1))
               (loc_of_value (!expr_rec mem e2)))

      | (Eq | Ne | Ge | Le | Gt | Lt) ->
          expr_cond_cmp mem binop t1 e1 t2 e2

      | PlusA | MinusA | Mult | Div | Mod
      | BAnd | BXor | BOr | Shiftlt | Shiftrt ->
          let ct1 = Ctypes.object_of t1 in
          let ct2 = Ctypes.object_of t2 in
          let ctr = Ctypes.object_of tr in
          begin
            match ctr , ct1 , ct2 with
              | C_int ir , C_int i1 , C_int i2 ->
                  boolean_of_int (expr_int_operator mem ir binop i1 e1 i2 e2)
              | C_float fr, C_float f1, C_float f2 ->
                  boolean_of_float
                    (expr_float_operator mem fr binop f1 e1 f2 e2)
              | _ -> WpLog.fatal "non arithmetics arguments"
          end

      | LAnd -> F.e_and (!cond_rec mem e1) (!cond_rec mem e2)
      | LOr ->  F.e_or  (!cond_rec mem e1) (!cond_rec mem e2)


  let prop_binop mem binop tr e1 t1 e2 t2 =
    match binop with
      | IndexPI | PlusPI ->
	  let te = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let idx = int_of_value (!expr_rec mem e2) in
          prop_of_loc (M.shift loc te idx)

      | MinusPI ->
	  let te = Ctypes.object_of_pointed (Ctypes.object_of t1) in
          let loc = loc_of_value (!expr_rec mem e1) in
          let neg_idx = int_of_value (!expr_rec mem e2) in
          let idx =  F.e_ineg neg_idx in
          prop_of_loc (M.shift loc te idx)

      | MinusPP ->
          prop_of_integer
            (M.minus_loc
               (loc_of_value (!expr_rec mem e1))
               (loc_of_value (!expr_rec mem e2)))

      | (Eq | Ne | Ge | Le | Gt | Lt) ->
         prop_cmp mem binop t1 e1 t2 e2

      | PlusA | MinusA | Mult | Div | Mod
      | BAnd | BXor | BOr | Shiftlt | Shiftrt ->
          let ct1 = Ctypes.object_of t1 in
          let ct2 = Ctypes.object_of t2 in
          let ctr = Ctypes.object_of tr in
          begin
            match ctr , ct1 , ct2 with
              | C_int ir , C_int i1 , C_int i2 ->
                  prop_of_int (expr_int_operator mem ir binop i1 e1 i2 e2)
              | C_float fr, C_float f1, C_float f2 ->
                  prop_of_float
                    (expr_float_operator mem fr binop f1 e1 f2 e2)
              | _ -> WpLog.fatal "non arithmetics arguments"
          end

      | LAnd -> F.p_and (!prop_rec mem e1) (!prop_rec mem e2)
      | LOr ->  F.p_or  (!prop_rec mem e1) (!prop_rec mem e2)


  (* ----------------------------------------------------------------------- *)
  (* --- Unary Operator                                                  --- *)
  (* ----------------------------------------------------------------------- *)

  (* [cond_unop mem op tyr e te ] interprets the unary operation [op]
     as a boolean of expression [e] of type [te].*)
  let cond_unop mem op tyr e te =
     let ct1 = object_of te in
     let ctr = object_of tyr in
    match op with
      | Neg  ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 ->
                  boolean_of_int (A.i_neg ir (expr_int mem i1 ir e))
              | C_float fr , C_float f1 ->
                  boolean_of_float (R.f_neg fr (expr_float mem f1 fr e))
              | _ ->WpLog.fatal "non arithmetics argument"
          end
      | BNot ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 ->
                  boolean_of_int (A.bits_not ir (expr_int mem i1 ir e))
              | _ -> WpLog.fatal "non integer argument"
          end
      | LNot ->
          let term = !expr_rec mem e in
          begin
            match ct1 with
              |  C_int i1     ->
                   (A.i_cmp i1 Formula.Ceq (int_of_value term) F.i_zero)
              |  C_float f1   ->
                   (R.f_cmp f1 Formula.Ceq (float_of_value term) F.r_zero)
              |  C_pointer _  ->
                   (M.is_null (loc_of_value term))
              | _ -> WpLog.fatal "non arithmetics nor pointer argument"

          end

  (* [expr_unop mem op tyr e te ] interprets the unary operation [op]
     as a value of type [tyr] of expression [e] of type [te].*)
  let expr_unop mem op tyr e te =
    let ct1 = object_of te in
    let ctr = object_of tyr in
    match op with
      | Neg  ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 ->
                  M.V_int(ir,A.i_neg ir (expr_int mem i1 ir e))
              | C_float fr , C_float f1 ->
                  M.V_float(fr,R.f_neg fr (expr_float mem f1 fr e))
              | _ -> WpLog.fatal "non arithmetic argument"
          end
      | BNot ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 ->
                  M.V_int(ir,expr_int mem i1 ir e)
              | _ -> WpLog.fatal "non intger argument"
          end
      | LNot ->
          let term = !expr_rec mem e in
          begin
            match ct1 with
              |  C_int i1 ->
                   value_of_boolean (A.i_cmp i1 Formula.Ceq
                             (int_of_value term) F.i_zero)
              |  C_float f1 ->
                   value_of_boolean (R.f_cmp f1 Formula.Ceq
                             (float_of_value term) F.r_zero)
              |  C_pointer _ ->
                   value_of_boolean
                     (M.is_null (loc_of_value term))
              | _ -> WpLog.fatal "non arithmetic nor pointer argument"
          end

  let prop_unop mem op tyr e te =
    let ct1 = object_of te in
    let ctr = object_of tyr in
    match op with
      | Neg  ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 ->
                  prop_of_int (A.i_neg ir (expr_int mem i1 ir e))
              | C_float fr , C_float f1 ->
                  prop_of_float (R.f_neg fr (expr_float mem f1 fr e))
              | _ -> WpLog.fatal "non arithmetic argument"
          end
      | BNot ->
          begin
            match ctr,ct1 with
              | C_int ir , C_int i1 -> not_of_int (expr_int mem i1 ir e)
              | _ -> WpLog.fatal "non integer argument"
          end
      | LNot -> let term = !expr_rec mem e in not_of_value term

  (* ----------------------------------------------------------------------- *)
  (* --- Expressions                                                     --- *)
  (* ----------------------------------------------------------------------- *)

  let rec expr mem e =
    match (Cil.stripInfo e).enode with
      | Info _ -> WpLog.fatal "non translation for info type expression"
      | Const (cnst) -> expr_const mem cnst
      | CastE (ty,e) ->
          if Cil.isPointerType ty && Cil.isZero e then
            (let t = Ctypes.object_of_pointed (Ctypes.object_of ty) in
            M.V_pointer(t, M.null))
          else
          expr_cast (Cil.typeOf e) ty (expr mem e)
      | BinOp (op, e1, e2, ty) ->
          expr_binop mem op ty
            e1 (Cil.typeOf e1)
            e2 (Cil.typeOf e2)
      | UnOp (op, e1, ty) ->
          expr_unop mem op ty e1 (Cil.typeOf e1)

      | Lval lval ->
          let t = Cil.typeOf e in
          let l = addr mem lval in
          M.load mem (Ctypes.object_of t) l

      | StartOf lval ->
          let ty_elt = 
	    Ctypes.object_of_pointed (Ctypes.object_of (Cil.typeOf e)) in
          M.V_pointer(ty_elt,startof mem ty_elt lval)

      | AddrOf lval ->
	  let ty_elt = 
	    Ctypes.object_of_pointed (Ctypes.object_of (Cil.typeOf e))
          in
          M.V_pointer(ty_elt,addr mem lval)

      | AlignOfE _ | AlignOf _
      | SizeOfE _ | SizeOf _ | SizeOfStr _ ->
          let e' = Cil.constFold true e in
          match e'.enode with
            | Const _ -> expr mem e'
            | _ ->
                WpLog.not_yet_implemented "sizeof(%a)"
                  !Ast_printer.d_exp e


  (* ----------------------------------------------------------------------- *)
  (* --- Conditional Expression                                          --- *)
  (* ----------------------------------------------------------------------- *)

  let cond mem e =
    match (Cil.stripInfo e).enode with

      | BinOp (op, e1, e2, ty) ->
          cond_binop mem op ty e1 (Cil.typeOf e1) e2 (Cil.typeOf e2)
      | UnOp (op, e1, ty)      ->
          cond_unop mem op ty e1 (Cil.typeOf e1)

      | _ -> boolean_of_value (expr mem e)




  (* ----------------------------------------------------------------------- *)
  (* --- Predicative translation of Conditional                          --- *)
  (* ----------------------------------------------------------------------- *)

  let prop mem e =
     match (Cil.stripInfo e).enode with
   | Info _ -> WpLog.fatal "non translation for info type expression"
      | Const (cnst) -> prop_const mem cnst
      | CastE (ty,e) ->
          if Cil.isPointerType ty && Cil.isZero e then
            F.p_false
          else
            prop_cast (Cil.typeOf e) ty (expr mem e)

      | BinOp (op, e1, e2, ty) ->
          prop_binop mem op ty
            e1 (Cil.typeOf e1)
            e2 (Cil.typeOf e2)
      | UnOp (op, e1, ty) ->
          prop_unop mem op ty e1 (Cil.typeOf e1)

      | Lval lval ->
          let t = Cil.typeOf e in
          let l = addr mem lval in
          prop_of_value (M.load mem (Ctypes.object_of t) l)

      | StartOf lval ->
          let ty_elt = 
	    Ctypes.object_of_pointed (Ctypes.object_of (Cil.typeOf e))
	  in
          prop_of_loc (startof mem ty_elt lval)

      | AddrOf lval ->
          prop_of_loc (addr mem lval)

      | AlignOfE _ | AlignOf _
      | SizeOfE _ | SizeOf _ | SizeOfStr _ ->
          let e' = Cil.constFold true e in
          match e'.enode with
            | Const c -> prop_const mem c
            | _ -> WpLog.not_yet_implemented "sizeof(%a)"
                !Ast_printer.d_exp e

  (* ----------------------------------------------------------------------- *)
  (* ---  Recursion Bindings                                             --- *)
  (* ----------------------------------------------------------------------- *)

  let () =
    begin
      expr_rec := expr ;
      cond_rec := cond ;
      prop_rec := prop;
    end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
