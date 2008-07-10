(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(*
  
ledit ocaml nums.cma

setenv OCAMLRUNPARAM bt 

ocamlc -g -o test_big_int nums.cma my_big_int.ml
./test_big_int

*)

include Big_int

(* Nombre de bits significatifs dans un "word" de "Big_int" *)
let nb_digits_of_big_int =
  let r =
    let rec nb_digits y =
      if 1 = Big_int.num_digits_big_int (Big_int.power_int_positive_int 2 y)
      then nb_digits (y + 1)
      else y
    in nb_digits 1
  in r
  
let base = Big_int.power_int_positive_int 2 nb_digits_of_big_int
let base16bits = Big_int.power_int_positive_int 2 16


(* Soit            X tel que x = let f a x   =(a * base) + x        in List.fold_left f 0 X,
 *                 Y tel que y = let f a y   =(a * base) + y        in List.fold_left f 0 Y,
 * alors map2_base base op x y = let f a x y =(a * base) + (op x y) in List.fold_left f 0 X Y
*)
let map2_base b op x y =
  let rec map2_base_rec a x y =
    let (qx, mx) = Big_int.quomod_big_int x b
    and (qy, my) = Big_int.quomod_big_int y b
    in let res_m = op mx my
       and res_q =
        if (Big_int.eq_big_int Big_int.zero_big_int qx)
          && (Big_int.eq_big_int Big_int.zero_big_int qy)
        then a
        else map2_base_rec a qx qy
    in Big_int.add_big_int (Big_int.mult_big_int res_q b) res_m
  in map2_base_rec Big_int.zero_big_int x y


let bitwise_op_positive_big_int op x y =
  assert (Big_int.ge_big_int x Big_int.zero_big_int);
  assert (Big_int.ge_big_int y Big_int.zero_big_int);
  let g =
    let f u v = assert(Big_int.is_int_big_int u) ;
      assert(Big_int.is_int_big_int v) ;
      let r = op (Big_int.int_of_big_int u) (Big_int.int_of_big_int v)
      in Big_int.big_int_of_int (r)
    in map2_base base16bits f 
  in let r = map2_base base g x y
  in assert (Big_int.ge_big_int r Big_int.zero_big_int);
    r


let lnot_big_int w = Big_int.minus_big_int (Big_int.succ_big_int w)
  
let shift_left_big_int x y = (* idem multiplication *) 
  Big_int.mult_big_int x (Big_int.power_int_positive_big_int 2 y)
    
let shift_right_big_int x y = (* idem division rounding to -oo *)
  Big_int.div_big_int x (Big_int.power_int_positive_big_int 2 y)

let power_two = 
  let h = Hashtbl.create 7 in
  fun k ->
      try
	Hashtbl.find h k
      with Not_found ->
	let p = Big_int.power_int_positive_int 2 k in
	Hashtbl.add h k p;
	p

let two_power y = 
    try
      let k = Big_int.int_of_big_int y in
      power_two k
    with Failure _ -> assert false

let log_shift_right_big_int x y = (* no meaning for negative value of x *)
  if (Big_int.lt_big_int x Big_int.zero_big_int)
  then raise (Invalid_argument "log_shift_right_big_int")
  else shift_right_big_int x y

let bitwise_op_big_int op x y =
  let (positive_x, op_sx) =
    if Big_int.gt_big_int Big_int.zero_big_int x
    then (lnot_big_int x, (fun u v -> op (lnot u) v))
    else (x, op)
  in let (positive_y, op_sx_sy) =
      if Big_int.gt_big_int Big_int.zero_big_int y
      then (lnot_big_int y, (fun u v -> op_sx u (lnot v)))
      else (y, op_sx)
  in let (positive_op_map, op_map) =
      if 0 = (op_sx_sy 0 0)
      then (op_sx_sy, (fun w -> w))
      else ((fun u v -> lnot (op_sx_sy u v)), lnot_big_int) 
  in op_map (bitwise_op_positive_big_int positive_op_map positive_x positive_y)


let land_big_int = bitwise_op_big_int (land)
let lor_big_int  = bitwise_op_big_int (lor)
let lxor_big_int = bitwise_op_big_int (lxor)

(* Get the value encoded from the 'first' to 'last' bit of 'x' :
   Shift right 'x' and apply a mask on it.
   The result is:  div (mod x (2**(last+1))) (2**first) *)
let bitwise_extraction first_bit last_bit x =
  assert (first_bit <= last_bit);(* first_bit <= last_bit *)
  assert (first_bit >= 0);       (* first_bit >= 0        *)
  let q = Big_int.div_big_int x (Big_int.power_int_positive_int 2 first_bit)
  in let r = Big_int.mod_big_int q (Big_int.power_int_positive_int 2 (1 + last_bit - first_bit))
  in r
       
(* Idem bitwise_extraction except it interprets the 'last' bit of 'x' as a bit of sign.
   Get the value encoded from the 'first' to 'last' bit of 'x' where the 'last' bit is a sign bit. *)
let bitwise_signed_extraction first_bit last_bit x =
  let r = bitwise_extraction first_bit last_bit x
  in if Big_int.ge_big_int r (Big_int.power_int_positive_int 2 (last_bit - first_bit))
    then (* last bit of x is set to 1, the result have to be a negative value *)
      (let r = lor_big_int r (Big_int.pred_big_int (Big_int.power_int_positive_int 2 (1 + last_bit - first_bit)))
      in assert (Big_int.lt_big_int r Big_int.zero_big_int); r)
    else r

(******************** TEST ******************)
(* test par iterations sur les entiers signés se codant sur "nb_bits+1" bits
   que l'on décalera "nb_declages" fois
   de "declage" bits *)
(*      
let test_bitwise_op_big_int nb_bits nb_declages declage=
  let test_int_bitwise_op_big_int x y op =
    let int_bitwise_op_big_int (op, txt) x y =
      (* Printf.printf "----\n<-int_bitwise_op_big_int: %s %s %s = %s\n"
         (string_of_int x) txt (string_of_int y) (string_of_int (op x y)) ; *)
      let r = bitwise_op_big_int op (Big_int.big_int_of_int x) (Big_int.big_int_of_int y)
      in (* Printf.printf "->int_bitwise_op_big_int= %s\n" (Big_int.string_of_big_int  r) ; *)
        assert (Big_int.eq_big_int r (Big_int.big_int_of_int (op x y))) ;
    in int_bitwise_op_big_int op  x    y ;
      int_bitwise_op_big_int op   x  (-y) ;
      int_bitwise_op_big_int op (-x)   y ;
      int_bitwise_op_big_int op (-x) (-y)
  in (* Vérification sur des "Big_int" que (X XOR -1) = NOT(X) *)
    Printf.printf "Test...\n" ;
    for i = 0 to ((1 lsl nb_bits) - 1) do 
      let u = ref (Big_int.big_int_of_int i)
      in for k = 0 to nb_declages do
          let test_xor w =
            let r = bitwise_op_big_int (lxor) w (Big_int.big_int_of_int (-1))
            in assert (Big_int.eq_big_int r (Big_int.minus_big_int (Big_int.succ_big_int w)))
          in test_xor !u ;
            test_xor (Big_int.minus_big_int !u);
            u := Big_int.add_big_int !u (Big_int.mult_big_int !u (Big_int.power_int_positive_int 2 declage)) ;
            u := Big_int.add_big_int !u (Big_int.mult_big_int !u (Big_int.power_int_positive_int 2 declage)) ;
            u := Big_int.add_big_int !u (Big_int.mult_big_int !u (Big_int.power_int_positive_int 2 declage)) ;
        done  
    done ;
    (* Vérification sur des "int" des opérations AND, OR et XOR *)    
    for i = 0 to ((1 lsl nb_bits) - 1) do 
      for j = 0 to ((1 lsl nb_bits) - 1) do
        let (u,v) = (ref i, ref j)
        in for k = 0 to nb_declages do
            List.iter (test_int_bitwise_op_big_int !u !v) [((land), " AND "); ((lor)," OR  ");((lxor)," XOR ")] ;
            u := (!u lsl declage) ;
            v := (!v lsl declage) ;        
          done 
      done  
    done  
;;
*)
      
(*
open Test_big_int;;
print_string ((Big_int.string_of_big_int (bitwise_op_big_int (lor) (Big_int.big_int_of_int (-1)) (Big_int.big_int_of_int (0))))^ "\n") ;;
 
(*
test_bitwise_op_big_int 4 5 7;;
test_bitwise_op_big_int 3 7 9;;
*)     
(* Conversion de x:Big_int dans une base *)
let decompose x base =
  let rec decompose_rec r x =
    let (qx, mx) = Big_int.quomod_big_int x base
    in if Big_int.eq_big_int qx Big_int.zero_big_int
       then (mx::r)
       else decompose_rec (mx::r) qx 
  in decompose_rec [] x
;;


(* Conversion inverse *)
let recompose x base =
  let f x y = Big_int.add_big_int (Big_int.mult_big_int y base) x
  in List.fold_left f Big_int.zero_big_int x
;; 


*)

(*
let () = print_endline 
  (string_of_big_int 
      (land_big_int (big_int_of_int 31) (big_int_of_int (-16))))
*)
