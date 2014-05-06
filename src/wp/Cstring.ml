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
(* --- C Strings                                                          --- *)
(* -------------------------------------------------------------------------- *)

open Definitions
open Qed.Logic
open Lang

type cst = 
  | C_str of string
  | W_str of int64 list

module STR = 
struct
  type t = cst
  let compare = Pervasives.compare (* only comparable types *)
  let pretty fmt = function
    | C_str s -> Format.fprintf fmt "%S" s
    | W_str _ -> Format.fprintf fmt "\"L<...>\""
  let hash (c:t) = Hashtbl.hash c land 0xFFFF
end

let pretty = STR.pretty

let cluster () =
  Definitions.cluster ~id:"cstring" ~title:"String Literals" ()

module LIT = Model.Generator(STR)
  (struct
     type key = cst
     type data = int * F.term
     let name = "Cstring.Litterals"

     let hid = Hashtbl.create 31

     let rec lookup id =
       if id=0 || Hashtbl.mem hid id 
       then lookup (succ id)
       else (Hashtbl.add hid id () ; id)

     let export_literal prefix lfun str = 
       let chars = ref [] in
       let array = F.e_fun lfun [] in
       let n = String.length str in
       for i = 0 to n do
	 let a = F.e_get array (F.e_int i) in
	 let c = 
	   if i = n 
	   then F.e_zero
	   else F.e_int (int_of_char str.[i]) 
	 in
	 chars := (F.p_equal a c) :: !chars ;
       done ;
       define_lemma {
	 l_name = prefix ^ "_literal" ;
	 l_cluster = cluster () ;
	 l_assumed = true ;
	 l_types = 0 ;
	 l_forall = [] ;
	 l_triggers = [] ;
	 l_lemma = F.p_conj (List.rev !chars) ;
       }

     let compile s =
       let id = lookup (STR.hash s) in
       let lfun = Lang.generated_f ~result:(Array(Int,Int)) "Lit_%04X" id in
       (** Since its a generated it is the unique name given ["Lit_%04X" id] *)
       let prefix = Lang.Fun.debug lfun in
       define_symbol {
	 d_lfun = lfun ;
	 d_cluster = cluster () ;
	 d_types = 0 ;
	 d_params = [] ;
	 d_definition = Logic (Array(Int,Int)) ;
       } ;
       if Wp_parameters.Literals.get () then
	 begin match s with
	   | C_str str -> export_literal prefix lfun str
	   | W_str _ -> 
	       Wp_parameters.warning ~current:false ~once:true
		 "Content of wide string literals not exported."
	 end ;
       id , F.e_fun lfun []
	 
   end)

let str_id s = fst (LIT.get s)
let str_val s = snd (LIT.get s) 

let str_len s n = match s with
  | C_str s -> F.p_equal n (F.e_int (String.length s))
  | W_str _ -> F.p_lt F.e_zero n

let char_at s k = F.e_get (str_val s) k
