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

(*

  Performs several analyses after Java typing and before interpretation into Jessie code.

  1) determines which structure type to introduce for arrays
  2) disambiguates names:
     . different constructors for the same class are named
         Class_typearg1_..._typeargn
     . methods
         * with same names in different classes or interfaces
             Class_or_Interface_method_name
         * with same names in same class or interface
             Class_or_Interface_method_name_typearg1_..._typeargn

*)

open Java_pervasives
open Java_env
open Java_tast
open Format

let array_struct_table = Hashtbl.create 17

let name_base_type t =
  match t with
    | Tboolean -> "boolean"
    | Tchar -> "char"
    | Tbyte -> "byte"
    | Tinteger -> "integer" 
    | Tshort -> "short"
    | Tlong -> "long"
    | Tint -> "int" 
    | Tunit -> "unit"
    | Tfloat -> "float"
    | Treal -> "real"
    | Tdouble -> "double"
    | Tstring -> "string"

let rec name_type t =
  match t with
    | JTYbase t -> name_base_type t
    | JTYclass(_,c) -> c.class_info_name
    | JTYinterface i -> i.interface_info_name
    | JTYarray (_, ty) -> name_type ty ^ "A"
    | JTYnull -> assert false
    | JTYlogic _ -> assert false

let rec intro_array_struct t =
  let n = name_type t in 
  try
    let _ = Hashtbl.find array_struct_table n in ()
  with Not_found ->
    Java_options.lprintf "Adding array struct for type %a under name %s@." 
      Java_typing.print_type t n;
      (* array structs indexed by name rather than by type,
	 to void generation of two structs for nullable and non_null types
	 of the same name.
	 - Nicolas R. *)
      Hashtbl.add array_struct_table n (t, n ^ "M", n ^ "P")

let term t = () (* TODO *)

let assertion a = () (* TODO *)

let rec expr e = 
  match e.java_expr_node with
    | JElit l -> ()
    | JEincr_local_var(op,v) -> ()
    | JEincr_field(op,e1,fi) -> expr e1
    | JEun (_, e1) -> expr e1
    | JEbin (e1, _, e2) | JEincr_array (_, e1, e2) -> expr e1; expr e2 
    | JEvar vi -> ()
    | JEstatic_field_access(ci,fi) -> ()
    | JEfield_access(e1,fi) -> expr e1
    | JEarray_length e -> 
	begin
	  match e.java_expr_type with
	    | JTYarray (_, ty) -> intro_array_struct ty 
	    | _ -> assert false
	end
    | JEarray_access(e1,e2) -> 
	expr e1; expr e2;
	begin
	  match e1.java_expr_type with
	    | JTYarray (_, ty) -> intro_array_struct ty
	    | _ -> assert false
	end
    | JEassign_local_var (_, e)
    | JEassign_local_var_op (_, _, e)
    | JEassign_static_field (_, e) 
    | JEassign_static_field_op (_, _, e) -> expr e
    | JEassign_field(e1,fi,e2) -> expr e1; expr e2
    | JEassign_field_op(e1,fi,op,e2) -> expr e1; expr e2
    | JEif(e1,e2,e3) -> expr e1; expr e2; expr e3
    | JEassign_array(e1,e2,e3) 
    | JEassign_array_op(e1,e2,_,e3) -> 
	expr e1; expr e2; expr e3;
	begin
	  match e1.java_expr_type with
	    | JTYarray (_, ty) -> intro_array_struct ty
	    | _ -> 
		eprintf "unexpected type: e1:%a e2:%a e3:%a@." 
		  Java_typing.print_type e1.java_expr_type
		  Java_typing.print_type e2.java_expr_type
		  Java_typing.print_type e3.java_expr_type;
		assert false
	end
    | JEcall(e,mi,args) ->
	expr e;	List.iter expr args
    | JEconstr_call (e, _, args) ->
	expr e; List.iter expr args
    | JEstatic_call(mi,args) ->
	List.iter expr args
    | JEnew_array(ty,dims) ->
	List.iter expr dims
	(* intro_array_struct ty ??? *)
    | JEnew_object(ci,args) ->
	List.iter expr args
    | JEinstanceof(e,_)
    | JEcast(_,e) -> expr e

let do_initializer i = 
  match i with
    | JIexpr e -> expr e
    | _ -> assert false (* TODO *)

let switch_label = function
  | Java_ast.Default -> ()
  | Java_ast.Case e -> expr e
  
let behavior (id,assumes,throws,assigns,ensures) =
  Option_misc.iter assertion assumes;
  Option_misc.iter (fun (_,l) -> List.iter term l) assigns;
  assertion ensures

let loop_annot annot =
  assertion annot.loop_inv;
  List.iter (fun (_,a) -> assertion a) annot.behs_loop_inv;
  Option_misc.iter term annot.loop_var

let rec statement s =
  match s.java_statement_node with
    | JSskip 
    | JSreturn_void
    | JSbreak _ | JScontinue _ -> ()
    | JSblock l -> List.iter statement l	  
    | JSvar_decl (vi, init, s) ->
	Option_misc.iter do_initializer init;
	statement s
    | JSif (e, s1, s2) -> expr e; statement s1; statement s2
    | JSdo (s, annot, e) ->
	statement s; loop_annot annot; expr e
    | JSwhile(e,annot,s) ->
	  expr e; loop_annot annot; statement s
    | JSfor (el1, e, annot, el2, body) ->
	List.iter expr el1;
	expr e;
	loop_annot annot;
	List.iter expr el2;
	statement body
    | JSfor_decl(decls,e,annot,sl,body) ->
	List.iter 
	  (fun (_,init) -> Option_misc.iter do_initializer init) decls;
	expr e;
	loop_annot annot;
	List.iter expr sl;
	statement body
    | JSthrow e
    | JSreturn e 
    | JSexpr e -> expr e
    | JSassert(id,e) -> assertion e
    | JSswitch(e,l) -> 
	expr e;
	List.iter (fun (labels,b) ->
		     List.iter switch_label labels;
		     List.iter statement b)
	  l
    | JStry(s, catches, finally) ->
	List.iter statement s;
	List.iter (fun (_,s) -> List.iter statement s) catches;
	Option_misc.iter (List.iter statement) finally
    | JSstatement_spec(req,dec,behs,s) ->
	Option_misc.iter assertion req;
	Option_misc.iter term dec;
	List.iter behavior behs;
	statement s

let param vi =
  match vi.java_var_info_type with
    | JTYarray (_, ty) -> intro_array_struct ty
    | _ -> ()

let string_of_parameters vil =
  (List.fold_right
     (fun (vi, _) acc -> "_" ^ name_type vi.java_var_info_type ^ acc) vil "")

let disambiguates_method_names () =
  let methods_list =
    Hashtbl.fold (fun _ mt acc -> mt :: acc) Java_typing.methods_table []
  in
  let methods_list =
    List.sort 
      (fun mt1 mt2 -> 
	 let mi1 = mt1.Java_typing.mt_method_info in
	 let mi2 = mt2.Java_typing.mt_method_info in
	   String.compare mi1.method_info_trans_name mi2.method_info_trans_name)
      methods_list 
  in
  let rec disambiguates methods_list =
    match methods_list with
      | [] | [_] -> ()
      | mt1::(mt2::_ as tl) ->
	  let mi1 = mt1.Java_typing.mt_method_info in
	  let mi2 = mt2.Java_typing.mt_method_info in
	    if mi1.method_info_trans_name = mi2.method_info_trans_name then
	      begin
		mi1.method_info_trans_name <-
		  mi1.method_info_trans_name ^
		  string_of_parameters mi1.method_info_parameters;
		mi2.method_info_trans_name <-
		  mi2.method_info_trans_name ^
		  string_of_parameters mi2.method_info_parameters;
	      end;
	    disambiguates tl
  in
    disambiguates methods_list;
    List.iter
      (fun mt -> Hashtbl.replace Java_typing.methods_table
	 mt.Java_typing.mt_method_info.method_info_tag mt)
      methods_list

let do_method mi req behs body =
(*
  Option_misc.iter assertion req;
  ... behs
*)
  mi.method_info_trans_name <-
    (get_method_info_class_or_interface_name mi) ^ "_" ^
    mi.method_info_name;
  disambiguates_method_names ();
  List.iter param (List.map fst mi.method_info_parameters);
  Option_misc.iter (List.iter statement) body;
  Option_misc.iter param mi.method_info_result


let do_constructor ci req behs body =
(*
  let l = ci.constr_info_class.class_info_constructors in
  if List.length l >= 2 then
    begin
*)
      ci.constr_info_trans_name <-
	"cons_" ^ ci.constr_info_class.class_info_name ^
	string_of_parameters ci.constr_info_parameters;
(*
    end;
*)
  List.iter statement body

let do_field fi =
  match fi.java_field_info_type with
    | JTYarray (_, ty) -> intro_array_struct ty
    | _ -> ()

let do_type ty =
  match ty with
    | TypeClass ci -> 
	List.iter do_field ci.class_info_fields
    | TypeInterface ii -> 
	List.iter do_field ii.interface_info_fields


(*
Local Variables: 
compile-command: "make -j -C .. bin/krakatoa.byte"
End: 
*)

