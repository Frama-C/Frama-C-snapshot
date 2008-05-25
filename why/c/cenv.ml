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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

open Format
open Coptions
open Ctypes
open Cutil
open Creport
open Info

(* Type equality (i.e. structural equality, but ignoring attributes) *)

let rec eq_type ty1 ty2 = 
  eq_type_node ty1.ctype_node ty2.ctype_node

and eq_type_node tn1 tn2 = match tn1, tn2 with
  | Tvoid, Tvoid
  | Tint _, Tint _ 
  | Tfloat _, Tfloat _ 
  | Tint _, Tenum _ 
  | Tenum _, Tint _ ->
      true
  | Tvar x1, Tvar x2 ->
      x1 = x2
  | Tarray (_,ty1,_), Tarray (_,ty2,_) ->
      eq_type ty1 ty2 
  | Tpointer (_,ty1), Tpointer (_,ty2) ->
      eq_type ty1 ty2
  | Tarray (_,ty1,_), Tpointer (_,ty2) | Tpointer (_,ty1), Tarray (_,ty2,_) ->
      eq_type ty1 ty2
  | Tstruct s1, Tstruct s2 ->
      s1 = s2
  | Tunion u1, Tunion u2 ->
      u1 = u2
  | Tenum e1, Tenum e2 ->
      e1 = e2
  | Tpointer(_,{ctype_node = Tfun _ as tn1}), (Tfun _ as tn2)
  | (Tfun _ as tn1), Tpointer(_, {ctype_node = Tfun _ as tn2}) ->
      eq_type_node tn1 tn2
  | Tfun ([], ty1), Tfun (_, ty2) | Tfun (_, ty1), Tfun ([], ty2) ->
      eq_type ty1 ty2
  | Tfun (pl1, ty1), Tfun (pl2, ty2) ->
      eq_type ty1 ty2 &&
      (try List.for_all2 eq_type pl1 pl2 with Invalid_argument _ -> false)
  | _ ->
      false

(* [sub_type ty1 ty2] is true if type [ty1] can be coerced to type [ty2] *)

let sub_type ty1 ty2 = match ty1.ctype_node, ty2.ctype_node with
  | Tint _, Tfloat _ -> true
  | Tpointer(_,{ ctype_node = Tvoid }), Tpointer _ -> true
  | _ -> eq_type ty1 ty2

let compatible_type ty1 ty2 = sub_type ty1 ty2 || sub_type ty2 ty1

let arith_type ty = match ty.ctype_node with
  | Tint _ | Tenum _ | Tfloat _ -> true
  | _ -> false

let array_type ty = match ty.ctype_node with
  | Tarray _ -> true
  | _ -> false

let pointer_type ty = match ty.ctype_node with
  | Tpointer _ -> true
  | _ -> false

let pointer_or_array_type ty = match ty.ctype_node with
  | Tpointer _ | Tarray _ -> true
  | _ -> false

(*s Global environment *)

(* tagged types *)

type tag_kind = Struct | Union | Enum

let tag_kind = function
  | Tstruct _ -> Struct
  | Tunion _ -> Union
  | Tenum _ -> Enum
  | _ -> assert false

type tag_type_definition = 
  | TTIncomplete
  | TTStructUnion of ctype_node * (var_info list)
  | TTEnum of ctype_node * (var_info * int64) list

type tag_type = { 
  tag_kind : tag_kind;
  tag_name : string; (* original source name *)
  tag_uname: string; (* unique name used for further reference *)
  mutable tag_type : tag_type_definition;
}

(* map from unique names to tagged types *)
let (tags_t : (string, tag_type) Hashtbl.t) = Hashtbl.create 97

let tag_type_definition n = 
  let tt = Hashtbl.find tags_t n in tt.tag_type

let create_tag_type k n ty =
  let rec fresh i = 
    let un = n ^ "_" ^ string_of_int i in
    if Hashtbl.mem tags_t un then fresh (succ i) else un
  in
  let un = if Hashtbl.mem tags_t n then fresh 0 else n in
  let tt = 
    { tag_kind = k; tag_name = n; 
      tag_uname = un; tag_type = ty } 
  in
  Hashtbl.add tags_t un tt;
  tt

let clash_tag l s1 s2 = 
  let redef t n = error l "redeclaration of `%s %s'" t n in
  match s1, s2 with
  | Tstruct (n), Tstruct _ -> redef "struct" n
  | Tunion (n), Tunion _ -> redef "union" n
  | Tenum (n), Tenum _ -> redef "enum" n
  | (Tstruct (n) | Tunion (n) | Tenum (n)), 
    (Tstruct _ | Tunion _ | Tenum _) -> 
      error l "`%s' defined as wrong kind of tag" n
  | _ -> assert false

let iter_all_struct f =
  Hashtbl.iter 
    (fun s tt -> match tt.tag_type with
       | TTStructUnion (tn, l) -> f s (tn, l)
       | _ -> ()) 
    tags_t

let fold_all_struct f x =
  Hashtbl.fold 
    (fun s tt acc ->
      match tt.tag_type with
       | TTStructUnion (tn, l) -> f s (tn, l) acc
       | _ -> acc) 
    tags_t x

let fold_all_struct_pairs f x =
  let ls = fold_all_struct (fun s tt acc -> (s,tt) :: acc) [] in
  let rec fold acc = function
    | [] -> acc
    | ((s1, tt1) :: r) as l -> 
	fold (List.fold_left (fun acc (s2,tt2) -> f s1 tt1 s2 tt2 acc) acc l) r
  in
  fold x ls

let fold_all_enum f x =
  Hashtbl.fold
    (fun s tt acc ->
       match tt.tag_type with
	 | TTEnum (tn, l) -> f s (tn, l) acc
	 | _ -> acc)
    tags_t x

(* typedefs *)

let typedef_t = (Hashtbl.create 97 : (string, 'a) Hashtbl.t)

let is_typedef = Hashtbl.mem typedef_t

let find_typedef = Hashtbl.find typedef_t

let add_typedef l x ty = 
  if is_typedef x then begin
    if ty = find_typedef x then error l "redefinition of `%s'" x
    else error l "conflicting types for `%s'" x
  end else
    Hashtbl.add typedef_t x ty

(* used names (in order to rename heap variables when necessary) *)
let used_names = Hashtbl.create 97
let mark_as_used x = Hashtbl.add used_names x ()
let () = 
  List.iter mark_as_used 
    [ (* Why keywords *)
      "absurd"; "and"; "array"; "as"; "assert"; "axiom"; "begin";
      "bool"; "do"; "done"; "else"; "end"; "exception"; "exists";
      "external"; "false"; "for"; "forall"; "fun"; "function"; "goal";
      "if"; "in"; "int"; "invariant"; "label"; "let"; "logic"; "not";
      "of"; "or"; "parameter"; "predicate"; "prop"; "raise"; "raises";
      "reads"; "real"; "rec"; "ref"; "returns"; "then"; "true"; "try";
      "type"; "unit"; "variant"; "void"; "while"; "with"; "writes" ;
      (* caduceus names *)
      "global" ; "alloc"  
    ]

let is_used_name n = Hashtbl.mem used_names n

let use_name ?local_names n = 
  if is_used_name n then raise Exit; 
  begin match local_names with 
    | Some h -> if Lib.Sset.mem n h then raise Exit 
    | None -> () 
  end;
  n

let rec next_name ?local_names n i = 
  let n_i = n ^ "_" ^ string_of_int i in
  try use_name ?local_names n_i with Exit -> next_name ?local_names n (succ i)

let unique_name ?local_names n = 
  try use_name ?local_names n with Exit -> next_name ?local_names n 0

let get_fresh_name n = unique_name n

(*s Environments for the logical side *)

let types = Hashtbl.create 17
let add_type x = Hashtbl.add types x ()
(*let () = add_type "real"*)
let mem_type = Hashtbl.mem types
let find_type = Hashtbl.find types
let iter_types f = Hashtbl.iter (fun s _ -> f s) types

(* type why *)

let count = ref 0

let zone_table = Hashtbl.create 97

let global_zone =
  {
    zone_is_var = false;
    number = 0;
    repr = None;
    name =  "global";
  }

let () =
  if not Coptions.zones 
  then Hashtbl.add zone_table global_zone.name global_zone

let make_zone ?name is_var =
  if Coptions.zones then 
    begin
      let n =
	match name with
	  | Some s -> sprintf "%s_%i" s !count
	  | None -> sprintf "Z%i" !count
      in
      let z = { zone_is_var = is_var;
		number = !count;
		repr = None;
		name = n;
	      } in
      Hashtbl.add zone_table z.name z;
      count := !count +1;
      z
    end
  else
    global_zone

let int_type_for_size signed size =
  (if signed = Signed then "" else "u") ^ "int" ^ string_of_int size 

module SI = 
  Set.Make(struct type t = Ctypes.sign * int
		  let compare = Pervasives.compare end)
let int_sizes = ref SI.empty
let int_types = ref StringSet.empty
let declare_int_size ((s,i) as is) = 
  int_types := StringSet.add (int_type_for_size s i) !int_types;
  int_sizes := SI.add is !int_sizes
let all_int_sizes () = SI.elements !int_sizes
let is_int_type s = StringSet.mem s !int_types

let int_size = function
  | Char -> char_size
  | Short -> short_size
  | Ctypes.Int -> int_size
  | Long -> long_size
  | LongLong -> long_long_size
  | Bitfield n -> Int64.to_int n
  | ExactInt -> assert false

let int_type_for (signed, ty) = 
  let n = int_size ty in
  declare_int_size (signed, n);
  int_type_for_size signed n

let enum_type_for s = 
  let n = "enum_" ^ s in
  int_types := StringSet.add n !int_types;
  n

let rec type_type_why ?name ty zone_is_var =
  match ty.ctype_node with
    | Tint (_,ExactInt) -> Int
    | Tint _ when not Coptions.machine_ints -> Int
    | Tenum _ when not Coptions.enum_check -> Int
    | Tint ik -> Why_Logic (int_type_for ik)
    | Tenum e -> Why_Logic (enum_type_for e)
    | Tfloat _ when not Coptions.floats -> Why_Logic "real"
    | Tfloat Float -> Why_Logic "single"
    | Tfloat Double -> Why_Logic "double"
    | Tfloat LongDouble -> Why_Logic "quad"
    | Tfloat Real -> Why_Logic "real"
    | Tarray (_,ty,_)  
    | Tpointer (_,ty) -> 
	begin match ty.ctype_node with 
	  | Tstruct s -> 
	      let z = make_zone ?name zone_is_var in
	      Pointer z
	  | _ ->
	      let z = make_zone ?name zone_is_var in
	      Pointer z
	end
    | Tvoid -> Unit 
    | Tfun (_,ty) -> type_type_why ?name ty zone_is_var
    | Tvar v -> 
	begin
	  try
	    type_type_why ?name (find_typedef v) zone_is_var
	  with Not_found -> 
	    if mem_type v
	    then
	      Why_Logic v
	    else
	      (Format.eprintf "Undefined type %s@." v; assert false)
	end
    | Tunion s -> 
	let z = make_zone ?name zone_is_var in
	Pointer z
    | Tstruct s -> 
	let z = make_zone ?name zone_is_var in
	Pointer z


let set_var_type info ty zone_is_var =
  Info.set_var_type info ty 
    (type_type_why ~name:(env_name info) ty zone_is_var)

(* global variables and functions *)

let (sym_t : (string, env_info) Hashtbl.t) = Hashtbl.create 97

let is_sym = Hashtbl.mem sym_t

let find_sym = Hashtbl.find sym_t

let iter_sym f =  Hashtbl.iter f sym_t

let add_sym l x ty info = 
  let n = unique_name x in
  mark_as_used n; 
  set_unique_name info n;
  if n <> x then Coptions.lprintf "renaming %s into %s@." x n;
  try
    let d = find_sym x in
    let varty = var_type d in
    if not (eq_type varty ty) then 
      (* TODO accepter fonctions avec arguments si aucun la première fois 
	 Question de Claude: accepter aussi un raffinement des specs ? *)
      begin
	eprintf "t : %a, ty : %a@." print_type  varty print_type  ty;
	error l "conflicting types for %s" x
      end;
    d
  with Not_found ->
    set_var_type info ty false;
    Hashtbl.add sym_t x info;
    info

let c_functions = 
  (Hashtbl.create 97 : 
     (string, Cast.nspec * ctype * Info.fun_info * Cast.nstatement option * 
	Loc.position) Hashtbl.t)
let add_c_fun f = Hashtbl.remove c_functions f; Hashtbl.add c_functions f
let find_c_fun = Hashtbl.find c_functions

let logic_functions = 
  (Hashtbl.create 97 : 
     (string, ctype list * ctype * Info.logic_info) Hashtbl.t)
let add_logic = Hashtbl.add logic_functions
let find_logic = Hashtbl.find logic_functions

let predicates = 
  (Hashtbl.create 97 : (string, ctype list * Info.logic_info) Hashtbl.t) 
let add_pred = Hashtbl.add predicates
let mem_pred =  Hashtbl.mem predicates
let find_pred =  Hashtbl.find predicates 

let ghost = 
  (Hashtbl.create 97 : (string, Info.var_info) Hashtbl.t) 
let is_ghost = Hashtbl.mem ghost 
let find_ghost = Hashtbl.find ghost

let add_ghost l x ty info = 
  let n = unique_name x in
  mark_as_used n; 
  set_unique_name (Var_info info) n;
  if n <> x then Coptions.lprintf "renaming ghost variable %s into %s@." x n;
  if is_ghost x then begin
    error l "ghost variable `%s' already declared" x;
  end
  else begin
    set_var_type (Var_info info) ty false;
    Hashtbl.add ghost x info;
    info
  end


(*s Environments for local variables and local structs/unions/enums *)

module Env = struct

  module M = Map.Make(String)

  (* [tags] is the stack of blocks; 
     each block maps a tag name to a tag type *)
  type t = { 
    vars : env_info M.t; 
    used_names : Lib.Sset.t;
    tags : (string, tag_type) Hashtbl.t list;
  }

  (* note: the first hash table in [tags] is shared *)
  let shared_hash_table = Hashtbl.create 17

  let empty () = 
    { vars = M.empty; 
      used_names = Lib.Sset.empty; 
      tags = [shared_hash_table] }

  let new_block env = { env with tags = Hashtbl.create 17 :: env.tags }

  (* symbols *)
  let add x t info env = 
    let n = unique_name ~local_names:env.used_names x in
    set_unique_name info n;
    Coptions.lprintf "local %s renamed into %s\n" x n;
    set_var_type info t true;
    Coptions.lprintf "local %s has why type %a\n" x 
      Output.fprintf_logic_type (Info.output_why_type (get_why_type info));
    { env with used_names = Lib.Sset.add n env.used_names;
	vars = M.add x info env.vars }

  let find x env = M.find x env.vars

  let mem x env = M.mem x env.vars

  (* tagged type *)
  let find_tag n env =
    let rec find = function
      | [] -> raise Not_found
      | h :: hl -> try Hashtbl.find h n with Not_found -> find hl
    in
    find env.tags

  let find_tag_type loc env tyn = 
    let tt = match tyn with
      | Tstruct (n) | Tunion (n) | Tenum (n) ->
          (try
	     find_tag n env
	   with Not_found -> 
	     let tt = create_tag_type (tag_kind tyn) n TTIncomplete in
	     Hashtbl.add (List.hd env.tags) n tt;
	     tt)
      | _ ->
	  assert false
    in
    match tt.tag_kind with
      | Struct -> Tstruct (tt.tag_uname)
      | Union -> Tunion (tt.tag_uname)
      | Enum -> Tenum (tt.tag_uname)

  let set_struct_union_type loc env tyn fields = 
    let tt = match tyn with
      | Tstruct (n) | Tunion (n) ->
	   (try
              let tt = Hashtbl.find (List.hd env.tags) n in
	      begin match tt.tag_type with
		| TTIncomplete ->
                    (* tag already seen in this block but not yet defined *)
                    tt.tag_type <- TTStructUnion (tyn,fields)
		| TTStructUnion (tyn',_) | TTEnum (tyn',_) ->  
		    (* tag [n] already defined in current block *)
		    clash_tag loc tyn tyn'
	      end;
	      tt
	    with Not_found ->
	      let tt = 
		create_tag_type (tag_kind tyn) n (TTStructUnion (tyn,fields)) 
	      in
	      Hashtbl.add (List.hd env.tags) n tt;
	      tt)
      | _ ->
	  assert false
    in
    match tt.tag_kind with
      | Struct -> Tstruct (tt.tag_uname)
      | Union -> Tunion (tt.tag_uname)
      | Enum -> assert false

  let set_enum_type loc env tyn fields = 
    let tt = match tyn with
      | Tenum (n) ->
	   (try
              let tt = Hashtbl.find (List.hd env.tags) n in
	      begin match tt.tag_type with
		| TTIncomplete ->
                    (* tag already seen in this block but not yet defined *)
                    tt.tag_type <- TTEnum (tyn,fields)
		| TTStructUnion (tyn',_) | TTEnum (tyn', _) ->
		    (* tag [n] already defined in current block *)
		    clash_tag loc tyn tyn'
	      end;
	      tt
	    with Not_found ->
	      let tt = 
		create_tag_type (tag_kind tyn) n (TTEnum (tyn,fields)) 
	      in
	      Hashtbl.add (List.hd env.tags) n tt;
	      tt)
      | _ ->
	  assert false
    in
    match tt.tag_kind with
      | Enum -> Tenum (tt.tag_uname)
      | Struct | Union -> assert false

end

(* Field access *)

let fields_t = Hashtbl.create 97

let find_field ~tag:n ~field:x = 
  try 
    Hashtbl.find fields_t (n,x)
  with Not_found -> 
    let u = 
      try use_name x with Exit -> 
	let n_x = n ^ "_" ^ x in 
	try use_name n_x with Exit -> 
	  next_name n_x 0
    in
    let f = default_var_info x in
    set_unique_name (Var_info f) u;
    mark_as_used u; 
    Hashtbl.add fields_t (n,x) f; f

let declare_fields tyn fl = match tyn with
  | Tstruct n | Tunion n ->
      List.iter 
	(fun (t,v) -> set_var_type (Var_info v) t false)
	fl
  | _ -> 
      assert false
  
let not_struct t = match t.ctype_node with
  | Tstruct _ -> false
  | _ -> true

let update_fields_type () =
  Hashtbl.iter
    (fun (n,_) x ->
       if x.var_is_referenced && not_struct x.var_type then
	 begin
	   Coptions.lprintf "field %s is now a pointer@." x.var_name;
	   set_var_type (Var_info x) 
	     (noattr (Tarray(Valid(Int64.zero,Int64.one),x.var_type,Some Int64.one))) false
	 end)
    fields_t

let type_of_field loc x ty = 
  let rec lookup su n = function
    | [] -> error loc "`%s' has no member named `%s'" su x
    | y :: _ when x = y.var_name -> find_field n x
    | _ :: fl -> lookup su n fl
  in
  match ty.ctype_node with
    | Tstruct (n) | Tunion (n) -> 
        assert (Hashtbl.mem tags_t n);
	let tt = Hashtbl.find tags_t n in
	begin match tt.tag_type with
	  | TTIncomplete -> error loc ("use of incomplete type")
	  | TTStructUnion (Tstruct _, fl) -> lookup "structure" n fl
	  | TTStructUnion (Tunion _, fl) -> lookup "union" n fl
	  | TTStructUnion _ | TTEnum _ ->
	      error loc 
		"request for member `%s' in something not a structure or union" x
	end
    | _ -> error loc 
	"request for member `%s' in something not a structure or union" x

