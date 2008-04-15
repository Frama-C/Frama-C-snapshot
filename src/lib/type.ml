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

type 'a t = int

exception AlreadyExists of string

module Types = struct
  
  include Hashtbl.Make
    (struct 
       type t = int
       let equal = (=) 
       let hash x = x
     end)

  module Name = struct
    
    module S = Set.Make(String)

    let names = ref S.empty

    let make name =
      if S.mem name !names then raise (AlreadyExists name);
      names := S.add name !names;
      name
	
  end

  let make =
    let cpt = ref 0 in
    fun tbl name ->
      incr cpt;
      add tbl !cpt (Name.make name);
      !cpt

end
   
let types_tbl = Types.create 97

let id x = x
let name = Types.find types_tbl
let name_of_id = name

let equal = (=)
let compare = Pervasives.compare

(** Signature of an instance checker *)
module type INSTANCE_CHECKER = sig
  
  val is_instance_of: 'a t -> bool
    (** @return true if it's a subset of the polymorph type, false otherwise. *)

end

(* ****************************************************************************)
(** {2 Basic types} *)
(* ****************************************************************************)

let make name _ = Types.make types_tbl name

let unit = make "unit" ()
let bool = make "bool" true
let int = make "int" 0
let float = make "float" 0.
let char = make "char" ' '
let string = make "string" ""

(* ****************************************************************************)
(** {2 Polymorph types} *)
(* ****************************************************************************)

module type POLY_OUTPUT = sig
  
  include INSTANCE_CHECKER

  type 'a poly
    (** A polymorph type. *)
    
  val realize: 'a t -> 'a poly t
    (** @return the realization of the polymorph type applied to the given
	one. *)

  val elem_type: 'a poly t -> 'a t
    (** @return the type of elements contained in the polymorphic container. *)
    
end

module Polymorphic
  (D:sig 
     type 'a t 
     val make_name: string -> string 
   end) =
struct
  
  type 'a poly = 'a D.t
      
  let instances : (int, int) Hashtbl.t = Hashtbl.create 17
    
  let realize a =
    try Hashtbl.find instances a
    with Not_found -> 
      let ty = Types.make types_tbl (D.make_name (name a)) in
      Hashtbl.add instances a ty;
      ty
      
  exception Found of int
  let is_instance_of ty =
    (** instances are unique *)
    try 
      Hashtbl.iter (fun k v -> if ty = v then raise (Found k)) instances;
      false
    with Found _ -> true

  let elem_type ty =
    try 
      Hashtbl.iter (fun k v -> if ty = v then raise (Found k)) instances;
      assert false (* the given type has been created yet! *)
    with Found k -> k
      
end

module type POLY2_OUTPUT = sig
  
  include INSTANCE_CHECKER

  type ('a, 'b) poly2
    (** A doubly polymorph type. *)

  val realize: 'a t -> 'b t -> ('a, 'b) poly2 t
    (** @return the type of the polymporph one applied to both given types
	@raise AlreadyExists if the given name is already registered. *)

  val elem_types: ('a, 'b) poly2 t -> 'a t * 'b t
    (** @return the types of elements contained in the doubly poly polymorphic
	container. *)

end

module Polymorphic2
  (D:sig 
     type ('a, 'b) t 
     val make_name: string -> string -> string 
   end) =
struct
    
  type ('a, 'b) poly2 = ('a, 'b) D.t
      
  let instances : (int * int, int) Hashtbl.t = Hashtbl.create 17
    
  let realize a b =
    try Hashtbl.find instances (a,b)
    with Not_found ->
      let ty = Types.make types_tbl (D.make_name (name a) (name b)) in
      Hashtbl.add instances (a,b) ty;
      ty
  
  exception Found of int * int
  let is_instance_of ty =
    try
      Hashtbl.iter 
	(fun (a,b) v -> if ty = v then raise (Found (a,b))) 
	instances;
      false
    with Found _ -> true

  let elem_types ty =
    try
      Hashtbl.iter 
	(fun (a,b) v -> if ty = v then raise (Found (a,b))) 
	instances;
      assert false (* the given type has been created yet! *)
    with Found (a,b) -> a, b
    
end

(* ****************************************************************************)
(** {2 Couple } *)
(* ****************************************************************************)

module Couple = Polymorphic2
  (struct
     type ('a, 'b) t = 'a * 'b
     let make_name n1 n2 = "(("^n1^") * ("^n2^"))"
   end)

let couple = Couple.realize

(* ****************************************************************************)
(** {2 Option } *)
(* ****************************************************************************)

module Option = Polymorphic
  (struct
     type 'a t = 'a option
     let make_name n = "("^n^") option"
   end)

let option = Option.realize

(* ****************************************************************************)
(** {2 Functional types} *)
(* ****************************************************************************)

exception Not_functional

module Function = struct

  include Polymorphic2
    (struct
       type ('a, 'b) t = 'a -> 'b
       let make_name n1 n2 = "("^n1^" -> "^n2^")"
     end)

    let split x =
      try elem_types x
      with Assert_failure _ -> raise Not_functional
	
end

let func = Function.realize
let split = Function.split

(* ****************************************************************************)
(** {2 Lists} *)
(* ****************************************************************************)

module List = Polymorphic
  (struct type 'a t = 'a list let make_name name = name^" list" end)

let list = List.realize

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
