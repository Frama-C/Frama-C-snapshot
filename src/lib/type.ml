(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(* Disclaimer
   ----------
   This module uses very unsafe caml features (module Obj).
   Modify it at your own risk.
   Sometimes the caml type system does not help you here.
   Introducing a bug here may introduce some "segmentation faults" in Frama-C *)

let use_obj = ref true
let no_obj () = use_obj := false

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(* from higher to lower precedence *)
type precedence =
  | Basic
  | Call
  | Couple
  | List
  | NoPar

(* p1 <= p2 *)
let lower_prec p1 p2 = match p1, p2 with
  | NoPar, _
  | _, Basic -> true
  | x, y when x = y -> true
  | List, (Couple | Call)
  | Couple, Call -> true
  | _, _ -> false

let par p_caller p_callee fmt pp =
  (* if p_callee <= p_caller then parenthesis else no parenthesis *)
  if lower_prec p_callee p_caller then Format.fprintf fmt "(%t)" pp
  else Format.fprintf fmt "%t" pp

type 'a t = string
type ty (* type tag for abstract type values *)

exception AlreadyExists of string

exception NoPrinter of string
let no_pp _ _ _ = assert false

let find_binding = ref (fun _ _ -> assert false)

exception Not_dynamic of string

(* All the registered type values. *)
module Types : sig
  val add: 
    name:string -> 
    pp_value_name:(precedence -> Format.formatter -> unit) ->
    dynamic:bool -> 
    ?pp:(precedence -> Format.formatter -> 'a -> unit) -> 
    ?varname:('a -> string) ->
    'a -> 'a t
  val name: 'a t -> string
  val pp_value_name: 'a t -> precedence -> Format.formatter -> unit
  val repr: 'a t -> 'a
  val pp: 'a t -> precedence -> Format.formatter -> 'a -> unit
  val varname: 'a t -> ('a -> string) option
  val is_dynamic: 'a t -> bool
  val get_dynamic: string -> ty t
  val set_pp: 'a t -> (precedence -> Format.formatter -> 'a -> unit) -> unit
end = struct

  type pp = precedence -> Format.formatter -> Obj.t -> unit
  type info = 
      { name: string;
	pp_value_name: precedence -> Format.formatter -> unit;
	repr: Obj.t;
	pp: pp;
	varname: (Obj.t -> string) option }

  let types : (string, info) Hashtbl.t = Hashtbl.create 97

  module S = Set.Make(String)
  let names = ref S.empty

  let dynamic_types : (string, string) Hashtbl.t = Hashtbl.create 7
	
  let mk_pp ty pp = 
    if !use_obj then
      let pp =
	if pp == no_pp then no_pp else fun p fmt v -> pp p fmt (Obj.obj v)
      in
      fun p fmt v ->
	try Format.fprintf fmt "%s" (!find_binding ty (Obj.obj v));
	with Not_found -> pp p fmt v
    else
      no_pp
  
  let add ~name ~pp_value_name ~dynamic ?(pp=no_pp) ?varname value =
    if !use_obj then begin
      (* check name unicity and extend the namespace *)
      if S.mem name !names then raise (AlreadyExists name);
      names := S.add name !names;
      (* compute the value representing the static type *)
      let repr =
	Digest.to_hex
	  (Digest.string 
	     (Marshal.to_string (name, value) [ Marshal.Closures ]))
      in
      (* register this value with its associated data *)
      assert (not (Hashtbl.mem types repr));
      let varname = match varname with 
	| None -> None
	| Some f -> Some (fun x -> f (Obj.obj x))
      in
      Hashtbl.add 
	types 
	repr 
	{ name = name; 
	  pp_value_name = pp_value_name;
	  repr = Obj.repr value; 
	  pp = mk_pp repr pp; varname = varname };
      (* register as an dynamic type if required *)
      assert (not (Hashtbl.mem dynamic_types name));
      if dynamic then Hashtbl.add dynamic_types name repr;
      repr
    end else
      ""
    
  let find ty = 
    try Hashtbl.find types ty 
    with Not_found as exn -> 
      assert (not !use_obj);
      raise exn

  let name ty = (find ty).name
  let pp_value_name ty = (find ty).pp_value_name
  let repr ty = 
    if !use_obj then Obj.obj (find ty).repr 
    else invalid_arg "cannot call function 'repr' in the 'no obj' mode"

  let valid_varname s = 
    let r = Str.regexp "[^A-Za-z0-9_]+" in
    let s = Str.global_replace r "__" s in
    String.uncapitalize s

  let varname ty = match (find ty).varname with
    | None -> 
	None
    | Some f -> 
	assert !use_obj;
	Some (fun x -> valid_varname (f (Obj.repr x)))

  let pp ty = 
    let pp = (find ty).pp in 
    if pp == no_pp then raise (NoPrinter (name ty));
    assert !use_obj;
    fun prec fmt v -> pp prec fmt (Obj.repr v)

  let set_pp ty pp = 
    if !use_obj then
      let info = find ty in
      Hashtbl.replace types ty { info with pp = mk_pp ty pp }

  let is_dynamic ty = 
    try 
      Hashtbl.iter (fun _ ty' -> if ty = ty' then raise Exit) dynamic_types; 
      false
    with Exit -> 
      true

  let get_dynamic s = 
    try Hashtbl.find dynamic_types s
    with Not_found -> raise (Not_dynamic s)

end
   
let name = Types.name
let pp_value_name = Types.pp_value_name
let pp = Types.pp
let is_dynamic = Types.is_dynamic
let get_dynamic = Types.get_dynamic
let varname = Types.varname

let use_pp ty fmt x = 
  let buf = Buffer.create 17 in
  let buffmt = Format.formatter_of_buffer buf in
  Format.fprintf buffmt "%a@?" (pp ty NoPar) x;
  let f = Scanf.format_from_string (String.escaped (Buffer.contents buf)) "" in
  Format.fprintf fmt f

let register ~name ~value_name ?pp ?varname v = 
  if !use_obj then
    let pp_value_name, dynamic = 
      match value_name with 
      | None -> 
	  let pp fmt = Format.fprintf fmt "Type.get_dynamic %S" name in
	  (fun p fmt -> par p Call fmt pp), true
      | Some s ->
	  (fun p fmt -> par p Basic fmt (fun fmt -> Format.fprintf fmt "%s" s)),
	  false
    in
    if Obj.tag (Obj.repr v) = Obj.closure_tag then
      invalid_arg ("[Type.make] invalid value for representing type " ^ name);
    Types.add ~name ~pp_value_name ~dynamic ?pp ?varname v
  else
    ""

let register_pp = Types.set_pp

(* ****************************************************************************)
(** {2 Type values are comparable} *)
(* ****************************************************************************)

let equal = (=)
let compare = Pervasives.compare
let hash = Hashtbl.hash

(* ****************************************************************************)
(** {2 Basic type values} *)
(* ****************************************************************************)

let reg x = register ~name:x ~value_name:(Some ("Type." ^ x))

let unit = reg "unit" ~pp:(fun _ fmt () -> Format.fprintf fmt "()") ()
let bool = reg "bool" ~pp:(fun _ fmt b -> Format.fprintf fmt "%B" b) true
let int = reg "int" ~pp:(fun _ fmt n -> Format.fprintf fmt "%d" n) 0
let int32 = 
  reg "int32" ~pp:(fun _ fmt n -> Format.fprintf fmt "%ld" n) Int32.zero
let int64 = 
  reg "int64" ~pp:(fun _ fmt n -> Format.fprintf fmt "%Ld" n) Int64.zero
let nativeint = 
  reg "nativeint" ~pp:(fun _ fmt n -> Format.fprintf fmt "%nd" n) Nativeint.zero
let float = reg "float" ~pp:(fun _ fmt f -> Format.fprintf fmt "%f" f) 0.
let char = reg "char" ~pp:(fun _ fmt c -> Format.fprintf fmt "%c" c) ' '
let string = reg "string" ~pp:(fun _ fmt s -> Format.fprintf fmt "%S" s) ""
let formatter = 
  register ~name:"Format.formatter" ~value_name:(Some "Type.formatter")
    (* Predefined formatter are abstract value that cannot be marshalled.
       this value is used only for computing the ty_repr from a digest:
       indeed can be any value of any type.
       Furthermore, this value is not_used in the 'no obj' mode. *)
    ~pp:(fun _ fmt _ ->  Format.fprintf fmt "((* Could not find the original formatter. Fallbacks to kernel.*) Kernel.result_fmt ())")
    (Obj.magic 0 : Format.formatter)

(* ****************************************************************************)
(** {2 Polymorphic type values} *)
(* ****************************************************************************)

module type POLYMORPHIC = sig
  type 'a poly
  val instantiate: 'a t -> 'a poly t
  val is_instance_of: 'a t -> bool
  val get_instance: 'a poly t -> 'a t
end

module Polymorphic
  (D:sig 
     val name: 'a t -> string 
     val value_name: string
     type 'a t 
     val repr: 'a -> 'a t
     val pp: 
       (precedence -> Format.formatter -> 'a -> unit) -> 
       precedence -> Format.formatter -> 'a t -> unit
   end) =
struct

  type 'a poly = 'a D.t
      
  let instances : (string, string) Hashtbl.t = Hashtbl.create 17
    
  let pp_value_name from_ty p fmt =
    par p Call fmt
      (fun fmt -> 
	 Format.fprintf fmt "%s.instantiate %t" 
	   D.value_name 
	   (pp_value_name from_ty Call))

  let instantiate ty =
    if !use_obj then
      try 
	Hashtbl.find instances ty
      with Not_found ->
	let repr = 
	  Types.add 
	    ~pp:(try fun p -> D.pp (Types.pp ty) p with NoPrinter _ -> no_pp)
	    ~name:(D.name ty) 
	    ~pp_value_name:(pp_value_name ty)
	    ~dynamic:false
	    (D.repr (Types.repr ty)) 
	in
	Hashtbl.add instances ty repr;
	repr
    else
      ""
      
  exception Found of string
  let search ty =
    Hashtbl.iter (fun k v -> if ty = v then raise (Found k)) instances

  let is_instance_of ty = try search ty; false with Found _ -> true

  let get_instance (ty:'a poly t) =
    try 
      search ty;
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false
    with Found a -> 
      a
      
end

module type POLYMORPHIC2 = sig
  type ('a, 'b) poly
  val instantiate: 'a t -> 'b t -> ('a, 'b) poly t
  val is_instance_of: 'a t -> bool
  val get_instance: ('a, 'b) poly t -> 'a t * 'b t
end

module type POLY2_INPUT = sig 
  val name: 'a t -> 'b t -> string
  val value_name: string
  type ('a, 'b) t 
  val repr: 'a -> 'b -> ('a, 'b) t
  val pp: 
    (precedence -> Format.formatter -> 'a -> unit) -> 
    (precedence -> Format.formatter -> 'b -> unit) -> 
    precedence -> Format.formatter -> ('a,'b) t -> unit
end

module Polymorphic2(D:POLY2_INPUT) = struct

  module H = 
    Hashtbl.Make
      (struct
	 type t = string * string
	 let equal (x1,y1) (x2,y2) = x1 == x2 && y1 == y2
	 let hash = Hashtbl.hash
       end)

  type ('a, 'b) poly = ('a, 'b) D.t
      
  let instances : (string * string, string) Hashtbl.t = Hashtbl.create 17

  let dynamic_instances : string H.t = H.create 17

  let pp_value_name from_ty1 from_ty2 p fmt =
    let pp fmt =
      Format.fprintf fmt "%s.instantiate %t %t" 
	D.value_name
	(pp_value_name from_ty1 Call)
	(pp_value_name from_ty2 Call)
    in
    par p Call fmt pp
    
  let instantiate a b =
    if !use_obj then
      let key = a, b in
      try 
	let repr = Hashtbl.find instances key in
	if not (H.mem dynamic_instances key) then
	  H.add dynamic_instances key (String.copy repr);
	repr
      with Not_found ->
	let ty = 
	  Types.add
	    ~pp:(try fun p -> D.pp (Types.pp a) (Types.pp b) p 
		 with NoPrinter _ -> no_pp)
	    ~name:(D.name a b) 
	    ~pp_value_name:(pp_value_name a b)
	    ~dynamic:false
	    (D.repr (Types.repr a) (Types.repr b)) 
	in
	Hashtbl.add instances key ty;
	ty
    else
      ""
  
  exception Found of (string * string)
  let search ty = 
    Hashtbl.iter (fun k v -> if ty = v then raise (Found k)) instances

  let is_instance_of ty = try search ty; false with Found _ -> true

  let get_instance (ty:('a, 'b) poly t) =
    try
      search ty;
      (* static typing ensures than [ty] has already been instantiated. *)
      assert false
    with Found k -> 
      k
    
end

let par_ty test ty =
  let s = name ty in
  if test ty then Format.sprintf "(%s)" s else Format.sprintf "%s" s

(* ****************************************************************************)
(** {2 Functional types} *)
(* ****************************************************************************)

module Function = struct

  type ('a, 'b) poly = 'a -> 'b
      
  let instances
      : (string * string * string option (* label *),
	 string * Obj.t option (* default value of an optional label *)) 
      Hashtbl.t
      = Hashtbl.create 17

  let repr (_:'a) (b:'b) : 'a -> 'b = fun _ -> b
  let pp _ _ = no_pp
  
  exception Found of (string * string * (string * Obj.t option) option)
  let search ty =
    Hashtbl.iter 
      (fun (a,b,l) (t,o) -> 
	 if ty = t then 
	   let lo = match l,o with 
	     | None, None -> None
	     | None, Some _ -> assert false 
	     | Some l, _ -> Some (l, o)
	   in
	   raise (Found (a,b,lo)))
      instances

  let is_instance_of ty = try search ty; false with Found _ -> true

  let name label ty1 ty2 = 
    (match label with None -> "" | Some l -> "~" ^ l ^ ":") 
    ^ par_ty is_instance_of ty1 ^ " -> " ^ name ty2

  let pp_value_name label ty1 ty2 p fmt =
    let pp fmt =
      Format.fprintf fmt
	"Type.func%s %t %t"
	(match label with None -> "" | Some l -> " ~label:(" ^ l ^ ", None)")
	(pp_value_name ty1 Call) (pp_value_name ty2 Call)
    in
    par p Call fmt pp

  let f x = x
    
  let instantiate ?label (a:'a) (b:'b t) =
    if !use_obj then
      let l, o = 
	match label with 
	| None -> None, None 
	| Some (l, None) -> Some l, None
	| Some (l, Some o) -> Some l, Some (Obj.repr o) 
      in
      let key = a, b, l in
      try 
	fst (Hashtbl.find instances key)
      with Not_found ->
	let ty = 
	  (* Do not inline [Types.repr b] in the closure below because
	     caml is not able to marshal the closure.
	     Sadly don't know exactly why. Seem to have some value tagged as
	     abstract in the closure environment. *)
	  let b_repr = Types.repr b in
	  Types.add
	    ~name:(name l a b)
	    ~pp_value_name:(pp_value_name l a b)
	    ~dynamic:false
	    (fun _ -> b_repr)
	in
	Hashtbl.add instances key (ty, o);
	ty
    else
      ""

  let get_instance (ty:('a, 'b) poly t) =
    if !use_obj then
      try
	search ty;
	(* static typing ensures than [ty] has already been instantiated. *)
	assert false
      with Found(a,b,lo) -> 
	a,b, 
	match lo with 
	| None -> None 
	| Some (l, None) -> Some (l, None) (* rebuild value for typing *)
	| Some (l, Some o) -> Some (l, Some (Obj.obj o))
    else 
      invalid_arg "cannot call function 'get_instance' in the 'no obj' mode"

end

let func = Function.instantiate

let optlabel_func lab dft = func ~label:(lab, Some dft)

(* ****************************************************************************)
(** {2 Couple } *)
(* ****************************************************************************)

module rec Couple_Arg : POLY2_INPUT with type ('a, 'b) t = 'a * 'b = struct
  type ('a, 'b) t = 'a * 'b
  let name ty1 ty2 = 
    let arg ty = 
      par_ty
	(fun ty -> Function.is_instance_of ty || Couple.is_instance_of ty)
	ty
    in
    arg ty1 ^ " * " ^ arg ty2
  let value_name = "Type.Couple"
  let repr a b = a, b
  let pp fa fb p fmt (a, b) = 
    let pp fmt =
      Format.fprintf fmt "@[<hv 2>%a,@;%a@]" (fa Couple) a (fb Couple) b
    in
    par p Couple fmt pp
end

and Couple : POLYMORPHIC2 with type ('a,'b) poly = 'a * 'b = 
  Polymorphic2(Couple_Arg)

let couple = Couple.instantiate

(* ****************************************************************************)
(** {2 Reference } *)
(* ****************************************************************************)

module Ref = Polymorphic
  (struct
     type 'a t = 'a ref
     let name ty = 
       par_ty 
	 (fun ty -> Function.is_instance_of ty || Couple.is_instance_of ty)
	 ty
       ^ " ref"
     let value_name = "Type.Ref"
     let repr ty = ref ty
     let pp f p fmt x = 
       let pp fmt = Format.fprintf fmt "@[<hv 2>ref@;%a@]" (f Call) !x in
       par p Call fmt pp
   end)
let t_ref = Ref.instantiate

(* ****************************************************************************)
(** {2 Option } *)
(* ****************************************************************************)

module Option = Polymorphic
  (struct
     type 'a t = 'a option
     let name ty = 
       par_ty 
	 (fun ty -> Function.is_instance_of ty || Couple.is_instance_of ty)
	 ty
       ^ " option"
     let value_name = "Type.Option"
     let repr ty = Some ty
     let pp f p fmt = function
       | None -> Format.fprintf fmt "None"
       | Some x -> 
	   let pp fmt = Format.fprintf fmt "@[<hv 2>Some@;%a@]" (f Call) x in
	   par p Call fmt pp
   end)
let option = Option.instantiate

(* ****************************************************************************)
(** {2 Lists} *)
(* ****************************************************************************)

module List = Polymorphic
  (struct 
     type 'a t = 'a list 
     let name ty = 
       par_ty 
	 (fun ty -> Function.is_instance_of ty || Couple.is_instance_of ty)
	 ty
       ^ " list"
     let value_name = "Type.List"
     let repr ty = [ ty ]
     let pp f p fmt l =
       let pp fmt =
	 Format.fprintf fmt "@[<hv 2>[ %t ]@]"
	   (fun fmt -> 
	      let rec print fmt = function
		| [] -> ()
		| [ x ] -> Format.fprintf fmt "%a" (f List) x
		| x :: l -> Format.fprintf fmt "%a;@;%a" (f List) x print l
	      in
	      print fmt l)
       in
       par p Basic fmt pp (* Never enclosed lists in parentheris *)
   end)
let list = List.instantiate

(* ****************************************************************************)
(** {2 Heterogeneous Tables indexed by string} *)
(* ****************************************************************************)

module TyTbl = struct
  type 'a ty = 'a t
  include Hashtbl.Make
    (struct 
       type t = string 
       let equal = equal 
       let hash = hash
     end)
end

module ObjTbl: sig
  type 'a ty = 'a t
  type 'a t
  val create: unit -> 'a t
  val add: 'a t -> 'b ty -> 'b -> 'a -> unit
  val find: 'a t -> 'b ty -> 'b -> 'a
  val mem: 'a t -> 'b ty -> 'b -> bool
  val iter: 'a t -> ('a -> unit) -> unit
end = struct

  type 'a ty = 'a t

  module O =
    Hashtbl.Make
      (struct
	 type t = Obj.t
	 let equal = (==)
	 let hash x = 
	   if !use_obj then
	     (* 0 is correct; trying to do a bit better... *)
	     let tag = Obj.tag x in
	     if tag = 0 then 
	       0 
	     else if tag = Obj.closure_tag then 
	       (* assumes that the first word of a closure does not change in
		  anyway (even by Gc.compact invokation). *)
	       Obj.magic (Obj.field x 0) 
	     else 
	       Hashtbl.hash x
	   else
	     0
       end)

  type 'a t = 'a O.t TyTbl.t

  let create () = TyTbl.create 7

  let add tbl ty k v =
    if !use_obj then
      let tytbl = 
	try TyTbl.find tbl ty 
	with Not_found ->
	  let tytbl = O.create 7 in
	  TyTbl.add tbl ty tytbl;
	  tytbl
      in
      O.replace tytbl (Obj.repr k) v

  let find tbl ty k = 
    if !use_obj then O.find (TyTbl.find tbl ty) (Obj.repr k)
    else invalid_arg "cannot call function 'find' in the 'no obj' mode"

  let mem tbl ty k = 
    try
      let objs = TyTbl.find tbl ty in
      assert !use_obj;
      O.mem objs (Obj.repr k)
    with Not_found ->
      false

  let iter tbl f = TyTbl.iter (fun _ objs -> O.iter (fun _ -> f) objs) tbl

end

module StringTbl = struct
  
  type 'a ty = 'a t
  type data = { ty: string; o: Obj.t }
  type t = (string, data) Hashtbl.t

  let create = Hashtbl.create

  exception Unbound_value of string
  exception Incompatible_type of string
  let type_error s ty ty' =
    raise 
      (Incompatible_type
	 (Format.sprintf "%s has type %s but is used with type %s." 
	    s (name ty') (name ty)))

  let values_of_dynamics : unit ObjTbl.t = ObjTbl.create ()

  let rec objectify name n ty x : Obj.t =
    assert !use_obj;
    if Types.is_dynamic ty then ObjTbl.add values_of_dynamics ty x ();
    if Function.is_instance_of ty then
      let f : 'a -> 'b = Obj.magic x in
      let (a:'a ty), (b:'b ty), _ = Function.get_instance ty in
      Obj.repr
	(fun (y:'a) ->
	   (* for dynamic types, dynamically check that the argument was
	      built with a constructor of this type. *)
	   if Types.is_dynamic a && not (ObjTbl.mem values_of_dynamics a y)
	   then begin
	     let msg =
	       Format.sprintf
		 "argument %d of %s not built with a constructor of type %s"
		 n name (Types.name a)
	     in
	     raise (Incompatible_type msg)
	   end;
	   (* recursive call *)
	   (Obj.obj (objectify name (succ n) b (f y)) : 'b))
    else
      Obj.repr x

  let add tbl s ty x =
    if !use_obj then begin
      if Hashtbl.mem tbl s then raise (AlreadyExists s);
      let o = objectify s 1 ty x in
      Hashtbl.add tbl s { ty = ty; o = o };
      Obj.obj o
    end else
      x

  let find tbl s ty =
    if !use_obj then
      try
	let data = Hashtbl.find tbl s in
	if ty <> data.ty then type_error s ty data.ty;
	Obj.obj data.o
      with Not_found ->
	raise (Unbound_value s)
    else
      invalid_arg "cannot call function 'find' in the 'no obj' mode"

end

module Binding = struct

  let bindings : string ObjTbl.t = ObjTbl.create ()

  let add ty v var = ObjTbl.add bindings ty v var (* eta-expansion required *)

  exception Name_already_exists of string
  let check_name s =
    let error () =
      Format.eprintf "[Type] A value of name %s already exists@." s;
      raise (Name_already_exists s)
    in
    ObjTbl.iter bindings (fun s' -> if s = s' then error ())

  let add_once ty x s =
    check_name s;
    add ty x s

  let find = ObjTbl.find bindings

end

let () = find_binding := Binding.find

(* add bindings for [Format.std_formatter] and [Format.err_formatter] *)
let () = 
  Binding.add formatter Format.std_formatter "Format.std_formatter";
  Binding.add formatter Format.err_formatter "Format.err_formatter"

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
