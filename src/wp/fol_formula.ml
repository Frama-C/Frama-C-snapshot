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

open Fol_decl
open Formula

(* -------------------------------------------------------------------------- *)
(* --- OPERATORS                                                          --- *)
(* -------------------------------------------------------------------------- *)

let i_pred = function
  | Ceq  -> eq_int
  | Cneq -> ne_int
  | Clt  -> lt_int
  | Cleq -> le_int

let i_bool = function
  | Ceq  -> eq_int_bool
  | Cneq -> ne_int_bool
  | Clt  -> lt_int_bool
  | Cleq -> le_int_bool

let i_op = function
  | Iadd -> add_int
  | Isub -> sub_int
  | Imul -> mul_int
  | Idiv -> div_int
  | Imod -> mod_int

let r_pred = function
  | Ceq  -> eq_real
  | Cneq -> ne_real
  | Clt  -> lt_real
  | Cleq -> le_real

let r_bool = function
  | Ceq  -> eq_real_bool
  | Cneq -> ne_real_bool
  | Clt  -> lt_real_bool
  | Cleq -> le_real_bool

let r_op = function
  | Radd -> add_real
  | Rsub -> sub_real
  | Rmul -> mul_real
  | Rdiv -> fract_real

      
type 'a term = Fol.term
type pred = Fol.pred
type decl = Fol.decl
    
type abstract = m_abstract term
type integer = m_integer term
type real = m_real term
type boolean = m_boolean term
type record =  m_record term
type urecord = m_array term
type array =  m_array term
type set = m_set term
type name = m_integer term
    
type var = Fol.Var.t
    
let e_true = Fol.e_true
let e_false = Fol.e_false
  
let e_int k   = Fol.e_int k
let e_int64 k = Fol.e_int64 k
let e_float k = Fol.e_float k
let e_icst z = Fol.e_cnst (Fol.c_int_of_str z)
let e_rcst z =  Fol.e_cnst (Fol.c_float_of_str z)
  
let wrap t = t
let unwrap t = t
  
let e_call f ts = Fol.e_app f ts
let p_call f xs = Fol.p_app f xs
  
let e_access = Fol.e_access
let e_update = Fol.e_update
let e_getfield = Fol.e_getfield
let e_setfield = Fol.e_setfield
  
let unop f a = unwrap (e_call f [wrap a])
let binop f a b = unwrap (e_call f [wrap a;wrap b])
let predop f a b = p_call f [wrap a;wrap b]
  
let e_ineg = unop neg_int
let e_rneg = unop neg_real
  
let e_icmp op = binop (i_bool op)
let p_icmp op = predop (i_pred op)
  
let e_rcmp op = binop (r_bool op)
let p_rcmp op = predop (r_pred op)
  
let e_iop op = binop (i_op op)
let e_rop op = binop (r_op op)
  
let real_of_integer = unop real_of_integer
let integer_of_real = unop integer_of_real
  
let a_true = wrap (e_int 1)
let a_false = wrap (e_int 0)
  
let e_bool (c : boolean) : integer  =
  unwrap (e_call "ite" [wrap c;a_true;a_false])
    
let e_cond (c : boolean) (a : 'a term) (b : 'a term) : 'a term =
  unwrap (e_call "ite" [wrap c;wrap a;wrap b])
    
let e_not = unop bool_not
let e_and = binop bool_and
let e_or  = binop bool_or
  
let e_bnot   = unop "bnot"
let e_band   = binop "band"
let e_bor    = binop "bor"
let e_bxor   = binop "bxor"
let e_lshift = binop "lshift"
let e_rshift = binop "rshift"
  
let i_zero = e_int 0
let i_one = e_int 1
let i_sub = e_iop Isub
let i_add = e_iop Iadd
let i_mult = e_iop Imul
  
let r_zero = e_float 0.0
  
(* --------------------------------------------------------------------- *)
(* --- Declaration                                                   --- *)
(* --------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Format

let rec is_incl_comp c1 c2 =
  match object_of c2 with
    | C_comp c -> Cil_datatype.Compinfo.equal c1 c ||
        List.exists (fun t -> is_incl_comp c1 t.ftype) c.cfields
    | C_array arr -> is_incl_comp c1 arr.arr_element
    | _ -> false

(* Ensures that structures comes in the correct order *)
	
let comp_compare c1 c2 =
  if (List.exists (fun t -> is_incl_comp c1 t.ftype) c2.cfields) then  -1
  else
    if (List.exists (fun t -> is_incl_comp c2 t.ftype) c1.cfields) then 1
    else Cil_datatype.Compinfo.compare c1 c2
      
let part_of_item = function
  | Formula.Type _ -> 1
  | Formula.Cons _ -> 2
  | Formula.FunctionDef _
  | Formula.PredicateDef _
  | Formula.Function _ 
  | Formula.Predicate _ -> 3
  | Formula.Axiom _ -> 4
  | Formula.Trecord _ -> 5
      
let compare_item d1 d2 =
  let p = part_of_item d1.Formula.d_item - part_of_item d2.Formula.d_item in
  if p = 0 then
    match d1.Formula.d_item , d2.Formula.d_item with
      | Formula.Trecord c1 , Formula.Trecord c2 -> comp_compare c1 c2
      | Formula.Cons i , Formula.Cons j -> i - j
      | _ ,_ -> String.compare d1.Formula.d_name d2.Formula.d_name
  else p
    

module Dset = Set.Make
  (struct
     type t = decl
     let compare = compare_item
   end)
  
let gindex : (string,decl) Hashtbl.t = Hashtbl.create 731
let gfresh = Hashtbl.create 131
  
let fresh_name prefix basename =
  let x = identifier basename in
  let m = if x="" then prefix else Printf.sprintf "%s_%s" prefix x in
  try
    let p = Hashtbl.find gfresh m in
    incr p ; Printf.sprintf "%s_%d" m !p
  with Not_found ->
    Hashtbl.add gfresh m (ref 0) ; m

(* order not really import, but they are in order *)      

let s_index = function
  | Formula.S_Type -> 0
  | Formula.S_Cons -> 1
  | Formula.S_Logic_Sig -> 2
  | Formula.S_Logic_Def -> 3
  | Formula.S_Logic_Prop -> 4
  | Formula.S_Model_Sig -> 5
  | Formula.S_Model_Def -> 6
  | Formula.S_Model_Prop -> 7
  | Formula.S_User_Sig -> 8
  | Formula.S_User_Prop -> 9

let gsection = Array.create 10 Dset.empty

(* order is important, and they are in order *)      

let gtoc = [|
  Formula.S_Type , "Type Definitions" ;
  Formula.S_Cons , "Type Constructors" ;
  Formula.S_Logic_Sig , "Logic Signatures" ;
  Formula.S_Logic_Def , "Logic Definitions" ;
  Formula.S_Logic_Prop , "Logic Properties" ;
  Formula.S_Model_Sig , "Model Signatures" ;
  Formula.S_Model_Def , "Model Definitions" ;
  Formula.S_Model_Prop , "Model Properties" ;
  Formula.S_User_Sig , "User-defined Signatures" ;
  Formula.S_User_Prop , "User-defined Properties" ;
|]

let gclear = ref []

let clear () =
  begin
    Hashtbl.clear gfresh ;
    Hashtbl.clear gindex ;
    Array.fill gsection 0 (Array.length gsection) Dset.empty ;
    List.iter (fun f -> f ()) !gclear ;
  end
let on_clear f = gclear := !gclear @ [f]

let locked = ref false
  (* ensures that no declaration has been added during the iteration *)

let has_declaration = Hashtbl.mem gindex
  
let compile_let_item = function
  | (Formula.Cons _ | Formula.Type _
    | Formula.Function _ | Formula.Predicate _ | Formula.Trecord _) as item -> item
  | Formula.FunctionDef(xs,t,e) -> Formula.FunctionDef(xs,t,Fol_eval.elet_expansion e)
  | Formula.PredicateDef(xs,p) -> 
      let xs,p = Fol_let.compile_def xs p in
      Formula.PredicateDef(xs,p)
  | Formula.Axiom p -> Formula.Axiom (Fol_let.compile p)
      
let compile_let_decl d =
  {
    Formula.d_section = d.Formula.d_section ;
    Formula.d_name = d.Formula.d_name ;
    Formula.d_title = d.Formula.d_title ;
    Formula.d_descr = d.Formula.d_descr ;
    Formula.d_source = d.Formula.d_source ;
    Formula.d_item = compile_let_item d.Formula.d_item ;
  }

let rec add_declaration d =
  try
    Wp_parameters.debug ~dkey:"logic" "Adding declaration %s (%t)@."
      d.Formula.d_name d.Formula.d_title ;
    if !locked then Wp_parameters.fatal
      "Locked datalib (when declaring %t)" d.Formula.d_title ;
    let old = Hashtbl.find gindex d.Formula.d_name in
    Wp_parameters.fatal
      "Duplicate definition for name '%s':@ Old: %t@ New: %t"
      d.Formula.d_name old.Formula.d_title d.Formula.d_title
  with Not_found ->
    begin
      Hashtbl.add gindex d.Formula.d_name (compile_let_decl d) ;
      let s = s_index d.Formula.d_section in
      gsection.(s) <- Dset.add d gsection.(s)
    end

(* -------------------------------------------------------------------------- *)
(* --- Dependencies for macros                                            --- *)
(* -------------------------------------------------------------------------- *)

open Fol

module Mset = Datatype.String.Set

let exported_macros = ref Mset.empty

let is_macro d = match d.d_item with
  | FunctionDef _ | PredicateDef _ -> true
  | _ -> false

let is_macro_section = function
  | S_Type | S_Cons 
  | S_Logic_Sig | S_Logic_Prop 
  | S_Model_Sig | S_Model_Prop 
  | S_User_Sig | S_User_Prop -> false
  | S_Logic_Def | S_Model_Def -> true

let rec do_export f d =
  export_depends_for_item f d.d_item ;
  if not (Mset.mem d.d_name !exported_macros) then
    ( f d ; exported_macros := Mset.add d.d_name !exported_macros )

and export_depends_for_item f = function
  | Type _ | Cons _ | Function _ | Predicate _ | Trecord _ | Axiom _ -> ()
  | FunctionDef(_,_,exp) -> export_depends_for_term f exp
  | PredicateDef(_,prop) -> export_depends_for_pred f prop

and export_depends_for_name f x =
  try
    let d = Hashtbl.find gindex x in
    if is_macro d then do_export f d
  with Not_found -> ()
      
and export_depends_for_term f = function
  | Tconst _ | Tvar _ -> ()
  | Tapp(x,ts) -> 
      export_depends_for_name f x ; 
      List.iter (export_depends_for_term f) ts
  | Tgetfield(_,t) -> 
      export_depends_for_term f t
  | Tsetfield(_,t,v) | Taccess(t,v) | Tlet(_,t,v) -> 
      export_depends_for_term f t ; 
      export_depends_for_term f v
  | Tupdate(t,v,w) | Tif(t,v,w) -> 
      export_depends_for_term f t ; 
      export_depends_for_term f v ;
      export_depends_for_term f w

and export_depends_for_pred f = function
  | Papp(x,ts) -> 
      export_depends_for_name f x ; 
      List.iter (export_depends_for_term f) ts
  | Ptrue | Pfalse -> ()
  | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) ->
      export_depends_for_pred f p ;
      export_depends_for_pred f q
  | Pif(c,p,q) ->
      export_depends_for_term f c ;
      export_depends_for_pred f p ;
      export_depends_for_pred f q
  | Pnot p | Pnamed(_,p) | Pexists(_,p) | Pforall(_,p) ->
      export_depends_for_pred f p
  | Plet(_,t,p) ->
      export_depends_for_term f t ;
      export_depends_for_pred f p

let iter_all section f =
  try
    locked := true ;
    exported_macros := Mset.empty ;
    Array.iter
      (fun (s,t) ->
	 let k = s_index s in
	 if not (Dset.is_empty gsection.(k)) then
	   begin
	     let job = if is_macro_section s then do_export f else f in
	     section t ; Dset.iter job gsection.(k) ;
	   end
      ) gtoc ;
    locked := false ;
    exported_macros := Mset.empty ;
  with e ->
    locked := false ;
    exported_macros := Mset.empty ;
    raise e

(* -------------------------------------------------------------------------- *)
(* --- Declaration Functors                                               --- *)
(* -------------------------------------------------------------------------- *)
    
module type Identifiable =
sig
  type t
  module H : Hashtbl.S
  val index : t -> H.key
  val prefix : string
  val basename : t -> string
  val location : t -> Lexing.position option
  val pp_title : Format.formatter -> t -> unit
  val pp_descr : Format.formatter -> t -> unit
end

module type Registry =
sig
  type t
  val define : t -> unit
  val get_definition : t -> Fol.decl
  val on_definition : (t -> Fol.decl -> unit) -> unit
end

module type Declarator =
sig
  include Identifiable
  val clear : unit -> unit
  val section : Formula.section
  val declare : t -> string -> (Fol.Var.t,Fol.term,Fol.pred) Formula.item
end

module DRegister
  (D : Declarator) :
  (Registry with type t = D.t) =
struct
  let () = register_prefix D.prefix

  type t = D.t
  let index : Fol.decl D.H.t = D.H.create 131
  let demons : (t -> Fol.decl -> unit) list ref = ref []

  let () = on_clear (fun () -> D.clear () ; D.H.clear index)

  let get_definition x =
    let k = D.index x in
    try D.H.find index k
    with Not_found ->
      let name = fresh_name D.prefix (D.basename x) in
      let item = D.declare x name in
      let d = {
        Formula.d_name = name ;
        Formula.d_section = D.section ;
        Formula.d_source = D.location x ;
        Formula.d_item = item ;
        Formula.d_title = (fun fmt -> D.pp_title fmt x) ;
        Formula.d_descr = (fun fmt -> D.pp_descr fmt x) ;
      } in
      add_declaration d ;
      D.H.add index k d ;
      List.iter (fun f -> f x d) !demons ;
      d

  let define x = ignore (get_definition x)
  let on_definition f = demons := !demons @ [f]
end



(* -------------------------------------------------------------------------- *)
(* --- Built-in Identifiables                                             --- *)
(* -------------------------------------------------------------------------- *)
let rec pp_dim fmt = function
  | TArray (typ_elt,lo,_,_) ->
      (match lo with
         | Some lo ->
             Format.fprintf fmt "[%a]" !Ast_printer.d_exp lo
         | None ->
             Format.fprintf fmt "[]"); pp_dim fmt typ_elt
  | _ -> ()


let rec pp_ctype dim fmt = function
  | TInt(ikind,_) -> Format.fprintf fmt "%a" Cil.d_ikind ikind
  | TFloat(fkind,_) -> Format.fprintf fmt "%a" Cil.d_fkind fkind
  | TPtr(typ,_) -> Format.fprintf fmt "%a*" (pp_ctype dim) typ
  | TFun _ as t ->  Format.fprintf fmt "%a*" (pp_ctype dim) t
  | TEnum (e,_) -> Format.fprintf fmt "enum %s " e.ename
  | TComp (comp,_,_) ->
      Format.fprintf fmt
        "%s %s"
        (if comp.cstruct then "struct" else "union")
        comp.cname

  | TArray (typ_elt,_,_,_) as t ->
      pp_ctype false fmt typ_elt ;
      if dim then pp_dim fmt t;

  | TBuiltin_va_list _ -> pp_print_string fmt "builtin type"
  | TVoid _ -> pp_print_string fmt "void"
  | TNamed (t,_)  -> Format.fprintf fmt "%s" t.tname

module Varinfo : Identifiable with type t = varinfo =
struct
  type t = varinfo
  let prefix = "X"
  let index x = x
  let basename x = x.vname
  let location x = Some(fst x.vdecl)
  module H = Cil_datatype.Varinfo.Hashtbl
  let pp_title fmt x =
    if x.vglob
    then Format.fprintf fmt "Global '%a'" !Ast_printer.d_var x
    else Format.fprintf fmt "Local '%a'" !Ast_printer.d_var x
  let pp_descr fmt x =
    Format.fprintf fmt "%a %a ;"
      (pp_ctype true) x.vtype !Ast_printer.d_var x
end

module Varaddr : Identifiable with type t = varinfo =
struct
  include Varinfo
  let prefix = "A"
  let pp_title fmt x = Format.fprintf fmt "Address of '%a'" pp_title x
end

module Fieldinfo : Identifiable with type t = fieldinfo =
struct
  type t = fieldinfo
  let prefix = "F"
  let index f = f
  let basename f = f.fname
  let location f = Some(fst f.floc)
  module H = Cil_datatype.Fieldinfo.Hashtbl
  let pp_title fmt f = Format.fprintf fmt "Field '%s'" f.fname
  let pp_descr fmt f =
    Format.fprintf fmt "@[<hov 0>@[<hov 2>%s %s {@ ... ;@ %a %s ;@ ...@]@ }@]"
      (if f.fcomp.cstruct then "struct" else "union")
      f.fcomp.cname
      !Ast_printer.d_type f.ftype f.fname
end

module Compinfo : Identifiable with type t = compinfo =
struct
  type t = compinfo
  let prefix = "C"
  let index c = c
  let basename c = c.cname
  let location c =
    match c.cfields with
      | f :: _ ->
          let s = fst f.floc in
          if s.Lexing.pos_fname = "" then None else Some s
      | [] -> None
  module H = Cil_datatype.Compinfo.Hashtbl
  let pp_title fmt c =
    Format.fprintf fmt "%s '%s'"
      (if c.cstruct then "Struct" else "Union") c.cname
  let pp_descr fmt c =
    Format.fprintf fmt "typedef %s %s { ... }"
      (if c.cstruct then "struct" else "union") c.cname
end

module Arrayinfo : Identifiable with type t = arrayinfo =
struct
  type t = arrayinfo
  let prefix = "A"
  module H = Hashtbl.Make(AinfoComparable)
  let index a = a
  let location _ = None
  let basename a = Ctypes.basename (C_array a)
  let pp_title fmt a = Ctypes.pretty fmt (C_array a)
  let pp_descr fmt _ = Format.fprintf fmt "Logic array"
end

module LTypeinfo : Identifiable with type t = Cil_types.logic_type =
struct
  type t = Cil_types.logic_type
  let prefix = "AT"
  let index c = c
  let basename ty =
    let rec typ_basename ty =
      match object_of ty with
        | C_int i -> Pretty_utils.sfprintf "%a" Ctypes.pp_int i
        | C_float f -> Pretty_utils.sfprintf "%a" Ctypes.pp_float f
        | C_pointer _ -> "pointer"
        | C_comp c -> (if c.cstruct then "struct_" else "union_")^
            c.cname
        | C_array arr -> typ_basename arr.Ctypes.arr_element^"_array"
    in
    match ty with
      | Ctype c -> "is_"^typ_basename c
      | Linteger | Lreal  -> ""
      | Lvar x -> x
      | Ltype (lt,_) -> lt.lt_name
      | Larrow _ ->""

  let location _ = None
  module H = Cil_datatype.Logic_type.Hashtbl
  let pp_title fmt _ =
    Format.fprintf fmt "Acsl type"
  let pp_descr fmt _ =
    Format.fprintf fmt "Declaration"
end

module Logicvar : Identifiable with type t = logic_var =
struct
  type t = logic_var
  let prefix = "D"
  let index x = x
  let basename x = x.lv_name
  let location _ = None
  module H = Cil_datatype.Logic_var.Hashtbl
  let pp_title fmt x = Format.fprintf fmt "Logic variable '%s'" x.lv_name
  let pp_descr fmt _x = Format.fprintf fmt "Declaration"
end


module HC_object =
struct
  type t = Ctypes.c_object
  let equal = Ctypes.equal
  let hash = Ctypes.hash
end

module Cobject : Identifiable with type t = Ctypes.c_object =
struct
  type t = Ctypes.c_object
  let prefix = "Ct"
  let index t = t
  let basename x = Ctypes.basename x
  let location _ = None
  module H = Hashtbl.Make(HC_object)
  let pp_title fmt x = Format.fprintf fmt "C type '%a'" Ctypes.pp_object x
  let pp_descr fmt _ = Format.fprintf fmt "Declaration"
end

module HC_ArrayDim =
struct
  type t = Ctypes.c_object * int (* object with n-dimensions (number of []) *)
  let equal (ta,n) (tb,m) = (n=m) && Ctypes.equal ta tb
  let hash (ta,n) = 31*n + 73*Ctypes.hash ta
end

module ArrayDim : Identifiable with type t = Ctypes.c_object * int =
struct
  type t = Ctypes.c_object * int
  let prefix = "Ca"
  let index t = t
  let basename (te,n) = 
    if n > 1 then Printf.sprintf "%s_d%d" (Ctypes.basename te) n
    else Ctypes.basename te
  let location _ = None
  module H = Hashtbl.Make(HC_ArrayDim)
  let pp_title fmt (te,n) = 
    Format.fprintf fmt "Array %a" Ctypes.pp_object te ;
    for i = 1 to n do Format.fprintf fmt "[]" done
  let pp_descr fmt _ = Format.fprintf fmt "Declaration"
end

(* ----------------------------------------------------------------------- *)
(* --- User Type Registry                                              --- *)
(* ----------------------------------------------------------------------- *)

module LTinfo =
struct
  type t = logic_type_info
  let compare t1 t2 = String.compare t1.lt_name t2.lt_name
  let hash v = Hashtbl.hash v.lt_name
  let equal t1 t2 = t1.lt_name = t2.lt_name
end

module LTinfoId : Identifiable with type t = logic_type_info  =
struct
  type t = logic_type_info
  let prefix = "T"
  let index t = t
  let basename c = c.lt_name
  let location _c = None
  module H = Hashtbl.Make(LTinfo)
  let pp_title fmt x =
    Format.fprintf fmt "Logic type '%s'" x.lt_name
  let pp_descr fmt _x =
    Format.fprintf fmt "Declaration"
end

module ADTDecl = DRegister
  (struct
     include LTinfoId
       let declare t _ =
         Formula.Type (List.length t.lt_params)
       let section = Formula.S_Type
       let clear () = ()
   end)

let adt_decl lt = (ADTDecl.get_definition lt).Formula.d_name
let () = Fol_decl.Tau.name_of_adt := adt_decl

(* --------------------------------------------------------------------- *)
(* --- HOL                                                           --- *)
(* --------------------------------------------------------------------- *)

type pool = (string,int option) Hashtbl.t
    
let pool () = Hashtbl.create 7
  
let fresh_var pool basename =
  let new_contents = 
    try
      let counter = Hashtbl.find pool basename in
      match counter with
	| None -> Some 0
	| Some i -> Some (succ i)
    with Not_found -> None
  in
  Hashtbl.replace pool basename new_contents;
  new_contents
    
let p_fresh pool x kind =
  let x = 
    if Fol_decl.has_reserved_prefix (x^"_") then
      let new_name = "G_"^x in
      if Fol_decl.has_reserved_prefix new_name then
	Wp_parameters.fatal
          "Reserved prefix for '%s' is clashing. Frama-C WP plugin cannot reserve the prefix 'G'"
          new_name
      else new_name
    else x
  in
  let vx =
    let tau, ltype = match kind with
      | Formula.Model t -> t, None
      | Formula.Acsl(t,ty) -> t, Some (ty)
    in Fol.Var.mk x (fresh_var pool x) tau ltype
  in vx
       
let var v = Fol.e_var v
  
let tau_of_var = Fol.Var.var_type
let name_of_var = Fol.Var.var_name
let basename_of_var = Fol.Var.basename
let kind_of_var = Fol.Var.kind_of_var
  
let p_freshen pool v = p_fresh pool (Fol.Var.basename v) (Fol.Var.kind_of_var v)
  
let p_true = Fol.Ptrue
let p_false = Fol.Pfalse
  
let p_not a   = Fol.p_not a
let p_bool a  = Fol.p_eq e_true a
  
let equal_terms e1 e2 = Fol.eq_terms e1 e2
  
let p_and a b = Fol.p_and a b
let p_or  a b = Fol.p_or a b
let p_xor  a b = Fol.p_xor a b
let p_eq a b = Fol.p_eq a b
let p_neq a b = Fol.p_neq a b
let p_iff a b = Fol.p_iff a b
let p_conj l = Fol.p_conj l
let p_disj l = Fol.p_disj l
let p_cond b pt pf = Fol.p_if (wrap b) pt pf
let p_named = Fol.p_named
let is_true = Fol.is_true
let is_false = Fol.is_false
  
let eq_var = Fol.Var.equal
  
let e_subst alpha x v t =
  match v with
    | Fol.Tvar y when (Fol.Var.equal x y) -> t (* v is equal to x *)
	
    | (Fol.Tconst _ | Fol.Tvar _ | Fol.Tapp(_,[]))
        when Wp_parameters.Simpl.get () ->
        Fol.term_replace alpha x v t
	  
    | _ -> Fol.e_let x v t
	
let p_forall xs p =
  List.fold_right Fol.p_forall xs p
    
let p_exists xs p =
  List.fold_right Fol.p_exists xs p
    
let p_subst alpha x v p =
  match v with
    | Fol.Tvar y when (Fol.Var.equal x y) -> p (* v is equal to x *)
	
    | (Fol.Tconst _ | Fol.Tvar _ | Fol.Tapp(_,[]))
        when Wp_parameters.Simpl.get () ->
        Fol.pred_replace alpha x v p
	  
    | _ -> Fol.p_let x v p
	
let p_implies h p = Fol.p_implies h p
  
let rec apply alpha x = match alpha with
  | [] -> None
  | (y,y')::s ->
      if Fol.Var.equal x y then Some y' else apply s x
	
let rec e_rename s t =
  match t with
    | Fol.Tconst _ -> t
    | Fol.Tvar x ->
        ( match apply s x with
            | None -> t
            | Some y -> Fol.e_var y )
    | Fol.Tapp(f,ts) -> Fol.e_app f (List.map (e_rename s) ts)
    | Fol.Taccess(t,i) -> e_access (e_rename s t) (e_rename s i)
    | Fol.Tupdate(t,i,v) -> e_update (e_rename s t) (e_rename s i) (e_rename s v)
    | Fol.Tgetfield(f,r) -> e_getfield f (e_rename s r)
    | Fol.Tsetfield(f,r,v) -> e_setfield f (e_rename s r) (e_rename s v)
    | Fol.Tif(a,b,c) -> Fol.e_if (e_rename s a) (e_rename s b) (e_rename s c)
    | Fol.Tlet(y,a,b) ->
        let a' = e_rename s a in
        let s' = (y,y)::s in (* defensive ! *)
        Fol.e_let y a' (e_rename s' b)
	  
let term_has_var = Fol.e_has_var
let pred_has_var = Fol.p_has_var
  
let term_calls = Fol.term_calls
let pred_calls = Fol.pred_calls

let term_closed t = Fol.e_closed [] t
let pred_closed p = Fol.p_closed [] p
  
type alpha = Fol.Var.t Fol.Vmap.t
let empty_alpha = Fol.Vmap.empty
let fold_alpha = Fol.Vmap.fold
let p_more_alpha_cv = Fol.pred_alpha_cv
let p_alpha_cv p = Fol.p_alpha_cv p


(* -------------------------------------------------------------------------- *)
(* --- Free Variables                                                     --- *)
(* -------------------------------------------------------------------------- *)


let rec free_term xs = function
  | Tconst _ -> xs
  | Tvar x -> Vset.add x xs
  | Tapp(_,ts) -> List.fold_left free_term xs ts
  | Tgetfield(_,t) -> free_term xs t
  | Tsetfield(_,t,t') | Taccess(t,t') -> free_term (free_term xs t) t'
  | Tupdate(t1,t2,t3) | Tif(t1,t2,t3) -> free_term (free_term (free_term xs t1) t2) t3
  | Tlet(x,a,b) ->
      if Vset.mem x xs then
	free_term (free_term xs b) a
      else
	free_term (Vset.remove x (free_term xs b)) a

let rec free_pred xs = function
  | Papp(_,ts) -> List.fold_left free_term xs ts
  | Ptrue | Pfalse -> xs
  | Pimplies(p,q) | Pand(p,q) | Por(p,q) | Piff(p,q) -> free_pred (free_pred xs p) q
  | Pnot p | Pnamed(_,p) -> free_pred xs p
  | Pif(a,p,q) -> free_pred (free_pred (free_term xs a) p) q
  | Plet(x,t,p) ->
      if Vset.mem x xs then
	free_term (free_pred xs p) t
      else
	free_term (Vset.remove x (free_pred xs p)) t
  | Pexists(x,p) | Pforall(x,p) ->
      if Vset.mem x xs then
	free_pred xs p
      else
	Vset.remove x (free_pred xs p)

let freevars p = Vset.elements (free_pred Vset.empty p)
  
(* -------------------------------------------------------------------------- *)
(* --- Pretty Print                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec pp_term fmt t = Fol_pretty.fpretty_term pp_term fmt t
  
let () = Fol.pp_term := pp_term

(*Be careful, only for debug *)
let pp_tau = Fol_pretty.pp_tau
  
let rec pp_pred_atom fmt p =
  Fol_pretty.epp_pred_atom {
    Fol_pretty.pp_type =  Fol_pretty.pp_tau ;
    Fol_pretty.pp_term = pp_term;
    Fol_pretty.pp_pred = pp_pred_atom ;
  } fmt p
    
let pp_pred_vbox fmt p =
  Fol_pretty.epp_pred_vbox {
    Fol_pretty.pp_type = Fol_pretty.pp_tau ;
    Fol_pretty.pp_term = pp_term ;
    Fol_pretty.pp_pred = pp_pred_atom ;
  } fmt p
    
let rec pp_pred_debug fmt p =
  Fol_pretty.fpp_pred pp_pred_debug pp_tau pp_term fmt p
    
let pp_pred fmt p =
  if Wp_parameters.debug_atleast 1 then
    pp_pred_debug fmt p
  else
    pp_pred_vbox fmt p

let pp_section = Fol_pretty.pp_section          
let pp_term fmt t = pp_term fmt t
let pp_decl fmt d = Fol_pretty.fpp_decl pp_term pp_pred fmt d
let pp_goal fmt x g = Fol_pretty.fpp_goal pp_pred fmt x g
  
let pp_var fmt x = pp_term fmt (Fol.e_var x)
  
let pp_vkind fmt = function
  | Formula.Model t -> pp_tau fmt t
  | Formula.Acsl (_t,ty) -> !Ast_printer.d_logic_type fmt ty

let huge_term = Fol.huge_term
let huge_pred = Fol.huge_pred

(* -------------------------------------------------------------------------- *)
(* --- Creates the Data Library                                           --- *)
(* -------------------------------------------------------------------------- *)

type interval = {
  inf : integer option ;
  sup : integer option ;
}
    
type 'a assigned =
  | Aloc of Ctypes.c_object * 'a
  | Arange of Ctypes.c_object * 'a * interval
      
type havoc =
  | Fresh of var
  | Update of var * ((var * var) list  -> abstract)

let pp_interval fmt rg =
  let pp_opt fmt = function
    | None -> ()
    | Some d -> pp_term fmt d
  in
  Format.fprintf fmt "[%a..%a]" pp_opt rg.inf pp_opt rg.sup
    
(* ------------------------------------------------------------------------ *)
(* --- Calling Logic Functions and Predicates                           --- *)
(* ------------------------------------------------------------------------ *)
    
let p_app0 f = p_call f []
let p_app1 f a = p_call f [wrap a]
let p_app2 f a b = p_call f [wrap a;wrap b]
let p_app3 f a b c = p_call f [wrap a;wrap b;wrap c]
let p_app4 f a b c d = p_call f [wrap a;wrap b;wrap c;wrap d]
let p_app5 f a b c d e = p_call f [wrap a; wrap b; wrap c; wrap d; wrap e]
  
let e_app0 f = unwrap (e_call f [])
let e_app1 f a = unwrap (e_call f [wrap a])
let e_app2 f a b = unwrap (e_call f [wrap a;wrap b])
let e_app3 f a b c = unwrap (e_call f [wrap a;wrap b;wrap c])
let e_app4 f a b c d =
  unwrap (e_call f [wrap a;wrap b;wrap c;wrap d])
let e_app5 f a b c d e =
  unwrap (e_call f [wrap a;wrap b;wrap c;wrap d; wrap e])
    
(* -------------------------------------------------------------------------- *)
(* --- Dummy                                                              --- *)
(* -------------------------------------------------------------------------- *)
    
let gdummy = ref 0
  
let () = on_clear (fun () -> gdummy :=0)
let dummy () = incr gdummy; p_app1 "dummy" (e_int !gdummy)
  
(* ------------------------------------------------------------------------ *)
(* --- Logic Integer Cast                                               --- *)
(* ------------------------------------------------------------------------ *)
  
let modulo ti e = e_app1 (mk_imodulo ti) e
let guard ti e = p_app1 (mk_iguard ti) e
let i_convert tfrom tto e =
  if Ctypes.sub_c_int tfrom tto then e else modulo tto e
        
(* ------------------------------------------------------------------------ *)
(* --- Set Interface                                                    --- *)
(* ------------------------------------------------------------------------ *)
  
let empty = e_app0 mk_empty
let singleton e = e_app1 mk_singleton e
  
let union a b = e_app2 mk_union a b
  
let unions l =
  let rec unions_aux l s =
    match l with
      | [] -> s
      | a::m -> unions_aux m (union a s)
  in
  match l with
    | [] -> empty
    | a::m -> unions_aux m a
	
let inter a b = e_app2 mk_inter a b
let remove a b = e_app2 mk_remove a b
let add_set s1 s2 = e_app2 mk_radd s1 s2
  
let mult_set s1 s2 = e_app2 mk_rmult s1 s2
let neg_set s1 = e_app1 mk_rneg s1
  
(* ------------------------------------------------------------------------ *)
(* --- Integer Range Inteface                                           --- *)
(* ------------------------------------------------------------------------ *)
  
let loc_range = e_app2 mk_range
let range_inf = e_app1 mk_range_inf
let range_sup = e_app1 mk_range_sup
let integers  = e_app0 mk_integers
  
let rec set_of_list = function
  | [] -> empty
  | a::m -> union (singleton a) (set_of_list m)
      
let interval r =
  match r.inf,r.sup with
    | None,None -> integers
    | Some i, None -> range_inf i
    | None, Some j -> range_sup j
    | Some i, Some j -> loc_range i j
	
let seed = ref 0
  
let set_range_index t ri =
  let rdm = e_int (incr seed ; !seed) in
  e_app3 set_range_index t (interval ri) rdm
    

(* ------------------------------------------------------------------------ *)
(* --- Records, Unions and Arrays as First Class Values                 --- *)
(* ------------------------------------------------------------------------ *)
  
module RecName = DRegister
  (struct
     include Compinfo
     let declare tcomp _eqname =
       Formula.Trecord tcomp
     let prefix = " "
     let section = S_Type
     let clear () = ()
     let pp_title _fmt _x = ()
   end)

let acc_field (s:record) (f:Cil_types.fieldinfo) =
  RecName.define f.Cil_types.fcomp ; e_getfield f s
    
let upd_field (s:record) f v : record  =
  RecName.define f.Cil_types.fcomp ; e_setfield f s v
    

let acc_index = e_access
let upd_index = e_update

(* -------------------------------------------------------------------------- *)
(* --- Index Functor                                                       --- *)
(* -------------------------------------------------------------------------- *)
  
let gindexref = ref 0
  
module type Indexed =
sig
  include Registry
  val get_ind : t -> integer
  val has_ind : t -> pred -> bool
end
  
module Dindex
  (I : Identifiable) :
  (Indexed with type t = I.t) =
struct
  let hindex = I.H.create 131
  include DRegister
  (struct
     include I
     let clear () = I.H.clear hindex
     let section = S_Cons
     let declare _ _ =
       (* MUST BE INCREMENTED BEFORE: 0 is reserved for models *)
       incr gindexref ;
       Formula.Cons (!gindexref)
   end)
  let get_ind x =
    let k = I.index x in
    try I.H.find hindex k
    with Not_found ->
      let d = get_definition x in
      let t : integer =  unwrap (e_call d.d_name []) in
      I.H.add hindex k t ; t

  let has_ind x p =
    try
      let d = get_definition x in
      Fol.pred_calls d.d_name p
    with Not_found -> false

end
  
(* -------------------------------------------------------------------------- *)
(* --- Built-in Names                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Findex = Dindex(Fieldinfo)
module Xindex = Dindex(Varinfo)
module Tindex = Dindex(Compinfo)
module Aindex = Dindex(Varaddr)
module LTindex = Dindex(LTypeinfo)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
