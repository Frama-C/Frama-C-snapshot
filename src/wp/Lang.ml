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
(* --- Logical Language                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes
open Qed
open Qed.Logic

(* -------------------------------------------------------------------------- *)

let basename def name =
  let rec lookup def s k n =
    if k < n then 
      let c = s.[k] in
      if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') 
      then String.sub s k 1
      else lookup def s (succ k) n
    else def
  in lookup def name 0 (String.length name)

(* -------------------------------------------------------------------------- *)
(* Naming Prefixes                                                           
   Names starting with a lower-case character belong to logic language
   or external model(s).

  'pointer' Pointer type
  'Lit_<hex>' String Literal Values
  'Str_<eid>' String Literal Pointers
  'S_<s>' Structure <s>
  'U_<u>' Union <u>
  'F_<c>_<f>' Field <f> in compound <c>
  'A_<t>' ACSL Logic type <t> 
  'C_<c>' ACSL Constructor <c>
  'P_<p>' ACSL Predicate <p> (see LogicUsage.get_name)
  'L_<f>' ACSL Logic function <f> (see LogicUsage.get_name)
  'FixP_<p>' ACSL Recursive Predicate <p> (see LogicUsage.get_name)
  'FixL_<f>' ACSL Recursive Logic function <f> (see LogicUsage.get_name)
  'Q_<l>' ACSL Lemma or Axiom
  'S_<n>' Set comprehension predicate
  'Is<phi>' Typing predicate for type <phi>
  'Null<phi>' Null value for type <phi>
*)
let avoid_leading_backlash s = 
  if s.[0]='\\' then 
    let s = String.copy s in
    s.[0]<-'_';
    s
  else s

let comp_id c = 
  if c.cstruct 
  then Printf.sprintf "S_%s" c.cname
  else Printf.sprintf "U_%s" c.cname

let field_id f =
  Printf.sprintf "F_%s_%s" f.fcomp.cname f.fname

let type_id l =
  Printf.sprintf "A_%s" l.lt_name

let logic_id f =
  let name = avoid_leading_backlash (LogicUsage.get_name f) in
  if f.l_type = None 
  then Printf.sprintf "P_%s" name
  else Printf.sprintf "L_%s" name

let ctor_id c = Printf.sprintf "C_%s" (avoid_leading_backlash c.ctor_name)
let lemma_id l = Printf.sprintf "Q_%s" (avoid_leading_backlash l)

(* -------------------------------------------------------------------------- *)

type 'a infoprover =
  {
    altergo: 'a;
    why3   : 'a;
    coq    : 'a;
  }

(* generic way to have different informations for the provers *)

let infoprover x = {
    altergo = x;
    why3    = x;
    coq     = x;
  }

let map_infoprover f i = {
    altergo = f i.altergo;
    why3    = f i.why3;
    coq     = f i.coq;
  }

type library = string

type adt = 
  | Mtype of mdt (* Model type *)
  | Mrecord of mdt * fields (* Model record-type *)
  | Atype of logic_type_info (* Logic Type *)
  | Comp of compinfo (* C-code struct or union *)
and mdt = string extern (** name to print to the provers *)
and 'a extern = {
  ext_id      : int;
  ext_link : 'a infoprover;
  ext_library : library; (** a library which it depends on *)
  ext_debug   : string; (** just for printing during debugging *)
}
and fields = { mutable fields : field list }
and field =
  | Mfield of mdt * fields * string * tau
  | Cfield of fieldinfo
and tau = (field,adt) Logic.datatype

let pointer = Context.create "Lang.pointer"

let new_extern_id = ref (-1)
let new_extern ~debug ~library ~link =
    incr new_extern_id;
    {ext_id     = !new_extern_id;
     ext_library = library;
     ext_debug  = debug;
     ext_link   = link}
let ext_compare a b = Datatype.Int.compare a.ext_id b.ext_id

(* -------------------------------------------------------------------------- *)
(* --- Sorting & Typing                                                   --- *)
(* -------------------------------------------------------------------------- *)

let sort_of_object = function
  | C_int _ -> Logic.Sint
  | C_float _ -> Logic.Sreal
  | C_pointer _ | C_comp _ | C_array _ -> Logic.Sdata

let sort_of_ctype t = sort_of_object (Ctypes.object_of t)

let sort_of_ltype t = match Logic_utils.unroll_type t with
  | Ctype typ -> sort_of_ctype typ
  | Ltype _ | Lvar _ | Larrow _ -> Logic.Sdata
  | Linteger -> Logic.Sint
  | Lreal -> Logic.Sreal

let tau_of_comp c = Logic.Data(Comp c,[])

let array a = Logic.Array(Logic.Int,a)
let farray a b = Logic.Array(a,b)

let rec tau_of_object = function
  | C_int _ -> Logic.Int
  | C_float _ -> Logic.Real
  | C_pointer t -> Context.get pointer t
  | C_comp c -> tau_of_comp c
  | C_array { arr_element = typ } -> array (tau_of_ctype typ)

and tau_of_ctype typ = tau_of_object (Ctypes.object_of typ)

let poly = Context.create "Wp.Lang.poly"

let rec varpoly k x = function
  | [] -> Warning.error "Unbound type parameter <%s>" x
  | y::ys -> if x = y then k else varpoly (succ k) x ys

let builtins = Hashtbl.create 131

let rec tau_of_ltype t = match Logic_utils.unroll_type t with
  | Linteger -> Logic.Int
  | Lreal -> Logic.Real
  | Ctype typ -> tau_of_ctype typ
  | Lvar x -> Logic.Tvar (varpoly 1 x (Context.get poly))
  | Larrow _ ->
      Warning.error "array type non-supported(%a)" 
	Printer.pp_logic_type t
  | Ltype _ as b when Logic_const.is_boolean_type b -> Logic.Bool
  | Ltype(lt,ps) -> 
      try
	let mdt = Hashtbl.find builtins lt.lt_name in
	assert (ps = []) ;
	Logic.Data(Mtype mdt,[])
      with Not_found ->
	Logic.Data(Atype lt,List.map tau_of_ltype ps)

let tau_of_return l = match l.l_type with
  | None -> Logic.Prop
  | Some t -> tau_of_ltype t

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)
  
module ADT =
struct

  type t = adt

  let basename = function
    | Mtype a -> basename "M" a.ext_link.altergo
    | Mrecord(r,_) -> basename "R" r.ext_link.altergo
    | Comp c -> basename (if c.cstruct then "S" else "U") c.corig_name
    | Atype lt -> basename "A" lt.lt_name

  let debug = function
    | Mtype a -> a.ext_debug
    | Mrecord(a,_) -> a.ext_debug
    | Comp c -> comp_id c
    | Atype lt -> type_id lt

  let hash = function
    | Mtype a | Mrecord(a,_) -> Hashtbl.hash a
    | Comp c -> Compinfo.hash c
    | Atype lt -> Logic_type_info.hash lt

  let compare a b =
    if a==b then 0 else
      match a,b with
	| Mtype a , Mtype b -> ext_compare a b
	| Mtype _ , _ -> (-1)
	| _ , Mtype _ -> 1
	| Mrecord(a,_) , Mrecord(b,_) -> ext_compare a b
	| Mrecord _ , _ -> (-1)
	| _ , Mrecord _ -> 1
	| Comp a , Comp b -> Compinfo.compare a b
	| Comp _ , _ -> (-1)
	| _ , Comp _ -> 1
	| Atype a , Atype b -> Logic_type_info.compare a b

  let equal a b = (compare a b = 0)

  let pretty fmt a = Format.pp_print_string fmt (debug a)

end

(* -------------------------------------------------------------------------- *)
(* --- Datatypes                                                          --- *)
(* -------------------------------------------------------------------------- *)

let atype t = 
  try Mtype(Hashtbl.find builtins t.lt_name)
  with Not_found -> Atype t

let builtin_type ~name ~link ~library =
  let m = new_extern ~link ~library ~debug:name in
  Hashtbl.add builtins name m

let datatype ~library name =
  let m = new_extern ~link:(infoprover name) ~library ~debug:name in
  Mtype m

let record ~link ~library fts =
  let m = new_extern ~link ~library ~debug:link.altergo in
  let r = { fields = [] } in
  let fs = List.map (fun (f,t) -> Mfield(m,r,f,t)) fts in
  r.fields <- fs ; Mrecord(m,r)

let field t f =
  match t with
    | Mrecord(_,r) ->
	begin
	  try List.find (function Mfield(_,_,g,_) -> f = g | _ -> false) r.fields
	  with Not_found -> Wp_parameters.fatal "No field <%s> in record" f
	end
    | _ -> Wp_parameters.fatal "No field <%s> in type '%a'" f ADT.pretty t

let comp c = Comp c

let fields_of_tau = function
  | Record _ -> assert false
  | Data(Mrecord(_,r),_) -> r.fields
  | Data(Comp c,_) -> List.map (fun f -> Cfield f) c.cfields
  | _ -> []

let fields_of_field = function
  | Mfield(_,r,_,_) -> r.fields
  | Cfield f -> List.map (fun f -> Cfield f) f.fcomp.cfields

let tau_of_field = function
  | Mfield(_,_,_,t) -> t
  | Cfield f -> tau_of_ctype f.ftype

let tau_of_record = function
  | Mfield(mdt,fs,_,_) -> Logic.Data(Mrecord(mdt,fs),[])
  | Cfield f -> tau_of_comp f.fcomp

module Field =
struct

  type t = field

  let debug = function
    | Mfield(_,_,f,_) -> f
    | Cfield f -> field_id f

  let hash = function
    | Mfield(_,_,f,_) -> Hashtbl.hash f
    | Cfield f -> Fieldinfo.hash f

  let compare f g = 
    if f==g then 0 else
      match f , g with
	| Mfield(_,_,f,_) , Mfield(_,_,g,_) -> String.compare f g
	| Mfield _ , Cfield _ -> (-1)
	| Cfield _ , Mfield _ -> 1
	| Cfield f , Cfield g -> Fieldinfo.compare f g
	    
  let equal f g = (compare f g = 0)

  let pretty fmt f = Format.pp_print_string fmt (debug f)

  let sort = function
    | Mfield(_,_,_,s) -> Qed.Kind.of_tau s
    | Cfield f -> sort_of_object (Ctypes.object_of f.ftype)

end

(* -------------------------------------------------------------------------- *)
(* --- Functions & Predicates                                             --- *)
(* -------------------------------------------------------------------------- *)

type lfun =
  | ACSL of Cil_types.logic_info (** Registered in Definition.t,
                                     only  *)
  | CTOR of Cil_types.logic_ctor_info (** Not registered in Definition.t
                                          directly converted/printed *)
  | Model of model (** *)

and model = {
  m_category : lfun category ;
  m_params : sort list ;
  m_resort : sort ;
  m_result : tau option ;
  m_source : source ;
}

and source =
  | Generated of string
  | Extern of Engine.link extern

let tau_of_lfun = function
  | ACSL f -> tau_of_return f
  | CTOR c -> 
      if c.ctor_type.lt_params = [] then Logic.Data(Atype c.ctor_type,[])
      else raise Not_found
  | Model { m_result = Some t } -> t
  | Model m -> match m.m_resort with
      | Sint -> Int
      | Sreal -> Real
      | Sbool -> Bool
      | _ -> raise Not_found

type balance = Nary | Left | Right

type 'a linkinfo = {
  mutable thdep : string list;
  mutable link : 'a
}


let symbolf
    ?library
    ?link
    ?(balance=Nary) (** specify a default for link *)
    ?(category=Logic.Function)
    ?(params=[])
    ?(sort=Logic.Sdata)
    ?(result:tau option)
    name =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       let name = Buffer.contents buffer in
       let source = match library with
         | None -> assert (link = None); Generated name
         | Some th ->
           let conv n = function
             | Nary  -> Engine.F_call n
             | Left  -> Engine.F_left n
             | Right -> Engine.F_right n
           in
           let link = match link with
             | None -> infoprover (conv name balance)
             | Some info -> info
           in
           Extern (new_extern ~library:th ~link ~debug:name) in
       let resort,result = match sort,result with
	 | _,Some t -> Kind.of_tau t,result
	 | Sint,None -> sort,Some Int
	 | Sreal,None -> sort,Some Real
	 | Sbool,None -> sort,Some Bool
	 | Sprop,None -> sort,Some Prop
	 | _ -> sort,None in
       Model {
	 m_category = category ;
	 m_params = params ;
	 m_result = result ;
	 m_resort = resort ;
         m_source = source;
       }
    ) (Format.formatter_of_buffer buffer) name

let extern_s ~library ?link ?category ?params ?sort ?result name =
  symbolf ~library ?category ?params ?sort ?result ?link "%s" name

let extern_f ~library ?link ?balance ?category ?params ?sort ?result name =
  symbolf ~library ?category ?params ?link ?balance ?sort ?result name

let extern_p ~library ?bool ?prop ?link ?(params=[]) () =
  let link = 
    match bool,prop,link with
      | Some b , Some p , None -> infoprover (Engine.F_bool_prop(b,p))
      | _ , _ , Some info -> info 
      | _ , _ , _ -> assert false 
  in
  let debug = Export.debug link.altergo in
  Model {
    m_category = Logic.Function;
    m_params = params ;
    m_resort = Logic.Sprop;
    m_result = Some Logic.Prop;
    m_source = Extern (new_extern ~library ~link ~debug)
  }

let extern_fp ~library ?(params=[]) ?link phi =
  let link = match link with
    | None -> infoprover (Engine.F_call phi)
    | Some link -> map_infoprover (fun phi -> Engine.F_call(phi)) link in
  Model {
    m_category = Logic.Function ;
    m_params = params ;
    m_resort = Logic.Sprop;
    m_result = Some Logic.Prop;
    m_source = Extern (new_extern
                         ~library
                         ~link
                         ~debug:phi)
  }

let generated_f ?category ?params ?sort ?result name = 
  symbolf ?category ?params ?sort ?result name

let generated_p name = 
  Model {
    m_category = Logic.Function ;
    m_params = [] ;
    m_resort = Logic.Sprop;
    m_result = Some Logic.Prop;
    m_source = Generated name
  }

let constructor ct = CTOR ct
let logic_info lf = ACSL lf

module Fun =
struct

  type t = lfun

  let debug = function
    | ACSL f -> logic_id f
    | CTOR c -> ctor_id c
    | Model({m_source=Generated n}) -> n
    | Model({m_source=Extern e})    -> e.ext_debug

  let hash = function
    | ACSL f -> Logic_info.hash f
    | CTOR c -> Logic_ctor_info.hash c
    | Model({m_source=Generated n}) -> Datatype.String.hash n
    | Model({m_source=Extern e})    -> e.ext_id

  let compare f g =
    if f==g then 0 else
      match f , g with
        | Model({m_source=Generated f}), Model({m_source=Generated g})
          -> String.compare f g
        | Model({m_source=Generated _}), _ -> (-1)
        | _, Model({m_source=Generated _}) -> 1
        | Model({m_source=Extern f}), Model({m_source=Extern g})
          -> ext_compare f g
        | Model({m_source=Extern _}), _ -> (-1)
        | _, Model({m_source=Extern _}) -> 1
	| ACSL f , ACSL g -> Logic_info.compare f g
	| ACSL _ , _ -> (-1)
	| _ , ACSL _ -> 1
	| CTOR c , CTOR d -> Logic_ctor_info.compare c d

  let equal f g = (compare f g = 0)

  let pretty fmt f = Format.pp_print_string fmt (debug f)

  let category = function
    | Model m -> m.m_category
    | ACSL _ -> Logic.Function
    | CTOR _ -> Logic.Constructor

  let sort = function
    | Model m -> m.m_resort
    | ACSL { l_type=None } -> Logic.Sprop
    | ACSL { l_type=Some t } -> sort_of_ltype t
    | CTOR _ -> Logic.Sdata

  let params = function
    | Model m -> m.m_params
    | ACSL lt ->
	if lt.l_labels=[] then
	  List.map (fun x -> sort_of_ltype x.lv_type) lt.l_profile
	else []
    | CTOR ct -> List.map sort_of_ltype ct.ctor_params

end

class virtual idprinting =
  object(self)
    method virtual basename  : string -> string
    method virtual infoprover: 'a. 'a infoprover -> 'a

    method datatypename  = self#basename
    method fieldname     = self#basename
    method funname       = self#basename

    method datatype = function
      | Mtype a -> self#infoprover a.ext_link
      | Mrecord(a,_) -> self#infoprover a.ext_link
      | Comp c -> self#datatypename (comp_id c)
      | Atype lt -> self#datatypename (type_id lt)
    method field = function
      | Mfield(_,_,f,_) -> self#fieldname f
      | Cfield f -> self#fieldname (field_id f)
    method link = function
    | ACSL f -> Engine.F_call (self#funname (logic_id f))
    | CTOR c -> Engine.F_call (self#funname (ctor_id c))
    | Model({m_source=Generated n}) -> Engine.F_call (self#funname n)
    | Model({m_source=Extern e})    -> self#infoprover e.ext_link
  end

(* -------------------------------------------------------------------------- *)
(* --- Terms                                                              --- *)
(* -------------------------------------------------------------------------- *)

module F = 
struct

  module ZInteger = 
  struct
    include Integer
    let pretty = Integer.pretty ~hexa:false
    let leq = Integer.le
  end

  module T = Qed.Term.Make(ZInteger)(ADT)(Field)(Fun)

  (* -------------------------------------------------------------------------- *)
  (* --- Qed Projectified State                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module DATA = 
    Datatype.Make
      (struct
        type t = T.state
        let name = "Wp.Qed"
        let rehash = Datatype.identity
        let structural_descr = Structural_descr.t_unknown
        let reprs = [T.get_state ()]
        let equal = Datatype.undefined
        let compare = Datatype.undefined
        let hash = Datatype.undefined
        let copy _old = T.create ()
        let varname = Datatype.undefined
        let pretty = Datatype.undefined
        let internal_pretty_code = Datatype.undefined 
        let mem_project _ _ = false
      end)
      
  module STATE =
    State_builder.Register(DATA)
      (struct
        type t = T.state
        let create = T.create
        let clear = T.clr_state
        let get = T.get_state 
        let set = T.set_state
        let clear_some_projects _ _ = false
      end)
      (struct
        let name = "Wp.Qed"
        let dependencies = []
        let unique_name = name
      end)

  (* -------------------------------------------------------------------------- *)
  (* --- Term API                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  module Pretty = Qed.Pretty.Make(T)
  include T

  (* -------------------------------------------------------------------------- *)
  (* --- Term Checking                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let do_checks = ref false
  let iter_checks f = T.iter_checks 
    (fun ~qed ~raw -> f ~qed ~raw ~goal:(T.check_unit ~qed ~raw))

  let e_add a b = 
    let r = T.e_add a b in
    if !do_checks then T.check (Add[a;b]) r else r
  let e_sum xs = 
    let r = T.e_sum xs in
    if !do_checks then T.check (Add xs) r else r
  let e_times k x = 
    let r = T.e_times k x in
    if !do_checks then T.check (Times(k,x)) r else r
  let e_opp x =
    let r = T.e_opp x in
    if !do_checks then T.check (Times(Z.minus_one,x)) r else r
  let e_leq a b =
    let r = T.e_leq a b in
    if !do_checks then T.check (Leq(a,b)) r else r
  let e_lt a b =
    let r = T.e_lt a b in
    if !do_checks then T.check (Lt(a,b)) r else r
  let e_eq a b =
    let r = T.e_eq a b in
    if !do_checks then T.check (Eq(a,b)) r else r
  let e_neq a b =
    let r = T.e_neq a b in
    if !do_checks then T.check (Neq(a,b)) r else r
  let e_fun f xs =
    let r = T.e_fun f xs in
    if !do_checks then T.check (Fun(f,xs)) r else r

  (* -------------------------------------------------------------------------- *)
  (* --- Term Extensions                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  type unop = term -> term
  type binop = term -> term -> term

  let e_zero = T.constant (e_zint Z.zero)
  let e_one  = T.constant (e_zint Z.one)
  let e_minus_one = T.constant (e_zint Z.minus_one)
  let e_one_real  = T.constant (e_real (R.of_string "1.0"))
  let e_zero_real = T.constant (e_real (R.of_string "0.0"))

  let hex_of_float f = 
    Pretty_utils.to_string (Floating_point.pretty_normal ~use_hex:true) f
    
  let e_int64 z = e_zint (Z.of_string (Int64.to_string z))
  let e_fact k e = e_times (Z.of_int k) e
  let e_bigint z = e_zint (Z.of_string (Integer.to_string z))
  let e_range a b = e_sum [b;e_one;e_opp a]
  let e_mthfloat f = T.e_real (R.of_string (string_of_float f))
  let e_hexfloat f = T.e_real (R.of_string (hex_of_float f))

  let e_setfield r f v =
    (*TODO:NUPW: check for UNIONS *)
    let r = List.map 
      (fun g -> g,if Field.equal f g then v else e_getfield r g)
      (fields_of_field f)
    in e_record r

  (* -------------------------------------------------------------------------- *)
  (* --- Predicates                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  type pred = term
  type cmp = term -> term -> pred

  let p_bool t = t
  let e_prop t = t
  let p_bools xs = xs
  let e_props xs = xs
  let lift f x = f x

  let is_zero e = match T.repr e with
    | Kint z -> Integer.equal z Integer.zero
    | _ -> false

  let eqp = equal
  let comparep = compare

  let is_ptrue = is_true
  let is_pfalse = is_false
  let is_equal a b = is_true (e_eq a b)

  let p_equal = e_eq
  let p_neq = e_neq
  let p_leq = e_leq
  let p_lt = e_lt

  let p_positive e = e_leq e_zero e

  let p_true = e_true
  let p_false = e_false

  let p_not = e_not
  let p_bind = e_bind
  let p_forall = e_forall
  let p_exists = e_exists
  let p_subst = e_subst

  let p_and p q = e_and [p;q]
  let p_or p q = e_or [p;q]
  let p_imply h p = e_imply [h] p
  let p_hyps hs p = e_imply hs p
  let p_equiv = e_equiv
  let p_if = e_if

  let p_conj = e_and
  let p_disj = e_or

  let p_all f xs = e_and (List.map f xs)
  let p_any f xs = e_or (List.map f xs)

  let p_call = e_fun
  let p_close p = p_forall (Vars.elements (vars p)) p

  let occurs x t = Vars.mem x (vars t)
  let intersect a b = Vars.intersect (vars a) (vars b)
  let occursp = occurs
  let intersectp = intersect
  let varsp = vars
  let pred = repr
  let idp = id

  let head e = match T.repr e with
    | Kint _ -> "int"
    | Kreal _ -> "real"
    | True -> "true"
    | False -> "false"
    | Var _ -> "var"
    | Add _ -> "add"
    | Mul _ -> "mul"
    | Times _ -> "times"
    | Div _ -> "div"
    | Mod _ -> "mod"
    | Eq _ -> "eq"
    | Neq _ -> "neq"
    | Not _ -> "not"
    | Lt _ -> "lt"
    | Leq _ -> "leq"
    | And _ -> "and"
    | Or _ -> "or"
    | Imply _ -> "imply"
    | If _ -> "if"
    | Fun _ -> "fun"
    | Aget _ -> "access"
    | Aset _ -> "update"
    | Rget _ -> "getfield"
    | Rdef _ -> "record"
    | Bind(Forall,_,_) -> "forall"
    | Bind(Exists,_,_) -> "exists"
    | Bind(Lambda,_,_) -> "lambda"
    | Apply _ -> "apply"

      
  let pp_term fmt e = 
    if Wp_parameters.has_dkey "pretty"
    then T.debug fmt e
    else Pretty.pp_term Pretty.empty fmt e 
  let pp_pred fmt p = 
    if Wp_parameters.has_dkey "pretty"
    then T.debug fmt p
    else Pretty.pp_term Pretty.empty fmt p
  let pp_var fmt x = pp_term fmt (e_var x)
  let pp_vars fmt xs =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      Vars.iter (fun x -> Format.fprintf fmt "@ %a" pp_var x) xs ;
      Format.fprintf fmt " }@]" ;
    end
      
  let debugp = T.debug

  type env = Pretty.env
  let empty = Pretty.empty
  let closed = Pretty.closed
  let marker = Pretty.marks
  let mark_e = T.mark
  let mark_p = T.mark
  let define f env m =
    List.fold_left
      (fun env t ->
	 let x,env_x = Pretty.fresh env t in
	 f env x t ; env_x)
      env (T.defs m)

  let pp_eterm = Pretty.pp_term
  let pp_epred = Pretty.pp_term

  module Pmap = Tmap
  module Pset = Tset
      
  let set_builtin_1 f r = 
    set_builtin f (function [e] -> r e | _ -> raise Not_found)

  let set_builtin_2 f r = 
    set_builtin f (function [a;b] -> r a b | _ -> raise Not_found)
      
  let set_builtin_eqp = set_builtin_eq

end

open F
  
(* -------------------------------------------------------------------------- *)
(* --- Fresh Variables & Local Assumptions                                --- *)
(* -------------------------------------------------------------------------- *)
  
type gamma = { 
  mutable hyps : pred list ; 
  mutable vars : var list ;
}

(* -------------------------------------------------------------------------- *)
    
let cpool = Context.create "Lang.pool"
let cgamma = Context.create "Lang.gamma"
let apool = function None -> F.pool () | Some p -> p
let agamma = function None -> { hyps=[] ; vars=[] } | Some g -> g

let new_pool = F.pool
let new_gamma ?copy () =
  match copy with
    | None -> { hyps=[] ; vars=[] }
    | Some g -> { hyps = g.hyps ; vars = g.vars }

let get_pool () = Context.get cpool
let get_gamma () = Context.get cgamma
    
let freshvar ?basename tau = F.fresh (Context.get cpool) ?basename tau
let freshen x = F.alpha (Context.get cpool) x
  
let local ?pool ?gamma f =
  Context.bind cpool (apool pool)
    (Context.bind cgamma (agamma gamma) f)
  
(* -------------------------------------------------------------------------- *)
(* --- Hypotheses                                                         --- *)
(* -------------------------------------------------------------------------- *)

let masked = ref false

let without_assume job x =
  if !masked 
  then job x
  else
    try masked := true ; let y = job x in masked := false ; y
    with err -> masked := false ; raise err

let assume p =
  if p != p_true && not !masked then
    let d = Context.get cgamma in
    d.hyps <- p :: d.hyps
    
let epsilon ?basename t phi =
  let d = Context.get cgamma in
  let x = freshvar ?basename t in
  let e = e_var x in 
  d.hyps <- phi e :: d.hyps ; 
  d.vars <- x :: d.vars ; 
  e

let hypotheses g = g.hyps
let variables g = List.rev g.vars

let get_hypotheses () = (Context.get cgamma).hyps
let get_variables () = (Context.get cgamma).vars

(* -------------------------------------------------------------------------- *)
(* --- Alpha Conversion                                                   --- *)
(* -------------------------------------------------------------------------- *)

module Alpha =
struct

  module Vmap = FCMap.Make(Var)

  type t = {
    mutable vars : var Vmap.t ;
    mutable cache : term Tmap.t ;
  }
      
  let create () = { vars = Vmap.empty ; cache = Tmap.empty }

  let get w x =
    try Vmap.find x w.vars
    with Not_found -> 
      let y = freshen x in
      w.vars <- Vmap.add x y w.vars ; y
	
  let iter f w = Vmap.iter f w.vars

  let rec convert w e =
    try Tmap.find e w.cache
    with Not_found ->
      let a =
	match F.repr e with
	  | Logic.Var x -> e_var (get w x)
	  | Logic.Bind(q,x,t) ->
	      let v_temp = w.vars in
	      let c_temp = w.cache in
	      let y = freshen x in
	      w.vars <- Vmap.add x y w.vars ;
	      let b = convert w t in
	      w.vars <- 
		if Vmap.mem x v_temp then
		  Vmap.add x (Vmap.find x v_temp) w.vars
		else
		  Vmap.remove x w.vars ;
	      w.cache <- c_temp ;
	      F.e_bind q y b
	  | _ -> e_map (convert w) e
      in w.cache <- Tmap.add e a w.cache ; a

  let convertp = convert
	
end

(* -------------------------------------------------------------------------- *)
