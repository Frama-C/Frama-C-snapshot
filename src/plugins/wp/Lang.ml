(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

let dkey_pretty = Wp_parameters.register_category "pretty"

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
    let s = Bytes.of_string s in
    Bytes.set s 0 '_'; Bytes.to_string s
  else s

let comp_id c =
  let prefix = if c.cstruct then 'S' else 'U' in
  if c.corig_name = "" then
    Printf.sprintf "%c%d" prefix c.ckey
  else
    Printf.sprintf "%c%d_%s" prefix c.ckey c.corig_name

let field_id f =
  let c = f.fcomp in
  if c.corig_name = "" then
    Printf.sprintf "F%d_%s" c.ckey f.fname
  else
    Printf.sprintf "F%d_%s_%s" c.ckey c.corig_name f.fname

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
let floats = Context.create "Lang.floats"

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

let sort_of_ltype t = match Logic_utils.unroll_type ~unroll_typedef:false t with
  | Ctype typ -> sort_of_ctype typ
  | Ltype _ | Lvar _ | Larrow _ -> Logic.Sdata
  | Linteger -> Logic.Sint
  | Lreal -> Logic.Sreal

let tau_of_comp c = Logic.Data(Comp c,[])

let t_int = Logic.Int
let t_bool = Logic.Bool
let t_real = Logic.Real
let t_prop = Logic.Prop
let t_addr () = Context.get pointer Cil.voidType
let t_array a = Logic.Array(Logic.Int,a)
let t_farray a b = Logic.Array(a,b)
let t_datatype adt ts = Logic.Data(adt,ts)

let rec tau_of_object = function
  | C_int _ -> Logic.Int
  | C_float f -> Context.get floats f
  | C_pointer t -> Context.get pointer t
  | C_comp c -> tau_of_comp c
  | C_array { arr_element = typ } -> t_array (tau_of_ctype typ)

and tau_of_ctype typ = tau_of_object (Ctypes.object_of typ)

let poly = Context.create "Wp.Lang.poly"

let rec varpoly k x = function
  | [] -> Warning.error "Unbound type parameter <%s>" x
  | y::ys -> if x = y then k else varpoly (succ k) x ys

let builtins = Hashtbl.create 131

let rec tau_of_ltype t = match Logic_utils.unroll_type ~unroll_typedef:false t with
  | Linteger -> Logic.Int
  | Lreal -> Logic.Real
  | Ctype typ -> tau_of_ctype typ
  | Lvar x -> Logic.Tvar (varpoly 1 x (Context.get poly))
  | Larrow _ ->
      Warning.error "array type non-supported(%a)"
        Printer.pp_logic_type t
  | Ltype _ as b when Logic_const.is_boolean_type b -> Logic.Bool
  | Ltype(lt,ps) ->
      let tau =
        (*TODO: check arity *)
        try Mtype(Hashtbl.find builtins lt.lt_name)
        with Not_found -> Atype lt
      in Logic.Data(tau,List.map tau_of_ltype ps)

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
    | Mtype a | Mrecord(a,_) -> FCHashtbl.hash a
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

let atype lt =
  try Mtype(Hashtbl.find builtins lt.lt_name)
  with Not_found -> Atype lt

let get_builtin_type ~name ~link ~library =
  try Mtype (Hashtbl.find builtins name)
  with Not_found ->
    let m = new_extern ~link ~library ~debug:name in
    Hashtbl.add builtins name m ; Mtype m

let set_builtin_type ~name ~link ~library =
  let m = new_extern ~link ~library ~debug:name in
  Hashtbl.add builtins name m

let mem_builtin_type ~name =
  Hashtbl.mem builtins name

let is_builtin lt = Hashtbl.mem builtins lt.lt_name

let is_builtin_type ~name = function
  | Data(Mtype m,_) ->
      begin
        try m == Hashtbl.find builtins name
        with Not_found -> false
      end
  | _ -> false

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

let fields_of_adt = function
  | Mrecord(_,r) -> r.fields
  | Comp c -> List.map (fun f -> Cfield f) c.cfields
  | _ -> []

let fields_of_tau = function
  | Record fts -> List.map fst fts
  | Data(adt,_) -> fields_of_adt adt
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
    | Mfield(_,_,f,_) -> FCHashtbl.hash f
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
  | ACSL of Cil_types.logic_info
  (** Registered in Definition.t, only  *)
  | CTOR of Cil_types.logic_ctor_info
  (** Not registered in Definition.t, directly converted/printed *)
  | Model of model
  (** Generated or External function *)

and model = {
  m_category : lfun category ;
  m_params : sort list ;
  m_result : sort ;
  m_typeof : tau option list -> tau ;
  m_source : source ;
}

and source =
  | Generated of WpContext.context option * string
  | Extern of Engine.link extern

let tau_of_lfun phi ts =
  match phi with
  | ACSL f -> tau_of_return f
  | CTOR c ->
      if c.ctor_type.lt_params = [] then Logic.Data(Atype c.ctor_type,[])
      else raise Not_found
  | Model m -> match m.m_result with
    | Sint -> Int
    | Sreal -> Real
    | Sbool -> Bool
    | _ -> m.m_typeof ts

type balance = Nary | Left | Right

let not_found _ = raise Not_found

let generated ?(context=false) name =
  let ctxt = if context
    then Some (WpContext.get_context ())
    else None in
  Generated(ctxt,name)

let symbolf
    ?library
    ?context
    ?link
    ?(balance=Nary) (** specify a default for link *)
    ?(category=Logic.Function)
    ?(params=[])
    ?(sort=Logic.Sdata)
    ?(result:tau option)
    ?(typecheck:(tau option list -> tau) option)
    name =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       let name = Buffer.contents buffer in
       let source = match library with
         | None ->
             assert (link = None);
             generated ?context name
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
       let typeof =
         match typecheck with Some phi -> phi | None ->
         match result with Some t -> fun _ -> t | None -> not_found in
       let result =
         match result with Some t -> Kind.of_tau t | None -> sort in
       Model {
         m_category = category ;
         m_params = params ;
         m_result = result ;
         m_typeof = typeof ;
         m_source = source ;
       }
    ) (Format.formatter_of_buffer buffer) name

let extern_s
    ~library ?link ?category ?params ?sort ?result ?typecheck name =
  symbolf
    ~library ?category ?params ?sort ?result ?typecheck ?link "%s" name

let extern_f
    ~library ?link ?balance ?category ?params ?sort ?result ?typecheck name =
  symbolf
    ~library ?category ?params ?link ?balance ?sort ?result ?typecheck name

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
    m_result = Logic.Sprop;
    m_typeof = not_found;
    m_source = Extern (new_extern ~library ~link ~debug)
  }

let extern_fp ~library ?(params=[]) ?link phi =
  let link = match link with
    | None -> infoprover (Engine.F_call phi)
    | Some link -> map_infoprover (fun phi -> Engine.F_call(phi)) link in
  Model {
    m_category = Logic.Function ;
    m_params = params ;
    m_result = Logic.Sprop;
    m_typeof = not_found;
    m_source = Extern (new_extern
                         ~library
                         ~link
                         ~debug:phi)
  }

let generated_f ?context ?category ?params ?sort ?result name =
  symbolf ?context ?category ?params ?sort ?result name

let generated_p ?context name =
  Model {
    m_category = Logic.Function ;
    m_params = [] ;
    m_result = Logic.Sprop;
    m_typeof = not_found;
    m_source = generated ?context name
  }

module Fun =
struct

  type t = lfun

  let debug = function
    | ACSL f -> logic_id f
    | CTOR c -> ctor_id c
    | Model({m_source=Generated(_,n)}) -> n
    | Model({m_source=Extern e})    -> e.ext_debug

  let hash = function
    | ACSL f -> Logic_info.hash f
    | CTOR c -> Logic_ctor_info.hash c
    | Model({m_source=Generated(_,n)}) -> Datatype.String.hash n
    | Model({m_source=Extern e})    -> e.ext_id

  let compare_context c1 c2 =
    match c1 , c2 with
    | None , None -> 0
    | None , _ -> (-1)
    | _ , None -> 1
    | Some c1 , Some c2 -> WpContext.S.compare c1 c2

  let compare_source s1 s2 =
    match s1 , s2 with
    | Generated(m1,f1), Generated(m2,f2) ->
        let cmp = String.compare f1 f2 in
        if cmp<>0 then cmp else compare_context m1 m2
    | Extern f , Extern g ->
        ext_compare f g
    | Generated _ , Extern _ -> (-1)
    | Extern _ , Generated _ -> 1

  let compare f g =
    if f==g then 0 else
      match f , g with
      | Model {m_source=mf} , Model {m_source=mg} -> compare_source mf mg
      | Model _ , _ -> (-1)
      | _ , Model _ -> 1
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
    | Model m -> m.m_result
    | ACSL { l_type=None } -> Logic.Sprop
    | ACSL { l_type=Some t } -> sort_of_ltype t
    | CTOR _ -> Logic.Sdata

  let parameters = ref (fun _ -> [])

  let params = function
    | Model m -> m.m_params
    | CTOR ct -> List.map sort_of_ltype ct.ctor_params
    | (ACSL _) as f -> !parameters f

end

let parameters phi = Fun.parameters := phi

class virtual idprinting =
  object(self)
    method virtual infoprover: 'a. 'a infoprover -> 'a
    method virtual sanitize : string -> string

    method sanitize_type  = self#sanitize
    method sanitize_field = self#sanitize
    method sanitize_fun   = self#sanitize

    method datatype = function
      | Mtype a -> self#infoprover a.ext_link
      | Mrecord(a,_) -> self#infoprover a.ext_link
      | Comp c -> self#sanitize_type (comp_id c)
      | Atype lt -> self#sanitize_type (type_id lt)
    method field = function
      | Mfield(_,_,f,_) -> self#sanitize_field f
      | Cfield f -> self#sanitize_field (field_id f)
    method link = function
      | ACSL f -> Engine.F_call (self#sanitize_fun (logic_id f))
      | CTOR c -> Engine.F_call (self#sanitize_fun (ctor_id c))
      | Model({m_source=Generated(_,n)}) -> Engine.F_call (self#sanitize_fun n)
      | Model({m_source=Extern e}) -> self#infoprover e.ext_link
  end

let name_of_lfun = function
  | ACSL f -> logic_id f
  | CTOR c -> ctor_id c
  | Model({m_source=Generated(_,f)}) -> f
  | Model({m_source=Extern e}) -> e.ext_debug

let name_of_field = function
  | Mfield(_,_,f,_) -> f
  | Cfield f -> field_id f

(* -------------------------------------------------------------------------- *)
(* --- Terms                                                              --- *)
(* -------------------------------------------------------------------------- *)

module F =
struct

  module QZERO = Qed.Term.Make(ADT)(Field)(Fun)

  (* -------------------------------------------------------------------------- *)
  (* --- Qed Projectified State                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module DATA =
    Datatype.Make
      (struct
        type t = QZERO.state
        let name = "Wp.Qed"
        let rehash = Datatype.identity
        let structural_descr = Structural_descr.t_unknown
        let reprs = [QZERO.get_state ()]
        let equal = Datatype.undefined
        let compare = Datatype.undefined
        let hash = Datatype.undefined
        let copy _old = QZERO.create ()
        let varname = Datatype.undefined
        let pretty = Datatype.undefined
        let internal_pretty_code = Datatype.undefined
        let mem_project _ _ = false
      end)

  module STATE = State_builder.Register(DATA)
      (struct
        type t = QZERO.state
        let create = QZERO.create
        let clear = QZERO.clr_state
        let get = QZERO.get_state
        let set = QZERO.set_state
        let clear_some_projects _ _ = false
      end)
      (struct
        let name = "Wp.Qed"
        let dependencies = [Ast.self]
        let unique_name = name
      end)
  include (STATE : sig end) (* For OCaml-4.0 *)

  (* -------------------------------------------------------------------------- *)
  (* --- Term API                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  module Pretty = Qed.Pretty.Make(QZERO)
  module QED =
  struct
    include QZERO
    let typeof ?(field=tau_of_field) ?(record=tau_of_record) ?(call=tau_of_lfun) e =
      QZERO.typeof ~field ~record ~call e
  end
  include QED

  (* -------------------------------------------------------------------------- *)
  (* --- Term Extensions                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  type unop = term -> term
  type binop = term -> term -> term

  let e_zero = QED.constant (e_zint Z.zero)
  let e_one  = QED.constant (e_zint Z.one)
  let e_minus_one = QED.constant (e_zint Z.minus_one)
  let e_minus_one_real  = QED.constant (e_real Q.minus_one)
  let e_one_real  = QED.constant (e_real Q.one)
  let e_zero_real = QED.constant (e_real Q.zero)

  let e_int64 z = e_zint (Z.of_string (Int64.to_string z))
  let e_fact k e = e_times (Z.of_int k) e
  let e_bigint z = e_zint (Z.of_string (Integer.to_string z))
  let e_range a b = e_sum [b;e_one;e_opp a]

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
  type operator = pred -> pred -> pred

  let p_bool t = t
  let e_prop t = t
  let p_bools xs = xs
  let e_props xs = xs
  let lift f x = f x

  let is_zero e = match QED.repr e with
    | Kint z -> Integer.equal z Integer.zero
    | _ -> false

  let eqp = equal
  let comparep = compare

  let is_ptrue = is_true
  let is_pfalse = is_false
  let is_equal a b = is_true (e_eq a b)

  let p_equal = e_eq
  let p_equals = List.map (fun (x,y) -> p_equal x y)
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
  let p_subst_var = e_subst_var

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

  let e_vars e = List.sort Var.compare (Vars.elements (vars e))
  let p_vars = e_vars
  let p_call = e_fun ~result:Prop
  let p_close p = p_forall (p_vars p) p

  let occurs x t = Vars.mem x (vars t)
  let intersect a b = Vars.intersect (vars a) (vars b)
  let occursp = occurs
  let intersectp = intersect
  let varsp = vars
  let p_expr = repr
  let e_expr = repr

  let pp_tau = Pretty.pp_tau
  let context_pp = Context.create "Lang.F.pp"
  let pp_term fmt e =
    if Wp_parameters.has_dkey dkey_pretty
    then QED.debug fmt e
    else
      match Context.get_opt context_pp with
      | Some env -> Pretty.pp_term_env env  fmt e
      | None ->
          let env = Pretty.known Pretty.empty (QED.vars e) in
          Pretty.pp_term env fmt e
  let pp_pred = pp_term
  let pp_var fmt x = pp_term fmt (e_var x)
  let pp_vars fmt xs =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      Vars.iter (fun x -> Format.fprintf fmt "@ %a" pp_var x) xs ;
      Format.fprintf fmt " }@]" ;
    end

  let debugp = QED.debug

  type env = Pretty.env
  let env xs = Pretty.known Pretty.empty xs
  let marker = Pretty.marks
  let mark_e = QED.mark
  let mark_p = QED.mark
  let define f env m =
    List.fold_left
      (fun env t ->
         let x,env_x = Pretty.fresh env t in
         f env x t ; env_x)
      env (QED.defs m)

  let pp_eterm = Pretty.pp_term
  let pp_epred = Pretty.pp_term

  module Pmap = Tmap
  module Pset = Tset

  let set_builtin_1 f r =
    set_builtin f (function [e] -> r e | _ -> raise Not_found)

  let set_builtin_2 f r =
    set_builtin f (function [a;b] -> r a b | _ -> raise Not_found)

  let set_builtin_2' f r =
    set_builtin' f (function [a;b] -> r a b | _ -> raise Not_found)

  let set_builtin_eqp = set_builtin_eq

end

open F

module N = struct

  let ( + ) = e_add
  let ( ~- ) x = e_sub e_zero x
  let ( - ) = e_sub
  let ( * ) = e_mul
  let ( / ) = e_div
  let ( mod ) = e_mod

  let ( = ) = p_equal
  let ( < ) = p_lt
  let ( > ) x y = p_lt y x
  let ( <= ) = p_leq
  let ( >= ) x y = p_leq y x
  let ( <> ) = p_neq

  let ( && ) = p_and
  let ( || ) = p_or
  let not = p_not

  let ( $ ) = e_fun
  let ( $$ ) = p_call

end


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
let add_vars pool = function
  | None -> ()
  | Some xs -> F.add_vars pool xs

let new_pool ?copy ?(vars = Vars.empty) () =
  let pool = F.pool ?copy () in
  F.add_vars pool vars ; pool
let new_gamma ?copy () =
  match copy with
  | None -> { hyps=[] ; vars=[] }
  | Some g -> { hyps = g.hyps ; vars = g.vars }

let get_pool () = Context.get cpool
let get_gamma () = Context.get cgamma
let has_gamma () = Context.defined cgamma

let freshvar ?basename tau = F.fresh (Context.get cpool) ?basename tau
let freshen x = F.alpha (Context.get cpool) x

let local ?pool ?vars ?gamma f =
  let pool = match pool with None -> F.pool () | Some p -> p in
  add_vars pool vars ;
  let gamma = match gamma with None -> { hyps=[] ; vars=[] } | Some g -> g in
  Context.bind cpool pool (Context.bind cgamma gamma f)

let sigma () = F.sigma ~pool:(Context.get cpool) ()

let alpha () =
  let sigma = sigma () in
  let alpha = ref Tmap.empty in
  let lookup e x =
    try Tmap.find e !alpha with Not_found ->
      let y = F.Subst.fresh sigma (F.tau_of_var x) in
      let ey = e_var y in alpha := Tmap.add e ey !alpha; ey in
  let compute e =
    match F.repr e with
    | Fvar x -> lookup e x
    | _ -> raise Not_found in
  F.Subst.add_fun sigma compute ; sigma

let subst xs vs =
  let bind w x v = Tmap.add (e_var x) v w in
  let vmap =
    try List.fold_left2 bind Tmap.empty xs vs
    with _ -> raise (Invalid_argument "Wp.Lang.Subst.sigma")
  in
  let sigma = sigma () in
  F.Subst.add_map sigma vmap ; sigma

let e_subst f =
  let sigma = sigma () in
  F.Subst.add_fun sigma f ; F.e_subst sigma

let p_subst f =
  let sigma = sigma () in
  F.Subst.add_fun sigma f ; F.p_subst sigma

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

(** For why3_api but circular dependency *)

module For_export = struct

  type specific_equality = {
    for_tau:(tau -> bool);
    mk_new_eq:F.binop;
  }

  (** delay the create at most as possible (due to constants handling in qed) *)
  let state = ref None

  let init = ref (fun () -> ())

  let add_init f =
    let old = !init in
    init := (fun () -> old (); f ())

  let get_state () =
    match !state with
    | None ->
        let st = QZERO.create () in
        QZERO.in_state st !init ();
        state := Some st;
        st
    | Some st -> st

  let rebuild ?cache t = QZERO.rebuild_in_state (get_state ()) ?cache t

  let set_builtin f c =
    add_init (fun () -> QZERO.set_builtin f c)

  let set_builtin' f c =
    add_init (fun () -> QZERO.set_builtin' f c)
  let set_builtin_eq f c =
    add_init (fun () -> QZERO.set_builtin_eq f c)
  let set_builtin_leq f c =
    add_init (fun () -> QZERO.set_builtin_leq f c)

  let in_state f v = QZERO.in_state (get_state ()) f v

end

(* -------------------------------------------------------------------------- *)
(* --- Simplifier                                                         --- *)
(* -------------------------------------------------------------------------- *)

exception Contradiction

class type simplifier =
  object
    method name : string
    method copy : simplifier
    method assume : F.pred -> unit
    method target : F.pred -> unit
    method fixpoint : unit
    method infer : F.pred list

    method simplify_exp : F.term -> F.term
    method simplify_hyp : F.pred -> F.pred
    method simplify_branch : F.pred -> F.pred
    method simplify_goal : F.pred -> F.pred
  end

let is_atomic_pred = function
  | Neq _ | Eq _ | Leq _ | Lt _ | Fun _ -> true
  | _ -> false
let is_literal p = match repr p with
  | Not p -> is_atomic_pred (repr p)
  | _ ->  is_atomic_pred (repr p)

let iter_consequence_literals f_literal p =
  let f_literal = (fun p -> if QED.lc_closed p then f_literal p else ()) in
  let rec aux_pos p = match repr p with
    | And ps -> List.iter aux_pos ps
    | Not p ->  aux_neg p
    | Bind((Forall|Exists),_,a) -> aux_pos (QED.lc_repr a)
    | rep when is_atomic_pred rep -> f_literal p
    | _ -> ()
  and aux_neg p = match repr p with
    | Imply (hs,p) -> List.iter aux_pos hs ; aux_neg p
    | Or ps -> List.iter aux_neg ps
    | Not p -> aux_pos p
    | Bind((Forall|Exists),_,a) -> aux_neg (QED.lc_repr a)
    | rep when is_atomic_pred rep -> f_literal (e_not p)
    | _ -> ()
  in aux_pos p

(* -------------------------------------------------------------------------- *)
