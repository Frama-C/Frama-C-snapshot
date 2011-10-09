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

module WpLog = Wp_parameters
open Format
open Ctypes
open Cil_types
open Formula

(* ------------------------------------------------------------------------ *)
(* --- Goals information collection                                     --- *)
(* ------------------------------------------------------------------------ *)

module Collector =
struct

  type t = {
    mutable c_warning : Wpo.warning list;
    mutable c_depends : Property.t list;
  }
      
  let stack = ref []
    
  let push () =
    let c = { c_warning = []; c_depends = [] } in
    stack := c::(!stack) ; c
      
  let pop c = match !stack with
    | top::tl when top == c ->
        stack := tl; (List.rev c.c_warning, List.rev c.c_depends)
    | _ -> Wp_parameters.fatal "Datalib.Collector: inconsistent stack"

  let add_warning ?(severe=false) ?source ~reason effect =
    let f _ =
      let msg = Format.flush_str_formatter () in
      let wrn = {
        Wpo.wrn_loc = Log.get_current_source () ;
        Wpo.wrn_severe = severe ;
        Wpo.wrn_source = (match source with None -> "wp" | Some r -> r) ;
        Wpo.wrn_reason = reason ;
        Wpo.wrn_effect = msg ;
      } in
      match !stack with
        | top::_ -> top.c_warning <- wrn::top.c_warning
        | _ -> Wp_parameters.fatal "Datalib.Collector: empty stack"
    in Format.kfprintf f Format.str_formatter effect

  let add_depend pid =
    match !stack with
      | top::_ -> top.c_depends <- pid::top.c_depends
      | _ -> Wp_parameters.fatal "Datalib.Collector: empty stack"

end

module Create (V:Mvalues.Values) : Formula.Logic with module F = V.F =
struct

  module F = V.F

  (* ------------------------------------------------------------------------ *)
  (* --- Acsl Types Guards Generations                                    --- *)
  (* ------------------------------------------------------------------------ *)
    
  let has_type_rec = ref (fun (_:F.abstract) _ -> assert false)
  let has_obj_rec = ref (fun (_:F.abstract) _ -> assert false)

  module IsComp = F.DRegister
    (struct
       include F.Compinfo
       let prefix = "Is"
       let section = S_Logic_Def
       let clear () = ()
       let pp_title fmt x = Format.fprintf fmt "Type constraint for %a"
	 F.Compinfo.pp_title x
       let declare compinfo _ =
	 let pool = F.pool () in
	 let x = F.p_fresh pool "x" (Model(Record compinfo)) in
	 let vx = F.var x in
	 let has_type_fd f = !has_type_rec (F.acc_field vx f) (Ctype f.ftype) in
	 let def = F.p_conj (List.map has_type_fd compinfo.cfields) in
	 PredicateDef([x],def)
     end)

  let rec is_array_dim pool a te n =
    if n > 0 then
      let i = F.p_fresh pool "i" (Model Formula.Integer) in
      let a_i = F.acc_index (F.unwrap a) (F.var i) in
      F.p_forall [i] (is_array_dim pool a_i te (pred n))
    else !has_obj_rec a te
    
  module IsArray = F.DRegister
    (struct
       include F.ArrayDim
       let prefix = "IsArray"
       let section = S_Logic_Def
       let clear () = ()
       let pp_title fmt x = Format.fprintf fmt "Type constraint for %a"
	 F.ArrayDim.pp_title x
       let declare (te,n) _ =
	 let pool = F.pool () in
	 let a = F.p_fresh pool "a" (Model(V.tau_of_object_array te n)) in
	 PredicateDef([a],is_array_dim pool (F.var a) te n)
     end)
    
  let has_type_int i e = F.guard i e

  let is_comp comp e = F.p_app1 (IsComp.get_definition comp).d_name e
  let is_array arr e = 
    let adim = Ctypes.array_dim arr in
    F.p_app1 (IsArray.get_definition adim).d_name e

    	
  let has_obj e = function
    | C_int i -> has_type_int i (F.unwrap e)
    | C_float _ -> F.p_true
    | C_pointer _ -> F.p_true
    | C_array arr -> is_array arr e
    | C_comp comp -> is_comp comp e

  let has_type e = function
    | Ctype ty -> has_obj e (object_of ty)
    | (Ltype _ | Linteger | Lreal | Lvar _ | Larrow _ ) -> F.p_true

  let () = has_type_rec := has_type
  let () = has_obj_rec := has_obj
    
  (* ------------------------------------------------------------------------ *)
  (* --- Constrained Terms                                                --- *)
  (* ------------------------------------------------------------------------ *)
    
  type bindings = binding list and binding =
    | Forall of F.var list
    | Exists of F.var list
    | Any of F.var * F.pred
    | Let of F.var * F.abstract
	
  let pp_binding fmt = function
    | Forall xs -> Format.fprintf fmt "Forall @[%a@]" (Pretty_utils.pp_list F.pp_var) xs
    | Exists xs -> Format.fprintf fmt "Exists @[%a@]" (Pretty_utils.pp_list F.pp_var) xs
    | Any(x,p) -> Format.fprintf fmt "Any %a s.t. @[%a@]" F.pp_var x F.pp_pred p
    | Let(x,t) -> Format.fprintf fmt "Let %a = @[%a@]" F.pp_var x F.pp_term t
	
  type context = {
    pool : F.pool ;
    mutable bindings : bindings ;
  }
      
  let closed : bindings = []
  let context = ref []
    
  let occur_check y xs =
    if List.exists (F.eq_var y) xs then
      Wp_parameters.fatal
        "Quantification of constrained variable"
	
  let pp_vkind fmt = function
    | Formula.Model t -> F.pp_tau fmt t
    | Formula.Acsl (_t,ty) -> !Ast_printer.d_logic_type fmt ty
	
  let guards_with f xs p =
    let do_with f p x =
      match F.kind_of_var x with
        | Model _ -> p
        | Acsl (_,ty) -> f (has_type (F.var x) ty) p
	    
    in
    List.fold_left (do_with f) xs p
      
  let sub y xs = List.filter (fun x -> not (F.eq_var x y)) xs
    
  let some_alpha pool x = Some(F.p_freshen pool x)
    
  let rec apply_bindings alpha bindings p =
    match bindings with
      | [] -> p
      | b::bindings ->
          let p = apply_bindings alpha bindings p in
          match b with
            | Forall xs -> F.p_forall xs p
            | Exists xs -> F.p_exists xs p
            | Any(x,q) -> F.p_forall [x] (F.p_implies q p)
            | Let(x,t) -> F.p_subst alpha x t p
		
  let alpha x =
    match !context with
      | [] -> None
      | c::_ -> Some(F.p_freshen c.pool x)

  let do_subst x v p =
    if F.pred_has_var [x] p
    then F.p_subst alpha x v p
    else p
      
  let do_forall xs p =
    if F.pred_has_var xs p
    then
      let ys = List.filter (fun y -> F.pred_has_var [y] p) xs in
      if ys = [] then p
      else
        F.p_forall ys (guards_with F.p_implies p ys)
    else p
      
  let do_exists xs p =
    if F.pred_has_var xs p
    then
      let ys = List.filter (fun y -> F.pred_has_var [y] p) xs in
      if ys = [] then p
      else F.p_exists ys (guards_with F.p_and p ys)
    else p
      
  let rec has_var xs bindings =
    xs <> [] &&
      match bindings with
        | [] -> false
        | b::others ->
            match b with
              | Forall ys | Exists ys ->
                  let xs = List.fold_right sub ys xs in
                  has_var xs others
              | Any(y,p) ->
                  F.pred_has_var xs p || has_var (sub y xs) others
              | Let(y,t) ->
                  F.term_has_var xs t || has_var (sub y xs) others
		    
  let forall xs p =
    match !context with
      | [] -> do_forall xs p
      | c :: _ ->
          if has_var xs c.bindings
          then ( c.bindings <- Forall xs :: c.bindings ; p )
          else do_forall xs p

  let exists xs p =
    match !context with
      | [] -> do_exists xs p
      | c :: _ ->
          if has_var xs c.bindings
          then ( c.bindings <- Exists xs :: c.bindings ; p )
          else do_exists xs p

  let fresh id vk =
    match !context with
        | [] ->
            Wp_parameters.fatal "Bad context (for %S:%a)" id pp_vkind vk
        | c::_ ->
            F.p_fresh c.pool id
              (match vk with
                 | Formula.Model t -> Model t
                 | Formula.Acsl (t,ty) -> Acsl (t,ty)
              )

  let pool () =
    match !context with
      | [] -> Wp_parameters.fatal "Bad context (no pool available)"
      | c::_ -> c.pool

  let vkind_of_var x =
    match F.kind_of_var x with
      | Model t -> Formula.Model t
      | Acsl(t,c) -> Formula.Acsl (t,c)

  let term_such_that tau phi =
    match !context with
      | [] -> Wp_parameters.fatal "No context opened for constraints"
      | c::_ ->
          let x = F.p_fresh c.pool "X" (Model tau) in
          let t = F.var x in
          c.bindings <- Any(x,phi t) :: c.bindings ; t

  let subst_in_bindings c x v p =
    if has_var [x] c.bindings
    then ( c.bindings <- Let(x,v) :: c.bindings ; p )
    else ( do_subst x v p )

  let subst x v p =
    match !context with
      | [] -> do_subst x v p
      | c::_ -> subst_in_bindings c x (F.wrap v) p

  let close bindings p =
    apply_bindings (fun _ -> None) bindings p

  let has_context_vars xs p =
    F.pred_has_var xs p ||
      ( match !context with
          | [] -> false
          | c::_ -> has_var xs c.bindings )

  (* -------------------------------------------------------------------------- *)
  (* --- Context Management                                                 --- *)
  (* -------------------------------------------------------------------------- *)
      
  let push where pool bindings =
    Wp_parameters.debug ~dkey:"context" "PUSH %d: %S@." (List.length !context) where ;
    let c = { pool=pool ; bindings=bindings } in
    context := c :: !context ; c
      
  let pop where c0 =
    Wp_parameters.debug ~dkey:"context" "POPK %d: %S@." (pred (List.length !context)) where ;
    match !context with
      | [] -> Wp_parameters.fatal "No context for constrained term"
      | c::stack ->
          if not (c0 == c)
          then Wp_parameters.fatal "Context mismatch for constrained term" ;
          context := stack ;
          c.bindings
	    
  let flush where c0 p =
    Wp_parameters.debug ~dkey:"context" "FLUSH %d %S@." (pred (List.length !context)) where ;
    match !context with
      | [] -> Wp_parameters.fatal "No context for constrained term"
      | c::stack ->
          if not (c0 == c)
          then Wp_parameters.fatal "Context mismatch for constrained term" ;
          context := stack ;
          apply_bindings (some_alpha c.pool) c.bindings p
	    
  let kill where c = ignore (pop where c)
    
  (* ------------------------------------------------------------------------ *)
  (* --- Generalized Substitutions                                        --- *)
  (* ------------------------------------------------------------------------ *)
    
    
  let pp_sigma fmt s =
    begin
      Format.fprintf fmt "[" ;
      List.iter (fun (x,x') -> Format.fprintf fmt " %a:=%a" F.pp_var x F.pp_var x') s ;
    Format.fprintf fmt " ]" ;
    end
      
  let pp_bindings fmt xts =
    List.iter (fun (x,t) -> Format.fprintf fmt "%a:= %a@\n" F.pp_var x F.pp_term t) xts
      
      
  let freshen x =
    match !context with
      | [] -> Wp_parameters.fatal "no context opened for havoc"
      | c::_ -> F.p_freshen c.pool x
	  
  let apply = F.e_rename
    
  let rec domain xs d = function
    | [] -> List.rev xs,List.rev d
    | h::hs ->
	match h with
          | F.Fresh x -> domain (x::xs) d hs
          | F.Update(x,_) ->
              let d' = if List.exists (F.eq_var x) d then d else (x::d) in
              domain xs d' hs
		
  let reverse = List.map (fun (x,y) -> (y,x))
    
  let freshen_sigma y =
    List.map (fun u -> if F.eq_var (fst u) y then y,freshen y else u)
      
  let rec compute_bindings bindings sigmaR sigma = function
    | [] -> bindings , sigma
    | F.Fresh _ :: havocs -> compute_bindings bindings sigmaR sigma havocs
    | F.Update(x,phi) :: havocs ->
	let sigma' = freshen_sigma x sigma in
      let x' = snd (List.find (fun (y,_) -> F.eq_var x y) sigma') in
      let t' = apply sigmaR (phi sigma) in
      compute_bindings ((x',t')::bindings) sigmaR sigma' havocs
	
	
  let rec apply_bindings xts p =
    match xts with
      | [] -> p
      | (x,t)::xts -> apply_bindings xts (subst x t p)
	  
  let rec rename s p =
    match s with
      | [] -> p
      | (x,x')::s -> subst x (F.var x') (rename s p)
	  
  let rec fixpoint sf sn p =
    match sf,sn with
      | (_,x')::sf , (_,y)::sn ->
          F.p_implies
          (F.p_eq (F.var x') (F.var y))
            (fixpoint sf sn p)
      | [] , [] -> p
      | _ -> Wp_parameters.fatal "inconsistent domains in fixpoint substitutions"
	  
  let havoc_inductive hs p =
    let xs,d = domain [] [] hs in
    let sigma_0 = List.map (fun x -> x,freshen x) d in
    let sigma_F = List.map (fun x -> x,freshen x) d in
    let bindings,sigma_N = compute_bindings [] sigma_F sigma_0 hs in
    forall ( xs @ List.map snd sigma_F )
      (rename (reverse sigma_0)
         (apply_bindings bindings
            (fixpoint sigma_F sigma_N
               (rename sigma_F p))))
      
  let havoc_static hs p =
    let xs,d = domain [] [] hs in
    let sigma_0 = List.map (fun x -> x,freshen x) d in
    let bindings,sigma_N = compute_bindings [] sigma_0 sigma_0 hs in
    forall xs
      (rename (reverse sigma_0)
	 (apply_bindings bindings
          (rename sigma_N p)))
      
end

(* -------------------------------------------------------------------------- *)
(* --- Interpret C-runtime values                                         --- *)
(* -------------------------------------------------------------------------- *)

module Cvalues(M : Mvalues.Model) =
struct

  module A = M.A
  module R = M.R
  module F = M.F
  type loc = M.loc
  type m_cell
  type cell = F.abstract

  let loc_of_term = M.loc_of_term
  let term_of_loc = M.term_of_loc

  let cast_int_to_loc = M.cast_int_to_loc
  let cast_loc_to_int = M.cast_loc_to_int

  type value =
    | V_int of Ctypes.c_int * F.integer
    | V_float of Ctypes.c_float * F.real
    | V_pointer of Ctypes.c_object * M.loc
    | V_record of compinfo * F.record
    | V_union of compinfo * F.urecord
    | V_array of arrayinfo * F.array

  let rec logic_of_value = function
    | V_int(_,t) -> F.wrap t
    | V_float(_,t) -> F.wrap t
    | V_pointer(_,loc) -> F.wrap (M.term_of_loc loc)
    | V_record(_,t) -> F.wrap t
    | V_union(_,t) -> F.wrap t
    | V_array(_,t) -> F.wrap t


  let rec value_of_logic c_obj t =
    match c_obj with
      | C_int i -> V_int(i,F.unwrap t)
      | C_float f -> V_float(f,F.unwrap t)
      | C_pointer typ ->
	  V_pointer
	    (object_of typ,
	     M.loc_of_term (object_of typ) (F.unwrap t))
      | C_comp cinfo ->
          if cinfo.cstruct
          then V_record(cinfo,F.unwrap t)
          else V_union(cinfo,F.unwrap t)
      | C_array ainfo -> V_array(ainfo,F.unwrap t)

  let pp_loc = M.pp_loc
  let pp_value fmt = function
    | V_pointer(_,loc) -> M.pp_loc fmt loc
    | v -> F.pp_term fmt (logic_of_value v)

  (* ------------------------------------------------------------------------ *)
  (* --- Logic type of Values                                             --- *)
  (* ------------------------------------------------------------------------ *)

  let tau_of_loc = M.tau_of_loc
  let tau_of_object = function
    | C_int _ -> Integer
    | C_float _ -> Real
    | C_pointer _ -> Pointer tau_of_loc
    | C_comp c -> Record c
    | C_array a -> Array a

  let tau_of_object_array obj n = 
    let rec apply_dim t n =
      (*[LC] not perfect, but works *)
      if n > 0 then ADT("farray",[apply_dim t (pred n)])
      else t
    in apply_dim (tau_of_object obj) n

  let tau_of_ctype_logic t = tau_of_object (object_of t)

  let rec pp_tau fmt = function
    | Formula.Integer -> pp_print_string fmt "int"
    | Formula.Real -> pp_print_string fmt "real"
    | Formula.Boolean -> pp_print_string fmt "bool"
    | Formula.Pointer t -> Format.fprintf fmt "%a" pp_tau t
    | Formula.Record c -> Format.fprintf fmt "%s" c.Cil_types.cname
    | Formula.Array a ->
        Format.fprintf fmt "%a farray" pp_tau
          (tau_of_object (object_of a.arr_element))
    | Formula.Set te ->
        if Wp_parameters.verbose_atleast 2
        then Format.fprintf fmt "%a set" pp_tau te
        else pp_print_string fmt "set"
    | Formula.ADT(s,[]) -> pp_print_string fmt s
    | Formula.ADT(s,[t]) -> Format.fprintf fmt "%a %s" pp_tau t s
    | Formula.ADT(s,t::ts) ->
        Format.fprintf fmt "@[(%a" pp_tau t ;
        List.iter (fun t -> Format.fprintf fmt ",@,%a" pp_tau t) ts ;
        Format.fprintf fmt ") %s@]" s


  let tau_of_ctype t = tau_of_object (Ctypes.object_of t)

  let rec tau_of_logic_type = function
    | Ctype c -> tau_of_object (object_of c)
    | Linteger ->  Formula.Integer
    | Lreal ->  Formula.Real
    | Ltype( d , [] ) when d.lt_name = Utf8_logic.boolean ->  Formula.Boolean
    | Ltype( {lt_name="set"} , [t] ) -> Formula.Set (tau_of_logic_type t)
    | Ltype( lt , ts) ->
        let d = F.adt_decl lt in
         Formula.ADT (d,List.map tau_of_logic_type ts)
    | Lvar _ -> Wp_parameters.not_yet_implemented "logic type variables"
    | Larrow _ -> Wp_parameters.not_yet_implemented "type of logic function"


  (* ------------------------------------------------------------------------ *)
  (* --- Default Initialization of value                                  --- *)
  (* ------------------------------------------------------------------------ *)

  let init_value_term_rec = ref (fun _ _ _  -> assert false)

  module InitObj = F.DRegister
    (struct
       include F.Cobject
       let declare obj _ = 
	 Formula.Predicate [tau_of_object obj]
       let prefix = "IsInit"
       let section = S_Logic_Sig
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt
           "Predicate is initial value of type %a" F.Cobject.pp_title x
     end)


  module InitObjRange = F.DRegister
    (struct
       include F.Cobject
       let declare obj _ =
         Formula.Predicate [tau_of_object_array obj 1;Integer;Integer]
       let prefix = "IsInitRange"
       let section = S_Logic_Sig
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt
           "Initialisation of a range of type %a in a array" F.Cobject.pp_title x
     end)
    
  module InitObjRangeDef = F.DRegister
    (struct
       include F.Cobject
       let declare obj _ =
         let pool = F.pool () in
         let xt = F.p_fresh pool "t"
           (Model (ADT("farray",[tau_of_object obj])))
         in
         let t = F.var xt in
         let xi = F.p_fresh pool "i" (Model Integer) in
         let i = F.var xi in
         let xlow = F.p_fresh pool "low" (Model Integer) in
         let low = F.var xlow in
         let xhigh = F.p_fresh pool "high" (Model Integer) in
         let high = F.var xhigh in
         let is_init =
           F.p_app3 ((InitObjRange.get_definition obj).d_name) t low high
         in
         let i_low = F.p_icmp Cleq low i in
         let i_high = F.p_icmp Clt i high in
         let i_init =
           !init_value_term_rec pool (F.acc_index (F.unwrap t) i) obj
         in
         let body =
           F.p_forall [xi]
             (F.p_implies i_low (F.p_implies i_high i_init))
         in
         Formula.Axiom
           (F.p_forall [xt;xlow;xhigh] (F.p_iff is_init body))

       let prefix = "IsInitRangeDef"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt
           "Initialisation of a range of type %a in a array" F.Cobject.pp_title x
     end)

  let rec init_value_term pool x = function
    | C_int _      -> F.p_eq x (F.wrap F.i_zero)
    | C_float _    -> F.p_eq x (F.wrap F.r_zero)
    | C_pointer te  -> M.equal_loc (M.loc_of_term (object_of te) x) M.null
    | C_array arr ->
        let obj = object_of arr.arr_element in
	begin
          match arr.arr_flat with
            | None -> F.p_true
            | Some f -> 
		let bound = F.e_int64 f.arr_size in 
		InitObjRangeDef.define obj ;
		F.p_app3 (InitObjRange.get_definition obj).d_name x
		  F.i_zero bound
	end
		
    | C_comp cp ->
        begin
          let get_f f =  F.acc_field (F.unwrap x) f in
          let field_init f =
            init_value_term pool (get_f f) (object_of f.ftype)
          in
          List.fold_left (fun p f -> F.p_and p (field_init f))
            F.p_true cp.cfields
        end

  module InitObjDef = F.DRegister
    (struct
       include F.Cobject
       let declare obj _ =
         let p_is_init = (InitObj.get_definition obj).d_name in
         let pool = F.pool () in
         let vx = F.p_fresh pool "x" (Model (tau_of_object obj)) in
         let x = F.var vx in
         let is_init_x = F.p_app1 p_is_init x in
         let body = init_value_term pool x obj in
         Formula.Axiom (F.p_forall [vx ] (F.p_iff is_init_x body))

       let prefix = "IsInitDef"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt
           "Axiomatic definition of the predicate of  is initial value of type %a" F.Cobject.pp_title x
       let prefix = "IsInitRangeDef"
       let clear () = () 
       let pp_title fmt x = 
	 Format.fprintf fmt 
	   "Initialisation of a range of type %a in a array" F.Cobject.pp_title x	   
     end)

  let rec init_value_term pool x = function 
    | C_int _      -> F.p_eq x (F.wrap F.i_zero) 
    | C_float _    -> F.p_eq x (F.wrap F.r_zero) 
    | C_pointer te  -> M.equal_loc (M.loc_of_term (object_of te) x) M.null
    | C_array arr ->
        let obj = object_of arr.arr_element in
	begin
          match arr.arr_flat with
            | None -> F.p_true
            | Some f -> 
		let bound = F.e_int64 f.arr_size in 
		InitObjRangeDef.define obj ;
		F.p_app3 (InitObjRange.get_definition obj).d_name x
		  F.i_zero bound
	end
		
    | C_comp cp ->
	begin
	  let get_f f =  F.acc_field (F.unwrap x) f in 
	  let field_init f = 
	    init_value_term pool (get_f f) (object_of f.ftype) 
	  in
	  List.fold_left (fun p f -> F.p_and p (field_init f)) 
	    F.p_true cp.cfields
	end 
	  
 
  let () = init_value_term_rec := init_value_term

  let symb_is_init obj =
    if Ctypes.no_infinite_array obj then 
      (InitObjDef.define obj ;
       Some (InitObj.get_definition obj).d_name)
    else None 

  let symb_is_init_range obj =
    if Ctypes.no_infinite_array obj then 
      (InitObjRangeDef.define obj;
       Some (InitObjRange.get_definition obj).d_name)
    else None 

  (* ------------------------------------------------------------------------ *)
  (* --- Pointer Arithmetics                                              --- *)
  (* ------------------------------------------------------------------------ *)

  let lt_loc = M.lt_loc
  let le_loc = M.le_loc

  let minus_loc = M.minus_loc

  let equal_loc_bool = M.equal_loc_bool
  let lt_loc_bool = M.lt_loc_bool
  let le_loc_bool = M.le_loc_bool
  let equal_loc = M.equal_loc
  let le_loc = M.le_loc
  let lt_loc = M.lt_loc

  let null = M.null
  let is_null = M.is_null

  (* ------------------------------------------------------------------------ *)
  (* --- Comparison of Record and Arrays                                  --- *)
  (* ------------------------------------------------------------------------ *)

  module RecEqName = F.DRegister
    (struct
       include F.Compinfo
       let declare tcomp _eqname =
         Formula.Predicate[Record tcomp;Record tcomp]
       let prefix = "Eqrec"
       let section = S_Logic_Sig
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Equality for %a"
           F.Compinfo.pp_title x
     end)


  let equal_rec = ref (fun _ _ _ -> assert false)

  (** 2 struct or union objects are equal when all the fields are equal. *)
  let eq_record_definition tcomp eq_name =
    let pool = F.pool() in
    let xa = F.p_fresh pool "a" (Model (Record tcomp)) in
    let xb = F.p_fresh pool "b" (Model (Record tcomp)) in
    let ra = F.var xa in
    let rb = F.var xb in
    let p_comp =
      List.fold_left
        (fun p field ->
           let tf = Ctypes.object_of field.ftype in
           let va = F.acc_field ra field in
           let vb = F.acc_field rb field in
           let ef = !equal_rec tf va vb in
           F.p_and p ef
        ) F.p_true tcomp.cfields
    in
    F.p_forall [xa;xb]
      (F.p_iff (F.p_app2 eq_name ra rb) p_comp)

  let eq_array_definition ta eq_name =
    let pool = F.pool() in
    let te = Ctypes.object_of ta.arr_element in
    let vmodel = Model(Array ta) in
    let xa = F.p_fresh pool "a" vmodel in
    let xb = F.p_fresh pool "b" vmodel in
    let ra = F.var xa in
    let rb = F.var xb in
    let xi = F.p_fresh pool "i" (Model Integer) in
    let vi = F.var xi in
    let i_pos = F.p_icmp Cleq (F.i_zero) vi in
    let i_max =
      match ta.arr_flat with
        | None -> F.p_true
        | Some f -> F.p_icmp Clt vi (F.e_icst (Int64.to_string f.arr_size))
    in
    let i_range = F.p_and i_pos i_max in
    let a_comp =
      F.p_forall [xi]
        (F.p_implies
           i_range
           (!equal_rec te (F.acc_index ra vi) (F.acc_index rb vi))) in
    F.p_forall [xa;xb]
      (F.p_iff (F.p_app2 eq_name ra rb) a_comp)


  module RecEqSym = F.DRegister
    (struct
       include F.Compinfo
       let declare tcomp _eqname =
         let eq_name = F.p_app2 (RecEqName.get_definition tcomp).d_name in
         let pool = F.pool() in
         let xa = F.p_fresh pool "a" (Model (Record tcomp)) in
         let ra = F.var xa in
         let p =  F.p_forall [xa] (eq_name ra ra) in
         Formula.Axiom p
       let prefix = "EqrecSym"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Symmetry of Equality for %a"
           F.Compinfo.pp_title x
       let pp_descr fmt _ =
         Format.pp_print_string fmt "Axiomatic Definition"
     end)


  module RecEqTrans = F.DRegister
    (struct
       include F.Compinfo
       let declare tcomp _eqname =
         let eq_name = F.p_app2 (RecEqName.get_definition tcomp).d_name in
         let pool = F.pool() in
         let xa = F.p_fresh pool "a" (Model (Record tcomp)) in
         let ra = F.var xa in
         let xb = F.p_fresh pool "b" (Model (Record tcomp)) in
         let rb = F.var xb in
         let xc = F.p_fresh pool "c" (Model (Record tcomp)) in
         let rc = F.var xc in
         Formula.Axiom
           (F.p_forall [xa;xb;xc]
              (F.p_implies (eq_name ra rb)
                 (F.p_implies (eq_name rb rc) (eq_name ra rc))))
       let prefix = "EqrecTrans"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Symmetry of Equality for %a"
           F.Compinfo.pp_title x
       let pp_descr fmt _ =
         Format.pp_print_string fmt "Axiomatic Definition"
     end)

  module RecEqDef = F.DRegister
    (struct
       include F.Compinfo
       let declare tcomp _axname =
         let eq_name = RecEqName.get_definition tcomp in
         RecEqSym.define tcomp ;
         RecEqTrans.define tcomp ;
         Formula.Axiom(eq_record_definition tcomp eq_name.d_name)
       let prefix = "EqrecDef"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Equality for %a"
           F.Compinfo.pp_title x
       let pp_descr fmt _ =
         Format.pp_print_string fmt "Axiomatic Definition"
     end)

  module ArrEqName = F.DRegister
    (struct
       include F.Arrayinfo
       let declare arr _eqname =
         Formula.Predicate[Array arr;Array arr]
       let prefix = "Eqarr"
       let section = S_Logic_Sig
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Equality for %a" F.Arrayinfo.pp_title x
     end)

  module ArrEqDef = F.DRegister
    (struct
       include F.Arrayinfo
       let declare arr _axname =
         let eq_name = ArrEqName.get_definition arr in
         Formula.Axiom(eq_array_definition arr eq_name.d_name)
       let prefix = "EqarrDef"
       let section = S_Logic_Prop
       let clear () = ()
       let pp_title fmt x =
         Format.fprintf fmt "Equality for %a"
           F.Arrayinfo.pp_title x
       let pp_descr fmt _ =
         Format.pp_print_string fmt "Axiomatic Definition"
     end)

  let eq_record comp (ta:F.record) (tb:F.record) =
    if comp.cfields <>[] then RecEqDef.define comp ;
    let eq = RecEqName.get_definition comp in
    F.p_app2 eq.d_name ta tb

  let eq_array arr (ta:F.array) (tb:F.array) =
    ArrEqDef.define arr ;
    let eq = ArrEqName.get_definition arr in
    F.p_app2 eq.d_name ta tb

  let equal te a b =
    match te with
      | C_int _ | C_float _  -> F.p_eq a b
      | C_pointer t ->
          let obj = object_of t in
          M.equal_loc (M.loc_of_term obj a) (M.loc_of_term obj b)
      | C_comp comp -> eq_record comp (F.unwrap a) (F.unwrap b)
      | C_array arr -> eq_array arr (F.unwrap a) (F.unwrap b)

  let () = equal_rec := equal

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
