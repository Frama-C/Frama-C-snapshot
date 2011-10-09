(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies        *)
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

(* ------------------------------------------------------------------------ *)
(* ---  Translation of Term and Predicats                               --- *)
(* ------------------------------------------------------------------------ *)

module WpLog = Wp_parameters

open Ctypes
open Clabels
open Formula
open Cil_types
open Cil_datatype

let debug = WpLog.debug ~dkey:"trans"

module Create
  (M : Mlogic.S)
  =

struct

  module F = M.F
  module L = M.L

  (* ----------------------------------------------------------------------- *)
  (* --- Registration of User-defined Predicate and Functions            --- *)
  (* ----------------------------------------------------------------------- *)

  type user_formal =
    | UF_logic of logic_var * F.var
    | UF_references of 
	logic_var * F.var option * ( F.var * M.formal * string ) list
         (*lv, associated C var, list of formals at each label *)
    | UF_closure of F.var * M.closure * string
	(* a model variable (closure) required at a given label *)

  type userdef = {
    d_info : logic_info ;
    d_callname : string ;
    d_formals : user_formal list ;
  }

  type axiomlabel = {
    a_name : string ;
    a_defname : string ;
    a_property : F.pred ;
    a_memory : user_formal list ;
  }

  module Hdef = Logic_var.Hashtbl

  (* Memoization of axioms compilation tables *)
  let user_axioms : (string,F.pred option) Hashtbl.t = Hashtbl.create 131
  let user_axiomlabels : (string,axiomlabel option) Hashtbl.t =
    Hashtbl.create 131
  let user_definitions = Hdef.create 131

  let () = F.on_clear
    (fun () ->
       Hashtbl.clear user_axioms ;
       Hashtbl.clear user_axiomlabels ;
       Hdef.clear user_definitions ;
    )

  let rec pp_closures fmt (xs,cs) =
    match xs , cs with
      | [] , [] -> ()
      | x::xs , (c,l)::cs ->
          Format.fprintf fmt "{%s:%a=%a@@%s}@,"
            (F.name_of_var x)
            M.pp_tau (F.tau_of_var x)
            M.pp_closure c l ;
          pp_closures fmt (xs,cs)
      | x::xs , [] ->
          Format.fprintf fmt "{%s:%a=?}@,"
            (F.name_of_var x)
            M.pp_tau (F.tau_of_var x) ;
          pp_closures fmt (xs,[])
      | [] , (c,l)::cs ->
          Format.fprintf fmt "{?=%a@@%s}@,"
            M.pp_closure c l ;
          pp_closures fmt ([],cs)

  let pp_formals fmt xs =
    List.iter
      (fun x ->
         Format.fprintf fmt "(%s:%a)@,"
           (F.name_of_var x)
           M.pp_tau (F.tau_of_var x) ;
      ) xs

  module UserDefinition =
  struct
    let lock : unit Hdef.t = Hdef.create 131
    let () = F.on_clear (fun () -> Hdef.clear lock)

    let pp_userdef_title fmt d =
      let f = d.d_info in
      let x = f.l_var_info.lv_name in
      match f.l_type with
        | Some _ ->
            if f.l_tparams=[] && f.l_labels=[]
            then Format.fprintf fmt "User-defined constant %s" x
            else Format.fprintf fmt "User-defined function %s" x
        | None ->
            Format.fprintf fmt "User-defined predicate %s" x

    let pp_userformals fmt ufs =
      List.iter
	(function
	   | UF_logic(lv,x) -> 
	       Format.fprintf fmt
		 "@\n * (%a:%a) parameter '%s' in ACSL definition" 
		 F.pp_var x F.pp_tau (F.tau_of_var x)
		 (* original name of lv *) lv.lv_name
	   | UF_references(lv,opt_cx,refs) ->
	       begin
		 match opt_cx with 
		   | None -> () 
		   | Some y -> 
		       Format.fprintf fmt "@\n * (%a:%a) C reference to %a" 
			 F.pp_var y F.pp_tau (F.tau_of_var y)
			 !Ast_printer.d_logic_var lv 
	       end;
	       List.iter
		 (fun (x,formal,label) ->
		    Format.fprintf fmt "@\n * (%a:%a) reference to %a at %s" 
		      F.pp_var x F.pp_tau (F.tau_of_var x)
		      M.pp_formal (formal,lv) label )
		       refs
	   | UF_closure(x,closure,label) ->
	       Format.fprintf fmt "@\n * (%a:%a) %a at %s"
		 F.pp_var x F.pp_tau (F.tau_of_var x)
		 M.pp_closure closure label
	) ufs
	
    let pp_userdef_descr fmt d =
      if d.d_formals <> [] then
	begin
	  Format.fprintf fmt "Signature:" ;
	  pp_userformals fmt d.d_formals ;
	end

    let define duser items =
      let k = duser.d_info.l_var_info in
      List.iter
        (fun (name,item) ->
	   let section = 
	     match item with
	       | Function _ | Predicate _ -> S_User_Sig
	       | Axiom _ -> S_User_Prop
	       | _ -> assert false
	   in
           F.add_declaration {
             d_name = name ;
             d_section = section ;
             d_title = (fun fmt -> pp_userdef_title fmt duser) ;
             d_descr = (fun fmt -> pp_userdef_descr fmt duser) ;
             d_source = None ;
             d_item = item ;
           })
        items ;
      Hdef.remove lock k

    let unlock xdef =
      Hdef.remove lock xdef

    let lock xdef =
      if Hdef.mem lock xdef then
        ( Wp_parameters.not_yet_implemented "Recursive definition (in '%a')"
            !Ast_printer.d_logic_var xdef ) ;
      Hdef.add lock xdef ()

  end

  module UserAxiom = F.DRegister
    (struct
       type t = string
       module H = Hashtbl.Make
         (struct
            type t = string
            let hash = Hashtbl.hash
            let equal: string -> string -> bool = (=)
          end)
       let section = S_User_Prop
       let source = None
       let prefix = "Hyp"
       let clear () = ()
       let index x = x
       let basename x = x
       let location _x = None
       let declare x _ =
         try
           match Hashtbl.find user_axioms x with
             | Some p -> Formula.Axiom p
             | None -> raise Not_found
         with Not_found -> Wp_parameters.fatal "Uncompiled axiom (%s)" x
       let pp_title fmt x = Format.fprintf fmt "User-defined axiom %s" x
       let pp_descr fmt _x = Format.fprintf fmt "No labels."
     end)

  module UserAxiomDefs =
  struct

    let pp_labels fmt = function
      | [] -> ()
      | x::xs ->
          Format.fprintf fmt "@[{%s" x ;
          List.iter (fun x -> Format.fprintf fmt ",%s" x) xs ;
          Format.fprintf fmt "}@]"

    let pp_axiomdef_title fmt a =
      Format.fprintf fmt "User defined axiom %s" a.a_name

    let pp_axiomdef_descr fmt a =
      if a.a_memory <> [] then
	begin
          Format.fprintf fmt "Memory parameters: " ;
	  UserDefinition.pp_userformals fmt a.a_memory ;
	end

    let define axdef =
      F.add_declaration {
        d_name = axdef.a_defname ;
        d_section = S_User_Prop ;
        d_title = (fun fmt -> pp_axiomdef_title fmt axdef) ;
        d_descr = (fun fmt -> pp_axiomdef_descr fmt axdef) ;
        d_source = None ;
        d_item = Formula.Axiom axdef.a_property ;
      }

    let is_defined name = F.has_declaration ("Hyp_" ^ name)

  end

  (* ----------------------------------------------------------------------- *)
  (* --- Frame Environment                                               --- *)
  (* ----------------------------------------------------------------------- *)

  module Lmap = Map.Make
    (struct
       type t = c_label
       let compare = Pervasives.compare
     end)

  type frame = {
    mutable states : M.mem Lmap.t ;
    mutable result : F.var option ;
    mutable status : F.var option ;
    mutable return : Cil_types.typ option ;
  }

  let new_frame kf ?m_here ?m_pre ?m_post ?x_result ?x_status () =
    let bind l x s = match x with None -> s | Some m -> Lmap.add l m s in
    let states =
      bind Clabels.Here m_here
        (bind Clabels.Pre m_pre
           (bind Clabels.Post m_post
              Lmap.empty))
    in {
      states = states ;
      result = x_result ;
      status = x_status ;
      return = Some(Kernel_function.get_return_type kf) ;
    }

  let user_frame () =
    {
      states = Lmap.empty ;
      result = None ;
      status = None ;
      return = None ;
    }

  let result frame =
    match frame.result with
      | Some x -> x
      | None ->
          match frame.return with
            | Some typ ->
                let tau = M.tau_of_logic_type (Ctype typ) in
                let x = L.fresh "result" (Formula.Acsl(tau,Ctype typ)) in
                frame.result <- Some x ; x
            | None ->
                Wp_parameters.fatal "Result type undefined"

  let status frame =
    match frame.status with
      | Some x -> x
      | None ->
          let x = L.fresh "status" (Formula.Model Formula.Integer) in
          frame.status <- Some x ; x

  (* ----------------------------------------------------------------------- *)
  (* --- Translation Environment                                         --- *)
  (* ----------------------------------------------------------------------- *)

  type lvar_kind = (* What represents a given logic_var (lv) *)
    | Logic_cvar of varinfo    (* (lv) is a varinfo *)
    | Logic_value of M.value   (* (lv) mapsto a value in the model *)
    | Logic_term of F.abstract (* (lv) mapsto an arbitrary ACSL value *)
    | Logic_var of F.var       (* (lv) mapsto a collectable FOL variable *)
    | Logic_byref (* (lv) is a formal parameter of logic passed by reference *)

  type env = {
    formals_in_pre : bool ;
    frame : frame ;
    label : c_label ;
    xvars : M.value Varinfo.Map.t ; 
    (* maping of logic_vars when lv_origin<>None *)
    lvars : lvar_kind Logic_var.Map.t ; 
    (* lvar_kind of logic_vars when lv_origin=None *)
    mutable laddr : F.var Logic_var.Map.t ;
    (* addresses of by-reference user formal *)
  }



  let fresh_addr lv : F.var =  
    debug "[fresh_addr] of %a" !Ast_printer.d_logic_var lv;
    let tau = Formula.Pointer (M.tau_of_loc) in 
    let x = L.fresh lv.lv_name (Formula.Model tau) in 
    debug "[fresh_addr] of %a : %a" !Ast_printer.d_logic_var lv F.pp_var x;
    x

  let addr_of_ref env lv = 
    debug "[addr_of_ref] of %a" !Ast_printer.d_logic_var lv;
    try 
      let x = Logic_var.Map.find lv env.laddr in  
      debug "[addr_of_ref] of %a already recorded :%a"
	!Ast_printer.d_logic_var lv F.pp_var x ; x
    with 
	Not_found -> 
	  debug "[addr_of_ref] %a not yet in" 
	    !Ast_printer.d_logic_var lv;
	  let x = fresh_addr lv in 
	  env.laddr <-Logic_var.Map.add lv x env.laddr ; 
	  debug "[addr_of_ref] of %a recorded with %a"
	    !Ast_printer.d_logic_var lv F.pp_var x ; x
	  
	  

(* -------------------------------------------------------------------------- *)
(* ---  Global Recursion for Logic Functions (for logic constants)        --- *)
(* -------------------------------------------------------------------------- *)

  let rec_apply_function
      : (env -> logic_info ->
           (logic_label * logic_label) list ->
             term list -> F.abstract) ref
      = ref (fun _ _ _ _ -> assert false)

(* -------------------------------------------------------------------------- *)
(* ---  Logic-Variable access                                             --- *)
(* -------------------------------------------------------------------------- *)

  let lvar env lv : lvar_kind =
    match lv.lv_origin with
      | None ->
	  begin
	    try Logic_var.Map.find lv env.lvars
	    with Not_found ->
	      try
		let cst = Logic_env.find_logic_cons lv in
		Logic_term (!rec_apply_function env cst [] [])
	      with Not_found ->
		Wp_parameters.abort "Unknown logic constant %s" lv.lv_name
	  end
      | Some vi -> 
	  begin
	    try Logic_value (Varinfo.Map.find vi env.xvars)
	    with Not_found -> Logic_cvar vi
	  end

  let xvar env vi : M.value option =
    try Some(Varinfo.Map.find vi env.xvars) with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Allocation of (collectable) logic variables                        --- *)
(* -------------------------------------------------------------------------- *)

  let fresh_local lv : F.var =
    let lt = lv.lv_type in
    let tau = M.tau_of_logic_type lt in
    L.fresh lv.lv_name (Formula.Acsl(tau,lt))

  let fresh_logic_var pool lv =
    let lt = lv.lv_type in
    let t = M.tau_of_logic_type lt in
    F.p_fresh pool lv.lv_name (Formula.Acsl(t,lt))

  let add_logic_vars env pool lvs =
    let lvars = 
      List.fold_left
	(fun lvars lv ->
	   let x = fresh_logic_var pool lv in
	   Logic_var.Map.add lv (Logic_var x) lvars)
	env.lvars lvs
    in { env with lvars = lvars }

  let collect_logic_vars env = 
    Logic_var.Map.fold 
      (fun _lv lk acc -> 
	 match lk with
	   | Logic_var x -> x::acc
	   | _ -> acc)
      env.lvars []

  (* -------------------------------------------------------------------------- *)
  (* --- Local Bindings of logic variables to values                        --- *)
  (* -------------------------------------------------------------------------- *)

  let bind_lvars env (bindings : (logic_var * F.abstract) list)  =
    let lvars =
      List.fold_left
	(fun lvars (lv,term) -> Logic_var.Map.add lv (Logic_term term) lvars)
	env.lvars bindings
    in { env with lvars = lvars }

  let bind_lvar env lv term =
    { env with lvars = Logic_var.Map.add lv (Logic_term term) env.lvars }

  let bind_fresh env lv : (F.var * env) =
    let x = fresh_local lv in
    x , bind_lvar env lv (F.var x)

(* -------------------------------------------------------------------------- *)
(* --- Access to memory label in environment                              --- *)
(* -------------------------------------------------------------------------- *)

  let env_at e label = {
    formals_in_pre = e.formals_in_pre ;
    frame = e.frame ;
    label = label ;
    lvars = e.lvars ;
    xvars = e.xvars ;
    laddr = e.laddr;
  }

  let find_mem env label =
    try Some (Lmap.find label env.frame.states)
    with Not_found -> None

  let mem_at env label =
    try Lmap.find label env.frame.states
    with Not_found ->
      let m = M.mem () in
      env.frame.states <- Lmap.add label m env.frame.states ; m

  let mem_at_env env = mem_at env env.label

(* -------------------------------------------------------------------------- *)
(* ---  Return & Exit-Status variables                                    --- *)
(* -------------------------------------------------------------------------- *)

  let subst_result env vopt p =
    if env.frame.result = None then p
    else
      let x = result env.frame in
      match vopt with
        | None -> L.forall [x] p
        | Some v -> L.subst x (M.logic_of_value v) p

  let result_type env =
    match env.frame.return with
      | None -> Wp_parameters.fatal "no result type"
      | Some t -> t

  let exit_status env = status env.frame

(* -------------------------------------------------------------------------- *)
(* ---  Environment Constructors                                          --- *)
(* -------------------------------------------------------------------------- *)

  let env kf ?m_here ?m_pre ?m_post ?x_result () =
    {
      formals_in_pre = false ;
      frame = new_frame kf ?m_here ?m_pre ?m_post ?x_result () ;
      label = Here ;
      lvars = Logic_var.Map.empty ;
      xvars = Varinfo.Map.empty ;
      laddr = Logic_var.Map.empty;
    }

(* -------------------------------------------------------------------------- *)
(* ---  Environment Constructors for Calling Functions                    --- *)
(* -------------------------------------------------------------------------- *)

  (* Associates formal parameters to values *)
  let bind_formals called_kf vs =
    let rec bind xvars xs vs =
      match xs , vs with
	| x::xs , v::vs -> bind (Varinfo.Map.add x v xvars) xs vs
	| _ -> xvars
    in (* prototypes have exactly the good number of variables *)
    bind Varinfo.Map.empty (Kernel_function.get_formals called_kf) vs

  let call_pre caller_env called_kf vs m_pre =
    let frame = new_frame called_kf ~m_here:m_pre ~m_pre:m_pre () in
    {
      formals_in_pre = false ;
      frame = frame ;
      label = Here ;
      lvars = caller_env.lvars ;
      xvars = bind_formals called_kf vs ;
      laddr = caller_env.laddr ; 
    }

  let call_post caller_env called_kf vs m_pre m_post x_result =
    let frame =
      new_frame called_kf ~m_here:m_post ~m_pre ~m_post ?x_result ()
    in
    {
      formals_in_pre = true ;
      frame = frame ;
      label = Here ;
      lvars = caller_env.lvars ;
      xvars = bind_formals called_kf vs ;
      laddr = caller_env.laddr
    }

  let call_exit caller_env called_kf vs m_pre m_post x_status =
    let frame = 
      new_frame called_kf ~m_here:m_post ~m_pre ~m_post ~x_status ()
    in
    {
      formals_in_pre = false ;
      frame = frame ;
      label = Here ;
      lvars = caller_env.lvars ;
      xvars = bind_formals called_kf vs ;
      laddr = caller_env.laddr ; 
    }

  (* ----------------------------------------------------------------------- *)
  (* --- Translation Values                                              --- *)
  (* ----------------------------------------------------------------------- *)

  (* kinds are logic types *)
  type kind =
    | Kcint of Ctypes.c_int
    | Kint
    | Kreal
    | Kbool
    | Kptr of Cil_types.typ
    | Kset of kind
    | Kstruct of compinfo
    | Karray of arrayinfo
    | Kadt of string * kind list


  let rec kind_equal ka kb = 
    match ka,kb with 
      | Kcint i, Kcint j -> i = j
      | Kint, Kint | Kreal, Kreal | Kbool, Kbool -> true
      | Kptr t, Kptr t' -> Typ.equal t t'
      | Kset k, Kset k' -> kind_equal k k'
      | Kstruct cp, Kstruct cp' -> Compinfo.equal cp cp'
      | Karray a, Karray a' -> AinfoComparable.equal a a'
      | Kadt (s,ks), Kadt(s',ks') ->
	  s=s' && List.for_all2 kind_equal ks ks'
      | _, _ -> false

   let pp_kind fmt = function
     | Kcint i -> Ctypes.pp_int fmt i
     | Kint -> Format.pp_print_string fmt "int"
     | Kreal -> Format.pp_print_string fmt "real"
     | Kptr _-> Format.pp_print_string fmt "pointer"
     | Kset _-> Format.pp_print_string fmt "set"
     | Kstruct c -> Format.pp_print_string fmt c.cname
     | Karray a -> Ctypes.pretty fmt (C_array a)
     | Kadt (s,_) -> Format.pp_print_string fmt s
     | Kbool ->  Format.pp_print_string fmt "bool"

  type data =
    | Data     of F.abstract (* Singleton *)
    | Loc      of M.loc
    | Value    of M.value
    | Interval of F.interval
    | Range    of c_object * M.loc * F.interval
    | List     of data list
    | Set      of F.set

  let pp_data fmt = function
    | Data d -> Format.fprintf fmt "Data=%a" F.pp_term d
    | Loc l -> Format.fprintf fmt "Loc=%a" M.pp_loc l
    | Value v -> Format.fprintf fmt "Value=%a" M.pp_value v
    | _ -> Format.fprintf fmt "Blob"

  let data_of_integer (x : F.integer) : data = Data (F.wrap x)
  let data_of_real (x : F.real) : data = Data(F.wrap x)
  let data_of_boolean (x:F.boolean) : data = Data (F.wrap x)

  let integer_of_value = function
    | M.V_int(_,t) -> t
    | v -> Wp_parameters.fatal "integer_of_value %a" M.pp_value v

  let real_of_value = function
    | M.V_float(_,t) -> t
    | M.V_int(_,t) -> F.real_of_integer  t
    | v -> WpLog.fatal "[real_of_value] of %a" M.pp_value v

  let extract_from_data = function
    | Data d -> F.unwrap d
    | Value v -> F.unwrap (M.logic_of_value v)
    | d -> WpLog.fatal "[extract_from_data] of %a" pp_data d 

  let boolean_of_data d : F.boolean = extract_from_data d
  let array_of_data d : F.array = extract_from_data d
  let record_of_data d : F.record = extract_from_data d
  let urecord_of_data d : F.urecord = extract_from_data d

  let loc_of_data obj d =
    match d with
    | Value(M.V_pointer(_,l)) -> l
    | Value v -> M.loc_of_term obj (F.unwrap (M.logic_of_value v))
    | Data l -> M.loc_of_term obj (F.unwrap l)
    | Loc l ->  l
    | _ ->
        Wp_parameters.fatal ~current:true "not a loc (%a)" pp_data d

 let integer_of_data k d : F.integer =
    match d with
    | Data d ->
        begin
          match k with
            | Kint | Kcint _ -> F.unwrap d
            | k -> WpLog.fatal ~current:true "not an integer (%a : %a)" 
		F.pp_term d pp_kind k
        end
    | Value v -> integer_of_value v
    | _ -> WpLog.fatal ~current:true "not an integer (%a)" pp_data d

 let real_of_data k d =
    match k with
      | Kint | Kcint _-> F.real_of_integer (integer_of_data k d)
      | Kreal ->
          begin
            match d with
              | Data d -> F.unwrap d
              | Value v -> real_of_value v
              | _ -> WpLog.fatal ~current:true "not a real (%a)" pp_data d
          end
      | _ -> WpLog.fatal ~current:true "not a real (%a)" pp_data d


  let rec set_of  = function
    | Value v -> F.singleton (M.logic_of_value v)
    | Data d -> F.singleton d
    | Loc l ->  F.singleton (M.term_of_loc l)
    | List ds -> F.unions (List.map set_of ds)
    | Interval i -> F.interval i
    | Set s -> s
    | Range _ -> Wp_parameters.not_yet_implemented "set of zone"

  let list_of = function
    | (Value _|Data _|Loc _|Interval _|Range _) as d -> [d]
    | List xs -> xs
    | Set _ -> WpLog.fatal "[list_of] a set"

  let union_data a b =
    match a,b with
      | Set a , b | b, Set a -> Set (F.union a (set_of b))
      | List _ , _ | _ , List _ -> List (list_of a @ list_of b)
      | ( (Value _|Data _|Loc _|Range _|Interval _ ) ,
          (Value _|Data _|Loc _|Range _|Interval _ ) ) -> List[a;b]

  let union_map f = function
    | [] -> List []
    | d::ds ->
        List.fold_left
          (fun w x -> union_data w (f x))
          (f d) ds

  let term_of_data = function
    | Data t  -> t
    | Value v -> M.logic_of_value v
    | Loc l   -> F.wrap (M.term_of_loc l)
    | Set s   -> F.wrap s
    | List dl -> F.wrap (F.unions (List.map (set_of) dl))
    | Range _ -> Wp_parameters.not_yet_implemented "set of zone"
    | Interval i  -> F.wrap (F.interval i )

  let neg_interval r =
    match r.F.inf,r.F.sup with
      | Some j, None -> {F.inf = None ; F.sup = Some (F.e_ineg j)}
      | None , Some j -> {F.inf = Some (F.e_ineg j) ; F.sup = None}
      | Some j, Some k -> 
	  {F.inf = Some (F.e_ineg k) ; F.sup = Some (F.e_ineg j)}
      | None, None -> r

  (* ----------------------------------------------------------------------- *)
  (* --- Logic Types                                                     --- *)
  (* ----------------------------------------------------------------------- *)

  let rec object_of_pointed = function
    | Kptr te | Kset(Kptr te) ->  Ctypes.object_of te
    | _ -> WpLog.fatal "Dereferencing a non-pointer value"

  let kind_of_typ c=
      match object_of c with
        | C_int i -> Kcint i
        | C_float _ -> Kreal
        | C_pointer te -> Kptr te
        | C_comp comp -> Kstruct comp
        | C_array arr -> Karray arr

  let kind_of_data ty = function
    | (Data _ | Loc _ | Value _) -> kind_of_typ ty
    | (Interval _ | Range _ | List _ | Set _) -> Kset (kind_of_typ ty)

  let rec kind_of = function
    | Ctype c -> kind_of_typ c
    | Linteger -> Kint
    | Lreal -> Kreal
    | Ltype ({lt_name="ð”¹"}, _) -> Kbool
    | Ltype( {lt_name="set"} , [elt] ) -> Kset(kind_of elt)
    | Ltype( {lt_name=adt} , args ) -> Kadt(adt,List.map kind_of args)
    | Lvar _ -> WpLog.not_yet_implemented "logic type variables"
    | Larrow _ -> WpLog.not_yet_implemented "type of logic function"

  let typ_of_elements = function
    | Ctype c ->
        let o = object_of c in
        begin
          match o with
            | C_pointer te -> te
            | C_array arr -> arr.arr_element
            | _ ->
                WpLog.fatal "elements of non-pointer type %a" Ctypes.pp_object o
        end
    | t -> WpLog.fatal "elements of non-pointer type: %a"
        !Ast_printer.d_logic_type t

  (* ----------------------------------------------------------------------- *)
  (* --- Global Recursions                                               --- *)
  (* ----------------------------------------------------------------------- *)

  let data_rec : (env -> term -> data) ref = ref (fun _ _ -> assert false)

  (* ---------------------------------------------------------------------- *)
  (* --- Data memory predicate                                          --- *)
  (* ---------------------------------------------------------------------- *)

  let rec assigned_of_data te acc d =
    match d with
      | (Data _|Value _|Loc _) ->
          F.Aloc(te,loc_of_data te d)::acc
      | Range(te,loc,range) ->
          F.Arange(te,loc,range)::acc
      | List ds -> List.fold_left (assigned_of_data te) acc ds
      | _ -> Wp_parameters.not_yet_implemented "arbitrary zone"

  let data_valid m k d =
    let t =
      match k with
        | Kptr te -> te
        | Kset (Kptr te) -> te
        | _ -> WpLog.fatal "unexpected type for valid predicate"
    in
    let te = Ctypes.object_of t in
    F.p_conj (List.map (M.valid m) (assigned_of_data te [] d))

  let data_separated m (t1, d1) (t2, d2) =
    let r1 = assigned_of_data t1 [] d1 in
    let r2 = assigned_of_data t2 [] d2 in
    let p = ref F.p_true in
    List.iter
      (fun a ->
         List.iter
           (fun b ->
              p := F.p_and !p (M.separated m a b)
           ) r2
      ) r1 ;
    !p

  (* ---------------------------------------------------------------------- *)
  (* --- Data memory operation                                          --- *)
  (* ---------------------------------------------------------------------- *)

  let rec data_shift_range tobj loc kbi dindex ~is_pos =
    match dindex with
      | Data _ | Value _ | Loc _  as d ->
          let idx =integer_of_data kbi d in
          let idx = if is_pos then idx else F.e_ineg idx in
          Loc (M.shift loc tobj idx)
      | List il -> union_map (data_shift_range tobj loc kbi ~is_pos) il
      | Interval r ->
          let r = if is_pos then r else neg_interval r in Range(tobj,loc,r)
      | Set _ | Range _ ->
          Wp_parameters.not_yet_implemented "pointer shift over arbitrary sets"

  let rec data_index_range tobj loc kbi dindex =
    match dindex with
      | Data _ | Value _ | Loc _  as d ->
          Loc (M.index loc tobj (integer_of_data kbi d))
      | List il -> union_map (data_index_range tobj loc kbi ) il
      | Interval r -> Range(tobj,loc,r)
      | Set _ | Range _ ->
          Wp_parameters.not_yet_implemented "array access over arbitrary sets"


  let data_shift ka ga kb gb ~is_pos =
    match ka,kb with
      | (Karray _ | Kptr _ ) , (Kint | Kcint _) ->
          let obj = object_of_pointed ka in
          let gb = integer_of_data kb gb in
          let gb = if is_pos then gb else (F.e_ineg gb) in
          Loc (M.shift
                 (loc_of_data obj ga)
                  obj gb)
      | (Kptr _ | Karray _) , (Kset ((Kint | Kcint _) as kbi)) ->
          let obj = object_of_pointed ka in
          data_shift_range obj (loc_of_data obj ga) kbi gb ~is_pos
      | _ -> WpLog.not_yet_implemented "shift over arbitrary sets of pointers"

 let data_index ta ka ga kb gb =
   match ka,kb with
     | (Karray _ | Kptr _) , (Kint | Kcint _) ->
         Loc (M.index (loc_of_data ta ga) ta (integer_of_data kb gb))
     | (Karray _ | Kptr _) , (Kset ((Kint |Kcint _) as kbi)) ->
         data_index_range ta (loc_of_data ta ga) kbi gb
     | _ ->  WpLog.not_yet_implemented "shift over arbitrary sets of pointers"

  let data_field ka ga f =
    match ka with
      | Kset _ -> Wp_parameters.not_yet_implemented "field access over sets"
      | _ -> let obj = object_of f.ftype in Loc (M.field (loc_of_data obj ga) f)

  let rec data_startof_set ta ga =
    match ga with
      | Value _ | Data _ | Loc _ ->
          Loc (M.startof (loc_of_data ta ga) ta)
      | Set _ | Range _ ->
	  Wp_parameters.not_yet_implemented "start-of over sets"
      | List pl ->
          union_map (fun p -> data_startof_set ta p ) pl
      | Interval _ -> WpLog.fatal "unexpected argument for [startof]"

  let data_startof ta ka ga =
    match ka with
      | Kset _ -> data_startof_set ta ga
      | _ -> Loc( M.startof (loc_of_data ta ga) ta )

  let rec data_load env ty  = function
    | Loc _ | Data _ | Value _  as d ->
        let obj = object_of ty in
        Value (M.load (mem_at_env env) obj (loc_of_data obj d))
    | Range _ ->
	Wp_parameters.not_yet_implemented "load of arbitrary region"
    | Set _ ->
        Wp_parameters.not_yet_implemented "load of arbitrary sets"
    | List pl -> union_map (data_load env ty) pl
    | _ -> WpLog.fatal "unexpected argument for [load]"


  (* ---------------------------------------------------------------------- *)
  (* --- Offsets inside functional memory model                         --- *)
  (* ---------------------------------------------------------------------- *)

  let rec logic_offset env a = function
    | TNoOffset -> Data a
    | TField(f,off) ->
        let fieldvalue = F.acc_field (F.unwrap a) f in
        logic_offset env fieldvalue off
    | TIndex (t,off) ->
        let i = integer_of_data (kind_of t.term_type) (!data_rec env t) in
        logic_offset env (F.acc_index (F.unwrap a) i) off

  let rec loc_offset env loc ty = function
    | TNoOffset -> loc
    | TField(f,off) ->
        loc_offset env (M.field loc f) (Ctypes.object_of f.ftype) off
    | TIndex(t,off) ->
        let k = integer_of_data (kind_of t.term_type) (!data_rec env t) in
        let te = Ctypes.object_of_array_elem ty in
        loc_offset env (M.index loc te k) te off

  (* ---------------------------------------------------------------------- *)
  (* --- Offsets in the C-memory model                                  --- *)
  (* ---------------------------------------------------------------------- *)

  let rec memory_offset env ty (dp:data) = function
    | TNoOffset -> ty,dp
    | TIndex(t,off) ->
        let kp = kind_of_data ty dp in
        let ki = kind_of t.term_type in
        let di = !data_rec env t in
        let te = Cil.typeOf_array_elem ty in
        let ta = object_of te in
        let dq = data_index ta kp dp ki di in
        memory_offset env te dq off
    | TField(f,off) as offset ->
        (match dp with
          | Range _ | Set _ | List _ ->
              Datalib.Collector.add_warning
                ~reason:"field access over set of l-values"
                "Ignored offset '%a' in assign clause"
                !Ast_printer.d_term_offset offset ;
              ty, dp
          | dp ->
              let dq = data_field (kind_of_typ ty) dp f in
              memory_offset env f.ftype dq off )

  let gaddress_of_cvar tenv x off : (typ * data) =
    let tenv =
      if tenv.formals_in_pre && x.vformal
      then (env_at tenv Pre) else tenv
    in
    memory_offset tenv x.vtype (Loc (M.cvar (mem_at_env tenv) x)) off


  let gaddress_of_ref tenv lv off : data =
    let loc = M.lvar (mem_at_env tenv) lv (addr_of_ref tenv lv) in 
    match lv.lv_type with 
      | Ctype ty ->
	  let tr,gloc = memory_offset tenv ty (Loc loc) off in
	  data_load tenv tr gloc 
      | ty ->
	  begin
	    match off with 
	      | TNoOffset -> Loc loc 
	      |  _ ->
		   let s = "[gaddress_of_ref] C offset of logic_var" in 
		   WpLog.fatal "%s %a with a pure logic type %a" 
		     s !Ast_printer.d_logic_var lv 
		     !Ast_printer.d_logic_type ty
	  end
	  
    
   

  let gaddress_of_mem tenv e off : (typ * data) =
    let g = !data_rec tenv e in
    let te =
      match kind_of e.term_type with
        | Kptr telt -> telt
        | Kset(Kptr telt) -> telt
        | _ -> WpLog.fatal "expected pointer"
    in
    memory_offset tenv te g off

  let gstartof (ty,g) =
    let te = Cil.typeOf_array_elem ty in
    let ta = object_of te in
    data_startof ta (kind_of_typ ty) g

  let gstartof_cvar env x off =
    gstartof (gaddress_of_cvar env x off)

  let gstartof_mem env e off =
    gstartof (gaddress_of_mem env e off)

  let gstartof_value env ty v off =
    gstartof (memory_offset env ty (Value v) off)

  (* ---------------------------------------------------------------------- *)
  (* --- Arithmetics Cast                                               --- *)
  (* ---------------------------------------------------------------------- *)

  let cast v ty_from ty_to =
    if kind_equal ty_from ty_to then v
    else
      match ty_from with
        | Kcint i ->
            let vi = integer_of_data ty_from v in
            begin match ty_to with
              | Kcint j ->
                  if Ctypes.sub_c_int i j then v
                  else
                    Value (M.V_int(j,F.modulo j vi))
              | Kint -> Data(F.wrap vi)
              | Kreal -> Data(F.wrap (F.real_of_integer vi))
              | Kptr te -> Loc (M.cast_int_to_loc i vi te)
              | k -> WpLog.not_yet_implemented "logic cast from %a to %a"
                  Ctypes.pp_int i pp_kind k
            end
        | Kint ->
            let vi = integer_of_data ty_from v in
            begin match ty_to with
              | Kcint j -> Value (M.V_int(j,F.modulo j vi))
              | Kint -> Data(F.wrap vi)
              | Kptr te -> Loc (M.cast_int_to_loc (Ctypes.c_ptr()) vi te)
              | Kreal -> Data(F.wrap (F.real_of_integer vi))
              | k -> WpLog.not_yet_implemented
                  "logic cast from integer to %a" pp_kind k
            end
        | Kreal ->
            let vr = real_of_data ty_from v in
            begin match ty_to with
              | Kcint j -> Value(M.V_int(j,F.modulo j
                                               (F.integer_of_real vr)))
              | Kint -> Data(F.wrap (F.integer_of_real vr))
              | Kreal -> Data(F.wrap vr)
              | k ->  WpLog.not_yet_implemented
                  "logic cast from real to %a" pp_kind k
            end
        | Kptr tfrom ->
            let loc = loc_of_data (object_of tfrom) v in
            begin match ty_to with
              | Kcint j -> Value (M.V_int(j, M.cast_loc_to_int tfrom loc j))
              | Kptr tto -> Loc (M.cast_loc_to_loc tfrom tto loc)
              | k -> WpLog.not_yet_implemented
                  "logic cast from pointer over %a to %a"
                    !Ast_printer.d_type tfrom pp_kind k
            end
        | k -> WpLog.not_yet_implemented "logic cast from %a to %a"
                 pp_kind k pp_kind ty_to


  (* ---------------------------------------------------------------------- *)
  (* --- Binary Operators                                               --- *)
  (* ---------------------------------------------------------------------- *)

  let int_op = function
    | PlusA   -> F.e_iop Formula.Iadd
    | MinusA  -> F.e_iop Formula.Isub
    | Mult    -> F.e_iop Formula.Imul
    | Div     -> F.e_iop Formula.Idiv
    | Mod     -> F.e_iop Formula.Imod
    | BAnd    -> F.e_band
    | BXor    -> F.e_bxor
    | BOr     -> F.e_bor
    | Shiftlt -> F.e_lshift
    | Shiftrt -> F.e_rshift
    | _ ->       WpLog.fatal "[int_op] non integer operator"

  let real_op = function
    | PlusA  -> F.e_rop Formula.Radd
    | MinusA -> F.e_rop Formula.Rsub
    | Mult   -> F.e_rop Formula.Rmul
    | Div    -> F.e_rop Formula.Rdiv
    | _ ->      WpLog.fatal "[real_op] non real operator"

  let rel_op = function
    | Rlt  -> Lt
    | Rgt  -> Gt
    | Rle  -> Le
    | Rge  -> Ge
    | Req  -> Eq
    | Rneq -> Ne

  let real_cmp op r1 r2 =
    match op with
      | Lt -> F.e_rcmp Formula.Clt r1 r2
      | Gt -> F.e_rcmp Formula.Clt r2 r1
      | Le -> F.e_rcmp Formula.Cleq r1 r2
      | Ge -> F.e_rcmp Formula.Cleq r2 r1
      | Eq -> F.e_rcmp Formula.Ceq r1 r2
      | Ne -> F.e_rcmp Formula.Cneq r1 r2
      | _ ->  WpLog.fatal "[real_cmp] non real comparator"

  let int_cmp op i1 i2 =
    match op with
      | Lt -> F.e_icmp Formula.Clt i1 i2
      | Gt -> F.e_icmp Formula.Clt i2 i1
      | Le -> F.e_icmp Formula.Cleq i1 i2
      | Ge -> F.e_icmp Formula.Cleq i2 i1
      | Eq -> F.e_icmp Formula.Ceq i1 i2
      | Ne -> F.e_icmp Formula.Cneq i1 i2
      | _ ->  WpLog.fatal "[int_cmp] non intger comparator"

  let preal_cmp op r1 r2 =
    match op with
      | Lt -> F.p_rcmp Formula.Clt r1 r2
      | Gt -> F.p_rcmp Formula.Clt r2 r1
      | Le -> F.p_rcmp Formula.Cleq r1 r2
      | Ge -> F.p_rcmp Formula.Cleq r2 r1
      | Eq -> F.p_rcmp Formula.Ceq r1 r2
      | Ne -> F.p_rcmp Formula.Cneq r1 r2
      | _ ->  WpLog.fatal "[preal_cmp] non real relation"

  let pint_cmp op i1 i2 =
    match op with
      | Lt -> F.p_icmp Formula.Clt i1 i2
      | Gt -> F.p_icmp Formula.Clt i2 i1
      | Le -> F.p_icmp Formula.Cleq i1 i2
      | Ge -> F.p_icmp Formula.Cleq i2 i1
      | Eq -> F.p_icmp Formula.Ceq i1 i2
      | Ne -> F.p_icmp Formula.Cneq i1 i2
      | _ ->  WpLog.fatal "[pint_cmp] non integer relation"

  let ptr_rel op l1 l2 =
    match op with
      | Lt -> M.lt_loc l1 l2
      | Gt -> M.lt_loc l2 l1
      | Le -> M.le_loc l1 l2
      | Ge -> M.le_loc l2 l1
      | Eq -> M.equal_loc l1 l2
      | Ne -> F.p_not (M.equal_loc l1 l2)
      | _ ->  WpLog.fatal "[ptr_rel] non pointer relation"

  let ptr_cmp op l1 l2 =
    match op with
      | Lt -> M.lt_loc_bool l1 l2
      | Gt -> M.lt_loc_bool l2 l1
      | Le -> M.le_loc_bool l1 l2
      | Ge -> M.le_loc_bool l2 l1
      | Eq -> M.equal_loc_bool l1 l2
      | Ne -> F.e_not (M.equal_loc_bool l1 l2)
      | _ -> WpLog.fatal "[ptr_cmp] non pointer comparator"

  let plus i j  = F.e_iop Formula.Iadd i j

  let plus_interval r i =
    match r.F.inf , r.F.sup with
      | None , None -> r
      | Some j, None -> {F.inf = Some (plus i j ); F.sup = None}
      | None, Some k -> {F.inf = None ; F.sup = Some (plus i k)}
      | Some j, Some k -> {F.inf = Some (plus i j );F.sup = Some (plus i k)}

  let plus_interval_interval a b =
    match a , b with
      | ({ F.sup=None ; F.inf=None } as top) , _
      | _ , ({ F.sup=None ; F.inf=None } as top) -> Interval top
      | _ -> Set (F.add_set (F.interval a) (F.interval b))


  let rec add_integer  ka ga kb gb =
    match ga,gb with
      | Set _ , _ | _ , Set _ -> Set (F.add_set (set_of ga) (set_of gb))
      | (Value _ | Data _), (Value _ | Data _) ->
          data_of_integer (plus (integer_of_data ka ga) (integer_of_data kb gb))
      | Interval r1 , Interval r2 ->plus_interval_interval r1 r2
      | (Value _ | Data _ ),Interval r ->
          Interval (plus_interval r (integer_of_data ka ga))
      | Interval r,(Value _ | Data _ ) ->
          Interval (plus_interval r (integer_of_data kb gb))
      | List _ , List _ -> Set (F.add_set (set_of ga) (set_of gb))
      | List ds , b ->  union_map (add_integer kb b ka) ds
      | b , List ds -> union_map (add_integer ka b kb) ds
      | _ -> WpLog.fatal "unsuitable arguments for integer addition"

  let data_cmp binop ka ga kb gb =
    match ka,kb with
      |( Kint  | Kcint _) , (Kint| Kcint _) ->
        int_cmp binop (integer_of_data ka ga) (integer_of_data kb gb)
      | (Kreal|Kint| Kcint _) , (Kreal|Kint| Kcint _) ->
         real_cmp binop (real_of_data ka ga) (real_of_data kb gb)
      | Kptr ty , Kptr _ ->
          let obj = object_of ty in
          ptr_cmp  binop  (loc_of_data obj ga) (loc_of_data obj gb)
      | _ ->
          Wp_error.not_yet_implemented
            "boolean comparison between %a and %a"
            pp_kind ka pp_kind kb

  let data_binop kr binop ka ga kb gb =
    match binop with

      (* pointer arithmetics *)

      | (IndexPI | PlusPI) -> data_shift ka ga kb gb ~is_pos:true
      | MinusPI -> data_shift ka ga kb gb ~is_pos:false
      | MinusPP ->
          begin
            match ka with
              | Kptr te ->
                  let obj = object_of te in
                 data_of_integer
                   (M.minus_loc (loc_of_data obj ga) (loc_of_data obj gb))
              | _ -> WpLog.fatal "wrong parameters for pointer arithmetics"
          end

      (* scalar arithmetics *)

      | PlusA | MinusA | Mult | Div | Mod
      | BAnd | BXor | BOr | Shiftlt | Shiftrt ->
          begin
            match kr with
             | (Kint  | Kcint _)  -> data_of_integer
                   (int_op binop
                      (integer_of_data ka ga) (integer_of_data kb gb))
              | Kreal -> data_of_real
                  (real_op binop (real_of_data ka ga) (real_of_data kb gb))
              | (Kset Kint |Kset (Kcint _)) when binop = PlusA ->
                  add_integer ka ga kb gb
              | _ -> WpLog.fatal "wrong parameters for scalar arithmetics"
          end

      (* comparisons *)
      | Eq | Ne | Gt | Ge | Lt | Le ->
          data_of_boolean (data_cmp binop ka ga kb gb)

      (*logic or *)
      | LOr ->
          data_of_boolean (F.e_or (boolean_of_data ga) (boolean_of_data gb))

      (* logic and *)
      | LAnd ->
          data_of_boolean (F.e_and (boolean_of_data ga) (boolean_of_data gb))

  (* ------------------------------------------------------------------------ *)
  (* --- Unary Operators                                                  --- *)
  (* ------------------------------------------------------------------------ *)

  let data_unop kr unop ka ga =
    match unop with
      | Neg ->
          begin
            match kr with
              | (Kint  | Kcint _)  ->
                  data_of_integer (F.e_ineg (integer_of_data ka ga))
              | Kreal -> data_of_real (F.e_rneg (real_of_data ka ga))
              | _ -> WpLog.fatal
                  "wrong type of parameter for negation operator"
          end
      | LNot ->
          let b =
            match kr with
              |(Kint | Kcint _)->
                 F.e_icmp Ceq (integer_of_data ka ga) (F.e_icst "0")
              |Kreal -> F.e_rcmp Ceq (real_of_data ka ga) (F.e_rcst "0.0")
              |Kptr ty ->
                 let obj = object_of ty in
                 M.is_null (loc_of_data obj ga)
              | Kbool ->
                  F.e_not (boolean_of_data ga)
              | k -> WpLog.fatal
                  "%a : wrong type of parameter for logic not operator" pp_kind k
          in data_of_boolean b
      | BNot ->
          begin
            match kr with
             | (Kint  | Kcint _) ->
                 data_of_integer (F.e_bnot (integer_of_data ka ga))
             | _ -> WpLog.fatal
                 "wrong type of parameter for bitwise not operator"
          end

  (* ------------------------------------------------------------------------ *)
  (* --- Constants                                                        --- *)
  (* ------------------------------------------------------------------------ *)

  let data_const = function
    | CInt64(k,_,_) ->
        data_of_integer (F.e_icst (My_bigint.to_string k))

    | CChr c ->
        data_of_integer (F.e_icst (Int64.to_string (Ctypes.char c)))

    | CReal(f,_,_) ->
        data_of_real (F.e_rcst (string_of_float f))

    | CEnum e ->
        let machdep = true in
        let e' = Cil.constFold machdep e.eival in
        begin
          match e'.enode with
            | Const CInt64(k,_,_) ->
                data_of_integer (F.e_icst (My_bigint.to_string k))
            | Const CChr c ->
                data_of_integer (F.e_icst (string_of_int (Char.code c)))
            | _ -> WpLog.fatal "unrecognized sizeof/alignof "
        end
    | CWStr _        ->
        WpLog.not_yet_implemented "wide character string constant"
    | CStr s         ->
        WpLog.not_yet_implemented "character string constant (%S)" s

  (* ------------------------------------------------------------------------ *)
  (* --- Terms                                                            --- *)
  (* ------------------------------------------------------------------------ *)

  let rec data_of_term env term =
    match term.term_node with

      (* Constants *)
      | TConst c -> data_const c

      (* Operators *)

      | TUnOp (unop,a) ->
          data_unop (kind_of term.term_type) unop
            (kind_of a.term_type) (data_of_term env a)

      | TBinOp(binop,a,b) ->
          data_binop
            (kind_of term.term_type) binop
            (kind_of a.term_type) (data_of_term env a)
            (kind_of b.term_type) (data_of_term env b)

      (* L-Values *)

      | TLval(TResult _ ,off) ->
          logic_offset env (F.var(result env.frame)) off

      | TLval (TVar{lv_name = "\\exit_status"},_) ->
          Data (F.var (status env.frame))

      | TLval (TVar lv,off) ->
	  begin
	    match lvar env lv with
	      | Logic_cvar x ->
		  let tr,gloc = gaddress_of_cvar env x off in
                  data_load env tr gloc
	      | Logic_value (M.V_pointer(ty,loc)) ->
		  Loc (loc_offset env loc ty off)
	      | Logic_value v ->
		  let t = M.logic_of_value v in
                  logic_offset env t off
	      | Logic_term t ->
		  logic_offset env t off
	      | Logic_var x -> logic_offset env (F.var x) off
	      | Logic_byref ->  gaddress_of_ref env lv off
	  end

      | TLval(TMem e,off) ->
          let tr,gloc = gaddress_of_mem env e off in
          data_load env tr gloc

      | TAddrOf(TVar{lv_origin=Some x},off) ->
           begin
            match xvar env x with
              | None -> snd (gaddress_of_cvar env x off)
              | Some v -> snd (memory_offset env x.vtype (Value v) off)
          end

      | TStartOf(TVar{lv_origin=Some x},off) ->
          begin
            match xvar env x with
              | None -> gstartof_cvar env x off
              | Some v -> gstartof_value env x.vtype v off
          end

      | TAddrOf(TMem e,off) ->
          snd (gaddress_of_mem env e off)
      | TStartOf(TMem e,off) ->
          gstartof_mem env e off

      | TAddrOf(TResult _,_)
      | TStartOf(TResult _,_) -> WpLog.not_yet_implemented "&\\result"

      | TAddrOf(TVar {lv_origin=None},_) ->
          WpLog.fatal "taking address of a logic variable"

      | TStartOf(TVar {lv_origin=None},_) ->
          WpLog.not_yet_implemented "reference to a logic array"

      (* At *)

      | Tat(t,label) -> data_of_term (env_at env (c_label label)) t

      (* Sizeof and alignment *)

      | TSizeOf _
      | TSizeOfE _
      | TSizeOfStr _
      | TAlignOf _
      | TAlignOfE _ ->
          let machdep = true in
          let e' = Cil.constFoldTerm machdep term in
          begin
            match e'.term_node with
            | TConst _ -> data_of_term env e'
            | _ -> WpLog.fatal "unrecognized sizeof/alignof (%a)"
                !Ast_printer.d_term term
          end

      (* Conditional *)

      | Tif (b, t, f) ->
          Data
          (F.e_cond (boolean_of_data (data_of_term env b))
            (term_of_data (data_of_term env t))
            (term_of_data (data_of_term env f)))

      (* Memory call *)

      | Tbase_addr t ->
          let obj = match t.term_type with
            | Ctype ty -> object_of ty
            | _ -> WpLog.fatal "Base-address of logic type object"
          in
          Loc (M.base_address
                 (mem_at_env env)
                 (loc_of_data  obj (data_of_term env t)))

      | Tblock_length t ->
          let obj = match t.term_type with
            | Ctype ty -> object_of ty
            | _ -> WpLog.fatal "Block-length of logic type object"
          in
          data_of_integer
            (M.block_length
               (mem_at_env env)
               (loc_of_data obj (data_of_term env t)))

      (* Range *)

      | Trange (ti,tj ) ->
          let option_int env  = function
            | None -> None
            | Some x -> Some (integer_of_data (kind_of x.term_type)
                                 (data_of_term env x))
          in
          let r = {F.inf =(option_int env ti);
                   F.sup =(option_int env tj)} in
          Interval r

      | Tempty_set -> List  []

      | Tunion xs -> union_map (data_of_term env) xs


      | Tinter(a::b) ->
          Set (List.fold_left
                 (fun s1 s2 ->
                    F.inter s1 (set_of (data_of_term env s2)))
                 (set_of (data_of_term env a)) b)

      | Tinter [] -> WpLog.fatal "empty intersection"

      | Tcomprehension (_, _, _) ->
          WpLog.not_yet_implemented "Set comprehension"

      (* Conversions *)

      | Tnull -> Loc M.null

      | TCastE (ty,t) ->
           if Cil.isPointerType ty && Cil.isLogicZero t then
             Loc M.null
           else
             cast (data_of_term env t) (kind_of t.term_type) (kind_of_typ ty)

      (* Logic ADT *)

      | TUpdate (_,TNoOffset,tv) ->
          data_of_term env tv

      | TUpdate (r,TField (f, TNoOffset),tv) ->
          begin
            match kind_of (r.term_type) with
              | Kstruct _ ->
                  let record = record_of_data (data_of_term env r) in
                  let v = term_of_data (data_of_term env tv) in
                  let r = F.upd_field record f v in
                  Data (F.wrap r)

              | _ ->
                  WpLog.fatal "Functional update of a non-record value"
          end

      | TUpdate (r,TIndex(k, TNoOffset),tv) ->
          begin
            match kind_of (r.term_type) with
              | Karray _ ->
                  let array = array_of_data (data_of_term env r) in
                  let idx = integer_of_data (kind_of k.term_type)
                    (data_of_term env k) in
                  let v = term_of_data (data_of_term env tv) in
                  let r = F.upd_index array idx v in
                  Data (F.wrap r)

              | _ ->
                  WpLog.fatal "Functional update of a non-array value"
          end

      | TUpdate (_,_,_) ->
          WpLog.not_yet_implemented "ACSL extension for functional update"

      | TDataCons({ctor_name="\\true"},[]) -> Data(F.wrap F.e_true)
      | TDataCons({ctor_name="\\false"},[]) -> Data(F.wrap F.e_false)

      | TDataCons (c,_)  ->
          WpLog.not_yet_implemented "Constructor (%s)" c.ctor_name

      (* Jessie *)
      | TCoerce (_,_)
      | TCoerceE (_,_) ->
          WpLog.fatal "Only produced by Jessie plugin"

      (* Type *)

      | Ttypeof _
      | Ttype _ ->
          WpLog.not_yet_implemented "Type Tag"

      (* Let binding  *)

      | Tlet (({l_var_info =x;
               l_labels=[];l_tparams=[];
               l_profile =[];l_body=(LBterm t1); l_type=Some _ } as linfo),t2) ->
           if Logic_env.Logic_builtin_used.mem linfo then
            Wp_parameters.not_yet_implemented "Built-ins symbols"
           else
	     let var,env2 = bind_fresh env x in
             let t1' = term_of_data (data_of_term env t1) in
             let t2' = term_of_data (data_of_term env2 t2) in
             Data (F.e_subst L.alpha var t1' t2')
      | Tlet _ ->
          WpLog.not_yet_implemented
            "Complex Let-binding"

      (* Logic-Function Call *)

      | Tapp (lfun,labels,args) ->
          if Logic_env.Logic_builtin_used.mem lfun then
            Wp_parameters.not_yet_implemented "Built-ins symbols"
          else
            Data (!rec_apply_function env lfun labels args)

      (* Higer order function *)

      | Tlambda (_,_) ->
          WpLog.not_yet_implemented "Higher order functions"

  let () = data_rec := data_of_term

  let term env t = term_of_data (data_of_term env t)

  (* ------------------------------------------------------------------------ *)
  (* --- Assignable                                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let rec data_of_assignable env t=
    match t.term_node with

    | TLval (TVar{lv_origin=Some x}, off) ->
        begin
          match xvar env x with
            | None -> snd (gaddress_of_cvar env  x off)
            | Some v -> Value v
        end

    | TLval (TMem e, off) ->
        snd (gaddress_of_mem env e off)

    | TStartOf (TVar{lv_origin=Some x}, off)  ->
        begin
          match xvar env x with
            | None -> gstartof_cvar env x off
            | Some v -> gstartof_value env x.vtype v off
        end

    | TStartOf (TMem e, off) -> gstartof_mem env e off

    | Tempty_set  | TLval (TResult _, _ )-> List []

    | Tat(t ,lab) -> data_of_assignable (env_at env (c_label lab)) t

    | TBinOp((IndexPI | PlusPI),a,b) ->
        data_shift
          (kind_of a.term_type) (data_of_term env a)
          (kind_of b.term_type) (data_of_term env b)
          ~is_pos:true

    | TBinOp (MinusPI,a,b) ->
        data_shift
          (kind_of a.term_type) (data_of_term env a)
          (kind_of b.term_type) (data_of_term env b)
          ~is_pos:false

    | Tunion ts -> union_map (data_of_assignable env) ts
    | Tinter (t::ts) ->
        let f t = set_of (data_of_assignable env t) in
        Set(List.fold_left
              (fun s t -> F.inter s (f t)) (f t) ts)
    | Tinter [] -> WpLog.fatal "empty intersection"

    | TStartOf (TResult _, _ )
    | Tlet (_, _) | Tcomprehension (_, _, _)
    | Tif (_, _, _) | Tapp (_, _, _) | TCastE (_, _)

        -> WpLog.not_yet_implemented "assignation of (%a)"
        !Ast_printer.d_term t

    | TLval (TVar {lv_origin=None}, _)
    | TAddrOf _
    | TStartOf (TVar {lv_origin=None}, _)
    | Trange (_, _)|Ttype _|Ttypeof _|TCoerceE (_, _)|TCoerce (_, _)
    | TUpdate (_, _, _)|Tblock_length _
    | TDataCons (_, _)|Tlambda (_, _)
    | TUnOp (_, _)|TAlignOfE _|TAlignOf _|TSizeOfStr _|TSizeOfE _|TSizeOf _
    | TConst _ | Tnull | Tbase_addr _
    | TBinOp(
        (LOr|LAnd|BOr|BXor|BAnd|Ne|Eq|Ge|Le|Gt|Lt|
             Shiftrt|Shiftlt|Mod|Div|Mult|
                 MinusPP|MinusA|PlusA),_,_)
      -> WpLog.fatal "not assignable terms"

  let assigned env t =
    (* (ZD) DO preserve this filter, as \empty is polymorphic! *)
    match t.term_node with
      |  Tempty_set  | TLval (TResult _, _ ) -> []
      | _ ->
          let data = data_of_assignable env t in
          let te =
            match t.term_type with
              | Ctype te -> te
              | Ltype( {lt_name="set"} , [Ctype elt] ) -> elt
              | _ -> WpLog.fatal
                  "unexpected logic-type for assignable term %a"
                    !Ast_printer.d_logic_type t.term_type
          in
          assigned_of_data (object_of te) [] data

  (* ------------------------------------------------------------------------ *)
  (* --- Properties                                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let rec_apply_predicate
      : (env -> logic_info ->
           (logic_label * logic_label) list -> term list -> F.pred) ref
      = ref (fun _ _ _ _ -> assert false)

  let pred_cmp binop ka ga kb gb =
    match ka,kb with
      | (Kint  | Kcint _) , (Kint  | Kcint _) ->
          pint_cmp binop (integer_of_data ka ga) (integer_of_data kb gb)
      | (Kreal|Kint| Kcint _) , (Kreal|Kint| Kcint _) ->
          preal_cmp binop (real_of_data ka ga) (real_of_data kb gb)
      |  Kptr ty , Kptr _ ->
           let obj = object_of ty in
           ptr_rel binop (loc_of_data obj ga) (loc_of_data obj gb)
      | Kstruct s, Kstruct s' when (s.Cil_types.ckey=s'.Cil_types.ckey) ->
          M.eq_record s (record_of_data ga) (record_of_data gb)
      | Karray arr, Karray arr' when Ctypes.AinfoComparable.equal arr arr' ->
          M.eq_array arr (array_of_data ga) (array_of_data gb)
      | _ ->
          begin
            match binop with
              | Eq -> F.p_eq (term_of_data ga) (term_of_data gb)
              | Ne -> F.p_not (F.p_eq (term_of_data ga) (term_of_data gb))
              | _  -> WpLog.fatal ~current:true "Unexpected comparison"
          end

  let rec prop env p =
    List.fold_right F.p_named p.name (prop_body env p)

  and prop_body env p =
    match p.content with
      | Pfalse -> F.p_false

      | Ptrue  -> F.p_true

      | Pand(p1,p2) ->
          F.p_and (prop env p1)(prop env p2)

      | Por(p1,p2)  ->
          F.p_or (prop env p1)(prop env p2)

      | Pxor(p1,p2) ->
          (*TODO : no primitive translation for XOR in WHY *)
          (*Infact in bool.why there is bool_xor *)
          F.p_xor(prop env p1)(prop env p2)

      | Pimplies(p1,p2) ->
          F.p_implies(prop env p1)(prop env p2)

      | Piff(p1,p2)->
          F.p_iff(prop env p1)(prop env p2)

      | Pnot p -> F.p_not (prop env p)

      | Pif(c,pt,pf) ->
          F.p_cond (F.unwrap (term env c))
            (prop env pt)
            (prop env pf)

      | Pat (p,l) ->  prop (env_at env (c_label l))  p

      | Prel ( rel ,t1,t2) ->
          let ct1 = kind_of t1.term_type in
          let ct2 = kind_of t2.term_type in
          let m1 = data_of_term env t1 in
          let m2 = data_of_term env t2 in
          pred_cmp (rel_op rel) ct1 m1 ct2 m2

      | Pvalid t ->
          let k = kind_of t.term_type in
          let d  = data_of_term env t in
          data_valid (mem_at_env env) k d

      | Pvalid_index(tp,ti) ->
          let ty = match tp.term_type with
            | Ctype te -> te
            | _ -> WpLog.fatal "expected a non logic type"
          in
          let kp = kind_of_typ ty in
          let ki = kind_of ti.term_type in
          let dp = data_of_term env tp in
          let di = data_of_term env ti in
          if Cil.isArrayType ty then
            let te = Cil.typeOf_array_elem ty in
            let ta = object_of te in
            let d = data_index ta kp dp ki di in
            data_valid (mem_at_env env) kp d
          else
            if Cil.isPointerType ty then
              let d = data_shift kp dp ki di ~is_pos:true in
              data_valid (mem_at_env env) kp d
            else WpLog.fatal "unexepected type for valid index"

      | Pvalid_range(b,l,h) ->
          let tb = b.term_type in
          let k = kind_of tb in
          begin
            match k with
              | Kptr _ ->
                  let ty = typ_of_elements tb in
                  let obj = object_of ty in
                  let loc = loc_of_data obj (data_of_term env b) in
                  let rg = {
                    F.inf = Some (integer_of_data (kind_of l.term_type)
                                    (data_of_term env l));
                    F.sup = Some (integer_of_data (kind_of h.term_type)
                                    (data_of_term env h));
                  } in
                  M.valid (mem_at_env env) (F.Arange(obj,loc,rg))
              | _ -> WpLog.fatal "unsuitable argument for [valid_range]"
          end

      | Pfresh _t -> WpLog.not_yet_implemented "fresh"

      | Pinitialized _t -> WpLog.not_yet_implemented "initialized"

      | Psubtype (_t1,_t2) ->  WpLog.not_yet_implemented "subtype"

      | Plet(def, p) ->
          begin
            let lv = def.l_var_info in
            match def.l_body, def.l_profile with
                LBterm t, [] ->
                  let x = fresh_local lv in
                  L.subst x (term env t)
                    (prop (bind_lvar env lv (F.var x)) p)
              | _ -> WpLog.not_yet_implemented "local binding"
          end

      | Pforall (xs,p) ->
          let freshes = List.map (fun x -> x , fresh_local x) xs in
          let quantified = List.map snd freshes in
          let assoc = List.map (fun (x,v) -> x,F.var v) freshes in
          L.forall quantified (prop (bind_lvars env assoc) p)

      | Pexists (xs,p) ->
          let freshes = List.map (fun x -> x , fresh_local x) xs in
          let quantified = List.map snd freshes in
          let assoc = List.map (fun (x,v) -> x,F.var v) freshes in
          L.exists quantified (prop (bind_lvars env assoc) p)

      | Pseparated tl ->
          let gs =
            List.map
              (fun t ->
                 let te =
                   match kind_of t.term_type with
                     | Kptr te -> te
                     | Kset (Kptr te) -> te
                     | k -> WpLog.fatal "separated on non pointer type : %a " pp_kind k
                 in
                 Ctypes.object_of te, data_of_term env t) tl in
          let ags = Array.of_list gs in
          let p = ref F.p_true in
	  let m = mem_at_env env in 
          for i=0 to Array.length ags - 2 do
            for j=i+1 to Array.length ags - 1 do
              p := F.p_and !p (data_separated m ags.(i) ags.(j))
            done
          done ;
          !p

      | Papp (predicate,labels,args) ->
          if Logic_env.Logic_builtin_used.mem predicate then
            Wp_parameters.not_yet_implemented "Built-ins symbol %a (%d)"
              !Ast_printer.d_logic_var predicate.l_var_info predicate.l_var_info.lv_id
          else
            !rec_apply_predicate env predicate labels args




  (* ------------------------------------------------------------------------ *)
  (* --- Accessing User-Definitions                                       --- *)
  (* ------------------------------------------------------------------------ *)

  let get_definition cc fdef =
    let xdef = fdef.l_var_info in
    try Hdef.find user_definitions xdef
    with Not_found ->
      UserDefinition.lock xdef ;
      try
        let udef , items = cc fdef in
        Hdef.add user_definitions xdef udef ;
        UserDefinition.define udef items ; udef
      with error ->
        UserDefinition.unlock xdef ; raise error

  (* ----------------------------------------------------------------------- *)
  (* --- Compilation of User-defined Predicate and Functions             --- *)
  (* ----------------------------------------------------------------------- *)

  let push_context where =
    L.push where (F.pool()) L.closed

  let flush_context where context p =
    L.flush where context p

  let kill_context where context =
    L.kill where context

  let user_default_label = function
    | [] -> Clabels.Here
    | LogicLabel (None, first) :: _ -> Clabels.LabelParam first
    | LogicLabel (Some _, _) :: _ ->
        Wp_parameters.fatal
          "Unexpected redefined labels in user-defined predicates"
    | StmtLabel _ :: _ ->
        Wp_parameters.fatal
          "Unexpected stmt-labels in user-defined predicates"

  let user_env pdef =
    let context = push_context "user" in
    try
      let frame = user_frame () in
      let lvars = ref Logic_var.Map.empty in
      let laddr = ref Logic_var.Map.empty in 
      let profile = List.map
        (fun lv ->
	   if M.userdef_is_ref_param lv then
	     begin
	       lvars := Logic_var.Map.add lv Logic_byref !lvars ;
	       let opt_cx = 
		 if M.userdef_ref_has_cvar lv 
		 then 
		   let x = fresh_addr lv in 
		   laddr := Logic_var.Map.add lv x !laddr ;
		   Some x else None
	       in
	       UF_references ( lv ,opt_cx, [] ) (* initially empty *)
	     end
	   else
	     begin
	       let x = fresh_local lv in
	       lvars := Logic_var.Map.add lv (Logic_term (F.var x)) !lvars ;
	       UF_logic ( lv , x )
	     end) 
	pdef.l_profile
      in
      let env = {
	formals_in_pre = false ;
        frame = frame ;
        label = user_default_label pdef.l_labels ;
        lvars = !lvars ;
        xvars = Varinfo.Map.empty ;
	laddr = !laddr;
      }
      in ( context , profile , env )
    with err ->
      kill_context "user env" context; raise err

  let collect_signature profile filter env =
    let closures = ref [] in
    let references = ref Logic_var.Map.empty in
    let get_refs lv refs =
      try Logic_var.Map.find lv refs
      with Not_found -> []
    in
    Lmap.iter
      (fun label mem ->
	 match label with
	   | LabelParam label ->
	       begin
		 (* Collecting reference parameters *)
		 List.iter
		   (fun (x,lv,formal) -> if filter x then
		      let refs = 
			(x,formal,label) :: (get_refs lv !references) in
		      references := Logic_var.Map.add lv refs !references)
		   (M.userdef_ref_signature mem) ;
		 (* Collecting memory parameters *)
		 List.iter
		   (fun (x,clos) -> if filter x then
		      closures := UF_closure(x,clos,label) :: !closures)
		   (M.userdef_mem_signature mem) ;
	       end
	   | _ -> ())
      env.frame.states ;
    begin
      List.rev !closures @
	List.map
	(function
	   | (UF_logic _ | UF_closure _) as p -> p
	   | UF_references(lv,opt_cx,_) -> 
	       UF_references(lv, opt_cx, List.rev (get_refs lv !references))
	) profile
    end

  (* WARNING: should be the same order of binding that apply_formals *)
  let rec flatten_formals = function
    | [] -> []
    | UF_logic(_,x)::ufs -> x :: flatten_formals ufs
    | UF_closure(x,_,_)::ufs -> x :: flatten_formals ufs
    | UF_references(_,None,refs)::ufs -> flatten_references refs ufs
    | UF_references(_,Some x,refs)::ufs -> x :: flatten_references refs ufs

  and flatten_references refs ufs =
    match refs with
      | [] -> flatten_formals ufs
      | (x,_,_)::refs -> x :: flatten_references refs ufs

  let all_filter (_:F.var) = true
  let term_filter t x = F.term_has_var [x] t
  let pred_filter p x = F.pred_has_var [x] p

  let compile_predicate pdef =
    let o_name = Fol_decl.identifier (pdef.l_var_info.lv_name) in
    let p_name = "D_" ^ o_name in
    let d_name = "Def_" ^ o_name in
    let context, profile, env = user_env pdef in
    try
      let body,filter =
        match pdef.l_body with
          | LBpred named ->
              let v = prop env named in Some v , pred_filter v
          | LBterm def  ->
              let v = term env def in Some (F.p_bool (F.unwrap v)) , term_filter v
          | LBnone -> 
	      Wp_parameters.warning ~once:true ~current:false
		"No definition for '%s' interpreted as reads nothing" o_name ;
	      None , all_filter
          | LBreads xs ->
              (*TODO: Below is an incorrect translation (LC+BM) (Idem for "\from")
                (LC) Reason : xs are to be interpreted as left-values.
                (Use case : f reads t[0..n])
                let vs = List.map (fun x -> term env x.it_content) xs in
                None,vs,[]
              *)
              Wp_parameters.warning ~once:true ~current:false
		"Interpreting reads-definition as expressions rather than tsets" ;
              List.iter (fun x -> ignore (term env x.it_content)) xs ;
              None , all_filter
          | LBinductive _ ->
              Wp_parameters.not_yet_implemented "Inductive predicates"
      in
      let signature = collect_signature profile filter env in
      let formals = flatten_formals signature in
      let declaration = 
	p_name , Formula.Predicate(List.map F.tau_of_var formals) in
      let definitions =
        match body with
          | None -> kill_context "compile" context ; []
          | Some body ->
              let p_axiom =
                L.forall formals
                  (F.p_iff
                     (F.p_call p_name (List.map F.var formals))
                     (flush_context "compile" context body))
              in
              [ d_name , Formula.Axiom p_axiom ]
      in
      {
        d_info = pdef ;
        d_callname = p_name ;
        d_formals = signature ;
      } ,
      declaration :: definitions
    with err ->
      kill_context "compile" context ;
      raise err

  let compile_function fdef =
    let o_name = Fol_decl.identifier (fdef.l_var_info.lv_name) in
    let f_name = "D_" ^ o_name in
    let d_name = "Def_" ^ o_name in
    let context, profile, env = user_env fdef in
    try
      let body,filter =
        match fdef.l_body with
          | LBterm def  ->
              let v = term env def in Some v , term_filter v
          | LBreads xs ->
              (*TODO: Incorrect translation (Cf. predicates)
              None,List.map (fun x -> term env x.it_content) xs
              *)
              Wp_parameters.warning ~once:true ~current:false
		"Interpreting reads-definition as expressions rather than tsets" ;
              List.iter (fun x -> ignore (term env x.it_content)) xs ;
              None , all_filter
          | LBnone ->
	      Wp_parameters.warning ~once:true ~current:false
		"No definition for '%s' interpreted as reads nothing" o_name ;
              None , all_filter
          | LBinductive _ ->
              Wp_parameters.fatal "Inductive function"
          | LBpred _ ->
              Wp_parameters.fatal "Function defined by a predicate"
      in
      let ltyp =  
	match fdef.l_type with
          | Some ltyp -> ltyp 
          | None -> Wp_parameters.fatal "Function defined with not result type"
      in
      let t_result = M.tau_of_logic_type ltyp in
      let signature = collect_signature profile filter env in
      let formals = flatten_formals signature in
      let declaration =
        f_name , Formula.Function(List.map F.tau_of_var formals,t_result) in
      let call_f = F.e_call f_name (List.map F.var formals) in
      let definitions =
        match body with
          | None -> kill_context "compile" context ; []
          | Some def ->
              let f_axiom = F.p_forall formals
                (flush_context "compile" context (F.p_eq call_f def))
              in
              [ d_name , Formula.Axiom f_axiom ]
      in
      let guards = 
	let cond = L.has_type call_f ltyp in
	if F.is_true cond then []
	else [ f_name ^ "_result" , Formula.Axiom (L.forall formals cond) ]
      in
      {
        d_info = fdef ;
        d_callname = f_name ;
        d_formals = signature ;
      } ,
      declaration :: (definitions @ guards)
    with err ->
      kill_context "compile" context ;
      raise err

  (* ----------------------------------------------------------------------- *)
  (* --- Compilation of User-defined Axiom                               --- *)
  (* ----------------------------------------------------------------------- *)

  let axiom_env here =
    let frame = user_frame () in
    let here = Clabels.LabelParam here (* Non-expected ! *) in
    {
      formals_in_pre = false ;
      frame = frame ;
      label = here ;
      lvars = Logic_var.Map.empty ;
      xvars = Varinfo.Map.empty ;
      laddr = Logic_var.Map.empty ;
    }

  let compile_user_axiom _name predicate =
    let context = push_context "axiom" in
    try
      let env = axiom_env "WP_nowhere" in
      let def = prop env predicate in
      flush_context "axiom" context def
    with err ->
      kill_context "axiom" context ;
      raise err

  let compile_user_axiom_labels name (labels,predicate) =
    let d_name = "Hyp_" ^ name in
    let context = push_context "axiom-labels" in
    try
      let here =
        match labels with
          | LogicLabel (None, l)::_ -> l
          | _ -> Wp_parameters.fatal "No logic label for Axiom '%s'" name
      in
      let env = axiom_env here in
      let body = prop env predicate in
      let signature = collect_signature [] (pred_filter body) env in
      let formals = flatten_formals signature in
      let property = L.forall formals (flush_context "axiom-labels" context body)
      in {
        a_name = name ;
        a_defname = d_name ;
	a_memory = signature ;
        a_property = property ;
      }
    with err ->
      kill_context "axiom-labels" context ;
      raise err

  let compile_and_define hdefs name data compiler definer =
    let cdata =
      try Hashtbl.find hdefs name with Not_found ->
        try
          let cdata = Some (compiler name data) in
          Hashtbl.add hdefs name cdata ; cdata
        with error ->
          Hashtbl.add hdefs name None ; raise error
    in match cdata with
      | None -> ()
      | Some data -> definer name data

  let add_axiom name labels predicate =    
    if not (UserAxiomDefs.is_defined name) then
      if labels = [] then
	compile_and_define user_axioms name predicate
          compile_user_axiom
          (fun name _ -> UserAxiom.define name)
      else
	compile_and_define user_axiomlabels name (labels,predicate)
          compile_user_axiom_labels
          (fun _ axdef -> UserAxiomDefs.define axdef)

  (* ------------------------------------------------------------------------ *)
  (* --- Applying Definitions                                             --- *)
  (* ------------------------------------------------------------------------ *)

  (* Binds formal parameters to their actual arguments *)
  (* - ufs: user_formal list *)
  (* - dargs: data list *)
  (* - returns: F.abstract list *)

  (* WARNING: apply_formals must bind formals in the same way that
     flatten_formals collect the formals *)

  let bool_of_option = function Some _ -> true | None -> false 

  let rec apply_formals env labels ufs dargs =
    let s = "[apply_formals]" in
    match ufs , dargs with
      | [] , [] -> []
      | [] , d :: _ ->
	  Wp_parameters.fatal 
	    "WP.UserDefs: signature mismatch (args) to much args %a"
	    pp_data d ;
      | UF_logic (lv,_) :: ufs_tail , data :: dargs_tail -> 
	  debug "%s : %a binds to %a, ufs_tail:%d, darg_tail:%d"
	    s !Ast_printer.d_logic_var lv pp_data data 
	    (List.length ufs_tail) (List.length dargs_tail);
	  term_of_data data :: apply_formals env labels ufs_tail dargs_tail
      | UF_logic (lv,_) :: _ , [] ->
	  Wp_parameters.fatal 
	    "WP.UserDefs: signature mismatch (args) %a have no value"
	    !Ast_printer.d_logic_var lv;
      | UF_closure(_,closure,at) :: ufs_tail , _ ->
	  let label = Clabels.lookup labels at in
	  let mem = mem_at env label in
	  let varg = M.userdef_mem_apply mem closure in
	  varg :: apply_formals env labels ufs_tail dargs

      | UF_references(_,_,_)::_ , [] ->
	  Wp_parameters.fatal "WP.UserDefs: signature mismatch (refs)"

      | UF_references(_,None,refs)::ufs_tail , data::dargs ->
	  apply_references env labels refs data ufs_tail dargs

      | UF_references(_,Some _,refs)::ufs_tail , data::dargs ->
	  let loc = 
	    match data with
	      | Value(M.V_pointer(_,loc)) -> loc
	      | Loc loc -> loc
	      | _ -> Wp_parameters.fatal "WP.UserDefs: no reference found"
	  in
	  let inner_loc = M.inner_loc loc in
	  debug "%s the location of %a : %a the C loc : %a" s 
	    pp_data data M.pp_loc loc F.pp_term inner_loc; 

	  inner_loc :: apply_references env labels refs data ufs_tail dargs


  and apply_references env labels refs data ufs dargs =
    match refs with
      | [] -> apply_formals env labels ufs dargs

      | (_,formal,at) :: refs_tail ->
	  let loc = 
	    match data with
	      | Value(M.V_pointer(_,loc)) -> loc
	      | Loc loc -> loc
	      | _ -> Wp_parameters.fatal "WP.UserDefs: no reference found"
	  in
	  let label = Clabels.lookup labels at in
	  let mem = mem_at env label in
	  let value = M.logic_of_value (M.userdef_ref_apply mem formal loc) in
	  value :: apply_references env labels refs_tail data ufs dargs

  let apply_predicate env def labels args =
    let definition = get_definition compile_predicate def in
    let arguments = List.map (data_of_term env) args in
    let bindings = apply_formals env labels definition.d_formals arguments in
    F.p_call definition.d_callname bindings

  let apply_function env def labels args =
    let definition = get_definition compile_function def in
    let arguments = List.map (data_of_term env) args in
    let bindings = apply_formals env labels definition.d_formals arguments in
    F.e_call definition.d_callname bindings

  let () =
    begin
      rec_apply_predicate := apply_predicate ;
      rec_apply_function  := apply_function ;
    end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
