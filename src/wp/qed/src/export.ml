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
(* --- Exportation to Foreign Languages                                   --- *)
(* -------------------------------------------------------------------------- *)

open Format
open Logic
open Plib
open Linker
open Engine

let cmode = function 
  | Mpositive | Mnegative -> Cprop 
  | Mterm | Mterm_int | Mterm_real | Mint | Mreal -> Cterm

let pmode = function 
  | Mpositive -> Positive 
  | Mnegative -> Negative 
  | Mterm | Mterm_int | Mterm_real | Mint | Mreal -> Boolean

let amode = function 
  | Mpositive | Mnegative | Mterm | Mterm_int | Mint -> Aint 
  | Mterm_real | Mreal -> Areal

let smode = function
  | Sprop -> Mpositive
  | Sint -> Mterm_int
  | Sreal -> Mterm_real
  | Sbool | Sarray _ | Sdata -> Mterm

let tmode = function
  | Prop -> Mpositive
  | Bool -> Mterm
  | Int -> Mterm_int
  | Real -> Mterm_real
  | Tvar _ | Array _ | Record _ | Data _ -> Mterm

let ctau = function
  | Prop -> Cprop
  | _ -> Cterm

let declare_name = function
  | F_call f -> f
  | _ -> assert false (** Only normal function call F_call can be declared *)

let debug = function
  | F_call f | F_left f | F_right f | F_bool_prop(_,f) | F_subst f | F_assoc f -> f

module Make(T : Term) =
struct
  open T

  (* -------------------------------------------------------------------------- *)
  (* --- Linkers                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  module ADT = T.ADT
  module Field = T.Field
  module Fun = T.Fun

  type tau = (Field.t,ADT.t) datatype
  type var = Var.t
  type term = T.term
  type record = (Field.t * term) list
  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  module Mvar   = Map.Make(Var)
  module Ladt   = Link(ADT)
  module Lfield = Link(Field)
  module Lfun   = Link(Fun)
  module Lvar   = Link(Var)
  module STerm  = Link
      (struct
        type t = term
        let hash = T.hash
        let equal = T.equal
        let compare = T.compare
        let pretty = T.pretty
        let debug t = Printf.sprintf "E%03d" (T.id t)
      end)

  (* -------------------------------------------------------------------------- *)
  (* --- Pretty Printing Engine                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module TauMap = Map.Make
      (struct
        type t = T.tau
        let compare = Kind.compare_tau Field.compare ADT.compare
      end)

  let add_var x vars =
    let tx = T.tau_of_var x in
    let xs = try TauMap.find tx vars with Not_found -> [] in
    TauMap.add tx (x::xs) vars

  let rec binders q xs p =
    match T.repr p with
    | Bind(q',y,p') when q'=q -> binders q (add_var y xs) p'
    | _ -> xs,p

  let rec lambda xs p =
    match T.repr p with
    | Bind(Lambda,y,p') -> lambda (y::xs) p'
    | _ -> List.rev xs , p

  let rec has_prop_form link e = match T.repr e with
    | Eq _ | Neq _ | Leq _ | Lt _ | Imply _ | And _ | Or _ | If _
    | Bind((Forall|Exists),_,_) | True | False -> true
    | Not a -> has_prop_form link a
    | Fun(f,_) ->
        begin match link f with
          | F_bool_prop _ -> true
          | _ -> T.Fun.sort f = Sprop
        end
    | _ -> false

  class virtual engine =
    object(self)

      method virtual datatype : ADT.t -> string
      method virtual field : Field.t -> string
      method basename : string -> string = fun x -> x

      val mutable global = allocator () 
      val mutable vars = Vars.empty

      method declare = Linker.declare global
      method declare_all = List.iter (Linker.declare global)

      val linker_variable  = Lvar.linker ()
      val linker_shared = STerm.linker ()

      method private push =
        let gstack = global in
        begin
          global <- copy global ;
          linker_variable#alloc_with global ;
          linker_shared#alloc_with global ;
          gstack , linker_variable#push , linker_shared#push
        end

      method private pop (gstack,idx_var,idx_shared) =
        begin
          global <- gstack ;
          linker_variable#alloc_with gstack ;
          linker_variable#pop idx_var ;
          linker_shared#alloc_with gstack ;
          linker_shared#pop idx_shared ;
        end

      method local (job : unit -> unit) =
        let gstack = self#push in
        try job () ; self#pop gstack
        with err -> self#pop gstack ; raise err

      method global (job : unit -> unit) =
        let gstack = self#push in
        try
          linker_variable#clear ;
          linker_shared#clear ;
          vars <- Vars.empty ;
          job () ; 
          self#pop gstack
        with err -> 
            self#pop gstack ; 
            raise err

      (* -------------------------------------------------------------------------- *)
      (* --- Types                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual t_int  : string
      method virtual t_real : string
      method virtual t_bool : string
      method virtual t_prop : string
      method virtual t_atomic : tau -> bool
      method virtual pp_tvar : int printer
      method virtual pp_array : tau printer
      method virtual pp_farray : tau printer2
      method virtual pp_datatype : ADT.t -> tau list printer

      method pp_subtau fmt t = 
        if self#t_atomic t
        then self#pp_tau fmt t
        else fprintf fmt "@[<hov 1>(%a)@]" self#pp_tau t

      method pp_tau fmt = function
        | Int  -> pp_print_string fmt self#t_int
        | Real -> pp_print_string fmt self#t_real
        | Bool -> pp_print_string fmt self#t_bool
        | Prop -> pp_print_string fmt self#t_prop
        | Array(Int,d) -> self#pp_array fmt d
        | Array(k,d) -> self#pp_farray fmt k d
        | Record _fts -> failwith "Qed.Export.record"
        | Tvar x -> self#pp_tvar fmt x
        | Data(adt,ts) -> self#pp_datatype adt fmt ts

      (* -------------------------------------------------------------------------- *)
      (* --- Mode                                                               --- *)
      (* -------------------------------------------------------------------------- *)

      val mutable mode = Mpositive
      method mode = mode
      method with_mode m f = 
        let m0 = mode in 
        if m = m0 then f m 
        else
          try mode <- m ; f m0 ; mode <- m0
          with err -> mode <- m0 ; raise err

      (* -------------------------------------------------------------------------- *)
      (* --- Variables                                                          --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_var = linker_variable#print

      (* -------------------------------------------------------------------------- *)
      (* --- Atoms                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual e_true : cmode -> string
      method virtual e_false : cmode -> string
      method virtual pp_int : amode -> Z.t printer
      method virtual pp_cst : Numbers.cst printer
      method virtual is_atomic : term -> bool

      method pp_real fmt x =
        let cst = Numbers.parse (R.to_string x) in
        if Numbers.is_zero cst 
        then self#pp_int Areal fmt Z.zero
        else self#pp_cst fmt cst

      (* -------------------------------------------------------------------------- *)
      (* --- Calls                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual op_spaced : string -> bool
      method virtual callstyle : callstyle
      method virtual link : Fun.t -> link

      method private pp_call ~f fmt xs =
        match self#callstyle with
        | CallVar -> Plib.pp_call_var ~f self#pp_flow fmt xs
        | CallVoid -> Plib.pp_call_void ~f self#pp_flow fmt xs
        | CallApply -> Plib.pp_call_apply ~f self#pp_atom fmt xs

      method private pp_callsorts ~f fmt sorts xs =
        let pp_mode pp fmt (m,x) = self#with_mode m (fun _ -> pp fmt x) in
        let rec wrap sorts xs = match sorts , xs with
          | [] , _ -> List.map (fun x -> Mterm,x) xs
          | _ , [] -> []
          | m::ms , x::xs -> (smode m,x)::(wrap ms xs) in
        let mxs = wrap sorts xs in
        match self#callstyle with
        | CallVar -> Plib.pp_call_var ~f (pp_mode self#pp_flow) fmt mxs
        | CallVoid -> Plib.pp_call_void ~f (pp_mode self#pp_flow) fmt mxs
        | CallApply -> Plib.pp_call_apply ~f (pp_mode self#pp_atom) fmt mxs

      method private pp_unop ~op fmt x =
        match op with
        | Assoc op | Op op ->
            if self#op_spaced op && self#is_atomic x then
              fprintf fmt "%s %a" op self#pp_flow x
            else
              fprintf fmt "%s%a" op self#pp_atom x
        | Call f -> self#pp_call f fmt [x]

      method private pp_binop ~op fmt x y =
        match op with
        | Assoc op | Op op -> 
            fprintf fmt "%a %s@ %a" self#pp_atom x op self#pp_atom y
        | Call f -> self#pp_call f fmt [x;y]

      method private pp_binop_term ~op fmt x y =
        self#with_mode Mterm (fun _old -> self#pp_binop ~op fmt x y)

      method private pp_nary ~op fmt xs =
        match op with
        | Assoc op -> Plib.pp_assoc ~op self#pp_atom fmt xs
        | Op op -> Plib.pp_fold_binop ~op self#pp_atom fmt xs
        | Call f -> 
            match self#callstyle with
            | CallVar | CallVoid -> 
                Plib.pp_fold_call  ~f self#pp_flow fmt xs
            | CallApply -> 
                Plib.pp_fold_apply ~f self#pp_atom fmt xs

      method pp_fun cmode fct fmt xs = 
        match self#link fct, cmode with
        | F_call f, _
        | F_bool_prop (f,_), Cterm
        | F_bool_prop (_,f), Cprop ->
            self#pp_callsorts ~f fmt (Fun.params fct) xs
        | F_assoc op, _ -> Plib.pp_assoc ~e:"?" ~op self#pp_atom fmt xs
        | F_left f, _ ->
            begin
              match self#callstyle with
              | CallVar | CallVoid -> 
                  Plib.pp_fold_call ~f self#pp_flow fmt xs
              | CallApply ->
                  Plib.pp_fold_apply ~f self#pp_atom fmt xs
            end
        | F_right f, _ ->
            begin
              let xs = List.rev xs in
              match self#callstyle with
              | CallVar | CallVoid -> 
                  Plib.pp_fold_call_rev ~f self#pp_flow fmt xs
              | CallApply ->
                  Plib.pp_fold_apply_rev ~f self#pp_atom fmt xs
            end
        | F_subst s, _ ->
            let print = match self#callstyle with
              | CallVar | CallVoid -> self#pp_flow
              | CallApply -> self#pp_atom in
            Plib.substitute_list print s fmt xs

      method virtual pp_apply : cmode -> term -> term list printer

      (* -------------------------------------------------------------------------- *)
      (* --- Arithmetics Operators                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual op_scope : amode -> string option
      method virtual op_real_of_int : op
      method virtual op_add : amode -> op
      method virtual op_sub : amode -> op
      method virtual op_mul : amode -> op
      method virtual op_div : amode -> op
      method virtual op_mod : amode -> op
      method virtual op_minus : amode -> op

      (* -------------------------------------------------------------------------- *)
      (* --- Comparisons                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual op_equal : cmode -> op
      method virtual op_noteq : cmode -> op
      method virtual op_eq  : cmode -> amode -> op
      method virtual op_neq : cmode -> amode -> op
      method virtual op_lt  : cmode -> amode -> op
      method virtual op_leq : cmode -> amode -> op

      (* -------------------------------------------------------------------------- *)
      (* --- Arithmetics Printers                                               --- *)
      (* -------------------------------------------------------------------------- *)

      method private pp_arith_arg flow fmt e =
        match T.repr e with
        | Kint _ | Kreal _ -> self#pp_atom fmt e
        | _ -> self#pp_arith_atom flow fmt e

      method private pp_arith_atom flow fmt e =
        if mode = Mreal && T.is_int e then
          self#with_mode Mint
            (fun _ -> 
               match self#op_real_of_int with
               | Op op | Assoc op -> 
                   begin
                     match flow with
                     | Atom -> fprintf fmt "(%s %a)" op self#pp_atom e
                     | Flow -> fprintf fmt "%s %a" op self#pp_atom e
                   end
               | Call f ->
                   begin
                     match self#callstyle with
                     | CallVar | CallVoid -> 
                         fprintf fmt "%s(%a)" f self#pp_flow e
                     | CallApply -> 
                         match flow with
                         | Atom -> fprintf fmt "(%s %a)" f self#pp_atom e
                         | Flow -> fprintf fmt "%s %a" f self#pp_atom e
                   end)
        else match flow with
          | Flow -> self#pp_flow fmt e
          | Atom -> self#pp_atom fmt e

      method private pp_arith_call ~f fmt xs =
        match self#callstyle with
        | CallVar -> Plib.pp_call_var ~f (self#pp_arith_arg Flow) fmt xs
        | CallVoid -> Plib.pp_call_void ~f (self#pp_arith_arg Flow) fmt xs
        | CallApply -> Plib.pp_call_apply ~f (self#pp_arith_arg Atom) fmt xs

      method private pp_arith_unop ~phi fmt a =
        self#with_mode
          (if T.is_real a then Mreal else Mint)
          begin fun _ ->
            match phi (amode mode) with
            | Assoc op | Op op ->
                if self#op_spaced op && self#is_atomic a then
                  fprintf fmt "%s %a" op (self#pp_arith_arg Atom) a
                else
                  fprintf fmt "%s%a" op (self#pp_arith_arg Atom) a
            | Call f -> self#pp_arith_call ~f fmt [a]
          end

      method private pp_arith_binop ~phi fmt a b =
        self#with_mode
          (if T.is_real a || T.is_real b then Mreal else Mint)
          begin fun _ ->
            match phi (amode mode) with
            | Assoc op | Op op -> 
                Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b
            | Call f -> self#pp_arith_call ~f fmt [a;b]
          end

      method private pp_arith_nary ~phi fmt xs =
        self#with_mode
          (if List.exists T.is_real xs then Mreal else Mint)
          begin fun _ ->
            match phi (amode mode) with
            | Assoc op -> Plib.pp_assoc ~e:"?" ~op (self#pp_arith_arg Atom) fmt xs
            | Op op -> Plib.pp_fold_binop ~e:"?" ~op (self#pp_arith_arg Atom) fmt xs
            | Call f ->
                match self#callstyle with
                | CallVar | CallVoid ->
                    Plib.pp_fold_call ~e:"?" ~f (self#pp_arith_arg Flow) fmt xs
                | CallApply ->
                    Plib.pp_fold_apply ~e:"?" ~f (self#pp_arith_arg Atom) fmt xs
          end

      method private pp_arith_cmp ~phi fmt a b =
        let is_real = T.is_real a || T.is_real b in
        let amode = if is_real then Areal else Aint in
        let gmode = if is_real then Mreal else Mint in
        match phi (cmode mode) amode with
        | Assoc op | Op op -> 
            self#with_mode gmode 
              (fun emode ->
                 let scope = 
                   match emode with
                   | Mpositive | Mnegative 
                   | Mterm | Mterm_int | Mterm_real -> self#op_scope amode
                   | Mint | Mreal -> None
                 in match scope with
                 | None ->
                     begin
                       fprintf fmt "@[<hov 2>" ;
                       Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b ;
                       fprintf fmt "@]" ;
                     end
                 | Some s ->
                     begin
                       fprintf fmt "@[<hov 1>(" ;
                       Plib.pp_binop op (self#pp_arith_arg Atom) fmt a b ;
                       fprintf fmt ")%s@]" s ;
                     end)
        | Call f ->
            begin
              fprintf fmt "@[<hov 2>" ;
              self#with_mode gmode 
                (fun _ -> self#pp_arith_call ~f fmt [a;b]) ;
              fprintf fmt "@]" ;
            end

      method pp_times fmt k e =
        if Z.equal k Z.minus_one 
        then self#pp_arith_unop ~phi:(self#op_minus) fmt e
        else self#pp_arith_binop ~phi:(self#op_mul) fmt (T.e_zint k) e

      (* -------------------------------------------------------------------------- *)
      (* --- Arrays                                                             --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual pp_array_get : formatter -> term -> term -> unit
      method virtual pp_array_set : formatter -> term -> term -> term -> unit

      (* -------------------------------------------------------------------------- *)
      (* --- Records                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual pp_get_field : formatter -> term -> Field.t -> unit
      method virtual pp_def_fields : record printer

      (* -------------------------------------------------------------------------- *)
      (* --- Logical Connectives                                                --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual op_not   : cmode -> op
      method virtual op_and   : cmode -> op
      method virtual op_or    : cmode -> op
      method virtual op_imply : cmode -> op
      method virtual op_equiv : cmode -> op

      (* -------------------------------------------------------------------------- *)
      (* --- Polarity                                                           --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_not fmt p =
        let pp = self#pp_unop ~op:(self#op_not (cmode mode)) in
        match mode with
        | Mpositive -> mode <- Mnegative ; pp fmt p ; mode <- Mpositive
        | Mnegative -> mode <- Mpositive ; pp fmt p ; mode <- Mnegative
        | _ -> pp fmt p

      method private pp_polarity pp fmt (inv,x) =
        match mode with
        | Mpositive when inv -> mode <- Mnegative ; pp fmt x ; mode <- Mpositive
        | Mnegative when inv -> mode <- Mpositive ; pp fmt x ; mode <- Mnegative
        | _ -> pp fmt x

      method pp_imply fmt hs p =
        let op = self#op_imply (cmode mode) in
        let pp_atom = self#pp_polarity self#pp_atom in
        let pp_flow = self#pp_polarity self#pp_flow in
        let xs = List.map (fun h -> true,h) hs @ [false,p] in
        match op with
        | Assoc op -> Plib.pp_assoc ~e:"?" ~op pp_atom fmt xs
        | Op op -> Plib.pp_fold_binop ~e:"?" ~op pp_atom fmt xs
        | Call f -> 
            match self#callstyle with
            | CallVar | CallVoid -> 
                Plib.pp_fold_call ~e:"?" ~f pp_flow fmt xs
            | CallApply -> 
                Plib.pp_fold_apply ~e:"?" ~f pp_atom fmt xs

      (* -------------------------------------------------------------------------- *)
      (* --- Equality                                                           --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_equal fmt a b =
        let cm = cmode mode in
        match Kind.merge (T.sort a) (T.sort b) with
        | Sprop | Sbool -> self#pp_binop ~op:(self#op_equiv cm) fmt a b
        | Sdata | Sarray _ -> self#pp_binop_term ~op:(self#op_equal cm) fmt a b
        | Sint  | Sreal    -> self#pp_arith_cmp ~phi:(self#op_eq) fmt a b

      method pp_noteq fmt a b =
        let cm = cmode mode in
        match Kind.merge (T.sort a) (T.sort b) with
        | Sprop | Sbool    -> self#pp_unop ~op:(self#op_not cm) fmt (T.e_equiv a b)
        | Sdata | Sarray _ -> self#pp_binop_term ~op:(self#op_noteq cm) fmt a b
        | Sint  | Sreal    -> self#pp_arith_cmp ~phi:(self#op_neq) fmt a b

      (* -------------------------------------------------------------------------- *)
      (* --- Conditional                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual pp_conditional : formatter -> term -> term -> term -> unit

      (* -------------------------------------------------------------------------- *)
      (* --- Quantifiers                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual pp_forall : tau -> var list printer
      method virtual pp_exists : tau -> var list printer
      method virtual pp_lambda : var list printer

      method private pp_binders fmt p =
        match T.repr p with

        | Bind(Lambda,x,p) ->
            let xs,p = lambda [x] p in
            List.iter self#bind xs ;
            self#pp_lambda fmt xs ;
            self#pp_binders fmt p 

        | Bind((Forall|Exists) as q,x,p) -> 
            let vars,p = binders q (add_var x TauMap.empty) p in
            TauMap.iter
              (fun t xs ->
                 List.iter self#bind xs ;
                 let xs = List.sort Var.compare xs in
                 match q with
                 | Forall -> fprintf fmt "%a@ " (self#pp_forall t) xs
                 | Exists -> fprintf fmt "%a@ " (self#pp_exists t) xs
                 | Lambda -> assert false 
              ) vars ;
            self#pp_binders fmt p

        | _ -> self#pp_shared fmt p

      (* -------------------------------------------------------------------------- *)
      (* --- Sharing                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method bind x =
        let basename = self#basename (T.base_of_var x) in
        ignore (linker_variable#alloc ~basename x) ;
        vars <- Vars.add x vars

      method is_shareable e =
        match T.repr e with
        | Kint _ | Kreal _ | True | False -> false
        | Times _ | Add _ | Mul _ | Div _ | Mod _ -> true
        | Eq _ | Neq _ | Leq _ | Lt _ -> false
        | Aget _ | Aset _ | Rget _ | Rdef _ -> true
        | And _ | Or _ | Not _ | Imply _ | If _ -> false
        | Fun _ -> not (T.is_prop e)
        | Var _	| Apply _ | Bind _ -> false

      method virtual pp_let : Format.formatter -> pmode -> string -> term -> unit

      method private pp_lets fmt xes e =
        begin
          let m0 = mode in
          let p0 = pmode m0 in
          List.iter 
            (fun (x,e) ->
               mode <- Mterm ;
               self#pp_let fmt p0 x e ;
               linker_shared#bind_reserved e x ;
            ) xes ;
          mode <- m0 ;
          self#pp_flow fmt e ;
        end

      method private pp_shared fmt e =
        let shared e = linker_shared#mem e in
        let shareable e = self#is_shareable e in
        let es = T.shared ~shareable ~shared ~closed:vars [e] in
        if es <> [] then
          self#local 
            (fun () ->
               let xes =
                 List.map
                   (fun e -> 
                      let basename = self#basename (T.basename e) in
                      let var = linker_shared#reserve ~basename in
                      var , e
                   ) es 
               in self#pp_lets fmt xes e)
        else 
          self#pp_flow fmt e

      (* -------------------------------------------------------------------------- *)
      (* --- Expressions                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method private op_scope_for e =
        match mode with
        | (Mpositive | Mnegative | Mterm) when T.is_int e -> self#op_scope Aint
        | (Mpositive | Mnegative | Mterm) when T.is_real e -> self#op_scope Areal
        | Mterm_int -> self#op_scope Aint
        | Mterm_real -> self#op_scope Areal
        | _ -> None

      method private pp_bool pp fmt e =
        if cmode mode = Cprop && not (has_prop_form self#link e) then
          match T.repr e with
          | Not a -> fprintf fmt "(%a=%s)" self#pp_do_atom a (self#e_false Cterm)
          | _ -> fprintf fmt "(%a=%s)" self#pp_do_atom e (self#e_true Cterm)
        else pp fmt e
            
      method pp_atom fmt e = self#pp_bool self#pp_do_atom fmt e
      method pp_flow fmt e = self#pp_bool self#pp_do_flow fmt e

      method private pp_do_atom fmt e =
        try pp_print_string fmt (linker_shared#find e)
        with Not_found ->
          if self#is_atomic e
          then self#pp_repr fmt e
          else fprintf fmt "@[<hov 1>(%a)@]" self#pp_repr e ;
          match self#op_scope_for e with
          | None -> ()
          | Some s -> pp_print_string fmt s
                        
      method private pp_do_flow fmt e =
        try pp_print_string fmt (linker_shared#find e)
        with Not_found -> 
          match self#op_scope_for e with
          | None -> self#pp_repr fmt e
          | Some s -> fprintf fmt "@[<hov 1>(%a)%s@]" self#pp_repr e s
                        
      method private pp_addition fmt xs =
        let amode = if List.exists T.is_real xs then Areal else Aint in
        match 
          self#op_add amode , 
          self#op_sub amode , 
          self#op_minus amode 
        with
        | Assoc add , Assoc sub , Op minus ->
            let factor x = match T.repr x with
              | Kint z when Z.lt z Z.zero-> (false,T.e_zint (Z.neg z))
              | Kreal r when R.negative r -> (false,T.e_real (R.opp r))
              | Times(k,y) when Z.lt k Z.zero -> (false,T.e_times (Z.neg k) y)
              | _ -> (true,x) in
            let sxs = List.map factor xs in
            let sxs = List.stable_sort
                (fun (s1,e1) (s2,e2) ->
                   match s1,s2 with
                   | true,true | false,false ->
                       Pervasives.compare (T.weigth e1) (T.weigth e2)
                   | true,false -> (-1)
                   | false,true -> 1
                ) sxs in
            Plib.iteri
              (fun i (s,x) ->
                 begin
                   match i , s with
                   | (Ifirst | Isingle) , false ->
                       if self#op_spaced minus && self#is_atomic x
                       then fprintf fmt "%s " minus
                       else pp_print_string fmt minus
                   | (Ifirst | Isingle) , true -> ()
                   | (Imiddle | Ilast) , true -> fprintf fmt "@ %s " add
                   | (Imiddle | Ilast) , false -> fprintf fmt "@ %s " sub
                 end ;
                 self#pp_arith_arg Atom fmt x
              ) sxs
        | _ -> self#pp_arith_nary ~phi:(self#op_add) fmt xs

      method private pp_repr fmt e =
        match T.repr e with
        | True -> pp_print_string fmt (self#e_true (cmode mode))
        | False -> pp_print_string fmt (self#e_false (cmode mode))
        | Var x -> self#pp_var fmt x
        | Not p -> self#pp_not fmt p
        | Kint x -> self#pp_int (amode mode) fmt x
        | Kreal x -> self#pp_real fmt x
        | Add xs -> self#pp_addition fmt xs
        | Mul xs -> self#pp_arith_nary ~phi:(self#op_mul) fmt xs
        | Div(a,b) -> self#pp_arith_binop ~phi:(self#op_div) fmt a b
        | Mod(a,b) -> self#pp_arith_binop ~phi:(self#op_mod) fmt a b
        | Times(k,a) -> self#pp_times fmt k a
        | Eq(a,b)  -> self#pp_equal fmt a b
        | Neq(a,b) -> self#pp_noteq fmt a b
        | Lt(a,b)  -> self#pp_arith_cmp ~phi:(self#op_lt) fmt a b
        | Leq(a,b) -> self#pp_arith_cmp ~phi:(self#op_leq) fmt a b
        | Aget(a,k) -> self#with_mode Mterm (fun _ -> self#pp_array_get fmt a k)
        | Aset(a,k,v) -> self#with_mode Mterm (fun _ -> self#pp_array_set fmt a k v)
        | Rget(r,f) -> self#with_mode Mterm (fun _ -> self#pp_get_field fmt r f)
        | Rdef fts -> self#with_mode Mterm (fun _ -> self#pp_def_fields fmt fts)
        | If(a,b,c) -> self#pp_conditional fmt a b c
        | And ts -> self#pp_nary ~op:(self#op_and (cmode mode)) fmt ts
        | Or ts -> self#pp_nary ~op:(self#op_or (cmode mode)) fmt ts
        | Imply(hs,p) -> self#pp_imply fmt hs p
        | Apply(e,es) -> self#with_mode Mterm (fun em -> self#pp_apply (cmode em) e fmt es)
        | Fun(f,ts) -> self#with_mode Mterm (fun em -> self#pp_fun (cmode em) f fmt ts)
        | Bind _ -> self#local (fun () -> self#pp_binders fmt e)

      (* -------------------------------------------------------------------------- *)
      (* --- Formulae                                                           --- *)
      (* -------------------------------------------------------------------------- *)

      method private pp_expr_mode m fmt e =
        mode <- m ; self#pp_shared fmt e

      method pp_term = self#pp_expr_mode Mterm
      method pp_prop = self#pp_expr_mode Mpositive
      method pp_expr (tau:tau) = self#pp_expr_mode (tmode tau)

    end

end
