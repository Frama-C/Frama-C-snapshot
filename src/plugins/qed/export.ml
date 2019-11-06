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
(* --- Exportation to Foreign Languages                                   --- *)
(* -------------------------------------------------------------------------- *)

open Format
open Logic
open Plib
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

let link_name = function
  | F_call f -> f
  | _ -> assert false (** Only normal function call F_call can be declared *)

let debug = function
  | F_call f | F_left f | F_right f | F_bool_prop(_,f)
  | F_list(f,_) | F_subst f | F_assoc f -> f

(* -------------------------------------------------------------------------- *)
(* --- Identifiers                                                        --- *)
(* -------------------------------------------------------------------------- *)

let is_letter = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_identifier op =
  try
    for i = 0 to String.length op - 1 do
      if not (is_letter op.[i]) then raise Exit
    done ; true
  with Exit -> false

let sanitize ~to_lowercase base =
  let p = Buffer.create 32 in
  for i=0 to String.length base - 1 do
    let c = base.[i] in
    match c with
    | '0' .. '9' | 'a' .. 'z' | '_' ->
        Buffer.add_char p c
    | 'A' .. 'Z' ->
        Buffer.add_char p
          (if to_lowercase then Char.lowercase_ascii c else c)
    | _ -> ()
  done ;
  Buffer.contents p

(* -------------------------------------------------------------------------- *)
(* --- Generic Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

module Make(T : Term) =
struct
  open T

  type trigger = (var,Fun.t) ftrigger
  type typedef = (tau,Field.t,Fun.t) ftypedef

  (* -------------------------------------------------------------------------- *)
  (* --- Allocator                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  module VarMap = Map.Make(T.Var)
  module Ident = Map.Make(String)
  module Users = Set.Make(String)

  type index = {
    mutable short : bool ;
    mutable index : int Ident.t ;
    mutable fvars : string VarMap.t ;
    mutable bvars : string Intmap.t ;
    mutable share : string Tmap.t ;
    mutable unzip : Tset.t ;
    mutable users : Users.t ;
  }

  let create_index () = {
    short = true ;
    index = Ident.empty ;
    fvars = VarMap.empty ;
    bvars = Intmap.empty ;
    share = Tmap.empty ;
    unzip = Tset.empty ;
    users = Users.empty ;
  }

  let clear_index lnk =
    begin
      lnk.index <- Ident.empty ;
      lnk.fvars <- VarMap.empty ;
      lnk.bvars <- Intmap.empty ;
      lnk.share <- Tmap.empty ;
      lnk.unzip <- Tset.empty ;
      lnk.users <- Users.empty ;
    end

  let copy_index lnk = {
    short = lnk.short ;
    index = lnk.index ;
    fvars = lnk.fvars ;
    bvars = lnk.bvars ;
    share = lnk.share ;
    unzip = lnk.unzip ;
    users = lnk.users ;
  }

  let rec find_fresh index sanitizer ~suggest basename k =
    let x =
      if k=0 && index.short && String.length basename = 1 then basename
      else Printf.sprintf "%s_%d" basename k in
    if Users.mem x index.users then
      find_fresh index sanitizer ~suggest basename (succ k)
    else
      ( if not suggest then
          index.index <- Ident.add basename (succ k) index.index
      ; x )

  let fresh index sanitizer ?(suggest=false) basename =
    let basename = sanitizer basename in
    let k = try Ident.find basename index.index with Not_found -> 0 in
    find_fresh index sanitizer ~suggest basename k

  let bind_bvar k t index sanitizer =
    let x = fresh index sanitizer (Tau.basename t) in
    index.bvars <- Intmap.add k x index.bvars ; x

  let find_bvar k index =
    try Intmap.find k index.bvars
    with Not_found -> assert false

  let bind_fvar v index sanitizer =
    let x = fresh index sanitizer (Var.basename v) in
    index.fvars <- VarMap.add v x index.fvars ; x

  let find_fvar v index =
    try VarMap.find v index.fvars
    with Not_found ->
      Plib.sprintf "#{%a}" Var.pretty v

  let bind_term x t index =
    begin
      index.users <- Users.add x index.users ;
      index.share <- Tmap.add t x index.share ;
    end

  let unbind_term t index =
    begin
      (try
         let x = Tmap.find t index.share in
         index.users <- Users.remove x index.users ;
         index.share <- Tmap.remove t index.share ;
       with Not_found -> ()) ;
      index.unzip <- Tset.add t index.unzip ;
    end

  module Env =
  struct
    type t = index
    let create = create_index
    let copy index = copy_index index
    let clear index = clear_index index
    let used index name = Users.mem name index.users
    let fresh index ~sanitizer ?(suggest=false) basename =
      fresh index sanitizer ~suggest basename
    let define index x t = bind_term x t index
    let unfold index t = unbind_term t index
    let lookup index t =
      try `Defined(Tmap.find t index.share)
      with Not_found ->
        if Tset.mem t index.unzip then `Unfolded else `Auto
    let shared index t = Tmap.mem t index.share
    let shareable index t = not (Tset.mem t index.unzip)
    let set_indexed_vars index = index.short <- false
    let iter phi index = Tmap.iter (fun t x -> phi x t) index.share
  end

  (* -------------------------------------------------------------------------- *)
  (* --- Binders                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  module TauMap = Map.Make(T.Tau)

  let add_var k t vars =
    let ks = try TauMap.find t vars with Not_found -> [] in
    TauMap.add t (k::ks) vars

  let rec binders q k vars e =
    match T.repr e with
    | Bind(q',t,e) when q'=q ->
        binders q (succ k) (add_var k t vars) (lc_repr e)
    | _ -> k,vars,e

  let rec lambda k kts e =
    match T.repr e with
    | Bind(Lambda,t,e) -> lambda (succ k) ((k,t)::kts) (lc_repr e)
    | _ -> k,List.rev kts,e

  let rec has_prop_form link e = match T.repr e with
    | Eq _ | Neq _ | Leq _ | Lt _ | Imply _ | And _ | Or _ | If _
    | Bind((Forall|Exists),_,_) | True | False -> true
    | Not a -> has_prop_form link a
    | Fun(f,_) ->
        begin
          match link f with
          | F_bool_prop _ -> true
          | _ -> T.Fun.sort f = Sprop
        end
    | _ -> false

  (* -------------------------------------------------------------------------- *)
  (* --- Engine                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  class virtual engine =
    object(self)

      val mutable index = create_index ()

      method sanitize = sanitize ~to_lowercase:false
      method virtual datatype : ADT.t -> string
      method virtual field : Field.t -> string

      method lookup t : scope = Env.lookup index t

      method env = copy_index index
      method set_env env = index <- env
      method marks =
        let env = index (* NOT a fresh copy *) in
        let shared = Env.shared env in
        let shareable e = self#shareable e && Env.shareable env e in
        let subterms = self#subterms in
        let marks = T.marks ~shared ~shareable ~subterms () in
        env , marks

      method scope env (job : unit -> unit) =
        let stack = index in
        index <- env ;
        try job () ; index <- stack
        with err -> index <- stack ; raise err

      method local (job : unit -> unit) =
        self#scope (copy_index index) job

      method global (job : unit -> unit) =
        self#scope (create_index ()) job

      method bind v = bind_fvar v index self#sanitize
      method find v = VarMap.find v index.fvars

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

      method pp_var = Format.pp_print_string

      (* -------------------------------------------------------------------------- *)
      (* --- Atoms                                                              --- *)
      (* -------------------------------------------------------------------------- *)

      method virtual e_true : cmode -> string
      method virtual e_false : cmode -> string
      method virtual pp_int : amode -> Z.t printer
      method virtual pp_real : Q.t printer
      method virtual is_atomic : term -> bool

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
            if self#op_spaced op (*&& self#is_atomic x*) then
              fprintf fmt "%s %a" op self#pp_flow x
            else
              fprintf fmt "%s%a" op self#pp_atom x
        | Call f -> self#pp_call ~f fmt [x]

      method private pp_binop ~op fmt x y =
        match op with
        | Assoc op | Op op ->
            fprintf fmt "%a %s@ %a" self#pp_atom x op self#pp_atom y
        | Call f -> self#pp_call ~f fmt [x;y]

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
        | F_list(fc,fn), _ ->
            begin
              let rec plist w fmt xs =
                let style,fc,fn = w in
                match style , xs with
                | (CallVar|CallApply) , [] -> pp_print_string fmt fn
                | CallVoid , [] -> fprintf fmt "%s()" fn
                | (CallVar|CallVoid) , x::xs ->
                    fprintf fmt "@[<hov 2>%s(@,%a,@,%a)@]"
                      fc self#pp_flow x (plist w) xs
                | CallApply , x::xs ->
                    fprintf fmt "@[<hov 2>(%s@ %a @ %a)@]"
                      fc self#pp_atom x (plist w) xs
              in plist (self#callstyle,fc,fn) fmt xs
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
                if self#op_spaced op then
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
                Plib.pp_binop ~op (self#pp_arith_arg Atom) fmt a b
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
                       Plib.pp_binop ~op (self#pp_arith_arg Atom) fmt a b ;
                       fprintf fmt "@]" ;
                     end
                 | Some s ->
                     begin
                       fprintf fmt "@[<hov 1>(" ;
                       Plib.pp_binop ~op (self#pp_arith_arg Atom) fmt a b ;
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

      method virtual pp_array_cst : formatter -> tau -> term -> unit
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

      method virtual pp_forall : tau -> string list printer
      method virtual pp_exists : tau -> string list printer
      method virtual pp_lambda : (string * tau) list printer

      method private pp_binders fmt e =
        match T.repr e with

        | Bind(Lambda,t,e) ->
            let e = lc_repr e in
            let n,kts,e = lambda 1 [0,t] e in
            let last = Bvars.order (lc_vars e) + n - 1 in
            let binder (k,t) = bind_bvar (last-k) t index self#sanitize , t in
            let xts = List.map binder kts in
            self#pp_lambda fmt xts ;
            self#pp_binders fmt e

        | Bind((Forall|Exists) as q,t,e) ->
            let e = lc_repr e in
            let n,vars,e = binders q 1 (add_var 0 t TauMap.empty) e in
            let last = Bvars.order (lc_vars e) + n - 1 in
            TauMap.iter
              (fun t ks ->
                 let binder k = bind_bvar (last-k) t index self#sanitize in
                 let xs = List.fold_left (fun xs k -> binder k :: xs) [] ks in
                 match q with
                 | Forall -> fprintf fmt "%a@ " (self#pp_forall t) xs
                 | Exists -> fprintf fmt "%a@ " (self#pp_exists t) xs
                 | Lambda -> assert false
              ) vars ;
            self#pp_binders fmt e

        | _ ->
            self#pp_shared fmt e

      (* -------------------------------------------------------------------------- *)
      (* --- Sharing                                                            --- *)
      (* -------------------------------------------------------------------------- *)

      method shared (_ : term) = false

      method shareable e =
        match T.repr e with
        | Kint _ | Kreal _ | True | False -> false
        | Times _ | Add _ | Mul _ | Div _ | Mod _ -> true
        | Eq _ | Neq _ | Leq _ | Lt _ -> false
        | Acst _ -> false
        | Aget _ | Aset _ | Rget _ | Rdef _ -> true
        | And _ | Or _ | Not _ | Imply _ | If _ -> false
        | Fun _ -> not (T.is_prop e)
        | Bvar _ | Fvar _ | Apply _ | Bind _ -> false

      method subterms f e =
        match T.repr e with
        | Rdef fts ->
            begin
              match T.record_with fts with
              | None -> T.lc_iter f e
              | Some(a,fts) -> f a ; List.iter (fun (_,e) -> f e) fts
            end
        | _ -> T.lc_iter f e

      method virtual pp_let : Format.formatter -> pmode -> string -> term -> unit

      method private pp_shared fmt e =
        let shared e = Tmap.mem e index.share || self#shared e in
        let shareable e = self#shareable e && not (Tset.mem e index.unzip) in
        let subterms = self#subterms in
        let es = T.shared ~shareable ~shared ~subterms [e] in
        if es <> [] then
          self#local
            begin fun () ->
              let m0 = mode in
              let p0 = pmode m0 in
              List.iter
                (fun e ->
                   let x = fresh index self#sanitize (T.basename e) in
                   mode <- Mterm ;
                   self#pp_let fmt p0 x e ;
                   bind_term x e index ;
                ) es ;
              mode <- m0 ;
              self#pp_flow fmt e ;
            end
        else
          self#pp_flow fmt e

      (* -------------------------------------------------------------------------- *)
      (* --- Expressions                                                        --- *)
      (* -------------------------------------------------------------------------- *)

      method pp_atom fmt e = self#pp_bool self#pp_do_atom fmt e
      method pp_flow fmt e = self#pp_bool self#pp_do_flow fmt e

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

      method private pp_do_atom fmt e =
        try self#pp_var fmt (Tmap.find e index.share)
        with Not_found ->
          if self#is_atomic e
          then self#pp_repr fmt e
          else fprintf fmt "@[<hov 1>(%a)@]" self#pp_repr e ;
          match self#op_scope_for e with
          | None -> ()
          | Some s -> pp_print_string fmt s

      method private pp_do_flow fmt e =
        try self#pp_var fmt (Tmap.find e index.share)
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
              | Kreal r when Q.lt r Q.zero -> (false,T.e_real (Q.neg r))
              | Times(k,y) when Z.lt k Z.zero -> (false,T.e_times (Z.neg k) y)
              | _ -> (true,x) in
            let sxs = List.map factor xs in
            let sxs = List.stable_sort
                (fun (s1,e1) (s2,e2) ->
                   match s1,s2 with
                   | true,true | false,false ->
                       Transitioning.Stdlib.compare (T.weigth e1) (T.weigth e2)
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

      method pp_repr fmt e =
        match T.repr e with
        | True -> pp_print_string fmt (self#e_true (cmode mode))
        | False -> pp_print_string fmt (self#e_false (cmode mode))
        | Fvar x -> self#pp_var fmt (find_fvar x index)
        | Bvar(k,_) -> self#pp_var fmt (find_bvar k index)
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
        | Acst(t,v) -> self#with_mode Mterm (fun _ -> self#pp_array_cst fmt t v)
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
        self#with_mode m (fun _old -> self#pp_shared fmt e)

      method pp_term = self#pp_expr_mode Mterm
      method pp_prop = self#pp_expr_mode Mpositive
      method pp_expr (tau:tau) = self#pp_expr_mode (tmode tau)
      method pp_sort fmt e =
        let mode = match T.sort e with
          | Sprop -> Mpositive
          | Sbool when has_prop_form self#link e -> Mpositive
          | Sint -> Mterm_int
          | Sreal -> Mterm_real
          | Sbool | Sdata | Sarray _ -> Mterm
        in self#pp_expr_mode mode fmt e
    end

end
