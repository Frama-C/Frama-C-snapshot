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
(* --- Memory Model                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

module L = Qed.Logic

let library = "memory"

let a_addr = Lang.datatype ~library "addr"
let t_addr = L.Data(a_addr,[])
let f_base   = Lang.extern_f ~library ~result:L.Int
    ~link:{altergo = Qed.Engine.F_subst("%1.base");
           why3    = Qed.Engine.F_call "base";
           coq     = Qed.Engine.F_subst("(base %1)");
          } "base"
let f_offset = Lang.extern_f ~library ~result:L.Int
    ~link:{altergo = Qed.Engine.F_subst("%1.offset");
           why3    = Qed.Engine.F_call "offset";
           coq     = Qed.Engine.F_subst("(offset %1)");
          } "offset"
let f_shift  = Lang.extern_f ~library ~result:t_addr "shift"
let f_global = Lang.extern_f ~library ~result:t_addr ~category:L.Injection "global"
let f_null   = Lang.extern_f ~library ~result:t_addr "null"

let f_base_offset = Lang.extern_f ~library
    ~category:Qed.Logic.Injection ~result:L.Int "base_offset"

let ty_havoc = function
  | Some l :: _ -> l
  | _ -> raise Not_found

let l_havoc = Qed.Engine.{
    coq = F_call "fhavoc" ;
    altergo = F_call "havoc" ;
    why3 = F_call "havoc" ;
  }

let p_valid_rd = Lang.extern_fp ~library "valid_rd"
let p_valid_rw = Lang.extern_fp ~library "valid_rw"
let p_invalid = Lang.extern_fp ~library "invalid"
let p_separated = Lang.extern_fp ~library "separated"
let p_included = Lang.extern_fp ~library "included"
let p_eqmem = Lang.extern_fp ~library "eqmem"
let f_havoc = Lang.extern_f ~library ~typecheck:ty_havoc ~link:l_havoc "havoc"
let f_region = Lang.extern_f ~library ~result:L.Int "region" (* base -> region *)
let p_framed = Lang.extern_fp ~library "framed" (* m-pointer -> prop *)
let p_linked = Lang.extern_fp ~library "linked" (* allocation-table -> prop *)
let p_sconst = Lang.extern_fp ~library "sconst" (* int-memory -> prop *)
let p_addr_lt = Lang.extern_p ~library ~bool:"addr_lt_bool" ~prop:"addr_lt" ()
let p_addr_le = Lang.extern_p ~library ~bool:"addr_le_bool" ~prop:"addr_le" ()

let f_addr_of_int = Lang.extern_f
    ~category:L.Injection
    ~library ~result:t_addr "addr_of_int"

let f_int_of_addr = Lang.extern_f
    ~category:L.Injection
    ~library ~result:L.Int "int_of_addr"

(* -------------------------------------------------------------------------- *)
(* --- Utilities                                                          --- *)
(* -------------------------------------------------------------------------- *)

let t_mem t = L.Array(t_addr,t)
let t_malloc = L.Array(L.Int,L.Int)

let a_null = F.constant (e_fun f_null [])
let a_base p = e_fun f_base [p]
let a_offset p = e_fun f_offset [p]
let a_global b = e_fun f_global [b]
let a_shift l k = e_fun f_shift [l;k]
let a_addr b k = a_shift (a_global b) k
let a_base_offset k = e_fun f_base_offset [k]

(* -------------------------------------------------------------------------- *)
(* --- Qed Simplifiers                                                    --- *)
(* -------------------------------------------------------------------------- *)

(*
    Pointer arithmetic for structure access and array access could be
    defined directly using the record [{ base = p.base; offset = p.offset
    + c*i + c' }]. However that gives very bad triggers for the memory
    model axiomatization, so `shift p (c*i+c')` was used instead. It is
    not sufficient for user axiomatisation because memory access in
    axioms require trigger with arithmetic operators which is badly
    handled by provers. So for each c and c', ie for each kind of
    structure access and array access a specific function is used
    `shift_xxx`.

    Moreover no simplification of `shift_xxx` is done for keeping the
    same terms in axioms and the goal. `base` and `offset` function
    simplify all the `shift_xxx` because it seems they don't appear
    often in axioms and they are useful for simplifying `separated`,
    `assigns` and pointer comparisons in goals.

    To sum up memory access should match, but not `\base`, `\offset`,
    `\separated`, ...
*)

type addr_builtin = {
  base: term list -> term ;
  offset: term list -> term ;
}

module ADDR_BUILTIN = WpContext.Static
    (struct
      type key = lfun
      type data = addr_builtin
      let name = "MemMemory.ADDR_BUILTIN"
      include Lang.Fun
    end)

let phi_base l =
  match F.repr l with
  | L.Fun(f,[p;_]) when f==f_shift -> a_base p
  | L.Fun(f,[b]) when f==f_global -> b
  | L.Fun(f,[]) when f==f_null -> e_zero
  | L.Fun(f,args) -> (ADDR_BUILTIN.find f).base args
  | _ -> raise Not_found

let phi_offset l = match F.repr l with
  | L.Fun(f,[p;k]) when f==f_shift -> e_add (a_offset p) k
  | L.Fun(f,_) when f==f_global || f==f_null -> F.e_zero
  | L.Fun(f,args) -> (ADDR_BUILTIN.find f).offset args
  | _ -> raise Not_found

let phi_shift f p i =
  match F.repr p with
  | L.Fun(g,[q;j]) when f == g -> F.e_fun f [q;F.e_add i j]
  | _ -> raise Not_found

let eq_shift a b =
  let p = a_base a in
  let q = a_base b in
  let i = a_offset a in
  let j = a_offset b in
  if i==j then F.p_equal p q else
    match F.is_equal p q with
    | L.No -> F.p_false
    | L.Yes -> F.p_equal i j
    | L.Maybe -> raise Not_found

let eq_shift_gen phi a b =
  try phi a b with Not_found -> eq_shift a b

let nop _ = raise Not_found

let register ?(base=nop) ?(offset=nop) ?equal ?(linear=false) lfun =
  begin
    if base != nop || offset != nop then
      ADDR_BUILTIN.define lfun { base ; offset } ;
    if linear then
      F.set_builtin_2 lfun (phi_shift lfun) ;
    let phi_equal = match equal with
      | None -> eq_shift
      | Some phi -> eq_shift_gen phi
    in
    F.set_builtin_eqp lfun phi_equal ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'separated'                                         --- *)
(* -------------------------------------------------------------------------- *)

let r_separated = function
  | [p;a;q;b] ->
      if a == F.e_one && b == F.e_one then F.e_neq p q
      else
        begin
          let a_negative = F.e_leq a F.e_zero in
          let b_negative = F.e_leq b F.e_zero in
          if a_negative == e_true || b_negative == e_true then e_true else
            let bp = a_base p in
            let bq = a_base q in
            let open Qed.Logic in
            match F.is_true (F.e_eq bp bq) with
            | No -> e_true (* Have S *)
            | Yes when (a_negative == e_false && b_negative == e_false) ->
                (* Reduced to S *)
                let p_ofs = a_offset p in
                let q_ofs = a_offset q in
                let p_ofs' = F.e_add p_ofs a in
                let q_ofs' = F.e_add q_ofs b in
                F.e_or [ F.e_leq q_ofs' p_ofs ;
                         F.e_leq p_ofs' q_ofs ]
            | _ -> raise Not_found
        end
  | _ -> raise Not_found

let is_separated args = F.is_true (r_separated args)

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'included'                                          --- *)
(* -------------------------------------------------------------------------- *)

(*
logic a : int
logic b : int

predicate R =     p.base = q.base
              /\ (q.offset <= p.offset)
              /\ (p.offset + a <= q.offset + b)

predicate included = 0 < a -> ( 0 <= b and R )
predicate a_empty = a <= 0
predicate b_negative = b < 0

lemma SAME_P: p=q -> (R <-> a<=b)
lemma SAME_A: a=b -> (R <-> p=q)

goal INC_P:  p=q -> (included <-> ( 0 < a -> a <= b )) (by SAME_P)
goal INC_A:  a=b -> 0 < a -> (included <-> R) (by SAME_A)
goal INC_1:  a_empty -> (included <-> true)
goal INC_2:  b_negative -> (included <-> a_empty)
goal INC_3:  not R -> (included <-> a_empty)
goal INC_4:  not a_empty -> not b_negative -> (included <-> R)
*)

let r_included = function
  | [p;a;q;b] ->
      if F.e_eq p q == F.e_true
      then F.e_imply [F.e_lt F.e_zero a] (F.e_leq a b) (* INC_P *)
      else
      if (F.e_eq a b == F.e_true) && (F.e_lt F.e_zero a == F.e_true)
      then F.e_eq p q (* INC_A *)
      else
        begin
          let a_empty = F.e_leq a F.e_zero in
          let b_negative = F.e_lt b F.e_zero in
          if a_empty == F.e_true then F.e_true (* INC_1 *) else
          if b_negative == F.e_true then a_empty (* INC_2 *) else
            let bp = a_base p in
            let bq = a_base q in
            let open Qed.Logic in
            match F.is_true (F.e_eq bp bq) with
            | No -> a_empty (* INC_3 *)
            | Yes when (a_empty == e_false && b_negative == e_false) ->
                (* INC_4 *)
                let p_ofs = a_offset p in
                let q_ofs = a_offset q in
                if a == b then F.e_eq p_ofs q_ofs
                else
                  let p_ofs' = e_add p_ofs a in
                  let q_ofs' = e_add q_ofs b in
                  e_and [ F.e_leq q_ofs p_ofs ; F.e_leq p_ofs' q_ofs' ]
            | _ -> raise Not_found
        end
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for 'havoc'                                             --- *)
(* -------------------------------------------------------------------------- *)

(* havoc(m_undef, havoc(_undef,m0,p0,a0), p1,a1) =
   - havoc(m_undef, m0, p1,a1) WHEN included (p1,a1,p0,a0) *)
let r_havoc = function
  | [undef1;m1;p1;a1] -> begin
      match F.repr m1 with
      | L.Fun( f , [_undef0;m0;p0;a0] ) when f == f_havoc -> begin
          let open Qed.Logic in
          match F.is_true (r_included [p0;a0;p1;a1]) with
          | Yes -> F.e_fun f_havoc [undef1;m0;p1;a1]
          | _ -> raise Not_found
        end
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

(* havoc(undef,m,p,a)[k] =
   - undef[k]      WHEN separated (p,a,k,1)
   - m[k]  WHEN NOT separated (p,a,k,1)
*)
let r_get_havoc = function
  | [undef;m;p;a] ->
      (fun _ k ->
         match is_separated [p;a;k;e_one] with
         | L.Yes -> F.e_get m k
         | L.No  -> F.e_get undef k
         | _ -> raise Not_found)
  | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifier for int/addr conversion                                 --- *)
(* -------------------------------------------------------------------------- *)

let phi_int_of_addr p =
  if p == a_null then F.e_zero else
    match F.repr p with
    | L.Fun(f,[a]) when f == f_addr_of_int -> a
    | _ -> raise Not_found

let phi_addr_of_int p =
  if p == F.e_zero then a_null else
    match F.repr p with
    | L.Fun(f,[a]) when f == f_int_of_addr -> a
    | _ -> raise Not_found

(* -------------------------------------------------------------------------- *)
(* --- Simplifiers Registration                                           --- *)
(* -------------------------------------------------------------------------- *)

let () = Context.register
    begin fun () ->
      F.set_builtin_1   f_base   phi_base ;
      F.set_builtin_1   f_offset phi_offset ;
      F.set_builtin_2   f_shift  (phi_shift f_shift) ;
      F.set_builtin_eqp f_shift  eq_shift ;
      F.set_builtin_eqp f_global eq_shift ;
      F.set_builtin p_separated r_separated ;
      F.set_builtin p_included  r_included ;
      F.set_builtin f_havoc r_havoc ;
      F.set_builtin_get f_havoc r_get_havoc ;
      F.set_builtin_1 f_addr_of_int phi_addr_of_int ;
      F.set_builtin_1 f_int_of_addr phi_int_of_addr ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Frame Conditions                                                   --- *)
(* -------------------------------------------------------------------------- *)

module T = Definitions.Trigger

let frames ~addr:p ~offset:n ~sizeof:s ?(basename="mem") tau =
  let t_mem = L.Array(t_addr,tau) in
  let m  = F.e_var (Lang.freshvar ~basename t_mem) in
  let m' = F.e_var (Lang.freshvar ~basename t_mem) in
  let p' = F.e_var (Lang.freshvar ~basename:"q" t_addr) in
  let n' = F.e_var (Lang.freshvar ~basename:"n" L.Int) in
  let mh = F.e_fun f_havoc [m';m;p';n'] in
  let v' = F.e_var (Lang.freshvar ~basename:"v" tau) in
  let meq = F.p_call p_eqmem [m;m';p';n'] in
  let diff = F.p_call p_separated [p;n;p';s] in
  let sep = F.p_call p_separated [p;n;p';n'] in
  let inc = F.p_call p_included [p;n;p';n'] in
  let teq = T.of_pred meq in
  [
    "update" , [] , [diff] , m , e_set m p' v' ;
    "eqmem" , [teq] , [inc;meq] , m , m' ;
    "havoc" , [] , [sep] , m , mh ;
  ]

(* -------------------------------------------------------------------------- *)
(* --- Range Comparison                                                   --- *)
(* -------------------------------------------------------------------------- *)

type range =
  | LOC of term * term (* loc - size *)
  | RANGE of term * Vset.set (* base - range offset *)

let range ~shift ~addrof ~sizeof = function
  | Sigs.Rloc(obj,loc) ->
      LOC( addrof loc , F.e_int (sizeof obj) )
  | Sigs.Rrange(loc,obj,Some a,Some b) ->
      let s = sizeof obj in
      let p = addrof (shift loc obj a) in
      let n = e_fact s (e_range a b) in
      LOC( p , n )
  | Sigs.Rrange(loc,_obj,None,None) ->
      RANGE( a_base (addrof loc) , Vset.range None None )
  | Sigs.Rrange(loc,obj,Some a,None) ->
      let s = sizeof obj in
      RANGE( a_base (addrof loc) , Vset.range (Some (e_fact s a)) None )
  | Sigs.Rrange(loc,obj,None,Some b) ->
      let s = sizeof obj in
      RANGE( a_base (addrof loc) , Vset.range None (Some (e_fact s b)) )

let range_set = function
  | LOC(l,n) ->
      let a = a_offset l in
      let b = e_add a n in
      a_base l , Vset.range (Some a) (Some b)
  | RANGE(base,set) -> base , set

let r_included r1 r2 =
  match r1 , r2 with
  | LOC(l1,n1) , LOC(l2,n2) ->
      F.p_call p_included [l1;n1;l2;n2]
  | _ ->
      let base1,set1 = range_set r1 in
      let base2,set2 = range_set r2 in
      F.p_if (F.p_equal base1 base2)
        (Vset.subset set1 set2)
        (Vset.is_empty set1)

let r_disjoint r1 r2 =
  match r1 , r2 with
  | LOC(l1,n1) , LOC(l2,n2) ->
      F.p_call p_separated [l1;n1;l2;n2]
  | _ ->
      let base1,set1 = range_set r1 in
      let base2,set2 = range_set r2 in
      F.p_imply (F.p_equal base1 base2) (Vset.disjoint set1 set2)

let included ~shift ~addrof ~sizeof s1 s2  =
  let range = range ~shift ~addrof ~sizeof in
  r_included (range s1) (range s2)

let separated ~shift ~addrof ~sizeof s1 s2 =
  let range = range ~shift ~addrof ~sizeof in
  r_disjoint (range s1) (range s2)

(* -------------------------------------------------------------------------- *)
