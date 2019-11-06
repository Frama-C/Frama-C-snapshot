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
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes

open MemoryContext
open Lang
open Lang.F
open Sigs

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> MemoryContext.param
  val hypotheses : unit -> MemoryContext.clause list
end

module Make(V : VarUsage)(M : Sigs.Model) =
struct

  (* -------------------------------------------------------------------------- *)
  (* ---  Model                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let datatype = "MemVar." ^ V.datatype ^ M.datatype
  let configure = M.configure
  let no_binder = { bind = fun _ f v -> f v }
  let configure_ia _ = no_binder

  let hypotheses () = V.hypotheses () @ M.hypotheses ()

  (* -------------------------------------------------------------------------- *)
  (* ---  Chunk                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  type chunk =
    | Var of varinfo
    | Alloc of varinfo
    | Mem of M.Chunk.t

  let is_framed_var x = not x.vglob && not x.vaddrof
  (* Can not use VarUsage info, since (&x) can still be passed
     to the function and be modified by the call (when it assigns everything). *)

  module VAR =
  struct
    type t = varinfo
    let self = "var"
    let hash = Varinfo.hash
    let equal = Varinfo.equal
    let compare = Varinfo.compare
    let pretty = Varinfo.pretty
    let typ_of_chunk x =
      match V.param x with
      | ByRef -> Cil.typeOf_pointed x.vtype
      | _ -> x.vtype
    let tau_of_chunk x = Lang.tau_of_ctype (typ_of_chunk x)
    let is_framed = is_framed_var
    let basename_of_chunk = LogicUsage.basename
  end

  module VALLOC =
  struct
    type t = varinfo
    let self = "alloc"
    let hash = Varinfo.hash
    let compare = Varinfo.compare
    let equal = Varinfo.equal
    let pretty = Varinfo.pretty
    let tau_of_chunk _x = Qed.Logic.Bool
    let basename_of_chunk x =
      match V.param x with
      | ByRef ->
          "ra_" ^ LogicUsage.basename x
      | NotUsed | ByValue | ByShift | ByAddr | InContext | InArray ->
          "ta_" ^ LogicUsage.basename x
    let is_framed = is_framed_var
  end

  module Chunk =
  struct
    type t = chunk
    let self = "varmem"
    let hash = function
      | Var x -> 3 * Varinfo.hash x
      | Alloc x -> 5 * Varinfo.hash x
      | Mem m -> 7 * M.Chunk.hash m
    let compare c1 c2 =
      if c1 == c2 then 0 else
        match c1 , c2 with
        | Var x , Var y
        | Alloc x , Alloc y -> Varinfo.compare x y
        | Mem p , Mem q -> M.Chunk.compare p q
        | Var _ , _ -> (-1)
        | _ , Var _ -> 1
        | Alloc _  , _ -> (-1)
        | _ , Alloc _ -> 1
    let equal c1 c2 = (compare c1 c2 = 0)
    let pretty fmt = function
      | Var x -> Varinfo.pretty fmt x
      | Alloc x -> Format.fprintf fmt "alloc(%a)" Varinfo.pretty x
      | Mem m -> M.Chunk.pretty fmt m
    let tau_of_chunk = function
      | Var x -> VAR.tau_of_chunk x
      | Alloc x -> VALLOC.tau_of_chunk x
      | Mem m -> M.Chunk.tau_of_chunk m
    let basename_of_chunk = function
      | Var x -> VAR.basename_of_chunk x
      | Alloc x -> VALLOC.basename_of_chunk x
      | Mem m -> M.Chunk.basename_of_chunk m
    let is_framed = function
      | Var x -> VAR.is_framed x
      | Alloc x -> VALLOC.is_framed x
      | Mem m -> M.Chunk.is_framed m
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Sigma                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module HEAP = Qed.Collection.Make(VAR)
  module TALLOC = Qed.Collection.Make(VALLOC)
  module SIGMA = Sigma.Make(VAR)(HEAP)
  module ALLOC = Sigma.Make(VALLOC)(TALLOC)
  module Heap = Qed.Collection.Make(Chunk)

  type sigma = {
    mem : M.Sigma.t ;
    vars : SIGMA.t ;
    alloc : ALLOC.t ;
  }

  module Sigma =
  struct
    type t = sigma
    type chunk = Chunk.t
    module Chunk = Heap
    type domain = Heap.set
    let empty = Heap.Set.empty
    let union = Heap.Set.union

    let create () = {
      vars = SIGMA.create () ;
      alloc = ALLOC.create () ;
      mem = M.Sigma.create () ;
    }
    let copy s = {
      vars = SIGMA.copy s.vars ;
      alloc = ALLOC.copy s.alloc ;
      mem = M.Sigma.copy s.mem ;
    }

    let choose s1 s2 =
      let s = SIGMA.choose s1.vars s2.vars in
      let a = ALLOC.choose s1.alloc s2.alloc in
      let m = M.Sigma.choose s1.mem s2.mem in
      { vars = s ; alloc = a ; mem = m }

    let merge s1 s2 =
      let s,pa1,pa2 = SIGMA.merge s1.vars s2.vars in
      let a,ta1,ta2 = ALLOC.merge s1.alloc s2.alloc in
      let m,qa1,qa2 = M.Sigma.merge s1.mem s2.mem in
      { vars = s ; alloc = a ; mem = m } ,
      Passive.union (Passive.union pa1 ta1) qa1 ,
      Passive.union (Passive.union pa2 ta2) qa2

    let merge_list l =
      let s,pa = SIGMA.merge_list (List.map (fun s -> s.vars) l) in
      let a,ta = ALLOC.merge_list (List.map (fun s -> s.alloc) l) in
      let m,qa = M.Sigma.merge_list (List.map (fun s -> s.mem) l) in
      { vars = s ; alloc = a ; mem = m } ,
      let union = List.map2 Passive.union in
      union (union pa ta) qa

    let join s1 s2 =
      Passive.union
        (Passive.union
           (SIGMA.join s1.vars s2.vars)
           (ALLOC.join s1.alloc s2.alloc))
        (M.Sigma.join s1.mem s2.mem)

    let get s = function
      | Var x -> SIGMA.get s.vars x
      | Alloc x -> ALLOC.get s.alloc x
      | Mem m -> M.Sigma.get s.mem m

    let mem s = function
      | Var x -> SIGMA.mem s.vars x
      | Alloc x -> ALLOC.mem s.alloc x
      | Mem m -> M.Sigma.mem s.mem m

    let value s c = e_var (get s c)

    let iter f s =
      begin
        SIGMA.iter (fun x -> f (Var x)) s.vars ;
        ALLOC.iter (fun x -> f (Alloc x)) s.alloc ;
        M.Sigma.iter (fun m -> f (Mem m)) s.mem ;
      end

    let iter2 f s t =
      begin
        SIGMA.iter2 (fun x a b -> f (Var x) a b) s.vars t.vars ;
        ALLOC.iter2 (fun x a b -> f (Alloc x) a b) s.alloc t.alloc ;
        M.Sigma.iter2 (fun m p q -> f (Mem m) p q) s.mem t.mem ;
      end

    let domain_partition r =
      begin
        let xs = ref HEAP.Set.empty in
        let ts = ref TALLOC.Set.empty in
        let ms = ref M.Heap.Set.empty in
        Heap.Set.iter
          (function
            | Var x -> xs := HEAP.Set.add x !xs
            | Alloc x -> ts := TALLOC.Set.add x !ts
            | Mem c -> ms := M.Heap.Set.add c !ms
          ) r ;
        !xs , !ts , !ms
      end

    let domain_var xs =
      HEAP.Set.fold (fun x s -> Heap.Set.add (Var x) s) xs Heap.Set.empty

    let domain_alloc ts =
      TALLOC.Set.fold (fun x s -> Heap.Set.add (Alloc x) s) ts Heap.Set.empty

    let domain_mem ms =
      M.Heap.Set.fold (fun m s -> Heap.Set.add (Mem m) s) ms Heap.Set.empty

    let assigned ~pre ~post w =
      let w_vars , w_alloc , w_mem = domain_partition w in
      let h_vars = SIGMA.assigned ~pre:pre.vars ~post:post.vars w_vars in
      let h_alloc = ALLOC.assigned ~pre:pre.alloc ~post:post.alloc w_alloc in
      let h_mem = M.Sigma.assigned ~pre:pre.mem ~post:post.mem w_mem in
      Bag.ulist [h_vars;h_alloc;h_mem]

    let havoc s r =
      let rvar , ralloc , rmem = domain_partition r
      in {
        vars = SIGMA.havoc s.vars rvar ;
        alloc = ALLOC.havoc s.alloc ralloc ;
        mem = M.Sigma.havoc s.mem rmem ;
      }

    let havoc_chunk s = function
      | Var x -> { s with vars = SIGMA.havoc_chunk s.vars x }
      | Alloc x -> { s with alloc = ALLOC.havoc_chunk s.alloc x }
      | Mem m -> { s with mem = M.Sigma.havoc_chunk s.mem m }

    let havoc_any ~call s = {
      alloc = s.alloc ;
      vars = SIGMA.havoc_any ~call s.vars ;
      mem = M.Sigma.havoc_any ~call s.mem ;
    }

    let remove_chunks s r =
      let rvar , ralloc , rmem = domain_partition r
      in {
        vars = SIGMA.remove_chunks s.vars rvar ;
        alloc = ALLOC.remove_chunks s.alloc ralloc ;
        mem = M.Sigma.remove_chunks s.mem rmem ;
      }


    let domain s =
      Heap.Set.union
        (Heap.Set.union
           (domain_var (SIGMA.domain s.vars))
           (domain_alloc (ALLOC.domain s.alloc)))
        (domain_mem (M.Sigma.domain s.mem))

    let writes s =
      Heap.Set.union
        (Heap.Set.union
           (domain_var (SIGMA.writes {pre=s.pre.vars;post=s.post.vars}))
           (domain_alloc (ALLOC.writes {pre=s.pre.alloc;post=s.post.alloc})))
        (domain_mem (M.Sigma.writes {pre=s.pre.mem;post=s.post.mem}))

    let pretty fmt s =
      Format.fprintf fmt "@[<hov 2>{X:@[%a@]@ T:@[%a@]@ M:@[%a@]}@]"
        SIGMA.pretty s.vars
        ALLOC.pretty s.alloc
        M.Sigma.pretty s.mem

  end

  type domain = Sigma.domain

  let get_var s x = SIGMA.get s.vars x
  let get_term s x = e_var (get_var s x)

  (* -------------------------------------------------------------------------- *)
  (* ---  State Pretty Printer                                              --- *)
  (* -------------------------------------------------------------------------- *)

  type ichunk = Iref of varinfo | Ivar of varinfo

  type state = {
    svar : ichunk Tmap.t ;
    smem : M.state ;
  }

  module IChunk =
  struct

    let compare_var x y =
      let rank x =
        if x.vformal then 0 else
        if x.vglob then 1 else
        if x.vtemp then 3 else 2 in
      let cmp = rank x - rank y in
      if cmp <> 0 then cmp else Varinfo.compare x y

    type t = ichunk
    let hash = function Iref x | Ivar x -> Varinfo.hash x
    let compare x y =
      match x,y with
      | Iref x , Iref y -> Varinfo.compare x y
      | Iref _ , _ -> (-1)
      | _ , Iref _ -> 1
      | Ivar x , Ivar y -> compare_var x y
    let equal x y =
      match x,y with
      | Iref x , Iref y | Ivar x , Ivar y -> Varinfo.equal x y
      | Iref _ , Ivar _ | Ivar _ , Iref _ -> false

  end

  module Icmap = Qed.Mergemap.Make(IChunk)

  let set_chunk v c m =
    let c =
      try
        let c0 = Tmap.find v m in
        if IChunk.compare c c0 < 0 then c else c0
      with Not_found -> c in
    Tmap.add v c m

  let state s =
    let m = ref Tmap.empty in
    SIGMA.iter (fun x v ->
        let c = match V.param x with ByRef -> Iref x | _ -> Ivar x in
        m := set_chunk (e_var v) c !m
      ) s.vars ;
    { svar = !m ; smem = M.state s.mem }

  let ilval = function
    | Iref x -> (Mvar x,[Mindex e_zero])
    | Ivar x -> (Mvar x,[])

  let imval c = Sigs.Mlval (ilval c)

  let lookup s e =
    try imval (Tmap.find e s.svar)
    with Not_found -> M.lookup s.smem e

  let apply f s =
    let m = ref Tmap.empty in
    Tmap.iter (fun e c ->
        let e = f e in
        m := set_chunk e c !m ;
      ) s.svar ;
    { svar = !m ; smem = M.apply f s.smem }

  let iter f s =
    Tmap.iter (fun v c -> f (imval c) v) s.svar ;
    M.iter f s.smem

  let icmap domain istate =
    Tmap.fold (fun m c w ->
        if Vars.intersect (F.vars m) domain
        then Icmap.add c m w else w
      ) istate Icmap.empty

  let rec diff lv v1 v2 =
    if v1 == v2 then Bag.empty else
      match F.repr v2 with
      | Qed.Logic.Aset(m , k , vk) ->
          let upd = diff (Mstate.index lv k) (F.e_get m k) vk in
          Bag.concat (diff lv v1 m) upd
      | Qed.Logic.Rdef fvs ->
          rdiff lv v1 v2 fvs
      | _ ->
          Bag.elt (Mstore(lv,v2))

  and rdiff lv v1 v2 = function
    | (Lang.Cfield fi as fd ,f2) :: fvs ->
        let f1 = F.e_getfield v1 fd in
        if f1 == f2 then rdiff lv v1 v2 fvs else
          let upd = diff (Mstate.field lv fi) f1 f2 in
          let m = F.e_setfield v2 fd f1 in
          Bag.concat upd (diff lv v1 m)
    | (Lang.Mfield _,_)::_ -> Bag.elt (Mstore(lv,v2))
    | [] -> Bag.empty

  let updates seq domain =
    let pre = icmap domain seq.pre.svar in
    let post = icmap domain seq.post.svar in
    let pool = ref Bag.empty in
    Icmap.iter2
      (fun c v1 v2 ->
         match v1 , v2 with
         | _ , None -> ()
         | None , Some v -> pool := Bag.add (Mstore(ilval c,v)) !pool
         | Some v1 , Some v2 -> pool := Bag.concat (diff (ilval c) v1 v2) !pool
      ) pre post ;
    let seq_mem =  { pre = seq.pre.smem ; post = seq.post.smem } in
    Bag.concat !pool (M.updates seq_mem domain)

  (* -------------------------------------------------------------------------- *)
  (* ---  Location                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  type mem =
    | CVAL (* By-Value variable *)
    | CREF (* By-Ref variable *)
    | CTXT (* In-context pointer *)
    | CARR (* In-context array *)
    | HEAP (* In-heap variable *)

  type loc =
    | Ref of varinfo
    | Val of mem * varinfo * ofs list (* The varinfo has {i not} been contextualized yet *)
    | Loc of M.loc (* Generalized In-Heap pointer *)

  and ofs =
    | Field of fieldinfo
    | Shift of c_object * term

  type segment = loc rloc

  let rec ofs_vars xs = function
    | [] -> xs
    | Field _ :: ofs -> ofs_vars xs ofs
    | Shift(_,k) :: ofs -> ofs_vars (Vars.union xs (F.vars k)) ofs

  let vars = function
    | Ref _ -> Vars.empty
    | Loc l -> M.vars l
    | Val(_,_,ofs) -> ofs_vars Vars.empty ofs

  let rec ofs_occurs x = function
    | [] -> false
    | Field _ :: ofs -> ofs_occurs x ofs
    | Shift(_,k) :: ofs -> Vars.mem x (F.vars k) || ofs_occurs x ofs

  let occurs x = function
    | Ref _ -> false
    | Loc l -> M.occurs x l
    | Val(_,_,ofs) -> ofs_occurs x ofs

  let byte_offset n = function
    | Field fd -> F.e_add n (F.e_int (Ctypes.field_offset fd))
    | Shift(obj,k) -> F.e_add n (F.e_fact (Ctypes.sizeof_object obj) k)

  (* -------------------------------------------------------------------------- *)
  (* ---  Variable and Context                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let vtype m x =
    match m with
    | CVAL | HEAP -> x.vtype
    | CTXT | CREF -> Cil.typeOf_pointed x.vtype
    | CARR -> Ast_info.array_type (Cil.typeOf_pointed x.vtype)

  let vobject m x = Ctypes.object_of (vtype m x)

  let vbase m x =
    match m with
    | CVAL | HEAP -> x
    | _ -> { x with vglob = true ; vtype = vtype m x }

  (* -------------------------------------------------------------------------- *)
  (* ---  Pretty                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let rec pp_offset ~obj fmt = function
    | [] -> ()
    | Field f :: ofs ->
        Format.fprintf fmt ".%s" f.fname ;
        pp_offset ~obj:(object_of f.ftype) fmt ofs
    | Shift(elt,k) :: ofs ->
        if Ctypes.is_array obj ~elt then
          ( Format.fprintf fmt ".(%a)" F.pp_term k ;
            pp_offset ~obj:elt fmt ofs )
        else
          ( Format.fprintf fmt ".(%a : %a)" F.pp_term k Ctypes.pretty elt ;
            pp_offset ~obj:elt fmt ofs )

  let pp_mem fmt = function
    | CVAL -> Format.pp_print_string fmt "var"
    | CREF -> Format.pp_print_string fmt "ref"
    | CTXT -> Format.pp_print_string fmt "ptr"
    | CARR -> Format.pp_print_string fmt "arr"
    | HEAP -> Format.pp_print_string fmt "mem"

  (* re-uses strings that are used into the description of -wp-xxx-vars *)
  let pp_var_model fmt = function
    | ByValue | ByShift | NotUsed -> Format.pp_print_string fmt "non-aliased" (* cf.  -wp-unalias-vars *)
    | ByRef -> Format.pp_print_string fmt "by reference" (* cf. -wp-ref-vars *)
    | InContext | InArray -> Format.pp_print_string fmt "in an isolated context" (* cf. -wp-context-vars *)
    | ByAddr -> Format.pp_print_string fmt "aliased" (* cf. -wp-alias-vars *)

  let pretty fmt = function
    | Ref x -> VAR.pretty fmt x
    | Loc l -> M.pretty fmt l
    | Val(m,x,ofs) ->
        let obj = vobject m x in
        Format.fprintf fmt "@[%a:%a%a@]"
          pp_mem m VAR.pretty x
          (pp_offset ~obj) ofs

  let noref ~op var =
    Warning.error
      "forbidden %s variable '%a' considered %a.@\n\
       Use model 'Typed' instead or specify '-wp-unalias-vars %a'"
      op Varinfo.pretty var
      pp_var_model (V.param var)
      Varinfo.pretty var

  (* -------------------------------------------------------------------------- *)
  (* ---  Basic Constructors                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let null = Loc M.null

  let literal ~eid cst = Loc (M.literal ~eid cst)

  let cvar x = match V.param x with
    | NotUsed | ByValue | ByShift -> Val(CVAL,x,[])
    | ByAddr -> Val(HEAP,x,[])
    | InContext | InArray | ByRef -> Ref x

  (* -------------------------------------------------------------------------- *)
  (* ---  Lifting                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let moffset l = function
    | Field f -> M.field l f
    | Shift(e,k) -> M.shift l e k

  let mseq_of_seq seq = { pre = seq.pre.mem ; post = seq.post.mem }

  let mloc_of_path m x ofs =
    List.fold_left moffset (M.cvar (vbase m x)) ofs

  let mloc_of_loc = function
    | Loc l -> l
    | Ref x -> M.cvar x
    | Val(m,x,ofs) -> mloc_of_path m x ofs

  let pointer_loc p = Loc (M.pointer_loc p)
  let pointer_val l = M.pointer_val (mloc_of_loc l)

  let field l f = match l with
    | Loc l -> Loc (M.field l f)
    | Ref x -> noref ~op:"field access to" x
    | Val(m,x,ofs) -> Val(m,x,ofs @ [Field f])

  let rec ofs_shift obj k = function
    | [] -> [Shift(obj,k)]
    | [Shift(elt,i)] when Ctypes.equal obj elt -> [Shift(elt,F.e_add i k)]
    | f::ofs -> f :: ofs_shift obj k ofs

  let shift l obj k = match l with
    | Loc l -> Loc (M.shift l obj k)
    | Ref x -> noref ~op:"array access to" x
    | Val(m,x,ofs) -> Val(m,x,ofs_shift obj k ofs)

  let base_addr = function
    | Loc l -> Loc (M.base_addr l)
    | Ref x -> noref ~op:"base address of" x (* ??? ~suggest:ByValue *)
    | Val(m,x,_) -> Val(m,x,[])

  let base_offset = function
    | Loc l -> M.base_offset l
    | Ref x -> noref ~op:"offset address of" x (* ??? ~suggest:ByValue *)
    | Val(_,_,ofs) -> List.fold_left byte_offset e_zero ofs

  let block_length sigma obj = function
    | Loc l -> M.block_length sigma.mem obj l
    | Ref x -> noref ~op:"block-length of" x
    | Val(m,x,_) ->
        let obj = Ctypes.object_of (vtype m x) in
        let size =
          if Ctypes.sizeof_defined obj
          then Ctypes.sizeof_object obj
          else if Wp_parameters.ExternArrays.get ()
          then max_int
          else Warning.error ~source:"MemVar" "Unknown array-size"
        in F.e_int size

  let cast obj l = Loc(M.cast obj (mloc_of_loc l))
  let loc_of_int e a = Loc(M.loc_of_int e a)
  let int_of_loc i l = M.int_of_loc i (mloc_of_loc l)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Load                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec access a = function
    | [] -> a
    | Field f :: ofs -> access (e_getfield a (Cfield f)) ofs
    | Shift(_,k) :: ofs -> access (e_get a k) ofs

  let rec update a ofs v = match ofs with
    | [] -> v
    | Field f :: ofs ->
        let phi = Cfield f in
        let a_f = F.e_getfield a phi in
        let a_f_v = update a_f ofs v in
        F.e_setfield a phi a_f_v
    | Shift(_,k) :: ofs ->
        let a_k = F.e_get a k in
        let a_k_v = update a_k ofs v in
        F.e_set a k a_k_v

  let load sigma obj = function
    | Ref x ->
        begin match V.param x with
          | ByRef     -> Sigs.Loc(Val(CREF,x,[]))
          | InContext -> Sigs.Loc(Val(CTXT,x,[]))
          | InArray   -> Sigs.Loc(Val(CARR,x,[]))
          | NotUsed | ByAddr | ByValue | ByShift -> assert false
        end
    | Val((CREF|CVAL),x,ofs) ->
        Sigs.Val(access (get_term sigma x) ofs)
    | Loc l ->
        Cvalues.map_value
          (fun l -> Loc l)
          (M.load sigma.mem obj l)
    | Val((CTXT|CARR|HEAP) as m,x,ofs) ->
        Cvalues.map_value
          (fun l -> Loc l)
          (M.load sigma.mem obj (mloc_of_path m x ofs))

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Store                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let stored seq obj l v = match l with
    | Ref x -> noref ~op:"write to" x
    | Val((CREF|CVAL),x,ofs) ->
        let v1 = get_term seq.pre x in
        let v2 = get_term seq.post x in
        [Set( v2 , update v1 ofs v )]
    | Val((CTXT|CARR|HEAP) as m,x,ofs) ->
        M.stored (mseq_of_seq seq) obj (mloc_of_path m x ofs) v
    | Loc l ->
        M.stored (mseq_of_seq seq) obj l v

  let copied seq obj l1 l2 =
    let v = match load seq.pre obj l2 with
      | Sigs.Val r -> r
      | Sigs.Loc l -> pointer_val l
    in stored seq obj l1 v

  (* -------------------------------------------------------------------------- *)
  (* ---  Pointer Comparison                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let is_null = function
    | Loc l -> M.is_null l
    | Ref _ | Val _ -> F.p_false

  let rec offset = function
    | [] -> e_zero
    | Field f :: ofs ->
        e_add (e_int (Ctypes.field_offset f)) (offset ofs)
    | Shift(obj,k)::ofs ->
        e_add (e_fact (Ctypes.sizeof_object obj) k) (offset ofs)

  let loc_diff obj a b =
    match a , b with
    | Loc l1 , Loc l2 -> M.loc_diff obj l1 l2
    | Ref x , Ref y when Varinfo.equal x y -> e_zero
    | Val(_,x,p) , Val(_,y,q) when Varinfo.equal x y ->
        e_div (e_sub (offset p) (offset q)) (e_int (Ctypes.sizeof_object obj))
    | _ ->
        Warning.error ~source:"Reference Variable Model"
          "Uncomparable locations %a and %a" pretty a pretty b

  let loc_compare lcmp icmp same a b =
    match a , b with
    | Loc l1 , Loc l2 -> lcmp l1 l2
    | Ref x , Ref y ->
        if Varinfo.equal x y then same else p_not same
    | Val(_,x,p) , Val(_,y,q) ->
        if Varinfo.equal x y then icmp (offset p) (offset q) else p_not same
    | (Val _ | Loc _) , (Val _ | Loc _) -> lcmp (mloc_of_loc a) (mloc_of_loc b)
    | Ref _ , (Val _ | Loc _) | (Val _ | Loc _) , Ref _ -> p_not same

  let loc_eq = loc_compare M.loc_eq F.p_equal F.p_true
  let loc_lt = loc_compare M.loc_lt F.p_lt F.p_false
  let loc_leq = loc_compare M.loc_leq F.p_leq F.p_true
  let loc_neq = loc_compare M.loc_neq F.p_neq F.p_false

  (* -------------------------------------------------------------------------- *)
  (* ---  Range & Offset Fits                                               --- *)
  (* -------------------------------------------------------------------------- *)

  exception ShiftMismatch

  let is_heap_allocated = function
    | CREF | CVAL -> false | HEAP | CTXT | CARR -> true

  let shift_mismatch l =
    Wp_parameters.fatal "Invalid shift : %a" pretty l

  let unsized_array () = Warning.error ~severe:false
      "Validity of unsized-array not implemented yet"

  let fits_inside cond a b n =
    p_leq e_zero a :: p_lt b (e_int n) :: cond

  let stay_outside cond a b n =
    p_lt b e_zero :: p_leq (e_int n) a :: cond

  (* Append conditions to [cond] for [range=(elt,a,b)],
     consisting of [a..b] elements with type [elt] to fits inside the block,
     provided [a<=b]. *)
  let rec block_check fitting cond (block,size) ((elt,a,b) as range) =
    if Ctypes.equal block elt then
      fitting cond a b size
    else
      match Ctypes.get_array block with
      | Some( e , Some n ) -> block_check fitting cond (e , n * size) range
      | Some( _ , None ) -> unsized_array ()
      | None -> raise ShiftMismatch

  (* Append conditions for [offset] to fits [object], provided [a<=b]. *)
  let rec offset_fits cond obj offset =
    match offset with
    | [] -> cond
    | Field fd :: ofs ->
        offset_fits cond (Ctypes.object_of fd.ftype) ofs
    | Shift(te,k) :: ofs ->
        match Ctypes.get_array obj with
        | Some( e , Some n ) when Ctypes.equal e te ->
            let cond = p_leq e_zero k :: p_lt k (e_int n) :: cond in
            offset_fits cond e ofs
        | Some( _ , None ) -> unsized_array ()
        | _ ->
            let cond = block_check fits_inside cond (obj,1) (te,k,k) in
            offset_fits cond te ofs

  (* Append conditions to [cond] for [range=(elt,a,b)], starting at [offset],
     consisting of [a..b] elements with type [elt] to fits inside the block,
     of stay outside valid paths, provided [a<=b]. *)
  let rec range_check fitting cond alloc offset ((elt,a,b) as range) =
    match offset with
    | [] -> block_check fitting cond alloc range
    | Field fd :: ofs ->
        range_check fitting cond (Ctypes.object_of fd.ftype,1) ofs range
    | Shift(te,k) :: ofs ->
        if Ctypes.equal te elt then
          range_check fitting cond alloc ofs (elt,e_add a k,e_add b k)
        else
          match Ctypes.get_array (fst alloc) with
          | Some( e , Some n ) when Ctypes.equal e te ->
              let cond = fitting cond k k n in
              range_check fitting cond (e,n) ofs range
          | Some( _ , None ) ->
              unsized_array ()
          | _ ->
              let cond = block_check fitting cond alloc (te,k,k) in
              range_check fitting cond (te,1) ofs range

  (* -------------------------------------------------------------------------- *)
  (* ---  Validity                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let valid_offset obj ofs =
    F.p_conj (offset_fits [] obj ofs)

  let valid_range obj ofs range =
    F.p_conj (range_check fits_inside [] (obj,1) ofs range)

  (* varinfo *)

  let valid_base sigma acs mem x =
    if x.vglob then
      if acs = RW && Cil.typeHasQualifier "const" x.vtype
      then p_false
      else p_true
    else
      match mem with
      | CVAL | HEAP -> p_bool (ALLOC.value sigma.alloc x)
      | CREF | CTXT | CARR -> p_true

  (* segment *)

  let valid_offset_path sigma acs mem x ofs =
    p_and
      (valid_base sigma acs mem x)
      (valid_offset (vobject mem x) ofs)

  let valid_range_path sigma acs mem x ofs rg =
    p_and
      (valid_base sigma acs mem x)
      (valid_range (vobject mem x) ofs rg)

  (* in-model validation *)

  let valid sigma acs = function
    | Rloc(obj,l) ->
        begin match l with
          | Ref _ -> p_true
          | Loc l -> M.valid sigma.mem acs (Rloc(obj,l))
          | Val(m,x,p) ->
              try valid_offset_path sigma acs m x p
              with ShiftMismatch ->
                if is_heap_allocated m then
                  M.valid sigma.mem acs (Rloc(obj,mloc_of_loc l))
                else
                  shift_mismatch l
        end
    | Rrange(l,elt,a,b) ->
        begin match l with
          | Ref x -> noref ~op:"valid sub-range of" x
          | Loc l -> M.valid sigma.mem acs (Rrange(l,elt,a,b))
          | Val(m,x,p) ->
              match a,b with
              | Some ka,Some kb ->
                  begin
                    try
                      F.p_imply (F.p_leq ka kb)
                        (valid_range_path sigma acs m x p (elt,ka,kb))
                    with ShiftMismatch ->
                      if is_heap_allocated m then
                        let l = mloc_of_loc l in
                        M.valid sigma.mem acs (Rrange(l,elt,a,b))
                      else shift_mismatch l
                  end
              | _ ->
                  Warning.error "Validity of infinite range @[%a.(%a..%a)@]"
                    pretty l Vset.pp_bound a Vset.pp_bound b
        end

  (* -------------------------------------------------------------------------- *)
  (* ---  Invalidity                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let invalid_range obj ofs range =
    F.p_disj (range_check stay_outside [] (obj,1) ofs range)

  let invalid_offset_path sigma m x p =
    p_not (valid_offset_path sigma RD m x p)

  let invalid_range_path sigma m x p rg =
    p_imply
      (valid_base sigma RD m x)
      (invalid_range (vobject m x) p rg)

  let invalid sigma = function
    | Rloc(obj,l) ->
        begin match l with
          | Ref _ -> p_false
          | Loc l -> M.invalid sigma.mem (Rloc(obj,l))
          | Val(m,x,p) ->
              try invalid_offset_path sigma m x p
              with ShiftMismatch ->
                if is_heap_allocated m then
                  M.invalid sigma.mem (Rloc(obj,mloc_of_loc l))
                else
                  shift_mismatch l
        end
    | Rrange(l,elt,a,b) ->
        begin match l with
          | Ref x -> noref ~op:"invalid sub-range of" x
          | Loc l -> M.invalid sigma.mem (Rrange(l,elt,a,b))
          | Val(m,x,p) ->
              match a,b with
              | Some ka,Some kb ->
                  begin
                    try
                      F.p_imply (F.p_leq ka kb)
                        (invalid_range_path sigma m x p (elt,ka,kb))
                    with ShiftMismatch ->
                      if is_heap_allocated m then
                        let l = mloc_of_loc l in
                        M.invalid sigma.mem (Rrange(l,elt,a,b))
                      else shift_mismatch l
                  end
              | _ ->
                  Warning.error "Invalidity of infinite range @[%a.(%a..%a)@]"
                    pretty l Vset.pp_bound a Vset.pp_bound b
        end

  (* -------------------------------------------------------------------------- *)
  (* ---  Scope                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let is_mem x = match V.param x with
    | ByAddr -> true
    | _ -> false

  let is_mvar_alloc x =
    match V.param x with
    | ByRef | InContext | InArray | NotUsed -> false
    | ByValue | ByShift | ByAddr -> true

  let frame sigma =
    let pool = ref [] in
    SIGMA.iter
      (fun x p ->
         if (x.vglob || x.vformal) && Cil.isPointerType (VAR.typ_of_chunk x)
         then pool := M.global sigma.mem (e_var p) :: !pool
      ) sigma.vars ;
    !pool @ M.frame sigma.mem

  let alloc sigma xs =
    let xm = List.filter is_mem xs in
    let mem = M.alloc sigma.mem xm in
    let xv = List.filter is_mvar_alloc xs in
    let domain = TALLOC.Set.of_list xv in
    let alloc = ALLOC.havoc sigma.alloc domain in
    { sigma with alloc ; mem }

  let scope_vars seq scope xs =
    let xs = List.filter is_mvar_alloc xs in
    if xs = [] then []
    else
      let t_in = seq.pre.alloc in
      let t_out = seq.post.alloc in
      let v_in  = match scope with Enter -> e_false | Leave -> e_true in
      let v_out = match scope with Enter -> e_true | Leave -> e_false in
      List.map
        (fun x ->
           F.p_and
             (F.p_equal (ALLOC.value t_in x) v_in)
             (F.p_equal (ALLOC.value t_out x) v_out)
        ) xs

  let scope seq scope xs =
    let xm = List.filter is_mem xs in
    let smem = { pre = seq.pre.mem ; post = seq.post.mem } in
    let hmem = M.scope smem scope xm in
    let hvars = scope_vars seq scope xs in
    hvars @ hmem

  let global sigma p = M.global sigma.mem p

  (* -------------------------------------------------------------------------- *)
  (* ---  Havoc along a ranged-path                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec assigned_path
      (hs : pred list) (* collector of properties *)
      (xs : var list)  (* variable quantifying the assigned location *)
      (ys : var list)  (* variable quantifying others locations *)
      (a : term)  (* pre-term for root + current offset *)
      (b : term)  (* post-term for root + current offset *)
    = function
      | [] -> hs

      (*TODO: optimized version for terminal [Field _] and [Index _] *)

      | Field f :: ofs ->
          let cf = Cfield f in
          let af = e_getfield a cf in
          let bf = e_getfield b cf in
          let hs = assigned_path hs xs ys af bf ofs in
          List.fold_left
            (fun hs g ->
               if Fieldinfo.equal f g then hs else
                 let cg = Cfield g in
                 let ag = e_getfield a cg in
                 let bg = e_getfield b cg in
                 let eqg = p_forall ys (p_equal ag bg) in
                 eqg :: hs
            ) hs f.fcomp.cfields

      | Shift(_,e) :: ofs ->
          let y = Lang.freshvar ~basename:"k" Qed.Logic.Int in
          let k = e_var y in
          let ak = e_get a k in
          let bk = e_get b k in
          if List.exists (fun x -> F.occurs x e) xs then
            (* index [e] is covered by [xs]:
               must explore deeper the remaining path. *)
            assigned_path hs xs (y::ys) ak bk ofs
          else
            (* index [e] is not covered by [xs]:
               any index different from e is disjoint.
               explore also deeply with index [e]. *)
            let ae = e_get a e in
            let be = e_get b e in
            let ek = p_neq e k in
            let eqk = p_forall (y::ys) (p_imply ek (p_equal ak bk)) in
            assigned_path (eqk :: hs) xs ys ae be ofs

  let assigned_genset s xs mem x ofs p =
    let valid = valid_offset_path s.post Sigs.RW mem x ofs in
    let a = get_term s.pre x in
    let b = get_term s.post x in
    let a_ofs = access a ofs in
    let b_ofs = access b ofs in
    let p_sloc = p_forall xs (p_hyps [valid;p_not p] (p_equal a_ofs b_ofs)) in
    let conds = assigned_path [p_sloc] xs [] a b ofs in
    List.map (fun p -> Assert p) conds

  (* -------------------------------------------------------------------------- *)
  (* ---  Assigned                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let assigned_loc seq obj = function
    | Ref x -> noref ~op:"assigns to" x
    | Val((CVAL|CREF),_,[]) -> [] (* full update *)
    | Val((CVAL|CREF),_,_) as vloc ->
        let v = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
        stored seq obj vloc (e_var v)
    | Val((HEAP|CTXT|CARR) as m,x,ofs) ->
        M.assigned (mseq_of_seq seq) obj (Sloc (mloc_of_path m x ofs))
    | Loc l ->
        M.assigned (mseq_of_seq seq) obj (Sloc l)

  let assigned_array seq obj l elt n =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Val((CVAL|CREF),_,[]) -> [] (* full update *)
    | Val((CVAL|CREF),_,_) as vloc ->
        let te = Lang.tau_of_object elt in
        let v = Lang.freshvar ~basename:"v" Qed.Logic.(Array(Int,te)) in
        stored seq obj vloc (e_var v)
    | Val((HEAP|CTXT|CARR) as m,x,ofs) ->
        let l = mloc_of_path m x ofs in
        M.assigned (mseq_of_seq seq) obj (Sarray(l,elt,n))
    | Loc l ->
        M.assigned (mseq_of_seq seq) obj (Sarray(l,elt,n))

  let assigned_range seq obj l elt a b =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Loc l ->
        M.assigned (mseq_of_seq seq) obj (Srange(l,elt,a,b))
    | Val((HEAP|CTXT|CARR) as m,x,ofs) ->
        M.assigned (mseq_of_seq seq) obj (Srange(mloc_of_path m x ofs,elt,a,b))
    | Val((CVAL|CREF) as m,x,ofs) ->
        let k = Lang.freshvar ~basename:"k" Qed.Logic.Int in
        let p = Vset.in_range (e_var k) a b in
        let ofs = ofs_shift elt (e_var k) ofs in
        assigned_genset seq [k] m x ofs p

  let assigned_descr seq obj xs l p =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Loc l ->
        M.assigned (mseq_of_seq seq) obj (Sdescr(xs,l,p))
    | Val((HEAP|CTXT|CARR) as m,x,ofs) ->
        M.assigned (mseq_of_seq seq) obj (Sdescr(xs,mloc_of_path m x ofs,p))
    | Val((CVAL|CREF) as m,x,ofs) ->
        assigned_genset seq xs m x ofs p

  let assigned seq obj = function
    | Sloc l -> assigned_loc seq obj l
    | Sarray(l,elt,n) -> assigned_array seq obj l elt n
    | Srange(l,elt,a,b) -> assigned_range seq obj l elt a b
    | Sdescr(xs,l,p) -> assigned_descr seq obj xs l p

  (* -------------------------------------------------------------------------- *)
  (* --- Segments                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  type seq =
    | Rseg of varinfo
    | Fseg of varinfo * delta list
    | Mseg of M.loc rloc * varinfo * delta list
    | Lseg of M.loc rloc
  and delta =
    | Dfield of fieldinfo
    | Drange of term option * term option

  let dofs = function
    | Field f -> Dfield f
    | Shift(_,k) -> let u = Some k in Drange(u,u)

  let tofs = function
    | Field d -> Ctypes.object_of d.ftype
    | Shift(elt,_) -> elt

  let rec dstartof dim = function
    | C_array arr ->
        let n = match arr.arr_flat with None -> 1 | Some a -> a.arr_dim in
        if n > dim then
          let u = Some e_zero in
          let elt = Ctypes.object_of arr.arr_element in
          Drange(u,u) :: dstartof dim elt
        else []
    | _ -> []

  let rec doffset obj host = function
    | d::ds -> dofs d :: (doffset obj (tofs d) ds)
    | [] -> dstartof (Ctypes.get_array_dim obj) host

  let delta obj x ofs = doffset obj (Ctypes.object_of x.vtype) ofs

  let rec range ofs obj a b =
    match ofs with
    | [] -> [ Drange(a,b) ]
    | [Shift(elt,k)] when Ctypes.equal elt obj ->
        [ Drange( Vset.bound_shift a k , Vset.bound_shift b k ) ]
    | d :: ofs -> dofs d :: range ofs obj a b

  let locseg = function

    | Rloc(_,Ref x) -> Rseg x
    | Rrange(Ref x,_,_,_) -> noref ~op:"sub-range of" x

    | Rloc(obj,Loc l) -> Lseg (Rloc(obj,l))
    | Rloc(obj,Val((CVAL|CREF),x,ofs)) ->
        Fseg(x,delta obj x ofs)

    | Rrange(Loc l,obj,a,b) -> Lseg (Rrange(l,obj,a,b))
    | Rrange(Val((CVAL|CREF),x,ofs),obj,a,b) ->
        Fseg(x,range ofs obj a b)

    (* in M: *)
    | Rloc(obj,Val((CTXT|CARR|HEAP) as m,x,ofs)) ->
        Mseg(Rloc(obj,mloc_of_path m x ofs),x,delta obj x ofs)
    | Rrange(Val((CTXT|CARR|HEAP) as m,x,ofs),obj,a,b) ->
        Mseg(Rrange(mloc_of_path m x ofs,obj,a,b),x,range ofs obj a b)

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Inclusion                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let rec included_delta d1 d2 =
    match d1 , d2 with
    | _ , [] -> p_true
    | [] , _ -> p_false
    | u :: d1 , v :: d2 ->
        match u , v with
        | Dfield f , Dfield g when Fieldinfo.equal f g ->
            included_delta d1 d2
        | Dfield _ , _ | _ , Dfield _ -> p_false
        | Drange(a1,b1) , Drange(a2,b2) ->
            p_conj [ Vset.ordered ~strict:false ~limit:true a2 a1 ;
                     Vset.ordered ~strict:false ~limit:true b1 b2 ;
                     included_delta d1 d2 ]

  let included s1 s2 =
    match locseg s1 , locseg s2 with
    | Rseg x , Rseg y -> if Varinfo.equal x y then p_true else p_false
    | Rseg _ , _ | _ , Rseg _ -> p_false

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
        if Varinfo.equal x1 x2 then included_delta d1 d2 else p_false

    | Fseg _ , _ | _ , Fseg _ -> p_false

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.included s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Separation                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let rec separated_delta d1 d2 =
    match d1 , d2 with
    | [] , _ | _ , [] -> p_false
    | u :: d1 , v :: d2 ->
        match u , v with
        | Dfield f , Dfield g when Fieldinfo.equal f g
          -> separated_delta d1 d2
        | Dfield _ , _ | _ , Dfield _ -> p_true
        | Drange(a1,b1) , Drange(a2,b2) ->
            p_disj [ Vset.ordered ~strict:true ~limit:false b1 a2 ;
                     Vset.ordered ~strict:true ~limit:false b2 a1 ;
                     separated_delta d1 d2 ]

  let separated r1 r2 =
    match locseg r1 , locseg r2 with
    | Rseg x , Rseg y -> if Varinfo.equal x y then p_false else p_true
    | Rseg _ , _ | _ , Rseg _ -> p_true

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
        if Varinfo.equal x1 x2 then separated_delta d1 d2 else p_true
    | Fseg _ , _ | _ , Fseg _ -> p_true

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.separated s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Domain                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let domain obj l =
    match l with
    | Ref x | Val((CVAL|CREF),x,_) ->
        Heap.Set.singleton (Var x)
    | Loc _ | Val((CTXT|CARR|HEAP),_,_) ->
        M.Heap.Set.fold
          (fun m s -> Heap.Set.add (Mem m) s)
          (M.domain obj (mloc_of_loc l)) Heap.Set.empty

  (* -------------------------------------------------------------------------- *)

end
