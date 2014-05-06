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
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes

open Lang
open Lang.F
open Memory

type param = ByValue | ByRef | InHeap

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> param
end

module Make(V : VarUsage)(M : Memory.Model) =
struct

  let datatype = "MemVar." ^ V.datatype ^ M.datatype
  let configure = M.configure

  (* -------------------------------------------------------------------------- *)
  (* ---  Chunk                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  type chunk =
    | Var of varinfo
    | Alloc of varinfo
    | Mem of M.Chunk.t

  let is_framed_var x =
    not x.vglob && 
    match V.param x with
    | ByValue -> true
    | ByRef | InHeap -> false
      
  module VAR = 
  struct
    type t = varinfo
    let self = "var"
    let hash = Varinfo.hash
    let equal = Varinfo.equal
    let compare = Varinfo.compare
    let pretty = Varinfo.pretty
    let typ_of_param x = 
      match V.param x with
	| ByValue | InHeap -> x.vtype
	| ByRef -> Cil.typeOf_pointed x.vtype
    let tau_of_chunk x = Lang.tau_of_ctype (typ_of_param x)
    let basename_of_chunk = LogicUsage.basename
    let is_framed = is_framed_var
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
	| ByRef -> "ra_" ^ LogicUsage.basename x
	| ByValue | InHeap -> "ta_" ^ LogicUsage.basename x
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
    type domain = Heap.set
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
    let merge s1 s2 =
      let s,pa1,pa2 = SIGMA.merge s1.vars s2.vars in
      let a,ta1,ta2 = ALLOC.merge s1.alloc s2.alloc in
      let m,qa1,qa2 = M.Sigma.merge s1.mem s2.mem in
      { vars = s ; alloc = a ; mem = m } ,
      Passive.union (Passive.union pa1 ta1) qa1 , 
      Passive.union (Passive.union pa2 ta2) qa2
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

    let assigned s1 s2 w =
      let w_vars , w_alloc , w_mem = domain_partition w in
      let h_vars = SIGMA.assigned s1.vars s2.vars w_vars in
      let h_alloc = ALLOC.assigned s1.alloc s2.alloc w_alloc in
      let h_mem = M.Sigma.assigned s1.mem s2.mem w_mem in
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

    let domain s = 
      Heap.Set.union 
	(Heap.Set.union 
	   (domain_var (SIGMA.domain s.vars))
	   (domain_alloc (ALLOC.domain s.alloc)))
	(domain_mem (M.Sigma.domain s.mem))

    let pretty fmt s = 
      Format.fprintf fmt "@[<hov 2>{X:@[%a@]@ T:@[%a@]@ M:@[%a@]}@]"
	SIGMA.pretty s.vars 
	ALLOC.pretty s.alloc
	M.Sigma.pretty s.mem

  end

  let get_var s x = SIGMA.get s.vars x
  let get_term s x = e_var (get_var s x)

  (* -------------------------------------------------------------------------- *)
  (* ---  Location                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  type loc =
    | Mloc of M.loc
    | Fref of varinfo            (* by-reference *)
    | Fval of varinfo * ofs list (* in logic *)
    | Mval of varinfo * ofs list (* in heap  *)

  and ofs = Field of fieldinfo | Index of c_object * term

  type segment = loc rloc

  let rec pp_ofs fmt = function
    | [] -> ()
    | Field f :: ofs -> Format.fprintf fmt ".%s" f.fname ; pp_ofs fmt ofs
    | Index(_,k) :: ofs -> Format.fprintf fmt "[%a]" F.pp_term k ; pp_ofs fmt ofs

  let pretty fmt = function
    | Mloc l -> Format.fprintf fmt "ptr(%a)" M.pretty l
    | Fref x -> Format.fprintf fmt "ref(%a)" VAR.pretty x
    | Fval(x,ofs) ->
	Format.fprintf fmt "@[var(%a)%a@]" VAR.pretty x pp_ofs ofs
    | Mval(x,ofs) -> 
	Format.fprintf fmt "@[mem(%a)%a@]" VAR.pretty x pp_ofs ofs

  let rec ofs_vars xs = function
    | [] -> xs
    | Field _ :: ofs -> ofs_vars xs ofs
    | Index(_,k) :: ofs -> ofs_vars (Vars.union xs (F.vars k)) ofs

  let vars = function
    | Mloc l -> M.vars l
    | Fref _ -> Vars.empty
    | Fval(_,ofs) | Mval(_,ofs) -> ofs_vars Vars.empty ofs

  let rec ofs_occurs x = function
    | [] -> false
    | Field _ :: ofs -> ofs_occurs x ofs
    | Index(_,k) :: ofs -> Vars.mem x (F.vars k) || ofs_occurs x ofs

  let occurs x = function
    | Mloc l -> M.occurs x l
    | Fref _ -> false
    | Fval(_,ofs) | Mval(_,ofs) -> ofs_occurs x ofs

  (* -------------------------------------------------------------------------- *)
  (* ---  Location Constructors                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let null = Mloc M.null

  let literal ~eid cst = Mloc (M.literal ~eid cst)

  let cvar x = 	match V.param x with
    | ByRef -> Fref x
    | ByValue -> Fval(x,[])
    | InHeap -> Mval(x,[])

  let mloc x ofs = 
    List.fold_left
      (fun l d -> match d with
	 | Field f -> M.field l f
	 | Index(e,k) -> M.shift l e k)
      (M.cvar x) ofs
      
  let mloc_of_loc = function
    | Mloc l -> l
    | Fref _ -> 
	(* x should never be ByRef when its address is taken *)
	Wp_parameters.fatal "Addr of ref-var"
    | Fval(x,ofs) | Mval(x,ofs) -> mloc x ofs

  let pointer_loc p = Mloc (M.pointer_loc p)
  let pointer_val l = M.pointer_val (mloc_of_loc l)

  let field l f = match l with
    | Mloc l -> Mloc (M.field l f)
    | Fref _ -> Wp_parameters.fatal "Field of ref-var"
    | Fval(x,ofs) -> Fval(x,ofs @ [Field f])
    | Mval(x,ofs) -> Mval(x,ofs @ [Field f])

  let rec index ofs obj k =
    match ofs with
      | [] -> [Index(obj,k)]
      | [Index(elt,i)] when Ctypes.equal elt obj -> [Index(elt,e_add i k)]
      | delta :: ofs -> delta :: index ofs obj k

  let shift l obj k = match l with
    | Mloc l -> Mloc (M.shift l obj k)
    | Fref _ -> Wp_parameters.fatal "Index of ref-var"
    | Fval(x,ofs) -> Fval(x,index ofs obj k)
    | Mval(x,ofs) -> Mval(x,index ofs obj k)

  let base_addr = function
    | Mloc l -> Mloc (M.base_addr l)
    | Fref _ -> Wp_parameters.fatal "Base-addr of ref-var"
    | Fval(x,_) -> Fval(x,[])
    | Mval(x,_) -> Mval(x,[])

  let block_length sigma obj = function
    | Mloc l -> M.block_length sigma.mem obj l
    | Fref _ -> Wp_parameters.fatal "Block-length of ref-var"
    | Fval(x,_) | Mval(x,_) -> F.e_int (Ctypes.sizeof_typ (VAR.typ_of_param x))

  let cast obj l = Mloc(M.cast obj (mloc_of_loc l))
  let loc_of_int e a = Mloc(M.loc_of_int e a)
  let int_of_loc i l = M.int_of_loc i (mloc_of_loc l)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Load                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec access a = function 
    | [] -> a
    | Field f :: ofs -> access (e_getfield a (Cfield f)) ofs
    | Index(_,k) :: ofs -> access (e_get a k) ofs

  let rec update a ofs v = match ofs with
    | [] -> v
    | Field f :: ofs ->
	let phi = Cfield f in
	let a_f = F.e_getfield a phi in
	let a_f_v = update a_f ofs v in
	F.e_setfield a phi a_f_v
    | Index(_,k) :: ofs ->
	let a_k = F.e_get a k in
	let a_k_v = update a_k ofs v in
	F.e_set a k a_k_v

  let mload sigma obj l = 
    Cvalues.map_value (fun l -> Mloc l) (M.load sigma.mem obj l)

  let load sigma obj = function
    | Fref x -> Loc (Fval(x,[]))
    | Fval(x,ofs) -> Val (access (get_term sigma x) ofs)
    | (Mloc _ | Mval _) as l -> mload sigma obj (mloc_of_loc l)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Store                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let mstored seq obj l v =
    M.stored { pre = seq.pre.mem ; post = seq.post.mem } obj l v

  let stored seq obj l v = match l with
    | Fref _  -> Wp_parameters.fatal "Write to ref-var"
    | Fval(x,ofs) ->
	let v1 = get_term seq.pre x in
	let v2 = get_term seq.post x in
	[ F.p_equal v2 (update v1 ofs v) ]
    | (Mloc _ | Mval _) as l -> mstored seq obj (mloc_of_loc l) v

  let copied seq obj l1 l2 = 
    let v = match load seq.pre obj l2 with
      | Val r -> r
      | Loc l -> pointer_val l
    in stored seq obj l1 v

  (* -------------------------------------------------------------------------- *)
  (* ---  Pointer Comparison                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let is_null = function
    | Mloc l -> M.is_null l
    | Fref _ | Fval _ | Mval _ -> F.p_false

  let rec offset = function
    | [] -> e_zero
    | Field f :: ofs -> e_add (e_int (Ctypes.field_offset f)) (offset ofs)
    | Index(obj,k)::ofs -> e_add (e_fact (Ctypes.sizeof_object obj) k) (offset ofs)

  let loc_diff obj a b =
    match a , b with
      | Mloc l1 , Mloc l2 -> M.loc_diff obj l1 l2
      | Fref x , Fref y when Varinfo.equal x y -> e_zero
      | (Fval(x,p)|Mval(x,p)) , (Fval(y,q)|Mval(y,q)) when Varinfo.equal x y ->
	  e_div (e_sub (offset p) (offset q)) (e_int (Ctypes.sizeof_object obj))
      | Mval _ , _ | _ , Mval _
      | Fval _ , _ | _ , Fval _
      | Fref _ , _ | _ , Fref _
	  -> Warning.error ~source:"Reference Variable Model" 
	  "Uncomparable locations %a and %a" pretty a pretty b
	    
  let loc_compare lcmp icmp same a b =
    match a , b with
      | Mloc l1 , Mloc l2 -> lcmp l1 l2
      | Fref x , Fref y -> 
	  if Varinfo.equal x y then same else p_not same
      | (Fval(x,p)|Mval(x,p)) , (Fval(y,q)|Mval(y,q)) ->
	  if Varinfo.equal x y then icmp (offset p) (offset q) else p_not same
      | (Fval _|Mval _|Mloc _) , (Fval _|Mval _|Mloc _) -> lcmp (mloc_of_loc a) (mloc_of_loc b)
      | Fref _ , _ | _ , Fref _ -> p_not same

  let loc_eq = loc_compare M.loc_eq F.p_equal F.p_true
  let loc_lt = loc_compare M.loc_lt F.p_lt F.p_false
  let loc_leq = loc_compare M.loc_leq F.p_leq F.p_true
  let loc_neq = loc_compare M.loc_neq F.p_neq F.p_false

  (* -------------------------------------------------------------------------- *)
  (* ---  Validity                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let size_of_array_type typ = match object_of typ with
    | C_int _ | C_float _ | C_pointer _ | C_comp _ -> assert false
    | C_array { arr_flat=None } -> 
	if not (Wp_parameters.ExternArrays.get ())
	then Wp_parameters.warning ~once:true
	  "Validity of unsized array not implemented yet (considered valid)." ;
	None
    | C_array { arr_flat=Some s } -> Some (e_int s.arr_size)

  (* offset *)
	
  let first_index = Some e_zero
	
  let range_offset typ k = 
    match size_of_array_type typ with
      | None -> p_positive k
      | Some s -> p_and (p_positive k) (p_lt k s)

  let rec valid_offset typ = function
    | [] -> p_true
    | Field f :: ofs -> valid_offset f.ftype ofs
    | Index(_,k) :: ofs -> 
	let h = range_offset typ k in
	p_and h (valid_offset (Cil.typeOf_array_elem typ) ofs)

  let rec valid_offsetrange typ p a b = match p with
    | Field f :: ofs -> valid_offsetrange f.ftype ofs a b
    | [Index(obj,k)] ->
	let te = Cil.typeOf_array_elem typ in
	let elt = Ctypes.object_of te in
	if Ctypes.equal elt obj then
	  let n = size_of_array_type typ in
	  let a = Vset.bound_shift a k in
	  let b = Vset.bound_shift b k in
	  let p_inf = Vset.ordered ~limit:true ~strict:false first_index a in
	  let p_sup = Vset.ordered ~limit:true ~strict:true b n in
	  p_and p_inf p_sup
	else
	  let rg = range_offset typ k in
	  let te = Cil.typeOf_array_elem typ in
	  p_and rg (valid_offsetrange te [] a b)
    | Index(_,k) :: ofs -> 
	let rg = range_offset typ k in
	let te = Cil.typeOf_array_elem typ in
	p_and rg (valid_offsetrange te ofs a b)
    | [] ->
	let n = size_of_array_type typ in
	let p_inf = Vset.ordered ~limit:true ~strict:false first_index a in
	let p_sup = Vset.ordered ~limit:true ~strict:true b n in
	p_and p_inf p_sup

  (* varinfo + offset *)

  let valid_base sigma x =
    if x.vglob then p_true else 
      p_bool (ALLOC.value sigma.alloc x)

  let valid_path sigma x t ofs =
    p_and 
      (valid_base sigma x)
      (valid_offset t ofs)

  let valid_pathrange sigma x t ofs a b =
    p_and 
      (valid_base sigma x) 
      (p_imply 
	 (Vset.ordered ~limit:true ~strict:false a b) 
	 (valid_offsetrange t ofs a b))

  (* segment *)

  let valid_loc sigma acs obj = function
    | Fref _ -> p_true
    | Fval(x,p) | Mval(x,p) -> valid_path sigma x (VAR.typ_of_param x) p
    | Mloc _ as l -> M.valid sigma.mem acs (Rloc(obj,mloc_of_loc l))
	
  let valid_range sigma acs l obj a b = match l with
    | Fref _ -> Wp_parameters.fatal "range of ref-var"
    | Fval(x,p) | Mval(x,p) -> valid_pathrange sigma x (VAR.typ_of_param x) p a b
    | Mloc _ as l -> M.valid sigma.mem acs (Rrange(mloc_of_loc l,obj,a,b))

  let valid_array sigma acs l obj s = match l with
    | Fref _ -> Wp_parameters.fatal "range of ref-var"
    | Fval(x,p) | Mval(x,p) -> valid_path sigma x (VAR.typ_of_param x) p
    | Mloc _ as l -> 
	let a = Some e_zero in
	let b = Some (e_int (s-1)) in
	M.valid sigma.mem acs (Rrange(mloc_of_loc l,obj,a,b))
	  
  let valid sigma acs = function
    | Rloc(obj,l) -> valid_loc sigma acs obj l
    | Rarray(l,obj,s) -> valid_array sigma acs l obj s
    | Rrange(l,obj,a,b) -> valid_range sigma acs l obj a b

  (* -------------------------------------------------------------------------- *)
  (* ---  Scope                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let is_mem x = match V.param x with InHeap -> true | ByRef | ByValue -> false
  let is_ref x = match V.param x with ByRef -> true | ByValue | InHeap -> false

  let alloc_var ta xs v = 
    TALLOC.Set.fold
      (fun x hs -> p_equal (ALLOC.value ta x) v :: hs)
      xs []

  let allocates ta_out xs valid (* of introduced variables *) =
    let xs = List.filter (fun x -> not (is_ref x)) xs in
    if xs = [] then ta_out , [] 
    else
      let xs_all = List.fold_right TALLOC.Set.add xs TALLOC.Set.empty in
      let ta_in = ALLOC.havoc ta_out xs_all in
      let h_out = alloc_var ta_out xs_all (if valid then e_false else e_true) in
      let h_in  = alloc_var ta_in  xs_all (if valid then e_true else e_false) in
      begin
	ta_in , h_in @ h_out
      end

  let scope_vars ta sc xs = 
    match sc with
      | Mcfg.SC_Global | Mcfg.SC_Function_in -> ta , []
      | Mcfg.SC_Function_frame | Mcfg.SC_Block_in -> allocates ta xs false
      | Mcfg.SC_Function_out | Mcfg.SC_Block_out -> allocates ta xs true
      
  let scope sigma sc xs = 
    let xmem = List.filter is_mem xs in
    let smem , hmem = M.scope sigma.mem sc xmem in
    let ta , hvars = scope_vars sigma.alloc sc xs in
    { vars = sigma.vars ; alloc = ta ; mem = smem } , hvars @ hmem

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment                                                           --- *)
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
    | Index(_,k) -> let u = Some k in Drange(u,u)

  let delta ofs = List.map dofs ofs

  let rec range ofs obj a b = 
    match ofs with
      | [] -> [ Drange(a,b) ]
      | [Index(elt,k)] when Ctypes.equal elt obj ->
	  [ Drange( Vset.bound_shift a k , Vset.bound_shift b k ) ]
      | d :: ofs -> dofs d :: range ofs obj a b

  let dsize s = Drange(Some (e_int 0) , Some (e_int (s-1)))
  let rsize ofs s = delta ofs @ [ dsize s ]



  let locseg = function

    | Rloc(_,Fref x) -> Rseg x
    | Rarray(Fref _,_,_) | Rrange(Fref _,_,_,_) -> 
	Wp_parameters.fatal "range of ref-var"

    | Rloc(obj,Mloc l) -> Lseg (Rloc(obj,l))
    | Rloc(_,Fval(x,ofs)) -> Fseg(x,delta ofs)

    | Rarray(Mloc l,obj,s) -> Lseg (Rarray(l,obj,s))
    | Rarray(Fval(x,ofs),_,s) -> Fseg(x,rsize ofs s)

    | Rrange(Mloc l,obj,a,b) -> Lseg (Rrange(l,obj,a,b))
    | Rrange(Fval(x,ofs),obj,a,b) -> Fseg(x,range ofs obj a b)

	(* in M: *)
    | Rloc(obj,Mval(x,ofs)) -> 
	Mseg(Rloc(obj,mloc x ofs),x,delta ofs)
    | Rarray(Mval(x,ofs),obj,s) ->
	Mseg(Rarray(mloc x ofs,obj,s),x,rsize ofs s)
    | Rrange(Mval(x,ofs),obj,a,b) -> 
	Mseg(Rrange(mloc x ofs,obj,a,b),x,range ofs obj a b)

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
  (* ---  Segment Assignation                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let sloc_descr = function
    | Sloc l -> [],l,p_true
    | Sdescr(xs,l,p) -> xs,l,p
    | Sarray(l,obj,s) ->
	let x = Lang.freshvar ~basename:"k" Qed.Logic.Int in
	let k = e_var x in
	[x],shift l obj k,Vset.in_size k s
    | Srange(l,obj,a,b) -> 
	let x = Lang.freshvar ~basename:"k" Qed.Logic.Int in
	let k = e_var x in
	[x],shift l obj k,Vset.in_range k a b

  let floc_path = function
    | Mloc _ | Mval _ -> assert false (* Filtered in assigned *)
    | Fref _ -> Wp_parameters.fatal "assigned of ref-var"
    | Fval(x,ofs) -> x,ofs

  let rec assigned_path 
      (hs : pred list) (* collector of properties *)
      (xs : var list)  (* variable quantifying by the assigned location *)
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
		   let cg = Cfield f in
		   let ag = e_getfield a cg in
		   let bg = e_getfield b cg in
		   let eqg = p_forall ys (p_equal ag bg) in
		   eqg :: hs
	      ) hs f.fcomp.cfields

	| Index(_,e) :: ofs ->
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
		 any indice different from e is disjoint. 
		 explore also deeply with index [e]. *)
	      let ae = e_get a e in
	      let be = e_get b e in
	      let ek = p_neq e k in
	      let eqk = p_forall (y::ys) (p_imply ek (p_equal ak bk)) in
	      assigned_path (eqk :: hs) xs ys ae be ofs
	  
  let assigned s obj = function

    (* Optimisation for functional updates in one variable *)
    | Sloc(Fval(_,_::_) as loc) ->
	let v = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
	stored s obj loc (e_var v)

    (* Optimisation for full update of one array variable *)
    | Sarray(Fval(_,[]),_,_) -> []
    | Sarray(Fval(x,ofs),_,_) ->
	let a = get_term s.pre x in
	let b = get_term s.post x in
	assigned_path [] [] [] a b ofs

    | sloc ->

	(* Transfer the job to memory model M if sloc is in M *)
	try 
	  let sloc = Cvalues.map_sloc 
	    (function
	       | (Mloc _ | Mval _) as l -> mloc_of_loc l
	       | Fval _ | Fref _ -> raise Exit
	    ) sloc in
	  M.assigned { pre=s.pre.mem ; post=s.post.mem } obj sloc
	with Exit ->

	  (* Otherwize compute a set of equalities for each sub-path
	     of the assigned location *)

	  let xs,l,p = sloc_descr sloc in
	  let x,ofs = floc_path l in
	  let a = get_term s.pre x in
	  let b = get_term s.post x in
	  let a_ofs = access a ofs in
	  let b_ofs = access b ofs in
	  let p_sloc = p_forall xs (p_imply (p_not p) (p_equal a_ofs b_ofs)) in
	  assigned_path [p_sloc] xs [] a b ofs

  (* -------------------------------------------------------------------------- *)
  (* ---  Domain                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let domain obj = function
    | (Mloc _ | Mval _) as l -> 
	M.Heap.Set.fold 
	  (fun m s -> Heap.Set.add (Mem m) s) 
	  (M.domain obj (mloc_of_loc l)) Heap.Set.empty
    | Fref x | Fval(x,_) ->
	Heap.Set.singleton (Var x)

  (* -------------------------------------------------------------------------- *)

end
