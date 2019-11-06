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

open Cil_types
open Cil_datatype
open Definitions
open Ctypes
open Lang
open Lang.F
open Sigs

(* -------------------------------------------------------------------------- *)
(* --- Compound Loader                                                    --- *)
(* -------------------------------------------------------------------------- *)

let cluster () =
  Definitions.cluster ~id:"Compound" ~title:"Memory Compound Loader" ()

module type Model =
sig

  module Chunk : Chunk
  module Sigma : Sigma with type chunk = Chunk.t

  val name : string

  type loc
  val sizeof : c_object -> int
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc

  val to_addr : loc -> term
  val to_region_pointer : loc -> int * term
  val of_region_pointer : int -> c_object -> term -> loc

  val domain : c_object -> loc -> Sigma.domain
  val frames : c_object -> loc -> Chunk.t -> frame list

  val last : Sigma.t -> c_object -> loc -> term

  val havoc : c_object -> loc -> length:term ->
    Chunk.t -> fresh:term -> current:term -> term

  val eqmem : c_object -> loc -> Chunk.t -> term -> term -> pred

  val eqmem_forall :
    c_object -> loc -> Chunk.t -> term -> term -> var list * pred * pred

  val load_int : Sigma.t -> c_int -> loc -> term
  val load_float : Sigma.t -> c_float -> loc -> term
  val load_pointer : Sigma.t -> typ -> loc -> loc

  val store_int : Sigma.t -> c_int -> loc -> term -> Chunk.t * term
  val store_float : Sigma.t -> c_float -> loc -> term -> Chunk.t * term
  val store_pointer : Sigma.t -> typ -> loc -> term -> Chunk.t * term

end

module Make (M : Model) =
struct

  type chunk = M.Chunk.t

  module Chunk = M.Chunk
  module Sigma = M.Sigma
  module Domain = M.Sigma.Chunk.Set

  let signature ft =
    let s = Sigma.create () in
    let xs = ref [] in
    let cs = ref [] in
    Domain.iter
      (fun c ->
         cs := c :: !cs ;
         xs := (Sigma.get s c) :: !xs ;
      ) ft ;
    List.rev !xs , List.rev !cs , s

  let pp_rid fmt r = if r <> 0 then Format.fprintf fmt "_R%03d" r

  let loadrec = ref (fun _ _ _ -> assert false)

  (* -------------------------------------------------------------------------- *)
  (* --- Frame Lemmas for Compound Access                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let memories sigma chunks = List.map (Sigma.value sigma) chunks
  let assigned sigma c m chunks =
    List.map
      (fun c0 -> if Chunk.equal c0 c then m else Sigma.value sigma c0)
      chunks

  let frame_lemmas phi obj loc params chunks =
    begin
      let prefix = Fun.debug phi in
      let sigma = Sigma.create () in
      List.iter
        (fun chunk ->
           List.iter
             (fun (name,triggers,conditions,m1,m2) ->
                let mem1 = assigned sigma chunk m1 chunks in
                let mem2 = assigned sigma chunk m2 chunks in
                let value1 = e_fun phi (params @ mem1) in
                let value2 = e_fun phi (params @ mem2) in
                let vars1 = F.vars value1 in
                let vars2 = F.vars value2 in
                let l_triggers =
                  if Vars.subset vars1 vars2 then
                    [ (Trigger.of_term value2 :: triggers ) ]
                  else
                  if Vars.subset vars2 vars1 then
                    [ (Trigger.of_term value1 :: triggers ) ]
                  else
                    [ (Trigger.of_term value1 :: triggers );
                      (Trigger.of_term value2 :: triggers ) ]
                in
                let l_name = Pretty_utils.sfprintf "%s_%s_%a"
                    prefix name Chunk.pretty chunk in
                let l_lemma = F.p_hyps conditions (p_equal value1 value2) in
                Definitions.define_lemma {
                  l_assumed = true ;
                  l_name ; l_types = 0 ;
                  l_triggers ;
                  l_forall = F.p_vars l_lemma ;
                  l_lemma = l_lemma ;
                  l_cluster = cluster () ;
                }
             ) (M.frames obj loc chunk)
        ) chunks
    end

  (* -------------------------------------------------------------------------- *)
  (* ---  Compound Loader                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  module COMP_KEY =
  struct
    type t = int * compinfo
    let compare (r,c) (r',c') = if r=r' then Compinfo.compare c c' else r-r'
    let pretty fmt (r,c) = Format.fprintf fmt "%d:%a" r Compinfo.pretty c
  end

  module COMP = WpContext.Generator(COMP_KEY)
      (struct
        let name = M.name ^ ".COMP"
        type key = int * compinfo
        type data = lfun * chunk list

        let generate (r,c) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = C_comp c in
          let loc = M.of_region_pointer r obj v in (* t_pointer -> loc *)
          let domain = M.domain obj loc in
          let result = Lang.tau_of_comp c in
          let lfun = Lang.generated_f ~result "Load%a_%s" pp_rid r (Lang.comp_id c) in
          (* Since its a generated it is the unique name given *)
          let xms,chunks,sigma = signature domain in
          let def = List.map
              (fun f ->
                 Cfield f , !loadrec sigma (object_of f.ftype) (M.field loc f)
              ) c.cfields in
          let dfun = Definitions.Function( result , Def , e_record def ) in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = x :: xms ;
            d_definition = dfun ;
            d_cluster = cluster () ;
          } ;
          frame_lemmas lfun obj loc [v] chunks ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  (* -------------------------------------------------------------------------- *)
  (* ---  Array Loader                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  module ARRAY_KEY =
  struct
    type t = int * arrayinfo * Matrix.matrix
    let pretty fmt (r,_,m) =
      Format.fprintf fmt "%d:%a" r Matrix.NATURAL.pretty m
    let compare (r1,_,m1) (r2,_,m2) =
      if r1 = r2 then Matrix.NATURAL.compare m1 m2 else r1-r2
  end

  module ARRAY = WpContext.Generator(ARRAY_KEY)
      (struct
        open Matrix
        let name = M.name ^ ".ARRAY"
        type key = int * arrayinfo * Matrix.matrix
        type data = lfun * chunk list

        let generate (r,ainfo,(obj_e,ds)) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj_a = C_array ainfo in
          let loc = M.of_region_pointer r obj_a v in (* t_pointer -> loc *)
          let domain = M.domain obj_a loc in
          let result = Matrix.tau obj_e ds in
          let lfun = Lang.generated_f ~result "Array%a%s_%s" pp_rid r
              (Matrix.id ds) (Matrix.natural_id obj_e) in
          let prefix = Lang.Fun.debug lfun in
          let axiom = prefix ^ "_access" in
          let xmem,chunks,sigma = signature domain in
          let denv = Matrix.denv ds in
          let phi = e_fun lfun (v :: denv.size_val @ List.map e_var xmem) in
          let va = List.fold_left e_get phi denv.index_val in
          let ofs = e_sum denv.index_offset in
          let vm = !loadrec sigma obj_e (M.shift loc obj_e ofs) in
          let lemma = p_hyps denv.index_range (p_equal va vm) in
          let cluster = cluster () in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = x :: denv.size_var @ xmem ;
            d_definition = Logic result ;
            d_cluster = cluster ;
          } ;
          Definitions.define_lemma {
            l_assumed = true ;
            l_name = axiom ; l_types = 0 ;
            l_forall = F.p_vars lemma ;
            l_triggers = [[Trigger.of_term va]] ;
            l_lemma = lemma ;
            l_cluster = cluster ;
          } ;
          if denv.monotonic then
            begin
              let ns = List.map F.e_var denv.size_var in
              frame_lemmas lfun obj_a loc (v::ns) chunks
            end ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  (* -------------------------------------------------------------------------- *)
  (* --- Loader                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let load_comp sigma comp loc =
    let r , p = M.to_region_pointer loc in
    let f , m = COMP.get (r,comp) in
    F.e_fun f (p :: memories sigma m)

  let load_array sigma a loc =
    let d = Matrix.of_array a in
    let r , p = M.to_region_pointer loc in
    let f , m = ARRAY.get (r,a,d) in
    F.e_fun f (p :: Matrix.size d @ memories sigma m)

  let loadvalue sigma obj loc =
    match obj with
    | C_int i -> M.load_int sigma i loc
    | C_float f -> M.load_float sigma f loc
    | C_pointer t -> snd @@ M.to_region_pointer @@ M.load_pointer sigma t loc
    | C_comp c -> load_comp sigma c loc
    | C_array a -> load_array sigma a loc

  let load sigma obj loc =
    let open Sigs in
    match obj with
    | C_int i -> Val (M.load_int sigma i loc)
    | C_float f -> Val (M.load_float sigma f loc)
    | C_pointer t -> Loc (M.load_pointer sigma t loc)
    | C_comp c -> Val (load_comp sigma c loc)
    | C_array a -> Val (load_array sigma a loc)

  let () = loadrec := loadvalue

  (* -------------------------------------------------------------------------- *)
  (* --- Havocs                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let havoc_length s obj loc length =
    let ps = ref [] in
    Domain.iter
      (fun chunk ->
         let pre = Sigma.value s.pre chunk in
         let post = Sigma.value s.post chunk in
         let tau = Chunk.tau_of_chunk chunk in
         let basename = Chunk.basename_of_chunk chunk ^ "_undef" in
         let fresh = F.e_var (Lang.freshvar ~basename tau) in
         let havoc = M.havoc obj loc ~length chunk ~fresh ~current:pre in
         ps := Set(post,havoc) :: !ps
      ) (M.domain obj loc) ; !ps

  let havoc seq obj loc = havoc_length seq obj loc F.e_one

  (* -------------------------------------------------------------------------- *)
  (* --- Stored & Copied                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let updated seq phi alpha loc value =
    let chunk,mem = phi seq.pre alpha loc value in
    [Set(Sigma.value seq.post chunk,mem)]

  let stored seq obj loc value =
    match obj with
    | C_int i -> updated seq M.store_int i loc value
    | C_float f -> updated seq M.store_float f loc value
    | C_pointer ty -> updated seq M.store_pointer ty loc value
    | C_comp _ | C_array _ ->
        Set(loadvalue seq.post obj loc, value) :: havoc seq obj loc

  let copied s obj p q = stored s obj p (loadvalue s.pre obj q)

  (* -------------------------------------------------------------------------- *)
  (* --- Assigned                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let assigned_loc seq obj loc =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
        let x = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
        stored seq obj loc (e_var x)
    | C_comp _ | C_array _ ->
        havoc seq obj loc

  let assigned_range s obj l a b =
    havoc_length s obj (M.shift l obj a) (e_range a b)

  let assigned seq obj = function
    | Sloc loc -> assigned_loc seq obj loc
    | Sdescr(xs,loc,condition) ->
        let ps = ref [] in
        Domain.iter
          (fun c ->
             let m1 = Sigma.value seq.pre c in
             let m2 = Sigma.value seq.post c in
             let p,separated,equal = M.eqmem_forall obj loc c m1 m2 in
             let sep_from_all = F.p_forall xs (F.p_imply condition separated) in
             let phi = F.p_forall p (F.p_imply sep_from_all equal) in
             ps := Assert phi :: !ps
          ) (M.domain obj loc) ; !ps
    | Sarray(loc,obj,n) ->
        assigned_range seq obj loc e_zero (e_int (n-1))
    | Srange(loc,obj,u,v) ->
        let a = match u with Some a -> a | None -> e_zero in
        let b = match v with Some b -> b | None -> M.last seq.pre obj loc in
        assigned_range seq obj loc a b

  (* -------------------------------------------------------------------------- *)

end
