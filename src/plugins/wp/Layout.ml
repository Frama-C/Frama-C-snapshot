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

open Pretty_utils
open Cil_datatype
open Cil_types

module Wp = Wp_parameters

module type Data =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : t formatter
end

(* -------------------------------------------------------------------------- *)
(* --- Offsets                                                            --- *)
(* -------------------------------------------------------------------------- *)

type offset =
  | Field of fieldinfo
  | Index of typ * int

module Offset =
struct
  type t = offset
  let compare a b =
    if a == b then 0 else match a,b with
      | Field f, Field g -> Fieldinfo.compare f g
      | Field _ , _ -> (-1)
      | _ , Field _ -> 1
      | Index(ta,n) , Index(tb,m) ->
          let cmp = Typ.compare ta tb in
          if cmp <> 0 then cmp else Pervasives.compare n m

  let equal a b = (compare a b = 0)

  let pretty fmt = function
    | Field fd -> Format.fprintf fmt "{%s}.%a" fd.fcomp.cname Fieldinfo.pretty fd
    | Index(ty,n) -> Format.fprintf fmt "{%a}[%d]" Typ.pretty ty n

  let typeof = function
    | Field f -> f.ftype
    | Index(ty,_) -> ty

  let field fd = Field fd

  let index ty =
    match Cil.unrollType ty with
    | TArray(te,n,_,_) ->
        begin
          match Extlib.opt_bind Ctypes.get_int n with
          | None -> failwith "Wp.Layout: unkown array size"
          | Some n -> Index(te,n)
        end
    | _ -> failwith "Wp.Layout: not an array-type"

  let rec typeof_chain ty = function [] -> ty | _::ds -> typeof_chain ty ds

  let rec pp_chain ty fmt = function
    | [] -> ()
    | d::ds ->
        let next =
          Format.pp_print_cut fmt () ;
          match d with
          | Index(t,n) when Typ.equal t ty ->
              Format.fprintf fmt "[%d]" n ; t
          | d -> Format.fprintf fmt "%a" pretty d ; typeof d
        in pp_chain next fmt ds

  module H = Compinfo.Hashtbl

  type cache = typ H.t
  let cache () : cache = H.create 0

  let typ_of_comp cache comp =
    try H.find cache comp with Not_found ->
      let typ = TComp(comp,Cil.empty_size_cache (),[]) in
      H.add cache comp typ ; typ

  let field_offset cache fd =
    let typ = typ_of_comp cache fd.fcomp in
    let offset = Cil_types.(Field(fd,NoOffset)) in
    Cil.bitsOffset typ offset

  let range_field cache fd =
    let typ = typ_of_comp cache fd.fcomp in
    let offset = Cil_types.(Field(fd,NoOffset)) in
    Cil.bitsOffset typ offset , Cil.bitsSizeOf typ

  let range_index typ n =
    let len = Cil.bitsSizeOf typ * n in (0 , len) , len

  let range cache = function
    | Field fd -> range_field cache fd
    | Index(typ,n) -> range_index typ n

  let sizeof = function
    | Field fd -> Cil.bitsSizeOf fd.ftype
    | Index(ty,n) -> Cil.bitsSizeOf ty * n

  let container cache = function
    | Index(ty,n) -> Cil.bitsSizeOf ty * n
    | Field fd -> Cil.bitsSizeOf (typ_of_comp cache fd.fcomp)

end

(* -------------------------------------------------------------------------- *)
(* --- Deref                                                              --- *)
(* -------------------------------------------------------------------------- *)

type alias = NotUsed | NotAliased | Aliased
type usage = Value | Deref | Array
type deref = usage * typ

module Alias =
struct
  let use = function NotUsed | NotAliased -> NotAliased | Aliased -> Aliased
  let is_aliased = function NotUsed | NotAliased -> false | Aliased -> true
  let merge a b =
    match a,b with
    | Aliased,_ | _,Aliased -> Aliased
    | NotAliased,NotAliased -> NotAliased
    | NotUsed,c | c,NotUsed -> c
  let alias a b =
    match a,b with
    | NotUsed,c | c,NotUsed -> c
    | _ -> Aliased
  let to_string = function
    | NotUsed -> "not used"
    | NotAliased -> "not aliased"
    | Aliased -> "aliased"
  let pretty fmt a = Format.pp_print_string fmt (to_string a)
end

module Usage =
struct
  let pretty fmt = function
    | Value -> ()
    | Deref -> Format.pp_print_char fmt '*'
    | Array -> Format.pp_print_string fmt "[]"
  let order = function Value -> 0 | Deref -> 1 | Array -> 2
  let merge a b = if order a < order b then b else a
  let is_aliased = function Value -> false | Deref | Array -> true
  let is_shifted = function Value | Deref -> false | Array -> true
end

module Deref =
struct
  type t = deref

  let pretty fmt (usage,typ) =
    Format.fprintf fmt "{%a}" Typ.pretty typ ;
    Usage.pretty fmt usage

  let compare ((da,ta):t) ((db,tb):t) =
    let cmp = Pervasives.compare da db in
    if cmp <> 0 then cmp else Typ.compare ta tb

  let equal a b = (compare a b = 0)
end

(* -------------------------------------------------------------------------- *)
(* --- Access                                                             --- *)
(* -------------------------------------------------------------------------- *)

type lvalue =
  | Eval of exp
  | Tval of term
  | Assigned of stmt

module Lvalue =
struct

  type t = lvalue

  let order = function Eval _ -> 0 | Tval _ -> 1 | Assigned _ -> 2

  let compare a b = if a == b then 0 else match a,b with
      | Eval x , Eval y -> Exp.compare x y
      | Tval x , Tval y -> Term.compare x y
      | Assigned a , Assigned b -> Stmt.compare a b
      | _ -> order a - order b

  let equal a b = a == b || match a,b with
    | Eval x , Eval y -> Exp.equal x y
    | Tval x , Tval y -> Term.equal x y
    | Assigned a , Assigned b -> Stmt.equal a b
    | _ -> false

  let pretty fmt = function
    | Eval x -> Exp.pretty fmt x
    | Tval t -> Term.pretty fmt t
    | Assigned { skind = Instr(Set(lv,_,_)) }
    | Assigned { skind = Instr(Call(Some lv,_,_,_)) } -> Lval.pretty fmt lv
    | Assigned { skind = Instr(Local_init(x,_,_)) } -> Varinfo.pretty fmt x
    | Assigned stmt -> Format.fprintf fmt "stmt:s%d" stmt.sid

end

module Mode(OPT : sig val get : unit -> bool end) =
struct
  let default = OPT.get
  let merge a b = if default () then a && b else a || b
end

module RW = Mode(Wp.Region_rw)
module Flat = Mode(Wp.Region_flat)
module Pack = Mode(Wp.Region_pack)

(* -------------------------------------------------------------------------- *)
(* --- Data Layout                                                        --- *)
(* -------------------------------------------------------------------------- *)

type 'a value =
  | Int of Ctypes.c_int
  | Float of Ctypes.c_float
  | Pointer of 'a

module Value =
struct

  let compare phi u v =
    if u == v then 0 else match u,v with
      | Int a , Int b -> Ctypes.compare_c_int a b
      | Int _ , _ -> (-1)
      | _ , Int _ -> 1
      | Float a , Float b -> Ctypes.compare_c_float a b
      | Float _ , _ -> (-1)
      | _ , Float _ -> 1
      | Pointer ra , Pointer rb -> phi ra rb

  let equal phi a b =
    match a,b with
    | Pointer ra , Pointer rb -> phi ra rb
    | Int a , Int b -> a = b
    | Float a , Float b -> a = b
    | _ -> false

  let pretty pp fmt = function
    | Int iota -> Ctypes.pp_int fmt iota
    | Float flt -> Ctypes.pp_float fmt flt
    | Pointer r -> Format.fprintf fmt "ptr(%a)" pp r

  let sizeof = function
    | Int iota -> Ctypes.i_bits iota
    | Float flt -> Ctypes.f_bits flt
    | Pointer _ -> Ctypes.p_bits ()

  let pointed = function
    | Int _ | Float _ -> None
    | Pointer r -> Some r

  let merge mu a b =
    match a,b with
    | Int i , Int j when i = j -> Some a
    | Float f , Float g when f = g -> Some a
    | Pointer r , Pointer r' -> Some(Pointer(mu r r'))
    | _ -> None

end

module Matrix =
struct

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  let pretty fmt = function
    | [] -> ()
    | d::ds ->
        Format.fprintf fmt "@[<hov 1>[%d" d ;
        List.iter (fun d -> Format.fprintf fmt ",@,%d" d) ds ;
        Format.fprintf fmt "]@]"

  let rec sizeof n = function [] -> n | d::ds -> sizeof (n*d) ds

  let array ds n = if n = 1 then ds else ds @ [n]

  (* Assumes s divides len *)
  let join_array s len = let n = len / s in if n = 1 then [] else [n]

  (* Assumes s divides len , computes (s,ds) that fits exactly in len
     with ds maximal prefix of da and db *)
  let rec join s da db len =
    match da , db with
    | d::da , d'::db when d = d' ->
        let s' = s * d in
        if len mod s' = 0 then d :: join s' da db len else
          join_array s len
    | _ -> join_array s len

  let rec merge d1 d2 =
    match d1 , d2 with
    | n::d1 , n'::d2 when n=n' -> n :: merge d1 d2
    | _ -> []

end

(* -------------------------------------------------------------------------- *)
(* --- Range & Overlays                                                   --- *)
(* -------------------------------------------------------------------------- *)

let garbled_key = Wp.register_category "garbled"

type dim = Raw of int | Dim of int * int list

type 'a range = {
  ofs : int ; (* in bits, start from 0 *)
  len : int ;
  reg : 'a ;
  dim : dim ;
}

type 'a overlay = 'a range list

type 'a merger = raw:bool -> 'a -> 'a -> 'a

module Range =
struct

  let pp_dim fmt = function
    | Raw _ -> Format.pp_print_string fmt "raw"
    | Dim(s,ds) -> Format.fprintf fmt "{%d}%a" s Matrix.pretty ds

  let pretty pp fmt { ofs ; len ; reg ; dim } =
    Format.fprintf fmt "%d..%d: %a#%a" ofs (ofs+len-1) pp reg pp_dim dim

  let overlap (type a) (_ : a formatter) (mu : a merger) ra rb =
    let aligned = ref None in
    let ofs = min ra.ofs rb.ofs in
    let len = max (ra.ofs + ra.len) (rb.ofs + rb.len) - ofs in
    begin match ra.dim , rb.dim with
      | Dim(s,da) , Dim(s',db) when s = s' ->
          if len mod s = 0 then
            let ta = abs (ra.ofs - rb.ofs) in
            let tb = abs (ra.ofs + ra.len - rb.ofs - rb.len) in
            if ta mod s = 0 && tb mod s = 0 then
              let reg = mu ~raw:false ra.reg rb.reg in
              let ds = Matrix.join s da db len in
              let dim = Dim(s,ds) in
              aligned := Some { ofs ; len ; reg ; dim }
      | _ -> ()
    end ;
    match !aligned with
    | Some rg -> rg
    | None -> { ofs ; len ; reg = mu ~raw:true ra.reg rb.reg ; dim = Raw len }

  let shift ofs rg = { rg with ofs = rg.ofs + ofs }

  let flatten rg = match rg.dim with
    | Dim(s,ds) when ds <> [] ->
        let n = Matrix.sizeof 1 ds in
        { rg with dim = Dim(s,Matrix.array [] n) }
    | _ -> rg

  let included p n { ofs ; len } =
    ofs <= p && p + n <= ofs + len

end

module Overlay =
struct

  let pretty ?title pp fmt rs =
    begin
      Format.fprintf fmt "@[<hv 0>" ;
      Extlib.may (fun pp -> pp fmt) title ;
      Format.fprintf fmt "@[<hov 2>{" ;
      List.iter
        (fun rg ->
           Format.fprintf fmt "@ @[<hov 2>%a@];" (Range.pretty pp) rg
        ) rs ;
      Format.fprintf fmt "@]@ }@]" ;
    end

  let rec merge (pp : 'a formatter) (mu : _ merger) ova ovb =
    match ova , ovb with
    | [],ovc | ovc,[] -> ovc
    | ra::wa , rb::wb ->
        let sa = ra.ofs + ra.len in
        let sb = rb.ofs + rb.len in
        if sa <= rb.ofs then ra :: merge pp mu wa ovb else
        if sb <= ra.ofs then rb :: merge pp mu ova wb else
        if sa < sb then
          merge pp mu wa (Range.overlap pp mu ra rb :: wb)
        else
          merge pp mu (Range.overlap pp mu ra rb :: wa) wb

  let rec pack eq = function
    | ({ dim = Dim(s ,da) } as ra ) ::
      ({ dim = Dim(s',db) } as rb ) ::
      ovl when eq ra.reg rb.reg && s = s' && ra.ofs + ra.len = rb.ofs ->
        let len = ra.len + rb.len in
        let ds = Matrix.join s da db len in
        pack eq ({ ofs = ra.ofs ; len ; reg = ra.reg ; dim = Dim(s,ds) } :: ovl)
    | rg :: ovl ->
        rg :: pack eq ovl
    | [] -> []

  let flatten ovl = List.map Range.flatten ovl

  let once reg overlay =
    match List.filter (fun rg -> rg.reg == reg) overlay with
    | [] | [_] -> true | _ -> false

end

(* -------------------------------------------------------------------------- *)
(* --- Layout                                                             --- *)
(* -------------------------------------------------------------------------- *)

type 'a layout = {
  sizeof : int ;
  layout : 'a overlay ;
}

module Compound =
struct

  let garbled cache offset reg =
    let (ofs,len),sizeof = Offset.range cache offset in
    { sizeof ; layout = [ { ofs ; len ; reg ; dim = Raw len } ] }

  let field cache fd reg dim =
    let (ofs,len),sizeof = Offset.range_field cache fd in
    { sizeof ; layout = [ { ofs ; len ; reg ; dim } ] }

  let index te n reg dim =
    let len = Cil.bitsSizeOf te * n in
    { sizeof = len ; layout = [ { ofs = 0 ; len ; reg ; dim } ] }

  let reshape ~eq ~flat ~pack { sizeof ; layout } =
    let ovl = if flat then Overlay.flatten layout else layout in
    let ovl = if pack then Overlay.pack eq ovl else ovl in
    { sizeof ; layout = ovl }

end

(* -------------------------------------------------------------------------- *)
(* --- Clustering                                                         --- *)
(* -------------------------------------------------------------------------- *)

type 'a cluster =
  | Empty
  | Garbled
  | Chunk of 'a value
  | Layout of 'a layout

module Cluster =
struct

  let is_empty = function Empty -> true | _ -> false
  let is_garbled = function Garbled -> true | _ -> false

  let pretty pp fmt = function
    | Empty -> Format.pp_print_string fmt "empty"
    | Garbled -> Format.pp_print_string fmt "garbled"
    | Chunk v -> Value.pretty pp fmt v
    | Layout { sizeof ; layout } ->
        Overlay.pretty
          ~title:(fun fmt -> Format.fprintf fmt "sizeof:%d" sizeof)
          pp fmt layout

  let deref ~pointed (_,typ) =
    match Cil.unrollType typ with
    | TInt(ti,_) | TEnum({ ekind = ti },_) -> Chunk (Int (Ctypes.c_int ti))
    | TFloat(tf,_) -> Chunk (Float (Ctypes.c_float tf))
    | TPtr _ | TFun _ -> Chunk(Pointer(Lazy.force pointed))
    | TVoid _ | TNamed _ | TComp _ | TArray _ | TBuiltin_va_list _ -> Empty

  let rec get_dim s rds typ =
    if s = Cil.bitsSizeOf typ then Some (List.rev rds) else
      match Cil.unrollType typ with
      | TArray( te , Some e , _ , _ ) ->
          begin match Ctypes.get_int e with
            | None -> None
            | Some n -> get_dim s (if n = 1 then rds else n::rds) te
          end
      | _ -> None

  let shift_may cache pp offset reg ~inline cluster =
    match offset , cluster with
    | _ , Garbled -> None
    | _ , Empty ->
        let sizeof = Offset.container cache offset in
        Some { sizeof ; layout = [] }
    | Field fd , Chunk v ->
        begin
          let s = Value.sizeof v in
          match get_dim s [] fd.ftype with
          | None -> None
          | Some ds ->
              let dim = Dim(s,ds) in
              Some (Compound.field cache fd reg dim)
        end
    | Index(te,n) , Chunk v ->
        begin
          let s = Value.sizeof v in
          match get_dim s (Matrix.array [] n) te with
          | None -> None
          | Some ds ->
              let dim = Dim(s,ds) in
              Some (Compound.index te n reg dim)
        end
    | Field fd , Layout d ->
        let (ofs,len),sizeof = Offset.range_field cache fd in
        if d.sizeof = len then
          let layout =
            if inline then
              List.map (Range.shift ofs) d.layout
            else
              [ { ofs ; len ; reg ; dim=Dim(len,[]) } ]
          in Some { sizeof ; layout }
        else None
    | Index(te,n) , Layout {
        sizeof = s ;
        layout = [ { ofs=0 ; len ; reg ; dim = Dim(se,dse) } ]
      }
      when inline && s = len && Cil.bitsSizeOf te = len ->
        let dim = Dim(se,Matrix.array dse n) in
        Some (Compound.index te n reg dim)
    | Index(te,n) , Layout { sizeof } ->
        let size = Cil.bitsSizeOf te in
        if sizeof = size then
          let dim = Dim(size,Matrix.array [] n) in
          Some (Compound.index te n reg dim)
        else
          ( if Wp.has_dkey garbled_key then
              Wp.debug ~dkey:garbled_key
                "@[<hv 0>Garbled Offset:@ Index= {%a}[%d];@ Cluster= %a;@]"
                Cil_datatype.Typ.pretty te n (pretty pp) cluster ;
            None )

  let shift cache pp offset reg ~inline cluster =
    match shift_may cache pp offset reg ~inline cluster
    with Some ovl -> ovl
       | None -> Compound.garbled cache offset reg

  let do_merge pp (mu : 'a merger) (a : 'a cluster) (b : 'a cluster) =
    match a,b with
    | Empty , c | c , Empty -> c
    | Chunk va , Chunk vb ->
        begin match Value.merge (mu ~raw:false) va vb with
          | None -> Garbled
          | Some v -> Chunk v
        end
    | Layout { layout = [ { ofs=0 ; len=la ; reg=ra ; dim=Dim(s,da) } ] } ,
      Layout { layout = [ { ofs=0 ; len=lb ; reg=rb ; dim=Dim(s',db) } ] }
      when s = s' ->
        let reg = mu ~raw:false ra rb in
        let len = max la lb in
        let ds = Matrix.join s da db len in
        let layout = [ { ofs=0 ; len ; reg ; dim=Dim(s,ds) } ] in
        Layout { sizeof = len ; layout }
    | Layout { sizeof ; layout = la } ,
      Layout { sizeof = s ; layout = lb }
      when s = sizeof ->
        let layout = Overlay.merge pp mu la lb in
        Layout { sizeof ;  layout }
    | _ -> Garbled

  let merge pp mu a b =
    let result = do_merge pp mu a b in
    if result = Garbled && Wp.has_dkey garbled_key then
      Wp.debug ~dkey:garbled_key
        "@[<hv 0>Garbled Clusters:@ A=%a@ B=%a@]"
        (pretty pp) a (pretty pp) b ;
    result

  let reshape ~eq ~flat ~pack = function
    | Layout layout when flat || pack ->
        Layout (Compound.reshape ~eq ~flat ~pack layout)
    | cluster -> cluster

end

(* -------------------------------------------------------------------------- *)
(* --- Roots                                                              --- *)
(* -------------------------------------------------------------------------- *)

type 'a from =
  | Fvar of varinfo
  | Ffield of 'a * int
  | Findex of 'a
  | Fderef of 'a
  | Farray of 'a

type root =
  | Rnone
  | Rfield of varinfo * int (* static offset *)
  | Rindex of varinfo (* any offset rooted at var *)
  | Rtop

module Root =
struct

  let pretty fmt = function
    | Rtop -> Format.pp_print_string fmt "*"
    | Rnone -> Format.pp_print_string fmt "-"
    | Rfield(x,0) -> Format.fprintf fmt "&%a" Varinfo.pretty x
    | Rfield(x,ofs) -> Format.fprintf fmt "&%a+%d" Varinfo.pretty x ofs
    | Rindex(x) -> Format.fprintf fmt "&%a+(..)" Varinfo.pretty x

  let field ofs = function
    | Rfield(x,p) -> Rfield(x,p+ofs)
    | (Rindex _ | Rnone | Rtop) as r -> r

  let index = function
    | Rfield(x,_) -> Rindex x
    | (Rindex _ | Rnone | Rtop) as r -> r

  let from ~root = function
    | Fvar x -> Rfield(x,0)
    | Ffield(r,ofs) -> field ofs (root r)
    | Findex r -> index (root r)
    | Fderef r -> root r
    | Farray _ -> Rtop

  let merge_var a b = match a,b with
    | (Rfield(x,_) | Rindex x) ,
      (Rfield(y,_) | Rindex y)
      when Varinfo.equal x y -> Some x
    | _ -> None

  let merge_field x a b = match a,b with
    | Rfield(_,p) , Rfield(_,q) when p = q -> a
    | _ -> Rindex x

  let merge a b =
    if a == b then a else
      match a,b with
      | Rnone,s | s,Rnone -> s
      | Rtop,_ | _,Rtop -> Rtop
      | _ ->
          match merge_var a b with
          | Some x -> merge_field x a b
          | None -> Rtop

  let indexed = function
    | Rnone | Rfield _ -> false
    | Rindex _ | Rtop -> true

  let framed = function
    | Rfield(x,_) | Rindex x -> not x.vglob && not x.vaddrof (* Cf. MemVar *)
    | Rnone -> true
    | Rtop -> false

end

(* -------------------------------------------------------------------------- *)
(* --- Chunks                                                             --- *)
(* -------------------------------------------------------------------------- *)

module R = Qed.Intset

type chunks = R.t

type 'a chunk =
  | Mref of 'a
  | Mmem of root * 'a value
  | Mraw of root * 'a option
  | Mcomp of chunks * 'a overlay

module Chunk =
struct
  let mem = R.mem
  let empty = R.empty
  let singleton = R.singleton
  let union = R.union
  let union_map f es = List.fold_left (fun w e -> R.union w @@ f e) R.empty es
  let disjoint a b = not (R.intersect a b)
  let pretty pp fmt es =
    begin
      Format.fprintf fmt "@[<hov 2>{" ;
      R.iter (fun e -> Format.fprintf fmt "@ %a" pp e) es ;
      Format.fprintf fmt " }@]" ;
    end
end

(* -------------------------------------------------------------------------- *)
