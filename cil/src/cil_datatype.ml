(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

open Cil_types
let (=?=) = Extlib.compare_basic
let compare_list = Extlib.list_compare
let hash_list f = List.fold_left (fun acc d -> 65537 * acc + f d) 1

(* Functions that will clear internal, non-project compliant, caches *)
let clear_caches = ref []

(**************************************************************************)
(** {3 Generic builders for Cil datatypes} *)
(**************************************************************************)

module Make
  (X: sig
    type t
    val name: string
    val reprs: t list
    val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
    val pretty: Format.formatter -> t -> unit
    val varname: t -> string
  end) =
  Datatype.Make
    (struct
      include Datatype.Undefined
      include X
      let name = "Cil_datatype." ^ name
      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
     end)

module Make_with_collections
  (X: sig
    type t
    val name: string
    val reprs: t list
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val internal_pretty_code: Type.precedence -> Format.formatter -> t -> unit
    val pretty: Format.formatter -> t -> unit
    val varname: t -> string
    val hash: t -> int
    val copy: t -> t
  end) =
  Datatype.Make_with_collections
    (struct
      include X
      let name = "Cil_datatype." ^ name
      let structural_descr = Structural_descr.t_abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
     end)

let compare_chain cmp x1 x2 next arg1 arg2 =
  let res = cmp x1 x2 in if res = 0 then next arg1 arg2 else res

let rank_term = function
  | TConst _ -> 0
  | TLval _ -> 1
  | TSizeOf _ -> 2
  | TSizeOfE _ -> 3
  | TSizeOfStr _ -> 4
  | TAlignOf _ -> 5
  | TAlignOfE _ -> 6
  | TUnOp _ -> 7
  | TBinOp _ -> 8
  | TCastE _ -> 9
  | TAddrOf _ -> 10
  | TStartOf _ -> 11
  | Tapp _ -> 12
  | Tlambda _ -> 13
  | TDataCons _ -> 14
  | Tif _ -> 15
  | Tat _ -> 16
  | Tbase_addr _ -> 17
  | Tblock_length _ -> 18
  | Tnull -> 19
  | TCoerce _ -> 20
  | TCoerceE _ -> 21
  | TUpdate _ -> 22
  | Ttypeof _ -> 23
  | Ttype _ -> 24
  | Tempty_set -> 25
  | Tunion _ -> 26
  | Tinter _ -> 27
  | Trange _ -> 28
  | Tlet _ -> 29
  | Tcomprehension _ -> 30
  | Toffset _ -> 31
  | TLogic_coerce _ -> 32


(**************************************************************************)
(** {3 Cabs types} *)
(**************************************************************************)

module Cabs_file =
  Make
    (struct
      type t = Cabs.file
      let name = "Cabs_file"
      let reprs = [ "", []; "", [ true, Cabs.GLOBANNOT [] ] ]
      let varname (s, _) = "cabs_" ^ s
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
     end)

(**************************************************************************)
(** {3 C types} *)
(**************************************************************************)

module Position =
  Make_with_collections
    (struct
      type t = Lexing.position
      let name = "Position"
      let reprs = [ Lexing.dummy_pos ]
      let compare: t -> t -> int = (=?=)
      let hash = Hashtbl.hash
      let copy = Datatype.identity
      let equal: t -> t -> bool = ( = )
      let internal_pretty_code = Datatype.undefined
      let pretty fmt pos =
        Format.fprintf fmt "%s:%d char %d"
          pos.Lexing.pos_fname pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      let varname _ = "pos"
     end)

module Location = struct
  let unknown = Lexing.dummy_pos, Lexing.dummy_pos
  include Make_with_collections
    (struct
      type t = location
      let name = "Location"
      let reprs = [ unknown ]
      let compare: location -> location -> int = (=?=)
      let hash (b, _e) = Hashtbl.hash (b.Lexing.pos_fname, b.Lexing.pos_lnum)
      let copy = Datatype.identity (* immutable strings *)
      let equal : t -> t -> bool = ( = )
      let internal_pretty_code = Datatype.undefined
      let pretty fmt loc = 
	let loc = (fst loc) in
	Format.fprintf fmt "%s:%d" 
          (Filepath.pretty loc.Lexing.pos_fname)
          loc.Lexing.pos_lnum
      let varname _ = "loc"
     end)

  let pretty_long fmt loc =
    let file = Filepath.pretty (fst loc).Lexing.pos_fname in
    let line = (fst loc).Lexing.pos_lnum in
    if file <> "." && file <> "" && line > 0 then
      Format.fprintf fmt "file %s, line %d" file line
    else
      Format.fprintf fmt "generated"

  let pretty_line fmt loc =
    let line = (fst loc).Lexing.pos_lnum in
    if line > 0 then
      Format.fprintf fmt "line %d" line
    else
      Format.fprintf fmt "generated"

end

module Instr = struct

  let pretty_ref = ref (fun _ _ -> assert false)
  include Make
    (struct
      type t = instr
      let name = "Instr"
      let reprs = List.map (fun l -> Skip l) Location.reprs
      let internal_pretty_code = Datatype.undefined
      let pretty fmt x = !pretty_ref fmt x
      let varname = Datatype.undefined
     end)

  let loc = function
    | Skip l
    | Set (_,_,l)
    | Call (_,_,_,l)
    | Asm (_,_,_,_,_,_,l)
    | Code_annot (_,l) -> l

end

module File =
  Make
    (struct
      type t = file
      let name = "File"
      let reprs =
	[ { fileName = "";
	    globals = [];
	    globinit = None;
	    globinitcalled = false } ]
      include Datatype.Undefined
      let varname _ = "ast"
     end)

module Stmt_Id = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = stmt
      let name = "Stmt"
      let reprs =
	[ { labels = [];
	    skind = UnspecifiedSequence [];
	    sid = -1;
	    succs = [];
	    preds = [];
	    ghost  = false } ]
      let compare t1 t2 = Datatype.Int.compare t1.sid t2.sid
      let hash t1 = t1.sid
      let equal t1 t2 = t1.sid = t2.sid
      let copy = Datatype.undefined
      let internal_pretty_code p_caller fmt s =
	let pp fmt =
	  Format.fprintf fmt
	    "@[<hv 2>fst@;@[<hv 2>(Kernel_function.find_from_sid@;%d)@]@]"
	    s.sid
	in
	Type.par p_caller Type.Call fmt pp
      let pretty fmt s = !pretty_ref fmt s
      let varname _ = "stmt"
     end)
  let id stmt = stmt.sid
end
module Stmt = struct
  include Stmt_Id

  let pretty_sid fmt s = Format.pp_print_int fmt s.sid

  module Hptset = struct
    include Hptset.Make
      (Stmt_Id)
      (struct let v = [ [ ] ] end)
      (struct let l = [ ] (* This should be [Ast.self], but cannot be done
                             here *) end)
  end
  let () = clear_caches := Hptset.clear_caches :: !clear_caches

  let rec loc_skind = function
    | Return(_, l) | Goto(_, l) | Break(l) | Continue l | If(_, _, _, l)
    | Switch (_, _, _, l) | Loop (_, _, l, _, _)
    | TryFinally (_, _, l) | TryExcept (_, _, _, l) -> l
    | Instr hd -> Instr.loc hd
    | Block b -> (match b.bstmts with [] -> Location.unknown | s :: _ -> loc s)
    | UnspecifiedSequence ((s,_,_,_,_) :: _) -> loc s
    | UnspecifiedSequence [] -> Location.unknown

  and loc s = loc_skind s.skind

end

module Kinstr = struct

  include Make_with_collections
    (struct
      type t = kinstr
      let name = "Kinstr"
      let reprs = Kglobal :: List.map (fun s -> Kstmt s) Stmt.reprs
      let compare i1 i2 = match i1, i2 with
	| Kglobal, Kglobal -> 0
	| Kglobal, _ -> 1
	| _, Kglobal -> -1
	| Kstmt s1, Kstmt s2 -> Stmt.compare s1 s2
      let equal t1 t2 = compare t1 t2 = 0
      let hash = function
	| Kglobal -> 1 lsl 29
	| Kstmt s -> s.sid
      let copy = Datatype.undefined
      let internal_pretty_code p fmt = function
	| Kglobal ->
	  Format.fprintf fmt "Kglobal"
	| Kstmt s ->
	  let pp fmt =
	    Format.fprintf fmt "@[<hv 2>Kstmt@;%a@]"
	      (Stmt.internal_pretty_code Type.Call) s
	  in
	  Type.par p Type.Call fmt pp
      let pretty = Datatype.from_pretty_code
      let varname _ = "ki"
     end)

  let loc = function
    | Kstmt st -> Stmt.loc st
    | Kglobal -> assert false

  let kinstr_of_opt_stmt = function
    | None -> Kglobal
    | Some s -> Kstmt s

end

let index_attrparam = function
  | AInt _ -> 0
  | AStr _ -> 1
  | ACons _ -> 2
  | ASizeOf _ -> 3
  | ASizeOfE _ -> 4
  | AAlignOf _ -> 6
  | AAlignOfE _ -> 7
  | AUnOp _ -> 9
  | ABinOp _ -> 10
  | ADot _ -> 11
  | AStar _ -> 12
  | AAddrOf _ -> 13
  | AIndex _ -> 14
  | AQuestion _ -> 15

let index_typ = function
  | TVoid _ -> 0
  | TInt _ -> 1
  | TFloat _ -> 2
  | TPtr _ -> 3
  | TArray _ -> 4
  | TFun _ -> 5
  | TNamed _ -> 6
  | TComp _ -> 7
  | TEnum _ -> 8
  | TBuiltin_va_list _ -> 9

let pbitsSizeOf = ref (fun _ -> failwith "pbitsSizeOf not yet defined")
let punrollType =
  ref (fun _ -> failwith "punrollType not yet defined")

let drop_non_logic_attributes = ref (fun a -> a)

type type_compare_config =
    { by_name : bool;
      logic_type: bool;
      unroll: bool }

let rec compare_attribute config a1 a2 = match a1, a2 with
    | Attr (s1, l1), Attr (s2, l2) ->
	compare_chain (=?=) s1 s2 (compare_attrparam_list config) l1 l2
    | AttrAnnot s1, AttrAnnot s2 -> s1 =?= s2
    | Attr _, AttrAnnot _ -> -1
    | AttrAnnot _, Attr _ -> 1
and compare_attributes config  l1 l2 = 
  let l1, l2 = if config.logic_type
    then !drop_non_logic_attributes l1, !drop_non_logic_attributes l2
    else l1,l2
  in compare_list (compare_attribute config) l1 l2
and compare_attrparam_list config l1 l2 =
  compare_list (compare_attrparam config) l1 l2
and compare_attrparam config a1 a2 = match a1, a2 with
  | AInt i1, AInt i2 -> Integer.compare i1 i2
  | AStr s1, AStr s2 -> s1 =?= s2
  | ACons ((s1: string), l1), ACons (s2, l2) ->
      let r1 = (=?=) s1 s2 in
      if r1 <> 0 then r1
      else
        compare_attrparam_list config l1 l2
  | ASizeOf t1, ASizeOf t2 -> compare_type config t1 t2
  | ASizeOfE p1, ASizeOfE p2 -> compare_attrparam config p1 p2
  | AAlignOf t1, AAlignOf t2 -> compare_type config t1 t2
  | AAlignOfE p1, AAlignOfE p2 -> compare_attrparam config p1 p2
  | AUnOp (op1, a1), AUnOp (op2, a2) ->
     compare_chain (=?=) op1 op2 (compare_attrparam config) a1 a2
  | ABinOp (op1, a1, a1'), ABinOp (op2, a2, a2') ->
     compare_chain (=?=) op1 op2
       (compare_chain
          (compare_attrparam config) a1 a2 (compare_attrparam config))
       a1' a2'
  | ADot (a1, s1), ADot (a2, s2) ->
      compare_chain (=?=) s1 s2 (compare_attrparam config) a1 a2
  | AStar a1, AStar a2
  | AAddrOf a1, AAddrOf a2 -> compare_attrparam config a1 a2
  | AIndex (a1, a1'), AIndex (a2, a2') ->
      compare_chain
        (compare_attrparam config) a1 a2
        (compare_attrparam config) a1' a2'
  | AQuestion (a1, a1', a1''), AQuestion (a2, a2', a2'') ->
      compare_chain
        (compare_attrparam config) a1 a2
        (compare_chain (compare_attrparam config) a1' a2'
           (compare_attrparam  config))
        a1'' a2''
  | (AInt _ | AStr _ | ACons _ | ASizeOf _ | ASizeOfE _ | 
        AAlignOf _ | AAlignOfE _ | AUnOp _ | ABinOp _ | ADot _ |
        AStar _ | AAddrOf _ | AIndex _ | AQuestion _ as a1), a2 ->
      index_attrparam a1 - index_attrparam a2
and compare_type config t1 t2 =
  if t1 == t2 then 0
  else
    let typs =
      if config.unroll then !punrollType t1, !punrollType t2
      else t1,t2
    in
    match typs with
      | TVoid l1, TVoid l2 -> compare_attributes config l1 l2
      | TInt (i1, l1), TInt (i2, l2) ->
          compare_chain (=?=) i1 i2 (compare_attributes config) l1 l2
      | TFloat (f1, l1), TFloat (f2, l2) ->
          compare_chain (=?=) f1 f2 (compare_attributes config) l1 l2
      | TPtr (t1, l1), TPtr (t2, l2) ->
          compare_chain
            (compare_type config) t1 t2
            (compare_attributes config) l1 l2
      | TArray (t1', _, _, l1), TArray (t2', _, _, l2) ->
          (* bitsSizeOf is here to compare the size of the arrays *)
          compare_chain (=?=)
            (!pbitsSizeOf t1) (!pbitsSizeOf t2)
          (compare_chain
            (compare_type config) t1' t2'
            (compare_attributes config)) l1 l2
      | TFun (r1, a1, v1, l1), TFun (r2, a2, v2, l2) ->
          compare_chain (compare_type config) r1 r2
            (compare_chain (=?=) v1 v2
               (compare_chain (compare_arg_list config) a1 a2
                  (compare_attributes config))) l1 l2
      | TNamed (t1,a1), TNamed (t2,a2) ->
          assert (not config.unroll);
          compare_chain (=?=) t1.tname t2.tname
            (compare_attributes config) a1 a2
      | TComp (c1, _, l1), TComp (c2, _, l2) ->
          let res =
            if config.by_name
            then (=?=) c1.cname c2.cname
            else (=?=) c1.ckey c2.ckey
          in
          if res <> 0 then res
          else compare_attributes config l1 l2
      | TEnum (e1, l1), TEnum (e2, l2) ->
          compare_chain
            (=?=) e1.ename e2.ename
            (compare_attributes config) l1 l2
      | TBuiltin_va_list l1, TBuiltin_va_list l2 ->
          compare_attributes config l1 l2
      | (TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _ | TNamed _ |
             TComp _ | TEnum _ | TBuiltin_va_list _ as a1), a2 ->
          index_typ a1 - index_typ a2

and compare_arg_list  config l1 l2 =
  Extlib.opt_compare
    (compare_list
       (fun (_n1, t1, l1) (_n2, t2, l2) ->
           (compare_chain (compare_type config) t1 t2
              (compare_attributes config)) l1 l2
       )) l1 l2

let hash_attribute _config = function
  | AttrAnnot s -> Hashtbl.hash s
  | Attr (s, _) -> (* We do not hash attrparams. There is a recursivity problem
       with typ, and the equal function will be complicated enough in itself *)
      3 * Hashtbl.hash s + 117
let hash_attributes config l =
  let attrs = if config.logic_type then !drop_non_logic_attributes l else l in
  hash_list (hash_attribute config) attrs

let rec hash_type config t =
  let t = if config.unroll then !punrollType t else t in
  match t with
    | TVoid l -> Hashtbl.hash (hash_attributes config l, 1)
    | TInt (i, l) -> Hashtbl.hash (i, 2, hash_attributes config l)
    | TFloat (f, l) -> Hashtbl.hash (f, 3, hash_attributes config l)
    | TPtr (t, l) ->
        Hashtbl.hash (hash_type config t, 4, hash_attributes config l)
    | TArray (t, _, _, l) ->
        Hashtbl.hash (hash_type config t, 5, hash_attributes config l)
  | TFun (r, a, v, l) ->
      Hashtbl.hash
        (hash_type config r, 6, hash_args config a, v, hash_attributes config l)
  | TNamed (ti, l) ->
      Hashtbl.hash (ti.tname, 7, hash_attributes config l)
  | TComp (c, _, l) -> 
      Hashtbl.hash 
        ((if config.by_name then Hashtbl.hash c.cname else c.ckey), 8, 
         hash_attributes config l)
  | TEnum (e, l) ->
      Hashtbl.hash (e.ename, 9, hash_attributes config l)
  | TBuiltin_va_list l -> Hashtbl.hash (hash_attributes config l, 10)
and hash_args config = function
  | None -> 11713
  | Some l ->
      hash_list
        (fun (_, t, l) ->
          Hashtbl.hash (17, hash_type config t, hash_attributes config l)) l

module Attribute=struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = attribute
      let config = { by_name = false; logic_type = false; unroll = true }
      let name = "Attribute"
      let reprs = [ AttrAnnot "" ]
      let compare = compare_attribute config
      let hash = hash_attribute config
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t = !pretty_ref fmt t
      let varname = Datatype.undefined
     end)
end

let pretty_typ_ref = ref (fun _ _ -> assert false)

module Attributes=
  Datatype.List_with_collections(Attribute) 
    (struct let module_name = "Attributes" end)

module MakeTyp(M:sig val config: type_compare_config val name: string end) =
struct
  include Make_with_collections
    (struct
      type t = typ
      let name = M.name
      let reprs = [ TVoid [] ]
      let compare = compare_type M.config
      let hash = hash_type M.config
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t = !pretty_typ_ref fmt t
      let varname = Datatype.undefined
     end)
end

module Typ=
  MakeTyp
    (struct
      let config = { by_name = false; logic_type = false; unroll = true; }
      let name = "Typ"
     end)

module TypByName =
  MakeTyp
    (struct
      let config = { by_name = true; logic_type = false; unroll = false; }
      let name = "TypByName"
     end)

module TypNoUnroll =
  MakeTyp
    (struct
      let config = { by_name = false; logic_type = false; unroll = false; }
      let name = "TypNoUnroll"
     end)

module Typeinfo =
  Make_with_collections
    (struct
      include Datatype.Undefined
      type t = typeinfo
      let name = "Type_info"
      let reprs =
	[ { torig_name = "";
	    tname = "";
	    ttype = TVoid [];
	    treferenced = false } ]
      let compare v1 v2 = String.compare v1.tname v2.tname
      let hash v = Hashtbl.hash v.tname
      let equal v1 v2 = v1.tname = v2.tname
     end)

module Exp = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let dummy = { eid = -1; enode = Const (CStr ""); eloc = Location.unknown }
  include Make_with_collections
    (struct
      include Datatype.Undefined
      type t = exp
      let name = "Exp"
      let reprs =
	[ dummy ]
      let compare e1 e2 = Datatype.Int.compare e1.eid e2.eid
      let hash e = Hashtbl.hash e.eid
      let equal e1 e2 = e1.eid = e2.eid
      let pretty fmt t = !pretty_ref fmt t
     end)
end

module Label =
  Make_with_collections
    (struct
      type t = label
      let name = "Label"
      let reprs =
	[ Label("", Location.unknown, false); Default Location.unknown ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
      let hash = function
        | Default _ -> 7
        | Case (e, _) -> Exp.hash e
        | Label (s, _, b) -> Hashtbl.hash s + (if b then 13 else 59)
      let compare l1 l2 = match l1, l2 with
        | Default loc1, Default loc2 -> Location.compare loc1 loc2
        | Case (e1, loc1), Case (e2, loc2) ->
            let c = Exp.compare e1 e2 in
            if c = 0 then Location.compare loc1 loc2
            else c
        | Label (s1, loc1, b1), Label (s2, loc2, b2) ->
            let c = s1 =?= s2 in
            if c = 0 then
              let c = b1 =?= b2 in
              if c = 0 then Location.compare loc1 loc2
              else c
            else c
        | Label _, (Case _ | Default _)
        | Case _, Default _ -> -1
        | Case _, Label _
        | Default _, (Label _ | Case _) -> 1
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
     end)

module Varinfo_Id = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let internal_pretty_code_ref = ref (fun _ _ _ -> assert false)
  let dummy =
    { vname = "";
      vorig_name = "";
      vtype = TVoid [];
      vattr = [];
      vstorage = NoStorage;
      vglob = false;
      vdefined = false;
      vformal = false;
      vinline = false;
      vdecl = Location.unknown;
      vid = -1;
      vaddrof = false;
      vreferenced = false;
      vgenerated = false;
      vdescr = None;
      vdescrpure = false;
      vghost = false;
      vlogic = false;
      vlogic_var_assoc = None }

  include Make_with_collections
      (struct
      type t = varinfo
      let name = "Varinfo"
      let reprs = [ dummy ]
      let compare v1 v2 = Datatype.Int.compare v1.vid v2.vid
      let hash v = v.vid
      let equal v1 v2 = v1.vid = v2.vid
      let copy = Datatype.undefined
      let internal_pretty_code p fmt v = !internal_pretty_code_ref p fmt v
      let pretty fmt v = !pretty_ref fmt v
      let varname v = "vi_" ^ v.vorig_name
     end)
  let id v = v.vid
end

module Varinfo = struct
  include Varinfo_Id

  let pretty_vname fmt v = Format.pp_print_string fmt v.vname

  module Hptset = struct
    include Hptset.Make
      (Varinfo_Id)
      (struct let v = [ [ ] ] end)
      (struct let l = [ ] (* Should morally be [Ast.self] *) end)
  end
  let () = clear_caches := Hptset.clear_caches :: !clear_caches
end

module Compinfo =
  Make_with_collections
    (struct
      type t = compinfo
      let name = "compinfo"
      let reprs =
	[ { cstruct = false;
	    corig_name = "";
	    cname = "";
	    ckey = -1;
	    cfields = [];
	    cattr = [];
	    cdefined = false;
	    creferenced = false } ]
      let compare v1 v2 = Datatype.Int.compare v1.ckey v2.ckey
      let hash v = Hashtbl.hash v.ckey
      let equal v1 v2 = v1.ckey = v2.ckey
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Fieldinfo =
  Make_with_collections
    (struct
      type t = fieldinfo
      let name = "fieldinfo"
      let reprs =
	List.fold_left
	  (fun acc ci ->
	    List.fold_left
	      (fun acc typ ->
		List.fold_left
		  (fun acc loc ->
		    { fcomp = ci;
		      forig_name = "";
		      fname = "";
		      ftype = typ;
		      fbitfield = None;
		      fattr = [];
		      floc = loc;
		      faddrof = false;
		      fsize_in_bits = None;
		      foffset_in_bits = None;
		      fpadding_in_bits = None }
		    :: acc)
		  acc
		  Location.reprs)
	      acc
	      Typ.reprs)
	  []
	  Compinfo.reprs
      let fid fi = fi.fcomp.ckey, fi.fname
      let compare f1 f2 = Extlib.compare_basic (fid f1) (fid f2)
      let hash f1 = Hashtbl.hash (fid f1)
      let equal f1 f2 = (fid f1) = (fid f2)
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Enuminfo =
  Make_with_collections
    (struct
       include Datatype.Undefined
       type t = enuminfo
       let name = "Enuminfo"
       let reprs =
	 [ { eorig_name = "";
	     ename = "";
	     eitems = [];
	     eattr = [];
	     ereferenced = false;
             ekind = IInt; } ]
       let compare v1 v2 = String.compare v1.ename v2.ename
       let hash v = Hashtbl.hash v.ename
       let equal v1 v2 = v1.ename = v2.ename
     end)

module Enumitem =
  Make_with_collections
    (struct
      include Datatype.Undefined
      type t = enumitem
      let name = "Enumitem"
      let reprs =
	List.map
	  (fun i ->
	    { eiorig_name = "";
	      einame = "";
	      eival =
		{ eloc = Location.unknown; eid = -1; enode = Const (CStr "") };
	      eihost = i;
	      eiloc = Location.unknown })
	  Enuminfo.reprs
      let compare v1 v2 = String.compare v1.einame v2.einame
      let hash v = Hashtbl.hash v.einame
      let equal v1 v2 = v1.einame = v2.einame
     end)

let compare_constant c1 c2 = match c1, c2 with
  | CInt64(v1,k1,_), CInt64(v2,k2,_) ->
    compare_chain Integer.compare v1 v2 Extlib.compare_basic k1 k2
  | CStr s1, CStr s2 -> Datatype.String.compare s1 s2
  | CWStr s1, CWStr s2 -> compare_list Datatype.Int64.compare s1 s2
  | CChr c1, CChr c2 -> Datatype.Char.compare c1 c2
  | CReal (f1,k1,_), CReal(f2,k2,_) ->
    compare_chain Datatype.Float.compare f1 f2
      Extlib.compare_basic k1 k2
  | CEnum e1, CEnum e2 -> Enumitem.compare e1 e2
  | (CInt64 _, (CStr _ | CWStr _ | CChr _ | CReal _ | CEnum _)) -> 1
  | (CStr _, (CWStr _ | CChr _ | CReal _ | CEnum _)) -> 1
  | (CWStr _, (CChr _ | CReal _ | CEnum _)) -> 1
  | (CChr _, (CReal _ | CEnum _)) -> 1
  | (CReal _, CEnum _) -> 1
  | (CStr _ | CWStr _ | CChr _ | CReal _ | CEnum _),
    (CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _) -> -1
 
let hash_const c =
  match c with
      CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _  -> Hashtbl.hash c
    | CEnum ei -> 95 + Enumitem.hash ei

module StructEq =
  struct
    let rec compare_exp e1 e2 =
      match e1.enode, e2.enode with
        | Const c1, Const c2 -> compare_constant c1 c2
        | Const _, _ -> 1
        | _, Const _ -> -1
        | Lval lv1, Lval lv2 -> compare_lval lv1 lv2
        | Lval _, _ -> 1
        | _, Lval _ -> -1
        | SizeOf t1, SizeOf t2 -> Typ.compare t1 t2
        | SizeOf _, _  -> 1
        | _, SizeOf _ -> -1
        | SizeOfE e1, SizeOfE e2 -> compare_exp e1 e2
        | SizeOfE _, _ -> 1
        | _, SizeOfE _ -> -1
        | SizeOfStr s1, SizeOfStr s2 -> String.compare s1 s2
        | SizeOfStr _, _ -> 1
        | _, SizeOfStr _ -> -1
        | AlignOf ty1, AlignOf ty2 -> Typ.compare ty1 ty2
        | AlignOf _, _ -> 1
        | _, AlignOf _ -> -1
        | AlignOfE e1, AlignOfE e2 -> compare_exp e1 e2
        | AlignOfE _, _ -> 1
        | _, AlignOfE _ -> -1
        | UnOp(op1,e1,ty1), UnOp(op2,e2,ty2) ->
            let res = Extlib.compare_basic op1 op2 in
            if res = 0 then 
              let res = compare_exp e1 e2 in
              if res = 0 then Typ.compare ty1 ty2 else res
            else res
        | UnOp _, _ -> 1
        | _, UnOp _ -> -1
        | BinOp(op1,e11,e21, ty1), BinOp(op2,e12,e22, ty2) ->
            let res = Extlib.compare_basic op1 op2 in
            if res = 0 then
              let res = compare_exp e11 e12 in
              if res = 0 then
                let res = compare_exp e21 e22 in
                if res = 0 then Typ.compare ty1 ty2 else res
              else res
            else res
        | BinOp _, _ -> 1
        | _, BinOp _ -> -1
        | CastE(t1,e1), CastE(t2, e2) ->
            let res = Typ.compare t1 t2 in
            if res = 0 then compare_exp e1 e2 else res
        | CastE _, _ -> 1
        | _, CastE _ -> -1
        | AddrOf lv1, AddrOf lv2 -> compare_lval lv1 lv2
        | AddrOf _, _ -> 1
        | _, AddrOf _ -> -1
        | StartOf lv1, StartOf lv2 -> compare_lval lv1 lv2
        | StartOf _, _ -> 1
        | _, StartOf _ -> -1
        | Info _, Info _ ->
            Kernel.fatal "[exp_compare] Info node is obsolete. Do not use it"

    and compare_lval (h1,o1) (h2,o2) =
      let res = compare_lhost h1 h2 in
      if res = 0 then compare_offset o1 o2 else res
        
    and compare_lhost h1 h2 =
      match h1, h2 with
        | Var v1, Var v2 -> Varinfo.compare v1 v2
        | Var _, Mem _ -> 1
        | Mem e1, Mem e2 -> compare_exp e1 e2
        | Mem _, Var _ -> -1
            
    and compare_offset o1 o2 =
      match o1, o2 with
        | NoOffset, NoOffset -> 0
        | NoOffset, _ -> 1
        | _, NoOffset -> -1
        | Field(f1,o1), Field(f2, o2) ->
            let res = Fieldinfo.compare f1 f2 in
            if res = 0 then compare_offset o1 o2 else res
        | Field _, _ -> 1
        | _, Field _ -> -1
        | Index(e1, o1), Index(e2, o2) ->
            let res = compare_exp e1 e2 in
            if res = 0 then compare_offset o1 o2 else res
              
    let prime = 83047
    let rec hash_exp acc e =
      match e.enode with
        | Const c -> prime * acc lxor hash_const c
        | Lval lv -> hash_lval ((prime*acc) lxor 42) lv
        | SizeOf t -> (prime*acc) lxor Typ.hash t
        | SizeOfE e -> hash_exp ((prime*acc) lxor 75) e
        | SizeOfStr s -> (prime*acc) lxor Hashtbl.hash s
        | AlignOf t -> (prime*acc) lxor Typ.hash t
        | AlignOfE e -> hash_exp ((prime*acc) lxor 153) e
        | UnOp(op,e,ty) ->
            let res = hash_exp ((prime*acc) lxor Hashtbl.hash op) e in
            (prime*res) lxor Typ.hash ty
        | BinOp(op,e1,e2,ty) ->
            let res = hash_exp ((prime*acc) lxor Hashtbl.hash op) e1 in
            let res = hash_exp ((prime*res) lxor 257) e2 in
            (prime * res) lxor Typ.hash ty
        | CastE(ty,e) -> hash_exp ((prime*acc) lxor Typ.hash ty) e
        | AddrOf lv -> hash_lval (prime*acc lxor 329) lv
        | StartOf lv -> hash_lval (prime*acc lxor 431) lv
        | Info _ ->
            Kernel.fatal "Info node is deprecated and should not be used"
    and hash_lval acc (h,o) =
      hash_offset ((prime * acc) lxor hash_lhost 856 h) o
    and hash_lhost acc = function
      | Var v -> (prime * acc) lxor (Varinfo.hash v)
      | Mem e -> hash_exp ((prime * acc) lxor 967) e
    and hash_offset acc = function
      | NoOffset -> (prime * acc) lxor 1583
      | Index(e,o) ->
          let res = hash_exp 1790 e in
          hash_offset ((prime * acc) lxor res) o
      | Field(f,o) -> hash_offset ((prime * acc) lxor Hashtbl.hash f.fname) o
  end

module Wide_string =
  Datatype.List_with_collections(Datatype.Int64)
    (struct let module_name = "Cil_datatype.Wide_string" end)

module Constant =
  struct
    let pretty_ref = Extlib.mk_fun "Cil_datatype.Constant.pretty_ref"
    include Make_with_collections
    (struct
      include Datatype.Undefined
      type t = constant
      let name = "Constant"
      let reprs = [ CInt64(Integer.zero, IInt, Some "0") ]
      let compare = compare_constant
      let hash = hash_const
      let equal = Datatype.from_compare
      let pretty fmt t = !pretty_ref fmt t
     end)
  end

module ExpStructEq =
  Make_with_collections
    (struct
      include Datatype.Undefined
      type t = exp
      let name = "ExpStructEq"
      let reprs = [ Exp.dummy ]
      let compare = StructEq.compare_exp
      let hash = StructEq.hash_exp 7863
      let equal = Datatype.from_compare
      let pretty fmt t = !Exp.pretty_ref fmt t
     end)

module Block = struct
  let pretty_ref = Extlib.mk_fun "Cil_datatype.Block.pretty_ref"
  include Make
    (struct
      type t = block
      let name = "Block"
      let reprs =
	[ { battrs = []; blocals = Varinfo.reprs; bstmts = Stmt.reprs } ]
      let internal_pretty_code = Datatype.undefined
      let pretty fmt b = !pretty_ref fmt b
      let varname = Datatype.undefined
     end)
  let equal b1 b2 = (b1 == b2)
end

let rec equal_lval (h1, o1) (h2, o2) =
  equal_lhost h1 h2 && equal_offset o1 o2

and equal_lhost h1 h2 =
  match h1,h2 with
  | Var v1, Var v2 -> Datatype.Int.equal v1.vid v2.vid
  | Mem e1, Mem e2 -> Exp.equal e1 e2
  | (Var _ | Mem _), _-> false

and equal_offset o1 o2 =
  match o1,o2 with
  | NoOffset, NoOffset -> true
  | Field(f1,o1), Field(f2,o2) ->
      Fieldinfo.equal f1 f2 && equal_offset o1 o2
  | Index(e1,o1), Index(e2,o2) ->
      Exp.equal e1 e2 && equal_offset o1 o2
  | (NoOffset | Field _ | Index _), _ -> false

let rec compare_lval (h1,o1) (h2,o2) =
  compare_chain compare_lhost h1 h2 compare_offset o1 o2

and compare_lhost h1 h2 =
  match h1,h2 with
    Var v1, Var v2 -> Datatype.Int.compare v1.vid v2.vid
  | Mem e1, Mem e2 -> Exp.compare e1 e2
  | Var _, Mem _ -> 1
  | Mem _, Var _ -> -1

and compare_offset o1 o2 =
  match o1,o2 with
    NoOffset, NoOffset -> 0
  | Field(f1,o1), Field(f2,o2) ->
    compare_chain Fieldinfo.compare f1 f2 compare_offset o1 o2
  | Index(e1,o1), Index(e2,o2) ->
    compare_chain Exp.compare e1 e2 compare_offset o1 o2
  | (NoOffset, (Field _ | Index _)) -> 1
  | (Field _, Index _) -> 1
  | ((Field _ | Index _), (NoOffset | Field _ )) -> -1

let rec hash_lval (h,o) =
  Hashtbl.hash (hash_lhost h, hash_offset o)

and hash_lhost = function
  | Var v -> 17 + v.vid
  | Mem e -> 13 + 5 * e.eid

and hash_offset = function
  | NoOffset -> 19
  | Field(f,o) -> Hashtbl.hash (Fieldinfo.hash f, hash_offset o)
  | Index (e, o) -> Hashtbl.hash (e.eid, hash_offset o)

module Lval = struct
  let pretty_ref = ref (fun _ -> assert false)
  include Make_with_collections
    (struct
      type t = lval
       let name = "Lval"
       let reprs = List.map (fun v -> Var v, NoOffset) Varinfo.reprs
       let compare = compare_lval
       let equal = equal_lval
       let hash = hash_lval
       let copy = Datatype.undefined
       let internal_pretty_code = Datatype.undefined
       let pretty fmt x = !pretty_ref fmt x
       let varname _ = "lv"
     end)
end

module LvalStructEq =
  Make_with_collections
    (struct
      type t = lval
       let name = "LvalStructEq"
       let reprs = List.map (fun v -> Var v, NoOffset) Varinfo.reprs
       let compare = StructEq.compare_lval
       let equal = Datatype.from_compare
       let hash = StructEq.hash_lval 13598
       let copy = Datatype.undefined
       let internal_pretty_code = Datatype.undefined
       let pretty fmt x = !Lval.pretty_ref fmt x
       let varname _ = "lv"
     end)

module Offset = struct
  let pretty_ref = ref (fun _ -> assert false)
  include Make_with_collections
    (struct
      type t = offset
       let name = "Offset"
       let reprs = [NoOffset]
       let compare = compare_offset
       let equal = equal_offset
       let hash = hash_offset
       let copy = Datatype.undefined
       let internal_pretty_code = Datatype.undefined
       let pretty fmt x = !pretty_ref fmt x
       let varname _ = "offs"
     end)
end

module OffsetStructEq =
  Make_with_collections
    (struct
      type t = offset
       let name = "OffsetStructEq"
       let reprs = [NoOffset]
       let compare = StructEq.compare_offset
       let equal = Datatype.from_compare
       let hash = StructEq.hash_offset 75489
       let copy = Datatype.undefined
       let internal_pretty_code = Datatype.undefined
       let pretty fmt x = !Offset.pretty_ref fmt x
       let varname _ = "offs"
     end)

(**************************************************************************)
(** {3 ACSL types} *)
(**************************************************************************)

module Logic_var = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = logic_var
      let name = "Logic_var"
      let reprs =
	let dummy v =
          let kind = match v with None -> LVQuant | Some _ -> LVC in
	  { lv_name = "";
            lv_kind = kind;
	    lv_id = -1;
	    lv_type = Linteger;
	    lv_origin = v }
	in
	dummy None
	:: List.map (fun v -> dummy (Some v)) Varinfo.reprs
      let compare v1 v2 = Datatype.Int.compare v1.lv_id v2.lv_id
      let hash v = v.lv_id
      let equal v1 v2 = v1.lv_id = v2.lv_id
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t = !pretty_ref fmt t
      let varname _ = "logic_var"
     end)
end

module Builtin_logic_info =
  Make_with_collections
    (struct
      type t = builtin_logic_info
      let name = "Builtin_logic_info"
      let reprs =
	[ { bl_name = "";
	    bl_labels = [];
	    bl_params = [];
	    bl_type = None;
	    bl_profile = [] } ]
      let compare i1 i2 = String.compare i1.bl_name i2.bl_name
      let hash i = Hashtbl.hash i.bl_name
      let equal i1 i2 = i1.bl_name = i2.bl_name
      let copy = Datatype.identity (* works only if an AST is never modified *)
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Logic_type_info =
  Make_with_collections
    (struct
      type t = logic_type_info
      let name = "Logic_type_info"
      let reprs = [ { lt_name = ""; lt_params = []; lt_def = None } ]
      let compare t1 t2 = String.compare t1.lt_name t2.lt_name
      let equal t1 t2 = t1.lt_name = t2.lt_name
      let hash t = Hashtbl.hash t.lt_name
      let copy = Datatype.identity (* works only if an AST is never modified *)
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Logic_ctor_info =
  Make_with_collections
    (struct
      type t = logic_ctor_info
      let name = "Logic_ctor_info"
      let reprs =
	List.map
	  (fun v -> { ctor_name = ""; ctor_type = v; ctor_params = [] })
	  Logic_type_info.reprs
      let compare t1 t2 = String.compare t1.ctor_name t2.ctor_name
      let equal t1 t2 = t1.ctor_name = t2.ctor_name
      let hash t = Hashtbl.hash t.ctor_name
      let copy = Datatype.identity (* works only if an AST is never modified *)
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Initinfo =
  Make
    (struct
      type t = initinfo
      let name = "Initinfo"
      let reprs =
	{ init = None } ::
	  List.map (fun t -> { init = Some (CompoundInit(t, [])) }) Typ.reprs
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Logic_info =
  Make_with_collections
    (struct
      type t = logic_info
      let name = "Logic_info"
      let reprs =
	List.map
	  (fun v ->
	    { l_var_info = v;
	      l_labels = [];
	      l_tparams = [];
	      l_type = None;
	      l_profile = [];
	      l_body = LBnone })
	  Logic_var.reprs
      let compare i1 i2 = Logic_var.compare i1.l_var_info i2.l_var_info
      let equal i1 i2 = Logic_var.equal i1.l_var_info i2.l_var_info
      let hash i = Logic_var.hash i.l_var_info
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname _ = "logic_varinfo"
     end)


let rec compare_logic_type config v1 v2 =
  let rank = function
    | Linteger -> 0
    | Lreal -> 1
    | Ctype _ -> 2
    | Lvar _ -> 3
    | Ltype _ -> 4
    | Larrow _ -> 5
  in
  let k1 = rank v1 in
  let k2 = rank v2 in
  if k1 <> k2 then k1-k2
  else
    match v1,v2 with
      | Ctype t1 , Ctype t2 -> compare_type config t1 t2
      | Ltype ({lt_def = Some (LTsyn t1)},ts1),
        Ltype ({lt_def = Some (LTsyn t2)},ts2) when config.unroll ->
          let c = compare_logic_type config t1 t2 in
          if c <> 0 then c
          else compare_list (compare_logic_type config) ts1 ts2
      | Ltype(a,ts1), Ltype(b,ts2) ->
  	  let c = Logic_type_info.compare a b in
          if c <> 0 then c
          else compare_list (compare_logic_type config) ts1 ts2
      | Lvar x1, Lvar x2 -> Datatype.String.compare x1 x2
      | Linteger, Linteger -> 0
      | Lreal, Lreal -> 0
      | Larrow(l1, t1), Larrow(l2, t2) ->
          let c = compare_logic_type config t1 t2 in
          if c <> 0 then c else compare_list (compare_logic_type config) l1 l2
      | _ -> assert false

let rec hash_logic_type config = function
  | Linteger -> 0
  | Lreal -> 1
  | Ctype ty -> hash_type config ty
  | Ltype({ lt_def = Some (LTsyn t)},_) when config.unroll ->
      hash_logic_type config t
  | Ltype(t,_) ->
      Logic_type_info.hash t
  | Lvar x -> Datatype.String.hash x
  | Larrow (_,t) -> 41 * hash_logic_type config t


let pretty_logic_type_ref = ref (fun _ _ -> assert false)
module Make_Logic_type
  (M: sig val config: type_compare_config val name: string end) =
  Make_with_collections(
    struct
      include Datatype.Undefined
      type t = logic_type
      let name = M.name
      let reprs = List.map (fun t -> Ctype t) Typ.reprs
        
      let compare = compare_logic_type M.config
      let equal = Datatype.from_compare
      let hash = hash_logic_type M.config
        
      let pretty fmt t = !pretty_logic_type_ref fmt t
        
    end)

module Logic_type =
  Make_Logic_type(
    struct
      let config = { by_name = false; logic_type = true; unroll = true }
      let name = "Logic_type"
    end)

module Logic_type_ByName =
  Make_Logic_type(
  struct
    let name = "Logic_type_ByName"
    let config = { by_name = true; logic_type = true; unroll = false }
  end)

module Logic_type_NoUnroll =
  Make_Logic_type(
    struct
      let name = "Logic_type_NoUnroll"
      let config = { by_name = false; logic_type = false; unroll = false }
    end)

module Model_info = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections(
    struct
      type t = model_info
      include Datatype.Undefined
      let name = "model_info"
      let reprs =
        Extlib.product
          (fun base field ->
            { mi_name = "dummy";
              mi_base_type = base;
              mi_field_type = field;
              mi_decl = Location.unknown;
            })
          Typ.reprs
          Logic_type.reprs
      let compare mi1 mi2 =
        let scmp = String.compare mi1.mi_name mi2.mi_name in
        if scmp <> 0 then scmp
        else
          Typ.compare mi1.mi_base_type mi2.mi_base_type
      let equal = Datatype.from_compare
      let hash mi = Hashtbl.hash mi.mi_name + 3 * Typ.hash mi.mi_base_type
      let copy mi =
        {
          mi_name = String.copy mi.mi_name;
          mi_base_type = Typ.copy mi.mi_base_type;
          mi_field_type = Logic_type.copy mi.mi_field_type;
          mi_decl = Location.copy mi.mi_decl;
        }
      let pretty fmt t = !pretty_ref fmt t
    end)
end

(* -------------------------------------------------------------------------- *)
(* --- Comparison Over Terms                                              --- *)
(* -------------------------------------------------------------------------- *)

let compare_logic_constant c1 c2 = match c1,c2 with
  | Integer (i1,_), Integer(i2,_) -> Integer.compare i1 i2
  | LStr s1, LStr s2 -> Datatype.String.compare s1 s2
  | LWStr s1, LWStr s2 -> compare_list Datatype.Int64.compare s1 s2
  | LChr c1, LChr c2 -> Datatype.Char.compare c1 c2
  | LReal r1, LReal r2 -> Datatype.String.compare r1.r_literal r2.r_literal
  | LEnum e1, LEnum e2 -> Enumitem.compare e1 e2
  | Integer _,(LStr _|LWStr _ |LChr _|LReal _|LEnum _) -> 1
  | LStr _ ,(LWStr _ |LChr _|LReal _|LEnum _) -> 1
  | LWStr _ ,(LChr _|LReal _|LEnum _) -> 1
  | LChr _,(LReal _|LEnum _) -> 1
  | LReal _,LEnum _ -> 1
  | (LStr _|LWStr _ |LChr _|LReal _|LEnum _),
    (Integer _|LStr _|LWStr _ |LChr _|LReal _) -> -1

let rec compare_term t1 t2 =
  let r1 = rank_term t1.term_node in
  let r2 = rank_term t2.term_node in
  if r1 <> r2 then r1 - r2 else
    match t1.term_node , t2.term_node with
    | TConst c1 , TConst c2 -> compare_logic_constant c1 c2
    | TLval lv1 , TLval lv2
    | TAddrOf lv1 , TAddrOf lv2
    | TStartOf lv1 , TStartOf lv2 -> compare_tlval lv1 lv2
    | TSizeOf ty1 , TSizeOf ty2
    | TAlignOf ty1 , TAlignOf ty2 -> Typ.compare ty1 ty2
    | TSizeOfE t1 , TSizeOfE t2
    | TAlignOfE t1 , TAlignOfE t2 -> compare_term t1 t2
    | TSizeOfStr s1 , TSizeOfStr s2 -> String.compare s1 s2
    | TUnOp(op1,t1) , TUnOp(op2,t2) ->
      let c = Extlib.compare_basic op1 op2 in
      if c <> 0 then c else compare_term t1 t2
    | TBinOp(op1,x1,y1) , TBinOp(op2,x2,y2) ->
      let c = Extlib.compare_basic op1 op2 in
      if c <> 0 then c else
	let cx = compare_term x1 x2 in
	if cx <> 0 then cx else compare_term y1 y2
    | TCastE(ty1,t1) , TCastE(ty2,t2) ->
      let c = Typ.compare ty1 ty2 in
      if c <> 0 then c else compare_term t1 t2
    | Tapp(f1,labs1,ts1) , Tapp(f2,labs2,ts2) ->
      let cf = Logic_info.compare f1 f2 in
      if cf <> 0 then cf else
	let cl = compare_list compare_logic_label_pair labs1 labs2 in
	if cl <> 0 then cl else compare_list compare_term ts1 ts2
    | Tlambda(q1,t1) , Tlambda(q2,t2) ->
      let cq = compare_list Logic_var.compare q1 q2 in
      if cq <> 0 then cq else compare_term t1 t2
    | TDataCons(f1,ts1) , TDataCons(f2,ts2) ->
      let cq = compare_ctor f1 f2 in
      if cq <> 0 then cq else compare_list compare_term ts1 ts2
    | Tif(c1,a1,b1) , Tif(c2,a2,b2) ->
      compare_list compare_term [c1;a1;b1] [c2;a2;b2]
    | Tbase_addr (l1,t1) , Tbase_addr (l2,t2)
    | Tblock_length (l1,t1) , Tblock_length (l2,t2)
    | Toffset (l1,t1) , Toffset (l2,t2)
    | Tat(t1,l1) , Tat(t2,l2) ->
      let cl = compare_logic_label l1 l2 in
      if cl <> 0 then cl else compare_term t1 t2
    | Tnull , Tnull -> 0
    | TCoerce(t1,ty1) , TCoerce(t2,ty2) ->
      let ct = Typ.compare ty1 ty2 in
      if ct <> 0 then ct else compare_term t1 t2
    | TCoerceE(t1,ty1) , TCoerceE(t2,ty2) ->
      let ct = compare_term ty1 ty2 in
      if ct <> 0 then ct else compare_term t1 t2
    | TUpdate(x1,off1,y1) , TUpdate(x2,off2,y2) ->
      let cx = compare_term x1 x2 in
      if cx <> 0 then cx else
	let cf = compare_toffset off1 off2 in
	if cf <> 0 then cf else compare_term y1 y2
    | Ttypeof t1 , Ttypeof t2 -> compare_term t1 t2
    | Ttype ty1 , Ttype ty2 -> Typ.compare ty1 ty2
    | Tempty_set , Tempty_set -> 0
    | Tunion ts1 , Tunion ts2
    | Tinter ts1 , Tinter ts2 -> compare_list compare_term ts1 ts2
    | Trange(a1,b1) , Trange(a2,b2) ->
      let c = compare_bound a1 a2 in
      if c <> 0 then c else compare_bound b1 b2
    | Tlet(x1,t1) , Tlet(x2,t2) ->
      let c = Logic_info.compare x1 x2 in
      if c <> 0 then c else compare_term t1 t2
    | Tcomprehension (t1, q1, _p1), Tcomprehension (t2, q2, _p2) ->
        let c = compare_term t1 t2 in
        if c <> 0 then c
        else
          let cq = compare_list Logic_var.compare q1 q2 in
          if cq <> 0 then cq else assert false (* TODO !*)
    | TLogic_coerce(ty1,e1), TLogic_coerce(ty2,e2) ->
        let ct = Logic_type.compare ty1 ty2 in
        if ct <> 0 then ct
        else compare_term e1 e2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _
      | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _ | TAddrOf _ | TStartOf _
      | Tapp _ | Tlambda _ | TDataCons _ | Tif _ | Tat _
      | Tbase_addr _ | Tblock_length _ | Toffset _
      | Tnull | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _
      | Ttype _ | Tempty_set | Tunion _ | Tinter _  | Tcomprehension _
      | Trange _ | Tlet _ 
      | TLogic_coerce _), _ -> assert false

and compare_tlval (h1,off1) (h2,off2) =
  let ch = compare_tlhost h1 h2 in
  if ch <> 0 then ch else compare_toffset off1 off2

and compare_tlhost h1 h2 =
  match h1 , h2 with
  | TVar x1 , TVar x2 -> Logic_var.compare x1 x2
  | TMem m1 , TMem m2 -> compare_term m1 m2
  | TResult ty1 , TResult ty2 -> Typ.compare ty1 ty2
  | TVar _ , TMem _ | TVar _ , TResult _ | TMem _ , TResult _ -> (-1)
  | TMem _ , TVar _ | TResult _ , TVar _ | TResult _ , TMem _ -> 1

and compare_toffset off1 off2 =
  match off1 , off2 with
  | TNoOffset , TNoOffset -> 0
  | TField(f1,next1) , TField(f2,next2) ->
    let cf = Fieldinfo.compare f1 f2 in
    if cf <> 0 then cf else compare_toffset next1 next2
  | TIndex(t1,next1) , TIndex(t2,next2) ->
    let cf = compare_term t1 t2 in
    if cf <> 0 then cf else compare_toffset next1 next2
  | TModel(f1,next1), TModel(f2,next2) ->
      let cf = Model_info.compare f1 f2 in
      if cf <> 0 then cf else compare_toffset next1 next2
  | TNoOffset , (TField _ | TModel _ | TIndex _ )
  | TField _,   (TModel _ | TIndex _)
  | TModel _, TIndex _ -> (-1)
  | TField _, TNoOffset
  | TModel _, (TField _ | TNoOffset)
  | TIndex _, (TModel _ | TField _ | TNoOffset) -> 1

and compare_logic_label_pair (x1,p1) (x2,p2) =
  let c1 = compare_logic_label x1 x2 in
  if c1 <> 0 then c1 else compare_logic_label p1 p2

and compare_logic_label l1 l2 = match l1, l2 with
  | StmtLabel s1 , StmtLabel s2 -> Stmt.compare !s1 !s2
  | LogicLabel (None,l1), LogicLabel (None,l2) -> String.compare l1 l2
  | LogicLabel (Some s1,l1), LogicLabel (Some s2,l2) ->
    let cl = String.compare l1 l2 in
    if cl <> 0 then cl else Stmt.compare s1 s2
  | (StmtLabel _ , LogicLabel _
	| LogicLabel (None,_),LogicLabel (Some _,_)) -> (-1)
  | ( LogicLabel _ , StmtLabel _
	| LogicLabel (Some _,_),LogicLabel (None,_)) -> 1

and compare_ctor c1 c2 = String.compare c1.ctor_name c2.ctor_name

and compare_bound b1 b2 = match b1, b2 with
  | None , None -> 0
  | Some _ , None -> 1
  | None , Some _ -> (-1)
  | Some x , Some y -> compare_term x y


exception StopRecursion of int
let _hash_const c =
  match c with
      CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _  -> Hashtbl.hash c
    | CEnum ei -> 95 + Enumitem.hash ei

let hash_logic_constant c =
  match c with
      Integer _ | LStr _ | LWStr _ | LChr _ | LReal _  -> Hashtbl.hash c
    | LEnum ei -> 95 + Enumitem.hash ei

let hash_label x =
  match x with
      StmtLabel r -> 2*(Stmt.hash !r)
    | LogicLabel(_,l) -> 2*(Hashtbl.hash l) + 1

let rec hash_term (acc,depth,tot) t =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match t.term_node with
      | TConst c -> (acc + hash_logic_constant c, tot - 1)
      | TLval lv -> hash_tlval (acc+19,depth - 1,tot -1) lv
      | TSizeOf t -> (acc + 38 + Typ.hash t, tot - 1)
      | TSizeOfE t -> hash_term (acc+57,depth -1, tot-1) t
      | TSizeOfStr s -> (acc + 76 + Hashtbl.hash s, tot - 1)
      | TAlignOf t -> (acc + 95 + Typ.hash t, tot - 1)
      | TAlignOfE t -> hash_term (acc+114,depth-1,tot-1) t
      | TUnOp(op,t) -> hash_term (acc+133+Hashtbl.hash op,depth-1,tot-2) t
      | TBinOp(bop,t1,t2) ->
        let hash1,tot1 =
          hash_term (acc+152+Hashtbl.hash bop,depth-1,tot-2) t1
        in
        hash_term (hash1,depth-1,tot1) t2
      | TCastE(ty,t) ->
        let hash1 = Typ.hash ty in
        hash_term (acc+171+hash1,depth-1,tot-2) t
      | TAddrOf lv -> hash_tlval (acc+190,depth-1,tot-1) lv
      | TStartOf lv -> hash_tlval (acc+209,depth-1,tot-1) lv
      | Tapp (li,labs,apps) ->
        let hash1 = acc + 228 + Logic_info.hash li in
        let hash_lb (acc,tot) (_,lb) =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + hash_label lb,tot - 1)
        in
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        let res = List.fold_left hash_lb (hash1,tot-1) labs in
        List.fold_left hash_one_term res apps
      | Tlambda(quants,t) ->
        let hash_var (acc,tot) lv =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + Logic_var.hash lv,tot-1)
        in
        let (acc,tot) = List.fold_left hash_var (acc+247,tot-1) quants in
        hash_term (acc,depth-1,tot-1) t
      | TDataCons(ctor,args) ->
        let hash = acc + 266 + Logic_ctor_info.hash ctor in
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (hash,tot-1) args
      | Tif(t1,t2,t3) ->
        let hash1,tot1 = hash_term (acc+285,depth-1,tot) t1 in
        let hash2,tot2 = hash_term (hash1,depth-1,tot1) t2 in
        hash_term (hash2,depth-1,tot2) t3
      | Tat(t,l) ->
        let hash = acc + 304 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tbase_addr (l,t) ->
        let hash = acc + 323 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tblock_length (l,t) ->
        let hash = acc + 342 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Toffset (l,t) ->
        let hash = acc + 351 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tnull -> acc+361, tot - 1
      | TCoerce(t,ty) ->
        let hash = Typ.hash ty in
        hash_term (acc+380+hash,depth-1,tot-2) t
      | TCoerceE(t1,t2) ->
        let hash1,tot1 = hash_term (acc+399,depth-1,tot-1) t1 in
        hash_term (hash1,depth-1,tot1) t2
      | TUpdate(t1,off,t2) ->
        let hash1,tot1 = hash_term (acc+418,depth-1,tot-1) t1 in
        let hash2,tot2 = hash_toffset (hash1,depth-1,tot1) off in
        hash_term (hash2,depth-1,tot2) t2
      | Ttypeof t -> hash_term (acc+437,depth-1,tot-1) t
      | Ttype t -> acc + 456 + Typ.hash t, tot - 1
      | Tempty_set -> acc + 475, tot - 1
      | Tunion tl ->
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (acc+494,tot-1) tl
      | Tinter tl ->
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (acc+513,tot-1) tl
      | Tcomprehension (t,quants,_) -> (* TODO: hash predicates *)
        let hash_var (acc,tot) lv =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + Logic_var.hash lv,tot-1)
        in
        let (acc,tot) = List.fold_left hash_var (acc+532,tot-1) quants in
        hash_term (acc,depth-1,tot-1) t
      | Trange(t1,t2) ->
        let acc = acc + 551 in
        let acc,tot =
          match t1 with
              None -> acc,tot - 1
            | Some t -> hash_term (acc,depth-1,tot-2) t
        in
        if tot <= 0 then raise (StopRecursion acc)
        else
          (match t2 with
              None -> acc, tot - 1
            | Some t -> hash_term (acc,depth-1,tot-1) t)
      | Tlet(li,t) ->
        hash_term
          (acc + 570 + Hashtbl.hash li.l_var_info.lv_name, depth-1, tot-1)
          t
      | TLogic_coerce(_,e) -> hash_term (acc + 587, depth - 1, tot - 1) e
  end

and hash_tlval (acc,depth,tot) (h,o) =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    let hash, tot = hash_tlhost (acc, depth - 1, tot - 1) h in
    hash_toffset (hash,depth-1,tot) o
  end

and hash_tlhost (acc,depth,tot) t =
  if tot <=0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match t with
      | TVar v -> acc + 17 + Logic_var.hash v, tot - 1
      | TResult typ -> 31 + 7 * Typ.hash typ, tot - 2
      | TMem t -> hash_term (acc + 71, depth - 1, tot - 1) t
  end

and hash_toffset (acc, depth, tot) t =
  if depth <= 0 || tot <= 0 then raise (StopRecursion acc)
  else begin
    match t with
      | TNoOffset -> acc, tot - 1
      | TField(f,o) ->
          hash_toffset (acc+13+Fieldinfo.hash f, depth -1, tot - 1) o
      | TModel (mi, o) ->
          hash_toffset (acc+41+Model_info.hash mi, depth - 1, tot - 1) o
      | TIndex (t, o) ->
          let hash, tot = hash_term (acc+73, depth - 1, tot - 1) t in
          hash_toffset (hash, depth - 1, tot) o
  end

let hash_fct f t = try fst (f (0,10,100) t) with StopRecursion n -> n

module Logic_constant =
  Make_with_collections
    (struct
      type t = logic_constant
      let name = "Logic_constant"
      let reprs = [LStr "Foo"]
      let compare = compare_logic_constant
      let equal = Datatype.from_compare
      let hash = hash_logic_constant
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname _ = "lconst"
     end)

module Term = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = term
      let name = "Term"
      let reprs =
	List.map
	  (fun t ->
	    { term_node = TConst (LStr "");
	      term_loc = Location.unknown;
	      term_type =  t;
	      term_name = [] })
	  Logic_type.reprs
      let compare = compare_term
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
      let hash = hash_fct hash_term
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t = !pretty_ref fmt t
      let varname _ = "term"
     end)
end

module Identified_term =
  Make_with_collections
    (struct
        type t = identified_term
        let name = "Identified_term"
        let reprs =
          List.map (fun t -> { it_id = -1; it_content = t}) Term.reprs
        let compare x y = Extlib.compare_basic x.it_id y.it_id
        let equal x y = x.it_id = y.it_id
        let copy x =
	  (* NB: Term.copy itself is undefined. *)
          { it_id = x.it_id; it_content = Term.copy x.it_content }
        let hash x = x.it_id
        let internal_pretty_code = Datatype.undefined
        let pretty = Datatype.undefined
        let varname _ = "id_term"
     end)

module Term_lhost =
  Make_with_collections
    (struct
      type t = term_lhost
      let name = "Term_lhost"
      let reprs =
	List.fold_left
	  (fun acc ty ->
	    List.fold_left
	      (fun acc t -> TMem t :: acc)
	      (TResult ty :: acc)
	      Term.reprs)
	  (List.map (fun lv -> TVar lv) Logic_var.reprs)
	  Typ.reprs
      let compare = compare_tlhost
      let equal = Datatype.from_compare
      let hash = hash_fct hash_tlhost
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Term_offset = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = term_offset
      let name = "Term_offset"
      let reprs = [ TNoOffset ]
      let compare = compare_toffset
      let equal = Datatype.from_compare
      let hash = hash_fct hash_toffset
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t_o = !pretty_ref fmt t_o
      let varname = Datatype.undefined
     end)
end

module Term_lval = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Datatype.Pair_with_collections
    (Term_lhost)
    (Term_offset)
    (struct let module_name = "Cil_datatype.Term_lval" end)
  let pretty fmt t = !pretty_ref fmt t
end

module Logic_label =
  Make_with_collections
    (struct
        type t = logic_label
        let name = "Logic_label"
        let reprs =
          (LogicLabel (None,"Pre"))
          :: List.map (fun x -> StmtLabel (ref x)) Stmt.reprs
        let compare = compare_logic_label
        let equal = Datatype.from_compare
        let copy = Datatype.undefined
        let hash = hash_label
        let internal_pretty_code = Datatype.undefined
        let pretty = Datatype.undefined
        let varname _ = "logic_label"
     end)

module Global_annotation = struct

  include Make_with_collections
    (struct
      type t = global_annotation
      let name = "Global_annotation"
      let reprs = List.map (fun l -> Daxiomatic ("", [], l)) Location.reprs
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined

      let rec compare g1 g2 =
        match g1,g2 with
          | Dfun_or_pred (l1,_), Dfun_or_pred(l2,_) -> Logic_info.compare l1 l2
          | Dfun_or_pred _,_ -> -1
          | _, Dfun_or_pred _ -> 1
          | Dvolatile (it1,_,_,_), Dvolatile(it2,_,_,_) ->
            compare_list Identified_term.compare it1 it2
          | Dvolatile _,_ -> -1
          | _,Dvolatile _ -> 1
          | Daxiomatic (_,g1,_), Daxiomatic (_,g2,_) ->
            (* ACSL does not require the name to be unique. *)
            compare_list compare g1 g2
          | Daxiomatic _, _ -> -1
          | _, Daxiomatic _ -> 1
          | Dtype(t1,_), Dtype(t2,_) -> Logic_type_info.compare t1 t2
          | Dtype _, _ -> -1
          | _, Dtype _ -> 1
          | Dlemma (l1,_,_,_,_,_), Dlemma(l2,_,_,_,_,_) ->
            Datatype.String.compare l1 l2
          | Dlemma _, _ -> -1
          | _, Dlemma _ -> 1
          | Dinvariant (l1,_), Dinvariant (l2,_) -> Logic_info.compare l1 l2
          | Dinvariant _, _ -> -1
          | _, Dinvariant _ -> 1
          | Dtype_annot(l1, _), Dtype_annot (l2, _) -> Logic_info.compare l1 l2
          | Dtype_annot _, _ -> -1
          | _, Dtype_annot _ -> 1
          | Dmodel_annot(l1,_), Dmodel_annot(l2,_) -> Model_info.compare l1 l2
	  | Dmodel_annot _, _ -> -1
          | _, Dmodel_annot _ -> 1
	  | Dcustom_annot(_, n1, _), Dcustom_annot(_, n2, _) -> Datatype.String.compare n1 n2

      let equal = Datatype.from_compare

      let rec hash g = match g with
        | Dfun_or_pred (l,_) -> 2 * Logic_info.hash l
        | Dvolatile ([],_,_,(source,_)) ->
          Kernel.fatal ~source "Empty location list for volatile annotation"
        | Dvolatile (t::_,_,_,_) -> 3 * Identified_term.hash t
        | Daxiomatic (_,[],_) -> 5
        (* Empty axiomatic is weird but authorized. *)
        | Daxiomatic (_,g::_,_) -> 5 * hash g
        | Dtype (t,_) -> 7 * Logic_type_info.hash t
        | Dlemma(n,_,_,_,_,_) -> 11 * Datatype.String.hash n
        | Dinvariant(l,_) -> 13 * Logic_info.hash l
        | Dtype_annot(l,_) -> 17 * Logic_info.hash l
        | Dmodel_annot(l,_) -> 19 * Model_info.hash l
        | Dcustom_annot(_,n,_) -> 23 * Datatype.String.hash n

      let copy = Datatype.undefined
     end)

  let loc = function
    | Dfun_or_pred(_, loc)
    | Daxiomatic(_, _, loc)
    | Dtype (_, loc)
    | Dlemma(_, _, _, _, _, loc)
    | Dinvariant(_, loc)
    | Dtype_annot(_, loc) -> loc
    | Dmodel_annot(_, loc) -> loc
    | Dvolatile(_, _, _, loc) -> loc
    | Dcustom_annot(_,_,loc) -> loc
end

module Global = struct

  include Make_with_collections
    (struct
      type t = global
      let name = "Global"
      let reprs = [ GText "" ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined

      let compare g1 g2 =
        match g1, g2 with
          | GType (t1,_), GType (t2,_) -> Typeinfo.compare t1 t2
          | GType _,_ -> -1
          | _, GType _ -> 1
          | GCompTag (t1,_), GCompTag(t2,_) -> Compinfo.compare t1 t2
          | GCompTag _,_ -> -1
          | _, GCompTag _ -> 1
          | GCompTagDecl (t1,_), GCompTagDecl(t2,_) -> Compinfo.compare t1 t2
          | GCompTagDecl _,_ -> -1
          | _,GCompTagDecl _ -> 1
          | GEnumTag(t1,_), GEnumTag(t2,_) -> Enuminfo.compare t1 t2
          | GEnumTag _,_ -> -1
          | _, GEnumTag _ -> 1
          | GEnumTagDecl(t1,_), GEnumTagDecl(t2,_) -> Enuminfo.compare t1 t2
          | GEnumTagDecl _, _ -> -1
          | _, GEnumTagDecl _ -> 1
          | GVarDecl (_,v1,_), GVarDecl(_,v2,_) -> Varinfo.compare v1 v2
          | GVarDecl _,_ -> -1
          | _,GVarDecl _ -> 1
          | GVar (v1,_,_), GVar (v2,_,_) -> Varinfo.compare v1 v2
          | GVar _,_ -> -1
          | _, GVar _ -> 1
          | GFun(f1,_), GFun(f2,_) -> Varinfo.compare f1.svar f2.svar
          | GFun _, _ -> -1
          | _, GFun _ -> 1
          | GAsm (_,l1), GAsm(_,l2) -> Location.compare l1 l2
          | GAsm _, _ -> -1
          | _, GAsm _ -> 1
          | GPragma(_,l1), GPragma(_,l2) -> Location.compare l1 l2
          | GPragma _, _ -> -1
          | _, GPragma _ -> 1
          | GText s1, GText s2 -> Datatype.String.compare s1 s2
          | GText _, _ -> -1
          | _, GText _ -> 1
          | GAnnot (g1,_), GAnnot(g2,_) -> Global_annotation.compare g1 g2

      let equal = Datatype.from_compare

      let hash g = match g with
          GType (t,_) -> 2 * Typeinfo.hash t
        | GCompTag (t,_) -> 3 * Compinfo.hash t
        | GCompTagDecl (t,_) -> 5 * Compinfo.hash t
        | GEnumTag (t,_) -> 7 * Enuminfo.hash t
        | GEnumTagDecl(t,_) -> 11 * Enuminfo.hash t
        | GVarDecl (_,v,_) -> 13 * Varinfo.hash v
        | GVar (v,_,_) -> 17 * Varinfo.hash v
        | GFun (f,_) -> 19 * Varinfo.hash f.svar
        | GAsm (_,l) -> 23 * Location.hash l
        | GText t -> 29 * Datatype.String.hash t
        | GAnnot (g,_) -> 31 * Global_annotation.hash g
        | GPragma(_,l) -> 37 * Location.hash l

      let copy = Datatype.undefined
     end)

  let loc = function
    | GFun(_, l)
    | GType(_, l)
    | GEnumTag(_, l)
    | GEnumTagDecl(_, l)
    | GCompTag(_, l)
    | GCompTagDecl(_, l)
    | GVarDecl(_, _, l)
    | GVar(_, _, l)
    | GAsm(_, l)
    | GPragma(_, l)
    | GAnnot (_, l) -> l
    | GText _ -> Location.unknown

end

module Kf = struct

  let vi kf = match kf.fundec with
    | Definition (d, _) -> d.svar
    | Declaration (_,vi,_, _) -> vi

  let id kf = (vi kf).vid

  let set_formal_decls = ref (fun _ _ -> assert false)

  include Datatype.Make_with_collections
  (struct
    type t = kernel_function
    let name = "Cil_datatype.Kf"
    let structural_descr = Structural_descr.t_abstract
    let reprs =
      let empty_spec =
	{ spec_behavior = [];
	  spec_variant = None;
	  spec_terminates = None;
	  spec_complete_behaviors = [];
	  spec_disjoint_behaviors = [] }
      in
      List.fold_left
	(fun acc loc ->
	  List.fold_left
	    (fun acc b ->
	      List.fold_left
		(fun acc vi ->
		  { fundec =
		      Definition
			({ svar  = vi;
			   smaxid = 0;
			   slocals = [];
			   sformals = [];
			   sbody = b;
			   smaxstmtid = None;
			   sallstmts = [];
			   sspec = empty_spec },
			 loc);
		      return_stmt = None;
		      spec = empty_spec } :: acc)
		acc
		Varinfo.reprs)
	    acc
	    Block.reprs)
	[]
	Location.reprs
    let compare k1 k2 = Datatype.Int.compare (id k1) (id k2)
    let equal k1 k2 =
      if k1 != k2 then (
        if (id k1) = (id k2) then
          Kernel.fatal "Two kf for %a (%d) and %a (%d) (%d)"
            Varinfo.pretty (vi k1) (Extlib.address_of_value k1)
	    Varinfo.pretty (vi k2) (Extlib.address_of_value k2)
	    (id k1);
        false)
      else true
    let hash = id
    let copy = Datatype.undefined
    let rehash x = match x.fundec with
    | Definition _ | Declaration (_, _, None, _)-> x
    | Declaration (_, v, Some args, _) ->
      !set_formal_decls v args;
      x
    let get_name_kf kf = (vi kf).Cil_types.vname
    let internal_pretty_code p_caller fmt kf =
      Type.par p_caller Type.Call fmt
	(fun fmt ->
	  Format.fprintf fmt "@[<hv 2>Globals.Functions.find_by_name@;%S@]"
	    (get_name_kf kf))
    let pretty fmt kf = Varinfo.pretty fmt (vi kf)
    let mem_project = Datatype.never_any_project
    let varname kf = "kf_" ^ (get_name_kf kf)
   end)
  let () = Type.set_ml_name ty (Some "Kernel_function.ty")

end

module Code_annotation = struct

  let pretty_ref = ref (fun _ _ -> assert false)

  include Make_with_collections
    (struct
      type t = code_annotation
      let name = "Code_annotation"
      let reprs =
	[ { annot_content = AAssigns([],WritesAny); annot_id = -1 } ]
      let hash x = x.annot_id
      let equal x y = x.annot_id = y.annot_id
      let compare x y = Datatype.Int.compare x.annot_id y.annot_id
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt ca = !pretty_ref fmt ca
      let varname _ = "code_annot"
     end)

  let loc ca = match ca.annot_content with
    | AAssert(_,{loc=loc})
    | AInvariant(_,_,{loc=loc})
    | AVariant({term_loc=loc},_) -> Some loc
    | AAssigns _ | AAllocation _ | APragma _
    | AStmtSpec _ -> None

end

module Funspec =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = funspec
      let name = "Funspec"
      let reprs =
	[ { spec_behavior = [];
	    spec_variant = None;
	    spec_terminates = None;
	    spec_complete_behaviors = [];
	    spec_disjoint_behaviors = [] } ]
      let mem_project = Datatype.never_any_project
     end)

module Fundec = struct 

  let make_dummy vi fs = { 
    svar = vi; 
    sformals = [];
    slocals = [];
    smaxid = 0;
    sbody = { battrs = [] ; blocals = []; bstmts = [] };
    smaxstmtid = None;
    sallstmts = [];
    sspec = fs ;
  }

  let reprs = List.fold_left (fun list vi ->
    List.fold_left (fun list fs ->
      ((make_dummy vi fs)::list)) list Funspec.reprs)
    [] Varinfo.reprs;;

  include Datatype.Make_with_collections
  (struct
    type t = fundec
    let name = "Fundec"
    let varname v = "fd_" ^ v.svar.vorig_name
    let reprs = reprs
    let structural_descr = Structural_descr.t_abstract
    let compare v1 v2 = Datatype.Int.compare v1.svar.vid v2.svar.vid
    let hash v = v.svar.vid
    let equal v1 v2 = v1.svar.vid = v2.svar.vid
    let rehash = Datatype.identity
    let copy = Datatype.undefined
    let pretty = Datatype.undefined
    let internal_pretty_code = Datatype.undefined
    let mem_project = Datatype.never_any_project
   end)
end

module Predicate_named = 
  Make
    (struct
      type t = predicate named
      let name = "Predicate_named"
      let reprs = 
	[ { name = [ "" ]; loc = Location.unknown; content = Pfalse } ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname _ = "p"
     end)

module Identified_predicate =
  Make_with_collections
    (struct
        type t = identified_predicate
        let name = "Identified_predicate"
        let reprs =
          [ { ip_name = [ "" ]; 
	      ip_loc = Location.unknown; 
	      ip_content = Pfalse; 
	      ip_id = -1} ]
        let compare x y = Extlib.compare_basic x.ip_id y.ip_id
        let equal x y = x.ip_id = y.ip_id
        let copy = Datatype.undefined
        let hash x = x.ip_id
        let internal_pretty_code = Datatype.undefined
        let pretty = Datatype.undefined
        let varname _ = "id_predyes"
     end)

(**************************************************************************)
(** {3 Logic_ptree}
    Sorted by alphabetic order. *)
(**************************************************************************)

module Lexpr =
  Make
    (struct
      open Logic_ptree
      type t = lexpr
      let name = "Lexpr"
      let reprs = [ { lexpr_node = PLvar ""; lexpr_loc = Location.unknown } ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

(**************************************************************************)
(** {3 Other types} *)
(**************************************************************************)

module Localisation =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = localisation
      let name = "Localisation"
      let reprs = [ VGlobal ]
      let internal_pretty_code p_caller fmt loc =
	let pp s kf =
	  Type.par p_caller Type.Call fmt
	    (fun fmt ->
	      Format.fprintf fmt "@[<hv 2>%s@;%a@]"
		s
		(Kf.internal_pretty_code Type.Call)
		kf)
	in
	match loc with
	| VGlobal -> Format.fprintf fmt "Cil_types.VGlobal"
	| VLocal kf -> pp "Cil_types.VLocal" kf
	| VFormal kf -> pp "Cil_types.VFormal" kf
      let mem_project = Datatype.never_any_project
     end)

(* -------------------------------------------------------------------------- *)
(* --- Internal                                                           --- *)
(* -------------------------------------------------------------------------- *)

let clear_caches () = List.iter (fun f -> f ()) !clear_caches

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
