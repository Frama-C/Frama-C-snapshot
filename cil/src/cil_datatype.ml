(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

open Cil_types
let (=?=) = Extlib.compare_basic
let compare_list = Extlib.list_compare
let hash_list f = List.fold_left (fun acc d -> 65537 * acc + f d) 1

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
      let structural_descr = Structural_descr.Abstract
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
      let structural_descr = Structural_descr.Abstract
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
     end)

let compare_chain cmp x1 x2 next arg =
  let res = cmp x1 x2 in if res = 0 then next arg else res


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
  | TStartOf _ -> 10
  | Tapp _ -> 11
  | Tlambda _ -> 12
  | TDataCons _ -> 13
  | Tif _ -> 14
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
  | _ -> assert false

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
  let pretty_ref = ref (fun _ _ -> assert false)
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
      let pretty fmt x = !pretty_ref fmt x
      let varname _ = "loc"
     end)
end

module Instr = struct

  include Make
    (struct
      type t = instr
      let name = "Instr"
      let reprs = List.map (fun l -> Skip l) Location.reprs
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

  let loc = function
    | Skip l
    | Set (_,_,l)
    | Call (_,_,_,l)
    | Asm (_,_,_,_,_,l)
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

module Stmt = struct

  let pretty_ref = ref (fun _ _ -> assert false)

  module Aux = 
    Make_with_collections
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

  include Aux

  let pretty_sid fmt s = Format.pp_print_int fmt s.sid

  module Hptset = struct
    include Hptset.Make
      (struct include Aux
              let id s = s.sid
              let pretty = pretty_sid end)
      (struct let v = [ [ ] ] end)
      (struct let l = [ ] (* This should be [Ast.self], but cannot be done
                             here *) end)
  end

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

module Label =
  Make
    (struct
      type t = label
      let name = "Label"
      let reprs =
	[ Label("", Location.unknown, false); Default Location.unknown ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

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
  | ASizeOfS _ -> 5
  | AAlignOf _ -> 6
  | AAlignOfE _ -> 7
  | AAlignOfS _ -> 8
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
let ptypeAddAttributes =
  ref (fun _ _ -> failwith "ptypedAddAttributes not yet defined")

let rec compare_attribute a1 a2 = match a1, a2 with
  | Attr (s1, l1), Attr (s2, l2) ->
      compare_chain (=?=) s1 s2 (compare_attrparam_list l1) l2
  | AttrAnnot s1, AttrAnnot s2 -> s1 =?= s2
  | Attr _, AttrAnnot _ -> -1
  | AttrAnnot _, Attr _ -> 1
and compare_attributes l1 l2 = compare_list compare_attribute l1 l2
and compare_attrparam_list l1 l2 =
  compare_list compare_attrparam l1 l2
and compare_attrparam a1 a2 = match a1, a2 with
  | AInt i1, AInt i2 -> i1 =?= i2
  | AStr s1, AStr s2 -> s1 =?= s2
  | ACons (s1, l1), ACons (s2, l2) ->
      compare_chain (=?=) s1 s2 (compare_attrparam_list l1) l2
  | ASizeOf t1, ASizeOf t2 -> compare_type t1 t2
  | ASizeOfE p1, ASizeOfE p2 -> compare_attrparam p1 p2
  | ASizeOfS s1, ASizeOfS s2 -> compare_typsig s1 s2
  | AAlignOf t1, AAlignOf t2 -> compare_type t1 t2
  | AAlignOfE p1, AAlignOfE p2 -> compare_attrparam p1 p2
  | AAlignOfS s1, AAlignOfS s2 -> compare_typsig s1 s2
  | AUnOp (op1, a1), AUnOp (op2, a2) ->
     compare_chain (=?=) op1 op2 (compare_attrparam a1) a2
  | ABinOp (op1, a1, a1'), ABinOp (op2, a2, a2') ->
     compare_chain (=?=) op1 op2
       (compare_chain compare_attrparam a1 a2 (compare_attrparam a1')) a2'
  | ADot (a1, s1), ADot (a2, s2) ->
      compare_chain (=?=) s1 s2 (compare_attrparam a1) a2
  | AStar a1, AStar a2
  | AAddrOf a1, AAddrOf a2 -> compare_attrparam a1 a2
  | AIndex (a1, a1'), AIndex (a2, a2') ->
      compare_chain compare_attrparam a1 a2 (compare_attrparam a1') a2'
  | AQuestion (a1, a1', a1''), AQuestion (a2, a2', a2'') ->
      compare_chain compare_attrparam a1 a2
        (compare_chain compare_attrparam a1' a2' (compare_attrparam a1'')) a2''
  | (AInt _ | AStr _ | ACons _ | ASizeOf _ | ASizeOfE _ | ASizeOfS _ |
        AAlignOf _ | AAlignOfE _ | AAlignOfS _ | AUnOp _ | ABinOp _ | ADot _ |
        AStar _ | AAddrOf _ | AIndex _ | AQuestion _ as a1), a2 ->
      index_attrparam a1 - index_attrparam a2
and compare_type t1 t2 =
  if t1 == t2 then 0
  else
    match t1, t2 with
      | TVoid l1, TVoid l2 -> compare_attributes l1 l2
      | TInt (i1, l1), TInt (i2, l2) ->
          compare_chain (=?=) i1 i2 (compare_attributes l1) l2
      | TFloat (f1, l1), TFloat (f2, l2) ->
          compare_chain (=?=) f1 f2 (compare_attributes l1) l2
      | TPtr (t1, l1), TPtr (t2, l2) ->
          compare_chain compare_type t1 t2 (compare_attributes l1) l2
      | TArray (t1', _, _, l1), TArray (t2', _, _, l2) ->
          compare_chain (=?=) (!pbitsSizeOf t1) (!pbitsSizeOf t2)
            (compare_chain compare_type t1' t2' (compare_attributes l1)) l2
      | TFun (r1, a1, v1, l1), TFun (r2, a2, v2, l2) ->
          compare_chain compare_type r1 r2
            (compare_chain (=?=) v1 v2
               (compare_chain compare_arg_list a1 a2
                  (compare_attributes l1))) l2
      | TNamed (t1, l1), TNamed (t2, l2) ->
          compare_type
            (if l1 == [] then t1.ttype else !ptypeAddAttributes l1 t1.ttype)
            (if l2 == [] then t2.ttype else !ptypeAddAttributes l2 t2.ttype)
      | TComp (c1, _, l1), TComp (c2, _, l2) ->
          compare_chain (=?=) c1.ckey c2.ckey (compare_attributes l1) l2
      | TEnum (e1, l1), TEnum (e2, l2) ->
          compare_chain (=?=) e1.ename e2.ename (compare_attributes l1) l2
      | TBuiltin_va_list l1, TBuiltin_va_list l2 -> compare_attributes l1 l2
      | (TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _ | TNamed _ |
             TComp _ | TEnum _ | TBuiltin_va_list _ as a1), a2 ->
          index_typ a1 - index_typ a2

and compare_typsig _t1 _t2 = Kernel.not_yet_implemented "Typsig comparison"
and compare_arg_list l1 l2 =
  Extlib.opt_compare
    (compare_list
       (fun (n1, t1, l1) (n2, t2, l2) ->
         compare_chain (=?=) n1 n2
           (compare_chain compare_type t1 t2 (compare_attributes l1)) l2
       )) l1 l2

let hash_attribute = function
  | AttrAnnot s -> Hashtbl.hash s
  | Attr (s, _) -> (* We do not hash attrparams. There is a recursivity problem
       with typ, and the equal function will be complicated enough in itself *)
      3 * Hashtbl.hash s + 117
let hash_attributes = hash_list hash_attribute

let rec hash_type = function
  | TVoid l -> Hashtbl.hash (hash_attributes l, 1)
  | TInt (i, l) -> Hashtbl.hash (i, 2, hash_attributes l)
  | TFloat (f, l) -> Hashtbl.hash (f, 3, hash_attributes l)
  | TPtr (t, l) -> Hashtbl.hash (hash_type t, 4, hash_attributes l)
  | TArray (t, _, { scache = Computed i}, l) ->
      Hashtbl.hash (hash_type t, 5, i, hash_attributes l)
  | TArray (t, _, { scache = _}, l) as tar ->
      Hashtbl.hash (hash_type t, 5, !pbitsSizeOf tar, hash_attributes l)
  | TFun (r, a, v, l) ->
      Hashtbl.hash (hash_type r, 6, hash_args a, v, hash_attributes l)
  | TNamed (ti, l) -> Hashtbl.hash (ti.tname, 7, hash_attributes l)
  | TComp (c, _, l) -> Hashtbl.hash (c.ckey, 8, hash_attributes l)
  | TEnum (e, l) -> Hashtbl.hash (e.ename, 9, hash_attributes l)
  | TBuiltin_va_list l -> Hashtbl.hash (hash_attributes l, 10)
and hash_args = function
  | None -> 11713
  | Some l ->
      hash_list (fun (n, t, l) -> Hashtbl.hash (n, 17, hash_type t, hash_attributes l)) l

module Typ = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = typ
      let name = "Typ"
      let reprs = [ TVoid [] ]
      let compare = compare_type
      let hash = hash_type
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt t = !pretty_ref fmt t
      let varname = Datatype.undefined
     end)
end

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
     end)
end

module Varinfo = struct
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

  module Aux = Make_with_collections
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
  let pretty_vname fmt v = Format.pp_print_string fmt v.vname

  include Aux

  let pretty_vid fmt v = Format.pp_print_int fmt v.vid

  module Hptset = struct
    include Hptset.Make
      (struct include Aux
              let id v = v.vid
              let pretty = pretty_vid end)
      (struct let v = [ [ ] ] end)
      (struct let l = [ ] (* Should morally be [Ast.self] *) end)
  end

end

module Block = struct
  include Make
    (struct
      type t = block
      let name = "Block"
      let reprs =
	[ { battrs = []; blocals = Varinfo.reprs; bstmts = Stmt.reprs } ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)
  let equal b1 b2 = (b1 == b2)
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
  compare_chain compare_lhost h1 h2 (compare_offset o1) o2

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
    compare_chain Fieldinfo.compare f1 f2 (compare_offset o1) o2
  | Index(e1,o1), Index(e2,o2) ->
    compare_chain Exp.compare e1 e2 (compare_offset o1) o2
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
	  { lv_name = "";
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

module Logic_type = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      include Datatype.Undefined
      type t = logic_type
      let name = "Logic_type"
      let reprs = List.map (fun t -> Ctype t) Typ.reprs

      let rank = function
	| Linteger -> 0
	| Lreal -> 1
	| Ctype _ -> 2
	| Lvar _ -> 3
	| Ltype _ -> 4
	| Larrow _ -> 5

      let rec compare v1 v2 =
	let k1 = rank v1 in
	let k2 = rank v2 in
	if k1 <> k2 then k1-k2
	else
	  match v1,v2 with
	  | Ctype t1 , Ctype t2 ->
	    Typ.compare t1 t2
	  | Ltype (a,ts1), Ltype (b,ts2) ->
	    let c = Logic_type_info.compare a b in
	    if c <> 0 then c else compare_list compare ts1 ts2
	  | Lvar x1, Lvar x2 -> Datatype.String.compare x1 x2
	  | Linteger, Linteger -> 0
	  | Lreal, Lreal -> 0
	  | Larrow(l1, t1), Larrow(l2, t2) ->
	    let c = compare t1 t2 in
	    if c <> 0 then c else compare_list compare l1 l2
	  | _ -> assert false

      let rec hash = function
	| Linteger -> 0
	| Lreal -> 1
	| Ctype ty -> Typ.hash ty
	| Ltype(t,_) -> Logic_type_info.hash t
	| Lvar x -> Datatype.String.hash x
	| Larrow (_,t) -> 41 * hash t

      let rec equal v1 v2 = match v1, v2 with
	| Ctype t1 , Ctype t2 -> Typ.equal t1 t2
	| Ltype (a,ts1), Ltype (b,ts2) ->
	  Logic_type_info.equal a b
	  && (try List.for_all2 equal ts1 ts2 with Invalid_argument _ -> false)
	| Lvar x1,Lvar x2 -> Datatype.String.equal x1 x2
	| Linteger,Linteger -> true
	| Lreal, Lreal -> true
	| Larrow(l1, t1), Larrow(l2, t2) ->
	  equal t1 t2
	  && (try List.for_all2 equal l1 l2 with Invalid_argument _ -> false)
	| _ -> false

      let pretty fmt t = !pretty_ref fmt t

     end)
end

(* -------------------------------------------------------------------------- *)
(* --- Comparison Over Terms                                              --- *)
(* -------------------------------------------------------------------------- *)

let compare_constant c1 c2 = match c1, c2 with
  | CInt64(v1,k1,_), CInt64(v2,k2,_) ->
    compare_chain My_bigint.compare v1 v2 (Extlib.compare_basic k1) k2
  | CStr s1, CStr s2 -> Datatype.String.compare s1 s2
  | CWStr s1, CWStr s2 -> compare_list Datatype.Int64.compare s1 s2
  | CChr c1, CChr c2 -> Datatype.Char.compare c1 c2
  | CReal (f1,k1,_), CReal(f2,k2,_) ->
    compare_chain Datatype.Float.compare f1 f2
      (Extlib.compare_basic k1) k2
  | CEnum e1, CEnum e2 -> Enumitem.compare e1 e2
  | (CInt64 _, (CStr _ | CWStr _ | CChr _ | CReal _ | CEnum _)) -> 1
  | (CStr _, (CWStr _ | CChr _ | CReal _ | CEnum _)) -> 1
  | (CWStr _, (CChr _ | CReal _ | CEnum _)) -> 1
  | (CChr _, (CReal _ | CEnum _)) -> 1
  | (CReal _, CEnum _) -> 1
  | (CStr _ | CWStr _ | CChr _ | CReal _ | CEnum _),
    (CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _) -> -1

let rec compare_term t1 t2 =
  let r1 = rank_term t1.term_node in
  let r2 = rank_term t2.term_node in
  if r1 <> r2 then r1 - r2 else
    match t1.term_node , t2.term_node with
      | TConst c1 , TConst c2 -> compare_constant c1 c2
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
      | Tbase_addr t1 , Tbase_addr t2
      | Tblock_length t1 , Tblock_length t2
	  -> compare_term t1 t2
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
      | _ -> assert false

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
    | TNoOffset , TField _ | TNoOffset , TIndex _ | TField _ , TIndex _ -> (-1)
    | TField _ , TNoOffset | TIndex _ , TNoOffset | TIndex _ , TField _ -> 1

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

module Term = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = term
      let name = "Term"
      let reprs =
	List.map
	  (fun t ->
	    { term_node = TConst (CStr "");
	      term_loc = Location.unknown;
	      term_type =  t;
	      term_name = [] })
	  Logic_type.reprs
      let compare = compare_term
      let equal = Datatype.from_compare
      let copy = Datatype.undefined
      let hash = Hashtbl.hash (* could be optimized *)
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
        let compare x y = Pervasives.compare x.it_id y.it_id
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
      let hash = Hashtbl.hash
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Term_offset =
  Make_with_collections 
    (struct
      type t = term_offset
      let name = "Term_offset"
      let reprs = [ TNoOffset ]
      let compare = compare_toffset
      let equal = Datatype.from_compare
      let hash = Hashtbl.hash
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

module Term_lval = 
  Datatype.Pair_with_collections
    (Term_lhost)
    (Term_offset)
    (struct let module_name = "Cil_datatype.Term_lval" end)

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
        let hash x =
          match x with
              StmtLabel r -> 2*(Stmt.hash !r)
            | LogicLabel(_,l) -> 2*(Hashtbl.hash l) + 1
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
          | Dmodel_annot(l1,_), Dmodel_annot(l2,_) -> Logic_info.compare l1 l2

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
        | Dmodel_annot(l,_) -> 19 * Logic_info.hash l

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
    let structural_descr = Structural_descr.Abstract
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
      if k1 != k2 then 
        (assert 
           (Kernel.verify 
              ((id k1) <> (id k2)) "Two kf for %a(%d)" 
              Varinfo.pretty (vi k1) (id k1)); 
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

end

module Code_annotation = struct

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
      let pretty = Datatype.undefined
      let varname _ = "code_annot"
     end)

  let loc ca = match ca.annot_content with
    | AAssert(_,{loc=loc})
    | AInvariant(_,_,{loc=loc})
    | AVariant({term_loc=loc},_) -> Some loc
    | AAssigns _ | APragma _
    | AStmtSpec _ -> None

end

module Rooted_code_annotation =
  Datatype.Make
    (struct
       include Datatype.Serializable_undefined
       type t = rooted_code_annotation
       let name = "rooted_code_annotation"
       let reprs =
	 List.map (fun c -> User c) Code_annotation.reprs
       let compare x y = match x, y with
	 | User a, User b
	 | AI(_, a), AI(_, b) -> Code_annotation.compare a b
	 | User _, AI _ -> -1
	 | AI _, User _ -> 1
       let equal = Datatype.from_compare
       let mem_project = Datatype.never_any_project
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

module Int_hashtbl =
  Datatype.Hashtbl
    (Inthash)
    (Datatype.Int)
    (struct let module_name = "Inthash" end)

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

module Alarm =
  Make_with_collections
    (struct
      type t = alarm
      let name = "Alarm"
      let reprs = [ Division_alarm ]
      let compare = Pervasives.compare
      let equal = (=)
      let hash = Hashtbl.hash
      let copy = Datatype.identity
      let pretty = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
