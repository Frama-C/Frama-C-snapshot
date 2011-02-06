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

let rec compare_list f xs ys =
  match xs , ys with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs, y :: ys ->
    let c = f x y in
    if c <> 0 then c else compare_list f xs ys

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
  | Told _ -> 15
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

module Location = struct
  let unknown = Lexing.dummy_pos, Lexing.dummy_pos
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = location
      let name = "Location"
      let reprs = [ unknown ]
      let compare: location -> location -> int = Extlib.compare_basic
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

end

let pTypeSig : (typ -> typsig) ref = ref (fun _ -> assert false)
module Typ = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = typ
      let name = "Typ"
      let reprs = [ TVoid [] ]
      let tid ty = !pTypeSig ty
      let compare ty1 ty2 = Pervasives.compare (tid ty1) (tid ty2)
      let hash ty1 = Hashtbl.hash (tid ty1)
      let equal ty1 ty2 = tid ty1 = tid ty2
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

module Exp =
  Make_with_collections
    (struct
      include Datatype.Undefined
      type t = exp
      let name = "Exp"
      let reprs =
	[ { eid = -1; enode = Const (CStr ""); eloc = Location.unknown } ]
      let compare e1 e2 = Datatype.Int.compare e1.eid e2.eid
      let hash e = Hashtbl.hash e.eid
      let equal e1 e2 = e1.eid = e2.eid
     end)

module Varinfo = struct
  let pretty_ref = ref (fun _ _ -> assert false)
  let internal_pretty_code_ref = ref (fun _ _ _ -> assert false)
  include Make_with_collections
    (struct
      type t = varinfo
      let name = "Varinfo"
      let reprs =
	List.map
	  (fun loc ->
	    { vname = "";
	      vorig_name = "";
	      vtype = TVoid [];
	      vattr = [];
	      vstorage = NoStorage;
	      vglob = false;
	      vdefined = false;
	      vformal = false;
	      vinline = false;
	      vdecl = loc;
	      vid = -1;
	      vaddrof = false;
	      vreferenced = false;
	      vgenerated = false;
	      vdescr = None;
	      vdescrpure = false;
	      vghost = false;
	      vlogic = false;
	      vlogic_var_assoc = None })
	  Location.reprs
      let compare v1 v2 = Datatype.Int.compare v1.vid v2.vid
      let hash v = v.vid
      let equal v1 v2 = v1.vid = v2.vid
      let copy = Datatype.undefined
      let internal_pretty_code p fmt v = !internal_pretty_code_ref p fmt v
      let pretty fmt v = !pretty_ref fmt v
      let varname v = "vi_" ^ v.vorig_name
     end)
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

module Lval = struct
  let pretty_ref = ref (fun _ -> assert false)
  include Make_with_collections
    (struct
      type t = lval
       let name = "Lval"
       let reprs = List.map (fun v -> Var v, NoOffset) Varinfo.reprs
       let compare = compare_lval
       let equal = Datatype.from_compare
       let copy = Datatype.undefined
       let hash = Hashtbl.hash (* could be optimized *)
       let internal_pretty_code = Datatype.undefined
       let pretty fmt x = !pretty_ref fmt x
       let varname _ = "lv"
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

module Global = struct

  include Make
    (struct
      type t = global
      let name = "Global"
      let reprs = [ GText "" ]
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
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

module Global_annotation = struct

  include Make
    (struct
      type t = global_annotation
      let name = "Global_annotation"
      let reprs = List.map (fun l -> Daxiomatic ("", [], l)) Location.reprs
      let internal_pretty_code = Datatype.undefined
      let pretty = Datatype.undefined
      let varname = Datatype.undefined
     end)

  let loc = function
    | Dfun_or_pred(_, loc)
    | Daxiomatic(_, _, loc)
    | Dtype (_, loc)
    | Dlemma(_, _, _, _, _, loc)
    | Dinvariant(_, loc)
    | Dtype_annot(_, loc) -> loc

end

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
	     ereferenced = false } ]
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

(* -------------------------------------------------------------------------- *)
(* --- Comparison Over Terms                                              --- *)
(* -------------------------------------------------------------------------- *)

let compare_constant c1 c2 = match c1, c2 with
  | CInt64(v1,k1,_), CInt64(v2,k2,_) ->
    compare_chain Int64.compare v1 v2 (Extlib.compare_basic k1) k2
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
      | Told t1 , Told t2
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
        let copy x = (* NB: Term.copy itself is undefined. *)
          { it_id = x.it_id;
            it_content = Term.copy x.it_content }
        let hash x = x.it_id
        let internal_pretty_code = Datatype.undefined
        let pretty = Datatype.undefined
        let varname _ = "id_term"
     end) 

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

module Annotation_status =
  Make
    (struct
      type t = annotation_status
      let name = "Annotation_status"
      let reprs = [ Unknown; Checked { emitter = ""; valid = False } ]
      let internal_pretty_code = Datatype.undefined
      let pretty fmt s =
        match s with
          | Unknown -> Format.fprintf fmt "No proof attempted"
          | Checked {emitter=s; valid=True} ->
              Format.fprintf fmt "Valid according to %s" s
          | Checked {emitter=s; valid=False} ->
              Format.fprintf fmt "NOT valid according to %s" s
          | Checked {emitter=s; valid=Maybe} ->
              Format.fprintf fmt
                "Unknown (%s could not decide the status for this property)" s
      let varname = Datatype.undefined
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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
