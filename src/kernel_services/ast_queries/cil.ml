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

(* Modified by TrustInSoft *)

(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)

open Logic_const
open Format
open Cil_datatype
open Cil_types

(* ************************************************************************* *)
(* Reporting messages *)
(* ************************************************************************* *)

(* Set this to true to check that your code correctly calls some of the
   functions below. *)
let check_invariants = false

(* A reference to the current location *)
module CurrentLoc = Cil_const.CurrentLoc
let () = Log.set_current_source (fun () -> fst (CurrentLoc.get ()))

let pp_thisloc fmt = Location.pretty fmt (CurrentLoc.get ())

let set_dependencies_of_ast, dependency_on_ast  =
  let list_self = ref [] in
  (fun ast -> State_dependency_graph.add_dependencies ~from:ast !list_self),
  (fun state -> list_self := state :: !list_self)

let voidType = Cil_const.voidType
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let longLongType = TInt(ILongLong,[])
let ulongType = TInt(IULong,[])
let ulongLongType = TInt(IULongLong, [])
let charType = TInt(IChar, [])
let ucharType = TInt(IUChar, [])
let scharType = TInt(ISChar, [])

let charPtrType = TPtr(charType,[])
let ucharPtrType = TPtr(ucharType,[])
let scharPtrType = TPtr(scharType,[])
let charConstPtrType = TPtr(TInt(IChar, [Attr("const", [])]),[])

let voidPtrType = TPtr(voidType, [])
let voidConstPtrType = TPtr(TVoid [Attr ("const", [])], [])

let intPtrType = TPtr(intType, [])
let uintPtrType = TPtr(uintType, [])

let doubleType = TFloat(FDouble, [])
let floatType = TFloat(FFloat, [])
let longDoubleType = TFloat (FLongDouble, [])

let empty_size_cache () = {scache=Not_Computed}

type theMachine =
    { mutable useLogicalOperators: bool;
      mutable theMachine: mach;
      (** Cil.initCil will set this to the current machine description. *)
      mutable lowerConstants: bool; (** Do lower constants (default true) *)
      mutable insertImplicitCasts: bool; (** Do insert implicit casts
					     (default true) *)
      mutable underscore_name: bool;
      mutable stringLiteralType: typ;
      mutable upointKind: ikind;
      mutable upointType: typ;
      mutable wcharKind: ikind; (** An integer type that fits wchar_t. *)
      mutable wcharType: typ;
      mutable ptrdiffKind: ikind; (** An integer type that fits ptrdiff_t. *)
      mutable ptrdiffType: typ;
      mutable typeOfSizeOf: typ; (** An integer type that is the type of
				      sizeof. *)
      mutable kindOfSizeOf: ikind;
    }

type lineDirectiveStyle =
  | LineComment                (** Before every element, print the line
                                * number in comments. This is ignored by
                                * processing tools (thus errors are reproted
                                * in the CIL output), but useful for
                                * visual inspection *)
  | LineCommentSparse          (** Like LineComment but only print a line
                                * directive for a new source line *)
  | LinePreprocessorInput      (** Use #line directives *)
  | LinePreprocessorOutput     (** Use # nnn directives (in gcc mode) *)

type miscState =
    { mutable lineDirectiveStyle: lineDirectiveStyle option;
      mutable print_CIL_Input: bool;
      mutable printCilAsIs: bool;
      mutable lineLength: int;
      mutable warnTruncate: bool }

let default_machdep = Machdeps.x86_32

let createMachine () = (* Contain dummy values *)
  { useLogicalOperators = false;
    theMachine = default_machdep;
    lowerConstants = false(*true*);
    insertImplicitCasts = true;
    underscore_name = true;
    stringLiteralType = charPtrType;
    upointKind = IChar;
    upointType = voidType;
    wcharKind = IChar;
    wcharType = voidType;
    ptrdiffKind = IChar;
    ptrdiffType = voidType;
    typeOfSizeOf = voidType;
    kindOfSizeOf = IUInt;
}

let copyMachine src dst =
  dst.useLogicalOperators <- src.useLogicalOperators;
  dst.theMachine <- src.theMachine;
  dst.lowerConstants <- src.lowerConstants;
  dst.insertImplicitCasts <- src.insertImplicitCasts;
  dst.underscore_name <- src.underscore_name;
  dst.stringLiteralType <- src.stringLiteralType;
  dst.upointKind <- src.upointKind;
  dst.upointType <- src.upointType;
  dst.wcharKind <- src.wcharKind;
  dst.wcharType <- src.wcharType;
  dst.ptrdiffKind <- src.ptrdiffKind;
  dst.ptrdiffType <- src.ptrdiffType;
  dst.typeOfSizeOf <- src.typeOfSizeOf;
  dst.kindOfSizeOf <- src.kindOfSizeOf

(* A few globals that control the interpretation of C source *)
let theMachine = createMachine ()

let msvcMode () = (theMachine.theMachine.compiler = "msvc")
let gccMode () = (theMachine.theMachine.compiler = "gcc")

let theMachineProject = ref (createMachine ())

module Machine_datatype =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = theMachine
      let name = "theMachine"
      let reprs = [ theMachine ]
      let copy x =
	let m = createMachine () in
	copyMachine x m;
	m
      let mem_project = Datatype.never_any_project
     end)

module TheMachine =
  State_builder.Register
    (Machine_datatype)
    (struct
       type t = theMachine
       let create = createMachine
       let get () = !theMachineProject
       let set m =
	 theMachineProject := m;
	 copyMachine !theMachineProject theMachine
       let clear m = copyMachine (createMachine ()) m
       let clear_some_projects _ _ = false
     end)
    (struct
       let name = "theMachine"
       let unique_name = name
       let dependencies = [ Kernel.Machdep.self ]
     end)

let selfMachine = TheMachine.self

let () =
  State_dependency_graph.add_dependencies
    ~from:selfMachine
    Logic_env.builtin_states

let selfMachine_is_computed = TheMachine.is_computed

let miscState =
  { lineDirectiveStyle = Some LinePreprocessorInput;
    print_CIL_Input = false;
    printCilAsIs = false;
    lineLength = 80;
    warnTruncate = true }

(* sm: return the string 's' if we're printing output for gcc, suppres
 * it if we're printing for CIL to parse back in.  the purpose is to
 * hide things from gcc that it complains about, but still be able
 * to do lossless transformations when CIL is the consumer *)
let forgcc (s: string) : string = if miscState.print_CIL_Input then "" else s


let debugConstFold = false

(* TODO: migrate that to Cil_const as well *)
module Sid = State_builder.SharedCounter(struct let name = "sid" end)

module Eid = State_builder.SharedCounter(struct let name = "eid" end)

let new_exp ~loc e = { eloc = loc; eid = Eid.next (); enode = e }

let dummy_exp e = { eid = -1; enode = e; eloc = Cil_datatype.Location.unknown }

(** The Abstract Syntax of CIL *)

(** To be able to add/remove features easily, each feature should be packaged
   * as an interface with the following interface. These features should be *)
type featureDescr = {
    fd_enabled: bool ref;
    (** The enable flag. Set to default value  *)

    fd_name: string;
    (** This is used to construct an option "--doxxx" and "--dontxxx" that
     * enable and disable the feature  *)

    fd_description: string;
    (* A longer name that can be used to document the new options  *)

    fd_extraopt: (string * Arg.spec * string) list;
    (** Additional command line options.  The description strings should
        usually start with a space for Arg.align to print the --help nicely. *)

    fd_doit: (file -> unit);
    (** This performs the transformation *)

    fd_post_check: bool;
    (* Whether to perform a CIL consistency checking after this stage, if
     * checking is enabled (--check is passed to cilly) *)
}

(* A reference to the current global being visited *)
let currentGlobal: global ref = ref (GText "dummy")

let argsToList : (string * typ * attributes) list option
                  -> (string * typ * attributes) list
    = function
    None -> []
  | Some al -> al


(* A hack to allow forward reference of d_exp *)
let pp_typ_ref = Extlib.mk_fun "Cil.pp_typ_ref"
let pp_global_ref = Extlib.mk_fun "Cil.pp_global_ref"
let pp_exp_ref = Extlib.mk_fun "Cil.pp_exp_ref"
let pp_lval_ref = Extlib.mk_fun "Cil.pp_lval_ref"
let pp_ikind_ref = Extlib.mk_fun "Cil.pp_ikind_ref"
let pp_attribute_ref = Extlib.mk_fun "Cil.pp_attribute_ref"
let pp_attributes_ref = Extlib.mk_fun "Cil.pp_attributes_ref"

let default_behavior_name = "default!"
let is_default_mk_behavior ~name ~assumes = name = default_behavior_name && assumes =[]
let is_default_behavior b = is_default_mk_behavior b.b_name b.b_assumes

let find_default_behavior spec =
  try
    Some (List.find is_default_behavior spec.spec_behavior)
  with Not_found -> None

let find_default_requires behaviors =
  try (List.find is_default_behavior behaviors).b_requires
  with Not_found -> []

let rec stripInfo e =
  match e.enode with 
  | Info(e',_) -> stripInfo e' 
  | _ -> e

let rec addOffset (toadd: offset) (off: offset) : offset =
  match off with
  | NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

let mkBlock (slst: stmt list) : block =
  { battrs = []; bstmts = slst; blocals = []}

let mkStmt ?(ghost=false) ?(valid_sid=false) (sk: stmtkind) : stmt =
  { skind = sk;
    labels = [];
     (* It is better to create statements with a valid sid, so that they can
	safely be used in tables. I only do it when performing Jessie
	analysis, as other plugins rely on specific sid values for their tests
	(e.g. slicing). *)
    sid = if valid_sid then Sid.next () else -1;
    succs = []; preds = [];
    ghost = ghost}

 let stmt_of_instr_list ?(loc=Location.unknown) = function
   | [] -> Instr (Skip loc)
   | [i] -> Instr i
   | il ->
       let b = mkBlock (List.map (fun i -> mkStmt (Instr i)) il) in
       match b.bstmts with
       | [] -> Instr (Skip loc)
       | [s] when b.battrs = [] -> s.skind
       | _ -> Block b

 (**** Utility functions ******)

 (**** ATTRIBUTES ****)

 let bitfield_attribute_name = "FRAMA_C_BITFIELD_SIZE"

 (** Construct sorted lists of attributes ***)
 let attributeName = function Attr(a, _) | AttrAnnot a -> a

 let addAttribute
     (Attr(an, _) | AttrAnnot an as a: attribute) (al: attributes) =
   let rec insertSorted = function
       [] -> [a]
     | ((Attr(an0, _) | AttrAnnot an0 as a0) :: rest) as l ->
	 if an < an0 then a :: l
	 else if Cil_datatype.Attribute.equal a a0 then l (* Do not add if already in there *)
	 else a0 :: insertSorted rest (* Make sure we see all attributes with
				       * this name *)
   in
   insertSorted al

 (** The second attribute list is sorted *)
 let addAttributes al0 (al: attributes) : attributes =
     if al0 == [] then al else
     List.fold_left (fun acc a -> addAttribute a acc) al al0

 let dropAttribute (an: string) (al: attributes) =
   List.filter (fun a -> attributeName a <> an) al

 let hasAttribute (s: string) (al: attribute list) : bool =
   List.exists (fun a -> attributeName a = s) al

 let rec dropAttributes (anl: string list) (al: attributes) =
   match al with
   | [] -> []
   | a :: q ->
     let q' = dropAttributes anl q in
     if List.mem (attributeName a) anl then
       q' (* drop this attribute *)
     else
       if q' == q then al (* preserve sharing *) else a :: q'

 let filterAttributes (s: string) (al: attribute list) : attribute list =
   List.filter (fun a -> attributeName a = s) al

 let findAttribute (s: string) (al: attribute list) : attrparam list =
   List.fold_left
     (fun acc -> function
      | Attr (an, param) when an = s -> param @ acc
      | _ -> acc)
     [] al

 let rec typeAttrs = function
     TVoid a -> a
   | TInt (_, a) -> a
   | TFloat (_, a) -> a
   | TNamed (t, a) -> addAttributes a (typeAttrs t.ttype)
   | TPtr (_, a) -> a
   | TArray (_, _, _,a) -> a
   | TComp (comp, _, a) -> addAttributes comp.cattr a
   | TEnum (enum, a) -> addAttributes enum.eattr a
   | TFun (_, _, _, a) -> a
   | TBuiltin_va_list a -> a

 let typeAttr = function
   | TVoid a
   | TInt (_, a)
   | TFloat (_, a)
   | TNamed (_, a)
   | TPtr (_, a)
   | TArray (_, _, _, a)
   | TComp (_, _, a)
   | TEnum (_, a)
   | TFun (_, _, _, a)
   | TBuiltin_va_list a -> a

 let setTypeAttrs t a =
   match t with
     TVoid _ -> TVoid a
   | TInt (i, _) -> TInt (i, a)
   | TFloat (f, _) -> TFloat (f, a)
   | TNamed (t, _) -> TNamed(t, a)
   | TPtr (t', _) -> TPtr(t', a)
   | TArray (t', l, s, _) -> TArray(t', l, s, a)
   | TComp (comp, s, _) -> TComp (comp, s, a)
   | TEnum (enum, _) -> TEnum (enum, a)
   | TFun (r, args, v, _) -> TFun(r,args,v,a)
   | TBuiltin_va_list _ -> TBuiltin_va_list a

 let qualifier_attributes = [ "const"; "restrict"; "volatile"]

 let filter_qualifier_attributes al =
   List.filter
     (fun a -> List.mem (attributeName a) qualifier_attributes) al

 let splitArrayAttributes =
   List.partition
     (fun a -> List.mem (attributeName a) qualifier_attributes)

 let rec typeAddAttributes a0 t =
 begin
   match a0 with
   | [] ->
       (* no attributes, keep same type *)
       t
   | _ ->
       (* anything else: add a0 to existing attributes *)
       let add (a: attributes) = addAttributes a0 a in
       match t with
	 TVoid a -> TVoid (add a)
       | TInt (ik, a) -> TInt (ik, add a)
       | TFloat (fk, a) -> TFloat (fk, add a)
       | TEnum (enum, a) -> TEnum (enum, add a)
       | TPtr (t, a) -> TPtr (t, add a)
       | TArray (t, l, s, a) ->
           let att_elt, att_typ = splitArrayAttributes a0 in
           TArray (arrayPushAttributes att_elt t, l, s,
                   addAttributes att_typ a)
       | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
       | TComp (comp, s, a) -> TComp (comp, s, add a)
       | TNamed (t, a) -> TNamed (t, add a)
       | TBuiltin_va_list a -> TBuiltin_va_list (add a)
 end
 (* Push attributes that belong to the type of the elements of the array as
    far as possible *)
 and arrayPushAttributes al = function
   | TArray (bt, l, s, a) ->
       TArray (arrayPushAttributes al bt, l, s, a)
   | t -> typeAddAttributes al t

 let rec typeRemoveAttributes ?anl t =
   (* Try to preserve sharing. We use sharing to be more efficient, but also
      to detect that we have removed an attribute under typedefs *)
   let new_attr al =
     match anl with None -> [] | Some anl -> dropAttributes anl al
   in
   let reshare al f =
     let al' = new_attr al in if al' == al then t else f al'
   in
   match t with
   | TVoid a -> reshare a (fun a -> TVoid a)
   | TInt (ik, a) -> reshare a (fun a -> TInt (ik, a))
   | TFloat (fk, a) -> reshare a (fun a -> TFloat (fk, a))
   | TEnum (enum, a) -> reshare a (fun a -> TEnum (enum, a))
   | TPtr (t, a) -> reshare a (fun a -> TPtr (t, a))
   | TArray (t, l, s, a) -> reshare a (fun a -> TArray (t, l, s, a))
   | TFun (t, args, isva, a) -> reshare a (fun a -> TFun(t, args, isva, a))
   | TComp (comp, s, a) -> reshare a (fun a -> TComp (comp, s, a))
   | TBuiltin_va_list a -> reshare a (fun a -> TBuiltin_va_list a)
   | TNamed (tn, a) ->
     let tn' = typeRemoveAttributes ?anl tn.ttype in
     if tn' == tn.ttype then reshare a (fun a -> TNamed (tn, a))
     else typeAddAttributes (new_attr a) tn'

 let typeRemoveAllAttributes t = typeRemoveAttributes t

 let typeRemoveAttributes anl t = typeRemoveAttributes ~anl t

 let rec typeRemoveAttributesDeep (anl: string list) t =
   (* Try to preserve sharing. We use sharing to be more efficient, but also
      to detect that we have removed an attribute under typedefs *)
   let reshare al f =
     let al' = dropAttributes anl al in
     if al' == al then t else f al'
   in
   match t with
   | TVoid a -> reshare a (fun a -> TVoid a)
   | TInt (ik, a) -> reshare a (fun a -> TInt (ik, a))
   | TFloat (fk, a) -> reshare a (fun a -> TFloat (fk, a))
   | TEnum (enum, a) -> reshare a (fun a -> TEnum (enum, a))
   | TPtr (t, a) ->
     let t' = typeRemoveAttributesDeep anl t in
     if t != t' then TPtr(t', dropAttributes anl a)
     else reshare a (fun a -> TPtr(t,a))
   | TArray (t, l, s, a) ->
     let t' = typeRemoveAttributesDeep anl t in
     if t!=t' then TArray(t', l, s, dropAttributes anl a)
     else reshare a (fun a -> TArray (t, l, s, a))
   | TFun (t, args, isva, a) -> reshare a (fun a -> TFun(t, args, isva, a))
   | TComp (comp, s, a) -> reshare a (fun a -> TComp (comp, s, a))
   | TBuiltin_va_list a -> reshare a (fun a -> TBuiltin_va_list a)
   | TNamed (tn, a) ->
     let tn' = typeRemoveAttributesDeep anl tn.ttype in
     if tn' == tn.ttype then
       reshare a (fun a -> TNamed (tn, a))
     else
       typeAddAttributes (dropAttributes anl a) tn'

(* JS: build an attribute annotation from [s]. *)
let mkAttrAnnot s = "/*@ " ^ s ^ " */"

  
let type_remove_qualifier_attributes = 
  typeRemoveAttributes qualifier_attributes

let type_remove_qualifier_attributes_deep =
  typeRemoveAttributesDeep qualifier_attributes
    
type attributeClass =
   | AttrName of bool
	 (* Attribute of a name. If argument is true and we are on MSVC then
	  * the attribute is printed using __declspec as part of the storage
	  * specifier  *)
   | AttrFunType of bool
	 (* Attribute of a function type. If argument is true and we are on
	  * MSVC then the attribute is printed just before the function name *)

   | AttrType  (* Attribute of a type *)

 (* This table contains the mapping of predefined attributes to classes.
  * Extend this table with more attributes as you need. This table is used to
  * determine how to associate attributes with names or type during cabs2cil
  * conversion *)
 let attributeHash: (string, attributeClass) Hashtbl.t =
   let table = Hashtbl.create 13 in
   List.iter (fun a -> Hashtbl.add table a (AttrName false))
     [ "section"; "constructor"; "destructor"; "unused"; "used"; "weak";
       "no_instrument_function"; "alias"; "no_check_memory_usage";
       "exception"; "model"; (* "restrict"; *)
       "aconst"; "__asm__" (* Gcc uses this to specifiy the name to be used in
			    * assembly for a global  *)];
   (* Now come the MSVC declspec attributes *)
   List.iter (fun a -> Hashtbl.add table a (AttrName true))
     [ "thread"; "naked"; "dllimport"; "dllexport";
       "selectany"; "allocate"; "nothrow"; "novtable"; "property";  "noreturn";
       "uuid"; "align" ];
   List.iter (fun a -> Hashtbl.add table a (AttrFunType false))
     [ "format"; "regparm"; "longcall"; "noinline"; "always_inline"; ];
   List.iter (fun a -> Hashtbl.add table a (AttrFunType true))
     [ "stdcall";"cdecl"; "fastcall" ];
   List.iter (fun a -> Hashtbl.add table a AttrType)
     [ "const"; "volatile"; "restrict"; "mode" ];
   table

 let attributeClass = Hashtbl.find attributeHash

 let registerAttribute = Hashtbl.add attributeHash
 let removeAttribute = Hashtbl.remove attributeHash

 (** Partition the attributes into classes *)
 let partitionAttributes
     ~(default:attributeClass)
     (attrs:  attribute list) :
     attribute list * attribute list * attribute list =
   let rec loop (n,f,t) = function
       [] -> n, f, t
     | (Attr(an, _) | AttrAnnot an as a) :: rest ->
	 match (try Hashtbl.find attributeHash an with Not_found -> default) with
	   AttrName _ -> loop (addAttribute a n, f, t) rest
	 | AttrFunType _ ->
	     loop (n, addAttribute a f, t) rest
	 | AttrType -> loop (n, f, addAttribute a t) rest
   in
   loop ([], [], []) attrs


 let unrollType (t: typ) : typ =
   let rec withAttrs (al: attributes) (t: typ) : typ =
     match t with
       TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
     | x -> typeAddAttributes al x
   in
   withAttrs [] t

 let () = punrollType := unrollType

 (* Unroll typedefs, discarding all intermediate attribute. To be used only
    when one is interested in the shape of the type *)
 let rec unrollTypeSkel = function
   | TNamed (r, _) -> unrollTypeSkel r.ttype
   | x -> x


 let isFunctionType t =
   match unrollTypeSkel t with
     TFun _ -> true
   | _ -> false

 (* Make a varinfo. Used mostly as a helper function below  *)
 let makeVarinfo ?(source=true) ?(temp=false) global formal name typ =
   (* Strip const from type for locals *)
   let vi =
     { vorig_name = name;
       vname = name;
       vid   = -1;
       vglob = global;
       vdefined = false;
       vformal = formal;
       vtemp = temp;
       vtype = if formal || global then typ
       else typeRemoveAttributes ["const"] typ;
       vdecl = Location.unknown;
       vinline = false;
       vattr = [];
       vstorage = NoStorage;
       vaddrof = false;
       vreferenced = false;
       vdescr = None;
       vdescrpure = true;
       vghost = false;
       vsource = source;
       vlogic_var_assoc = None
     }
   in
   Cil_const.set_vid vi;
   vi

 module FormalsDecl =
   State_builder.Hashtbl
     (Varinfo.Hashtbl)
     (Datatype.List(Varinfo))
     (struct
	let name = "FormalsDecl"
	let dependencies = [] (* depends on Ast.self; see below *)
	let size = 47
      end)

 let selfFormalsDecl = FormalsDecl.self
 let () = dependency_on_ast selfFormalsDecl

 let makeFormalsVarDecl (n,t,a) =
   let vi = makeVarinfo ~temp:false false true n t in
   vi.vattr <- a;
   vi

 let setFormalsDecl vi typ =
   match unrollType typ with
   | TFun(_, Some args, _, _) ->
     FormalsDecl.replace vi (List.map makeFormalsVarDecl args)
   | TFun(_,None,_,_) -> ()
   | _ -> 
     Kernel.error ~current:true
       "trying to assigns formal parameters to an object \
        that is not a function prototype"

 let getFormalsDecl vi = FormalsDecl.find vi

 let unsafeSetFormalsDecl vi args = FormalsDecl.replace vi args

 let removeFormalsDecl vi = FormalsDecl.remove vi

 let iterFormalsDecl = FormalsDecl.iter

 let () = Cil_datatype.Kf.set_formal_decls := unsafeSetFormalsDecl

(* Set the formals and re-create the function name based on the information*)
 let setFormals (f: fundec) (forms: varinfo list) =
   unsafeSetFormalsDecl f.svar forms;
   List.iter (fun v -> v.vformal <- true) forms;
   f.sformals <- forms; (* Set the formals *)
   assert (getFormalsDecl f.svar == f.sformals);
   match unrollType f.svar.vtype with
     TFun(rt, _, isva, fa) ->
       f.svar.vtype <-
	  TFun(rt,
	       Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) forms),
	       isva, fa)
   | _ -> 
     Kernel.fatal "Set formals. %s does not have function type" f.svar.vname

 let empty_funspec () =
   { spec_behavior = [];
     spec_variant = None;
     spec_terminates = None;
     spec_complete_behaviors = [];
     spec_disjoint_behaviors = [] }

 let no_behavior l =
   match l with
     | [] -> true
     | [ b ] ->
       b.b_name = default_behavior_name &&
       b.b_requires = [] && 
       b.b_post_cond = [] &&
       b.b_assigns = WritesAny &&
       b.b_allocation = FreeAllocAny &&
       b.b_extended = []
     | _ -> false
 
 let is_empty_funspec (spec : funspec) =
   (no_behavior spec.spec_behavior) &&
   spec.spec_variant = None && spec.spec_terminates = None &&
   spec.spec_complete_behaviors = [] && spec.spec_disjoint_behaviors = []

let is_empty_behavior b =
  b.b_assumes = [] && b.b_requires = [] && b.b_post_cond = [] &&
  b.b_assigns = WritesAny && b.b_allocation = FreeAllocAny && b.b_extended = []

 (** Get the full name of a comp *)
 let compFullName comp =
   (if comp.cstruct then "struct " else "union ") ^ comp.cname


 let missingFieldName = "" (* "___missing_field_name"*)

(* The next compindo identifier to use. Counts up. *)
 let nextCompinfoKey =
   let module M =
         State_builder.SharedCounter(struct let name = "compinfokey" end)
   in
   M.next

 (** Creates a (potentially recursive) composite type. Make sure you add a
   * GTag for it to the file! **)
 let mkCompInfo
       (isstruct: bool)
       (n: string)
       ?(norig=n)
       (* fspec is a function that when given a forward
	* representation of the structure type constructs the type of
	* the fields. The function can ignore this argument if not
	* constructing a recursive type.  *)
	(mkfspec: compinfo -> (string * typ * int option * attribute list *
			      location) list)
	(a: attribute list) : compinfo =

   (* make a new name for anonymous structs *)
   if n = "" then Kernel.fatal "mkCompInfo: missing structure name\n" ;
   (* Make a new self cell and a forward reference *)
   let comp =
     { cstruct = isstruct;
       corig_name = norig;
       cname = n;
       ckey = nextCompinfoKey ();
       cfields = []; (* fields will be added afterwards. *)
       cattr = a;
       creferenced = false;
       (* Make this compinfo undefined by default *)
       cdefined = false; }
   in
   let flds =
     List.map (fun (fn, ft, fb, fa, fl) ->
		 { fcomp = comp;
		   ftype = ft;
		   forig_name = fn;
		   fname = fn;
		   fbitfield = fb;
		   fattr = fa;
		   floc = fl;
		   faddrof = false;
		   fsize_in_bits = None;
		   foffset_in_bits = None;
		   fpadding_in_bits = None;
		 }) (mkfspec comp) in
   comp.cfields <- flds;
   if flds <> [] then comp.cdefined <- true;
   comp

 (** Make a copy of a compinfo, changing the name and the key *)
 let copyCompInfo ?(fresh=true) ci cname =
   let ckey = if fresh then nextCompinfoKey () else ci.ckey in
   let ci' = { ci with cname; ckey } in
   (* Copy the fields and set the new pointers to parents *)
   ci'.cfields <- List.map (fun f -> {f with fcomp = ci'}) ci'.cfields;
   ci'

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc.
    @see Plugin Development Guide *)
type 'a visitAction =
    SkipChildren                        (** Do not visit the children. Return
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this
                                            node. Rebuild the node on return
                                            if any of the children changes
                                            (use == test) *)
  | DoChildrenPost of ('a -> 'a)
  | JustCopy
  | JustCopyPost of ('a -> 'a)
  | ChangeTo of 'a                      (** Replace the expression with the
                                            given one *)
  | ChangeToPost of 'a * ('a -> 'a)

  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire
                                           exp is replaced by the first
                                           parameter. Then continue with
                                           the children. On return rebuild
                                           the node if any of the children
                                           has changed and then apply the
                                           function on the node *)

type visitor_behavior =
    { (* copy mutable structure which are not shared across the AST*)
      cfile: file -> file;
      cinitinfo: initinfo -> initinfo;
      cblock: block -> block;
      cfunspec: funspec -> funspec;
      cfunbehavior: funbehavior -> funbehavior;
      cidentified_term: identified_term -> identified_term;
      cidentified_predicate: identified_predicate -> identified_predicate;
      cexpr: exp -> exp;
      ccode_annotation: code_annotation -> code_annotation;
      (* get the copy of a shared value *)
      get_stmt: stmt -> stmt;
      get_compinfo: compinfo -> compinfo;
      get_fieldinfo: fieldinfo -> fieldinfo;
      get_model_info: model_info -> model_info;
      get_enuminfo: enuminfo -> enuminfo;
      get_enumitem: enumitem -> enumitem;
      get_typeinfo: typeinfo -> typeinfo;
      get_varinfo: varinfo -> varinfo;
      get_logic_info: logic_info -> logic_info;
      get_logic_type_info: logic_type_info -> logic_type_info;
      get_logic_var: logic_var -> logic_var;
      get_kernel_function: kernel_function -> kernel_function;
      get_fundec: fundec -> fundec;
      (* get the original value tied to a copy *)
      get_original_stmt: stmt -> stmt;
      get_original_compinfo: compinfo -> compinfo;
      get_original_fieldinfo: fieldinfo -> fieldinfo;
      get_original_model_info: model_info -> model_info;
      get_original_enuminfo: enuminfo -> enuminfo;
      get_original_enumitem: enumitem -> enumitem;
      get_original_typeinfo: typeinfo -> typeinfo;
      get_original_varinfo: varinfo -> varinfo;
      get_original_logic_info: logic_info -> logic_info;
      get_original_logic_type_info: logic_type_info -> logic_type_info;
      get_original_logic_var: logic_var -> logic_var;
      get_original_kernel_function: kernel_function -> kernel_function;
      get_original_fundec: fundec -> fundec;
      (* change a binding... use with care *)
      set_stmt: stmt -> stmt -> unit;
      set_compinfo: compinfo -> compinfo -> unit;
      set_fieldinfo: fieldinfo -> fieldinfo -> unit;
      set_model_info: model_info -> model_info -> unit;
      set_enuminfo: enuminfo -> enuminfo -> unit;
      set_enumitem: enumitem -> enumitem -> unit;
      set_typeinfo: typeinfo -> typeinfo -> unit;
      set_varinfo: varinfo -> varinfo -> unit;
      set_logic_info: logic_info -> logic_info -> unit;
      set_logic_type_info: logic_type_info -> logic_type_info -> unit;
      set_logic_var: logic_var -> logic_var -> unit;
      set_kernel_function: kernel_function -> kernel_function -> unit;
      set_fundec: fundec -> fundec -> unit;
      (* change a reference... use with care *)
      set_orig_stmt: stmt -> stmt -> unit;
      set_orig_compinfo: compinfo -> compinfo -> unit;
      set_orig_fieldinfo: fieldinfo -> fieldinfo -> unit;
      set_orig_model_info: model_info -> model_info -> unit;
      set_orig_enuminfo: enuminfo -> enuminfo -> unit;
      set_orig_enumitem: enumitem -> enumitem -> unit;
      set_orig_typeinfo: typeinfo -> typeinfo -> unit;
      set_orig_varinfo: varinfo -> varinfo -> unit;
      set_orig_logic_info: logic_info -> logic_info -> unit;
      set_orig_logic_type_info: logic_type_info -> logic_type_info -> unit;
      set_orig_logic_var: logic_var -> logic_var -> unit;
      set_orig_kernel_function: kernel_function -> kernel_function -> unit;
      set_orig_fundec: fundec -> fundec -> unit;
      (* copy fields that can referenced in other places of the AST*)
      memo_stmt: stmt -> stmt;
      memo_varinfo: varinfo -> varinfo;
      memo_compinfo: compinfo -> compinfo;
      memo_model_info: model_info -> model_info;
      memo_enuminfo: enuminfo -> enuminfo;
      memo_enumitem: enumitem -> enumitem;
      memo_typeinfo: typeinfo -> typeinfo;
      memo_logic_info: logic_info -> logic_info;
      memo_logic_type_info: logic_type_info -> logic_type_info;
      memo_fieldinfo: fieldinfo -> fieldinfo;
      memo_logic_var: logic_var -> logic_var;
      memo_kernel_function: kernel_function -> kernel_function;
      memo_fundec: fundec -> fundec;
      (* is the behavior a copy behavior *)
      is_copy_behavior: bool;
      is_fresh_behavior: bool;
      project: Project.t option;
      (* reset memoizing tables *)
      reset_behavior_varinfo: unit -> unit;
      reset_behavior_compinfo: unit -> unit;
      reset_behavior_enuminfo: unit -> unit;
      reset_behavior_enumitem: unit -> unit;
      reset_behavior_typeinfo: unit -> unit;
      reset_behavior_logic_info: unit -> unit;
      reset_behavior_logic_type_info: unit -> unit;
      reset_behavior_fieldinfo: unit -> unit;
      reset_behavior_model_info: unit -> unit;
      reset_behavior_stmt: unit -> unit;
      reset_logic_var: unit -> unit;
      reset_behavior_kernel_function: unit -> unit;
      reset_behavior_fundec: unit -> unit;
      (* iterates over tables *)
      iter_visitor_varinfo: (varinfo -> varinfo -> unit) -> unit;
      iter_visitor_compinfo: (compinfo -> compinfo -> unit) -> unit;
      iter_visitor_enuminfo: (enuminfo -> enuminfo -> unit) -> unit;
      iter_visitor_enumitem: (enumitem -> enumitem -> unit) -> unit;
      iter_visitor_typeinfo: (typeinfo -> typeinfo -> unit) -> unit;
      iter_visitor_stmt: (stmt -> stmt -> unit) -> unit;
      iter_visitor_logic_info: (logic_info -> logic_info -> unit) -> unit;
      iter_visitor_logic_type_info:
        (logic_type_info -> logic_type_info -> unit) -> unit;
      iter_visitor_fieldinfo: (fieldinfo -> fieldinfo -> unit) -> unit;
      iter_visitor_model_info: (model_info -> model_info -> unit) -> unit;
      iter_visitor_logic_var: (logic_var -> logic_var -> unit) -> unit;
      iter_visitor_kernel_function: 
        (kernel_function -> kernel_function -> unit) -> unit;
      iter_visitor_fundec: (fundec -> fundec -> unit) -> unit;
      (* folds over tables *)
      fold_visitor_varinfo: 'a.(varinfo -> varinfo -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_compinfo: 'a.(compinfo -> compinfo -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_enuminfo: 'a.(enuminfo -> enuminfo -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_enumitem: 'a.(enumitem -> enumitem -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_typeinfo: 'a.(typeinfo -> typeinfo -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_stmt: 'a.(stmt -> stmt -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_logic_info: 
        'a. (logic_info -> logic_info -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_logic_type_info: 
        'a.(logic_type_info -> logic_type_info -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_fieldinfo: 
        'a.(fieldinfo -> fieldinfo -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_model_info:
        'a. (model_info -> model_info -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_logic_var:
        'a.(logic_var -> logic_var -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_kernel_function:
        'a.(kernel_function -> kernel_function -> 'a -> 'a) -> 'a -> 'a;
      fold_visitor_fundec:
        'a.(fundec -> fundec -> 'a -> 'a) -> 'a -> 'a;
    }

let is_copy_behavior b = b.is_copy_behavior

let is_fresh_behavior b = b.is_fresh_behavior

let memo_varinfo b = b.memo_varinfo
let memo_compinfo b = b.memo_compinfo
let memo_fieldinfo b = b.memo_fieldinfo
let memo_model_info b = b.memo_model_info
let memo_enuminfo b = b.memo_enuminfo
let memo_enumitem b = b.memo_enumitem
let memo_stmt b = b.memo_stmt
let memo_typeinfo b = b.memo_typeinfo
let memo_logic_info b = b.memo_logic_info
let memo_logic_type_info b = b.memo_logic_type_info
let memo_logic_var b = b.memo_logic_var
let memo_kernel_function b = b.memo_kernel_function
let memo_fundec b = b.memo_fundec

let reset_behavior_varinfo b = b.reset_behavior_varinfo ()
let reset_behavior_compinfo b = b.reset_behavior_compinfo ()
let reset_behavior_enuminfo b = b.reset_behavior_enuminfo ()
let reset_behavior_enumitem b = b.reset_behavior_enumitem ()
let reset_behavior_typeinfo b = b.reset_behavior_typeinfo ()
let reset_behavior_logic_info b = b.reset_behavior_logic_info ()
let reset_behavior_logic_type_info b = b.reset_behavior_logic_type_info ()
let reset_behavior_fieldinfo b = b.reset_behavior_fieldinfo ()
let reset_behavior_model_info b = b.reset_behavior_model_info ()
let reset_behavior_stmt b = b.reset_behavior_stmt ()
let reset_logic_var b = b.reset_logic_var ()
let reset_behavior_kernel_function b = b.reset_behavior_kernel_function ()
let reset_behavior_fundec b = b.reset_behavior_fundec ()

let get_varinfo b = b.get_varinfo
let get_compinfo b = b.get_compinfo
let get_fieldinfo b = b.get_fieldinfo
let get_model_info b = b.get_model_info
let get_enuminfo b = b.get_enuminfo
let get_enumitem b = b.get_enumitem
let get_stmt b = b.get_stmt
let get_typeinfo b = b.get_typeinfo
let get_logic_info b = b.get_logic_info
let get_logic_type_info b = b.get_logic_type_info
let get_logic_var b = b.get_logic_var
let get_kernel_function b = b.get_kernel_function
let get_fundec b = b.get_fundec

let get_original_varinfo b = b.get_original_varinfo
let get_original_compinfo b = b.get_original_compinfo
let get_original_fieldinfo b = b.get_original_fieldinfo
let get_original_model_info b = b.get_original_model_info
let get_original_enuminfo b = b.get_original_enuminfo
let get_original_enumitem b = b.get_original_enumitem
let get_original_stmt b = b.get_original_stmt
let get_original_typeinfo b = b.get_original_typeinfo
let get_original_logic_info b = b.get_original_logic_info
let get_original_logic_type_info b = b.get_original_logic_type_info
let get_original_logic_var b = b.get_original_logic_var
let get_original_kernel_function b = b.get_original_kernel_function
let get_original_fundec b = b.get_original_fundec

let set_varinfo b = b.set_varinfo
let set_compinfo b = b.set_compinfo
let set_fieldinfo b = b.set_fieldinfo
let set_model_info b = b.set_model_info
let set_enuminfo b = b.set_enuminfo
let set_enumitem b = b.set_enumitem
let set_stmt b = b.set_stmt
let set_typeinfo b = b.set_typeinfo
let set_logic_info b = b.set_logic_info
let set_logic_type_info b = b.set_logic_type_info
let set_logic_var b = b.set_logic_var
let set_kernel_function b = b.set_kernel_function
let set_fundec b = b.set_fundec

let set_orig_varinfo b = b.set_orig_varinfo
let set_orig_compinfo b = b.set_orig_compinfo
let set_orig_fieldinfo b = b.set_orig_fieldinfo
let set_orig_model_info b = b.set_model_info
let set_orig_enuminfo b = b.set_orig_enuminfo
let set_orig_enumitem b = b.set_orig_enumitem
let set_orig_stmt b = b.set_orig_stmt
let set_orig_typeinfo b = b.set_orig_typeinfo
let set_orig_logic_info b = b.set_orig_logic_info
let set_orig_logic_type_info b = b.set_orig_logic_type_info
let set_orig_logic_var b = b.set_orig_logic_var
let set_orig_kernel_function b= b.set_orig_kernel_function
let set_orig_fundec b = b.set_orig_fundec

let iter_visitor_varinfo b = b.iter_visitor_varinfo
let iter_visitor_compinfo b = b.iter_visitor_compinfo
let iter_visitor_enuminfo b = b.iter_visitor_enuminfo
let iter_visitor_enumitem b = b.iter_visitor_enumitem
let iter_visitor_typeinfo b = b.iter_visitor_typeinfo
let iter_visitor_stmt b = b.iter_visitor_stmt
let iter_visitor_logic_info b= b.iter_visitor_logic_info
let iter_visitor_logic_type_info b = b .iter_visitor_logic_type_info
let iter_visitor_fieldinfo b = b.iter_visitor_fieldinfo
let iter_visitor_model_info b = b.iter_visitor_model_info
let iter_visitor_logic_var b = b.iter_visitor_logic_var
let iter_visitor_kernel_function b = b.iter_visitor_kernel_function
let iter_visitor_fundec b = b.iter_visitor_fundec

let fold_visitor_varinfo b = b.fold_visitor_varinfo
let fold_visitor_compinfo b = b.fold_visitor_compinfo
let fold_visitor_enuminfo b = b.fold_visitor_enuminfo
let fold_visitor_enumitem b = b.fold_visitor_enumitem
let fold_visitor_typeinfo b = b.fold_visitor_typeinfo
let fold_visitor_stmt b = b.fold_visitor_stmt
let fold_visitor_logic_info b = b.fold_visitor_logic_info
let fold_visitor_logic_type_info b = b.fold_visitor_logic_type_info
let fold_visitor_fieldinfo b = b.fold_visitor_fieldinfo
let fold_visitor_model_info b = b.fold_visitor_model_info
let fold_visitor_logic_var b = b.fold_visitor_logic_var
let fold_visitor_kernel_function b = b.fold_visitor_kernel_function
let fold_visitor_fundec b = b.fold_visitor_fundec

let id = Extlib.id
let alphabetaunit _ _ = ()
let alphabetabeta _ x = x
let alphabetafalse _ _ = false
let unitunit: unit -> unit = id
let alphatrue _ = true
let alphaunit _ = ()

let inplace_visit () =
  { cfile = id;
    get_compinfo = id;
    get_fieldinfo = id;
    get_model_info = id;
    get_enuminfo = id;
    get_enumitem = id;
    get_typeinfo = id;
    get_varinfo = id;
    get_logic_var = id;
    get_stmt = id;
    get_logic_info = id;
    get_logic_type_info = id;
    get_kernel_function = id;
    get_fundec = id;
    get_original_compinfo = id;
    get_original_fieldinfo = id;
    get_original_model_info = id;
    get_original_enuminfo = id;
    get_original_enumitem = id;
    get_original_typeinfo = id;
    get_original_varinfo = id;
    get_original_logic_var = id;
    get_original_stmt = id;
    get_original_logic_info = id;
    get_original_logic_type_info = id;
    get_original_kernel_function = id;
    get_original_fundec = id;
    cinitinfo = id;
    cblock = id;
    cfunspec = id;
    cfunbehavior = id;
    cidentified_term = id;
    cidentified_predicate = id;
    ccode_annotation = id;
    cexpr = id;
    is_copy_behavior = false;
    is_fresh_behavior = false;
    project = None;
    memo_varinfo = id;
    memo_compinfo = id;
    memo_enuminfo = id;
    memo_enumitem = id;
    memo_typeinfo = id;
    memo_logic_info = id;
    memo_logic_type_info = id;
    memo_stmt = id;
    memo_fieldinfo = id;
    memo_model_info = id;
    memo_logic_var = id;
    memo_kernel_function = id;
    memo_fundec = id;
    set_varinfo = alphabetaunit;
    set_compinfo = alphabetaunit;
    set_enuminfo = alphabetaunit;
    set_enumitem = alphabetaunit;
    set_typeinfo = alphabetaunit;
    set_logic_info = alphabetaunit;
    set_logic_type_info = alphabetaunit;
    set_stmt = alphabetaunit;
    set_fieldinfo = alphabetaunit;
    set_model_info = alphabetaunit;
    set_logic_var = alphabetaunit;
    set_kernel_function = alphabetaunit;
    set_fundec = alphabetaunit;
    set_orig_varinfo = alphabetaunit;
    set_orig_compinfo = alphabetaunit;
    set_orig_enuminfo = alphabetaunit;
    set_orig_enumitem = alphabetaunit;
    set_orig_typeinfo = alphabetaunit;
    set_orig_logic_info = alphabetaunit;
    set_orig_logic_type_info = alphabetaunit;
    set_orig_stmt = alphabetaunit;
    set_orig_fieldinfo = alphabetaunit;
    set_orig_model_info = alphabetaunit;
    set_orig_logic_var = alphabetaunit;
    set_orig_kernel_function = alphabetaunit;
    set_orig_fundec = alphabetaunit;
    reset_behavior_varinfo = unitunit;
    reset_behavior_compinfo = unitunit;
    reset_behavior_enuminfo = unitunit;
    reset_behavior_enumitem = unitunit;
    reset_behavior_typeinfo = unitunit;
    reset_behavior_logic_info = unitunit;
    reset_behavior_logic_type_info = unitunit;
    reset_behavior_fieldinfo = unitunit;
    reset_behavior_model_info = unitunit;
    reset_behavior_stmt = unitunit;
    reset_logic_var = unitunit;
    reset_behavior_kernel_function = unitunit;
    reset_behavior_fundec = unitunit;
    iter_visitor_varinfo = alphaunit;
    iter_visitor_compinfo = alphaunit;
    iter_visitor_enuminfo = alphaunit;
    iter_visitor_enumitem = alphaunit;
    iter_visitor_typeinfo = alphaunit;
    iter_visitor_stmt = alphaunit;
    iter_visitor_logic_info = alphaunit;
    iter_visitor_logic_type_info = alphaunit;
    iter_visitor_fieldinfo = alphaunit;
    iter_visitor_model_info = alphaunit;
    iter_visitor_logic_var = alphaunit;
    iter_visitor_kernel_function = alphaunit;
    iter_visitor_fundec = alphaunit;
    fold_visitor_varinfo = alphabetabeta;
    fold_visitor_compinfo = alphabetabeta;
    fold_visitor_enuminfo = alphabetabeta;
    fold_visitor_enumitem = alphabetabeta;
    fold_visitor_typeinfo = alphabetabeta;
    fold_visitor_stmt = alphabetabeta;
    fold_visitor_logic_info = alphabetabeta;
    fold_visitor_logic_type_info = alphabetabeta;
    fold_visitor_fieldinfo = alphabetabeta;
    fold_visitor_model_info = alphabetabeta;
    fold_visitor_logic_var = alphabetabeta;
    fold_visitor_kernel_function = alphabetabeta;
    fold_visitor_fundec = alphabetabeta;
  }

let copy_visit_gen fresh prj =
  let varinfos = Cil_datatype.Varinfo.Hashtbl.create 103 in
  let compinfos = Cil_datatype.Compinfo.Hashtbl.create 17 in
  let enuminfos = Cil_datatype.Enuminfo.Hashtbl.create 17 in
  let enumitems = Cil_datatype.Enumitem.Hashtbl.create 17 in
  let typeinfos = Cil_datatype.Typeinfo.Hashtbl.create 17 in
  let logic_infos = Cil_datatype.Logic_info.Hashtbl.create 17 in
  let logic_type_infos = Cil_datatype.Logic_type_info.Hashtbl.create 17 in
  let fieldinfos = Cil_datatype.Fieldinfo.Hashtbl.create 17 in
  let model_infos = Cil_datatype.Model_info.Hashtbl.create 17 in
  let stmts = Cil_datatype.Stmt.Hashtbl.create 103 in
  let logic_vars = Cil_datatype.Logic_var.Hashtbl.create 17 in
  let kernel_functions = Cil_datatype.Kf.Hashtbl.create 17 in
  let fundecs = Cil_datatype.Varinfo.Hashtbl.create 17 in
  let orig_varinfos = Cil_datatype.Varinfo.Hashtbl.create 103 in
  let orig_compinfos = Cil_datatype.Compinfo.Hashtbl.create 17 in
  let orig_enuminfos = Cil_datatype.Enuminfo.Hashtbl.create 17 in
  let orig_enumitems = Cil_datatype.Enumitem.Hashtbl.create 17 in
  let orig_typeinfos = Cil_datatype.Typeinfo.Hashtbl.create 17 in
  let orig_logic_infos = Cil_datatype.Logic_info.Hashtbl.create 17 in
  let orig_logic_type_infos = Cil_datatype.Logic_type_info.Hashtbl.create 17 in
  let orig_fieldinfos = Cil_datatype.Fieldinfo.Hashtbl.create 17 in
  let orig_model_infos = Cil_datatype.Model_info.Hashtbl.create 17 in
  let orig_stmts = Cil_datatype.Stmt.Hashtbl.create 103 in
  let orig_logic_vars = Cil_datatype.Logic_var.Hashtbl.create 17 in
  let orig_kernel_functions = Cil_datatype.Kf.Hashtbl.create 17 in
  let orig_fundecs = Cil_datatype.Varinfo.Hashtbl.create 17 in
  let temp_set_logic_var x new_x =
      Cil_datatype.Logic_var.Hashtbl.add logic_vars x new_x
  in
  let temp_set_orig_logic_var new_x x =
      Cil_datatype.Logic_var.Hashtbl.add orig_logic_vars new_x x
  in
  let temp_memo_logic_var x =
(*    Format.printf "search for %s#%d@." x.lv_name x.lv_id;*)
    let res =
    try Cil_datatype.Logic_var.Hashtbl.find logic_vars x
    with Not_found ->
(*      Format.printf "Not found@.";*)
      let id = if fresh then Cil_const.new_raw_id () else x.lv_id in
      let new_x = { x with lv_id = id } in
      temp_set_logic_var x new_x; temp_set_orig_logic_var new_x x; new_x
    in
(*    Format.printf "res is %s#%d@." res.lv_name res.lv_id;*)
    res
  in
  let temp_set_varinfo x new_x =
    Cil_datatype.Varinfo.Hashtbl.add varinfos x new_x;
    match x.vlogic_var_assoc, new_x.vlogic_var_assoc with
          | None, _ | _, None -> ()
          | Some lx, Some new_lx ->
            Cil_datatype.Logic_var.Hashtbl.add logic_vars lx new_lx
  in
  let temp_set_orig_varinfo new_x x =
    Cil_datatype.Varinfo.Hashtbl.add orig_varinfos new_x x;
    match new_x.vlogic_var_assoc, x.vlogic_var_assoc with
      | None, _ | _, None -> ()
      | Some new_lx, Some lx ->
        Cil_datatype.Logic_var.Hashtbl.add orig_logic_vars new_lx lx
  in
  let temp_memo_varinfo x =
    try Cil_datatype.Varinfo.Hashtbl.find varinfos x
    with Not_found ->
      let new_x =
        if fresh then Cil_const.copy_with_new_vid x else begin
          let new_x = { x with vid = x.vid } in
          (match x.vlogic_var_assoc with
            | None -> ()
            | Some lv ->
              let new_lv = { lv with lv_origin = Some new_x } in
              new_x.vlogic_var_assoc <- Some new_lv);
          new_x
        end
      in
      temp_set_varinfo x new_x; temp_set_orig_varinfo new_x x; new_x
  in
  let temp_set_fundec f new_f =
    Cil_datatype.Varinfo.Hashtbl.add fundecs f.svar new_f
  in
  let temp_set_orig_fundec new_f f =
    Cil_datatype.Varinfo.Hashtbl.add orig_fundecs new_f.svar f
  in
  let temp_memo_fundec f =
    try Cil_datatype.Varinfo.Hashtbl.find fundecs f.svar
    with Not_found ->
      let v = temp_memo_varinfo f.svar in
      let new_f = { f with svar = v } in
      temp_set_fundec f new_f; temp_set_orig_fundec new_f f; new_f
  in
  let temp_set_kernel_function kf new_kf =
    Cil_datatype.Kf.Hashtbl.replace kernel_functions kf new_kf;
    match kf.fundec, new_kf.fundec with
      | Declaration(_,vi,_,_), Declaration(_,new_vi,_,_)
      | Declaration(_,vi,_,_), Definition({ svar = new_vi }, _)
      | Definition({svar = vi},_), Declaration(_,new_vi,_,_) ->
        temp_set_varinfo vi new_vi
      | Definition (fundec,_), Definition(new_fundec,_) ->
        temp_set_fundec fundec new_fundec
  in
  let temp_set_orig_kernel_function new_kf kf =
    Cil_datatype.Kf.Hashtbl.replace orig_kernel_functions new_kf kf;
    match new_kf.fundec, kf.fundec with
      | Declaration(_,new_vi,_,_), Declaration(_,vi,_,_)
      | Declaration(_,new_vi,_,_), Definition({ svar = vi }, _)
      | Definition({svar = new_vi},_), Declaration(_,vi,_,_) ->
        temp_set_orig_varinfo new_vi vi
      | Definition (new_fundec,_), Definition(fundec,_) ->
        temp_set_orig_fundec new_fundec fundec
  in
  let temp_memo_kernel_function kf =
    try Cil_datatype.Kf.Hashtbl.find kernel_functions kf
    with Not_found ->
      let new_kf =
        match kf.fundec with
          | Declaration (spec,vi,prms,loc) ->
            let new_vi = temp_memo_varinfo vi in
            { kf with fundec = Declaration(spec,new_vi,prms,loc) }
          | Definition(f,loc) ->
            let new_f = temp_memo_fundec f in
            { kf with fundec = Definition(new_f,loc) }
      in
      temp_set_kernel_function kf new_kf;
      temp_set_orig_kernel_function new_kf kf;
      new_kf
  in
  let temp_set_compinfo c new_c =
    Cil_datatype.Compinfo.Hashtbl.add compinfos c new_c;
    List.iter2
      (fun f new_f -> Cil_datatype.Fieldinfo.Hashtbl.add fieldinfos f new_f)
      c.cfields new_c.cfields
  in
  let temp_set_orig_compinfo new_c c =
    Cil_datatype.Compinfo.Hashtbl.add orig_compinfos new_c c;
      List.iter2
        (fun new_f f ->
          Cil_datatype.Fieldinfo.Hashtbl.add orig_fieldinfos new_f f)
        new_c.cfields c.cfields
  in
  let temp_memo_compinfo c =
    try Cil_datatype.Compinfo.Hashtbl.find compinfos c
    with Not_found ->
      let new_c =
        copyCompInfo ~fresh c c.cname
      in
      temp_set_compinfo c new_c; temp_set_orig_compinfo new_c c; new_c
  in
  { cfile = (fun x -> { x with fileName = x.fileName });
    get_compinfo =
      (fun x -> 
        try Cil_datatype.Compinfo.Hashtbl.find compinfos x with Not_found -> x);
    get_fieldinfo =
      (fun x -> 
        try Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x
        with Not_found -> x);
    get_model_info =
      (fun x ->
        try Cil_datatype.Model_info.Hashtbl.find model_infos x
        with Not_found -> x);
    get_enuminfo =
      (fun x -> 
        try Cil_datatype.Enuminfo.Hashtbl.find enuminfos x with Not_found -> x);
    get_enumitem =
      (fun x ->
        try Cil_datatype.Enumitem.Hashtbl.find enumitems x with Not_found -> x);
    get_typeinfo =
      (fun x ->
        try Cil_datatype.Typeinfo.Hashtbl.find typeinfos x with Not_found -> x);
    get_varinfo =
      (fun x ->
        try Cil_datatype.Varinfo.Hashtbl.find varinfos x with Not_found -> x);
    get_stmt =
      (fun x -> try Cil_datatype.Stmt.Hashtbl.find stmts x with Not_found -> x);
    get_logic_info =
      (fun x ->
        try Cil_datatype.Logic_info.Hashtbl.find logic_infos x
        with Not_found -> x);
    get_logic_type_info =
      (fun x ->
        try Cil_datatype.Logic_type_info.Hashtbl.find logic_type_infos x 
        with Not_found -> x);
    get_logic_var =
      (fun x ->
        try Cil_datatype.Logic_var.Hashtbl.find logic_vars x
        with Not_found -> x);
    get_kernel_function =
      (fun x ->
        try Cil_datatype.Kf.Hashtbl.find kernel_functions x
        with Not_found -> x);
    get_fundec =
      (fun x ->
        try Cil_datatype.Varinfo.Hashtbl.find fundecs x.svar
        with Not_found -> x);
    get_original_compinfo =
      (fun x ->
        try Cil_datatype.Compinfo.Hashtbl.find orig_compinfos x 
        with Not_found -> x);
    get_original_fieldinfo =
      (fun x ->
        try Cil_datatype.Fieldinfo.Hashtbl.find orig_fieldinfos x
        with Not_found -> x);
    get_original_model_info =
      (fun x ->
        try Cil_datatype.Model_info.Hashtbl.find orig_model_infos x
        with Not_found -> x);
    get_original_enuminfo =
      (fun x ->
        try Cil_datatype.Enuminfo.Hashtbl.find orig_enuminfos x
        with Not_found -> x);
    get_original_enumitem =
      (fun x ->
        try Cil_datatype.Enumitem.Hashtbl.find orig_enumitems x
        with Not_found -> x);
    get_original_typeinfo =
      (fun x ->
        try Cil_datatype.Typeinfo.Hashtbl.find orig_typeinfos x
        with Not_found -> x);
    get_original_varinfo =
      (fun x ->
        try Cil_datatype.Varinfo.Hashtbl.find orig_varinfos x
        with Not_found -> x);
    get_original_stmt =
      (fun x ->
        try Cil_datatype.Stmt.Hashtbl.find orig_stmts x with Not_found -> x);
    get_original_logic_var =
      (fun x ->
        try Cil_datatype.Logic_var.Hashtbl.find orig_logic_vars x
        with Not_found -> x);
    get_original_logic_info =
      (fun x -> 
        try Cil_datatype.Logic_info.Hashtbl.find orig_logic_infos x
       with Not_found -> x);
    get_original_logic_type_info =
      (fun x ->
        try Cil_datatype.Logic_type_info.Hashtbl.find orig_logic_type_infos x
        with Not_found -> x);
    get_original_kernel_function =
      (fun x ->
        try Cil_datatype.Kf.Hashtbl.find orig_kernel_functions x
        with Not_found -> x);
    get_original_fundec =
      (fun x ->
        try Cil_datatype.Varinfo.Hashtbl.find orig_fundecs x.svar
        with Not_found -> x);
    cinitinfo = (fun x -> { init = x.init });
    cblock = (fun x -> { x with battrs = x.battrs });
    cfunspec = (fun x -> { x with spec_behavior = x.spec_behavior});
    cfunbehavior = (fun x -> { x with b_name = x.b_name});
    ccode_annotation =
      if fresh then Logic_const.refresh_code_annotation
      else (fun x -> { x with annot_id = x.annot_id });
    cidentified_predicate =
      if fresh then Logic_const.refresh_predicate
      else (fun x -> { x with ip_id = x.ip_id });
    cidentified_term =
      if fresh then Logic_const.refresh_identified_term
      else (fun x -> { x with it_id = x.it_id});
    cexpr =
      (fun x ->
        let id = if fresh then Eid.next () else x.eid in { x with eid = id });
    is_copy_behavior = true;
    is_fresh_behavior = fresh;
    project = Some prj;
    reset_behavior_varinfo =
      (fun () ->
        Cil_datatype.Varinfo.Hashtbl.clear varinfos;
        Cil_datatype.Varinfo.Hashtbl.clear orig_varinfos);
    reset_behavior_compinfo =
      (fun () ->
        Cil_datatype.Compinfo.Hashtbl.clear compinfos;
        Cil_datatype.Compinfo.Hashtbl.clear orig_compinfos);
    reset_behavior_enuminfo =
      (fun () ->
        Cil_datatype.Enuminfo.Hashtbl.clear enuminfos;
        Cil_datatype.Enuminfo.Hashtbl.clear orig_enuminfos);
    reset_behavior_enumitem =
      (fun () ->
        Cil_datatype.Enumitem.Hashtbl.clear enumitems;
        Cil_datatype.Enumitem.Hashtbl.clear orig_enumitems);
    reset_behavior_typeinfo =
      (fun () ->
        Cil_datatype.Typeinfo.Hashtbl.clear typeinfos;
        Cil_datatype.Typeinfo.Hashtbl.clear orig_typeinfos);
    reset_behavior_logic_info =
      (fun () ->
        Cil_datatype.Logic_info.Hashtbl.clear logic_infos;
        Cil_datatype.Logic_info.Hashtbl.clear orig_logic_infos);
    reset_behavior_logic_type_info =
      (fun () ->
        Cil_datatype.Logic_type_info.Hashtbl.clear logic_type_infos;
        Cil_datatype.Logic_type_info.Hashtbl.clear orig_logic_type_infos);
    reset_behavior_fieldinfo =
      (fun () ->
        Cil_datatype.Fieldinfo.Hashtbl.clear fieldinfos;
        Cil_datatype.Fieldinfo.Hashtbl.clear orig_fieldinfos);
    reset_behavior_model_info =
      (fun () ->
        Cil_datatype.Model_info.Hashtbl.clear model_infos;
        Cil_datatype.Model_info.Hashtbl.clear orig_model_infos);
    reset_behavior_stmt =
      (fun () ->
        Cil_datatype.Stmt.Hashtbl.clear stmts;
        Cil_datatype.Stmt.Hashtbl.clear orig_stmts);
    reset_logic_var =
      (fun () ->
        Cil_datatype.Logic_var.Hashtbl.clear logic_vars;
        Cil_datatype.Logic_var.Hashtbl.clear orig_logic_vars);
    reset_behavior_kernel_function =
      (fun () ->
        Cil_datatype.Kf.Hashtbl.clear kernel_functions;
        Cil_datatype.Kf.Hashtbl.clear orig_kernel_functions);
    reset_behavior_fundec =
      (fun () ->
        Cil_datatype.Varinfo.Hashtbl.clear fundecs;
        Cil_datatype.Varinfo.Hashtbl.clear orig_fundecs);
    memo_varinfo = temp_memo_varinfo;
    memo_compinfo = temp_memo_compinfo;
    memo_enuminfo =
      (fun x ->
         try Cil_datatype.Enuminfo.Hashtbl.find enuminfos x
         with Not_found ->
           let new_x = { x with ename = x.ename } in
           Cil_datatype.Enuminfo.Hashtbl.add enuminfos x new_x;
           Cil_datatype.Enuminfo.Hashtbl.add orig_enuminfos new_x x;
           new_x);
    memo_enumitem =
      (fun x ->
         try Cil_datatype.Enumitem.Hashtbl.find enumitems x
         with Not_found ->
           let new_x = { x with einame = x.einame } in
           Cil_datatype.Enumitem.Hashtbl.add enumitems x new_x;
           Cil_datatype.Enumitem.Hashtbl.add orig_enumitems new_x x;
           new_x);
    memo_typeinfo =
      (fun x ->
         try Cil_datatype.Typeinfo.Hashtbl.find typeinfos x
         with Not_found ->
           let new_x = { x with tname = x.tname } in
           Cil_datatype.Typeinfo.Hashtbl.add typeinfos x new_x;
           Cil_datatype.Typeinfo.Hashtbl.add orig_typeinfos new_x x;
           new_x);
    memo_logic_info =
      (fun x ->
         try Cil_datatype.Logic_info.Hashtbl.find logic_infos x
         with Not_found ->
	   let new_v = temp_memo_logic_var x.l_var_info in
           let new_x = { x with l_var_info = new_v } in
           Cil_datatype.Logic_info.Hashtbl.add logic_infos x new_x;
           Cil_datatype.Logic_info.Hashtbl.add orig_logic_infos new_x x;
           new_x);
    memo_logic_type_info =
      (fun x ->
         try Cil_datatype.Logic_type_info.Hashtbl.find logic_type_infos x
         with Not_found ->
           let new_x = { x with lt_name = x.lt_name } in
           Cil_datatype.Logic_type_info.Hashtbl.add logic_type_infos x new_x;
           Cil_datatype.Logic_type_info.Hashtbl.add
             orig_logic_type_infos new_x x;
           new_x);
    memo_stmt =
      (fun x ->
         try Cil_datatype.Stmt.Hashtbl.find stmts x
         with Not_found ->
           let sid = if fresh then Sid.next () else x.sid in
           let new_x = { x with sid = sid } in
           Cil_datatype.Stmt.Hashtbl.add stmts x new_x;
           Cil_datatype.Stmt.Hashtbl.add orig_stmts new_x x;
           new_x);
    memo_fieldinfo =
      (fun x ->
         try Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x
         with Not_found ->
           let _ = temp_memo_compinfo x.fcomp in
           (* memo_compinfo fills the field correspondance table as well *)
           let new_x = Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x in
           Cil_datatype.Fieldinfo.Hashtbl.add fieldinfos x new_x;
           Cil_datatype.Fieldinfo.Hashtbl.add orig_fieldinfos new_x x;
           new_x);
    memo_model_info =
      (fun x ->
        try Cil_datatype.Model_info.Hashtbl.find model_infos x
        with Not_found ->
          let new_x = { x with mi_name = x.mi_name } in
          Cil_datatype.Model_info.Hashtbl.add model_infos x new_x;
          Cil_datatype.Model_info.Hashtbl.add orig_model_infos new_x x;
          new_x
      );
    memo_logic_var = temp_memo_logic_var;
    memo_kernel_function = temp_memo_kernel_function;
    memo_fundec = temp_memo_fundec;
    set_varinfo = temp_set_varinfo;
    set_compinfo = temp_set_compinfo;
    set_enuminfo = Cil_datatype.Enuminfo.Hashtbl.replace enuminfos;
    set_enumitem = Cil_datatype.Enumitem.Hashtbl.replace enumitems;
    set_typeinfo = Cil_datatype.Typeinfo.Hashtbl.replace typeinfos;
    set_logic_info = Cil_datatype.Logic_info.Hashtbl.replace logic_infos;
    set_logic_type_info = 
      Cil_datatype.Logic_type_info.Hashtbl.replace logic_type_infos;
    set_stmt = Cil_datatype.Stmt.Hashtbl.replace stmts;
    set_fieldinfo = Cil_datatype.Fieldinfo.Hashtbl.replace fieldinfos;
    set_model_info = Cil_datatype.Model_info.Hashtbl.replace model_infos;
    set_logic_var = temp_set_logic_var;
    set_kernel_function = temp_set_kernel_function;
    set_fundec = temp_set_fundec;
    set_orig_varinfo = temp_set_orig_varinfo;
    set_orig_compinfo = temp_set_orig_compinfo;
    set_orig_enuminfo = Cil_datatype.Enuminfo.Hashtbl.replace orig_enuminfos;
    set_orig_enumitem = Cil_datatype.Enumitem.Hashtbl.replace orig_enumitems;
    set_orig_typeinfo = Cil_datatype.Typeinfo.Hashtbl.replace orig_typeinfos;
    set_orig_logic_info =
      Cil_datatype.Logic_info.Hashtbl.replace orig_logic_infos;
    set_orig_logic_type_info =
      Cil_datatype.Logic_type_info.Hashtbl.replace orig_logic_type_infos;
    set_orig_stmt = Cil_datatype.Stmt.Hashtbl.replace orig_stmts;
    set_orig_fieldinfo =
      Cil_datatype.Fieldinfo.Hashtbl.replace orig_fieldinfos;
    set_orig_model_info =
      Cil_datatype.Model_info.Hashtbl.replace orig_model_infos;
    set_orig_logic_var = temp_set_orig_logic_var;
    set_orig_kernel_function = temp_set_orig_kernel_function;
    set_orig_fundec = temp_set_orig_fundec;
    iter_visitor_varinfo = 
      (fun f -> Cil_datatype.Varinfo.Hashtbl.iter f varinfos);
    iter_visitor_compinfo =
      (fun f -> Cil_datatype.Compinfo.Hashtbl.iter f compinfos);
    iter_visitor_enuminfo = 
      (fun f -> Cil_datatype.Enuminfo.Hashtbl.iter f enuminfos);
    iter_visitor_enumitem = 
      (fun f -> Cil_datatype.Enumitem.Hashtbl.iter f enumitems);
    iter_visitor_typeinfo =
      (fun f -> Cil_datatype.Typeinfo.Hashtbl.iter f typeinfos);
    iter_visitor_stmt = 
      (fun f -> Cil_datatype.Stmt.Hashtbl.iter f stmts);
    iter_visitor_logic_info = 
      (fun f -> Cil_datatype.Logic_info.Hashtbl.iter f logic_infos);
    iter_visitor_logic_type_info = 
      (fun f -> Cil_datatype.Logic_type_info.Hashtbl.iter f logic_type_infos);
    iter_visitor_fieldinfo = 
      (fun f -> Cil_datatype.Fieldinfo.Hashtbl.iter f fieldinfos);
    iter_visitor_model_info =
      (fun f -> Cil_datatype.Model_info.Hashtbl.iter f model_infos);
    iter_visitor_logic_var =
      (fun f -> Cil_datatype.Logic_var.Hashtbl.iter f logic_vars);
    iter_visitor_kernel_function =
      (fun f -> Cil_datatype.Kf.Hashtbl.iter f kernel_functions);
    iter_visitor_fundec =
      (fun f -> 
        let f _ new_fundec =
          let old_fundec = 
            Cil_datatype.Varinfo.Hashtbl.find orig_fundecs new_fundec.svar
          in
          f old_fundec new_fundec
        in
        Cil_datatype.Varinfo.Hashtbl.iter f fundecs);
    fold_visitor_varinfo =
      (fun f i -> Cil_datatype.Varinfo.Hashtbl.fold f varinfos i);
    fold_visitor_compinfo = 
      (fun f i -> Cil_datatype.Compinfo.Hashtbl.fold f compinfos i);
    fold_visitor_enuminfo =
      (fun f i -> Cil_datatype.Enuminfo.Hashtbl.fold f enuminfos i);
    fold_visitor_enumitem =
      (fun f i -> Cil_datatype.Enumitem.Hashtbl.fold f enumitems i);
    fold_visitor_typeinfo =
      (fun f i -> Cil_datatype.Typeinfo.Hashtbl.fold f typeinfos i);
    fold_visitor_stmt =
      (fun f i -> Cil_datatype.Stmt.Hashtbl.fold f stmts i);
    fold_visitor_logic_info =
      (fun f i -> Cil_datatype.Logic_info.Hashtbl.fold f logic_infos i);
    fold_visitor_logic_type_info =
      (fun f i ->
        Cil_datatype.Logic_type_info.Hashtbl.fold f logic_type_infos i);
    fold_visitor_fieldinfo =
      (fun f i -> Cil_datatype.Fieldinfo.Hashtbl.fold f fieldinfos i);
    fold_visitor_model_info =
      (fun f i -> Cil_datatype.Model_info.Hashtbl.fold f model_infos i);
    fold_visitor_logic_var =
      (fun f i -> Cil_datatype.Logic_var.Hashtbl.fold f logic_vars i);
    fold_visitor_kernel_function =
      (fun f i ->
        Cil_datatype.Kf.Hashtbl.fold f kernel_functions i);
    fold_visitor_fundec =
      (fun f i -> 
        let f _ new_fundec acc =
          let old_fundec =
            Cil_datatype.Varinfo.Hashtbl.find orig_fundecs new_fundec.svar
          in
          f old_fundec new_fundec acc
        in
        Cil_datatype.Varinfo.Hashtbl.fold f fundecs i);
  }

let copy_visit = copy_visit_gen false

let refresh_visit = copy_visit_gen true

let visitor_tbl = Hashtbl.create 5

let register_behavior_extension name ext = Hashtbl.add visitor_tbl name ext

(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use
 * one of Change... actions if you want to copy the node *)

(** A visitor interface for traversing CIL trees. Create instantiations of
 * this type by specializing the class {!Cil.nopCilVisitor}. *)
class type cilVisitor = object

  method behavior: visitor_behavior

  method project: Project.t option

  method plain_copy_visitor: cilVisitor

  method vfile: file -> file visitAction
    (** visit a file. *)

  method vvdec: varinfo -> varinfo visitAction
    (** Invoked for each variable declaration. The subtrees to be traversed
     * are those corresponding to the type and attributes of the variable.
     * Note that variable declarations are all the [GVar], [GVarDecl], [GFun],
     * all the [varinfo] in formals of function types, and the formals and
     * locals for function definitions. This means that the list of formals
     * in a function definition will be traversed twice, once as part of the
     * function type and second as part of the formals in a function
     * definition. *)

  method vvrbl: varinfo -> varinfo visitAction
    (** Invoked on each variable use. Here only the [SkipChildren] and
     * [ChangeTo] actions make sense since there are no subtrees. Note that
     * the type and attributes of the variable are not traversed for a
     * variable use *)

  method vexpr: exp -> exp visitAction
    (** Invoked on each expression occurence. The subtrees are the
     * subexpressions, the types (for a [Cast] or [SizeOf] expression) or the
     * variable use. *)

  method vlval: lval -> lval visitAction
    (** Invoked on each lvalue occurence *)

  method voffs: offset -> offset visitAction
    (** Invoked on each offset occurrence that is *not* as part
      * of an initializer list specification, i.e. in an lval or
      * recursively inside an offset. *)

  method vinitoffs: offset -> offset visitAction
    (** Invoked on each offset appearing in the list of a
      * CompoundInit initializer.  *)

  method vinst: instr -> instr list visitAction
    (** Invoked on each instruction occurrence. The [ChangeTo] action can
     * replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction
    (** Control-flow statement. *)

  method vblock: block -> block visitAction     (** Block. Replaced in
                                                    place. *)
  method vfunc: fundec -> fundec visitAction    (** Function definition.
                                                    Replaced in place. *)
  method vglob: global -> global list visitAction (** Global (vars, types,
                                                      etc.)  *)
  method vinit: varinfo -> offset -> init -> init visitAction
                                                (** Initializers for globals,
                                                 * pass the global where this
                                                 * occurs, and the offset *)
  method vtype: typ -> typ visitAction          (** Use of some type. Note
                                                 * that for structure/union
                                                 * and enumeration types the
                                                 * definition of the
                                                 * composite type is not
                                                 * visited. Use [vglob] to
                                                 * visit it.  *)

  method vcompinfo: compinfo -> compinfo visitAction

  method venuminfo: enuminfo -> enuminfo visitAction

  method vfieldinfo: fieldinfo -> fieldinfo visitAction

  method venumitem: enumitem -> enumitem visitAction

  method vattr: attribute -> attribute list visitAction
    (** Attribute. Each attribute can be replaced by a list *)
  method vattrparam: attrparam -> attrparam visitAction
    (** Attribute parameters. *)

    (** Add here instructions while visiting to queue them to
     * preceede the current statement or instruction being processed *)
  method queueInstr: instr list -> unit

    (** Gets the queue of instructions and resets the queue *)
  method unqueueInstr: unit -> instr list

  val current_stmt : stmt Stack.t
  method push_stmt: stmt -> unit
  method  pop_stmt: stmt -> unit
  method current_stmt: stmt option
  method current_kinstr: kinstr

  method current_func: fundec option
  method set_current_func: fundec -> unit
  method reset_current_func: unit -> unit

  method vlogic_type: logic_type -> logic_type visitAction

  method vmodel_info: model_info -> model_info visitAction

  method videntified_term: identified_term -> identified_term visitAction

  method vterm: term -> term visitAction

  method vterm_node: term_node -> term_node visitAction

  method vterm_lval: term_lval -> term_lval visitAction

  method vterm_lhost: term_lhost -> term_lhost visitAction

  method vterm_offset: term_offset -> term_offset visitAction

  method vlogic_label: logic_label -> logic_label visitAction

  method vlogic_info_decl: logic_info -> logic_info visitAction

  method vlogic_info_use: logic_info -> logic_info visitAction

  method vlogic_type_info_decl: logic_type_info -> logic_type_info visitAction

  method vlogic_type_info_use: logic_type_info -> logic_type_info visitAction

  method vlogic_type_def: logic_type_def -> logic_type_def visitAction

  method vlogic_ctor_info_decl: logic_ctor_info -> logic_ctor_info visitAction

  method vlogic_ctor_info_use: logic_ctor_info -> logic_ctor_info visitAction

  method vlogic_var_use: logic_var -> logic_var visitAction

  method vlogic_var_decl: logic_var -> logic_var visitAction

  method vquantifiers: quantifiers -> quantifiers visitAction

  method videntified_predicate:
    identified_predicate -> identified_predicate visitAction

  method vpredicate: predicate -> predicate visitAction

  method vpredicate_named: predicate named -> predicate named visitAction

  method vbehavior: funbehavior -> funbehavior visitAction

  method vspec: funspec -> funspec visitAction

  method vassigns:
    identified_term assigns -> identified_term assigns visitAction

  method vfrees:
    identified_term list -> identified_term list visitAction
  method vallocates:
    identified_term list -> identified_term list visitAction
  method vallocation:
    identified_term allocation -> identified_term allocation visitAction

  method vloop_pragma: term loop_pragma -> term loop_pragma visitAction

  method vslice_pragma: term slice_pragma -> term slice_pragma visitAction
  method vimpact_pragma: term impact_pragma -> term impact_pragma visitAction

  method vdeps:
    identified_term deps -> identified_term deps visitAction

  method vfrom:
    identified_term from -> identified_term from visitAction

  method vcode_annot: code_annotation -> code_annotation visitAction

  method vannotation: global_annotation -> global_annotation visitAction
  method fill_global_tables: unit
  method get_filling_actions: (unit -> unit) Queue.t
end

 class internal_genericCilVisitor current_func behavior queue: cilVisitor =
 object(self)
   method behavior = behavior

   method project = behavior.project;

   method plain_copy_visitor =
     let obj =
       new internal_genericCilVisitor current_func behavior queue
     in
     assert (obj#get_filling_actions == self#get_filling_actions); obj

   method fill_global_tables =
    let action () = Queue.iter (fun f -> f()) queue in
    (match self#project with
    | None -> action ()
    | Some prj -> Project.on prj action ());
    Queue.clear queue

   method get_filling_actions = queue

   method vfile _f = DoChildren
   val current_stmt = Stack.create ()
   method push_stmt s = Stack.push s current_stmt
   method pop_stmt _s = ignore (Stack.pop current_stmt)
   method current_stmt =
     try Some (Stack.top current_stmt) with Stack.Empty -> None

   method current_kinstr =
     try Kstmt (Stack.top current_stmt) with Stack.Empty -> Kglobal

   method current_func = !current_func
   method set_current_func f = current_func := Some f
   method reset_current_func () = current_func := None

   method vvrbl (_v:varinfo) = DoChildren
   method vvdec (_v:varinfo) = DoChildren
   method vexpr (_e:exp) = DoChildren
   method vlval (_l:lval) = DoChildren
   method voffs (_o:offset) = DoChildren
   method vinitoffs (_o:offset) = DoChildren
   method vinst (_i:instr) = DoChildren
   method vstmt (_s:stmt) = DoChildren
   method vblock (_b: block) = DoChildren
   method vfunc (_f:fundec) = DoChildren
   method vglob (_g:global) = DoChildren
   method vinit (_forg: varinfo) (_off: offset) (_i:init) = DoChildren
   method vtype (_t:typ) = DoChildren
   method vcompinfo _ = DoChildren
   method venuminfo _ = DoChildren
   method vfieldinfo _ = DoChildren
   method venumitem _ = DoChildren
   method vattr (_a: attribute) = DoChildren
   method vattrparam (_a: attrparam) = DoChildren

   val mutable instrQueue = []

   method queueInstr (il: instr list) =
     List.iter (fun i -> instrQueue <- i :: instrQueue) il

   method unqueueInstr () =
     let res = List.rev instrQueue in
     instrQueue <- [];
     res

   method vmodel_info _ = DoChildren

   method vlogic_type _lt = DoChildren

   method videntified_term _t = DoChildren

   method vterm _t = DoChildren

   method vlogic_label _l = DoChildren

   method vterm_node _tn = DoChildren

   method vterm_lval _tl = DoChildren

   method vterm_lhost _tl = DoChildren

   method vterm_offset _vo = DoChildren

   method vlogic_info_decl _li = DoChildren

   method vlogic_info_use _li = DoChildren

   method vlogic_type_info_decl _ = DoChildren

   method vlogic_type_info_use _ = DoChildren

   method vlogic_type_def _ = DoChildren

   method vlogic_ctor_info_decl _ = DoChildren

   method vlogic_ctor_info_use _ = DoChildren

   method vlogic_var_decl _lv = DoChildren

   method vlogic_var_use _lv = DoChildren

   method vquantifiers _q = DoChildren

   method videntified_predicate _ip = DoChildren

   method vpredicate _p = DoChildren

   method vpredicate_named _p = DoChildren

   method vbehavior _b = DoChildren

   method vspec _s = DoChildren

   method vassigns _s = DoChildren
   method vfrees _s = DoChildren
   method vallocates _s = DoChildren
   method vallocation _s = DoChildren

   method vloop_pragma _ = DoChildren

   method vslice_pragma _ = DoChildren
   method vimpact_pragma _ = DoChildren

   method vdeps _ = DoChildren

   method vfrom _ = DoChildren

   method vcode_annot _ca = DoChildren

   method vannotation _a = DoChildren

 end

 class genericCilVisitor bhv =
   let current_func = ref None in
   let queue = Queue.create () in
   internal_genericCilVisitor current_func bhv queue

 class nopCilVisitor = object
   inherit genericCilVisitor (inplace_visit ())
 end

 let apply_on_project ?selection vis f arg =
   match vis#project with
     | None -> f arg
     | Some prj -> Project.on ?selection prj f arg

let assertEmptyQueue vis =
  if vis#unqueueInstr () <> [] then
    (* Either a visitor inserted an instruction somewhere that it shouldn't
       have (i.e. at the top level rather than inside of a statement), or
       there's a bug in the visitor engine. *)
    Kernel.fatal 
      "Visitor's instruction queue is not empty.@\n\
       You should only use queueInstr inside a function body!";
  ()

(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let doVisit (vis: 'visitor)
    only_copy_vis
    (previsit: 'a -> 'a)
            (startvisit: 'a -> 'a visitAction)
            (children: 'visitor -> 'a -> 'a)
            (node: 'a) : 'a =
  let node' = previsit node in
  let action = startvisit node' in
  match action with
      SkipChildren -> node'
    | ChangeTo node' -> node'
    | ChangeToPost (node',f) -> f node'
    | DoChildren | DoChildrenPost _ 
    | JustCopy | ChangeDoChildrenPost _ | JustCopyPost _ ->
      let nodepre = match action with
          ChangeDoChildrenPost (node', _) -> node'
        | _ -> node'
      in
      let vis = match action with
          JustCopy | JustCopyPost _ -> only_copy_vis
        | _ -> vis
      in
      let nodepost = children vis nodepre in
      match action with
        | DoChildrenPost f | ChangeDoChildrenPost (_, f) | JustCopyPost f ->
            f nodepost
        | _ -> nodepost

 let doVisitCil vis previsit startvisit children node =
   doVisit vis vis#plain_copy_visitor previsit startvisit children node

 let rev_until i l =
   let rec aux acc =
       function
	   [] -> acc
	 | i'::_ when i' == i -> acc
	 | i'::l -> aux (i'::acc) l
   in aux [] l

 (* mapNoCopy is like map but avoid copying the list if the function does not
  * change the elements. *)
 let mapNoCopy (f: 'a -> 'a) orig =
   let rec aux ((acc,has_changed) as res) l =
     match l with
	 [] -> if has_changed then List.rev acc else orig
       | i :: resti ->
	   let i' = f i in
	   if has_changed then
	     aux (i'::acc,true) resti
	   else if i' != i then
	     aux (i'::rev_until i orig,true) resti
	   else
	     aux res resti
   in aux ([],false) orig

 let mapNoCopyList (f: 'a -> 'a list) orig =
   let rec aux ((acc,has_changed) as res) l =
     match l with
	 [] -> if has_changed then List.rev acc else orig
       | i :: resti ->
	   let l' = f i in
	   if has_changed then
	     aux (List.rev_append l' acc,true) resti
	   else
	     (match l' with
		  [i'] when i' == i -> aux res resti
		| _ -> aux (List.rev_append l' (rev_until i orig), true) resti)
   in aux ([],false) orig

(* A visitor for lists *)
let doVisitList  (vis: 'visit)
                 only_copy_vis
                 (previsit: 'a -> 'a)
                 (startvisit: 'a -> 'a list visitAction)
                 (children: 'visit -> 'a -> 'a)
                 (node: 'a) : 'a list =
  let node' = previsit node in
  let action = startvisit node' in
  match action with
      SkipChildren -> [node']
    | ChangeTo nodes' -> nodes'
    | ChangeToPost (nodes',f) -> f nodes'
    | _ ->
        let nodespre = match action with
            ChangeDoChildrenPost (nodespre, _) -> nodespre
          | _ -> [node']
        in
        let vis = match action with
            JustCopy | JustCopyPost _ -> only_copy_vis
          | _ -> vis
        in
        let nodespost = mapNoCopy (children vis) nodespre in
        match action with
          | DoChildrenPost f | ChangeDoChildrenPost (_, f) | JustCopyPost f ->
              f nodespost
          | _ -> nodespost

 let doVisitListCil vis previsit startvisit children node =
   doVisitList vis vis#plain_copy_visitor previsit startvisit children node

 let optMapNoCopy f o =
   match o with
       None -> o
     | Some x ->
	 let x' = f x in if x' != x then Some x' else o

 let debugVisit = false

let visitCilConst vis c =
  match c with
    | CEnum ei -> (* In case of deep copy, we must change the enumitem*)
      let ei' = vis#behavior.get_enumitem ei in
      if ei' != ei then CEnum ei' else c
    |  _ -> c

let visitCilLConst vis c =
  match c with
    | LEnum ei -> (* In case of deep copy, we must change the enumitem*)
      let ei' = vis#behavior.get_enumitem ei in
      if ei' != ei then LEnum ei' else c
    |  _ -> c

let copy_logic_label is_copy l =
  if is_copy then begin
    match l with
      | StmtLabel s -> StmtLabel (ref !s)
      | LogicLabel(_,s) -> LogicLabel(None,s) 
          (* we don't copy the associated statement. It will be recomputed
             if needed. *)
  end else l

let rec visitCilTerm vis t =
  let oldloc = CurrentLoc.get () in
  CurrentLoc.set t.term_loc;
  let res = doVisitCil vis (fun x-> x) vis#vterm childrenTerm t in
  CurrentLoc.set oldloc; res

and childrenTerm vis t =
  let tn' = visitCilTermNode vis t.term_node in
  let tt' = visitCilLogicType vis t.term_type in
  if tn' != t.term_node || tt' != t.term_type then
      { t with term_node = tn'; term_type = tt' }
    else t
and visitCilTermNode vis tn =
  doVisitCil vis id vis#vterm_node childrenTermNode tn
and childrenTermNode vis tn =
  let vTerm t = visitCilTerm vis t in
  let vTermLval tl = visitCilTermLval vis tl in
  let vTyp t = visitCilType vis t in
  let vLogicInfo li = visitCilLogicInfoUse vis li in
    match tn with
    | TConst c ->
      let c' = visitCilLConst vis c in
      if c' != c then TConst c' else tn
    | TDataCons (ci,args) ->
        let ci' =
          doVisitCil vis id vis#vlogic_ctor_info_use alphabetabeta ci
        in
        let args' = mapNoCopy vTerm args in
        if ci' != ci || args != args' then TDataCons(ci',args') else tn
    | TLval tl ->
        let tl' = vTermLval tl in
        if tl' != tl then TLval tl' else tn
    | TSizeOf t ->
        let t' = vTyp t in if t' != t then TSizeOf t' else tn
    | TSizeOfE t ->
        let t' = vTerm t in if  t' != t then TSizeOfE t' else tn
    | TSizeOfStr _ -> tn
    | TAlignOf t ->
        let t' = vTyp t in if t' != t then TAlignOf t' else tn
    | TAlignOfE t ->
        let t' = vTerm t in if  t' != t then TAlignOfE t' else tn
    | TUnOp (op,t) ->
        let t' = vTerm t in if  t' != t then TUnOp (op,t') else tn
    | TBinOp(op,t1,t2) ->
        let t1' = vTerm t1 in
        let t2' = vTerm t2 in
        if t1' != t1 || t2' != t2 then TBinOp(op,t1',t2') else tn
    | TCastE(ty,te) ->
        let ty' = vTyp ty in
        let te' = vTerm te in
          if ty' != ty || te' != te then TCastE(ty',te') else tn
    | TAddrOf tl ->
        let tl' = vTermLval tl in
          if tl' != tl then TAddrOf tl' else tn
    | TStartOf tl ->
        let tl' = vTermLval tl in
          if tl' != tl then TStartOf tl' else tn
    | Tapp(li,labels,args) ->
        let li' = vLogicInfo li in
        let labels' = 
          mapNoCopy (visitCilLogicLabelApp vis) labels in
(*
	Format.eprintf "Cil.children_term_node: li = %s(%d), li' = %s(%d)@."
	  li.l_var_info.lv_name li.l_var_info.lv_id
          li'.l_var_info.lv_name li'.l_var_info.lv_id;
*)
        let args' = mapNoCopy vTerm args in
          if li' != li || labels' != labels || args' != args then 
            Tapp(li',labels',args') else tn
    | Tif(test,ttrue,tfalse) ->
        let test' = vTerm test in
        let ttrue' = vTerm ttrue in
        let tfalse' = vTerm tfalse in
          if test' != test || ttrue' != ttrue || tfalse' != tfalse then
            Tif(test',ttrue',tfalse')
          else tn
    | Tat(t,s) ->
        let t' = vTerm t in
        let s' = visitCilLogicLabel vis s in
        if t' != t || s' != s then Tat (t',s') else tn
    | Toffset (s,t) ->
        let s' = visitCilLogicLabel vis s in
        let t' = vTerm t in 
	if t' != t || s' != s then Toffset (s',t') else tn
    | Tbase_addr (s,t) ->
        let s' = visitCilLogicLabel vis s in
        let t' = vTerm t in 
	if t' != t || s' != s then Tbase_addr (s',t') else tn
    | Tblock_length (s,t)->
        let s' = visitCilLogicLabel vis s in
        let t' = vTerm t in 
	if t' != t || s' != s then Tblock_length (s',t') else tn
    | Tnull -> tn
    | TCoerce(te,ty) ->
        let ty' = vTyp ty in
        let te' = vTerm te in
        if ty' != ty || te' != te then TCoerce(te',ty') else tn
    | TCoerceE(te,tc) ->
        let tc' = vTerm tc in
        let te' = vTerm te in
        if tc' != tc || te' != te then TCoerceE(te',tc') else tn
    | TUpdate (tc,toff,te) ->
	let tc' = vTerm tc in
        let te' = vTerm te in
        let toff'  = visitCilTermOffset vis toff in
        if tc' != tc || (te' != te || toff' != toff)
	then TUpdate(tc',toff',te') else tn
    | Tlambda(prms,te) ->
        let prms' = visitCilQuantifiers vis prms in
        let te' = vTerm te in
        if prms' != prms || te' != te then Tlambda(prms',te') else tn
    | Ttypeof t ->
        let t' = vTerm t in if t' != t then Ttypeof t' else tn
    | Ttype ty ->
        let ty' = vTyp ty in if ty' != ty then Ttype ty' else tn
    | Tunion locs ->
        let locs' = mapNoCopy (visitCilTerm vis) locs in
        if locs != locs' then Tunion(locs') else tn
    | Tinter locs ->
        let locs' = mapNoCopy (visitCilTerm vis) locs in
        if locs != locs' then Tinter(locs') else tn
    | Tcomprehension(lval,quant,pred) ->
        let quant' = visitCilQuantifiers vis quant in
        let lval' = visitCilTerm vis lval in
        let pred' = (optMapNoCopy (visitCilPredicateNamed vis)) pred in
        if lval' != lval || quant' != quant || pred' != pred
        then
          Tcomprehension(lval',quant',pred')
        else
          tn
    | Tempty_set -> tn
    | Trange(low,high) ->
        let low' = optMapNoCopy (visitCilTerm vis) low in
        let high' = optMapNoCopy (visitCilTerm vis) high in
        if low != low' || high != high' then Trange(low',high')
        else tn
    | Tlet(def,body) ->
        let def'= visitCilLogicInfo vis def in
        let body' = visitCilTerm vis body in
        if def != def' || body != body' then Tlet(def',body') else tn
    | TLogic_coerce(ty,t) ->
        let ty' = visitCilLogicType vis ty in
        let t' = visitCilTerm vis t in
        if ty' != ty || t' != t then TLogic_coerce(ty',t') else tn

and visitCilLogicLabel vis l =
  doVisitCil vis 
    (copy_logic_label vis#behavior.is_copy_behavior)
    vis#vlogic_label childrenLogicLabel l

and childrenLogicLabel vis l = 
  match l with
      StmtLabel s -> s := vis#behavior.get_stmt !s; l
    | LogicLabel _ -> l

and visitCilLogicLabelApp vis (l1,l2 as p) =
  let l1' = visitCilLogicLabel vis l1 in
  let l2' = visitCilLogicLabel vis l2 in
  if l1 != l1' || l2 != l2' then (l1',l2') else p

 and visitCilTermLval vis tl =
   doVisitCil vis id vis#vterm_lval childrenTermLval tl

 and childrenTermLval vis ((tlv,toff) as tl)=
   let tlv' = visitCilTermLhost vis tlv in
   let toff' = visitCilTermOffset vis toff in
     if tlv' != tlv || toff' != toff then (tlv',toff') else tl

 and visitCilTermLhost vis tl =
   doVisitCil vis id vis#vterm_lhost childrenTermLhost tl

 and childrenTermLhost vis tl = match tl with
     TVar v ->
       let v' = visitCilLogicVarUse vis v in if v' != v then TVar v' else tl
   | TResult ty ->
       let ty' = visitCilType vis ty in if ty' != ty then TResult ty' else tl
   | TMem t ->
       let t' = visitCilTerm vis t in if t' != t then TMem t' else tl

 and visitCilTermOffset vis toff =
     doVisitCil vis id
       vis#vterm_offset childrenTermOffset toff

 and childrenTermOffset vis toff =
   let vOffset o = visitCilTermOffset vis o in
   let vTerm t = visitCilTerm vis t in
   match toff with
       TNoOffset -> toff
     | TField (fi, t) ->
	 let t' = vOffset t in
	 let fi' = vis#behavior.get_fieldinfo fi in
	   if t' != t || fi != fi' then TField(fi',t') else toff
     | TIndex(t,o) ->
	 let t' = vTerm t in let o' = vOffset o in
	 if t' != t || o' != o then TIndex(t',o') else toff
     | TModel (mi,t) ->
         let t' = vOffset t in
         let mi' = vis#behavior.get_model_info mi in
         if t' != t || mi != mi' then TModel(mi', t') else toff

 and visitCilLogicInfoUse vis li =
   (* First, visit the underlying varinfo to fill the copy tables if needed. *)
   let new_v = visitCilLogicVarUse vis li.l_var_info in
   let new_li =
     doVisitCil vis vis#behavior.get_logic_info
       vis#vlogic_info_use alphabetabeta li
   in
   new_li.l_var_info <- new_v;
   new_li

 and visitCilLogicInfo vis li =
   (* visit first the underlying varinfo. This will fill internal tables
      of copy behavior if needed.
    *)
   let new_v = visitCilLogicVarDecl vis li.l_var_info in
   let res =
     doVisitCil
       vis vis#behavior.memo_logic_info
       vis#vlogic_info_decl childrenLogicInfo li
   in res.l_var_info <- new_v; res

 and childrenLogicInfo vis li =
   (* NB: underlying varinfo has been already visited. *)
   let lt = optMapNoCopy (visitCilLogicType vis) li.l_type in
   let lp = mapNoCopy (visitCilLogicVarDecl vis) li.l_profile in
   li.l_type <- lt;
   li.l_profile <- lp;
   li.l_body <-
     begin
       match li.l_body with
	 | LBnone -> li.l_body
	 | LBreads ol ->
	     let l = mapNoCopy (visitCilIdTerm vis) ol in
	     if l != ol then LBreads l else li.l_body
	 | LBterm ot ->
	     let t = visitCilTerm vis ot in
	     if t != ot then LBterm t else li.l_body
	 | LBinductive inddef ->
	     let i =
	       mapNoCopy
		 (fun (id,labs,tvars,p) ->
		    (id, labs, tvars, visitCilPredicateNamed vis p))
		 inddef
	     in
	     if i != inddef then LBinductive i else li.l_body
	 | LBpred odef ->
	     let def = visitCilPredicateNamed vis odef in
	     if def != odef then LBpred def else li.l_body
     end;
   li

 and visitCilLogicTypeInfo vis lt =
   doVisitCil vis vis#behavior.memo_logic_type_info
     vis#vlogic_type_info_decl childrenLogicTypeInfo lt

 and childrenLogicTypeInfo vis lt =
   let def = optMapNoCopy (visitCilLogicTypeDef vis) lt.lt_def in
   lt.lt_def <- def; lt

 and visitCilLogicTypeDef vis def =
   doVisitCil vis id vis#vlogic_type_def childrenLogicTypeDef def

 and childrenLogicTypeDef vis def =
   match def with
     | LTsum l ->
	 let l' = mapNoCopy (visitCilLogicCtorInfoAddTable vis) l in
	 if l != l' then LTsum l' else def
     | LTsyn typ ->
	 let typ' = visitCilLogicType vis typ in
	 if typ != typ' then LTsyn typ else def

 and visitCilLogicCtorInfoAddTable vis ctor =
   let ctor' = visitCilLogicCtorInfo vis ctor in
   if is_copy_behavior vis#behavior then
     Queue.add
       (fun () ->
	  Logic_env.add_logic_ctor ctor'.ctor_name ctor')
       vis#get_filling_actions;
   ctor'

 and visitCilLogicCtorInfo vis ctor =
   doVisitCil vis id vis#vlogic_ctor_info_decl childrenLogicCtorInfo ctor

 and childrenLogicCtorInfo vis ctor =
   let ctor_type = doVisitCil vis vis#behavior.get_logic_type_info
     vis#vlogic_type_info_use alphabetabeta ctor.ctor_type
   in
   let ctor_params = mapNoCopy (visitCilLogicType vis) ctor.ctor_params in
   if ctor_type != ctor.ctor_type || ctor_params != ctor.ctor_params then
     { ctor with ctor_type = ctor_type; ctor_params = ctor_params }
   else ctor

 and visitCilLogicType vis t =
   doVisitCil vis id vis#vlogic_type childrenLogicType t

 and childrenLogicType vis ty =
   match ty with
       Ctype t ->
	 let t' = visitCilType vis t in
	 if t != t' then Ctype t' else ty
     | Linteger | Lreal -> ty
     | Ltype (s,l) ->
	 let s' = doVisitCil vis vis#behavior.get_logic_type_info
	   vis#vlogic_type_info_use alphabetabeta s in
	 let l' = mapNoCopy (visitCilLogicType vis) l in
	 if s' != s || l' != l then Ltype (s',l') else ty
     | Larrow(args,rttyp) ->
	 let args' = mapNoCopy(visitCilLogicType vis) args in
	 let rttyp' = visitCilLogicType vis rttyp in
	 if args' != args || rttyp' != rttyp then Larrow(args',rttyp') else ty
     | Lvar _ -> ty

 and visitCilLogicVarDecl vis lv =
   (* keep names in C and logic worlds in sync *)
   (match lv.lv_origin with
	 None -> ()
       | Some cv -> lv.lv_name <- cv.vname);
   doVisitCil vis vis#behavior.memo_logic_var vis#vlogic_var_decl
     childrenLogicVarDecl lv

 and childrenLogicVarDecl vis lv =
   lv.lv_type <- visitCilLogicType vis lv.lv_type;
   lv.lv_origin <-
     optMapNoCopy (visitCilVarUse vis) lv.lv_origin;
   lv

 and visitCilLogicVarUse vis lv =
   if vis#behavior.is_copy_behavior &&
     Logic_env.is_builtin_logic_function lv.lv_name then begin
       (* Do as if the variable has been declared.
	  We'll fill the logic info table of the new project at the end.
	  Behavior's logic_var table is filled as a side effect.
	*)
       let siblings = Logic_env.find_all_logic_functions lv.lv_name in
       let siblings' = List.map (visitCilLogicInfo vis) siblings in
       (*Format.printf "new vars:@.";
       List.iter (fun x -> Format.printf "%s#%d@." x.l_var_info.lv_name x.l_var_info.lv_id) siblings';
	*)
       Queue.add
	 (fun () ->
	    (* Add them to env only once *)
	    List.iter
	      (fun x ->
		 if not (Logic_env.Logic_builtin_used.mem x) then begin
 (*                  Format.printf
		     "Adding info for %s#%d@."
		     x.l_var_info.lv_name x.l_var_info.lv_id; *)
		   Logic_env.Logic_builtin_used.add x;
		   Logic_env.Logic_info.add x.l_var_info.lv_name x
		 end)
	      siblings')
	 vis#get_filling_actions;
   end;
   doVisitCil vis vis#behavior.get_logic_var vis#vlogic_var_use
     childrenLogicVarUse lv

 and childrenLogicVarUse vis lv =
   lv.lv_origin <- optMapNoCopy (visitCilVarUse vis) lv.lv_origin; lv

 and visitCilQuantifiers vis lv =
   doVisitCil vis id vis#vquantifiers
     (fun vis l -> mapNoCopy (visitCilLogicVarDecl vis) l) lv

 and visitCilIdPredicate vis ip =
   doVisitCil
     vis
     vis#behavior.cidentified_predicate 
     vis#videntified_predicate
     childrenIdentified_predicate
     ip
 and visitCilPredicate vis p =
   doVisitCil vis id vis#vpredicate childrenPredicate p

 and visitCilPredicateNamed vis p =
   doVisitCil vis
     id vis#vpredicate_named childrenPredicateNamed p

 and childrenIdentified_predicate vis ip =
  let p = Logic_const.pred_of_id_pred ip in
  let p' = visitCilPredicateNamed vis p in
  if p != p' then 
    { ip with ip_name = p'.name; ip_content = p'.content; ip_loc = p'.loc }
  else ip

 and childrenPredicateNamed vis p =
   let content = visitCilPredicate vis p.content in
   if content != p.content then { p with content = content} else p

 and childrenPredicate vis p =
   let vPred p = visitCilPredicateNamed vis p in
   let vLogicInfo li = visitCilLogicInfoUse vis li in
   let vTerm t = visitCilTerm vis t in
   match p with
       Pfalse | Ptrue -> p
     | Papp (pred,labels,args) ->
	 let pred' = vLogicInfo pred in
         let labels' = mapNoCopy (visitCilLogicLabelApp vis) labels in
	 let args' = mapNoCopy vTerm args in
	 if pred' != pred || labels' != labels || args' != args then
	   Papp(pred',labels',args')
	 else p
     | Prel(rel,t1,t2) ->
	 let t1' = vTerm t1 in
	 let t2' = vTerm t2 in
	 if t1' != t1 || t2' != t2 then
	   Prel(rel,t1',t2')
	 else p
     | Pand(p1,p2) ->
	 let p1' = vPred p1 in
	 let p2' = vPred p2 in
	 if p1' != p1 || p2' != p2 then
	   Pand(p1',p2')
	 else p
     | Por(p1,p2) ->
	 let p1' = vPred p1 in
	 let p2' = vPred p2 in
	 if p1' != p1 || p2' != p2 then
	   Por(p1',p2')
	 else p
     | Pxor(p1,p2) ->
	 let p1' = vPred p1 in
	 let p2' = vPred p2 in
	 if p1' != p1 || p2' != p2 then
	   Pxor(p1',p2')
	 else p
     | Pimplies(p1,p2) ->
	 let p1' = vPred p1 in
	 let p2' = vPred p2 in
	 if p1' != p1 || p2' != p2 then
	   Pimplies(p1',p2')
	 else p
     | Piff(p1,p2) ->
	 let p1' = vPred p1 in
	 let p2' = vPred p2 in
	 if p1' != p1 || p2' != p2 then
	   Piff(p1',p2')
	 else p
     | Pnot p1 ->
	 let p1' = vPred p1 in
	 if p1' != p1 then Pnot p1' else p
     | Pif(t,ptrue,pfalse) ->
	 let t' = vTerm t in
	 let ptrue' = vPred ptrue in
	 let pfalse' = vPred pfalse in
	 if t' != t || ptrue' != ptrue || pfalse' != pfalse then
	   Pif(t', ptrue',pfalse')
	 else p
     | Plet(def,p1) ->
	 let def' = visitCilLogicInfo vis def in
	 let p1' = vPred p1 in
	 if def' != def || p1' != p1 then
	   Plet(def',p1')
	 else p
     | Pforall(quant,p1) ->
	 let quant' = visitCilQuantifiers vis quant in
	 let p1' = vPred p1 in
	 if quant' != quant || p1' != p1 then
	   Pforall(quant', p1')
	 else p
     | Pexists(quant,p1) ->
	 let quant' = visitCilQuantifiers vis quant in
	 let p1' = vPred p1 in
	 if quant' != quant || p1' != p1 then
	   Pexists(quant', p1')
	 else p
     | Pat(p1,s) ->
	 let p1' = vPred p1 in
	 let s' = visitCilLogicLabel vis s in
	 if p1' != p1 || s != s' then Pat(p1',s') else p
     | Pallocable (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in 
	 if t' != t || s != s' then Pallocable (s',t') else p
     | Pfreeable (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in 
	 if t' != t || s != s' then Pfreeable (s',t') else p
     | Pvalid (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in 
	 if t' != t || s != s' then Pvalid (s',t') else p
     | Pvalid_read (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in
	 if t' != t || s != s' then Pvalid_read (s',t') else p
     | Pinitialized (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in
	 if t' != t || s != s' then Pinitialized (s',t') else p
     | Pdangling (s,t) ->
	 let s' = visitCilLogicLabel vis s in
	 let t' = vTerm t in
	 if t' != t || s != s' then Pdangling (s',t') else p
     | Pseparated seps ->
	 let seps' = mapNoCopy vTerm seps in
	 if seps' != seps then Pseparated seps' else p
     | Pfresh (s1,s2,t,n) ->
	 let s1' = visitCilLogicLabel vis s1 in
	 let s2' = visitCilLogicLabel vis s2 in
	 let t' = vTerm t in
	 let n' = vTerm n in 
	 if t' != t || n' != n || s1 != s1' || s2 != s2' then Pfresh (s1',s2',t',n') else p
     | Psubtype(te,tc) ->
	 let tc' = vTerm tc in
	 let te' = vTerm te in
	 if tc' != tc || te' != te then Psubtype(te',tc') else p

and visitCilIdTerm vis loc =
   doVisitCil vis vis#behavior.cidentified_term vis#videntified_term
     childrenIdentified_term loc
and childrenIdentified_term vis loc =
   let loc' = visitCilTerm vis loc.it_content in
   if loc' != loc.it_content then { loc with it_content = loc' } else loc

and visitCilAllocation vis fa =
  doVisitCil vis id vis#vallocation childrenAllocation fa
and childrenAllocation vis fa =
  match fa with
      FreeAllocAny -> fa
    | FreeAlloc(f,a)  ->
   let f' = visitCilFrees vis f in
   let a' = visitCilAllocates vis a in
   if f != f' || a' != a then FreeAlloc(f',a') else fa

 and visitCilFrees vis l =
   doVisitCil vis id vis#vfrees childrenFreeAlloc l
 and visitCilAllocates vis l =
   doVisitCil vis id vis#vallocates childrenFreeAlloc l
 and childrenFreeAlloc vis l =
   mapNoCopy (visitCilIdTerm vis) l

 and visitCilAssigns vis a =
   doVisitCil vis id vis#vassigns childrenAssigns a
 and childrenAssigns vis a =
  match a with
      WritesAny -> a
    | Writes l ->
      let l' = mapNoCopy (visitCilFrom vis) l in
      if l' != l then Writes l' else a

and visitCilFrom vis f =
  doVisitCil vis id vis#vfrom childrenFrom f
and childrenFrom vis ((b,f) as a) =
  let b' = visitCilIdTerm vis b in
  let f' = visitCilDeps vis f in
  if b!=b' || f!=f' then (b',f') else a

and visitCilDeps vis d =
  doVisitCil vis id vis#vdeps childrenDeps d
and childrenDeps vis d =
  match d with
      FromAny -> d
    | From l ->
      let l' = mapNoCopy (visitCilIdTerm vis) l in
      if l !=l' then From l' else d

and visitCilBehavior vis b =
   doVisitCil vis vis#behavior.cfunbehavior
     vis#vbehavior childrenBehavior b

and childrenBehavior vis b =
   b.b_assumes <- visitCilPredicates vis b.b_assumes;
   b.b_requires <- visitCilPredicates vis b.b_requires;
   b.b_post_cond <-
     mapNoCopy
     (function ((k,p) as pc) ->
	let p' = visitCilIdPredicate vis p in if p != p' then (k,p') else pc)
     b.b_post_cond;
   b.b_assigns <- visitCilAssigns vis b.b_assigns;
   b.b_allocation <- visitCilAllocation vis b.b_allocation ;
   b.b_extended <- mapNoCopy (visitCilExtended vis) b.b_extended;
   b

and visitCilExtended vis (s,i,p as orig) =
  let visit =
    try Hashtbl.find visitor_tbl s
    with Not_found -> (fun _ _ -> DoChildren)
  in
  let pre = i,p in
  let (i, p as res) = doVisitCil vis id (visit vis) childrenCilExtended pre in
  if res == pre then orig else (s,i,p)

and childrenCilExtended vis (i,p as orig) =
  let r = mapNoCopy (visitCilIdPredicate vis) p in
  if r == p then orig else (i,r)

and visitCilPredicates vis ps = mapNoCopy (visitCilIdPredicate vis) ps

and visitCilBehaviors vis bs = mapNoCopy (visitCilBehavior vis) bs

and visitCilFunspec vis s =
  doVisitCil vis vis#behavior.cfunspec vis#vspec childrenSpec s

and childrenSpec vis s =
  s.spec_behavior <- visitCilBehaviors vis s.spec_behavior;
  s.spec_variant <-
    optMapNoCopy (fun x -> (visitCilTerm vis (fst x), snd x)) s.spec_variant;
  s.spec_terminates <-
    optMapNoCopy (visitCilIdPredicate vis) s.spec_terminates;
  (* nothing is done now for behaviors names, no need to visit complete and
     disjoint behaviors clauses
  *)
  s

 and visitCilSlicePragma vis p =
   doVisitCil vis id vis#vslice_pragma childrenSlicePragma p

 and childrenSlicePragma vis p =
   match p with
       | SPexpr t ->
	   let t' = visitCilTerm vis t in if t' != t then SPexpr t' else p
       | SPctrl | SPstmt -> p

 and visitCilImpactPragma vis p =
   doVisitCil vis id vis#vimpact_pragma childrenImpactPragma p

 and childrenImpactPragma vis p = match p with
   | IPexpr t -> let t' = visitCilTerm vis t in if t' != t then IPexpr t' else p
   | IPstmt -> p

 and visitCilLoopPragma vis p =
   doVisitCil vis
     id vis#vloop_pragma childrenLoopPragma p

 and childrenLoopPragma vis p =
 match p with
   | Unroll_specs lt -> let lt' = mapNoCopy (visitCilTerm vis) lt in
     if lt' != lt then Unroll_specs lt' else p
   | Widen_hints lt -> let lt' = mapNoCopy (visitCilTerm vis) lt in
     if lt' != lt then Widen_hints lt' else p
   | Widen_variables lt -> let lt' = mapNoCopy (visitCilTerm vis) lt in
     if lt' != lt then Widen_variables lt' else p

 and childrenModelInfo vis m =
  let field_type = visitCilLogicType vis m.mi_field_type in
  let base_type = visitCilType vis m.mi_base_type in
  if field_type != m.mi_field_type || base_type != m.mi_base_type then
      {
        mi_name = m.mi_name;
        mi_field_type = field_type;
        mi_base_type = base_type;
        mi_decl = Cil_datatype.Location.copy m.mi_decl;
      }
  else m

 and visitCilModelInfo vis m =
  let oldloc = CurrentLoc.get () in
  CurrentLoc.set m.mi_decl;
  let m' =
    doVisitCil
      vis vis#behavior.memo_model_info vis#vmodel_info childrenModelInfo m
  in
  CurrentLoc.set oldloc;
  if m' != m then begin
    (* reflect changes in the behavior tables for copy visitor. *)
    vis#behavior.set_model_info m m';
    vis#behavior.set_orig_model_info m' m;
  end;
  m'

 and visitCilAnnotation vis a =
   let oldloc = CurrentLoc.get () in
   CurrentLoc.set (Global_annotation.loc a);
   let res = doVisitCil vis id vis#vannotation childrenAnnotation a in
   CurrentLoc.set oldloc;
   res

 and childrenAnnotation vis a =
   match a with
     | Dfun_or_pred (li,loc) ->
	 let li' = visitCilLogicInfo vis li in
	 if vis#behavior.is_copy_behavior then
	   Queue.add
	     (fun () ->
		Logic_env.add_logic_function_gen alphabetafalse li')
	     vis#get_filling_actions;
	 if li' != li then Dfun_or_pred (li',loc) else a
     | Dtype (ti,loc) ->
	 let ti' = visitCilLogicTypeInfo vis ti in
	 if vis#behavior.is_copy_behavior then
	   Queue.add
	     (fun () ->
		Logic_env.add_logic_type ti'.lt_name ti')
	     vis#get_filling_actions;
	 if ti' != ti then Dtype (ti',loc) else a
     | Dlemma(s,is_axiom,labels,tvars,p,loc) ->
	 let p' = visitCilPredicateNamed vis p in
	 if p' != p then Dlemma(s,is_axiom,labels,tvars,p',loc) else a
     | Dinvariant (p,loc) ->
	 let p' = visitCilLogicInfo vis p in
	 if vis#behavior.is_copy_behavior then
	   Queue.add
	     (fun () -> Logic_env.add_logic_function_gen alphabetafalse p')
	     vis#get_filling_actions;
	 if p' != p then Dinvariant (p',loc) else a
     | Dtype_annot (ta,loc) ->
	 let ta' = visitCilLogicInfo vis ta in
	 if vis#behavior.is_copy_behavior then
	   Queue.add
	     (fun () -> Logic_env.add_logic_function_gen alphabetafalse ta')
	     vis#get_filling_actions;
	 if ta' != ta then Dtype_annot (ta',loc) else a
     | Dmodel_annot (mfi,loc) ->
	 let mfi' = visitCilModelInfo vis mfi in
	 if vis#behavior.is_copy_behavior then
	   Queue.add (fun () -> Logic_env.add_model_field mfi')
	     vis#get_filling_actions;
	 if mfi' != mfi then Dmodel_annot (mfi',loc) else a
     | Dcustom_annot(_c,_n,_loc) -> 
       a
     | Dvolatile(tset,rvi,wvi,loc) ->
         let tset' = mapNoCopy (visitCilIdTerm vis) tset in
         let rvi' = optMapNoCopy (visitCilVarUse vis) rvi in
         let wvi' = optMapNoCopy (visitCilVarUse vis) wvi in
         if tset' != tset || rvi' != rvi || wvi' != wvi then
           Dvolatile(tset',rvi',wvi',loc)
         else a
     | Daxiomatic(id,l,loc) ->
 (*
	 Format.eprintf "cil.visitCilAnnotation on axiomatic %s@." id;
 *)
	 let l' = mapNoCopy (visitCilAnnotation vis) l in
	 if l' != l then Daxiomatic(id,l',loc) else a

 and visitCilCodeAnnotation vis ca =
   doVisitCil
     vis vis#behavior.ccode_annotation vis#vcode_annot childrenCodeAnnot ca

 and childrenCodeAnnot vis ca =
   let vPred p = visitCilPredicateNamed vis p in
   let vTerm t = visitCilTerm vis t in
   let vSpec s = visitCilFunspec vis s in
   let change_content annot = { ca with annot_content = annot } in
   match ca.annot_content with
       AAssert (behav,p) ->
	 let p' = vPred p in if p' != p then
	   change_content (AAssert (behav,p'))
	 else ca
     | APragma (Impact_pragma t) ->
	 let t' = visitCilImpactPragma vis t in
	 if t' != t then change_content (APragma (Impact_pragma t')) else ca
     | APragma (Slice_pragma t) ->
	 let t' = visitCilSlicePragma vis t in
	 if t' != t then change_content (APragma (Slice_pragma t')) else ca
     | APragma (Loop_pragma p) ->
	 let p' = visitCilLoopPragma vis p in
	 if p' != p then change_content (APragma (Loop_pragma p')) else ca
     | AStmtSpec (behav,s) ->
	 let s' = vSpec s in
	 if s' != s then change_content (AStmtSpec (behav,s')) else ca
     | AInvariant(behav,f,p) ->
	 let p' = vPred p in
	 if p' != p then change_content (AInvariant (behav,f,p')) else ca
     | AVariant ((t,s)) ->
	 let t' = vTerm t in
	 if t != t' then  change_content (AVariant ((t',s))) else ca
     | AAssigns(behav, a) ->
	 let a' = visitCilAssigns vis a in
	 if a != a' then change_content (AAssigns (behav,a')) else ca
     | AAllocation(behav, fa) ->
	 let fa' = visitCilAllocation vis fa in
	 if fa != fa' then change_content (AAllocation (behav,fa')) else ca

and visitCilExpr (vis: cilVisitor) (e: exp) : exp =
  let oldLoc = CurrentLoc.get () in
  CurrentLoc.set e.eloc;
  let res = doVisitCil vis vis#behavior.cexpr vis#vexpr childrenExp e in
  CurrentLoc.set oldLoc; res

and childrenExp (vis: cilVisitor) (e: exp) : exp =
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vLval lv = visitCilLval vis lv in
  let new_exp e' = { e with enode = e' } in
  match (stripInfo e).enode with
  | Info _ -> assert false
  | Const c ->
    let c' = visitCilConst vis c in
    if c' != c then new_exp (Const c') else e
  | SizeOf t ->
      let t'= vTyp t in
      if t' != t then new_exp (SizeOf t') else e
  | SizeOfE e1 ->
      let e1' = vExp e1 in
      if e1' != e1 then new_exp (SizeOfE e1') else e
  | SizeOfStr _s -> e

   | AlignOf t ->
       let t' = vTyp t in
       if t' != t then new_exp (AlignOf t') else e
   | AlignOfE e1 ->
       let e1' = vExp e1 in
       if e1' != e1 then new_exp (AlignOfE e1') else e
   | Lval lv ->
       let lv' = vLval lv in
       if lv' != lv then new_exp (Lval lv') else e
   | UnOp (uo, e1, t) ->
       let e1' = vExp e1 in let t' = vTyp t in
       if e1' != e1 || t' != t then new_exp (UnOp(uo, e1', t')) else e
   | BinOp (bo, e1, e2, t) ->
       let e1' = vExp e1 in let e2' = vExp e2 in let t' = vTyp t in
       if e1' != e1 || e2' != e2 || t' != t then
	 new_exp (BinOp(bo, e1',e2',t'))
       else e
   | CastE (t, e1) ->
       let t' = vTyp t in let e1' = vExp e1 in
       if t' != t || e1' != e1 then new_exp (CastE(t', e1')) else e
   | AddrOf lv ->
       let lv' = vLval lv in
       if lv' != lv then new_exp (AddrOf lv') else e
   | StartOf lv ->
       let lv' = vLval lv in
       if lv' != lv then new_exp (StartOf lv') else e

 and visitCilInit (vis: cilVisitor) (forglob: varinfo)
		  (atoff: offset) (i: init) : init =
   let childrenInit (vis: cilVisitor) (i: init) : init =
     let fExp e = visitCilExpr vis e in
     let fTyp t = visitCilType vis t in
     match i with
     | SingleInit e ->
	 let e' = fExp e in
	 if e' != e then SingleInit e' else i
     | CompoundInit (t, initl) ->
	 let t' = fTyp t in
	 (* Collect the new initializer list, in reverse. We prefer two
	  * traversals to ensure tail-recursion. *)
	 let newinitl : (offset * init) list ref = ref [] in
	 (* Keep track whether the list has changed *)
	 let hasChanged = ref false in
	 let doOneInit ((o, i) as oi) =
	   let o' = visitCilInitOffset vis o in    (* use initializer version *)
	   let i' = visitCilInit vis forglob (addOffset o' atoff) i in
	   let newio =
	     if o' != o || i' != i then
	       begin hasChanged := true; (o', i') end else oi
	   in
	   newinitl := newio :: !newinitl
	 in
	 List.iter doOneInit initl;
	 let initl' = if !hasChanged then List.rev !newinitl else initl in
	 if t' != t || initl' != initl then CompoundInit (t', initl') else i
   in
   doVisitCil vis id (vis#vinit forglob atoff) childrenInit i

 and visitCilLval (vis: cilVisitor) (lv: lval) : lval =
   doVisitCil vis id vis#vlval childrenLval lv
 and childrenLval (vis: cilVisitor) (lv: lval) : lval =
   (* and visit its subexpressions *)
   let vExp e = visitCilExpr vis e in
   let vOff off = visitCilOffset vis off in
   match lv with
     Var v, off ->
       let v'= visitCilVarUse vis v in
       let off' = vOff off in
       if v' != v || off' != off then Var v', off' else lv
   | Mem e, off ->
       let e' = vExp e in
       let off' = vOff off in
       if e' != e || off' != off then Mem e', off' else lv

 and visitCilOffset (vis: cilVisitor) (off: offset) : offset =
   doVisitCil vis id vis#voffs childrenOffset off
 and childrenOffset (vis: cilVisitor) (off: offset) : offset =
   let vOff off = visitCilOffset vis off in
   match off with
     Field (f, o) ->
       let o' = vOff o in
       let f' = vis#behavior.get_fieldinfo f in
       if o' != o || f' != f then Field (f', o') else off
   | Index (e, o) ->
       let e' = visitCilExpr vis e in
       let o' = vOff o in
       if e' != e || o' != o then Index (e', o') else off
   | NoOffset -> off

 (* sm: for offsets in initializers, the 'startvisit' will be the
  * vinitoffs method, but we can re-use the childrenOffset from
  * above since recursive offsets are visited by voffs.  (this point
  * is moot according to cil.mli which claims the offsets in
  * initializers will never recursively contain offsets)
  *)
 and visitCilInitOffset (vis: cilVisitor) (off: offset) : offset =
   doVisitCil vis id vis#vinitoffs childrenOffset off

 and visitCilInstr (vis: cilVisitor) (i: instr) : instr list =
   let oldloc = CurrentLoc.get () in
   CurrentLoc.set (Cil_datatype.Instr.loc i);
   assertEmptyQueue vis;
   let res =
     doVisitListCil vis id vis#vinst childrenInstr i in
   CurrentLoc.set oldloc;
   (* See if we have accumulated some instructions *)
   vis#unqueueInstr () @ res

 and childrenInstr (vis: cilVisitor) (i: instr) : instr =
   let fExp = visitCilExpr vis in
   let fLval = visitCilLval vis in
   match i with
   | Skip _l ->
       i
   | Set(lv,e,l) ->
       let lv' = fLval lv in let e' = fExp e in
       if lv' != lv || e' != e then Set(lv',e',l) else i
   | Call(None,f,args,l) ->
       let f' = fExp f in let args' = mapNoCopy fExp args in
       if f' != f || args' != args then Call(None,f',args',l) else i
   | Call(Some lv,fn,args,l) ->
       let lv' = fLval lv in let fn' = fExp fn in
       let args' = mapNoCopy fExp args in
       if lv' != lv || fn' != fn || args' != args
       then Call(Some lv', fn', args', l) else i

   | Asm(sl,isvol,outs,ins,clobs,labels,l) ->
       let outs' = mapNoCopy (fun ((id,s,lv) as pair) ->
				let lv' = fLval lv in
				if lv' != lv then (id,s,lv') else pair) outs in
       let ins'  = mapNoCopy (fun ((id,s,e) as pair) ->
				let e' = fExp e in
				if e' != e then (id,s,e') else pair) ins in
       if outs' != outs || ins' != ins then
	 Asm(sl,isvol,outs',ins',clobs,labels,l) else i
   | Code_annot (a,l) ->
       let a' = visitCilCodeAnnotation vis a in 
	 if a != a' then Code_annot(a',l) else i

 (* visit all nodes in a Cil statement tree in preorder *)
 and visitCilStmt (vis:cilVisitor) (s: stmt) : stmt =
   let oldloc = CurrentLoc.get () in
   CurrentLoc.set (Stmt.loc s) ;
   vis#push_stmt s; (*(vis#behavior.memo_stmt s);*)
   assertEmptyQueue vis;
   let toPrepend : instr list ref = ref [] in (* childrenStmt may add to this *)
   let res =
     doVisitCil vis
       vis#behavior.memo_stmt vis#vstmt (childrenStmt toPrepend) s in
   (* Now see if we have saved some instructions *)
   toPrepend := !toPrepend @ vis#unqueueInstr ();
   (match !toPrepend with
     [] -> () (* Return the same statement *)
   | _ ->
       (* Make our statement contain the instructions to prepend *)
       res.skind <-
	 Block (mkBlock
		 ((List.map (fun i -> mkStmt (Instr i)) !toPrepend) @
			  [ mkStmt res.skind ] )));
   CurrentLoc.set oldloc;
   vis#pop_stmt s;
   res

 and childrenStmt (toPrepend: instr list ref) (vis:cilVisitor) (s:stmt): stmt =
   let fExp e = (visitCilExpr vis e) in
   let fBlock b = visitCilBlock vis b in
   let fInst i = visitCilInstr vis i in
   let fLoopAnnot a = mapNoCopy (visitCilCodeAnnotation vis) a in
   (* Just change the statement kind *)
   let skind' =
     match s.skind with
       Break _ | Continue _ | Return (None, _) -> s.skind
     | UnspecifiedSequence seq ->
	 let seq' =
	   mapNoCopy
	     (function (stmt,modified,writes,reads,calls) as orig->
		let stmt' = visitCilStmt vis stmt in
		(* might make sense for the default to be
		   to just copy the varinfo when using the copy visitor,
		   and not apply vvrbl, i.e. not using vis but generic_visitor ?
		 *)
		let modified' = mapNoCopy (visitCilLval vis) modified in
		let writes' = mapNoCopy (visitCilLval vis) writes in
		let reads' = mapNoCopy (visitCilLval vis) reads in
		let calls' =
		  if vis#behavior.is_copy_behavior then
		    (* we need new references anyway, no need for mapNoCopy *)
		    List.map (fun x -> ref (vis#behavior.memo_stmt !x)) calls
		  else calls
		in
		if stmt' != stmt || writes' != writes || reads' != reads ||
		  modified != modified' || calls' != calls
		then
		  (stmt',modified', writes',reads',calls')
		else orig)
	     seq
	 in
	 if seq' != seq then UnspecifiedSequence seq' else s.skind
     | Goto (sr,l) ->
	 if vis#behavior.is_copy_behavior then
	   Goto(ref (vis#behavior.memo_stmt !sr),l)
	 else s.skind
     | Return (Some e, l) ->
	 let e' = fExp e in
	 if e' != e then Return (Some e', l) else s.skind
     | Loop (a, b, l, s1, s2) ->
	 let a' = fLoopAnnot a in
	 let b' = fBlock b in
	 if a' != a || b' != b then Loop (a', b', l, s1, s2) else s.skind
     | If(e, s1, s2, l) ->
	 let e' = fExp e in
	 (*if e queued any instructions, pop them here and remember them so that
	   they are inserted before the If stmt, not in the then block. *)
	 toPrepend := vis#unqueueInstr ();
	 let s1'= fBlock s1 in let s2'= fBlock s2 in
	 (* the stmts in the blocks should have cleaned up after themselves.*)
	 assertEmptyQueue vis;
	 if e' != e || s1' != s1 || s2' != s2 then
	   If(e', s1', s2', l) else s.skind
     | Switch (e, b, stmts, l) ->
	 let e' = fExp e in
	 toPrepend := vis#unqueueInstr (); (* insert these before the switch *)
	 let b' = fBlock b in
	 let stmts' = mapNoCopy (vis#behavior.get_stmt) stmts in
	 (* the stmts in b should have cleaned up after themselves.*)
	 assertEmptyQueue vis;
	 if e' != e || b' != b || stmts' != stmts then
	   Switch (e', b', stmts', l) else s.skind
     | Instr i ->
	 begin match fInst i with
	   | [i'] when i' == i -> s.skind
	   | il -> stmt_of_instr_list ~loc:(Cil_datatype.Instr.loc i) il
	 end
     | Block b ->
	 let b' = fBlock b in
	 if b' != b then Block b' else s.skind
     | Throw (e,loc) ->
       let visit (e,t as exc) =
         let e' = fExp e in
         let t' = visitCilType vis t in
         if e != e' || t != t' then (e',t') else exc
       in
       let e' = optMapNoCopy visit e in
       if e != e' then Throw (e,loc) else s.skind
     | TryCatch (b,l,loc) ->
       let b' = fBlock b in
       let visit (v,b as catch) =
         let v' = visitCilCatch_binder vis v in
         let b' = fBlock b in
         if v != v' || b != b' then (v', b') else catch
       in
       let l' = mapNoCopy visit l in
       if b != b' || l != l' then TryCatch (b', l',loc) else s.skind
     | TryFinally (b, h, l) ->
	 let b' = fBlock b in
	 let h' = fBlock h in
	 if b' != b || h' != h then TryFinally(b', h', l) else s.skind
     | TryExcept (b, (il, e), h, l) ->
	 let b' = fBlock b in
	 assertEmptyQueue vis;
	 (* visit the instructions *)
	 let il' = mapNoCopyList fInst il in
	 (* Visit the expression *)
	 let e' = fExp e in
	 let il'' =
	   let more = vis#unqueueInstr () in
	   if more != [] then
	     il' @ more
	   else
	     il'
	 in
	 let h' = fBlock h in
	 (* Now collect the instructions *)
	 if b' != b || il'' != il || e' != e || h' != h then
	   TryExcept(b', (il'', e'), h', l)
	 else s.skind
   in
   if skind' != s.skind then s.skind <- skind';
   (* Visit the labels *)
   let labels' =
     let fLabel = function
	 Case (e, l) as lb ->
	   let e' = fExp e in
	   if e' != e then Case (e', l) else lb
	 | lb -> lb
     in
     mapNoCopy fLabel s.labels
   in
   if labels' != s.labels then s.labels <- labels';
   s

 and visitCilCatch_binder vis cb =
  match cb with
    | Catch_exn (v,l) ->
      let visit_one_conversion (v, b as conv) =
        let v' = visitCilVarDecl vis v in
        let b' = visitCilBlock vis b in
        if v != v' || b != b' then (v', b') else conv
      in
      let v' = visitCilVarDecl vis v in
      let l' = mapNoCopy visit_one_conversion l in
      if v != v' || l != l' then Catch_exn(v',l') else cb
    | Catch_all -> cb
 and visitCilBlock (vis: cilVisitor) (b: block) : block =
   doVisitCil vis vis#behavior.cblock vis#vblock childrenBlock b
 and childrenBlock (vis: cilVisitor) (b: block) : block =
   let fStmt s = visitCilStmt vis s in
   let stmts' = mapNoCopy fStmt b.bstmts in
   let locals' = mapNoCopy (vis#behavior.get_varinfo) b.blocals in
   if stmts' != b.bstmts || locals' != b.blocals then
     { battrs = b.battrs; bstmts = stmts'; blocals = locals' }
   else b


 and visitCilType (vis : cilVisitor) (t : typ) : typ =
   doVisitCil vis id vis#vtype childrenType t
 and childrenType (vis : cilVisitor) (t : typ) : typ =
   (* look for types referred to inside t's definition *)
   let fTyp t  = visitCilType vis t in
   let fAttr a = visitCilAttributes vis a in
   match t with
     TPtr(t1, a) ->
       let t1' = fTyp t1 in
       let a' = fAttr a in
       if t1' != t1 || a' != a then TPtr(t1', a') else t
   | TArray(t1, None, _, a) ->
       let t1' = fTyp t1 in
       let a' = fAttr a in
       if t1' != t1 || a' != a  then TArray(t1', None, empty_size_cache (), a') else t
   | TArray(t1, Some e, _, a) ->
       let t1' = fTyp t1 in
       let e' = visitCilExpr vis e in
       let a' = fAttr a in
       if t1' != t1 || e' != e  || a' != a then TArray(t1', Some e',empty_size_cache (), a') else t

       (* DON'T recurse into the compinfo, this is done in visitCilGlobal.
	  User can iterate over cinfo.cfields manually, if desired.*)
   | TComp(cinfo, _, a) ->
       let cinfo' = vis#behavior.get_compinfo cinfo in
       let a' = fAttr a in
       if a != a' || cinfo' != cinfo then TComp(cinfo',empty_size_cache (), a') else t

   | TFun(rettype, args, isva, a) ->
       let rettype' = fTyp rettype in
       (* iterate over formals, as variable declarations *)
       let argslist = argsToList args in
       let visitArg ((an,at,aa) as arg) =
	 let at' = fTyp at in
	 let aa' = fAttr aa in
	 if at' != at || aa' != aa then (an,at',aa') else arg
       in
       let argslist' = mapNoCopy visitArg argslist in
       let a' = fAttr a in
       if rettype' != rettype || argslist' != argslist || a' != a  then
	 let args' = if argslist' == argslist then args else Some argslist' in
	 TFun(rettype', args', isva, a') else t

   | TNamed(t1, a) ->
       let a' = fAttr a in
       let t1' = vis#behavior.get_typeinfo t1 in
       if a' != a  || t1' != t1 then TNamed (t1', a') else t
   | TEnum(enum,a) ->
       let a' = fAttr a in
       let enum' = vis#behavior.get_enuminfo enum in
       if a' != a || enum' != enum then TEnum(enum',a') else t
   | TVoid _ | TInt _ | TFloat _ | TBuiltin_va_list _  ->
       (* no nested type. visit only the attributes. *)
       let a = typeAttrs t in
       let a' = fAttr a in
       if a' != a  then setTypeAttrs t a' else t

 (* for declarations, we visit the types inside; but for uses, *)
 (* we just visit the varinfo node *)
 and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
   let oldloc = CurrentLoc.get () in
   CurrentLoc.set v.vdecl;
   let res =
     doVisitCil vis vis#behavior.memo_varinfo
       vis#vvdec childrenVarDecl v
   in CurrentLoc.set oldloc; res

 and childrenVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  (* in case of refresh visitor, the associated new logic var has a different
     id. We must visit the original logic var associated to it. *)
  let visit_orig_var_assoc lv =
    let o = vis#behavior.get_original_logic_var lv in
    visitCilLogicVarDecl vis o
  in
   v.vtype <- visitCilType vis v.vtype;
   v.vattr <- visitCilAttributes vis v.vattr;
   v.vlogic_var_assoc <- optMapNoCopy visit_orig_var_assoc v.vlogic_var_assoc;
   v

 and visitCilVarUse vis v =
   doVisitCil vis vis#behavior.get_varinfo vis#vvrbl alphabetabeta v

 and visitCilAttributes (vis: cilVisitor) (al: attribute list) : attribute list=
    let al' =
      mapNoCopyList
	(doVisitListCil vis
	   id vis#vattr childrenAttribute) al in
    if al' != al then
      (* Must re-sort *)
      addAttributes al' []
    else
      al
 and childrenAttribute (vis: cilVisitor) (a: attribute) : attribute =
   let fAttrP a = visitCilAttrParams vis a in
   match a with
   | Attr (n, args) ->
       let args' = mapNoCopy fAttrP args in
       if args' != args then Attr(n, args') else a
   | AttrAnnot _ ->
       a

 and visitCilAttrParams (vis: cilVisitor) (a: attrparam) : attrparam =
    doVisitCil vis id vis#vattrparam childrenAttrparam a
 and childrenAttrparam (vis: cilVisitor) (aa: attrparam) : attrparam =
   let fTyp t  = visitCilType vis t in
   let fAttrP a = visitCilAttrParams vis a in
   match aa with
       AInt _ | AStr _ -> aa
     | ACons(n, args) ->
	 let args' = mapNoCopy fAttrP args in
	 if args' != args then ACons(n, args') else aa
     | ASizeOf t ->
	 let t' = fTyp t in
	 if t' != t then ASizeOf t' else aa
     | ASizeOfE e ->
	 let e' = fAttrP e in
	 if e' != e then ASizeOfE e' else aa
     | AAlignOf t ->
	 let t' = fTyp t in
	 if t' != t then AAlignOf t' else aa
     | AAlignOfE e ->
	 let e' = fAttrP e in
	 if e' != e then AAlignOfE e' else aa
     | AUnOp (uo, e1) ->
	 let e1' = fAttrP e1 in
	 if e1' != e1 then AUnOp (uo, e1') else aa
     | ABinOp (bo, e1, e2) ->
	 let e1' = fAttrP e1 in
	 let e2' = fAttrP e2 in
	 if e1' != e1 || e2' != e2 then ABinOp (bo, e1', e2') else aa
     | ADot (ap, s) ->
	 let ap' = fAttrP ap in
	 if ap' != ap then ADot (ap', s) else aa
     | AStar ap ->
	 let ap' = fAttrP ap in
	 if ap' != ap then AStar ap' else aa
     | AAddrOf ap ->
	 let ap' = fAttrP ap in
	 if ap' != ap then AAddrOf ap' else aa
     | AIndex (e1, e2) ->
	 let e1' = fAttrP e1 in
	 let e2' = fAttrP e2 in
	 if e1' != e1 || e2' != e2 then AIndex (e1', e2') else aa
     | AQuestion (e1, e2, e3) ->
	 let e1' = fAttrP e1 in
	 let e2' = fAttrP e2 in
	 let e3' = fAttrP e3 in
	 if e1' != e1 || e2' != e2 || e3' != e3
	 then AQuestion (e1', e2', e3') else aa


 let rec fix_succs_preds_block b block =
   List.iter (fix_succs_preds b) block.bstmts
 and fix_succs_preds b stmt =
   stmt.succs <- mapNoCopy b.get_stmt stmt.succs;
   stmt.preds <- mapNoCopy b.get_stmt stmt.preds;
   match stmt.skind with
       If(_,bthen,belse,_) ->
	 fix_succs_preds_block b bthen;
	 fix_succs_preds_block b belse
     | Switch(e,cases,stmts,l) ->
	 fix_succs_preds_block b cases;
	 stmt.skind <- Switch(e,cases,List.map b.get_stmt stmts,l)
     | Loop(annot,block,loc,stmt1,stmt2) ->
	 fix_succs_preds_block b block;
	 let stmt1' = optMapNoCopy b.get_stmt stmt1 in
	 let stmt2' = optMapNoCopy b.get_stmt stmt2 in
	 stmt.skind <- Loop(annot,block,loc,stmt1',stmt2')
     | Block block -> fix_succs_preds_block b block
     | TryFinally(block1,block2,_) ->
	 fix_succs_preds_block b block1;
	 fix_succs_preds_block b block2
     | TryExcept(block1,_,block2,_) ->
	 fix_succs_preds_block b block1;
	 fix_succs_preds_block b block2
     | _ -> ()

 let rec visitCilFunction (vis : cilVisitor) (f : fundec) : fundec =
   if debugVisit then Kernel.feedback "Visiting function %s" f.svar.vname ;
   assertEmptyQueue vis;
   vis#set_current_func f;
   (* update fundec tables *)
   let f = vis#behavior.memo_fundec f in
   let f =
     doVisitCil vis id (* copy has already been done *)
       vis#vfunc childrenFunction f
   in
   let toPrepend = vis#unqueueInstr () in
   if toPrepend <> [] then
     f.sbody.bstmts <-
       (List.map (fun i -> mkStmt (Instr i)) toPrepend) @ f.sbody.bstmts;
   if vis#behavior.is_copy_behavior then begin
     fix_succs_preds_block vis#behavior f.sbody;
     f.sallstmts <- List.map vis#behavior.get_stmt f.sallstmts
   end;
   vis#reset_current_func ();
   f

 and childrenFunction (vis : cilVisitor) (f : fundec) : fundec =
   (* we have already made a copy of the svar, but not visited it.
      Use the original variable as argument of visitCilVarDecl,
      update fundec table in case the vid gets changed. *)
   let v = vis#behavior.get_original_varinfo f.svar in
   let nv = visitCilVarDecl vis v in
   if not (Cil_datatype.Varinfo.equal nv f.svar) then begin
     Kernel.fatal
       "Visiting the varinfo declared for function %a changes its id."
       Cil_datatype.Varinfo.pretty nv
   end;
   f.svar <- nv; (* hit the function name *)
   (* visit local declarations *)
   f.slocals <- mapNoCopy (visitCilVarDecl vis) f.slocals;
   (* visit the formals *)
   let newformals = mapNoCopy (visitCilVarDecl vis) f.sformals in
   (* Make sure the type reflects the formals *)
   let selection = State_selection.singleton FormalsDecl.self in
   if vis#behavior.is_copy_behavior || newformals != f.sformals then begin
     apply_on_project ~selection vis (setFormals f) newformals;
   end;
   (* Remember any new instructions that were generated while visiting
      variable declarations. *)
   let toPrepend = vis#unqueueInstr () in
   f.sbody <- visitCilBlock vis f.sbody;       (* visit the body *)
   if toPrepend <> [] then
     f.sbody.bstmts <-
       (List.map (fun i -> mkStmt (Instr i)) toPrepend) @ f.sbody.bstmts;
   if not (is_empty_funspec f.sspec) then
     f.sspec <- visitCilFunspec vis f.sspec;
   f

 let childrenFieldInfo vis fi =
   (* already done at copy creation *)
   (* fi.fcomp <- vis#behavior.get_compinfo fi.fcomp; *)
   fi.ftype <- visitCilType vis fi.ftype;
   fi.fattr <- visitCilAttributes vis fi.fattr;
   fi

 let visitCilFieldInfo vis f =
   let f = vis#behavior.get_original_fieldinfo f in
   doVisitCil vis vis#behavior.memo_fieldinfo vis#vfieldinfo childrenFieldInfo f

 let childrenCompInfo vis comp =
   comp.cfields <- mapNoCopy (visitCilFieldInfo vis) comp.cfields;
   comp.cattr <- visitCilAttributes vis comp.cattr;
   comp

 let visitCilCompInfo vis c =
   doVisitCil vis vis#behavior.memo_compinfo vis#vcompinfo childrenCompInfo c

 let childrenEnumItem vis e =
   e.eival <- visitCilExpr vis e.eival;
   e.eihost <- vis#behavior.get_enuminfo e.eihost;
   e

 let visitCilEnumItem vis e =
   doVisitCil vis vis#behavior.memo_enumitem vis#venumitem childrenEnumItem e

 let childrenEnumInfo vis e =
   e.eitems <- mapNoCopy (visitCilEnumItem vis) e.eitems;
   e.eattr <- visitCilAttributes vis e.eattr;
   e

 let visitCilEnumInfo vis e =
   doVisitCil vis vis#behavior.memo_enuminfo vis#venuminfo childrenEnumInfo e

 let rec visitCilGlobal (vis: cilVisitor) (g: global) : global list =
   let oldloc = CurrentLoc.get () in
   CurrentLoc.set (Global.loc g) ;
   currentGlobal := g;
   let res =
     doVisitListCil vis id vis#vglob childrenGlobal g in
   CurrentLoc.set oldloc;
   res
 and childrenGlobal (vis: cilVisitor) (g: global) : global =
   match g with
   | GFun (f, l) ->
       let f' = visitCilFunction vis f in
       if f' != f then GFun (f', l) else g
   | GType(t, l) ->
       let t' = vis#behavior.memo_typeinfo t in
       t'.ttype <- visitCilType vis t'.ttype;
       if t' != t then GType(t',l) else g
   | GEnumTagDecl (enum,l) ->
       let enum' = vis#behavior.memo_enuminfo enum in
       if enum != enum' then GEnumTagDecl(enum',l) else g
	 (* real visit'll be done in the definition *)
   | GCompTagDecl (comp,l) ->
       let comp' = vis#behavior.memo_compinfo comp in
       if comp != comp' then GCompTagDecl(comp',l) else g
   | GEnumTag (enum, l) ->
       let enum' = visitCilEnumInfo vis enum in
       if enum != enum' then GEnumTag(enum',l) else g
   | GCompTag (comp, l) ->
       let comp' = visitCilCompInfo vis comp in
       if comp != comp' then GCompTag(comp',l) else g
   | GVarDecl(v, l) ->
       let v' = visitCilVarDecl vis v in
       if v' != v then GVarDecl (v', l) else g
   | GFunDecl(spec, v, l) ->
       let form =
	 try Some (getFormalsDecl v) with Not_found -> None
       in
       let v' = visitCilVarDecl vis v in
       let form' = optMapNoCopy (mapNoCopy (visitCilVarDecl vis)) form in
       let spec' =
	 if is_empty_funspec spec then begin
           if is_copy_behavior vis#behavior then
	     empty_funspec ()
           else spec (* do not need to change it if it's not a copy visitor. *)
         end else begin
	   visitCilFunspec vis spec
	 end
       in
       if v' != v || spec' != spec || form != form' then
	 begin
	   (match form' with
	      | Some formals 
                  when vis#behavior.is_copy_behavior || form != form' ->
		  let selection = State_selection.singleton FormalsDecl.self in
		  apply_on_project
                    ~selection vis (unsafeSetFormalsDecl v') formals
              | Some _ | None -> ());
	   GFunDecl (spec', v', l)
	 end
       else g
   | GVar (v, inito, l) ->
       let v' = visitCilVarDecl vis v in
       let inito' = vis#behavior.cinitinfo inito in
       (match inito'.init with
	 None -> ()
       | Some i -> let i' = visitCilInit vis v NoOffset i in
	 if i' != i then inito'.init <- Some i');
       if v' != v || inito' != inito then GVar (v', inito', l) else g
   | GPragma (a, l) -> begin
       match visitCilAttributes vis [a] with
	 [a'] -> if a' != a then GPragma (a', l) else g
       | _ -> Kernel.fatal "visitCilAttributes returns more than one attribute"
   end
   | GAnnot (a,l) ->
       let a' = visitCilAnnotation vis a in
	 if a' != a then GAnnot(a',l) else g
   | GText _ | GAsm _ -> g

(* sm: utility *)
let startsWith prefix s =
  let prefixLen = String.length prefix in
  String.length s >= prefixLen && String.sub s 0 prefixLen = prefix

let bytesSizeOfInt (ik: ikind): int =
  match ik with
  | IChar | ISChar | IUChar | IBool -> 1
  | IInt | IUInt -> theMachine.theMachine.sizeof_int
  | IShort | IUShort -> theMachine.theMachine.sizeof_short
  | ILong | IULong -> theMachine.theMachine.sizeof_long
  | ILongLong | IULongLong -> theMachine.theMachine.sizeof_longlong

let bitsSizeOfInt ik = 8 * bytesSizeOfInt ik

let intKindForSize (s:int) (unsigned:bool) : ikind =
  if unsigned then 
    (* Test the most common sizes first *)
    if s = 1 then IUChar
    else if s = theMachine.theMachine.sizeof_int then IUInt
    else if s = theMachine.theMachine.sizeof_long then IULong
    else if s = theMachine.theMachine.sizeof_short then IUShort
    else if s = theMachine.theMachine.sizeof_longlong then IULongLong
    else raise Not_found
  else
    (* Test the most common sizes first *)
    if s = 1 then ISChar
    else if s = theMachine.theMachine.sizeof_int then IInt
    else if s = theMachine.theMachine.sizeof_long then ILong
    else if s = theMachine.theMachine.sizeof_short then IShort
    else if s = theMachine.theMachine.sizeof_longlong then ILongLong
    else raise Not_found

let uint64_t () = TInt(intKindForSize 8 true,[])
let uint32_t () = TInt(intKindForSize 4 true,[])
let uint16_t () = TInt(intKindForSize 2 true,[])
let int64_t () = TInt(intKindForSize 8 false,[])
let int32_t () = TInt(intKindForSize 4 false,[])
let int16_t () = TInt(intKindForSize 2 false,[])

let floatKindForSize (s:int) = 
  if s = theMachine.theMachine.sizeof_double then FDouble
  else if s = theMachine.theMachine.sizeof_float then FFloat
  else if s = theMachine.theMachine.sizeof_longdouble then FLongDouble
  else raise Not_found

(** Returns true if and only if the given integer type is signed. *)
let isSigned = function
  | IUChar | IBool
  | IUShort
  | IUInt
  | IULong
  | IULongLong ->
      false
  | ISChar
  | IShort
  | IInt
  | ILong
  | ILongLong -> 
    true
  | IChar -> 
    not theMachine.theMachine.Cil_types.char_is_unsigned

let max_signed_number nrBits = 
  let n = nrBits-1 in
  Integer.pred (Integer.shift_left Integer.one (Integer.of_int n))
let max_unsigned_number nrBits = 
  Integer.pred (Integer.shift_left Integer.one (Integer.of_int nrBits))
let min_signed_number nrBits = 
  let n = nrBits-1 in
  Integer.neg (Integer.shift_left Integer.one (Integer.of_int n))

let debugTruncation = false

(* True if the integer fits within the kind's range *)
let fitsInInt k i = 
  let signed = isSigned k in 
  let nrBits =
    let unsignedbits = 8 * (bytesSizeOfInt k) in
    if signed then
      unsignedbits-1
    else
      unsignedbits
  in
  let max_strict_bound =
    Integer.shift_left Integer.one (Integer.of_int nrBits)
  in
  let min_bound = if signed then Integer.neg max_strict_bound
    else Integer.zero
  in
  let fits = Integer.le min_bound i && Integer.lt i max_strict_bound in
  if debugTruncation then
    Kernel.debug "Fits in %a %a : %b@."
      !pp_ikind_ref k Datatype.Integer.pretty i fits;
  fits
    
(* Represents an integer as for a given kind.
   Returns a flag saying whether the value was changed
   during truncation (because it was too large to fit in k). *)
let truncateInteger64 (k: ikind) i =
  if fitsInInt k i then 
    i, false
  else
    let i' = 
      let nrBits = Integer.of_int (8 * (bytesSizeOfInt k)) in
      let max_strict_bound = Integer.shift_left Integer.one nrBits in
      let modulo = Integer.pos_rem i max_strict_bound in
      let signed = isSigned k in
      if signed then 
        let max_signed_strict_bound = 
          Integer.shift_right max_strict_bound Integer.one
        in
        if Integer.ge modulo max_signed_strict_bound then
          Integer.sub modulo max_strict_bound
        else if Integer.lt modulo (Integer.neg max_signed_strict_bound)
        then Integer.add modulo max_strict_bound
        else modulo
        else 
          if Integer.lt modulo Integer.zero then
            Integer.add modulo max_strict_bound
          else
            modulo
    in
    if debugTruncation then
      Kernel.debug ~level:3 "Truncate %a to %a: %a" 
        Datatype.Integer.pretty i !pp_ikind_ref k Datatype.Integer.pretty i';
    i', true

exception Not_representable
let intKindForValue i (unsigned: bool) = 
  if unsigned then
    if fitsInInt IUChar i then IUChar
    else if fitsInInt IUShort i then IUShort
    else if fitsInInt IUInt i then IUInt
    else if fitsInInt IULong i then IULong
    else if fitsInInt IULongLong i then IULongLong
    else raise Not_representable
  else
    if fitsInInt ISChar i then ISChar
    else if fitsInInt IShort i then IShort
    else if fitsInInt IInt i then IInt
    else if fitsInInt ILong i then ILong
    else if fitsInInt ILongLong i then ILongLong
    else raise Not_representable

(* Construct an integer constant with possible truncation if the kind is not
   specified  *)
let kinteger64 ~loc ?repr ?kind i = 
  if debugTruncation then
    Kernel.debug ~level:3 "kinteger64 %a" Datatype.Integer.pretty i;
  let kind = match kind with 
    | None ->
      (* compute the best ikind: [int] whenever possible and, if no signed type
         is possible, try unsigned long long. *)
      if fitsInInt IInt i then IInt
      else begin
        try intKindForValue i false 
        with Not_representable as exn -> 
          if fitsInInt IULongLong i then IULongLong else raise exn
      end
    | Some k -> k
  in
  let i', _truncated = truncateInteger64 kind i in
  new_exp ~loc (Const (CInt64(i' , kind,  repr)))

(* Construct an integer of a given kind. *)
let kinteger ~loc kind (i: int) = kinteger64 ~loc ~kind (Integer.of_int i)

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer_constant i = CInt64(Integer.of_int i, IInt, None)
(* Construct an integer. Use only for values that fit on 31 bits *)
let integer ~loc (i: int) = new_exp ~loc (Const (integer_constant i))

let kfloat ~loc k f = new_exp ~loc (Const (CReal(f,k,None)))

let zero      ~loc = integer ~loc 0
let one       ~loc = integer ~loc 1
let mone      ~loc = integer ~loc (-1)

let integer_lconstant v = TConst (Integer (Integer.of_int v,None))

let lconstant ?(loc=Location.unknown) v =
  { term_node = TConst (Integer (v,None)); term_loc = loc;
    term_name = []; term_type = Linteger;}

let lzero ?(loc=Location.unknown) () = lconstant ~loc Integer.zero
let lone  ?(loc=Location.unknown) () = lconstant ~loc Integer.one
let lmone ?(loc=Location.unknown) () = lconstant ~loc (Integer.minus_one)

 (** Given the character c in a (CChr c), sign-extend it to 32 bits.
     (This is the official way of interpreting character constants, according 
     to ISO C 6.4.4.4.10, which says that character constants are chars cast 
     to ints) *)
let charConstToInt c =
  let c' = Char.code c in
  if c' < 128
  then Integer.of_int c'
  else Integer.of_int (c' - 256)

let charConstToIntConstant c =
  CInt64(charConstToInt c, IInt, None)

let rec isInteger e = match e.enode with
| Const(CInt64 (n,_,_)) -> Some n
| Const(CChr c) -> Some (charConstToInt c)
| Const(CEnum {eival = v}) -> isInteger v
| CastE(_, e) -> isInteger e (* BY: This is really strange... *)
| _ -> None

let isZero (e: exp) : bool = 
  match isInteger e with 
  | None -> false
  | Some i -> Integer.equal i Integer.zero

let rec isLogicZero t = match t.term_node with
| TConst (Integer (n,_)) -> Integer.equal n Integer.zero
| TConst (LChr c) -> Char.code c = 0
| TCastE(_, t) -> isLogicZero t
| _ -> false

let isLogicNull t =
  isLogicZero t ||
    (let rec aux t = match t.term_node with
    | Tnull -> true
    | TCastE(_, t) -> aux t
    | _ -> false
     in aux t)

let parseIntAux (str:string) =
  let hasSuffix str =
    let l = String.length str in
    fun s ->
      let ls = String.length s in
      l >= ls && s = String.uppercase (String.sub str (l - ls) ls)
  in
  let l = String.length str in
  (* See if it is octal or hex or binary *)
  let octalhexbin = l >= 1 && str.[0] = '0' in
  (* The length of the suffix and a list of possible kinds. See ISO
   * 6.4.4.1 *)
  let hasSuffix = hasSuffix str in
  let suffixlen, kinds =
    if hasSuffix "ULL" || hasSuffix "LLU" then
      3, [IULongLong]
    else if hasSuffix "LL" then
      2, if octalhexbin then [ILongLong; IULongLong] else [ILongLong]
    else if hasSuffix "UL" || hasSuffix "LU" then
      2, [IULong; IULongLong]
    else if hasSuffix "L" then
      1, if octalhexbin then [ILong; IULong; ILongLong; IULongLong]
        else [ILong; ILongLong]
    else if hasSuffix "U" then
      1, [IUInt; IULong; IULongLong]
    else
      0, if octalhexbin || true (* !!! This is against the ISO but it
                              * is what GCC and MSVC do !!! *)
        then [IInt; IUInt; ILong; IULong; ILongLong; IULongLong]
        else [IInt; ILong; IUInt; ILongLong]
  in
  (* Convert to integer. To prevent overflow we do the arithmetic
   * on Big_int and we take care of overflow. We work only with
   * positive integers since the lexer takes care of the sign *)
  let rec toInt base (acc: Integer.t) (idx: int) : Integer.t =
    let doAcc what =
      if Integer.ge what base 
      then 
	Kernel.fatal ~current:true 
	  "Invalid digit %a in integer constant '%s' in base %a." 
	  (Integer.pretty ~hexa:false) what
	  str
	  (Integer.pretty ~hexa:false) base;
      let acc' =
        Integer.add what (Integer.mul base acc) in
      toInt base acc' (idx + 1)
    in
    if idx >= l - suffixlen then begin
      acc
    end else
      let ch = String.get str idx in
      if ch >= '0' && ch <= '9' then
        doAcc (Integer.of_int (Char.code ch - Char.code '0'))
      else if  ch >= 'a' && ch <= 'f'  then
        doAcc (Integer.of_int (10 + Char.code ch - Char.code 'a'))
      else if  ch >= 'A' && ch <= 'F'  then
        doAcc (Integer.of_int (10 + Char.code ch - Char.code 'A'))
      else
        Kernel.fatal ~current:true "Invalid integer constant: %s" str
  in
  let i =
    if octalhexbin && l >= 2 then 
      (match String.get str 1 with 
      | 'x' | 'X' (* Hexadecimal number *) -> 
        toInt Integer.small_nums.(16) Integer.zero 2
      | 'b' | 'B' ->  (* Binary number *)
        toInt Integer.small_nums.(2) Integer.zero 2
      | _ -> (* Octal number *)
	toInt Integer.small_nums.(8) Integer.zero 1)
    else
      toInt Integer.small_nums.(10) Integer.zero 0
  in
  i,kinds

let parseInt s = fst (parseIntAux s)

let parseIntLogic ~loc str = 
  let i,_= parseIntAux str in
  { term_node = TConst (Integer (i,Some str)) ; term_loc = loc;
    term_name = []; term_type = Linteger;}
  
let parseIntExp ~loc repr =
  try
    let i,kinds = parseIntAux repr in
    let rec loop = function
      | k::rest ->
        if fitsInInt k i then (* i fits in the current type. *)
          kinteger64 ~loc ~repr ~kind:k i
        else loop rest
      | [] ->
        Kernel.fatal ~source:(fst loc) "Cannot represent the integer %s" repr
    in
    loop kinds
  with Failure "" as e ->
    Kernel.warning "int_of_string %s (%s)\n" repr (Printexc.to_string e);
    zero ~loc

 let mkStmtCfg ~before ~(new_stmtkind:stmtkind) ~(ref_stmt:stmt) : stmt =
   let new_ = { skind = new_stmtkind;
		labels = [];
		sid = -1; succs = []; preds = []; ghost = false }
   in
   new_.sid <- Sid.next ();
   if before then begin
     new_.succs <- [ref_stmt];
     let old_preds = ref_stmt.preds in
     ref_stmt.preds <- [new_];
     new_.preds <- old_preds;
     List.iter
       (fun pred_stmt ->
	  pred_stmt.succs <-
	    (List.map
	       (fun a_succ -> if a_succ.sid = ref_stmt.sid then new_ else a_succ)
	       pred_stmt.succs))
       old_preds
   end else begin
     let old_succs = ref_stmt.succs in
     ref_stmt.succs <- [new_];
     new_.preds <- [ref_stmt];
     new_.succs <- old_succs;
     List.iter
       (fun succ_stmt ->
	  succ_stmt.preds <-
	    (List.map
	       (fun a_pred -> if a_pred.sid = ref_stmt.sid then new_ else a_pred)
	       succ_stmt.preds))
       old_succs
   end;
   new_

 let mkStmtCfgBlock sl =
   let sid = Sid.next () in
   let n = mkStmt (Block (mkBlock sl)) in
   n.sid <- sid;
   match sl with
     | [] -> n
     | s::_ ->
	 let old_preds = s.preds in
	 n.succs <- [s];
	 n.preds <- s.preds;
	 List.iter
	   (fun pred_stmt ->
	      pred_stmt.succs <-
		(List.map
		   (fun a_succ -> if a_succ.sid = s.sid then
		      n
		    else a_succ)
		   pred_stmt.succs))
	   old_preds;
	 n

 let mkEmptyStmt ?ghost ?valid_sid ?(loc=Location.unknown) () =
   mkStmt ?ghost ?valid_sid (Instr (Skip loc))

 let mkStmtOneInstr ?ghost ?valid_sid i = mkStmt ?ghost ?valid_sid (Instr i)

 let dummyInstr = Asm([], ["dummy statement!!"], [], [], [], [], Location.unknown)
 let dummyStmt = mkStmt (Instr dummyInstr)

 let rec unrollTypeDeep (t: typ) : typ =
   let rec withAttrs (al: attributes) (t: typ) : typ =
     match t with
       TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
     | TPtr(t, a') -> TPtr(unrollTypeDeep t, addAttributes al a')
     | TArray(t, l, s, a') ->
         let att_elt, att_typ = splitArrayAttributes al in
         TArray(arrayPushAttributes att_elt (unrollTypeDeep t), l, s,
                addAttributes att_typ a')
     | TFun(rt, args, isva, a') ->
	 TFun (unrollTypeDeep rt,
	       (match args with
		 None -> None
	       | Some argl ->
		   Some (List.map (fun (an,at,aa) ->
		   (an, unrollTypeDeep at, aa)) argl)),
	       isva,
	       addAttributes al a')
     | x -> typeAddAttributes al x
   in
   withAttrs [] t

 let isSignedInteger ty =
   match unrollTypeSkel ty with
   | TInt(ik,_) | TEnum ({ekind=ik},_) -> isSigned ik
   | _ -> false

 let isUnsignedInteger ty =
   match unrollTypeSkel ty with
   | TInt(ik,_) | TEnum ({ekind=ik},_) -> not (isSigned ik)
   | _ -> false

 let var vi : lval = (Var vi, NoOffset)
 (* let assign vi e = Cil_datatype.Instrs(Set (var vi, e), lu) *)

 let evar ?(loc=Location.unknown) vi = new_exp ~loc (Lval (var vi))

 let mkString ~loc s = new_exp ~loc (Const(CStr s))

 let mkWhile ~(guard:exp) ~(body: stmt list) : stmt list =
  (* Do it like this so that the pretty printer recognizes it *)
  [ mkStmt ~valid_sid:true
      (Loop ([],
	     mkBlock
	       (mkStmt ~valid_sid:true
		  (If(guard,
                      mkBlock [],
                      mkBlock [ mkStmt (Break guard.eloc)], guard.eloc)) ::
                  body), guard.eloc, None, None)) ]

 let mkFor ~(start: stmt list) ~(guard: exp) ~(next: stmt list)
	   ~(body: stmt list) : stmt list =
   (start @
      (mkWhile guard (body @ next)))

 let mkForIncr ~(iter : varinfo) ~(first: exp) ~(stopat: exp) ~(incr: exp)
     ~(body: stmt list) : stmt list =
  (* See what kind of operator we need *)
   let nextop = match unrollTypeSkel iter.vtype with
     | TPtr _ -> PlusPI
     | _ -> PlusA
   in
   mkFor
     [ mkStmtOneInstr ~valid_sid:true (Set (var iter, first, first.eloc)) ]
     (new_exp ~loc:stopat.eloc (BinOp(Lt, evar iter, stopat, intType)))
     [ mkStmtOneInstr ~valid_sid:true
         (Set
            (var iter,
             (new_exp ~loc:incr.eloc
                (BinOp(nextop, evar iter, incr, iter.vtype))),
             incr.eloc))]
     body

 let block_from_unspecified_sequence us =
   { battrs = []; bstmts = List.map (fun (x,_,_,_,_) ->x) us; blocals = [] }

 let rec stripCasts (e: exp) =
   match e.enode with CastE(_, e') -> stripCasts e' | _ -> e

 let rec stripCastsAndInfo (e: exp) =
   match e.enode with Info(e',_) | CastE(_,e') -> stripCastsAndInfo e' | _ -> e

 let rec stripCastsButLastInfo (e: exp) =
   match e.enode with
       Info({enode = (Info _ | CastE _)} as e',_)
     | CastE(_,e') ->
	 stripCastsButLastInfo e'
     | _ -> e

 let rec stripTermCasts (t: term) =
   match t.term_node with TCastE(_, t') -> stripTermCasts t' | _ -> t

let exp_info_of_term t = { exp_type = t.term_type; exp_name = t.term_name;}

let term_of_exp_info loc tnode einfo =
  {
    term_node = tnode; term_loc = loc;
    term_type = einfo.exp_type; term_name = einfo.exp_name;
  }

let map_under_info f e = match e.enode with
  | Info(e,einfo) -> new_exp ~loc:e.eloc (Info(f e,einfo))
  | _ -> f e

 let app_under_info f e = match e.enode with
   | Info(e,_) -> f e
   | _ -> f e

 (* Separate out the storage-modifier name attributes *)
 let separateStorageModifiers (al: attribute list) =
   let isstoragemod (Attr(an, _) | AttrAnnot an : attribute) : bool =
     try
       match Hashtbl.find attributeHash an with
	 AttrName issm -> issm
       | _ -> false
     with Not_found -> false
   in
     let stom, rest = List.partition isstoragemod al in
     if not (msvcMode ()) then stom, rest
     else
       (* Put back the declspec. Put it without the leading __ since these will
	* be added later *)
       let stom' =
	 List.map
	   (function
	    | Attr(an, args) -> Attr("declspec", [ACons(an, args)])
	    | AttrAnnot _ -> assert false)
	   stom
       in
       stom', rest

 let isVoidType t =
   match unrollTypeSkel t with
     TVoid _ -> true
   | _ -> false
 let isVoidPtrType t =
   match unrollTypeSkel t with
     TPtr(tau,_) when isVoidType tau -> true
   | _ -> false

 let isCharType t =
   match unrollTypeSkel t with
     | TInt((IChar|ISChar|IUChar),_) -> true
     | _ -> false

let isShortType t =
  match unrollTypeSkel t with
    | TInt((IUShort|IShort),_) -> true
    | _ -> false

let isCharPtrType t =
  match unrollTypeSkel t with
    TPtr(tau,_) when isCharType tau -> true
  | _ -> false

 let isIntegralType t =
   match unrollTypeSkel t with
     (TInt _ | TEnum _) -> true
   | _ -> false

 let isIntegralOrPointerType t =
   match unrollTypeSkel t with
   | TInt _ | TEnum _ | TPtr _ -> true
   | _ -> false

 let isLogicIntegralType t =
   match t with
     | Ctype t -> isIntegralType t
     | Linteger -> true
     | Lreal -> false
     | Lvar _ | Ltype _ | Larrow _ -> false

 let isFloatingType t =
   match unrollTypeSkel t with
     TFloat _ -> true
   | _ -> false

 let isLogicFloatType t =
   match t with
     | Ctype t -> isFloatingType t
     | Linteger -> false
     | Lreal -> false
     | Lvar _ | Ltype _ | Larrow _ -> false

 let isLogicRealOrFloatType t =
   match t with
     | Ctype t -> isFloatingType t
     | Linteger -> false
     | Lreal -> true
     | Lvar _ | Ltype _ | Larrow _ -> false

 let isLogicRealType t =
   match t with
     | Ctype _ -> false
     | Linteger -> false
     | Lreal -> true
     | Lvar _ | Ltype _ | Larrow _ -> false

 let isArithmeticType t =
   match unrollTypeSkel t with
     (TInt _ | TEnum _ | TFloat _) -> true
   | _ -> false

 let isArithmeticOrPointerType t= 
   match unrollTypeSkel t with
   | TInt _ | TEnum _ | TFloat _ | TPtr _ -> true
   | _ -> false

 let isLogicArithmeticType t =
   match t with
     | Ctype t -> isArithmeticType t
     | Linteger | Lreal -> true
     | Lvar _ | Ltype _ | Larrow _ -> false

 let isPointerType t =
   match unrollTypeSkel t with
     TPtr _ -> true
   | _ -> false

 let isTypeTagType t =
   match t with
       Ltype({lt_name = "typetag"},[]) -> true
     | _ -> false

 let getReturnType t =
   match unrollType t with
     | TFun(rt,_,_,_) -> rt
     | _ -> Kernel.fatal "getReturnType: not a function type"

 let setReturnTypeVI (v: varinfo) (t: typ) =
   match unrollType v.vtype with
     | TFun (_, args, va, a) ->
	 v.vtype <- TFun (t, args, va, a)
     | _ -> Kernel.fatal "setReturnType: not a function type"

 let setReturnType (f:fundec) (t:typ) =
   setReturnTypeVI f.svar t

 (** Returns the type pointed by the given type. Asserts it is a pointer type *)
 let typeOf_pointed typ =
   match unrollType typ with
   | TPtr (typ,_) -> typ
   | _ -> assert false

 (** Returns the type of the elements of the array. Asserts it is an array type
 *)
 let typeOf_array_elem t =
   match unrollType t with
   | TArray (ty_elem, _, _, _) -> ty_elem
   | _ -> Kernel.fatal "Not an array type %a" !pp_typ_ref t

 (**** Compute the type of an expression ****)
 let rec typeOf (e: exp) : typ =
   match (stripInfo e).enode with
   | Info _ -> assert false
   | Const(CInt64 (_, ik, _)) -> TInt(ik, [])

     (* Character constants have type int.  ISO/IEC 9899:1999 (E),
      * section 6.4.4.4 [Character constants], paragraph 10, if you
      * don't believe me. *)
   | Const(CChr _) -> intType

     (* The type of a string is a pointer to characters ! The only case when
      * you would want it to be an array is as an argument to sizeof, but we
      * have SizeOfStr for that *)
   | Const(CStr _s) -> theMachine.stringLiteralType

   | Const(CWStr _s) -> TPtr(theMachine.wcharType,[])

   | Const(CReal (_, fk, _)) -> TFloat(fk, [])

   | Const(CEnum {eival=v}) -> typeOf v

      (* l-values used as r-values lose their qualifiers (C99 6.3.2.1:2) *)
   | Lval(lv) -> type_remove_qualifier_attributes (typeOfLval lv)

   | SizeOf _ | SizeOfE _ | SizeOfStr _ -> theMachine.typeOfSizeOf
   | AlignOf _ | AlignOfE _ -> theMachine.typeOfSizeOf
   | UnOp (_, _, t) -> t
   | BinOp (_, _, _, t) -> t
   | CastE (t, _) -> t
   | AddrOf (lv) -> TPtr(typeOfLval lv, [])
   | StartOf (lv) ->
     match unrollType (typeOfLval lv) with
     | TArray (t,_,_, _) -> TPtr(t, [])
     | _ ->  Kernel.fatal ~current:true "typeOf: StartOf on a non-array"

 and typeOfInit (i: init) : typ =
   match i with
     SingleInit e -> typeOf e
   | CompoundInit (t, _) -> t

 and typeOfLval = function
     Var vi, off -> typeOffset vi.vtype off
   | Mem addr, off -> begin
       match unrollType (typeOf addr) with
       | TPtr (t, _) -> typeOffset t off
       | _ -> Kernel.fatal ~current:true
	 "typeOfLval: Mem on a non-pointer (%a)" !pp_exp_ref addr
   end

 and typeOfLhost = function
   | Var x -> x.vtype
   | Mem e -> typeOf_pointed (typeOf e)

 and typeOffset basetyp = function
     NoOffset -> basetyp
   | Index (_, o) -> begin
       match unrollType basetyp with
	 TArray (t, _, _, _baseAttrs) ->
           typeOffset t o
       | _ -> Kernel.fatal ~current:true "typeOffset: Index on a non-array"
   end
   | Field (fi, o) ->
       match unrollType basetyp with
	 TComp (_, _,baseAttrs) ->
	   let fieldType = typeOffset fi.ftype o in
           let attrs = filter_qualifier_attributes baseAttrs in
	   typeAddAttributes attrs fieldType
       | basetyp -> 
	 Kernel.fatal ~current:true
	   "typeOffset: Field %s on a non-compound type '%a'"
	   fi.fname !pp_typ_ref basetyp

 (**** Compute the type of a term lval ****)
 let rec typeOfTermLval = function
     TVar vi, off ->
       let ty = match vi.lv_origin with
	 | Some v -> Ctype v.vtype
	 | None -> vi.lv_type
       in
       typeTermOffset ty off
   | TResult ty, off -> typeTermOffset (Ctype ty) off
   | TMem addr, off -> begin
     let type_of_pointed t = 
       match t with
	 | Ctype typ ->
	     begin match unrollType typ with
		 TPtr (t, _) -> typeTermOffset (Ctype t) off
	       | _ -> 
		 Kernel.fatal ~current:true
		   "typeOfTermLval: Mem on a non-pointer"
	     end
	 | Linteger | Lreal -> 
	   Kernel.fatal ~current:true "typeOfTermLval: Mem on a logic type"
	 | Ltype (s,_) -> 
           Kernel.fatal ~current:true
	     "typeOfTermLval: Mem on a non-C type (%s)" s.lt_name
	 | Lvar s -> 
	   Kernel.fatal ~current:true
	     "typeOfTermLval: Mem on a non-C type ('%s)" s
	 | Larrow _ -> 
	   Kernel.fatal ~current:true
	     "typeOfTermLval: Mem on a function type"
     in
     Logic_const.transform_element type_of_pointed addr.term_type
   end

 and typeTermOffset basetyp =
   let blendAttributes baseAttrs t =
     let (_, _, contageous) =
       partitionAttributes ~default:(AttrName false) baseAttrs in
     let putAttributes =
       function
         | Ctype typ ->
	   Ctype (typeAddAttributes contageous typ)
         | Linteger | Lreal -> 
           Kernel.fatal ~current:true
	     "typeTermOffset: Attribute on a logic type"
         | Ltype (s,_) -> 
           Kernel.fatal ~current:true
	     "typeTermOffset: Attribute on a non-C type (%s)" s.lt_name
         | Lvar s -> 
	   Kernel.fatal ~current:true
	     "typeTermOffset: Attribute on a non-C type ('%s)" s
         | Larrow _ -> 
	   Kernel.fatal ~current:true
	     "typeTermOffset: Attribute on a function type"
     in
     Logic_const.transform_element putAttributes t
   in
   function
     | TNoOffset -> basetyp
     | TIndex (e, o) -> begin
       let elt_type basetyp =
         match basetyp with
	   | Ctype typ ->
	     begin match unrollType typ with
	         TArray (t, _, _, baseAttrs) ->
		   let elementType = typeTermOffset (Ctype t) o in
	           blendAttributes baseAttrs elementType
	       | _ -> 
		 Kernel.fatal ~current:true
		   "typeTermOffset: Index on a non-array"
	     end
	   | Linteger | Lreal -> Kernel.fatal ~current:true "typeTermOffset: Index on a logic type"
	   | Ltype (s,_) -> 
             Kernel.fatal ~current:true "typeTermOffset: Index on a non-C type (%s)" s.lt_name
	   | Lvar s -> Kernel.fatal ~current:true "typeTermOffset: Index on a non-C type ('%s)" s
	   | Larrow _ -> Kernel.fatal ~current:true "typeTermOffset: Index on a function type"
       in
       Logic_const.set_conversion 
         (Logic_const.transform_element elt_type basetyp) e.term_type
     end
     | TModel (m,o) -> typeTermOffset m.mi_field_type o
     | TField (fi, o) ->
       let elt_type basetyp =
         match basetyp with
	   | Ctype typ ->
	     begin match unrollType typ with
	         TComp (_, _, baseAttrs) ->
		   let fieldType = typeTermOffset (Ctype fi.ftype) o in
		   blendAttributes baseAttrs fieldType
	       | _ ->  Kernel.fatal ~current:true "typeTermOffset: Field on a non-compound"
	     end
	   | Linteger | Lreal -> Kernel.fatal ~current:true "typeTermOffset: Field on a logic type"
	   | Ltype (s,_) ->
             Kernel.fatal ~current:true "typeTermOffset: Field on a non-C type (%s)" s.lt_name
	   | Lvar s ->  Kernel.fatal ~current:true "typeTermOffset: Field on a non-C type ('%s)" s
	   | Larrow _ -> Kernel.fatal ~current:true "typeTermOffset: Field on a function type"
       in Logic_const.transform_element elt_type basetyp

 (**** Look for the presence of an attribute in a type ****)

 let typeHasAttribute attr typ = hasAttribute attr (typeAttrs typ)

 let rec typeHasQualifier attr typ =
   match typ with
   | TNamed (t, a) ->
     hasAttribute attr a || typeHasQualifier attr t.ttype
   | TArray (t, _, _, a) ->
     typeHasQualifier attr t || (* ill-formed type *) hasAttribute attr a
   | _ -> hasAttribute attr (typeAttrs typ)

 let typeHasAttributeDeep a (ty:typ): bool =
   let f attrs = if hasAttribute a attrs then raise Exit in
   let rec visit (t: typ) : unit =
    match t with
      | TNamed (r, a') -> f a' ; visit r.ttype
      | TArray(t, _, _, a') -> f a'; visit t
      | TComp (comp, _, a') -> f a';
	  List.iter (fun fi -> f fi.fattr; visit fi.ftype) comp.cfields
      | TVoid a'
      | TInt (_, a')
      | TFloat (_, a')	  
      | TEnum (_, a')
      | TFun (_, _, _, a')
      | TBuiltin_va_list a'
      | TPtr(_, a') -> f a'
   in
   try visit ty; false
   with Exit -> true

 
 (**
  **
  ** MACHINE DEPENDENT PART
  **
  **)
 exception SizeOfError of string * typ
 let find_size_in_cache s f =
   match s.scache with
   | Not_Computed ->
       let r =
	 try
	   f ()
	 with SizeOfError (msg, typ) as e ->
	   s.scache <- Not_Computable (msg, typ);
	   raise e
       in
       s.scache <- Computed r;
       r
   | Not_Computable (msg, typ) -> raise (SizeOfError (msg, typ))
   | Computed r -> r


(* Some basic type utilities *)
 let rank : ikind -> int = function
   (* these are just unique numbers representing the integer
      conversion rank. *)
   | IBool | IChar | ISChar | IUChar -> 1
   | IShort | IUShort -> 2
   | IInt | IUInt -> 3
   | ILong | IULong -> 4
   | ILongLong | IULongLong -> 5

 let unsignedVersionOf (ik:ikind): ikind =
   match ik with
   | ISChar | IChar -> IUChar
   | IShort -> IUShort
   | IInt -> IUInt
   | ILong -> IULong
   | ILongLong -> IULongLong
   | _ -> ik

 let frank = function
   | FFloat -> 1
   | FDouble -> 2
   | FLongDouble -> 3


 (* Convert 2 integer constants to integers with the same type, in preparation
    for a binary operation.   See ISO C 6.3.1.8p1 *)
 let convertInts i1 ik1 i2 ik2 =
   if ik1 = ik2 then (* nothing to do *)
     i1, i2, ik1
   else begin
     let r1 = rank ik1 in
     let r2 = rank ik2 in
     let ik' =
       if (isSigned ik1) = (isSigned ik2) then begin
	 (* Both signed or both unsigned. *)
	 if r1 > r2 then ik1 else ik2
       end
       else begin
	 let signedKind, unsignedKind, signedRank, unsignedRank =
	   if isSigned ik1 then ik1, ik2, r1, r2 else ik2, ik1, r2, r1
	 in
	 (* The rules for signed + unsigned get hairy.
	    (unsigned short + long) is converted to signed long,
	    but (unsigned int + long) is converted to unsigned long.*)
	 if unsignedRank >= signedRank then unsignedKind
	 else if (bytesSizeOfInt signedKind) > (bytesSizeOfInt unsignedKind) then
	   signedKind
	 else
	   unsignedVersionOf signedKind
       end
     in
     let i1',_ = truncateInteger64 ik' i1 in
     let i2',_ = truncateInteger64 ik' i2 in
     i1', i2', ik'
   end

(* Local type to compute alignments of struct field. *)
 type offsetAcc =
     { oaFirstFree: int;        (* The first free bit *)
       oaLastFieldStart: int;   (* Where the previous field started *)
       oaLastFieldWidth: int;   (* The width of the previous field. Might not
				 * be same as FirstFree - FieldStart because
				 * of internal padding *)
       oaPrevBitPack: (int * ikind * int) option; (* If the previous fields
						    * were packed bitfields,
						    * the bit where packing
						    * has started, the ikind
						    * of the bitfield and the
						    * width of the ikind *)
     }



(* Hack to prevent infinite recursion in alignments *)
let ignoreAlignmentAttrs = ref false

 (* Get the minimum aligment in bytes for a given type *)
let rec bytesAlignOf t = 
  let alignOfType () = match t with 
  | TInt((IChar|ISChar|IUChar|IBool), _) -> 1
  | TInt((IShort|IUShort), _) -> theMachine.theMachine.alignof_short
  | TInt((IInt|IUInt), _) -> theMachine.theMachine.alignof_int
  | TInt((ILong|IULong), _) -> theMachine.theMachine.alignof_long
  | TInt((ILongLong|IULongLong), _) ->
    theMachine.theMachine.alignof_longlong
  | TEnum (ei,_) ->  bytesAlignOf (TInt(ei.ekind, []))
  | TFloat(FFloat, _) -> theMachine.theMachine.alignof_float
  | TFloat(FDouble, _) -> theMachine.theMachine.alignof_double
  | TFloat(FLongDouble, _) ->
    theMachine.theMachine.alignof_longdouble
  | TNamed (t, _) -> bytesAlignOf t.ttype
  | TArray (t, _, _, _) -> bytesAlignOf t
  | TPtr _ | TBuiltin_va_list _ ->
    theMachine.theMachine.alignof_ptr

  (* For composite types get the maximum alignment of any field inside *)
  | TComp (c, _, _) ->
    (* On GCC the zero-width fields do not contribute to the alignment. On
     * MSVC only those zero-width that _do_ appear after other
     * bitfields contribute to the alignment. So we drop those that
     * do not occur after othe bitfields *)
    (* This is not correct for Diab-C compiler. *)
    let rec dropZeros (afterbitfield: bool) = function
      | f :: rest when f.fbitfield = Some 0 && not afterbitfield ->
	dropZeros afterbitfield rest
      | f :: rest -> f :: dropZeros (f.fbitfield <> None) rest
      | [] -> []
    in
    let fields = dropZeros false c.cfields in
    List.fold_left
      (fun sofar f ->
	(* Bitfields with zero width do not contribute to the alignment in
	 * GCC *)
	if not (msvcMode ()) && f.fbitfield = Some 0 then sofar else
	  max sofar (alignOfField f)) 1 fields
  (* These are some error cases *)
  | TFun _ when not (msvcMode ()) ->
    theMachine.theMachine.alignof_fun
  | TFun _ as t -> raise (SizeOfError ("Undefined sizeof on a function.", t))
  | TVoid _ as t -> raise (SizeOfError ("Undefined sizeof(void).", t))
  in
  process_aligned_attribute
    (fun fmt -> !pp_typ_ref fmt t)
    (typeAttrs t) alignOfType

(* alignment of a possibly-packed or aligned struct field. *)
and alignOfField (fi: fieldinfo) =
  let fieldIsPacked = hasAttribute "packed" fi.fattr 
    || hasAttribute "packed" fi.fcomp.cattr
  in
  if fieldIsPacked then begin
    if hasAttribute "aligned" fi.fattr then
      Kernel.warning
	"packed attribute overrules aligned attributes for file %s"
	fi.fname ;
    1
  end else
    process_aligned_attribute
      (fun fmt -> Format.fprintf fmt "field %s" fi.fname)
      fi.fattr
      (fun () -> bytesAlignOf fi.ftype)
    
and intOfAttrparam (a:attrparam) : int option =
  let rec doit a : int =
    match a with
    |  AInt(n) -> Integer.to_int n
    | ABinOp(Shiftlt, a1, a2) -> (doit a1) lsl (doit a2)
    | ABinOp(Div, a1, a2) -> (doit a1) / (doit a2)
    | ASizeOf(t) ->
      let bs = bitsSizeOf t in
      bs / 8
    | AAlignOf(t) ->
      bytesAlignOf t
    | _ -> raise (SizeOfError ("Cannot convert an attribute to int.", voidType))
  in
  (* Use ignoreAlignmentAttrs here to prevent stack overflow if a buggy
     program does something like 
     struct s {...} __attribute__((aligned(sizeof(struct s))))
     This is too conservative, but it's often enough.
  *)
  assert (not !ignoreAlignmentAttrs);
  ignoreAlignmentAttrs := true;
  try
    let n = doit a in
    ignoreAlignmentAttrs := false;
    Some n
  with Failure _ | SizeOfError _ -> (* Can't compile *)
    ignoreAlignmentAttrs := false;
    None
and process_aligned_attribute (pp:Format.formatter->unit) attrs default_align = 
  match filterAttributes "aligned" attrs with
  | [] -> 
      (* no __aligned__ attribute, so get the default alignment *)
      default_align ()
  | _ when !ignoreAlignmentAttrs -> 
    Kernel.warning "ignoring recursive align attributes on %t" 
      pp;
    default_align ()
  | (Attr(_, [a]) as at)::rest -> begin
    if rest <> [] then
      Kernel.warning "ignoring duplicate align attributes on %t" 
        pp;
    match intOfAttrparam a with
      Some n -> n
    | None -> 
      Kernel.warning "alignment attribute \"%a\" not understood on %t" 
        !pp_attribute_ref at pp;
      default_align ()
  end
  | Attr(_, [])::rest ->
       (* aligned with no arg means a power of two at least as large as
          any alignment on the system.*)
    if rest <> [] then
      Kernel.warning "ignoring duplicate align attributes on %t" 
        pp;
    theMachine.theMachine.alignof_aligned
  | at::_ ->
    Kernel.warning "alignment attribute \"%a\" not understood on %t" 
      !pp_attribute_ref at pp;
    default_align ()

 (* Computation of the offset of the field [fi], given the information [sofar]
    computed for the previous fields. [last] indicates that we are considering
    the last field of the struct. Set to [false] by default for unions. *)
 and offsetOfFieldAcc ?(last=false) ~(fi: fieldinfo) ~(sofar: offsetAcc) : offsetAcc =
   if msvcMode () then offsetOfFieldAcc_MSVC last fi sofar
   else offsetOfFieldAcc_GCC last fi sofar

(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
and offsetOfFieldAcc_GCC last (fi: fieldinfo) (sofar: offsetAcc) : offsetAcc =
   (* field type *)
   let ftype = unrollType fi.ftype in
   let ftypeAlign = 8 * alignOfField fi in
   let ftypeBits = (if last then bitsSizeOfEmptyArray else bitsSizeOf) ftype in
   match ftype, fi.fbitfield with
     (* A width of 0 means that we must end the current packing. It seems that
      * GCC pads only up to the alignment boundary for the type of this field.
      * *)
   | _, Some 0 ->
       let firstFree      = addTrailing sofar.oaFirstFree ftypeAlign in
       { oaFirstFree      = firstFree;
	 oaLastFieldStart = firstFree;
	 oaLastFieldWidth = 0;
	 oaPrevBitPack    = None }

   (* A bitfield cannot span more alignment boundaries of its type than the
    * type itself *)
   | _, Some wdthis
       when (sofar.oaFirstFree + wdthis + ftypeAlign - 1) / ftypeAlign
	 - sofar.oaFirstFree / ftypeAlign > ftypeBits / ftypeAlign ->
       let start = addTrailing sofar.oaFirstFree ftypeAlign in
       { oaFirstFree      = start + wdthis;
	 oaLastFieldStart = start;
	 oaLastFieldWidth = wdthis;
	 oaPrevBitPack    = None }

   (* Try a simple method. Just put the field down *)
   | _, Some wdthis ->
       { oaFirstFree      = sofar.oaFirstFree + wdthis;
	 oaLastFieldStart = sofar.oaFirstFree;
	 oaLastFieldWidth = wdthis;
	 oaPrevBitPack    = None
       }

   (* Non-bitfield *)
   | _, None ->
       (* Align this field *)
       let newStart = addTrailing sofar.oaFirstFree ftypeAlign  in
       { oaFirstFree = newStart + ftypeBits;
	 oaLastFieldStart = newStart;
	 oaLastFieldWidth = ftypeBits;
	 oaPrevBitPack = None;
       }

 (* MSVC version *)
 and offsetOfFieldAcc_MSVC last (fi: fieldinfo)
     (sofar: offsetAcc) : offsetAcc =
   (* field type *)
   let ftype = unrollType fi.ftype in
   let ftypeAlign = 8 * alignOfField fi in
   let ftypeBits = (if last then bitsSizeOfEmptyArray else bitsSizeOf) ftype in
   match ftype, fi.fbitfield, sofar.oaPrevBitPack with
     (* Ignore zero-width bitfields that come after non-bitfields *)
   | TInt (_ikthis, _), Some 0, None ->
       let firstFree      = sofar.oaFirstFree in
       { oaFirstFree      = firstFree;
	 oaLastFieldStart = firstFree;
	 oaLastFieldWidth = 0;
	 oaPrevBitPack    = None }

   (* If we are in a bitpack and we see a bitfield for a type with the
    * different width than the pack, then we finish the pack and retry *)
   | _, Some _, Some (packstart, _, wdpack) when wdpack != ftypeBits ->
       let firstFree =
	 if sofar.oaFirstFree = packstart then packstart else
	   packstart + wdpack
       in
       offsetOfFieldAcc_MSVC last fi
	 { oaFirstFree      = addTrailing firstFree ftypeAlign;
	   oaLastFieldStart = sofar.oaLastFieldStart;
	   oaLastFieldWidth = sofar.oaLastFieldWidth;
	   oaPrevBitPack    = None }

   (* A width of 0 means that we must end the current packing. *)
   | TInt (ikthis, _), Some 0, Some (packstart, _, wdpack) ->
       let firstFree =
	 if sofar.oaFirstFree = packstart then packstart else
	   packstart + wdpack
       in
       let firstFree      = addTrailing firstFree ftypeAlign in
       { oaFirstFree      = firstFree;
	 oaLastFieldStart = firstFree;
	 oaLastFieldWidth = 0;
	 oaPrevBitPack    = Some (firstFree, ikthis, ftypeBits) }

   (* Check for a bitfield that fits in the current pack after some other
    * bitfields *)
   | TInt(_ikthis, _), Some wdthis, Some (packstart, _ikprev, wdpack)
       when  packstart + wdpack >= sofar.oaFirstFree + wdthis ->
       { oaFirstFree = sofar.oaFirstFree + wdthis;
	 oaLastFieldStart = sofar.oaFirstFree;
	 oaLastFieldWidth = wdthis;
	 oaPrevBitPack = sofar.oaPrevBitPack
       }


   | _, _, Some (packstart, _, wdpack) -> (* Finish up the bitfield pack and
					   * restart. *)
       let firstFree =
	 if sofar.oaFirstFree = packstart then packstart else
	   packstart + wdpack
       in
       offsetOfFieldAcc_MSVC last fi
	 { oaFirstFree      = addTrailing firstFree ftypeAlign;
	   oaLastFieldStart = sofar.oaLastFieldStart;
	   oaLastFieldWidth = sofar.oaLastFieldWidth;
	   oaPrevBitPack    = None }

   (* No active bitfield pack. But we are seeing a bitfield. *)
   | TInt(ikthis, _), Some wdthis, None ->
       let firstFree     = addTrailing sofar.oaFirstFree ftypeAlign in
       { oaFirstFree     = firstFree + wdthis;
	 oaLastFieldStart = firstFree;
	 oaLastFieldWidth = wdthis;
	 oaPrevBitPack = Some (firstFree, ikthis, ftypeBits); }

   (* No active bitfield pack. Non-bitfield *)
   | _, None, None ->
       (* Align this field *)
       let firstFree = addTrailing sofar.oaFirstFree ftypeAlign  in
       { oaFirstFree = firstFree + ftypeBits;
	 oaLastFieldStart = firstFree;
	 oaLastFieldWidth = ftypeBits;
	 oaPrevBitPack = None;
       }

   | _, Some _, None -> Kernel.fatal ~current:true "offsetAcc"

 (** This is a special version of [bitsSizeOf] that accepts empty arrays.
     Currently, we only use it for flexible array members *)
 and bitsSizeOfEmptyArray typ =
  match unrollType typ with
  | TArray (_, None, _, _) -> 0
  | TArray (_, Some e, _, _) -> begin
    match constFoldToInt e with
    | Some i when Integer.is_zero i ->
      (* GCC extension. Cabs2Cil currently rewrites all such toplevel arrays as
         having size 1. Hence this case can only appear for arrays within
         structures *)
      0
    | _ -> bitsSizeOf typ
  end
  | _ -> bitsSizeOf typ

 (* The size of a type, in bits. If struct or array then trailing padding is
  * added *)
 and bitsSizeOf t =
   match t with
   | TInt (ik,_) -> 8 * (bytesSizeOfInt ik)
   | TFloat(FDouble, _) -> 8 * theMachine.theMachine.sizeof_double
   | TFloat(FLongDouble, _) ->
       8 * theMachine.theMachine.sizeof_longdouble
   | TFloat _ -> 8 * theMachine.theMachine.sizeof_float
   | TEnum (ei,_) -> bitsSizeOf (TInt(ei.ekind, []))
   | TPtr _ -> 8 * theMachine.theMachine.sizeof_ptr
   | TBuiltin_va_list _ -> 8 * theMachine.theMachine.sizeof_ptr
   | TNamed (t, _) -> bitsSizeOf t.ttype
   | TComp (comp, scache, _) when comp.cfields == [] ->
       find_size_in_cache
	 scache
	 (fun () -> begin
	    (* Empty structs are allowed in msvc mode *)
	    if not comp.cdefined && not (msvcMode ()) then begin
              raise
		(SizeOfError
		   (Format.sprintf "abstract type '%s'" (compFullName comp), t))
	    end else
	      0
	  end)

   | TComp (comp, scache, _) when comp.cstruct -> (* Struct *)
       find_size_in_cache
	 scache
	 (fun () ->
	    (* Go and get the last offset *)
	    let startAcc =
	      { oaFirstFree = 0;
		oaLastFieldStart = 0;
		oaLastFieldWidth = 0;
		oaPrevBitPack = None;
	      } in
	    let lastoff =
	      fold_struct_fields
                (fun ~last acc fi -> offsetOfFieldAcc ~last ~fi ~sofar:acc)
		startAcc comp.cfields
	    in
	    if msvcMode () && lastoff.oaFirstFree = 0 && comp.cfields <> []
	    then
	      (* On MSVC if we have just a zero-width bitfields then the length
	       * is 32 and is not padded  *)
	      32
	    else
	      addTrailing lastoff.oaFirstFree (8 * bytesAlignOf t))

   | TComp (comp, scache, _) -> (* Union *)
       find_size_in_cache
	 scache
	 (fun () ->
	    (* Get the maximum of all fields *)
	    let startAcc =
	      { oaFirstFree = 0;
		oaLastFieldStart = 0;
		oaLastFieldWidth = 0;
		oaPrevBitPack = None;
	      } in
	    let max =
	      List.fold_left (fun acc fi ->
		let lastoff = offsetOfFieldAcc ?last:None ~fi ~sofar:startAcc in
		if lastoff.oaFirstFree > acc then
		  lastoff.oaFirstFree else acc) 0 comp.cfields in
	    (* Add trailing by simulating adding an extra field *)
	    addTrailing max (8 * bytesAlignOf t))

   | TArray(bt, Some len, scache, _) ->
       find_size_in_cache
	 scache
	 (fun () ->
	    begin
	      match (constFold true len).enode with
		Const(CInt64(l,_,_)) ->
		  let sz = Integer.mul (Integer.of_int (bitsSizeOf bt)) l in
		  let sz' = try 
                              Integer.to_int sz 
                    with Failure "to_int" -> 
		      raise 
                        (SizeOfError ("Array is so long that its size can't be "
				      ^"represented with an OCaml int.", t))

                  in
		  sz' (*WAS: addTrailing sz' (8 * bytesAlignOf t)*)
	      | _ -> raise (SizeOfError ("Array with non-constant length.", t))
	    end)
   | TVoid _ -> 8 * theMachine.theMachine.sizeof_void
   | TFun _ ->
       if not (msvcMode ()) then
         (* On GCC the size of a function is defined *)
         8 * theMachine.theMachine.sizeof_fun
       else
         raise (SizeOfError ("Undefined sizeof on a function.", t))

   | TArray (_, None, _, _) ->
       raise (SizeOfError ("Size of array without number of elements.", t))

 (* Iterator on the fields of a structure, with additional information about
    having reached the last field (for flexible member arrays) *)
 and fold_struct_fields f acc l = match l with
   | [] -> acc
   | [fi_last] -> f ~last:true acc fi_last
   | fi :: (_ :: _ as q) -> fold_struct_fields f (f ~last:false acc fi) q

 and addTrailing nrbits roundto =
   (nrbits + roundto - 1) land (lnot (roundto - 1))

and bytesSizeOf t = (bitsSizeOf t) lsr 3

and sizeOf ~loc t =
  try
    integer ~loc ((bitsSizeOf t) lsr 3)
  with SizeOfError _ -> new_exp ~loc (SizeOf(t))

 and bitsOffset (baset: typ) (off: offset) : int * int =
   let rec loopOff (baset: typ) (width: int) (start: int) = function
       NoOffset -> start, width
     | Index(e, off) -> begin
	 let ei =
	   match constFoldToInt e with
           | Some i -> Integer.to_int i
	   | None -> raise (SizeOfError ("Index is not constant", baset))
	 in
	 let bt = typeOf_array_elem baset in
	 let bitsbt = bitsSizeOf bt in
	 loopOff bt bitsbt (start + ei * bitsbt) off
       end
     | Field(f, off) when not f.fcomp.cstruct (* union *) ->
         if check_invariants then
           assert (match unrollType baset with
                     | TComp (ci, _, _) -> ci == f.fcomp
                     | _ -> false);
	 (* All union fields start at offset 0 *)
	 loopOff f.ftype (bitsSizeOf f.ftype) start off

     | Field(f, off) (* struct *) ->
         if check_invariants then
           assert (match unrollType baset with
                     | TComp (ci, _, _) -> ci == f.fcomp
                     | _ -> false);
         if f.foffset_in_bits = None then begin
           let aux ~last acc fi =
             let acc' = offsetOfFieldAcc ~last ~fi ~sofar:acc in
             fi.fsize_in_bits <- Some acc'.oaLastFieldWidth;
             fi.foffset_in_bits <- Some acc'.oaLastFieldStart;
             acc'
           in
           ignore (
	     fold_struct_fields aux
	       { oaFirstFree      = 0;
	         oaLastFieldStart = 0;
	         oaLastFieldWidth = 0;
	         oaPrevBitPack    = None }
               f.fcomp.cfields
           );
         end;
         let offsbits, size =
           Extlib.the f.foffset_in_bits, Extlib.the f.fsize_in_bits
         in
         loopOff f.ftype size (start + offsbits) off
   in
   loopOff baset (bitsSizeOf baset) 0 off

(** Do constant folding on an expression. If the first argument is true then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node.*)
and constFold (machdep: bool) (e: exp) : exp =
  if debugConstFold then Kernel.debug "ConstFold to %a@." !pp_exp_ref e;
  let loc = e.eloc in
  match e.enode with
    BinOp(bop, e1, e2, tres) -> constFoldBinOp ~loc machdep bop e1 e2 tres
  | UnOp(unop, e1, tres) -> begin
      try
        let tk =
          match unrollTypeSkel tres with
          | TInt(ik, _) -> ik
          | TEnum (ei,_) -> ei.ekind
          | _ -> raise Not_found (* probably a float *)
        in
        let e1c = constFold machdep e1 in
        match e1c.enode with
          Const(CInt64(i,_ik,repr)) -> begin
            match unop with
              Neg ->
                let repr = Extlib.opt_map (fun s -> "-" ^ s) repr in
                kinteger64 ~loc ?repr ~kind:tk (Integer.neg i)
            | BNot -> kinteger64 ~loc ~kind:tk (Integer.lognot i)
            | LNot -> 
              if Integer.equal i Integer.zero then one ~loc
              else zero ~loc
          end
        | _ -> if e1 == e1c then e else new_exp ~loc (UnOp(unop, e1c, tres))
      with Not_found -> e
    end
      (* Characters are integers *)
  | Const(CChr c) -> new_exp ~loc (Const(charConstToIntConstant c))
  | Const(CEnum {eival = v}) -> constFold machdep v
  | Const (CReal _ | CWStr _ | CStr _ | CInt64 _) -> e (* a constant *)
  | SizeOf t when machdep -> begin
      try
        let bs = bitsSizeOf t in
        kinteger ~loc theMachine.kindOfSizeOf (bs / 8)
      with SizeOfError _ -> e
    end
  | SizeOfE e when machdep -> constFold machdep
    (new_exp ~loc:e.eloc (SizeOf (typeOf e)))
  | SizeOfStr s when machdep ->
      kinteger ~loc theMachine.kindOfSizeOf (1 + String.length s)
  | AlignOf t when machdep ->
      kinteger ~loc theMachine.kindOfSizeOf (bytesAlignOf t)
  | AlignOfE e when machdep -> begin
      (* The alignment of an expression is not always the alignment of its
       * type. I know that for strings this is not true *)
      match e.enode with
      | Const (CStr _) when not (msvcMode ()) ->
          kinteger ~loc
            theMachine.kindOfSizeOf theMachine.theMachine.alignof_str
            (* For an array, it is the alignment of the array ! *)
      | _ -> constFold machdep (new_exp ~loc:e.eloc (AlignOf (typeOf e)))
    end
   | AlignOfE _ | AlignOf _ | SizeOfStr _ | SizeOfE _ | SizeOf _ ->
     e (* Depends on machdep. Do not evaluate in this case*)

   (* Special case to handle the C macro 'offsetof' *)
   | CastE(it,
	   { enode = AddrOf (Mem ({enode = CastE(TPtr(bt, _), z)}), off)})
       when machdep && isZero z -> begin
	 try
	   let start, _width = bitsOffset bt off in
	   if start mod 8 <> 0 then 
	     Kernel.error ~current:true "Using offset of bitfield" ;
	   constFold machdep 
             (new_exp ~loc (CastE(it, (integer ~loc (start / 8)))))
	 with SizeOfError _ -> e
       end

  | CastE (t, e) -> begin
    if debugConstFold then 
      Kernel.debug "ConstFold CAST to to %a@." !pp_typ_ref t ;
    let e = constFold machdep e in
    match e.enode, unrollType t with
      | Const(CInt64(i,_k,_)), TInt(nk,a) when a = [] ->
        begin
          (* If the cast has attributes, leave it alone. *)
          if debugConstFold then
            Kernel.debug "ConstFold to %a : %a@."
              !pp_ikind_ref nk Datatype.Integer.pretty i;
          (* Downcasts might truncate silently *)
          kinteger64 ~loc ~kind:nk i
        end
      | Const (CReal (f, _, _)), TInt (ik, a) when a = [] -> (* See above *)
        begin
          try
            let i = Floating_point.truncate_to_integer f in
            let _i', truncated = truncateInteger64 ik i in
            if truncated then (* Float is too big. Do not const-fold *)
              new_exp ~loc (CastE (t, e))
            else
              kinteger64 ~loc ~kind:ik i
          with Floating_point.Float_Non_representable_as_Int64 _ -> (* too big*)
            new_exp ~loc (CastE (t, e))
        end
      | _, _ -> new_exp ~loc (CastE (t, e))
    end
  | Lval lv -> new_exp ~loc (Lval (constFoldLval machdep lv))
  | AddrOf lv -> new_exp ~loc (AddrOf (constFoldLval machdep lv))
  | StartOf lv -> new_exp ~loc (StartOf (constFoldLval machdep lv))
  | Info _ -> e (* Deprecated constructor *)

 and constFoldLval machdep (host,offset) =
   let newhost =
     match host with
     | Mem e -> Mem (constFold machdep e)
     | Var _ -> host
   in
   let rec constFoldOffset machdep = function
     | NoOffset -> NoOffset
     | Field (fi,offset) -> Field (fi, constFoldOffset machdep offset)
     | Index (exp,offset) -> Index (constFold machdep exp,
				    constFoldOffset machdep offset)
   in
   (newhost, constFoldOffset machdep offset)

and constFoldBinOp ~loc (machdep: bool) bop e1 e2 tres =
  let e1' = constFold machdep e1 in
  let e2' = constFold machdep e2 in
  if isIntegralType tres then begin
    let newe =
      let rec mkInt e =
        let loc = e.eloc in
        match e.enode with
          | Const(CChr c) -> new_exp ~loc (Const(charConstToIntConstant c))
          | Const(CEnum {eival = v}) -> mkInt v
          | CastE(TInt (ik, ta), e) -> begin
              let exp = mkInt e in
              match exp.enode with
                  Const(CInt64(i, _, _)) ->
                    kinteger64 ~loc ~kind:ik i
                | _ -> {exp with enode = CastE(TInt(ik, ta), exp)}
            end
          | _ -> e
      in
      let tk =
        match unrollTypeSkel tres with
            TInt(ik, _) -> ik
          | TEnum (ei,_) -> ei.ekind
          | _ -> Kernel.fatal ~current:true "constFoldBinOp"
      in
      (* See if the result is unsigned *)
      let isunsigned typ = not (isSigned typ) in
      let shiftInBounds i2 =
        (* We only try to fold shifts if the second arg is positive and
           less than the size of the type of the first argument.
           Otherwise, the semantics are processor-dependent, so let the
           compiler sort it out. *)
        if machdep then
          try
            (Integer.ge i2 Integer.zero)
            && Integer.lt i2 (Integer.of_int (bitsSizeOf (typeOf e1')))
          with SizeOfError _ -> false
        else false
      in
      (* Assume that the necessary promotions have been done *)
      let e1'' = mkInt e1' in
      let e2'' = mkInt e2' in
      match bop, e1''.enode, e2''.enode with
      | PlusA, Const(CInt64(z,_,_)), _ 
        when Integer.equal z Integer.zero -> e2''
      | PlusA, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | PlusPI, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | IndexPI, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | MinusPI, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | PlusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.add i1 i2)
      | MinusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
          when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.sub i1 i2)
      | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.mul i1 i2)
      | Mult, Const(CInt64(z,_,_)), _
        when Integer.equal z Integer.zero -> zero ~loc
      | Mult, Const(CInt64(one,_,_)), _ 
        when Integer.equal one Integer.one -> e2''
      | Mult, _,    Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> zero ~loc
      | Mult, _, Const(CInt64(one,_,_)) 
        when Integer.equal one Integer.one -> e1''
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          begin
            try kinteger64 ~loc ~kind:tk (Integer.div i1 i2)
            with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
          end
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
          when bytesSizeOfInt ik1 = bytesSizeOfInt ik2 -> begin
            try kinteger64 ~loc ~kind:tk (Integer.div i1 i2)
            with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
          end
      | Div, _, Const(CInt64(one,_,_)) 
         when Integer.equal one Integer.one -> e1''
      | Mod, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          begin
            try kinteger64 ~loc ~kind:tk (Integer.rem i1 i2)
            with Division_by_zero -> new_exp ~loc (BinOp(bop, e1', e2', tres))
          end
      | BAnd, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.logand i1 i2)
      | BAnd, Const(CInt64(z,_,_)), _ 
        when Integer.equal z Integer.zero -> zero ~loc
      | BAnd, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> zero ~loc
      | BOr, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.logor i1 i2)
      | BOr, _, _ when isZero e1' -> e2'
      | BOr, _, _ when isZero e2' -> e1'
      | BXor, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc ~kind:tk (Integer.logxor i1 i2)
      | Shiftlt, Const(CInt64(i1,_ik1,_)),Const(CInt64(i2,_,_))
          when shiftInBounds i2 ->
          kinteger64 ~loc ~kind:tk (Integer.shift_left i1 i2)
      | Shiftlt, Const(CInt64(z,_,_)), _ 
        when Integer.equal z Integer.zero -> zero ~loc
      | Shiftlt, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | Shiftrt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,_,_))
          when shiftInBounds i2 ->
          if isunsigned ik1 then
            kinteger64 ~loc ~kind:tk 
              (Integer.shift_right_logical i1 i2)
          else
            kinteger64 ~loc ~kind:tk (Integer.shift_right i1 i2)
      | Shiftrt, Const(CInt64(z,_,_)), _ 
        when Integer.equal z Integer.zero -> zero ~loc
      | Shiftrt, _, Const(CInt64(z,_,_)) 
        when Integer.equal z Integer.zero -> e1''
      | Eq, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.equal i1' i2' then one ~loc else zero ~loc
      | Ne, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.equal i1' i2' then zero ~loc else one ~loc
      | Le, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.le i1' i2' then one ~loc else zero ~loc
      | Ge, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.ge i1' i2' then one ~loc else zero ~loc
      | Lt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.lt i1' i2' then one ~loc else zero ~loc
      | Gt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if Integer.gt i1' i2' then one ~loc else zero ~loc

      (* We rely on the fact that LAnd/LOr appear in global initializers
         and should not have side effects. *)
      | LAnd, _, _ when isZero e1' || isZero e2' -> zero ~loc
      | LAnd, _, _ when isInteger e1' <> None -> e2'  (* e1' is TRUE *)
      | LAnd, _, _ when isInteger e2' <> None -> e1'  (* e2' is TRUE *)
      | LOr, _, _ when isZero e1' -> e2'
      | LOr, _, _ when isZero e2' -> e1'
      | LOr, _, _ when isInteger e1' <> None || isInteger e2' <> None ->
          (* One of e1' or e2' is a nonzero constant *)
          one ~loc
      | _ -> new_exp ~loc (BinOp(bop, e1', e2', tres))
    in
    if debugConstFold then
      Format.printf "Folded %a to %a@."
        !pp_exp_ref (new_exp ~loc (BinOp(bop, e1', e2', tres)))
        !pp_exp_ref newe;
    newe
  end else
    new_exp ~loc (BinOp(bop, e1', e2', tres))

and constFoldToInt ?(machdep=true) e =
  match (constFold machdep e).enode with
  | Const(CInt64(c,_,_)) -> Some c
  | CastE (typ, e) when machdep && isPointerType typ -> begin
    (* Those casts are left left by constFold *)
    match constFoldToInt ~machdep e with
    | None -> None
    | Some i as r -> if fitsInInt theMachine.upointKind i then r else None
  end
  | _ -> None


let () = constfoldtoint := constFoldToInt ~machdep:true

let intTypeIncluded kind1 kind2 =
  let bitsize1 = bitsSizeOfInt kind1 in
  let bitsize2 = bitsSizeOfInt kind2 in
  match isSigned kind1, isSigned kind2 with
    | true, true
    | false, false -> bitsize1 <= bitsize2
    | true, false -> false
    | false, true -> bitsize1 < bitsize2


 (* CEA: moved from cabs2cil.ml. See cil.mli for infos *)
 (* Weimer
  * multi-character character constants
  * In MSCV, this code works:
  *
  * long l1 = 'abcd';  // note single quotes
  * char * s = "dcba";
  * long * lptr = ( long * )s;
  * long l2 = *lptr;
  * assert(l1 == l2);
  *
  * We need to change a multi-character character literal into the
  * appropriate integer constant. However, the plot sickens: we
  * must also be able to handle things like 'ab\nd' (value = * "d\nba")
  * and 'abc' (vale = *"cba").
  *
  * First we convert 'AB\nD' into the list [ 65 ; 66 ; 10 ; 68 ], then we
  * multiply and add to get the desired value.
  *)

 (* Given a character constant (like 'a' or 'abc') as a list of 64-bit
  * values, turn it into a CIL constant.  Multi-character constants are
  * treated as multi-digit numbers with radix given by the bit width of
  * the specified type (either char or wchar_t). *)
 let reduce_multichar typ : int64 list -> int64 =
   let radix = bitsSizeOf typ in
   List.fold_left
     (fun acc -> Int64.add (Int64.shift_left acc radix))
     Int64.zero

 let interpret_character_constant char_list =
   let value = reduce_multichar charType char_list in
   if value < (Int64.of_int 256) then
     (* ISO C 6.4.4.4.10: single-character constants have type int *)
     (CChr(Char.chr (Int64.to_int value))), intType
   else begin
     let orig_rep = None (* Some("'" ^ (String.escaped str) ^ "'") *) in
     if value <= (Int64.of_int32 Int32.max_int) then
       (CInt64(Integer.of_int64 value,IULong,orig_rep)),(TInt(IULong,[]))
     else
       (CInt64(Integer.of_int64 value,IULongLong,orig_rep)),(TInt(IULongLong,[]))
   end

 let invalidStmt = mkStmt (Instr (Skip Location.unknown))
 module Frama_c_builtins =
   State_builder.Hashtbl
   (Datatype.String.Hashtbl)
   (Cil_datatype.Varinfo)
   (struct
     let name = "Cil.Frama_c_Builtins"
     let dependencies = []
     let size = 3
    end)

 let () = dependency_on_ast Frama_c_builtins.self

 let is_builtin v = hasAttribute "FC_BUILTIN" v.vattr

 let is_unused_builtin v = is_builtin v && not v.vreferenced


(* [VP] Should we projectify this ?*)
let special_builtins_table = ref Datatype.String.Set.empty
let special_builtins = Queue.create ()

let is_special_builtin s =
  Queue.fold (fun res f -> res || f s) false special_builtins

let add_special_builtin_family f = Queue.add f special_builtins

let add_special_builtin s =
  special_builtins_table := Datatype.String.Set.add s !special_builtins_table

let () = add_special_builtin_family
  (fun s -> Datatype.String.Set.mem s !special_builtins_table)

let () = List.iter add_special_builtin
  [ "__builtin_stdarg_start"; "__builtin_va_arg";
    "__builtin_va_start"; "__builtin_expect"; "__builtin_next_arg"; ]

 module Builtin_functions =
   State_builder.Hashtbl
     (Datatype.String.Hashtbl)
     (Datatype.Triple(Typ)(Datatype.List(Typ))(Datatype.Bool))
     (struct
	let name = "Builtin_functions"
	let dependencies = [ TheMachine.self ]
	let size = 49
      end)

 let add_builtin ?(prefix="__builtin_") s t l b =
     Builtin_functions.add (prefix ^ s) (t, l, b)

 let () = registerAttribute "FC_BUILTIN" (AttrName true)

 (* Initialize the builtin functions after the machine has been initialized. *)
 let initGccBuiltins () : unit =
   let sizeType = theMachine.upointType in
   let add = add_builtin in
   add "__fprintf_chk"
     intType
     (* first argument is really FILE*, not void*, but we don't want to build in
	the definition for FILE *)
     [ voidPtrType; intType; charConstPtrType ]
     true;
   add "__memcpy_chk"
     voidPtrType
     [ voidPtrType; voidConstPtrType; sizeType; sizeType ]
     false;
   add "__memmove_chk"
     voidPtrType [ voidPtrType; voidConstPtrType; sizeType; sizeType ] false;
   add "__mempcpy_chk"
     voidPtrType [ voidPtrType; voidConstPtrType; sizeType; sizeType ] false;
   add "__memset_chk"
     voidPtrType [ voidPtrType; intType; sizeType; sizeType ] false;
   add "__printf_chk" intType [ intType; charConstPtrType ] true;
   add "__snprintf_chk"
     intType [ charPtrType; sizeType; intType; sizeType; charConstPtrType ] 
     true;
   add "__sprintf_chk"
     intType [ charPtrType; intType; sizeType; charConstPtrType ] true;
   add "__stpcpy_chk"
     charPtrType [ charPtrType; charConstPtrType; sizeType ] false;
   add "__strcat_chk"
     charPtrType [ charPtrType; charConstPtrType; sizeType ] false;
   add "__strcpy_chk"
     charPtrType [ charPtrType; charConstPtrType; sizeType ] false;
   add "__strncat_chk"
     charPtrType [ charPtrType; charConstPtrType; sizeType; sizeType ] false;
   add "__strncpy_chk"
     charPtrType [ charPtrType; charConstPtrType; sizeType; sizeType ] false;
   add "__vfprintf_chk"
     intType
     (* first argument is really FILE*, not void*, but we don't want to build in
	the definition for FILE *)
     [ voidPtrType; intType; charConstPtrType; TBuiltin_va_list [] ]
     false;
   add "__vprintf_chk"
     intType [ intType; charConstPtrType; TBuiltin_va_list [] ] false;
   add "__vsnprintf_chk"
     intType
     [ charPtrType; sizeType; intType; sizeType; charConstPtrType;
       TBuiltin_va_list [] ]
     false;
   add "__vsprintf_chk"
     intType
     [ charPtrType; intType; sizeType; charConstPtrType; TBuiltin_va_list [] ]
     false;

   add "alloca" voidPtrType [ sizeType ] false;

   add "acos" doubleType [ doubleType ] false;
   add "acosf" floatType [ floatType ] false;
   add "acosl" longDoubleType [ longDoubleType ] false;

   add "asin" doubleType [ doubleType ] false;
   add "asinf" floatType [ floatType ] false;
   add "asinl" longDoubleType [ longDoubleType ] false;

   add "atan" doubleType [ doubleType ] false;
   add "atanf" floatType [ floatType ] false;
   add "atanl" longDoubleType [ longDoubleType ] false;

   add "atan2" doubleType [ doubleType; doubleType ] false;
   add "atan2f" floatType [ floatType; floatType ] false;
   add "atan2l" longDoubleType [ longDoubleType;
						 longDoubleType ] false;

   let uint16t = uint16_t () in
   add "bswap16" uint16t [uint16t] false;

   let uint32t = uint32_t () in
   add "bswap32" uint32t [uint32t] false;

   let uint64t = uint64_t () in
   add "bswap64" uint64t [uint64t] false;

   add "ceil" doubleType [ doubleType ] false;
   add "ceilf" floatType [ floatType ] false;
   add "ceill" longDoubleType [ longDoubleType ] false;

   add "cos" doubleType [ doubleType ] false;
   add "cosf" floatType [ floatType ] false;
   add "cosl" longDoubleType [ longDoubleType ] false;

   add "cosh" doubleType [ doubleType ] false;
   add "coshf" floatType [ floatType ] false;
   add "coshl" longDoubleType [ longDoubleType ] false;

   add "clz" intType [ uintType ] false;
   add "clzl" intType [ ulongType ] false;
   add "clzll" intType [ ulongLongType ] false;
   add "constant_p" intType [ intType ] false;
   add "ctz" intType [ uintType ] false;
   add "ctzl" intType [ ulongType ] false;
   add "ctzll" intType [ ulongLongType ] false;

   add "exp" doubleType [ doubleType ] false;
   add "expf" floatType [ floatType ] false;
   add "expl" longDoubleType [ longDoubleType ] false;

   add "expect" longType [ longType; longType ] false;

   add "fabs" doubleType [ doubleType ] false;
   add "fabsf" floatType [ floatType ] false;
   add "fabsl" longDoubleType [ longDoubleType ] false;

   add "ffs" intType [ uintType ] false;
   add "ffsl" intType [ ulongType ] false;
   add "ffsll" intType [ ulongLongType ] false;
   add "frame_address" voidPtrType [ uintType ] false;

   add "floor" doubleType [ doubleType ] false;
   add "floorf" floatType [ floatType ] false;
   add "floorl" longDoubleType [ longDoubleType ] false;

   add "huge_val" doubleType [] false;
   add "huge_valf" floatType [] false;
   add "huge_vall" longDoubleType [] false;
   add "ia32_lfence" voidType [] false;
   add "ia32_mfence" voidType [] false;
   add "ia32_sfence" voidType [] false;

   add "inf" doubleType [] false;
   add "inff" floatType [] false;
   add "infl" longDoubleType [] false;
   add "memcpy" voidPtrType [ voidPtrType; voidConstPtrType; sizeType ] false;
   add "mempcpy" voidPtrType [ voidPtrType; voidConstPtrType; sizeType ] false;
   add "memset" voidPtrType [ voidPtrType; intType; intType ] false;

   add "fmod" doubleType [ doubleType ] false;
   add "fmodf" floatType [ floatType ] false;
   add "fmodl" longDoubleType [ longDoubleType ] false;

   add "frexp" doubleType [ doubleType; intPtrType ] false;
   add "frexpf" floatType [ floatType; intPtrType  ] false;
   add "frexpl" longDoubleType [ longDoubleType; intPtrType  ] false;

   add "ldexp" doubleType [ doubleType; intType ] false;
   add "ldexpf" floatType [ floatType; intType  ] false;
   add "ldexpl" longDoubleType [ longDoubleType; intType  ] false;

   add "log" doubleType [ doubleType ] false;
   add "logf" floatType [ floatType ] false;
   add "logl" longDoubleType [ longDoubleType ] false;

   add "log10" doubleType [ doubleType ] false;
   add "log10f" floatType [ floatType ] false;
   add "log10l" longDoubleType [ longDoubleType ] false;

   add "modff" floatType [ floatType; TPtr(floatType,[]) ] false;
   add "modfl"
     longDoubleType [ longDoubleType; TPtr(longDoubleType, []) ] false;

   add "nan" doubleType [ charConstPtrType ] false;
   add "nanf" floatType [ charConstPtrType ] false;
   add "nanl" longDoubleType [ charConstPtrType ] false;
   add "nans" doubleType [ charConstPtrType ] false;
   add "nansf" floatType [ charConstPtrType ] false;
   add "nansl" longDoubleType [ charConstPtrType ] false;
   add "object_size" sizeType [ voidPtrType; intType ] false;

   add "parity" intType [ uintType ] false;
   add "parityl" intType [ ulongType ] false;
   add "parityll" intType [ ulongLongType ] false;

   add "popcount" intType [ uintType ] false;
   add "popcountl" intType [ ulongType ] false;
   add "popcountll" intType [ ulongLongType ] false;

   add "powi" doubleType [ doubleType; intType ] false;
   add "powif" floatType [ floatType; intType ] false;
   add "powil" longDoubleType [ longDoubleType; intType ] false;
   add "prefetch" voidType [ voidConstPtrType ] true;
   add "return" voidType [ voidConstPtrType ] false;
   add "return_address" voidPtrType [ uintType ] false;

   add "sin" doubleType [ doubleType ] false;
   add "sinf" floatType [ floatType ] false;
   add "sinl" longDoubleType [ longDoubleType ] false;

   add "sinh" doubleType [ doubleType ] false;
   add "sinhf" floatType [ floatType ] false;
   add "sinhl" longDoubleType [ longDoubleType ] false;

   add "sqrt" doubleType [ doubleType ] false;
   add "sqrtf" floatType [ floatType ] false;
   add "sqrtl" longDoubleType [ longDoubleType ] false;

   add "stpcpy" charPtrType [ charPtrType; charConstPtrType ] false;
   add "strchr" charPtrType [ charPtrType; intType ] false;
   add "strcmp" intType [ charConstPtrType; charConstPtrType ] false;
   add "strcpy" charPtrType [ charPtrType; charConstPtrType ] false;
   add "strcspn" sizeType [ charConstPtrType; charConstPtrType ] false;
   add "strncat" charPtrType [ charPtrType; charConstPtrType; sizeType ] false;
   add "strncmp" intType [ charConstPtrType; charConstPtrType; sizeType ] false;
   add "strncpy" charPtrType [ charPtrType; charConstPtrType; sizeType ] false;
   add "strspn" sizeType [ charConstPtrType; charConstPtrType ] false;
   add "strpbrk" charPtrType [ charConstPtrType; charConstPtrType ] false;
   (* When we parse builtin_types_compatible_p, we change its interface *)
   add "types_compatible_p"
     intType
     [ theMachine.typeOfSizeOf;(* Sizeof the type *)
       theMachine.typeOfSizeOf (* Sizeof the type *) ]
     false;
   add "tan" doubleType [ doubleType ] false;
   add "tanf" floatType [ floatType ] false;
   add "tanl" longDoubleType [ longDoubleType ] false;

   add "tanh" doubleType [ doubleType ] false;
   add "tanhf" floatType [ floatType ] false;
   add "tanhl" longDoubleType [ longDoubleType ] false;

   add "unreachable" voidType [ ] false;

   let int8_t = Some scharType in
   let int16_t = try Some (int16_t ()) with Not_found -> None in 
   let int32_t = try Some (int32_t ()) with Not_found -> None in
   let int64_t = try Some (int64_t ()) with Not_found -> None in
   let uint8_t = Some ucharType in
   let uint16_t = try Some (uint16_t ()) with Not_found -> None in
   let uint32_t = try Some (uint32_t ()) with Not_found -> None in
   let uint64_t = try Some (uint64_t ()) with Not_found -> None in

   (* Binary monomorphic versions of atomic builtins *)
   let atomic_instances = 
     [int8_t, "_int8_t";
      int16_t,"_int16_t";
      int32_t,"_int32_t";
      int64_t,"_int64_t";
      uint8_t, "_uint8_t";
      uint16_t,"_uint16_t";
      uint32_t,"_uint32_t";
      uint64_t,"_uint64_t"]
   in
   let add_sync (typ,name) f = 
     match typ with 
     | Some typ -> 
       add ~prefix:"__sync_" (f^name) typ [ TPtr(typ,[]); typ] true
     | None -> ()
   in
   let add_sync f = 
     List.iter (fun typ -> add_sync typ f) atomic_instances
   in
   add_sync "fetch_and_add";
   add_sync "fetch_and_sub";
   add_sync "fetch_and_or";
   add_sync "fetch_and_and";
   add_sync "fetch_and_xor";
   add_sync "fetch_and_nand";
   add_sync "add_and_fetch";
   add_sync "sub_and_fetch";
   add_sync "or_and_fetch";
   add_sync "and_and_fetch";
   add_sync "xor_and_fetch";
   add_sync "nand_and_fetch";
   add_sync "lock_test_and_set";
   List.iter (fun (typ,n) -> 
     match typ with 
     | Some typ -> 
       add ~prefix:"" ("__sync_bool_compare_and_swap"^n)
         intType
         [ TPtr(typ,[]); typ ; typ] 
         true
     | None -> ())
     atomic_instances;
   List.iter (fun (typ,n) -> 
     match typ with 
     | Some typ -> 
       add ~prefix:"" ("__sync_val_compare_and_swap"^n)
         typ
         [ TPtr(typ,[]); typ ; typ] 
         true
     | None -> ())
     atomic_instances;
   List.iter (fun (typ,n) -> 
     match typ with 
     | Some typ -> 
       add ~prefix:"" ("__sync_lock_release"^n)
         voidType
         [ TPtr(typ,[]) ] 
         true;
     | None -> ())
     atomic_instances;
   add ~prefix:"" "__sync_synchronize" voidType [] true
;;

(* Builtins related to va_list. Added to all non-msvc machdeps, because
   Cabs2cil supposes they exist. *)
 let initVABuiltins () =
   let hasbva = theMachine.theMachine.has__builtin_va_list in
   let add = add_builtin in
   add "next_arg"
     (* When we parse builtin_next_arg we drop the second argument *)
     (if hasbva then TBuiltin_va_list [] else voidPtrType) [] false;
   if hasbva then begin
     add "va_end" voidType [ TBuiltin_va_list [] ] false;
     add "varargs_start" voidType [ TBuiltin_va_list [] ] false;
     (* When we parse builtin_{va,stdarg}_start, we drop the second argument *)
     add "va_start" voidType [ TBuiltin_va_list [] ] false;
     add "stdarg_start" voidType [ TBuiltin_va_list [] ] false;
     (* When we parse builtin_va_arg we change its interface *)
     add "va_arg"
       voidType
       [ TBuiltin_va_list [];
	 theMachine.typeOfSizeOf;(* Sizeof the type *)
	 voidPtrType (* Ptr to res *) ]
       false;
     add "va_copy" voidType [ TBuiltin_va_list []; TBuiltin_va_list [] ] false;
   end

let initMsvcBuiltins () : unit =
  (** Take a number of wide string literals *)
  Builtin_functions.add "__annotation" (voidType, [ ], true)
;;

let init_builtins () =
  if not (TheMachine.is_computed ()) then
    Kernel.fatal ~current:true "You must call initCIL before init_builtins" ;
  if Builtin_functions.length () <> 0 then
    Kernel.fatal ~current:true "Cil builtins already initialized." ;
  if msvcMode () then
    initMsvcBuiltins ()
  else begin
    initVABuiltins ();
    if gccMode () then initGccBuiltins ();
  end

 (** This is used as the location of the prototypes of builtin functions. *)
 let builtinLoc: location = Location.unknown

 let range_loc loc1 loc2 = fst loc1, snd loc2

(* JS 2012/11/16: probably broken since it may call constFold on some exp: this
   operation modifies this expression in-place! *)
 let compareConstant c1 c2 =
   match c1, c2 with
     | CEnum e1, CEnum e2 ->
       e1.einame = e2.einame && e1.eihost.ename = e2.eihost.ename &&
       (match constFoldToInt e1.eival, constFoldToInt e2.eival with
          | Some i1, Some i2 -> Integer.equal i1 i2
          | _ -> false)
     | CInt64 (i1,k1,_), CInt64(i2,k2,_) -> 
       k1 = k2 && Integer.equal i1 i2
     | CStr s1, CStr s2 -> s1 = s2
     | CWStr l1, CWStr l2 ->
       (try List.for_all2 (fun x y -> Int64.compare x y = 0) l1 l2
        with Invalid_argument _ -> false)
     | CChr c1, CChr c2 -> c1 = c2
     | CReal(f1,k1,_), CReal(f2,k2,_) -> k1 = k2 && f1 = f2
     | (CEnum _ | CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _), _ -> false

 let compareExp (e1: exp) (e2: exp) : bool =
   Cil_datatype.ExpStructEq.equal e1 e2

 let compareLval (lv1: lval) (lv2: lval) : bool =
   Cil_datatype.LvalStructEq.equal lv1 lv2

 let compareOffset (off1: offset) (off2: offset) : bool =
   Cil_datatype.OffsetStructEq.equal off1 off2

 (* Iterate over all globals, including the global initializer *)
 let iterGlobals (fl: file) (doone: global -> unit) : unit =
   let doone' g =
     CurrentLoc.set (Global.loc g);
     doone g
   in
   List.iter doone' fl.globals;
   match fl.globinit with
   | None -> ()
   | Some g -> doone' (GFun(g, Location.unknown))

 (* Fold over all globals, including the global initializer *)
 let foldGlobals (fl: file) (doone: 'a -> global -> 'a) (acc: 'a) : 'a =
   let doone' acc g =
     CurrentLoc.set (Global.loc g);
     doone acc g
   in
   let acc' = List.fold_left doone' acc fl.globals in
   match fl.globinit with
   | None -> acc'
   | Some g -> doone' acc' (GFun(g, Location.unknown))

 let is_skip = function Instr (Skip _) -> true | _ -> false

 (** [b_assumes] must be always empty for behavior named
     [Cil.default_behavior_name] *) 
 let mk_behavior ?(name=default_behavior_name) ?(assumes=[]) ?(requires=[])
     ?(post_cond=[]) ?(assigns=WritesAny) ?(allocation=None)  ?(extended=[]) ()
     =
   { b_name = name;
     b_assumes = assumes; (* must be always empty for default_behavior_name *)
     b_requires = requires;
     b_assigns = assigns ;
     b_allocation = (match allocation with
		       | None -> FreeAllocAny
		       | Some af -> af);
     b_post_cond = post_cond ;
     b_extended = extended;
   }

let spare_attributes_for_c_cast =
  "declspec"::"arraylen"::bitfield_attribute_name::qualifier_attributes

let type_remove_attributes_for_c_cast =
  typeRemoveAttributes spare_attributes_for_c_cast
    
let spare_attributes_for_logic_cast =
  spare_attributes_for_c_cast

let type_remove_attributes_for_logic_type =
  typeRemoveAttributes spare_attributes_for_logic_cast
    
let () = Cil_datatype.drop_non_logic_attributes :=
  dropAttributes spare_attributes_for_logic_cast

let need_cast ?(force=false) oldt newt =
  let oldt = type_remove_attributes_for_c_cast (unrollType oldt) in
  let newt = type_remove_attributes_for_c_cast (unrollType newt) in
  not (Cil_datatype.Typ.equal oldt newt) &&
    (force ||
       match oldt, newt with
         | TInt(ik,ai),TEnum(e,ae)
         | TEnum(e,ae),TInt(ik,ai) when Attributes.equal ai ae ->
             ik <> e.ekind
         | _ -> true)

(* Strip the "const" from the type. It is unfortunate that const variables can
   only be set in initialization. Once we decided to move all declarations to
   the top of the functions, we have no way of setting a "const"
   variable. Furthermore, if the type of the variable is an array or a struct
   we must recursively strip the "const" from fields and array elements. *)
 let rec stripConstLocalType (t: typ) : typ =
   let dc a =
     if hasAttribute "const" a then
       dropAttribute "const" a
     else a
   in
   match t with
   | TPtr (bt, a) ->
      (* We want to be able to detect by pointer equality if the type has
       * changed. So, don't realloc the type unless necessary. *)
      let a' = dc a in if a != a' then TPtr(bt, a') else t
  | TInt (ik, a) ->
      let a' = dc a in if a != a' then TInt(ik, a') else t
  | TFloat(fk, a) ->
      let a' = dc a in if a != a' then TFloat(fk, a') else t
  | TNamed (ti, a) ->
      (* We must go and drop the consts from the typeinfo as well ! *)
      let t' = stripConstLocalType ti.ttype in
      if t != t' then begin
        (* ignore (warn "Stripping \"const\" from typedef %s\n" ti.tname); *)
        ti.ttype <- t'
      end;
      let a' = dc a in if a != a' then TNamed(ti, a') else t

  | TEnum (ei, a) ->
      let a' = dc a in if a != a' then TEnum(ei, a') else t

  | TArray(bt, leno, _, a) ->
      (* We never assign to the array. So, no need to change the const. But
       * we must change it on the base type *)
      let bt' = stripConstLocalType bt in
      if bt' != bt then TArray(bt', leno, empty_size_cache (), a) else t

  | TComp(ci, _, a) ->
      (* Must change both this structure as well as its fields *)
      List.iter
        (fun f ->
          let t' = stripConstLocalType f.ftype in
          if t' != f.ftype then begin
            Kernel.debug ~level:3 "Stripping \"const\" from field %s of %s\n"
              f.fname (compFullName ci) ;
            f.ftype <- t'
          end)
        ci.cfields;
      let a' = dc a in if a != a' then TComp(ci, empty_size_cache (), a') else t

    (* We never assign functions either *)
  | TFun(_rt, _args, _va, _a) -> t
  | TVoid _ -> (* this may happen with temporary used only for their sizeof. *)
      t
  | TBuiltin_va_list a ->
      let a' = dc a in if a != a' then TBuiltin_va_list a' else t

 let cvar_to_lvar vi = match vi.vlogic_var_assoc with
   | None ->
     let lv =
       { lv_name = vi.vname;
	 lv_id = vi.vid;
         lv_kind = LVC;
	 lv_type = Ctype vi.vtype ;
	 lv_origin = Some vi}
     in 
     vi.vlogic_var_assoc <- Some lv; lv
   | Some lv -> lv

 let copyVarinfo (vi: varinfo) (newname: string) : varinfo =
   let vi' = Cil_const.copy_with_new_vid vi in
   vi'.vname <- newname;
   (match vi.vlogic_var_assoc with
	None -> ()
      | Some _ ->
	  vi'.vlogic_var_assoc <- None;
	  ignore(cvar_to_lvar vi'));
   vi'

 let rec findUniqueName ?(suffix="") fdec name =
   let current_name = name ^ suffix in
   (* Is this check a performance problem?  We could bring the old
      unchecked makeTempVar back as a separate function that assumes
      the prefix name does not occur in the original program. *)
   if (List.exists (fun vi -> vi.vname = current_name) fdec.slocals)
     || (List.exists (fun vi -> vi.vname = current_name) fdec.sformals) then begin
       fdec.smaxid <- 1 + fdec.smaxid;
       findUniqueName ~suffix:("_" ^ (string_of_int (1 + fdec.smaxid))) fdec name
     end else
       current_name

 let makeLocal ?(temp=false) ?(formal=false) fdec name typ =
   (* a helper function *)
   let name = findUniqueName fdec name in
   fdec.smaxid <- 1 + fdec.smaxid;
   let vi = makeVarinfo ~temp false formal name typ in
   vi

 (* Make a local variable and add it to a function *)
 let makeLocalVar fdec ?scope ?(temp=false) ?(insert = true) name typ =
   let typ = stripConstLocalType typ in
   let vi = makeLocal ~temp fdec name typ in
   if insert then
     begin
       fdec.slocals <- fdec.slocals @ [vi];
       let local_block =
	 match scope with
	 | None -> fdec.sbody
	 | Some b -> b
       in
       local_block.blocals <- vi::local_block.blocals
     end;
     vi

 let makeTempVar fdec ?insert ?(name = "__cil_tmp") ?descr ?(descrpure = true)
		 typ : varinfo =
   let vi = makeLocalVar fdec ~temp:true ?insert name typ in
   vi.vdescr <- descr;
   vi.vdescrpure <- descrpure;
   vi

    (* Set the types of arguments and results as given by the function type
     * passed as the second argument *)
 let setFunctionType (f: fundec) (t: typ) =
   match unrollType t with
     TFun (_rt, Some args, _va, _a) ->
       if List.length f.sformals <> List.length args then
	 Kernel.fatal ~current:true "setFunctionType: number of arguments differs from the number of formals" ;
       (* Change the function type. *)
       f.svar.vtype <- t;
       (* Change the sformals and we know that indirectly we'll change the
	* function type *)
       List.iter2
	 (fun (_an,at,aa) f ->
	   f.vtype <- at; f.vattr <- aa)
	 args f.sformals

   | _ -> Kernel.fatal ~current:true "setFunctionType: not a function type"

 (* Set the types of arguments and results as given by the function type
    passed as the second argument *)
 let setFunctionTypeMakeFormals (f: fundec) (t: typ) =
   match unrollType t with
     TFun (_rt, Some args, _va, _a) ->
       if f.sformals <> [] then
	 Kernel.fatal ~current:true "setFunctionTypMakeFormals called on function %s with some formals already"
	   f.svar.vname ;
       (* Change the function type. *)
       f.svar.vtype <- t;
       f.sformals <- 
	 List.map (fun (n,t,_a) -> makeLocal ~formal:true f n t) args;
       setFunctionType f t

   | _ -> 
     Kernel.fatal ~current:true "setFunctionTypeMakeFormals: not a function type: %a" 
       !pp_typ_ref t

 let setMaxId (f: fundec) =
   f.smaxid <- List.length f.sformals + List.length f.slocals

   (* Make a formal variable for a function. Insert it in both the sformals
    * and the type of the function. You can optionally specify where to insert
    * this one. If where = "^" then it is inserted first. If where = "$" then
    * it is inserted last. Otherwise where must be the name of a formal after
    * which to insert this. By default it is inserted at the end. *)
 let makeFormalVar fdec ?(where = "$") name typ : varinfo =
   (* Search for the insertion place *)
   let makeit name = makeLocal ~formal:true fdec name typ in
   let rec loopFormals acc = function
       [] ->
	 if where = "$" then 
           let vi = makeit name in vi, List.rev (vi::acc)
	 else Kernel.fatal ~current:true
           "makeFormalVar: cannot find insert-after formal %s" where
     | f :: rest when f.vname = where -> 
         let vi = makeit name in vi, List.rev_append acc (f :: vi :: rest)
     | f :: rest -> loopFormals (f::acc) rest
   in
   let vi, newformals =
     if where = "^" then let vi = makeit name in vi, vi :: fdec.sformals
     else
       loopFormals [] fdec.sformals
   in
   setFormals fdec newformals;
   vi

    (* Make a global variable. Your responsibility to make sure that the name
     * is unique *)
 let makeGlobalVar ?source ?temp name typ =
   makeVarinfo ?source ?temp true false name typ

 let emptyFunctionFromVI vi =
   let r =
     { svar  = vi;
       smaxid = 0;
       slocals = [];
       sformals = [];
       sbody = mkBlock [];
       smaxstmtid = None;
       sallstmts = [];
       sspec =   empty_funspec ()
     }
   in
   setFormalsDecl r.svar r.svar.vtype;
   r

 (* Make an empty function *)
 let emptyFunction name =
   let vi = 
     makeGlobalVar ~temp:false name (TFun(voidType, Some [], false,[]))
   in emptyFunctionFromVI vi

 let dummyFile =
   { globals = [];
     fileName = "<dummy>";
     globinit = None;
     globinitcalled = false;}


 (* Take the name of a file and make a valid varinfo name out of it. There are
  * a few characters that are not valid in varinfos *)
 let makeValidVarinfoName (s: string) =
   let s = String.copy s in (* So that we can update in place *)
   let l = String.length s in
   for i = 0 to l - 1 do
     let c = String.get s i in
     let isinvalid =
       match c with
	 '-' | '.' -> true
       | _ -> false
     in
     if isinvalid then
       String.set s i '_';
   done;
   s

 let rec lastOffset (off: offset) : offset =
   match off with
   | NoOffset | Field(_,NoOffset) | Index(_,NoOffset) -> off
   | Field(_,off) | Index(_,off) -> lastOffset off

 let isBitfield lval =
   match lval with
   | _, off ->
       let off = lastOffset off in
       match off with
	 Field({fbitfield=Some _}, _) -> true 
       | _ -> false

 let addOffsetLval toadd (b, off) : lval =
  b, addOffset toadd off

 let rec removeOffset (off: offset) : offset * offset =
   match off with
     NoOffset -> NoOffset, NoOffset
   | Field(_f, NoOffset) -> NoOffset, off
   | Index(_i, NoOffset) -> NoOffset, off
   | Field(f, restoff) ->
       let off', last = removeOffset restoff in
       Field(f, off'), last
   | Index(i, restoff) ->
       let off', last = removeOffset restoff in
       Index(i, off'), last

 let removeOffsetLval ((b, off): lval) : lval * offset =
   let off', last = removeOffset off in
   (b, off'), last

 class copyVisitExpr = object
     inherit genericCilVisitor (copy_visit (Project.current ()))
     method! vexpr e = 
       ChangeDoChildrenPost ({e with eid = Eid.next ()}, fun x -> x)
 end

 let copy_exp e = visitCilExpr (new copyVisitExpr) e

 (** A visitor that does constant folding. If "machdep" is true then we do
  * machine dependent simplification (e.g., sizeof) *)
 class constFoldVisitorClass (machdep: bool) : cilVisitor = object
   inherit nopCilVisitor

   method! vinst i =
     match i with
       (* Skip two functions to which we add Sizeof to the type arguments.
	  See the comments for these above. *)
       Call(_,({enode = Lval (Var vi,NoOffset)}),_,_)
	 when ((vi.vname = "__builtin_va_arg")
	       || (vi.vname = "__builtin_types_compatible_p")) ->
	   SkipChildren
     | _ -> DoChildren
   method! vexpr (e: exp) =
     (* Do it bottom up *)
     ChangeDoChildrenPost (e, constFold machdep)

 end
 let constFoldVisitor (machdep: bool) = new constFoldVisitorClass machdep

 let rec constFoldTermNodeAtTop = function
   | TSizeOf typ as t ->
     (try integer_lconstant (bytesSizeOf typ)
      with SizeOfError _ -> t)
   | TSizeOfStr str -> integer_lconstant (String.length str + 1)
   | TAlignOf typ -> integer_lconstant (bytesAlignOf typ)
   | TSizeOfE { term_type= Ctype typ } -> constFoldTermNodeAtTop (TSizeOf typ)
   | TAlignOfE { term_type= Ctype typ }
     -> 	constFoldTermNodeAtTop (TAlignOf typ)
   | TSizeOfE _ | TAlignOfE _ ->
     assert false (* sizeof/alignof of logic types are rejected
		     by typing anyway. *)
   | t -> t

 let constFoldTerm machdep t =
   let visitor = object
     inherit nopCilVisitor
     method! vterm_node t =
       if machdep then ChangeToPost (t,constFoldTermNodeAtTop)
       else DoChildren
   end
   in
   visitCilTerm visitor t


 (** Find a function or function prototype with the given name in the file.
   * If it does not exist, create a prototype with the given type, and return
   * the new varinfo.  This is useful when you need to call a libc function
   * whose prototype may or may not already exist in the file.
   *
   * Because the new prototype is added to the start of the file, you shouldn't
   * refer to any struct or union types in the function type.*)
 let findOrCreateFunc (f:file) (name:string) (t:typ) : varinfo =
   let rec search glist =
     match glist with
       | GFunDecl(_, vi, _) :: _rest when vi.vname = name -> vi
       | GVarDecl(vi,_) :: _rest when vi.vname = name ->
  	   Kernel.fatal ~current:true
	     "findOrCreateFunc: can't create %s because another global exists \
               with that name." name ;
       | _ :: rest -> search rest (* tail recursive *)
       | [] -> (*not found, so create one *)
	   let t' = unrollTypeDeep t in
	   let new_decl = makeGlobalVar ~temp:false name t' in
	   setFormalsDecl new_decl t';
           f.globals <- GFunDecl(empty_funspec (), new_decl, Location.unknown) :: f.globals;
	   new_decl
   in
   search f.globals

let childrenFileSameGlobals vis f =
  let fGlob g = visitCilGlobal vis g in
  iterGlobals f
    (fun g ->
       match fGlob g with
           [g'] when g' == g || Cil_datatype.Global.equal g' g -> ()
             (* Try to do the pointer check first *)
         | gl ->
             Kernel.fatal ~current:true
	       "You used visitCilFileSameGlobals but the global got changed:\n %a\nchanged to %a\n"
	       !pp_global_ref g
	       (Pretty_utils.pp_list ~sep:"@\n" !pp_global_ref) gl ;
    );
  f

 let post_file vis f =
   let res = vis#vfile f in
   let post_action res = vis#fill_global_tables; res in
   match res with
       SkipChildren -> ChangeToPost(f, post_action)
     | JustCopy -> JustCopyPost post_action
     | JustCopyPost f -> JustCopyPost (fun x -> f (post_action x))
     | ChangeTo res -> ChangeToPost(res, post_action)
     | ChangeToPost (res, f) -> ChangeToPost (res, fun x -> f (post_action x))
     | DoChildren -> DoChildrenPost post_action
     | DoChildrenPost f -> DoChildrenPost (fun x -> f (post_action x))
     | ChangeDoChildrenPost(f,post) ->
       ChangeDoChildrenPost(f, fun x -> post (post_action x))

 (* A visitor for the whole file that does not change the globals *)
 let visitCilFileSameGlobals (vis : cilVisitor) (f : file) : unit =
   if vis#behavior.is_copy_behavior then
     Kernel.fatal ~current:true "You used visitCilFileSameGlobals with a copy visitor. Nothing is done"
   else
     ignore
       (doVisitCil vis vis#behavior.cfile (post_file vis) childrenFileSameGlobals f)

 let childrenFileCopy vis f =
   let fGlob g = visitCilGlobal vis g in
   (* Scan the globals. Make sure this is tail recursive. *)
   let rec loop (acc: global list) = function
       [] -> f.globals <- List.rev acc
     | g :: restg ->
	 loop (List.rev_append (fGlob g) acc) restg
   in
   loop [] f.globals;
   (* the global initializer *)
   (match f.globinit with
     None -> ()
   | Some g -> f.globinit <- Some (visitCilFunction vis g));
   f

 (* Be careful with visiting the whole file because it might be huge. *)
 let visitCilFileCopy (vis : cilVisitor) (f : file) : file =
   if vis#behavior.is_copy_behavior then begin
     Queue.add Logic_env.prepare_tables vis#get_filling_actions;
   end;
   doVisitCil vis vis#behavior.cfile (post_file vis) childrenFileCopy f

 let visitCilFile vis f =
   if vis#behavior.is_copy_behavior then
     Kernel.fatal ~current:true "You used visitCilFile with a copy visitor. Nothing is done"
   else ignore (visitCilFileCopy vis f)


 let appears_in_expr v e =
   let module M = struct exception Found end in
   let vis = object
     inherit nopCilVisitor
     method! vvrbl v' =
       if Cil_datatype.Varinfo.equal v v' then raise M.Found;
       SkipChildren
   end
   in
   try ignore (visitCilExpr vis e); false
   with M.Found -> true

 (** Create or fetch the global initializer. Tries to put a call to the
  * function with the main_name into it *)
 let getGlobInit ?(main_name="main") (fl: file) =
   match fl.globinit with
     Some f -> f
   | None -> begin
       (* Sadly, we cannot use the Filename library because it does not like
	* function names with multiple . in them *)
       let f =
	 let len = String.length fl.fileName in
	 (* Find the last path separator and record the first . that we see,
	 * going backwards *)
	 let lastDot = ref len in
	 let rec findLastPathSep i =
	   if i < 0 then -1 else
	   let c = String.get fl.fileName i in
	   if c = '/' || c = '\\' then i
	   else begin
	     if c = '.' && !lastDot = len then
	       lastDot := i;
	     findLastPathSep (i - 1)
	   end
	 in
	 let lastPathSep = findLastPathSep (len - 1) in
	 let basenoext =
	   String.sub fl.fileName (lastPathSep + 1) (!lastDot - lastPathSep - 1)
	 in
	 emptyFunction
	   (makeValidVarinfoName ("__globinit_" ^ basenoext))
       in
       fl.globinit <- Some f;
       (* Now try to add a call to the global initialized at the beginning of
	* main *)
       let inserted = ref false in
       List.iter
	 (function
           | GFun(m, lm) when m.svar.vname = main_name ->
	       (* Prepend a prototype to the global initializer *)
	       fl.globals <- GFunDecl (empty_funspec (),f.svar, lm) :: fl.globals;
	       m.sbody.bstmts <-
		  mkStmt (Instr (Call(None,
				      new_exp ~loc:f.svar.vdecl (Lval(var f.svar)),
				      [], Location.unknown)))
		 :: m.sbody.bstmts;
	       inserted := true;
	       Kernel.feedback ~level:2 "Inserted the globinit" ;
	       fl.globinitcalled <- true;
	   | _ -> ())
	 fl.globals;

 (* YMo: remove useless warning that worries users *)
 (*       if not !inserted then *)
 (*         ignore (E.warn "Cannot find %s to add global initializer %s" *)
 (*                   main_name f.svar.vname); *)

       f
   end

 (* Fold over all globals, including the global initializer *)
 let mapGlobals (fl: file)
		(doone: global -> global) : unit =
   fl.globals <- List.map doone fl.globals;
   (match fl.globinit with
     None -> ()
   | Some g -> begin
       match doone (GFun(g, Location.unknown)) with
	 GFun(g', _) -> fl.globinit <- Some g'
       | _ -> Kernel.fatal ~current:true "mapGlobals: globinit is not a function"
   end)

 (***************************************************************************)

 (* Convert an expression into an attribute, if possible. Otherwise raise
    NotAnAttrParam *)
 exception NotAnAttrParam of exp
 let rec expToAttrParam (e: exp) : attrparam =
   match (constFold true e).enode with
     | Const(CInt64(i,k,_)) ->
         let i', _trunc = truncateInteger64 k i in
         AInt i'
     | Const(CEnum ei) -> expToAttrParam ei.eival
     | Lval (Var v, NoOffset) -> ACons(v.vname, [])
     | SizeOf t -> ASizeOf t
     | SizeOfE e' -> ASizeOfE (expToAttrParam e')
     | UnOp(uo, e', _)  -> AUnOp (uo, expToAttrParam e')
     | BinOp(bo, e1',e2', _)  -> ABinOp (bo, expToAttrParam e1',
				         expToAttrParam e2')
     | _ -> raise (NotAnAttrParam e)


 (******************** OPTIMIZATIONS *****)
 let rec peepHole1 (* Process one statement and possibly replace it *)
		   (doone: instr -> instr list option)
		   (* Scan a block and recurse inside nested blocks *)
		   (ss: stmt list) : unit =
   let rec doInstrList (il: instr list) : instr list =
     match il with
       [] -> []
     | i :: rest -> begin
	 match doone i with
	   None -> i :: doInstrList rest
	 | Some sl -> doInstrList (sl @ rest)
     end
   in
   List.iter
     (fun s ->
       match s.skind with
       | Instr i -> s.skind <- stmt_of_instr_list (doInstrList [i])
       | If (_e, tb, eb, _) ->
	   peepHole1 doone tb.bstmts;
	   peepHole1 doone eb.bstmts
       | Switch (_e, b, _, _) -> peepHole1 doone b.bstmts
       | Loop (_, b, _l, _, _) -> peepHole1 doone b.bstmts
       | Block b -> peepHole1 doone b.bstmts
       | UnspecifiedSequence seq ->
	   peepHole1 doone (List.map (fun (x,_,_,_,_) -> x) seq)
       | TryCatch(b,l,_) ->
         peepHole1 doone b.bstmts;
         List.iter (fun (_,b) -> peepHole1 doone b.bstmts) l
       | TryFinally (b, h, _l) ->
	   peepHole1 doone b.bstmts;
	   peepHole1 doone h.bstmts
       | TryExcept (b, (il, e), h, l) ->
	   peepHole1 doone b.bstmts;
	   peepHole1 doone h.bstmts;
	   s.skind <- TryExcept(b, (doInstrList il, e), h, l);
       | Return _ | Goto _ | Break _ | Continue _ | Throw _ -> ())
     ss

 (* Process two statements and possibly replace them both *)
 let rec peepHole2 ~agressive (dotwo: stmt * stmt -> stmt list option) (ss: stmt list) =
   let rec doStmtList acc (il: stmt list) : stmt list =
     match il with
       [] -> List.rev acc
     | [i] -> process i; List.rev (i::acc)
     | (i1 :: ((i2 :: rest) as rest2)) ->
	 begin
	   match dotwo (i1,i2) with
	     None -> process i1; doStmtList (i1::acc) rest2
	   | Some sl -> 
             if agressive then
               doStmtList acc (sl @ rest) 
             else 
               doStmtList (List.rev_append sl acc) rest
	 end
   and doUnspecifiedStmtList il =
     match il with
	 [] -> []
       | [ (s,_,_,_,_) ] -> process s; il
       | ((i1,m1,w1,r1,_) as hd)::(((i2,m2,w2,r2,_)::rest) as rest2) ->
	   begin
	     match dotwo (i1,i2) with
		 None -> process i1; hd :: doUnspecifiedStmtList rest2
	       | Some [] -> doUnspecifiedStmtList rest
	       | Some (hd::tl) ->
		   let call s = match s.skind with
		     | Instr(Call _ ) -> [ref s]
		     | _ -> []
		   in
		   let res =
		     (hd, m1@m2, w1 @ w2, r1 @ r2,call hd) ::
		       (List.map (fun x -> x,[],[],[],call x) tl)
		   in
		   if agressive then doUnspecifiedStmtList (res @ rest)
		   else res @ doUnspecifiedStmtList rest
	   end
   and process s =
     match s.skind with
	 Instr _i -> ()
       | If (_e, tb, eb, _) ->
	   tb.bstmts <- peepHole2 ~agressive dotwo tb.bstmts;
	   eb.bstmts <- peepHole2 ~agressive dotwo eb.bstmts
       | Switch (_e, b, _, _) -> b.bstmts <- peepHole2 ~agressive dotwo b.bstmts
       | Loop (_, b, _l, _, _) -> b.bstmts <- peepHole2 ~agressive dotwo b.bstmts
       | Block b -> b.bstmts <- doStmtList [] b.bstmts
       | TryCatch (b,l,_) ->
         b.bstmts <- peepHole2 ~agressive dotwo b.bstmts;
         List.iter 
           (fun (_,b) ->
             b.bstmts <-  peepHole2 ~agressive dotwo b.bstmts)
           l
       | TryFinally (b, h, _l) ->
         b.bstmts <- peepHole2 ~agressive dotwo b.bstmts;
	 b.bstmts <- peepHole2 ~agressive dotwo h.bstmts
       | TryExcept (b, (_il, _e), h, _l) ->
	   b.bstmts <- peepHole2 ~agressive dotwo b.bstmts;
	   h.bstmts <- peepHole2 ~agressive dotwo h.bstmts;
	   () (*s.skind <- TryExcept (b, (doInstrList il, e), h, l)*)

       | UnspecifiedSequence seq ->
	   s.skind <- UnspecifiedSequence (doUnspecifiedStmtList seq)
       | Return _ | Goto _ | Break _ | Continue _ | Throw _ -> ()
   in
   if agressive then List.iter process ss;
   doStmtList [] ss

 let dExp: string -> exp =
   fun d -> new_exp ~loc:Cil_datatype.Location.unknown (Const(CStr(d)))

 let dInstr: string -> location -> instr =
   fun d l -> Asm([], [d], [], [], [], [], l)

 let dGlobal: string -> location -> global =
   fun d l -> GAsm(d, l)

  (* Make an AddrOf. Given an lval of type T will give back an expression of
   * type ptr(T)  *)
 let mkAddrOf ~loc ((_b, _off) as lval) : exp =
   (* Never take the address of a register variable *)
   (match lval with
     Var vi, _off when vi.vstorage = Register -> vi.vstorage <- NoStorage
   | _ -> ());
   match lval with
     Mem e, NoOffset -> e
   | b, Index(z, NoOffset) when isZero z -> new_exp ~loc (StartOf (b, NoOffset))
  (* array *)
   | _ -> new_exp ~loc (AddrOf lval)

 let mkAddrOfVi vi = mkAddrOf vi.vdecl (var vi)

 let mkAddrOrStartOf ~loc (lv: lval) : exp =
   match unrollTypeSkel (typeOfLval lv) with
     TArray _ -> new_exp ~loc (StartOf lv)
   | _ -> mkAddrOf ~loc lv

 let mkMem ~(addr: exp) ~(off: offset) : lval =
   let res =
     match addr.enode, off with
     | AddrOf lv, _ -> addOffsetLval off lv
     | StartOf lv, _ -> (* Must be an array *)
       addOffsetLval (Index(zero ~loc:addr.eloc, off)) lv
     | _, _ -> Mem addr, off
   in
   (* ignore (E.log "memof : %a:%a\nresult = %a\n" d_plainexp addr d_plainoffset
      off d_plainexp res); *)
   res

 let mkTermMem ~(addr: term) ~(off: term_offset) : term_lval =
   let loc = addr.term_loc in
   let res =
     match addr.term_node, off with
       TAddrOf lv, _ -> addTermOffsetLval off lv
     | TStartOf lv, _ -> (* Must be an array *)
	 addTermOffsetLval (TIndex(lzero ~loc (), off)) lv
     | _, _ -> TMem addr, off
   in
 (*  ignore (E.log "memof : %a:%a\nresult = %a\n"
	     d_plainexp addr d_plainoffset off d_plainexp res); *)
   res

 let splitFunctionType (ftype: typ)
     : typ * (string * typ * attributes) list option * bool * attributes =
   match unrollType ftype with
       TFun (rt, args, isva, a) -> rt, args, isva, a
     | _ -> Kernel.fatal ~current:true "splitFunctionType invoked on a non function type %a"
       !pp_typ_ref ftype

 let splitFunctionTypeVI (fvi: varinfo)
     : typ * (string * typ * attributes) list option * bool * attributes =
   match unrollType fvi.vtype with
     TFun (rt, args, isva, a) -> rt, args, isva, a
   | _ -> Kernel.abort "Function %s invoked on a non function type" fvi.vname

 let rec integralPromotion ?(forComparison=false) (t : typ) : typ = (* c.f. ISO 6.3.1.1 *)
   match unrollType t with
   | TInt ((IShort|ISChar|IBool), a) -> TInt(IInt, a)
   | TInt (IUChar|IUShort as k, a) -> 
     if bitsSizeOfInt k < bitsSizeOf intType then 
       TInt(IInt, a)
     else
       TInt(IUInt,a)
   | TInt (IChar,a) ->
     let k = if isSigned IChar then ISChar else IUChar in
     integralPromotion ~forComparison (TInt (k, a))
   | TInt (k,a) ->
     begin match findAttribute bitfield_attribute_name a with
     | [AInt size] ->
       (* This attribute always fits in int. *)
       let size = Integer.to_int size in
       let sizeofint = bitsSizeOf intType in
       let attrs = dropAttribute bitfield_attribute_name a in
       let kind =
         if size < sizeofint then IInt
         else if size = sizeofint then
           if isSigned k then IInt
           else IUInt
         else k
       in
       TInt(kind,attrs)
     | [] -> t
     | _ -> assert false
     end
   | TEnum (ei, a) -> let r = integralPromotion (TInt(ei.ekind, a)) in
	 if forComparison then
	   (match r with
	     | TInt(kind,_) -> if kind <> ei.ekind then r else t
	     | t -> Kernel.fatal ~current:true "integralPromotion: not expecting %a" !pp_typ_ref t)  
	 else r
   (* gcc packed enums can be < int *)
   | t -> Kernel.fatal ~current:true "integralPromotion: not expecting %a" !pp_typ_ref t

 let integralPromotion (t : typ) : typ = integralPromotion t

 let arithmeticConversion t1 t2 = (* c.f. ISO 6.3.1.8 *)
  let checkToInt _ = () in  (* dummies for now *)
  let checkToFloat _ = () in
  match unrollTypeSkel t1, unrollTypeSkel t2 with
    TFloat(FLongDouble, _), _ -> checkToFloat t2; t1
  | _, TFloat(FLongDouble, _) -> checkToFloat t1; t2
  | TFloat(FDouble, _), _ -> checkToFloat t2; t1
  | _, TFloat (FDouble, _) -> checkToFloat t1; t2
  | TFloat(FFloat, _), _ -> checkToFloat t2; t1
  | _, TFloat (FFloat, _) -> checkToFloat t1; t2
  | _, _ -> begin
      let t1' = integralPromotion t1 in
      let t2' = integralPromotion t2 in
      match unrollTypeSkel t1', unrollTypeSkel t2' with
        TInt(IULongLong, _), _ -> checkToInt t2'; t1'
      | _, TInt(IULongLong, _) -> checkToInt t1'; t2'

      | TInt(ILongLong,_), _
            when bitsSizeOf t1' <= bitsSizeOf t2' &&
	      (not (isSignedInteger t2')) -> TInt(IULongLong,[])
      | _, TInt(ILongLong,_)
            when bitsSizeOf t2' <= bitsSizeOf t1' &&
	      (not (isSignedInteger t1')) -> TInt(IULongLong,[])

      | TInt(ILongLong, _), _ -> checkToInt t2'; t1'
      | _, TInt(ILongLong, _) -> checkToInt t1'; t2'

      | TInt(IULong, _), _ -> checkToInt t2'; t1'
      | _, TInt(IULong, _) -> checkToInt t1'; t2'


      | TInt(ILong,_), TInt(IUInt,_)
            when bitsSizeOf t1' <= bitsSizeOf t2' -> TInt(IULong,[])
      | TInt(IUInt,_), TInt(ILong,_)
            when bitsSizeOf t2' <= bitsSizeOf t1' -> TInt(IULong,[])

      | TInt(ILong, _), _ -> checkToInt t2'; t1'
      | _, TInt(ILong, _) -> checkToInt t1'; t2'

      | TInt(IUInt, _), _ -> checkToInt t2'; t1'
      | _, TInt(IUInt, _) -> checkToInt t1'; t2'

      | TInt(IInt, _), TInt (IInt, _) -> t1'

      | t1, t2 ->
        Kernel.fatal ~current:true "arithmeticConversion %a -> %a@." !pp_typ_ref t1 !pp_typ_ref t2
  end

let isArrayType t = match unrollTypeSkel t with
   | TArray _ -> true
   | _ -> false

let isCharArrayType t = match unrollTypeSkel t with
  | TArray(tau,_,_,_) when isCharType tau -> true
  | _ -> false

let isStructOrUnionType t = match unrollTypeSkel t with
  | TComp _ -> true
  | _ -> false

let isVariadicListType t = match unrollTypeSkel t with
  | TBuiltin_va_list _ -> true
  | _ -> false

let rec isConstantGen f e = match (stripInfo e).enode with
  | Info _ -> assert false
  | Const c -> f c
  | UnOp (_, e, _) -> isConstantGen f e
  | BinOp (_, e1, e2, _) -> isConstantGen f e1 && isConstantGen f e2
  | Lval (Var vi, NoOffset) ->
    (vi.vglob && isArrayType vi.vtype || isFunctionType vi.vtype)
  | Lval _ -> false
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> true
  (* see ISO 6.6.6 *)
  | CastE(t,{ enode = Const(CReal _)}) when isIntegralType t -> true
  | CastE (_, e) -> isConstantGen f e
  | AddrOf (Var vi, off) | StartOf (Var vi, off)
    -> vi.vglob && isConstantOffsetGen f off
  | AddrOf (Mem e, off) | StartOf(Mem e, off)
    -> isConstantGen f e && isConstantOffsetGen f off

and isConstantOffsetGen f = function
    NoOffset -> true
  | Field(_fi, off) -> isConstantOffsetGen f off
  | Index(e, off) -> isConstantGen f e && isConstantOffsetGen f off

let isConstant e = isConstantGen alphatrue e
let isConstantOffset o = isConstantOffsetGen alphatrue o

let isIntegerConstant e =
  isConstantGen
    (function
    | CInt64 _ | CChr _ | CEnum _ -> true
    | CStr _ | CWStr _ | CReal _ -> false) 
    e

let getCompField cinfo fieldName =
  List.find (fun fi -> fi.fname = fieldName) cinfo.cfields

let mkCastT ?(force=false) ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  let loc = e.eloc in
(* Issue #!1546
   let force = force || 
    (* see warning of need_cast function:
       [false] as default value for that option is not safe... *) 
    (match e.enode with | Const(CEnum _) -> false | _ -> true)
  in *)
  if need_cast ~force oldt newt then begin
    let mk_cast exp = (* to new type [newt] *)
      new_exp
        ~loc
        (CastE((type_remove_attributes_for_c_cast newt),exp))
    in
    (* Watch out for constants and cast of cast to pointer *)
    match unrollType newt, e.enode with
    (* In the case were we have a representation for the literal,
       explicitly add the cast. *)
    | TInt(newik, []), Const(CInt64(i, _, None)) -> 
      kinteger64 ~loc ~kind:newik i
    | TPtr _, CastE (_, e') ->
      (match unrollType (typeOf e') with
      | (TPtr _ as typ'') ->
	  (* Old cast can be removed...*)
        if need_cast ~force newt typ'' then mk_cast e'
	else (* In fact, both casts can be removed. *) e'
      | _ -> mk_cast e)
    | _ ->   
	(* Do not remove old casts because they are conversions !!! *)
      mk_cast e
  end else
    e

 let mkCast ?force ~(e: exp) ~(newt: typ) =
   mkCastT ?force ~e ~oldt:(typeOf e) ~newt

(* TODO: unify this with doBinOp in Cabs2cil. *)
 let mkBinOp ~loc op e1 e2 =
   let t1 = typeOf e1 in
   let t2 = typeOf e2 in
   let machdep = false in
   let make_expr common_type res_type =
     constFoldBinOp ~loc machdep op
       (mkCastT e1 t1 common_type)
       (mkCastT e2 t2 common_type)
       res_type
   in
   let doArithmetic () =
     let tres = arithmeticConversion t1 t2 in
     make_expr tres tres
   in
   let doArithmeticComp () =
     let tres = arithmeticConversion t1 t2 in
     make_expr tres intType
   in
   let doIntegralArithmetic () =
     let tres = arithmeticConversion t1 t2 in
     if isIntegralType tres then
       make_expr tres tres
     else 
       Kernel.fatal ~current:true "mkBinOp: %a" !pp_exp_ref (dummy_exp(BinOp(op,e1,e2,intType)))
   in
   match op with
       (Mult|Div) -> doArithmetic ()
     | (Mod|BAnd|BOr|BXor|LAnd|LOr) -> doIntegralArithmetic ()
     | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result
                             * has the same type as the left hand side *)
       if msvcMode () then
        (* MSVC has a bug. We duplicate it here *)
         doIntegralArithmetic ()
       else
         let t1' = integralPromotion t1 in
	 let t2' = integralPromotion t2 in
	 constFoldBinOp ~loc machdep op
           (mkCastT e1 t1 t1') (mkCastT e2 t2 t2') t1'
     | (PlusA|MinusA)
         when isArithmeticType t1 && isArithmeticType t2 -> doArithmetic ()
     | (PlusPI|MinusPI|IndexPI) when isPointerType t1 && isIntegralType t2 ->
       constFoldBinOp ~loc machdep op e1 e2 t1
     | MinusPP when isPointerType t1 && isPointerType t2 ->
       (* NB: Same as cabs2cil. Check if this is really what the standard says*)
       constFoldBinOp ~loc machdep op e1 (mkCastT e2 t2 t1) intType
     | (Eq|Ne|Lt|Le|Ge|Gt)
         when isArithmeticType t1 && isArithmeticType t2 ->
       doArithmeticComp ()
     | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
       constFoldBinOp ~loc machdep op
         (mkCastT e1 t1 theMachine.upointType)
         (mkCastT e2 t2 theMachine.upointType)
         intType
     | (Eq|Ne) when isPointerType t1 && isZero e2 ->
       constFoldBinOp ~loc machdep op
         e1 (mkCastT (zero ~loc)theMachine.upointType t1) intType
     | (Eq|Ne) when isPointerType t2 && isZero e1 ->
       constFoldBinOp ~loc machdep op
         (mkCastT (zero ~loc)theMachine.upointType t2) e2 intType
     | (Eq|Ne) when isVariadicListType t1 && isZero e2 ->
       Kernel.debug ~level:3 "Comparison of va_list and zero";
       constFoldBinOp ~loc machdep op e1
         (mkCastT (zero ~loc)theMachine.upointType t1) intType
     | (Eq|Ne) when isVariadicListType t2 && isZero e1 ->
       Kernel.debug ~level:3 "Comparison of zero and va_list";
       constFoldBinOp ~loc machdep op
         (mkCastT (zero ~loc)theMachine.upointType t2) e2 intType
     | _ ->
       Kernel.fatal ~current:true "mkBinOp: %a" 
	 !pp_exp_ref (dummy_exp(BinOp(op,e1,e2,intType)))


 type existsAction =
     ExistsTrue                          (* We have found it *)
   | ExistsFalse                         (* Stop processing this branch *)
   | ExistsMaybe                         (* This node is not what we are
					  * looking for but maybe its
					  * successors are *)
 let existsType (f: typ -> existsAction) (t: typ) : bool =
   let memo : (int, unit) Hashtbl.t = Hashtbl.create 17 in  (* Memo table *)
   let rec loop t =
     match f t with
       ExistsTrue -> true
     | ExistsFalse -> false
     | ExistsMaybe ->
	 (match t with
	   TNamed (t', _) -> loop t'.ttype
	 | TComp (c, _,_) -> loopComp c
	 | TArray (t', _, _, _) -> loop t'
	 | TPtr (t', _) -> loop t'
	 | TFun (rt, args, _, _) ->
	     (loop rt || List.exists (fun (_, at, _) -> loop at)
	       (argsToList args))
	 | _ -> false)
   and loopComp c =
     if Hashtbl.mem memo c.ckey then
       (* We are looping, the answer must be false *)
       false
     else begin
       Hashtbl.add memo c.ckey ();
       List.exists (fun f -> loop f.ftype) c.cfields
     end
   in
   loop t


 (* Try to do an increment, with constant folding *)
 let increm (e: exp) (i: int) =
   let e' = constFold false e in
   let et = typeOf e' in
   let bop = if isPointerType et then PlusPI else PlusA in
   let i = match et with
     | TInt (k, _) | TEnum ({ekind = k },_) -> kinteger k ~loc:e.eloc i
     | _ -> integer ~loc:e.eloc i
   in
   constFoldBinOp ~loc:e.eloc false bop e' i et

 (* Try to do an increment, with constant folding *)
 let increm64 (e: exp) i =
   let et = typeOf e in
   let bop = if isPointerType et then PlusPI else PlusA in
   constFold
     false
     (new_exp ~loc:e.eloc (BinOp(bop, e, kinteger64 ~loc:e.eloc i, et)))

 exception LenOfArray
 let lenOfArray64 eo =
   match eo with
     None -> raise LenOfArray
   | Some e -> begin
       match (constFold true e).enode with
       | Const(CInt64(ni, _, _)) when Integer.ge ni Integer.zero ->
	   ni
       | _ -> raise LenOfArray
     end
 let lenOfArray eo = Integer.to_int (lenOfArray64 eo)

(*** Make an initializer for zeroe-ing a data type ***)
let rec makeZeroInit ~loc (t: typ) : init =
  match unrollType t with
    TInt (ik, _) ->
      SingleInit (new_exp ~loc (Const(CInt64(Integer.zero, ik, None))))
  | TFloat(fk, _) -> SingleInit(new_exp ~loc (Const(CReal(0.0, fk, None))))
  | TEnum _ -> SingleInit (zero ~loc)
  | TComp (comp, _, _) as t' when comp.cstruct ->
      let inits =
        List.fold_right
          (fun f acc ->
            if f.fname <> missingFieldName then
              (Field(f, NoOffset), makeZeroInit ~loc f.ftype) :: acc
            else
              acc)
          comp.cfields []
      in
      CompoundInit (t', inits)
  | TComp (comp, _, _) when not comp.cstruct ->
      let fstfield, _rest =
        match comp.cfields with
          f :: rest -> f, rest
        | [] -> Kernel.fatal ~current:true "Cannot create init for empty union"
      in
      let fieldToInit =
          (* ISO C99 [6.7.8.10] says that the first field of the union
             is the one we should initialize. *)
          fstfield
      in
      CompoundInit(t, [(Field(fieldToInit, NoOffset),
                        makeZeroInit ~loc fieldToInit.ftype)])
  | TArray(bt, Some len, _, _) as t' ->
      let n =
        match constFoldToInt len with
        | Some n -> Integer.to_int n
        | _ -> Kernel.fatal ~current:true "Cannot understand length of array"
      in
      let initbt = makeZeroInit ~loc bt in
      let rec loopElems acc i =
        if i < 0 then acc
        else loopElems ((Index(integer ~loc i, NoOffset), initbt) :: acc) (i - 1)
      in
      CompoundInit(t', loopElems [] (n - 1))

   | TArray (_bt, None, _, _) as t' ->
       (* Unsized array, allow it and fill it in later
	* (see cabs2cil.ml, collectInitializer) *)
       CompoundInit (t', [])

   | TPtr _ as t ->
     SingleInit(
       if theMachine.insertImplicitCasts then mkCast (zero ~loc) t
       else zero ~loc)
   | x -> Kernel.fatal ~current:true "Cannot initialize type: %a" !pp_typ_ref x

 (** Fold over the list of initializers in a Compound (not also the nested
  * ones). [doinit] is called on every present initializer, even if it is of
  * compound type. The parameters of [doinit] are: the offset in the compound
  * (this is [Field(f,NoOffset)] or [Index(i,NoOffset)]), the initializer
  * value, expected type of the initializer value, accumulator. In the case of
  * arrays there might be missing zero-initializers at the end of the list.
  * These are scanned only if [implicit] is true. This is much like
  * [List.fold_left] except we also pass the type of the initializer. *)
 let foldLeftCompound
     ~(implicit: bool)
     ~(doinit: offset -> init -> typ -> 'a -> 'a)
     ~(ct: typ)
     ~(initl: (offset * init) list)
     ~(acc: 'a) : 'a =
   match unrollType ct with
   | TArray(bt, leno, _, _) -> begin
     let default () =
       (* iter over the supplied initializers *)
       List.fold_left (fun acc (o, i) -> doinit o i bt acc) acc initl
     in
     if implicit then
       match leno with
       | Some lene -> begin
	 match constFoldToInt lene with
	 | Some i ->
	   let len_array = Integer.to_int i in
	   let len_init = List.length initl in
	   if len_array <= len_init then
             default () (* enough elements in the initializers list *)
           else
             (* Some initializers are missing. Iterate over all the indexes in
                the array, and use either the supplied initializer, or a generic
                zero one.  *)
             let loc = CurrentLoc.get () in
	     let zinit = makeZeroInit ~loc bt in
             let acc = ref acc in
             let initl = ref initl in
             (* Is [off] the offset for the index [i] we are currently at.
                Works because [initl] is sorted by Cabs2cil.*)
             let good_offset i off = match off with
               | Index (i', NoOffset) ->
                 Integer.(equal (Extlib.the (constFoldToInt i')) (of_int i))
               | _ -> Kernel.fatal ~current:true
                 "Invalid initializer"
             in
             for i = 0 to len_array - 1 do
               match !initl with
               | (off, init) :: qinitl when good_offset i off->
                 acc := doinit off init bt !acc;
                 initl := qinitl
               | _ ->
                 acc := doinit (Index(integer ~loc i, NoOffset)) zinit bt !acc
             done;
             assert (!initl = []);
             !acc
	 | _ -> Kernel.fatal ~current:true
	   "foldLeftCompoundAll: array with initializer and non-constant length"
       end
       | _ -> Kernel.fatal ~current:true
         "foldLeftCompoundAll: TArray with initializer and no length"
     else default ()
   end

   | TComp (_comp, _, _) ->
       let getTypeOffset = function
	   Field(f, NoOffset) -> f.ftype
	 | _ -> Kernel.fatal ~current:true "foldLeftCompound: malformed initializer"
       in
       List.fold_left
	 (fun acc (o, i) -> doinit o i (getTypeOffset o) acc) acc initl

   | _ -> Kernel.fatal ~current:true "Type of Compound is not array or struct or union"




 let rec isCompleteType ?(allowZeroSizeArrays=false) t =
   match unrollType t with
   | TArray(_t, None, _, _) -> false
   | TArray(_t, Some z, _, _) when isZero z -> allowZeroSizeArrays
   | TComp (comp, _, _) -> (* Struct or union *)
       comp.cdefined &&
       List.for_all
         (fun fi -> isCompleteType ~allowZeroSizeArrays fi.ftype) comp.cfields
   | _ -> true


 (* makes sure that the type of a C variable and the type of its associated
   logic variable -if any- stay synchronized. See bts 1538 *)
 let update_var_type v t =
   v.vtype <- t;
   match v.vlogic_var_assoc with
     | None -> ()
     | Some lv -> lv.lv_type <- Ctype t

 (** Uniquefy the variable names *)
 let uniqueVarNames (f: file) : unit =
   (* Setup the alpha conversion table for globals *)
   let gAlphaTable
       : (string, location Alpha.alphaTableData ref) Hashtbl.t 
       = Hashtbl.create 113 in
   (* Keep also track of the global names that we have used. Map them to the
      variable ID. We do this only to check that we do not have two globals
      with the same name. *)
   let globalNames: (string, int) Hashtbl.t = Hashtbl.create 113 in
   (* Scan the file and add the global names to the table *)
   iterGlobals f
     (function
       | GVarDecl(vi, _) | GVar(vi, _, _)
       | GFunDecl(_, vi, _) | GFun({svar = vi}, _) ->
	   (* See if we have used this name already for something else *)
	   (try
	     let oldid = Hashtbl.find globalNames vi.vname in
	     if oldid <> vi.vid && not vi.vinline then
	       Kernel.warning
		 "The name %s is used for two distinct globals" vi.vname
	     (* Here if we have used this name already. Go ahead *)
	   with Not_found -> begin
	     (* Here if this is the first time we define a name *)
	     Hashtbl.add globalNames vi.vname vi.vid;
	     (* And register it *)
             Alpha.registerAlphaName gAlphaTable vi.vname (CurrentLoc.get ())
	   end)
       | _ -> ());

   (* Now we must scan the function bodies and rename the locals *)
   iterGlobals f
     (function
	 GFun(fdec, l) -> begin
	   CurrentLoc.set l;
	   (* Setup an undo list to be able to revert the changes to the
	    * global alpha table *)
	   let undolist = ref [] in
	   (* Process one local variable *)
	   let processLocal (v: varinfo) =
             let lookupname = v.vname in
             let data = CurrentLoc.get () in
	     let newname, oldloc =
               Alpha.newAlphaName
                 ~alphaTable:gAlphaTable ~undolist ~lookupname ~data
	     in
	     if false && newname <> v.vname then (* Disable this warning *)
	       Kernel.warning
		 "Changing the name of local %s in %s to %s \
                  (due to duplicate at %a)"
		 v.vname
		 fdec.svar.vname
		 newname Location.pretty oldloc ;
	     v.vname <- newname
	   in
	   (* Do the formals first *)
	   List.iter processLocal fdec.sformals;
	   (* Fix the type again *)
	   setFormals fdec fdec.sformals;
	   (* And now the locals *)
	   List.iter processLocal fdec.slocals;
	   (* Undo the changes to the global table *)
           Alpha.undoAlphaChanges gAlphaTable !undolist;
	   ()
	 end
       | _ -> ());
   ()

let is_case_label l = match l with
  | Case _ | Default _ -> true
  | _ -> false

let initCIL ~initLogicBuiltins machdep =
  if not (TheMachine.is_computed ()) then begin
    (* Set the machine *)
    theMachine.theMachine <- machdep;
    (* Pick type for string literals *)
    theMachine.stringLiteralType <-
      if theMachine.theMachine.const_string_literals then charConstPtrType
      else charPtrType;
    (* Find the right ikind given the size *)
    let findIkindSz (unsigned: bool) (sz: int) : ikind =
      (* Test the most common sizes first *)
      if sz = theMachine.theMachine.sizeof_int then
        if unsigned then IUInt else IInt
      else if sz = theMachine.theMachine.sizeof_long then
        if unsigned then IULong else ILong
      else if sz = 1 then
        if unsigned then IUChar else IChar
      else if sz = theMachine.theMachine.sizeof_short then
        if unsigned then IUShort else IShort
      else if sz = theMachine.theMachine.sizeof_longlong then
        if unsigned then IULongLong else ILongLong
      else
        Kernel.fatal ~current:true "initCIL: cannot find the right ikind for size %d\n" sz
    in
    (* Find the right ikind given the name *)
    let findIkindName (name: string) : ikind =
      (* Test the most common sizes first *)
      if name = "int" then IInt
      else if name = "unsigned int" then IUInt
      else if name = "long" then ILong
      else if name = "unsigned long" then IULong
      else if name = "short" then IShort
      else if name = "unsigned short" then IUShort
      else if name = "char" then IChar
      else if name = "unsigned char" then IUChar
      else
	Kernel.fatal
          ~current:true "initCIL: cannot find the right ikind for type %s" name
    in
    theMachine.upointKind <- findIkindSz true theMachine.theMachine.sizeof_ptr;
    theMachine.upointType <- TInt(theMachine.upointKind, []);
    theMachine.kindOfSizeOf <-
      findIkindName theMachine.theMachine.size_t;
    theMachine.typeOfSizeOf <- TInt(theMachine.kindOfSizeOf, []);
    theMachine.wcharKind <- findIkindName theMachine.theMachine.wchar_t;
    theMachine.wcharType <- TInt(theMachine.wcharKind, []);
    theMachine.ptrdiffKind <- findIkindName theMachine.theMachine.ptrdiff_t;
    theMachine.ptrdiffType <- TInt(theMachine.ptrdiffKind, []);
    theMachine.underscore_name <-
      theMachine.theMachine.Cil_types.underscore_name;
    theMachine.useLogicalOperators <- false (* do not use lazy LAND and LOR *);

    (*nextGlobalVID <- 1 ;
    nextCompinfoKey <- 1;*)

    (* Have to be marked before calling [init*Builtins] below. *)
    TheMachine.mark_as_computed ();
    (* projectify theMachine *)
    copyMachine theMachine !theMachineProject;

    init_builtins ();

    Logic_env.Builtins.extend initLogicBuiltins;

  end


(* We want to bring all type declarations before the data declarations. This
 * is needed for code of the following form:

   int f(); // Prototype without arguments
   typedef int FOO;
   int f(FOO x) { ... }

   In CIL the prototype also lists the type of the argument as being FOO,
   which is undefined.

   There is one catch with this scheme. If the type contains an array whose
   length refers to variables then those variables must be declared before
   the type *)

let pullTypesForward = true


    (* Scan a type and collect the variables that are refered *)
class getVarsInGlobalClass (pacc: varinfo list ref) = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    pacc := vi :: !pacc;
    SkipChildren

  method! vglob = function
      GType _ | GCompTag _ -> DoChildren
    | _ -> SkipChildren

end

let getVarsInGlobal (g : global) : varinfo list =
  let pacc : varinfo list ref = ref [] in
  let v : cilVisitor = new getVarsInGlobalClass pacc in
  ignore (visitCilGlobal v g);
  !pacc

let pushGlobal (g: global)
               ~(types:global list ref)
               ~(variables: global list ref) =
  if not pullTypesForward then
    variables := g :: !variables
  else
    begin
      (* Collect a list of variables that are refered from the type. Return
       * Some if the global should go with the types and None if it should go
       * to the variables. *)
      let varsintype : (varinfo list * location) option =
        match g with
          GType (_, l) | GCompTag (_, l) -> Some (getVarsInGlobal g, l)
        | GEnumTag (_, l) | GPragma (Attr("pack", _), l)
        | GCompTagDecl (_, l) | GEnumTagDecl (_, l) -> Some ([], l)
          (** Move the warning pragmas early
        | GPragma(Attr(s, _), l) when hasPrefix "warning" s -> Some ([], l)
          *)
        | _ -> None (* Does not go with the types *)
      in
      match varsintype with
      None -> variables := g :: !variables
    | Some (vl, loc) ->
        types :=
           (* insert declarations for referred variables ('vl'), before
            * the type definition 'g' itself *)
          let aux acc v =
            if isFunctionType v.vtype
            then GFunDecl (empty_funspec (),v, loc) :: acc
            else GVarDecl (v, loc) :: acc
          in
          g :: (List.fold_left aux !types vl)
  end


type formatArg =
    Fe of exp
  | Feo of exp option  (** For array lengths *)
  | Fu of unop
  | Fb of binop
  | Fk of ikind
  | FE of exp list (** For arguments in a function call *)
  | Ff of (string * typ * attributes) (** For a formal argument *)
  | FF of (string * typ * attributes) list (* For formal argument lists *)
  | Fva of bool (** For the ellipsis in a function type *)
  | Fv of varinfo
  | Fl of lval
  | Flo of lval option (** For the result of a function call *)
  | Fo of offset
  | Fc of compinfo
  | Fi of instr
  | FI of instr list
  | Ft of typ
  | Fd of int
  | Fg of string
  | Fs of stmt
  | FS of stmt list
  | FA of attributes

  | Fp of attrparam
  | FP of attrparam list

  | FX of string

let d_formatarg fmt = function
    Fe e -> fprintf fmt "Fe(%a)" !pp_exp_ref e
  | Feo None -> fprintf fmt "Feo(None)"
  | Feo (Some e) -> fprintf fmt "Feo(%a)" !pp_exp_ref e
  | FE _ -> fprintf fmt "FE()"
  | Fk _ik -> fprintf fmt "Fk()"
  | Fva b -> fprintf fmt "Fva(%b)" b
  | Ff (an, _, _) -> fprintf fmt "Ff(%s)" an
  | FF _ -> fprintf fmt "FF(...)"
  | FA _ -> fprintf fmt "FA(...)"
  | Fu _uo -> fprintf fmt "Fu()"
  | Fb _bo -> fprintf fmt "Fb()"
  | Fv v -> fprintf fmt "Fv(%s)" v.vname
  | Fl l -> fprintf fmt "Fl(%a)" !pp_lval_ref l
  | Flo None -> fprintf fmt "Flo(None)"
  | Flo (Some l) -> fprintf fmt "Flo(%a)" !pp_lval_ref l
  | Fo _o -> fprintf fmt "Fo"
  | Fc ci -> fprintf fmt "Fc(%s)" ci.cname
  | Fi _i -> fprintf fmt "Fi(...)"
  | FI _i -> fprintf fmt "FI(...)"
  | Ft t -> fprintf fmt "Ft(%a)" !pp_typ_ref t
  | Fd n -> fprintf fmt "Fd(%d)" n
  | Fg s -> fprintf fmt "Fg(%s)" s
  | Fp _ -> fprintf fmt "Fp(...)"
  | FP _n -> fprintf fmt "FP(...)"
  | Fs _ -> fprintf fmt "FS"
  | FS _ -> fprintf fmt "FS"

  | FX _ -> fprintf fmt "FX()"

let make_temp_logic_var =
  let counter = ref 0 in
  fun ty ->
    incr counter;
    let name = "__framac_tmp" ^ (string_of_int !counter) in
    Cil_const.make_logic_var_local name ty

let extract_varinfos_from_exp vexp =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = Varinfo.Set.empty;
    method varinfos = varinfos
    method! vvrbl (symb:varinfo) =
      varinfos <- Varinfo.Set.add symb varinfos;
      SkipChildren
  end
  in ignore (visitCilExpr (visitor :> nopCilVisitor) vexp) ;
    visitor#varinfos

let extract_varinfos_from_lval vlval =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = Varinfo.Set.empty;
    method varinfos = varinfos
    method! vvrbl (symb:varinfo) =
      varinfos <- Varinfo.Set.add symb varinfos;
      SkipChildren
  end
  in ignore (visitCilLval (visitor :> nopCilVisitor) vlval) ;
    visitor#varinfos

let rec free_vars_term bound_vars t = match t.term_node with
  | TConst _   | TSizeOf _
  | TSizeOfStr _ | TAlignOf _
  | Tnull
  | Ttype _ -> Logic_var.Set.empty
  | TLval lv
  | TAddrOf lv
  | TStartOf lv -> free_vars_lval bound_vars lv
  | TSizeOfE t
  | TAlignOfE t
  | TUnOp (_,t)
  | TCastE (_,t)
  | Tat (t,_)
  | Toffset (_,t)
  | Tbase_addr (_,t)
  | Tblock_length (_,t)
  | TCoerce (t,_)
  | Ttypeof t -> free_vars_term bound_vars t
  | TBinOp (_,t1,t2)
  | TCoerceE (t1,t2) ->
    Logic_var.Set.union
      (free_vars_term bound_vars t1)
      (free_vars_term bound_vars t2)
  | TUpdate (t1,toff,t2) ->
    Logic_var.Set.union
      (Logic_var.Set.union
	 (free_vars_term bound_vars t1)
	 (free_vars_term_offset bound_vars toff))
      (free_vars_term bound_vars t2)
  | Tif (t1,t2,t3) ->
    Logic_var.Set.union
      (free_vars_term bound_vars t1)
      (Logic_var.Set.union
         (free_vars_term bound_vars t2)
         (free_vars_term bound_vars t3))
  | TDataCons(_,t) | Tapp (_,_,t) ->
    List.fold_left
      (fun acc t ->
	Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty t
  | Tlambda(prms,expr) ->
    let bound_vars =
      List.fold_left (Extlib.swap Logic_var.Set.add) bound_vars prms
    in
    free_vars_term bound_vars expr
  | Trange(i1,i2) ->
    let fv = match i1 with
      | None -> Logic_var.Set.empty
      | Some i -> free_vars_term bound_vars i
    in
    (match i2 with
      | None -> fv
      | Some i ->
	Logic_var.Set.union fv (free_vars_term bound_vars i))
  | Tempty_set -> Logic_var.Set.empty
  | Tunion l | Tinter l ->
    List.fold_left
      (fun acc t ->
	Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty
      l
  | Tcomprehension(t,q,p) ->
    let new_bv =
      List.fold_left
	(fun acc v -> Logic_var.Set.add v acc) bound_vars q
    in
    let fv = free_vars_term new_bv t in
    (match p with
      | None -> fv
      | Some p ->
	Logic_var.Set.union fv (free_vars_predicate new_bv p))
  | Tlet(d,b) ->
    let fvd =
      match d.l_body with
	| LBterm term -> free_vars_term bound_vars term
	| LBpred p -> free_vars_predicate bound_vars p
	| LBnone
	| LBreads _ | LBinductive _ ->
          Kernel.fatal ~current:true
            "definition of local variable %s is not a term or a predicate"
            d.l_var_info.lv_name
    in
    let fvb =
      free_vars_term (Logic_var.Set.add d.l_var_info bound_vars) b
    in
    Logic_var.Set.union fvd fvb
  | TLogic_coerce(_,t) -> free_vars_term bound_vars t

and free_vars_lval bv (h,o) =
  Logic_var.Set.union
    (free_vars_lhost bv h) (free_vars_term_offset bv o)

and free_vars_lhost bv = function
  | TVar log_v ->
    if Logic_var.Set.mem log_v bv then
      Logic_var.Set.empty
    else
      Logic_var.Set.singleton log_v
  | TResult _ -> Logic_var.Set.empty
  | TMem t -> free_vars_term bv t

and free_vars_term_offset bv = function
  | TNoOffset -> Logic_var.Set.empty
  | TField (_,o) | TModel(_,o) -> free_vars_term_offset bv o
  | TIndex (t,o) ->
    Logic_var.Set.union
      (free_vars_term bv t)
      (free_vars_term_offset bv o)

and free_vars_predicate bound_vars p = match p.content with
  | Pfalse | Ptrue -> Logic_var.Set.empty
  | Papp (_,_,tl) ->
    List.fold_left
      (fun acc t ->
	Logic_var.Set.union (free_vars_term bound_vars t) acc)
      Logic_var.Set.empty tl
  | Pallocable (_,t) | Pfreeable (_,t)
  | Pvalid (_,t) | Pvalid_read (_,t) | Pinitialized (_,t) | Pdangling (_,t) -> 
    free_vars_term bound_vars t
  | Pseparated seps ->
    List.fold_left
      (fun free_vars tset ->
        Logic_var.Set.union
	  (free_vars_term bound_vars tset) free_vars)
      Logic_var.Set.empty
      seps
  | Pfresh (_,_,t1,t2) 
  | Prel (_,t1,t2)
  | Psubtype (t1,t2)
    ->
    Logic_var.Set.union
      (free_vars_term bound_vars t1)
      (free_vars_term bound_vars t2)
  | Pand (p1,p2)
  | Por (p1,p2)
  | Pxor (p1,p2)
  | Pimplies (p1,p2)
  | Piff (p1,p2) ->
    Logic_var.Set.union
      (free_vars_predicate bound_vars p1)
      (free_vars_predicate bound_vars p2)
  | Pnot p
  | Pat (p,_)
(*  | Pnamed (_,p) *) ->
    free_vars_predicate bound_vars p
  | Pif (t,p1,p2) ->
    Logic_var.Set.union
      (free_vars_term bound_vars t)
      (Logic_var.Set.union
         (free_vars_predicate bound_vars p1)
         (free_vars_predicate bound_vars p2))
  | Plet (d, p) ->
    let fvd =
      match d.l_body with
	| LBterm t -> free_vars_term bound_vars t
	| LBpred p -> free_vars_predicate bound_vars p
	| LBnone
	| LBreads _ | LBinductive _ ->
          Kernel.fatal ~current:true
            "Local logic var %s is not a defined term or predicate"
            d.l_var_info.lv_name
    in
    let new_bv = Logic_var.Set.add d.l_var_info bound_vars in
    Logic_var.Set.union fvd (free_vars_predicate new_bv p)

  | Pforall (lvs,p) | Pexists (lvs,p) ->
    let new_bv =
      List.fold_left
	(Extlib.swap Logic_var.Set.add) bound_vars lvs
    in
    free_vars_predicate new_bv p

let extract_free_logicvars_from_term t =
  free_vars_term Logic_var.Set.empty t

let extract_free_logicvars_from_predicate p =
  free_vars_predicate Logic_var.Set.empty p

let extract_labels_from_annot annot =
  let visitor = object
    inherit nopCilVisitor
    val mutable labels = Logic_label.Set.empty;
    method labels = labels
    method! vlogic_label (label:logic_label) =
      labels <- Logic_label.Set.add label labels;
      SkipChildren
  end
  in ignore (visitCilCodeAnnotation (visitor :> nopCilVisitor) annot) ;
    visitor#labels

let extract_labels_from_term term =
  let visitor = object
    inherit nopCilVisitor
    val mutable labels = Logic_label.Set.empty;
    method labels = labels
    method! vlogic_label (label:logic_label) =
      labels <- Logic_label.Set.add label labels;
      SkipChildren
  end
  in ignore (visitCilTerm (visitor :> nopCilVisitor) term) ;
    visitor#labels

let extract_labels_from_pred pred =
  let visitor = object
    inherit nopCilVisitor
    val mutable labels = Logic_label.Set.empty;
    method labels = labels
    method! vlogic_label (label:logic_label) =
      labels <- Logic_label.Set.add label labels;
      SkipChildren
  end
  in ignore (visitCilPredicateNamed (visitor :> nopCilVisitor) pred) ;
    visitor#labels



let extract_stmts_from_labels labels =
  Logic_label.Set.fold 
    (fun l a -> match l with
       | StmtLabel (stmt) -> Stmt.Set.add !stmt a
       | LogicLabel (Some (stmt), _str) -> Stmt.Set.add stmt a
       | LogicLabel (None, _str) -> a)
    labels Stmt.Set.empty

let close_predicate p =
  let free_vars = free_vars_predicate Logic_var.Set.empty p in
  if Logic_var.Set.is_empty free_vars then p
  else
    { name = [];
      loc = p.loc;
      content = Pforall (Logic_var.Set.elements free_vars, p)}

class alpha_conv tbl ltbl =
object
  inherit nopCilVisitor
  method! vvrbl v =
    try let v' = Hashtbl.find tbl v.vid in ChangeTo v'
    with Not_found -> DoChildren
  method! vlogic_var_use v =
    try let v' = Hashtbl.find ltbl v.lv_id in ChangeTo v'
    with Not_found -> DoChildren
end

let create_alpha_renaming old_args new_args =
  let conversion = Hashtbl.create 7 in
  let lconversion = Hashtbl.create 7 in
  List.iter2
    (fun old_vi new_vi ->
       Hashtbl.add conversion old_vi.vid new_vi;
       match old_vi.vlogic_var_assoc, new_vi.vlogic_var_assoc with
       | None, _ -> () (* nothing to convert in logic spec. *)
       | Some old_lv, Some new_lv ->
           Hashtbl.add lconversion old_lv.lv_id new_lv
       | Some old_lv, None ->
           Hashtbl.add lconversion old_lv.lv_id (cvar_to_lvar new_vi))
    old_args new_args;
  new alpha_conv conversion lconversion

(** Returns [true] whenever the type contains only arithmetic types *)
let is_fully_arithmetic ty =
  not (existsType
         (fun typ -> match typ with
            | TNamed _
            | TComp _
            | TArray _ -> ExistsMaybe
            | TPtr _ | TBuiltin_va_list _ | TFun _ | TVoid _ -> ExistsTrue
            | TEnum _ |TFloat _ | TInt _ ->  ExistsFalse)
         ty)

(* Note: The implementation preserves the order of s.succs in the returned list. *)
let separate_switch_succs s =
  let cases = match s.skind with
    | Switch (_, _, cases, _) -> cases
    | _ -> raise (Invalid_argument "separate_switch_succs")
  in
  let cases_set =
    List.fold_left (fun s stmt -> Stmt.Set.add stmt s) Stmt.Set.empty cases
  in
  let is_in_cases stmt = Stmt.Set.mem stmt cases_set in
  let contains_default_label stmt =
    let is_default_label = function
      | Default _ -> true
      | _ -> false
    in List.exists is_default_label stmt.labels
  in
  let contains_case_label stmt =
    let is_case_label = function
      | Case _ -> true
      | _ -> false
    in List.exists is_case_label stmt.labels
  in
  let default = ref None in
  let set_default s =
    if !default != None
    then Kernel.fatal ~current:true "Bad CFG: switch with multiple non-case successors.";
    default := Some s
  in
  let cases_non_default = ref [] in
  List.iter (fun stmt ->
    if not (is_in_cases stmt)
    then set_default stmt
    else
      if contains_default_label stmt
      then
	(set_default stmt;
	if contains_case_label stmt
	then cases_non_default := stmt::!cases_non_default)
      else
	(assert (contains_case_label stmt);
	 cases_non_default := stmt::!cases_non_default)) s.succs;
  match !default with
  | None ->
    Kernel.fatal ~current:true "Bad CFG: switch with no non-case successors."
  | Some(d) -> (List.rev cases, d)
;;

(** Get the two successors of an If statement *)
let separate_if_succs (s:stmt) : stmt * stmt =
  match s.skind, s.succs with
  | If _, [sthen; selse] -> sthen, selse
  | _-> Kernel.fatal ~current:true "ifSuccs on an invalid If statement."

module Switch_cases =
  State_builder.Hashtbl
    (Stmt.Hashtbl)
    (Datatype.Pair(Datatype.List(Stmt))(Stmt))
    (struct
       let name = "Switch_cases"
       let dependencies = []
       let size = 49
     end)
let () = dependency_on_ast Switch_cases.self
let separate_switch_succs = Switch_cases.memo separate_switch_succs

class dropAttributes ?select () = object
  inherit genericCilVisitor (copy_visit (Project.current ()))
  method! vattr a =
    match select with
      | None -> ChangeTo []
      | Some l ->
          (match a with
            | (Attr (s,_) | AttrAnnot s) when List.mem s l -> ChangeTo []
            | Attr _ | AttrAnnot _ -> DoChildren)
  method! vtype ty = match ty with
    | TNamed (internal_ty, attrs) ->
      let tty = typeAddAttributes attrs internal_ty.ttype in
      (* keep the original type whenever possible *)
      ChangeDoChildrenPost 
        (tty, fun x -> if x == internal_ty.ttype then ty else x)
    | TVoid _ | TInt _ | TFloat _ | TPtr _ | TArray _ | TFun _
    | TComp _ | TEnum _ | TBuiltin_va_list _ -> DoChildren
end

let typeDeepDropAttributes select t =
  let vis = new dropAttributes ~select () in
  visitCilType vis t

let typeDeepDropAllAttributes t =
  let vis = new dropAttributes () in
  visitCilType vis t

(** {1 Deprecated} *)

let lastTermOffset = 
  Kernel.deprecated "Cil.lastTermOffset" ~now:"Logic_const.lastTermOffset" 
    Logic_const.lastTermOffset

let addTermOffset = 
  Kernel.deprecated "Cil.addTermOffset" ~now:"Logic_const.addTermOffset" 
    Logic_const.addTermOffset

let addTermOffsetLval = 
  Kernel.deprecated "Cil.addTermOffsetLval" ~now:"Logic_const.addTermOffsetLval"
    Logic_const.addTermOffsetLval

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
