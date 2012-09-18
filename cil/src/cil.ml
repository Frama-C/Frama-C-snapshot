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

(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)

open Cil_const
open Logic_const
open Format
open Cil_datatype
open Cil_types

(* ************************************************************************* *)
(* Reporting messages *)
(* ************************************************************************* *)

(* A reference to the current location *)
module CurrentLoc = Cil_const.CurrentLoc
let () = Log.set_current_source (fun () -> fst (CurrentLoc.get ()))

let d_loc fmt loc =
  fprintf fmt "%s:%d" (fst loc).Lexing.pos_fname (fst loc).Lexing.pos_lnum
let () = Cil_datatype.Location.pretty_ref := d_loc

let d_thisloc (fmt: formatter) : unit = d_loc fmt (CurrentLoc.get ())

let register_ast_dependencies, add_ast_dependency  =
  let list_self = ref [] in
  (fun ast ->
    State_dependency_graph.Static.add_dependencies ~from:ast !list_self),
  (fun state -> list_self := state :: !list_self)

let voidType = Cil_const.voidType
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let ulongLongType = TInt(IULongLong, [])
let charType = TInt(IChar, [])

let charPtrType = TPtr(charType,[])
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
    { mutable msvcMode: bool; (** Whether the pretty printer should
				  print output for the MS VC
                                  compiler. Default is GCC *)
      mutable useLogicalOperators: bool;
      mutable theMachine: mach;
      (** Cil.initCil will set this to the current machine description. *)
      mutable lowerConstants: bool; (** Do lower constants (default true) *)
      mutable insertImplicitCasts: bool; (** Do insert implicit casts
					     (default true) *)
      mutable little_endian: bool;
      mutable char_is_unsigned: bool;
      mutable underscore_name: bool;
      mutable stringLiteralType: typ;
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

let createMachine () =
  { msvcMode = false;
    useLogicalOperators = false;
    theMachine = Machdep.state.Machdep.gcc;
    lowerConstants = false(*true*);
    insertImplicitCasts = true;
    little_endian = true;
    char_is_unsigned = false;
    underscore_name = true;
    stringLiteralType = charPtrType;
    upointType = voidType;
    wcharKind = IChar;
    wcharType = voidType;
    ptrdiffKind = IChar;
    ptrdiffType = voidType;
    typeOfSizeOf = voidType;
    kindOfSizeOf = IUInt }

let copyMachine src dst =
  dst.msvcMode <- src.msvcMode;
  dst.useLogicalOperators <- src.useLogicalOperators;
  dst.theMachine <- src.theMachine;
  dst.lowerConstants <- src.lowerConstants;
  dst.insertImplicitCasts <- src.insertImplicitCasts;
  dst.little_endian <- src.little_endian;
  dst.char_is_unsigned <- src.char_is_unsigned;
  dst.underscore_name <- src.underscore_name;
  dst.stringLiteralType <- src.stringLiteralType;
  dst.upointType <- src.upointType;
  dst.wcharKind <- src.wcharKind;
  dst.wcharType <- src.wcharType;
  dst.ptrdiffKind <- src.ptrdiffKind;
  dst.ptrdiffType <- src.ptrdiffType;
  dst.typeOfSizeOf <- src.typeOfSizeOf;
  dst.kindOfSizeOf <- src.kindOfSizeOf

(* A few globals that control the interpretation of C source *)
let theMachine = createMachine ()

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
  State_dependency_graph.Static.add_dependencies
    ~from:selfMachine
    Logic_env.builtin_states

let selfMachine_is_computed = TheMachine.is_computed

let set_msvcMode b = theMachine.msvcMode <- b

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


let compareLoc = Cil_datatype.Location.compare
(*   let namecmp = compare a.file b.file in *)
(*   if namecmp != 0 *)
(*   then namecmp *)
(*   else *)
(*     let linecmp = a.line - b.line in *)
(*     if linecmp != 0 *)
(*     then linecmp *)
(*     else a.byte - b.byte *)

let argsToList : (string * typ * attributes) list option
                  -> (string * typ * attributes) list
    = function
    None -> []
  | Some al -> al


(* A hack to allow forward reference of d_exp *)
let pd_exp : (formatter -> exp -> unit) ref =
  ref (fun _ -> Kernel.fatal "pd_exp not initialized")
let pd_global : (formatter -> global -> unit) ref =
  ref (fun _ -> Kernel.fatal "pd_global not initialized")
let pd_type: (formatter -> typ -> unit) ref =
  ref (fun _ -> Kernel.fatal "pd_type not initialized")
let pd_ikind =  
  ref (fun _ -> Kernel.fatal "pd_ikind not initialized")
let pd_attr =  
  ref (fun _ -> Kernel.fatal "pd_attr not initialized")
let pd_attrlist =  
  ref (fun _ -> Kernel.fatal "pd_attrlist not initialized")

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

 let dropAttributes (anl: string list) (al: attributes) =
   List.fold_left (fun acc an -> dropAttribute an acc) al anl

 let hasAttribute (s: string) (al: attribute list) : bool =
   List.exists (fun a -> attributeName a = s) al

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


 let typeRemoveAttributes (anl: string list) t =
   let drop (al: attributes) = dropAttributes anl al in
   match t with
     TVoid a -> TVoid (drop a)
   | TInt (ik, a) -> TInt (ik, drop a)
   | TFloat (fk, a) -> TFloat (fk, drop a)
   | TEnum (enum, a) -> TEnum (enum, drop a)
   | TPtr (t, a) -> TPtr (t, drop a)
   | TArray (t, l, s, a) -> TArray (t, l, s, drop a)
   | TFun (t, args, isva, a) -> TFun(t, args, isva, drop a)
   | TComp (comp, s, a) -> TComp (comp, s, drop a)
   | TNamed (t, a) -> TNamed (t, drop a)
   | TBuiltin_va_list a -> TBuiltin_va_list (drop a)



 (* JS: build an attribute annotation from [s]. *)
 let mkAttrAnnot s = "/*@ " ^ s ^ " */"

(* Internal attributes. Won't be pretty-printed *)
let reserved_attributes = ref []
let register_shallow_attribute s = reserved_attributes:=s::!reserved_attributes

  
let type_remove_qualifier_attributes = 
  typeRemoveAttributes qualifier_attributes

    
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
 let makeVarinfo ?(logic=false) ?(generated=true) global formal name typ =
   (* Strip const from type for locals *)
   let vi =
     { vorig_name = name;
       vname = name;
       vid   = -1;
       vglob = global;
       vdefined = false;
       vformal = formal;
       vgenerated = generated;
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
       vlogic = logic;
       vlogic_var_assoc = None
     }
   in
   set_vid vi;
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
 let () = add_ast_dependency selfFormalsDecl

 let makeFormalsVarDecl (n,t,a) =
   let vi = makeVarinfo ~generated:false false true n t in
   vi.vattr <- a;
   vi

 let setFormalsDecl vi typ =
   match unrollType typ with
   | TFun(_, Some args, _, _) ->
     FormalsDecl.replace vi (List.map makeFormalsVarDecl args)
   | TFun(_,None,_,_) -> ()
   | _ -> error
       "trying to assigns formal parameters to an object which is not a function prototype"

 let getFormalsDecl vi = FormalsDecl.find vi

 let unsafeSetFormalsDecl vi args =
   FormalsDecl.replace vi args

 let removeFormalsDecl vi = FormalsDecl.remove vi

 let iterFormalsDecl = FormalsDecl.iter

 let () = Cil_datatype.Kf.set_formal_decls := unsafeSetFormalsDecl

(* Set the formals and re-create the function name based on the information*)
 let setFormals (f: fundec) (forms: varinfo list) =
   unsafeSetFormalsDecl f.svar forms;
   List.iter (fun v -> v.vformal <- true) forms;
   f.sformals <- forms; (* Set the formals *)
   match unrollType f.svar.vtype with
     TFun(rt, _, isva, fa) ->
       f.svar.vtype <-
	  TFun(rt,
	       Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) forms),
	       isva, fa)
   | _ -> 
     Kernel.abort "Set formals. %s does not have function type" f.svar.vname

 let empty_funspec () =
   { spec_behavior = [];
     spec_variant = None;
     spec_terminates = None;
     spec_complete_behaviors = [];
     spec_disjoint_behaviors = [] }

 let is_empty_funspec (spec : funspec) =
   spec.spec_behavior = [] &&
   spec.spec_variant = None && spec.spec_terminates = None &&
   spec.spec_complete_behaviors = [] && spec.spec_disjoint_behaviors = []

let is_empty_behavior b =
  b.b_assumes = [] && b.b_requires = [] && b.b_post_cond = [] &&
  b.b_assigns = WritesAny && b.b_allocation = FreeAllocAny && b.b_extended = []

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

let inplace_visit =
  fun () ->
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
    is_copy_behavior = false;
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

let copy_visit () =
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
  let temp_memo_logic_var x =
(*    Format.printf "search for %s#%d@." x.lv_name x.lv_id;*)
    let res =
    try Cil_datatype.Logic_var.Hashtbl.find logic_vars x
    with Not_found ->
(*      Format.printf "Not found@.";*)
      let new_x = { x with lv_id = x.lv_id } in
      Cil_datatype.Logic_var.Hashtbl.add logic_vars x new_x;
      Cil_datatype.Logic_var.Hashtbl.add orig_logic_vars new_x x;
      new_x
    in
(*    Format.printf "res is %s#%d@." res.lv_name res.lv_id;*)
    res
  in
  let temp_memo_varinfo x =
    try Cil_datatype.Varinfo.Hashtbl.find varinfos x
    with Not_found ->
      let new_x = { x with vid = x.vid } in
      Cil_datatype.Varinfo.Hashtbl.add varinfos x new_x;
      Cil_datatype.Varinfo.Hashtbl.add orig_varinfos new_x x;
      new_x
  in
  let temp_memo_fundec f =
    try Cil_datatype.Varinfo.Hashtbl.find fundecs f.svar
    with Not_found ->
      let v = temp_memo_varinfo f.svar in
      let new_f = { f with svar = v } in
      Cil_datatype.Varinfo.Hashtbl.add fundecs f.svar new_f;
      Cil_datatype.Varinfo.Hashtbl.add orig_fundecs v f;
      new_f
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
    is_copy_behavior = true;
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
    memo_compinfo =
      (fun x ->
         try Cil_datatype.Compinfo.Hashtbl.find compinfos x
         with Not_found ->
           let new_x = { x with ckey = x.ckey } in
           Cil_datatype.Compinfo.Hashtbl.add compinfos x new_x;
           Cil_datatype.Compinfo.Hashtbl.add orig_compinfos new_x x;
           new_x);
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
           let new_x = { x with sid = x.sid } in
           Cil_datatype.Stmt.Hashtbl.add stmts x new_x;
           Cil_datatype.Stmt.Hashtbl.add orig_stmts new_x x;
           new_x);
    memo_fieldinfo =
      (fun x ->
         try Cil_datatype.Fieldinfo.Hashtbl.find fieldinfos x
         with Not_found ->
           let new_x = { x with fname = x.fname } in
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
    memo_kernel_function =
      (fun x ->
        try Cil_datatype.Kf.Hashtbl.find kernel_functions x
        with Not_found ->
          let fundec =
            match x.fundec with
              | Definition (f,l) -> Definition (temp_memo_fundec f,l)
              | Declaration(s,v,p,l) ->
                Declaration(s,temp_memo_varinfo v,p,l)
          in
          let new_x = { x with fundec = fundec } in
          Cil_datatype.Kf.Hashtbl.add kernel_functions x new_x;
          Cil_datatype.Kf.Hashtbl.add orig_kernel_functions new_x x;
          new_x);
    memo_fundec = temp_memo_fundec;
    set_varinfo = Cil_datatype.Varinfo.Hashtbl.replace varinfos;
    set_compinfo = Cil_datatype.Compinfo.Hashtbl.replace compinfos;
    set_enuminfo = Cil_datatype.Enuminfo.Hashtbl.replace enuminfos;
    set_enumitem = Cil_datatype.Enumitem.Hashtbl.replace enumitems;
    set_typeinfo = Cil_datatype.Typeinfo.Hashtbl.replace typeinfos;
    set_logic_info = Cil_datatype.Logic_info.Hashtbl.replace logic_infos;
    set_logic_type_info = 
      Cil_datatype.Logic_type_info.Hashtbl.replace logic_type_infos;
    set_stmt = Cil_datatype.Stmt.Hashtbl.replace stmts;
    set_fieldinfo = Cil_datatype.Fieldinfo.Hashtbl.replace fieldinfos;
    set_model_info = Cil_datatype.Model_info.Hashtbl.replace model_infos;
    set_logic_var = Cil_datatype.Logic_var.Hashtbl.replace logic_vars;
    set_kernel_function = Cil_datatype.Kf.Hashtbl.replace kernel_functions;
    set_fundec =
      (fun x y -> Cil_datatype.Varinfo.Hashtbl.replace fundecs x.svar y);
    set_orig_varinfo = Cil_datatype.Varinfo.Hashtbl.replace orig_varinfos;
    set_orig_compinfo = Cil_datatype.Compinfo.Hashtbl.replace orig_compinfos;
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
    set_orig_logic_var = Cil_datatype.Logic_var.Hashtbl.replace orig_logic_vars;
    set_orig_kernel_function =
      Cil_datatype.Kf.Hashtbl.replace orig_kernel_functions;
    set_orig_fundec = 
      (fun x y -> Cil_datatype.Varinfo.Hashtbl.replace orig_fundecs x.svar y);
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

(* the default visitor does nothing at each node, but does *)
 (* not stop; hence they return true *)
 class internal_genericCilVisitor current_func ?prj behavior: cilVisitor =
 object
   method behavior = behavior

   method project = prj

   method plain_copy_visitor =
     new internal_genericCilVisitor current_func ?prj behavior

   (* list of things to perform on the new project. Done at the end
      of the analysis in order to minimize the number of project changes.
   *)
   val global_tables_action = Queue.create ()

   method fill_global_tables =
    let action () = Queue.iter (fun f -> f()) global_tables_action in
    (match prj with
    | None -> action ()
    | Some prj -> Project.on prj action ());
    Queue.clear global_tables_action

   method get_filling_actions = global_tables_action

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

   initializer 
     if is_copy_behavior behavior then
       match prj with
         | None -> 
           Kernel.fatal
             "Cannot create a copy visitor without an explicit project"
         | Some _ -> ()

 end

 class genericCilVisitor ?prj bhv =
   let current_func = ref None in
   internal_genericCilVisitor current_func ?prj bhv

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
        if def != def' || body != body' then
          Tlet(def',body') else tn

and visitCilLogicLabel vis l =
  doVisitCil vis id vis#vlogic_label childrenLogicLabel l

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

 and visitCilPredicate vis p =
   doVisitCil vis id vis#vpredicate childrenPredicate p

 and visitCilPredicateNamed vis p =
   doVisitCil vis
     id vis#vpredicate_named childrenPredicateNamed p

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
   if f' != f' || a' != a then FreeAlloc(f',a') else fa

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

and visitCilExtended vis (s,id,p as orig) =
  let r = mapNoCopy (visitCilIdPredicate vis) p in
  if r == p then orig else (s,id,r)

and visitCilPredicates vis ps = mapNoCopy (visitCilIdPredicate vis) ps

and visitCilIdPredicate vis ps =
   let p' = visitCilPredicate vis ps.ip_content in
   if p' != ps.ip_content then { ps with ip_content = p' } else ps

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
   doVisitCil vis id vis#vcode_annot childrenCodeAnnot ca

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
  let res = doVisitCil vis id vis#vexpr childrenExp e in
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

   | Asm(sl,isvol,outs,ins,clobs,l) ->
       let outs' = mapNoCopy (fun ((id,s,lv) as pair) ->
				let lv' = fLval lv in
				if lv' != lv then (id,s,lv') else pair) outs in
       let ins'  = mapNoCopy (fun ((id,s,e) as pair) ->
				let e' = fExp e in
				if e' != e then (id,s,e') else pair) ins in
       if outs' != outs || ins' != ins then
	 Asm(sl,isvol,outs',ins',clobs,l) else i
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
	 let stmts' = mapNoCopy (visitCilStmt vis#plain_copy_visitor) stmts in
	 let b' = fBlock b in
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
   v.vtype <- visitCilType vis v.vtype;
   v.vattr <- visitCilAttributes vis v.vattr;
   v.vlogic_var_assoc <-
     optMapNoCopy (visitCilLogicVarDecl vis) v.vlogic_var_assoc;
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
   let f = vis#behavior.memo_fundec f in
   f.svar <- vis#behavior.memo_varinfo f.svar; (* hit the function name *)
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
   f.svar <- visitCilVarDecl vis f.svar; (* hit the function name *)
   (* visit local declarations *)
   f.slocals <- mapNoCopy (visitCilVarDecl vis) f.slocals;
   (* visit the formals *)
   let newformals = mapNoCopy (visitCilVarDecl vis) f.sformals in
   (* Make sure the type reflects the formals *)
   let selection = State_selection.singleton FormalsDecl.self in
   apply_on_project ~selection vis (setFormals f) newformals;
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
   fi.fcomp <- vis#behavior.get_compinfo fi.fcomp;
   fi.ftype <- visitCilType vis fi.ftype;
   fi.fattr <- visitCilAttributes vis fi.fattr;
   fi

 let visitCilFieldInfo vis f =
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
   | GVarDecl(spec, v, l) ->
       let form =
	 try Some (getFormalsDecl v) with Not_found -> None
       in
       let v' = visitCilVarDecl vis v in
       let form' = optMapNoCopy (mapNoCopy (visitCilVarDecl vis)) form in
       let spec' =
	 if isFunctionType v.vtype && not (is_empty_funspec spec) then
	   visitCilFunspec vis spec
	 else begin
	   assert (is_empty_funspec spec);
           if is_copy_behavior vis#behavior then
	     empty_funspec ()
           else spec (* do not need to change it if it's not a copy visitor. *)
	 end
       in
       if v' != v || spec' != spec || form != form' then
	 begin
	   (match form' with None -> ()
	      | Some form' ->
		  Queue.add (fun () -> unsafeSetFormalsDecl v' form')
		    vis#get_filling_actions);
	   GVarDecl (spec', v', l)
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


(* The next compindo identifier to use. Counts up. *)
let nextCompinfoKey =
  let module M =
    State_builder.SharedCounter(struct let name = "compinfokey" end)
  in
  M.next

let bytesSizeOfInt (ik: ikind): int =
  match ik with
  | IChar | ISChar | IUChar -> 1
  | IBool | IInt | IUInt -> theMachine.theMachine.sizeof_int
  | IShort | IUShort -> theMachine.theMachine.sizeof_short
  | ILong | IULong -> theMachine.theMachine.sizeof_long
  | ILongLong | IULongLong -> theMachine.theMachine.sizeof_longlong

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
  My_bigint.pred (My_bigint.shift_left My_bigint.one (My_bigint.of_int n))
let max_unsigned_number nrBits = 
  My_bigint.pred (My_bigint.shift_left My_bigint.one (My_bigint.of_int nrBits))
let min_signed_number nrBits = 
  let n = nrBits-1 in
  My_bigint.neg (My_bigint.shift_left My_bigint.one (My_bigint.of_int n))


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
    My_bigint.shift_left My_bigint.one (My_bigint.of_int nrBits)
  in
  let min_bound = if signed then My_bigint.neg max_strict_bound
    else My_bigint.zero
  in
  let fits = My_bigint.le min_bound i && My_bigint.lt i max_strict_bound in
  Kernel.debug "Fits in %a %s : %b@." !pd_ikind k (My_bigint.to_string i) fits;
  fits


    
(* Represents an integer as for a given kind.
   Returns a flag saying whether the value was changed
   during truncation (because it was too large to fit in k). *)
let truncateInteger64 (k: ikind) i =
  Kernel.debug "Truncate to %a: %s@." !pd_ikind k (My_bigint.to_string i);
  if fitsInInt k i then 
    i,false
  else
    begin 
      let nrBits = My_bigint.of_int (8 * (bytesSizeOfInt k)) in
      let max_strict_bound = My_bigint.shift_left My_bigint.one nrBits in
      let modulo = My_bigint.pos_rem i max_strict_bound in
      let signed = isSigned k in
      if signed 
      then 
        let max_signed_strict_bound = 
          My_bigint.shift_right max_strict_bound My_bigint.one
        in
        if My_bigint.ge modulo max_signed_strict_bound then
          My_bigint.sub modulo max_strict_bound
        else if My_bigint.lt modulo (My_bigint.neg max_signed_strict_bound)
        then My_bigint.add modulo max_strict_bound
        else modulo
        else 
          if My_bigint.lt modulo My_bigint.zero then
            My_bigint.add modulo max_strict_bound
          else
            modulo
    end,
      true

(* Return the smallest kind that will hold the integer's value.
   The kind will be unsigned if the 2nd argument is true *)
let intKindForValue i (unsigned: bool) = 
  if unsigned then
    if fitsInInt IUChar i then IUChar
    else if fitsInInt IUShort i then IUShort
    else if fitsInInt IUInt i then IUInt
    else if fitsInInt IULong i then IULong
    else IULongLong
  else
    if fitsInInt ISChar i then ISChar
    else if fitsInInt IShort i then IShort
    else if fitsInInt IInt i then IInt
    else if fitsInInt ILong i then ILong
    else ILongLong

(* Construct an integer constant with possible truncation *)
let kinteger64_repr ~loc (k: ikind) i repr =
  Kernel.debug "kinteger64_repr %s" (My_bigint.to_string i);
  let i', truncated = truncateInteger64 k i in
  if truncated then
    Kernel.debug ~level:3 "Truncating integer %s to %s"
      (My_bigint.to_string i)
      (My_bigint.to_string i');
  new_exp ~loc (Const (CInt64(i' , k,  repr)))

let kinteger64 ~loc k i = kinteger64_repr ~loc k i None

(* Construct an integer of a given kind. *)
let kinteger ~loc (k: ikind) (i: int) = kinteger64 ~loc k (My_bigint.of_int i)

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer_constant i = CInt64(My_bigint.of_int i, IInt, None)
(* Construct an integer. Use only for values that fit on 31 bits *)
let integer ~loc (i: int) = new_exp ~loc (Const (integer_constant i))

let kfloat ~loc k f = new_exp ~loc (Const (CReal(f,k,None)))

let zero      ~loc = integer ~loc 0
let one       ~loc = integer ~loc 1
let mone      ~loc = integer ~loc (-1)

let integer_lconstant v = TConst (Integer (My_bigint.of_int v,None))

let lconstant ?(loc=Location.unknown) v =
  { term_node = TConst (Integer (v,None)); term_loc = loc;
    term_name = []; term_type = Linteger;}

let lzero ?(loc=Location.unknown) () = lconstant ~loc My_bigint.zero
let lone  ?(loc=Location.unknown) () = lconstant ~loc My_bigint.one
let lmone ?(loc=Location.unknown) () = lconstant ~loc (My_bigint.minus_one)

 (** Given the character c in a (CChr c), sign-extend it to 32 bits.
     (This is the official way of interpreting character constants, according 
     to ISO C 6.4.4.4.10, which says that character constants are chars cast 
     to ints)
     Returns CInt64(sign-extened c, IInt, None) *)
let charConstToInt (c: char) : constant =
  let c' = Char.code c in
  let value =
    if c' < 128
    then My_bigint.of_int c'
    else My_bigint.of_int (c' - 256)
  in
  CInt64(value, IInt, None)


let rec isInteger e = match e.enode with
| Const(CInt64 (n,_,_)) -> Some n
| Const(CChr c) -> isInteger (dummy_exp (Const (charConstToInt c)))
| Const(CEnum {eival = v}) -> isInteger v
| CastE(_, e) -> isInteger e
| _ -> None

 (** Convert a 64-bit int to an OCaml int, or raise an exception if that
     can't be done. *)
let i64_to_int (i: int64) : int =
  let i': int = Int64.to_int i in (* i.e. i' = i mod 2^31 *)
  if i = Int64.of_int i' then i'
  else Kernel.abort "Int constant too large: %Ld\n" i

let isZero (e: exp) : bool = 
  match isInteger e with 
  | None -> false
  | Some i -> My_bigint.equal i My_bigint.zero

let rec isLogicZero t = match t.term_node with
| TConst (Integer (n,_)) -> My_bigint.equal n My_bigint.zero
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

let parseInt (str:string) =
  let hasSuffix str =
    let l = String.length str in
    fun s ->
      let ls = String.length s in
      l >= ls && s = String.uppercase (String.sub str (l - ls) ls)
  in
  let l = String.length str in
  (* See if it is octal or hex *)
  let octalhex = (l >= 1 && String.get str 0 = '0') in
  (* The length of the suffix and a list of possible kinds. See ISO
   * 6.4.4.1 *)
  let hasSuffix = hasSuffix str in
  let suffixlen, kinds =
    if hasSuffix "ULL" || hasSuffix "LLU" then
      3, [IULongLong]
    else if hasSuffix "LL" then
      2, if octalhex then [ILongLong; IULongLong] else [ILongLong]
    else if hasSuffix "UL" || hasSuffix "LU" then
      2, [IULong; IULongLong]
    else if hasSuffix "L" then
      1, if octalhex then [ILong; IULong; ILongLong; IULongLong]
        else [ILong; ILongLong]
    else if hasSuffix "U" then
      1, [IUInt; IULong; IULongLong]
    else
      0, if octalhex || true (* !!! This is against the ISO but it
                              * is what GCC and MSVC do !!! *)
        then [IInt; IUInt; ILong; IULong; ILongLong; IULongLong]
        else [IInt; ILong; IUInt; ILongLong]
  in
  (* Convert to integer. To prevent overflow we do the arithmetic
   * on Big_int and we take care of overflow. We work only with
   * positive integers since the lexer takes care of the sign *)
  let rec toInt base (acc: My_bigint.t) (idx: int) : 
      My_bigint.t =
    let doAcc what =
      let acc' =
        My_bigint.add what (My_bigint.mul base acc) in
      toInt base acc' (idx + 1)
    in
    if idx >= l - suffixlen then begin
      acc
    end else
      let ch = String.get str idx in
      if ch >= '0' && ch <= '9' then
        doAcc (My_bigint.of_int (Char.code ch - Char.code '0'))
      else if  ch >= 'a' && ch <= 'f'  then
        doAcc (My_bigint.of_int (10 + Char.code ch - Char.code 'a'))
      else if  ch >= 'A' && ch <= 'F'  then
        doAcc (My_bigint.of_int (10 + Char.code ch - Char.code 'A'))
      else
        fatal "Invalid integer constant: %s" str
  in
  let i =
    if octalhex then
      if l >= 2 &&
        (let c = String.get str 1 in c = 'x' || c = 'X') then
        toInt My_bigint.small_nums.(16) My_bigint.zero 2
      else
        toInt My_bigint.small_nums.(8) My_bigint.zero 1
    else
      toInt My_bigint.small_nums.(10) My_bigint.zero 0
  in
  i,kinds

let parseIntLogic ~loc str = 
  let i,_= parseInt str in
  { term_node = TConst (Integer (i,Some str)) ; term_loc = loc;
    term_name = []; term_type = Linteger;}
  
let parseIntExp ~loc (str: string) : exp =
  try
  let i,kinds = parseInt str in
  let res =
    let rec loop = function
      | k::rest ->
        if fitsInInt k i then (* i fits in the current type. *)
          kinteger64_repr ~loc k i (Some str)
        else loop rest
      | [] ->
        Kernel.fatal ~source:(fst loc) "Cannot represent the integer %s" str
    in
    loop kinds
  in
  res
  with Failure "" as e ->
    Kernel.warning "int_of_string %s (%s)\n" str (Printexc.to_string e);
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

 let mkEmptyStmt ?ghost ?(loc=Location.unknown) () = mkStmt ?ghost (Instr (Skip loc))
 let mkStmtOneInstr ?ghost ?valid_sid i = mkStmt ?ghost ?valid_sid (Instr i)

 let dummyInstr = Asm([], ["dummy statement!!"], [], [], [], Location.unknown)
 let dummyStmt = mkStmt (Instr dummyInstr)

 (***
 let compactStmts (b: stmt list) : stmt list =
       (* Try to compress statements. Scan the list of statements and remember
	* the last instrunction statement encountered, along with a Clist of
	* instructions in it. *)
   let rec compress (lastinstrstmt: stmt) (* Might be dummStmt *)
		    (lastinstrs: instr Clist.clist)
		    (body: stmt list) =
     let finishLast (tail: stmt list) : stmt list =
       if lastinstrstmt == dummyStmt then tail
       else begin
	 lastinstrstmt.skind <- Instr (Clist.toList lastinstrs);
	 lastinstrstmt :: tail
       end
     in
     match body with
       [] -> finishLast []
     | ({skind=Instr il} as s) :: rest ->
	 let ils = Clist.fromList il in
	 if lastinstrstmt != dummyStmt && s.labels == [] then
	   compress lastinstrstmt (Clist.append lastinstrs ils) rest
	 else
	   finishLast (compress s ils rest)

     | s :: rest ->
	 let res = s :: compress dummyStmt Clist.empty rest in
	 finishLast res
   in
   compress dummyStmt Clist.empty b
 ***)


 (** Get the full name of a comp *)
 let compFullName comp =
   (if comp.cstruct then "struct " else "union ") ^ comp.cname


 let missingFieldName = "" (* "___missing_field_name"*)

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
 let copyCompInfo (ci: compinfo) (n: string) : compinfo =
   let ci' = {ci with cname = n; ckey = nextCompinfoKey (); } in
   (* Copy the fields and set the new pointers to parents *)
   ci'.cfields <- List.map (fun f -> {f with fcomp = ci'}) ci'.cfields;
   ci'

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

 let isVoidType t =
   match unrollTypeSkel t with
     TVoid _ -> true
   | _ -> false
 let isVoidPtrType t =
   match unrollTypeSkel t with
     TPtr(tau,_) when isVoidType tau -> true
   | _ -> false

let isPtrType ct =
  match unrollTypeSkel ct with
    TPtr _ -> true
  | _ -> false

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
  [ mkStmt
      (Loop ([],
	     mkBlock
	       (mkStmt
		  (If(guard,
                      mkBlock [ mkEmptyStmt () ],
                      mkBlock [ mkStmt (Break guard.eloc)], guard.eloc)) ::
                  body), guard.eloc, None, None)) ]

 let mkFor ~(start: stmt list) ~(guard: exp) ~(next: stmt list)
	   ~(body: stmt list) : stmt list =
   (start @
      (mkWhile guard (body @ next)))

 let mkForIncr ~(iter : varinfo) ~(first: exp) ~stopat:(past: exp) ~(incr: exp)
     ~(body: stmt list) : stmt list =
  (* See what kind of operator we need *)
   let compop, nextop =
     match unrollTypeSkel iter.vtype with
         TPtr _ -> Lt, PlusPI
       | _ -> Lt, PlusA
   in
   mkFor
     [ mkStmt (Instr (Set (var iter, first, first.eloc))) ]
     (new_exp ~loc:past.eloc
        (BinOp(compop, new_exp ~loc:past.eloc (Lval(var iter)), past, intType)))
     [ mkStmt
         (Instr
            (Set
               (var iter,
                (new_exp ~loc:incr.eloc
                   (BinOp(nextop,
                          new_exp ~loc:past.eloc (Lval(var iter)),
                          incr,
                         iter.vtype))),
                incr.eloc)))]
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

 (* the name of the C function we call to get ccgr ASTs
 external parse : string -> file = "cil_main"
 *)
 (*
   Pretty Printing
  *)

 let d_ikind fmt c =
   fprintf fmt "%s"
     ( match c with
       | IChar -> "char"
       | IBool -> "_Bool"
       | ISChar -> "signed char"
       | IUChar -> "unsigned char"
       | IInt -> "int"
       | IUInt -> "unsigned int"
       | IShort -> "short"
       | IUShort -> "unsigned short"
       | ILong -> "long"
       | IULong -> "unsigned long"
       | ILongLong ->
	   if theMachine.msvcMode then "__int64" else "long long"
       | IULongLong ->
	   if theMachine.msvcMode then "unsigned __int64"
	   else "unsigned long long")

 let () = pd_ikind := d_ikind

 let d_fkind fmt = function
     FFloat -> fprintf fmt "float"
   | FDouble -> fprintf fmt "double"
   | FLongDouble -> fprintf fmt "long double"

 let d_storage fmt c =
   fprintf fmt "%s"
     ( match c with
       | NoStorage -> ""
       | Static ->  "static "
       | Extern -> "extern "
       | Register -> "register ")


 let pretty_C_constant suffix k fmt i = 
   let nb_signed_bits = 
     My_bigint.pred (My_bigint.of_int (8 * (bytesSizeOfInt k))) 
   in
   let max_strict_signed = My_bigint.shift_left My_bigint.one nb_signed_bits in
   let most_neg = My_bigint.neg max_strict_signed in
   if My_bigint.equal most_neg i then 
     (* sm: quirk here: if you print -2147483648 then this is two
        tokens in C, and the second one is too large to represent in 
        a signed int.. 
        so we do what's done in limits.h, and print (-2147483467-1); *)
     (* in gcc this avoids a warning, but it might avoid a real 
        problem on another compiler or a 64-bit architecture *)
     Format.fprintf fmt "(-%a-1)" 
       (My_bigint.pretty ~hexa:true) (My_bigint.pred max_strict_signed)
   else
     Format.fprintf fmt "%a%s" 
       (My_bigint.pretty ~hexa:false) i
       suffix

 let regexp_int_decimal = Str.regexp "^-?[0-9]+$"
 let print_as_source source =
   (Kernel.Debug.get () = 0) &&
     (Kernel.BigIntsHex.is_default () ||
        not (Str.string_match regexp_int_decimal source 0))

 (* constant *)
 let d_const fmt c =
   match c with
   | CInt64(_, _, Some s) when print_as_source s ->
       fprintf fmt "%s" s (* Always print the text if there is one, unless
                             we want to print it as hexa *)
   | CInt64(i, ik, _) ->
       (*fprintf fmt "/* %Lx */" i;*)
       (** We must make sure to capture the type of the constant. For some
	   constants this is done with a suffix, for others with a cast
	   prefix.*) 
     let suffix = match ik with
       | IUInt -> "U"
       | ILong -> "L"
       | IULong -> "UL"
       | ILongLong -> if theMachine.msvcMode then "L" else "LL"
       | IULongLong -> if theMachine.msvcMode then "UL" else "ULL"
       | IInt | IBool | IShort | IUShort | IChar | ISChar | IUChar -> ""
     in
     let prefix =
       if suffix <> "" then ""
       else if ik = IInt then ""
       else Pretty_utils.sfprintf "(%a)" d_ikind ik
     in
     fprintf fmt "%s%a" prefix (pretty_C_constant suffix ik) i

   | CStr(s) -> fprintf fmt "\"%s\"" (Escape.escape_string s)
   | CWStr(s) ->
       (* text ("L\"" ^ escape_string s ^ "\"")  *)
       fprintf fmt "L";
       List.iter
	 (fun elt ->
	    if (elt >= Int64.zero &&
		  elt <= (Int64.of_int 255)) then
	      fprintf fmt "%S"
		(Escape.escape_char (Char.chr (Int64.to_int elt)))
	    else
	      fprintf fmt "\"\\x%LX\"" elt;
	    fprintf fmt "@ ")
	 s;
	 (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" --
	  * the former has 7 wide characters and the later has 3. *)

   | CChr(c) -> fprintf fmt "'%s'" (Escape.escape_char c)
   | CReal(_, _, Some s) -> fprintf fmt "%s" s
   | CReal(f, fsize, None) ->
       fprintf fmt "%a%s" 
	 Floating_point.pretty f
	 (match fsize with
	    FFloat -> "f"
	  | FDouble -> ""
	  | FLongDouble -> "L")
   | CEnum {einame = s} -> fprintf fmt "%s" s

 (* logic constant *)
 let d_logic_const fmt c =
   match c with
   | Integer(_, Some s) when print_as_source s ->
       fprintf fmt "%s" s (* Always print the text if there is one, unless
                             we want to print it as hexa *)
   | Integer(i, _) ->  My_bigint.pretty fmt i
   | LStr(s) -> fprintf fmt "\"%s\"" (Escape.escape_string s)
   | LWStr(s) ->
       (* text ("L\"" ^ escape_string s ^ "\"")  *)
       fprintf fmt "L";
       List.iter
	 (fun elt ->
	    if (elt >= Int64.zero &&
		  elt <= (Int64.of_int 255)) then
	      fprintf fmt "%S"
		(Escape.escape_char (Char.chr (Int64.to_int elt)))
	    else
	      fprintf fmt "\"\\x%LX\"" elt;
	    fprintf fmt "@ ")
	 s;
	 (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" --
	  * the former has 7 wide characters and the later has 3. *)

   | LChr(c) -> fprintf fmt "'%s'" (Escape.escape_char c)
   | LReal(_, s) -> fprintf fmt "%s" s
   | LEnum {einame = s} -> fprintf fmt "%s" s

 (* Parentheses/precedence level. An expression "a op b" is printed
  * parenthesized if its parentheses level is >= that that of its context.
  * Identifiers have the lowest level and weakly binding operators (e.g. |)
  * have the largest level. The correctness criterion is that a smaller level
  * MUST correspond to a stronger precedence! *)

 let derefStarLevel = 20
 let indexLevel = 20
 let arrowLevel = 20
 let addrOfLevel = 30
 let additiveLevel = 60
 let comparativeLevel = 70
 let bitwiseLevel = 75
 let logic_level = 77
 let binderLevel = 90
 let questionLevel = 100
 let upperLevel = 110

 let getParenthLevelPred = function
   | Pfalse
   | Ptrue
   | Papp _
   | Pallocable _
   | Pfreeable _
   | Pvalid _
   | Pvalid_read _
   | Pinitialized _
   | Pseparated _
   | Pat _
   | Pfresh _ -> 0

   | Psubtype _ -> 75

   | Pnot _ -> 30

   | Pand _
   | Por _
   | Pxor _ -> 85

   | Pimplies _ -> 88
   | Piff _ -> 89
   | Pif _ -> questionLevel

   | Prel _ -> comparativeLevel

   | Plet _
   | Pforall _
   | Pexists _ -> binderLevel

 let getParenthLevel e = match (stripInfo e).enode with
   | Info _ -> assert false
   | BinOp((LAnd | LOr), _,_,_) -> 80
					 (* Bit operations. *)
   | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)

					 (* Comparisons *)
   | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_) ->
       comparativeLevel (* 70 *)
					 (* Additive. Shifts can have higher
					  * level than + or - but I want
					  * parentheses around them *)
   | BinOp((MinusA|MinusPP|MinusPI|PlusA|
	    PlusPI|IndexPI|Shiftlt|Shiftrt),_,_,_)
     -> additiveLevel (* 60 *)

					 (* Multiplicative *)
   | BinOp((Div|Mod|Mult),_,_,_) -> 40

					 (* Unary *)
   | CastE(_,_) -> 30
   | AddrOf(_) -> 30
   | StartOf(_) -> 30
   | UnOp((Neg|BNot|LNot),_,_) -> 30

					 (* Lvals *)
   | Lval(Mem _ , _) -> derefStarLevel (* 20 *)
   | Lval(Var _, (Field _|Index _)) -> indexLevel (* 20 *)
   | SizeOf _ | SizeOfE _ | SizeOfStr _ -> 20
   | AlignOf _ | AlignOfE _ -> 20

   | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
   | Const _ -> 0                        (* Constants *)

 let getParenthLevelLogic = function
   | Tlambda _ | Trange _ | Tlet _ -> binderLevel
   | TBinOp((LAnd | LOr), _,_) -> 80
					 (* Bit operations. *)
   | TBinOp((BOr|BXor|BAnd),_,_) -> bitwiseLevel (* 75 *)

					 (* Comparisons *)
   | TBinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_) ->
       comparativeLevel (* 70 *)
					 (* Additive. Shifts can have higher
					  * level than + or - but I want
					  * parentheses around them *)
   | TBinOp((MinusA|MinusPP|MinusPI|PlusA|
	    PlusPI|IndexPI|Shiftlt|Shiftrt),_,_)
     -> additiveLevel (* 60 *)

					 (* Multiplicative *)
   | TBinOp((Div|Mod|Mult),_,_) -> 40

					 (* Unary *)
   | TCastE(_,_) -> 30
   | TAddrOf(_) -> addrOfLevel
   | TStartOf(_) -> 30
   | TUnOp((Neg|BNot|LNot),_) -> 30
					 (* Unary post *)
   | TCoerce _ | TCoerceE _ -> 25

					 (* Lvals *)
   | TLval(TMem _ , _) -> derefStarLevel
   | TLval(TVar _, (TField _|TIndex _|TModel _)) -> indexLevel
   | TLval(TResult _,(TField _|TIndex _|TModel _)) -> indexLevel
   | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ -> 20
   | TAlignOf _ | TAlignOfE _ -> 20
       (* VP: I'm not sure I understand why sizeof(x) and f(x) should
	  have a separated treatment wrt parentheses. *)
       (* application and applications-like constructions *)
   | Tapp (_, _,_)|TDataCons _
   | Tblock_length _ | Tbase_addr _ | Toffset _ | Tat (_, _)
   | Tunion _ | Tinter _
   | TUpdate _ | Ttypeof _ | Ttype _ -> 10
   | TLval(TVar _, TNoOffset) -> 0        (* Plain variables *)
       (* Constructions that do not require parentheses *)
   | TConst _
   | Tnull | TLval (TResult _,TNoOffset) | Tcomprehension _  | Tempty_set -> 0
   | Tif (_, _, _)  -> logic_level

 let getParenthLevelAttrParam (a: attrparam) =
   (* Create an expression of the same shape, and use {!getParenthLevel} *)
   match a with
     AInt _ | AStr _ | ACons _ -> 0
   | ASizeOf _ | ASizeOfE _ -> 20
   | AAlignOf _ | AAlignOfE _ -> 20
   | AUnOp (uo, _) -> getParenthLevel
     (dummy_exp (UnOp(uo, zero ~loc:Cil_datatype.Location.unknown, intType)))
   | ABinOp (bo, _, _) ->
       getParenthLevel (dummy_exp(BinOp(bo,
                                        zero ~loc:Cil_datatype.Location.unknown,
                                        zero ~loc:Cil_datatype.Location.unknown,
                                        intType)))
   | AAddrOf _ -> 30
   | ADot _ | AIndex _ | AStar _ -> 20
   | AQuestion _ -> questionLevel


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
     if not theMachine.msvcMode then
       stom, rest
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
   | _ -> assert false

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

   | Lval(lv) -> typeOfLval lv
   | SizeOf _ | SizeOfE _ | SizeOfStr _ -> theMachine.typeOfSizeOf
   | AlignOf _ | AlignOfE _ -> theMachine.typeOfSizeOf
   | UnOp (_, _, t) -> t
   | BinOp (_, _, _, t) -> t
   | CastE (t, _) -> t
   | AddrOf (lv) -> TPtr(typeOfLval lv, [])
   | StartOf (lv) ->
       begin
	 match unrollType (typeOfLval lv) with
	     TArray (t,_,_, _) -> TPtr(t, [])
	   | _ -> fatal "typeOf: StartOf on a non-array"
       end

 and typeOfInit (i: init) : typ =
   match i with
     SingleInit e -> typeOf e
   | CompoundInit (t, _) -> t

 and typeOfLval = function
     Var vi, off -> typeOffset vi.vtype off
   | Mem addr, off -> begin
       match unrollType (typeOf addr) with
	 TPtr (t, _) -> typeOffset t off
       | _ -> fatal "typeOfLval: Mem on a non-pointer (%a)" !pd_exp addr
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
       | _ -> fatal "typeOffset: Index on a non-array"
   end
   | Field (fi, o) ->
       match unrollType basetyp with
	 TComp (_, _,baseAttrs) ->
	   let fieldType = typeOffset fi.ftype o in
           let attrs = filter_qualifier_attributes baseAttrs in
	   let typ = typeAddAttributes attrs fieldType in
	   (match fi.fbitfield with
	    | Some s ->
		typeAddAttributes 
		  [Attr ("FRAMA_C_BITFIELD_SIZE", [AInt (My_bigint.of_int s)])]
		  typ
	    | None -> typ)
       | basetyp -> fatal "typeOffset: Field %s on a non-compound type '%a'"
	   fi.fname !pd_type basetyp

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
	       | _ -> fatal "typeOfTermLval: Mem on a non-pointer"
	     end
	 | Linteger | Lreal -> fatal "typeOfTermLval: Mem on a logic type"
	 | Ltype (s,_) -> 
           fatal "typeOfTermLval: Mem on a non-C type (%s)" s.lt_name
	 | Lvar s -> fatal "typeOfTermLval: Mem on a non-C type ('%s)" s
	 | Larrow _ -> fatal "typeOfTermLval: Mem on a function type"
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
           fatal "typeTermOffset: Attribute on a logic type"
         | Ltype (s,_) -> 
           fatal "typeTermOffset: Attribute on a non-C type (%s)" s.lt_name
         | Lvar s -> fatal "typeTermOffset: Attribute on a non-C type ('%s)" s
         | Larrow _ -> fatal "typeTermOffset: Attribute on a function type"
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
	       | _ -> fatal "typeTermOffset: Index on a non-array"
	     end
	   | Linteger | Lreal -> fatal "typeTermOffset: Index on a logic type"
	   | Ltype (s,_) -> 
             fatal "typeTermOffset: Index on a non-C type (%s)" s.lt_name
	   | Lvar s -> fatal "typeTermOffset: Index on a non-C type ('%s)" s
	   | Larrow _ -> fatal "typeTermOffset: Index on a function type"
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
	       | _ ->  fatal "typeTermOffset: Field on a non-compound"
	     end
	   | Linteger | Lreal -> fatal "typeTermOffset: Field on a logic type"
	   | Ltype (s,_) ->
             fatal "typeTermOffset: Field on a non-C type (%s)" s.lt_name
	   | Lvar s ->  fatal "typeTermOffset: Field on a non-C type ('%s)" s
	   | Larrow _ -> fatal "typeTermOffset: Field on a function type"
       in Logic_const.transform_element elt_type basetyp

 (**** Look at at the presence of an attribute in a type ****)

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
	 with SizeOfError _ as e ->
	   s.scache <- Not_Computable e;
	   raise e
       in
       s.scache <- Computed r;
       r
   | Not_Computable e -> raise e
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

module CoupleTypOffset =
  Datatype.Pair_with_collections(Typ)(Offset)
    (struct let module_name = "Cil.CoupleTypOffset" end)

module CacheBitsOffset =
  State_builder.Hashtbl
    (CoupleTypOffset.Hashtbl)
    (Datatype.Pair(Datatype.Int)(Datatype.Int))
    (struct let size = 17
            let dependencies = []
            let name = "Cil.CacheBitsOffset"
     end) 

 (* Get the minimum aligment in bytes for a given type *)
let rec alignOf_int t = 
  let alignOfType () = match t with 
  | TInt((IChar|ISChar|IUChar|IBool), _) -> 1
  | TInt((IShort|IUShort), _) -> theMachine.theMachine.alignof_short
  | TInt((IInt|IUInt), _) -> theMachine.theMachine.alignof_int
  | TInt((ILong|IULong), _) -> theMachine.theMachine.alignof_long
  | TInt((ILongLong|IULongLong), _) ->
    theMachine.theMachine.alignof_longlong
  | TEnum (ei,_) ->  alignOf_int (TInt(ei.ekind, []))
  | TFloat(FFloat, _) -> theMachine.theMachine.alignof_float
  | TFloat(FDouble, _) -> theMachine.theMachine.alignof_double
  | TFloat(FLongDouble, _) ->
    theMachine.theMachine.alignof_longdouble
  | TNamed (t, _) -> alignOf_int t.ttype
  | TArray (t, _, _, _) -> (* Be careful for char[] of Diab-C like compilers. *)
    begin
      match unrollTypeSkel t with
      | TInt((IChar|ISChar|IUChar),_) ->
	theMachine.theMachine.alignof_char_array
      | _ -> alignOf_int t
    end

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
	if not theMachine.msvcMode && f.fbitfield = Some 0 then sofar else
	  max sofar (alignOfField f)) 1 fields
  (* These are some error cases *)
  | TFun _ when not theMachine.msvcMode ->
    theMachine.theMachine.alignof_fun
  | TFun _ as t -> raise (SizeOfError ("function", t))
  | TVoid _ as t -> raise (SizeOfError ("void", t))
  in
  match filterAttributes "aligned" (typeAttrs t) with
    [] -> 
      (* no __aligned__ attribute, so get the default alignment *)
      alignOfType ()
  | _ when !ignoreAlignmentAttrs -> 
    Kernel.warning "ignoring recursive align attributes on %a" 
      !pd_type t;
    alignOfType ()
  | (Attr(_, [a]) as at)::rest -> begin
    if rest <> [] then
      Kernel.warning "ignoring duplicate align attributes on %a" 
        !pd_type t;
    match intOfAttrparam a with
      Some n -> n
    | None -> 
      Kernel.warning "alignment attribute \"%a\" not understood on %a" 
        !pd_attr at !pd_type t;
      alignOfType ()
  end
  | Attr(_, [])::rest ->
       (* aligned with no arg means a power of two at least as large as
          any alignment on the system.*)
    if rest <> [] then
      Kernel.warning "ignoring duplicate align attributes on %a" 
        !pd_type t;
    theMachine.theMachine.alignof_aligned
  | at::_ ->
    Kernel.warning "alignment attribute \"%a\" not understood on %a" 
              !pd_attr at !pd_type t;
    alignOfType ()

(* alignment of a possibly-packed struct field. *)
and alignOfField (fi: fieldinfo) =
  let fieldIsPacked = hasAttribute "packed" fi.fattr 
    || hasAttribute "packed" fi.fcomp.cattr in
  if fieldIsPacked then 1
  else alignOf_int fi.ftype
    
and intOfAttrparam (a:attrparam) : int option = 
  let rec doit a : int =
    match a with
    |  AInt(n) -> My_bigint.to_int n
    | ABinOp(Shiftlt, a1, a2) -> (doit a1) lsl (doit a2)
    | ABinOp(Div, a1, a2) -> (doit a1) / (doit a2)
    | ASizeOf(t) ->
      let bs = bitsSizeOf t in
      bs / 8
    | AAlignOf(t) ->
      alignOf_int t
    | _ -> raise (SizeOfError ("", voidType))
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

and bitsSizeOfInt (ik: ikind): int =
   match ik with
   | IBool | IChar | ISChar | IUChar -> 8
   | IInt | IUInt -> 8 * theMachine.theMachine.sizeof_int
   | IShort | IUShort -> 8 * theMachine.theMachine.sizeof_short
   | ILong | IULong -> 8 * theMachine.theMachine.sizeof_long
   | ILongLong | IULongLong ->
       8 * theMachine.theMachine.sizeof_longlong

(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
and offsetOfFieldAcc_GCC (fi: fieldinfo) (sofar: offsetAcc) : offsetAcc =
   (* field type *)
   let ftype = unrollType fi.ftype in
   let ftypeAlign = 8 * alignOfField fi in
   let ftypeBits = bitsSizeOf ftype in
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
 and offsetOfFieldAcc_MSVC (fi: fieldinfo)
     (sofar: offsetAcc) : offsetAcc =
   (* field type *)
   let ftype = unrollType fi.ftype in
   let ftypeAlign = 8 * alignOfField fi in
   let ftypeBits = bitsSizeOf ftype in
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
       offsetOfFieldAcc_MSVC fi
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
       offsetOfFieldAcc_MSVC fi
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

   | _, Some _, None -> Kernel.fatal "offsetAcc"


 and offsetOfFieldAcc ~(fi: fieldinfo) ~(sofar: offsetAcc) : offsetAcc =
   if theMachine.msvcMode then offsetOfFieldAcc_MSVC fi sofar
   else offsetOfFieldAcc_GCC fi sofar

 (* The size of a type, in bits. If struct or array then trailing padding is
  * added *)
 and bitsSizeOf t =
   if not (TheMachine.is_computed ()) then
     Kernel.fatal "You did not call Cil.initCIL before using the CIL library" ;
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
	    if not comp.cdefined && not theMachine.msvcMode then begin
              raise
		(SizeOfError
		   (Format.sprintf
		      "abstract type: empty struct exist only with MSVC \
                       (comp %s)"
		      (compFullName comp),
		    t)) (*abstract type*)
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
	      List.fold_left (fun acc fi -> offsetOfFieldAcc ~fi ~sofar:acc)
		startAcc comp.cfields
	    in
	    if theMachine.msvcMode && lastoff.oaFirstFree = 0 && comp.cfields <> []
	    then
	      (* On MSVC if we have just a zero-width bitfields then the length
	       * is 32 and is not padded  *)
	      32
	    else
	      addTrailing lastoff.oaFirstFree (8 * alignOf_int t))

   | TComp (comp, scache, _) -> (* when not comp.cstruct *)
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
				let lastoff = offsetOfFieldAcc ~fi ~sofar:startAcc in
				if lastoff.oaFirstFree > acc then
				  lastoff.oaFirstFree else acc) 0 comp.cfields in
	    (* Add trailing by simulating adding an extra field *)
	    addTrailing max (8 * alignOf_int t))

   | TArray(bt, Some len, scache, _) ->
       find_size_in_cache
	 scache
	 (fun () ->
	    begin
	      match (constFold true len).enode with
		Const(CInt64(l,_,_)) ->
		  let sz = My_bigint.mul (My_bigint.of_int (bitsSizeOf bt)) l in
		  let sz' = try 
                              My_bigint.to_int sz 
                    with Failure "nativeint_of_big_int" -> 
		      raise 
                        (SizeOfError ("Array is so long that its size can't be "
				      ^"represented with an OCaml int.", t))

                  in
		  sz' (*WAS: addTrailing sz' (8 * alignOf_int t)*)
	      | _ -> raise (SizeOfError ("array non-constant length", t))
	    end)
   | TVoid _ -> 8 * theMachine.theMachine.sizeof_void
   | TFun _ when not theMachine.msvcMode ->
       (* On GCC the size of a function is defined *)
       8 * theMachine.theMachine.sizeof_fun

   | TArray (_, None, _, _) ->
       (* it seems that on GCC the size of such an
	* array is 0 *)
       0

   | TFun _ -> raise (SizeOfError ("function", t))


 and addTrailing nrbits roundto =
   (nrbits + roundto - 1) land (lnot (roundto - 1))

 and sizeOf_int t = (bitsSizeOf t) lsr 3

and sizeOf ~loc t =
  try
    integer ~loc ((bitsSizeOf t) lsr 3)
  with SizeOfError _ -> new_exp ?loc (SizeOf(t))

 and bitsOffset (baset: typ) (off: offset) : int * int =
  CacheBitsOffset.memo
  (fun (baset, off) ->
   let rec loopOff (baset: typ) (width: int) (start: int) = function
       NoOffset -> start, width
     | Index(e, off) -> begin
	 let ei =
	   match isInteger (constFold true e) with
           | Some i -> My_bigint.to_int i
	   | None -> raise (SizeOfError ("index not constant", baset))
	 in
	 let bt =
	   match unrollType baset with
	     TArray(bt, _, _, _) -> bt
           | t -> Kernel.fatal "bitsOffset: Index on a non-array %a" !pd_type t
	 in
	 let bitsbt = bitsSizeOf bt in
	 loopOff bt bitsbt (start + ei * bitsbt) off
       end
     | Field(f, off) when not f.fcomp.cstruct ->
	 (* All union fields start at offset 0 *)
	 loopOff f.ftype (bitsSizeOf f.ftype) start off

     | Field(f, off) ->
	 (* Construct a list of fields preceeding and including this one *)
	 let prevflds =
	   let rec loop = function
	     | [] ->
		 Kernel.abort
		   "bitsOffset: Cannot find field %s in %s"
		   f.fname
		   f.fcomp.cname
	     | fi' :: _ when fi' == f -> [ fi' ]
	     | fi' :: rest -> fi' :: loop rest
	   in
	   loop f.fcomp.cfields
	 in
	 let lastoff =
	   List.fold_left (fun acc fi' -> offsetOfFieldAcc ~fi:fi' ~sofar:acc)
	     { oaFirstFree      = 0; (* Start at 0 because each struct is done
				      * separately *)
	       oaLastFieldStart = 0;
	       oaLastFieldWidth = 0;
	       oaPrevBitPack    = None } prevflds
	 in
	 (* ignore (E.log "Field %s of %s: start=%d, lastFieldStart=%d\n"
	    f.fname f.fcomp.cname start lastoff.oaLastFieldStart); *)
	 loopOff f.ftype lastoff.oaLastFieldWidth
	   (start + lastoff.oaLastFieldStart) off
   in
   loopOff baset (bitsSizeOf baset) 0 off
  ) (baset, off)

(** Do constant folding on an expression. If the first argument is true then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node.*)
and constFold (machdep: bool) (e: exp) : exp =
  Kernel.debug "ConstFold to %a@." 
    !pd_exp e;

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
                kinteger64_repr ~loc tk (My_bigint.neg i) repr
            | BNot -> kinteger64 ~loc tk (My_bigint.lognot i)
            | LNot -> 
              if My_bigint.equal i My_bigint.zero then one ~loc
              else zero ~loc
          end
        | _ -> new_exp ~loc (UnOp(unop, e1c, tres))
      with Not_found -> e
    end
      (* Characters are integers *)
  | Const(CChr c) -> new_exp ~loc (Const(charConstToInt c))
  | Const(CEnum {eival = v}) -> constFold machdep v
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
      kinteger ~loc theMachine.kindOfSizeOf (alignOf_int t)
  | AlignOfE e when machdep -> begin
      (* The alignment of an expression is not always the alignment of its
       * type. I know that for strings this is not true *)
      match e.enode with
        Const (CStr _) when not theMachine.msvcMode ->
          kinteger ~loc
            theMachine.kindOfSizeOf theMachine.theMachine.alignof_str
            (* For an array, it is the alignment of the array ! *)
      | _ -> constFold machdep (new_exp ~loc:e.eloc (AlignOf (typeOf e)))
    end

   | CastE(it,
	   { enode = AddrOf (Mem ({enode = CastE(TPtr(bt, _), z)}), off)})
       when machdep && isZero z -> begin
	 try
	   let start, _width = bitsOffset bt off in
	   if start mod 8 <> 0 then error "Using offset of bitfield" ;
	   constFold machdep 
             (new_exp ~loc (CastE(it, (integer ~loc (start / 8)))))
	 with SizeOfError _ -> e
       end

  | CastE (t, e) -> begin
    Kernel.debug "ConstFold CAST to to %a@." 
      !pd_type t ;
    let e = constFold machdep e in
      match e.enode, unrollType t with
        (* Might truncate silently *)
        Const(CInt64(i,_k,_)), TInt(nk,a)
          (* It's okay to drop a cast to const.
             If the cast has any other attributes, leave the cast alone. *)
          when (dropAttributes ["const"] a) = [] ->
            Kernel.debug "ConstFold to %a : %s@." 
              !pd_ikind nk (My_bigint.to_string i);
            kinteger64 ~loc nk i
      | _, _ -> new_exp ~loc (CastE (t, e))
    end
  | Lval lv -> new_exp ~loc (Lval (constFoldLval machdep lv))
  | AddrOf lv -> new_exp ~loc (AddrOf (constFoldLval machdep lv))
  | StartOf lv -> new_exp ~loc (StartOf (constFoldLval machdep lv))
  | _ -> e

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
            Const(CChr c) -> new_exp ~loc (Const(charConstToInt c))
          | Const(CEnum {eival = v}) -> mkInt v
          | CastE(TInt (ik, ta), e) -> begin
              let exp = mkInt e in
              match exp.enode with
                  Const(CInt64(i, _, _)) ->
                    kinteger64 ~loc ik i
                | _ -> {exp with enode = CastE(TInt(ik, ta), exp)}
            end
          | _ -> e
      in
      let tk =
        match unrollTypeSkel tres with
            TInt(ik, _) -> ik
          | TEnum (ei,_) -> ei.ekind
          | _ -> Kernel.fatal "constFoldBinOp"
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
            (My_bigint.ge i2 My_bigint.zero)
            && My_bigint.lt i2 (My_bigint.of_int (bitsSizeOf (typeOf e1')))
          with SizeOfError _ -> false
        else false
      in
      (* Assume that the necessary promotions have been done *)
      let e1'' = mkInt e1' in
      let e2'' = mkInt e2' in
      match bop, e1''.enode, e2''.enode with
      | PlusA, Const(CInt64(z,_,_)), _ 
        when My_bigint.equal z My_bigint.zero -> e2''
      | PlusA, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | PlusPI, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | IndexPI, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | MinusPI, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | PlusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ~loc tk (My_bigint.add i1 i2)
      | MinusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
          when ik1 = ik2 ->
          kinteger64 ?loc tk (My_bigint.sub i1 i2)
      | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ?loc tk (My_bigint.mul i1 i2)
      | Mult, Const(CInt64(z,_,_)), _
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | Mult, Const(CInt64(one,_,_)), _ 
        when My_bigint.equal one My_bigint.one -> e2''
      | Mult, _,    Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | Mult, _, Const(CInt64(one,_,_)) 
        when My_bigint.equal one My_bigint.one -> e1''
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          begin
            try kinteger64 ?loc tk (My_bigint.div i1 i2)
            with Division_by_zero -> new_exp ?loc (BinOp(bop, e1', e2', tres))
          end
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
          when bytesSizeOfInt ik1 = bytesSizeOfInt ik2 -> begin
            try kinteger64 ?loc tk (My_bigint.div i1 i2)
            with Division_by_zero -> new_exp ?loc (BinOp(bop, e1', e2', tres))
          end
      | Div, _, Const(CInt64(one,_,_)) 
         when My_bigint.equal one My_bigint.one -> e1''
      | Mod, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          begin
            try kinteger64 ?loc tk (My_bigint.rem i1 i2)
            with Division_by_zero -> new_exp ?loc (BinOp(bop, e1', e2', tres))
          end
      | BAnd, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ?loc tk (My_bigint.logand i1 i2)
      | BAnd, Const(CInt64(z,_,_)), _ 
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | BAnd, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | BOr, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ?loc tk (My_bigint.logor i1 i2)
      | BOr, _, _ when isZero e1' -> e2'
      | BOr, _, _ when isZero e2' -> e1'
      | BXor, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 ?loc tk (My_bigint.logxor i1 i2)
      | Shiftlt, Const(CInt64(i1,_ik1,_)),Const(CInt64(i2,_,_))
          when shiftInBounds i2 ->
          kinteger64 ?loc tk (My_bigint.shift_left i1 i2)
      | Shiftlt, Const(CInt64(z,_,_)), _ 
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | Shiftlt, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | Shiftrt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,_,_))
          when shiftInBounds i2 ->
          if isunsigned ik1 then
            kinteger64 ?loc tk 
              (My_bigint.shift_right_logical i1 i2)
          else
            kinteger64 ?loc tk (My_bigint.shift_right i1 i2)
      | Shiftrt, Const(CInt64(z,_,_)), _ 
        when My_bigint.equal z My_bigint.zero -> zero ~loc
      | Shiftrt, _, Const(CInt64(z,_,_)) 
        when My_bigint.equal z My_bigint.zero -> e1''
      | Eq, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.equal i1' i2' then one ~loc else zero ~loc
      | Ne, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.equal i1' i2' then zero ~loc else one ~loc
      | Le, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.le i1' i2' then one ~loc else zero ~loc
      | Ge, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.ge i1' i2' then one ~loc else zero ~loc
      | Lt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.lt i1' i2' then one ~loc else zero ~loc
      | Gt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
	let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
	if My_bigint.gt i1' i2' then one ~loc else zero ~loc

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
      | _ -> new_exp ?loc (BinOp(bop, e1', e2', tres))
    in
    if debugConstFold then
      Format.printf "Folded %a to %a@."
        (!pd_exp) (new_exp ?loc (BinOp(bop, e1', e2', tres)))
        (!pd_exp) newe ;
    newe
  end else
    new_exp ?loc (BinOp(bop, e1', e2', tres))

let () = pbitsSizeOf := bitsSizeOf

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
       (CInt64(My_bigint.of_int64 value,IULong,orig_rep)),(TInt(IULong,[]))
     else
       (CInt64(My_bigint.of_int64 value,IULongLong,orig_rep)),(TInt(IULongLong,[]))
   end

 (*/CEA*)

let smallest_kind ~signed ~bits_size = 
  try 
    List.find 
      (fun kind -> isSigned kind=signed && bitsSizeOfInt kind = bits_size) 
      [IBool; (* This list is ordered by size of types *)
       IChar; ISChar; IUChar;
       IShort; IUShort;
       IInt; IUInt; 
       ILong; IULong; 
       ILongLong; IULongLong]
  with Not_found -> Kernel.fatal "Could not find a%signed type of size %d"
    (if signed then " " else "n un") bits_size

let uint64_t () = TInt(smallest_kind ~signed:false ~bits_size:64,[])
let uint32_t () = TInt(smallest_kind ~signed:false ~bits_size:32,[])
let uint16_t () = TInt(smallest_kind ~signed:false ~bits_size:16,[])

let d_unop fmt u =
  fprintf fmt "%s"
    (match u with
      Neg -> "-"
    | BNot -> "~"
    | LNot -> "!")

let d_binop fmt b =
  fprintf fmt "%s"
    (match b with
      PlusA | PlusPI | IndexPI -> "+"
    | MinusA | MinusPP | MinusPI -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Shiftlt -> "<<"
    | Shiftrt -> ">>"
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    | Eq -> "=="
    | Ne -> "!="
    | BAnd -> "&"
    | BXor -> "^"
    | BOr -> "|"
    | LAnd -> "&&"
    | LOr -> "||")

 let d_term_binop fmt b =
   fprintf fmt "%s"
   (match b with
      PlusA | PlusPI | IndexPI -> "+"
    | MinusA | MinusPP | MinusPI -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Shiftlt -> "<<"
    | Shiftrt -> ">>"
    | Lt -> "<"
    | Gt -> ">"
    | Le ->  if Kernel.Unicode.get () then Utf8_logic.le else "<="
    | Ge -> if Kernel.Unicode.get () then Utf8_logic.ge else ">="
    | Eq -> if Kernel.Unicode.get () then Utf8_logic.eq else "=="
    | Ne -> if Kernel.Unicode.get () then Utf8_logic.neq else "!="
    | BAnd -> "&"
    | BXor -> "^"
    | BOr -> "|"
    | LAnd -> if Kernel.Unicode.get () then Utf8_logic.conj else "&&"
    | LOr -> if Kernel.Unicode.get () then Utf8_logic.disj else "||")

 let d_relation fmt b =
   fprintf fmt "%s"
     (match b with
      | Rlt -> "<"
      | Rgt -> ">"
      | Rle -> if Kernel.Unicode.get () then Utf8_logic.le else "<="
      | Rge -> if Kernel.Unicode.get () then Utf8_logic.ge else ">="
      | Req -> if Kernel.Unicode.get () then Utf8_logic.eq else "=="
      | Rneq -> if Kernel.Unicode.get () then Utf8_logic.neq else "!=")

 let invalidStmt = mkStmt (Instr (Skip Location.unknown))

 module Builtin_functions =
   State_builder.Hashtbl
     (Datatype.String.Hashtbl)
     (Datatype.Triple(Typ)(Datatype.List(Typ))(Datatype.Bool))
     (struct
	let name = "Builtin_functions"
	let dependencies = [ TheMachine.self ]
	let size = 49
      end)

 let () = registerAttribute "FC_BUILTIN" (AttrName true)

 (* Initialize the builtin functions after the machine has been initialized. *)
 let initGccBuiltins () : unit =
   if not (TheMachine.is_computed ()) then
     Kernel.fatal "Call initCIL before initGccBuiltins" ;
   if Builtin_functions.length () <> 0 then
     Kernel.fatal "builtins already initialized." ;
   (* See if we have builtin_va_list *)
   let hasbva = Machdep.state.Machdep.gccHas__builtin_va_list in
   let sizeType = theMachine.upointType in
   let add s t l b = Builtin_functions.add ("__builtin_" ^ s) (t, l, b) in

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
     intType [ charPtrType; sizeType; intType; sizeType; charConstPtrType ] true;
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
   add "next_arg"
     (* When we parse builtin_next_arg we drop the second argument *)
     (if hasbva then TBuiltin_va_list [] else voidPtrType) [] false;
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

 module Frama_c_builtins =
   State_builder.Hashtbl
   (Datatype.String.Hashtbl)
   (Cil_datatype.Varinfo)
   (struct
     let name = "Cil.Frama_c_Builtins"
     let dependencies = []
     let size = 3
    end)

 let () = add_ast_dependency Frama_c_builtins.self

let is_unused_builtin v =
  hasAttribute "FC_BUILTIN" v.vattr && not v.vreferenced


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

(** Construct a hash with the builtins *)
let initMsvcBuiltins () : unit =
  if not (TheMachine.is_computed ()) then
    Kernel.fatal "Call initCIL before initMsvcBuiltins" ;
  if Builtin_functions.length () <> 0 then
    Kernel.fatal "builtins already initialized." ;
  (** Take a number of wide string literals *)
  Builtin_functions.add "__annotation" (voidType, [ ], true);
  ()

 (** This is used as the location of the prototypes of builtin functions. *)
 let builtinLoc: location = Location.unknown

 let range_loc loc1 loc2 = fst loc1, snd loc2

 let pred_body = function
   | LBpred a -> a
   | LBnone
   | LBreads _
   | LBinductive _
   | LBterm _ -> Kernel.fatal "definition expected in Cil.pred_body"


 let compareConstant c1 c2 =
   match c1, c2 with
     | CEnum e1, CEnum e2 ->
       e1.einame = e2.einame && e1.eihost.ename = e2.eihost.ename &&
       (match 
           isInteger (constFold true e1.eival),
           isInteger (constFold true e2.eival)
        with
          | Some i1, Some i2 -> My_bigint.equal i1 i2
          | _ -> false)
     | CInt64 (i1,k1,_), CInt64(i2,k2,_) -> 
       k1 = k2 && My_bigint.equal i1 i2
     | CStr s1, CStr s2 -> s1 = s2
     | CWStr l1, CWStr l2 ->
       (try List.for_all2 (fun x y -> Int64.compare x y = 0) l1 l2
        with Invalid_argument _ -> false)
     | CChr c1, CChr c2 -> c1 = c2
     | CReal(f1,k1,_), CReal(f2,k2,_) -> k1 = k2 && f1 = f2
     | (CEnum _ | CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _), _ -> false

 (* Moved from ext/expcompare.ml *)
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


 (** A printer interface for CIL trees. Create instantiations of
  * this type by specializing the class {!Cil.defaultCilPrinter}. *)
 class type cilPrinter = object

   (** Local logical annotation (function specifications and code annotations
       are printed only if [logic_printer_enabled] is set to true
    *)
   val mutable logic_printer_enabled : bool

   (** more info is displayed on verbose mode. *)
   val mutable verbose: bool

   method current_function: varinfo option
     (** Returns the [varinfo] corresponding to the function being printed *)

   method has_annot: bool
     (** true if [current_stmt] has some annotations attached to it. *)

   method current_stmt: stmt option
     (** Returns the stmt being printed *)

   method current_behavior: funbehavior option
     (** Returns the [funbehavior] being pretty-printed. *)

   method may_be_skipped: stmt -> bool

   method setPrintInstrTerminator : string -> unit
   method getPrintInstrTerminator : unit -> string

   method pVarName: Format.formatter -> string -> unit
     (** Invoked each time an identifier name is to be printed. Allows for
	 various manipulation of the name, such as unmangling. *)

   method pVDecl: Format.formatter -> varinfo -> unit
     (** Invoked for each variable declaration. Note that variable
      * declarations are all the [GVar], [GVarDecl], [GFun], all the [varinfo]
      * in formals of function types, and the formals and locals for function
      * definitions. *)

   method pVar: Format.formatter -> varinfo -> unit
     (** Invoked on each variable use. *)

   method pLval: Format.formatter -> lval -> unit
     (** Invoked on each lvalue occurence *)

   method pOffset: Format.formatter -> offset -> unit
     (** Invoked on each offset occurence. The second argument is the base. *)

   method pInstr: Format.formatter -> instr -> unit
     (** Invoked on each instruction occurrence. *)

   method pStmt: Format.formatter -> stmt -> unit
     (** Control-flow statement. This is used by
      * {!Cil.printGlobal} and by [Cil.dumpGlobal]. *)

   method pStmtNext : stmt -> Format.formatter -> stmt -> unit

   method requireBraces: block -> bool (* Cf. cil.mli *)

   method pBlock:
     ?nobrace:bool -> ?forcenewline:bool -> Format.formatter -> block -> unit
     (** Print a block. *)

   method pGlobal: Format.formatter -> global -> unit
     (** Global (vars, types, etc.). This can be slow and is used only by
      * {!Cil.printGlobal} but by {!Cil.dumpGlobal} for everything else except
      * [GVar] and [GFun]. *)

   method pFieldDecl: Format.formatter -> fieldinfo -> unit
     (** A field declaration *)

   method pType: ?fundecl:varinfo ->
     (Format.formatter -> unit) option -> Format.formatter -> typ -> unit

   method pAttr: Format.formatter -> attribute -> bool
     (** Attribute. Also return an indication whether this attribute must be
       * printed inside the __attribute__ list or not. *)

   method pAttrParam:  Format.formatter -> attrparam -> unit
     (** Attribute paramter *)

   method pAttrs:  Format.formatter -> attributes -> unit
     (** Attribute lists *)

   method pLabel:  Format.formatter -> label -> unit
     (** Label *)

   method pLineDirective: ?forcefile:bool ->  Format.formatter -> location -> unit
     (** Print a line-number. This is assumed to come always on an empty line.
      * If the forcefile argument is present and is true then the file name
      * will be printed always. Otherwise the file name is printed only if it
      * is different from the last time time this function is called. The last
      * file name is stored in a private field inside the cilPrinter object. *)

   method pStmtLabels : Format.formatter -> stmt -> unit
     (** Print only the labels of the statement. Used by [pAnnotatedStmt]. *)

   method pAnnotatedStmt : stmt ->  Format.formatter -> stmt -> unit
     (** Print an annotated statement. The code to be printed is given in the
      * last {!stmt} argument.  The initial {!stmt} argument
      * records the statement which follows the one being printed;
      * {!Cil.defaultCilPrinterClass} uses this information to prettify
      * statement printing in certain special cases. *)

   method pStmtKind : stmt ->  Format.formatter -> stmtkind -> unit
     (** Print a statement kind. The code to be printed is given in the
      * {!stmtkind} argument.  The initial {!Cil.stmt} argument
      * records the statement which follows the one being printed;
      * {!Cil.defaultCilPrinterClass} uses this information to prettify
      * statement printing in certain special cases.
      *)

   method pExp:  Format.formatter -> exp -> unit
     (** Print expressions *)

   method pInit:  Format.formatter -> init -> unit
     (** Print initializers. This can be slow and is used by
      * {!Cil.printGlobal} but not by {!Cil.dumpGlobal}. *)

   method pLogic_type:
     (Format.formatter -> unit) option ->
     Format.formatter -> logic_type -> unit
     (** The first argument gives the name of the declared variable. see pType for more
	 information. *)

   method pModel_info: Format.formatter -> model_info -> unit

   method pTerm: Format.formatter -> term -> unit

   method pTerm_node: Format.formatter -> term -> unit

   method pTerm_lval: Format.formatter -> term_lval -> unit

   method pTerm_offset: Format.formatter -> term_offset -> unit

   method pLogic_info_use: Format.formatter -> logic_info -> unit

   method pLogic_type_def: Format.formatter -> logic_type_def -> unit

   method pLogic_var: Format.formatter -> logic_var -> unit

   method pQuantifiers: Format.formatter -> quantifiers -> unit

   method pPredicate: Format.formatter -> predicate -> unit

   method pPredicate_named: Format.formatter -> predicate named -> unit

   method pIdentified_predicate:
     Format.formatter -> identified_predicate -> unit

 (*
   method pPredicate_info_use: Format.formatter -> predicate_info -> unit
 *)

   method pBehavior: Format.formatter -> funbehavior -> unit

   method pRequires: Format.formatter -> identified_predicate -> unit
   method pPost_cond: Format.formatter ->
     (termination_kind * identified_predicate) -> unit
   method pAssumes: Format.formatter -> identified_predicate -> unit

   method pComplete_behaviors: Format.formatter -> string list -> unit
   method pDisjoint_behaviors: Format.formatter -> string list -> unit

   method pTerminates: Format.formatter -> identified_predicate -> unit

   method pSpec: Format.formatter -> funspec -> unit

   method pAssigns:
     string -> Format.formatter -> identified_term assigns -> unit

   method pAllocation:
     isloop:bool -> Format.formatter -> identified_term allocation -> unit

   method pFrom:
     string -> Format.formatter -> identified_term from -> unit

   method pCode_annot: Format.formatter -> code_annotation -> unit

   method pAnnotation: Format.formatter -> global_annotation -> unit

   method pDecreases: Format.formatter -> term variant -> unit

   method pLoop_variant: Format.formatter -> term variant -> unit

   method pFile: Format.formatter -> file -> unit
 end


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

let get_termination_kind_name = function
    Normal -> "ensures" | Exits -> "exits" | Breaks -> "breaks"
  | Continues -> "continue" | Returns -> "returns"

let spare_attributes_for_c_cast =
  "declspec"::"arraylen"::"FRAMA_C_BITFIELD_SIZE"::qualifier_attributes

let type_remove_attributes_for_c_cast =
  typeRemoveAttributes spare_attributes_for_c_cast
    
let spare_attributes_for_logic_cast =
  spare_attributes_for_c_cast

let type_remove_attributes_for_logic_type =
  typeRemoveAttributes spare_attributes_for_logic_cast
    
let () = Cil_datatype.drop_non_logic_attributes :=
  dropAttributes spare_attributes_for_logic_cast

let need_cast oldt newt =
  not (Cil_datatype.Typ.equal 
         (type_remove_attributes_for_c_cast (unrollType oldt))
         (type_remove_attributes_for_c_cast (unrollType newt)))

class defaultCilPrinterClass : cilPrinter = object (self)
  val mutable logic_printer_enabled = true
  val mutable verbose = false

   val current_stmt = Stack.create ()
   val mutable current_function = None

   val mutable current_behavior = None

   method private in_current_function vi =
     assert (current_function = None);
     current_function <- Some vi
   method private out_current_function =
     assert (current_function <> None);
     current_function <- None

   val mutable has_annot = false
   method has_annot = has_annot

   method current_function = current_function
   method current_behavior = current_behavior

   method private set_current_behavior b =
     assert (current_behavior = None); current_behavior <- Some b

   method private reset_current_behavior () =
     assert (current_behavior <> None); current_behavior <- None

   method private push_stmt s = Stack.push s current_stmt
   method private pop_stmt s =
     ignore (Stack.pop current_stmt); has_annot<-false; s
   method current_stmt =
     try Some (Stack.top current_stmt) with Stack.Empty -> None

   method may_be_skipped s = s.labels = []

   (** Returns the stmt being printed *)

   val mutable currentFormals : varinfo list = []
   method private getLastNamedArgument (s: string) : exp =
     match List.rev currentFormals with
     | [] ->
       Kernel.abort
	 "Cannot find the last named argument when printing call to %s"
	 s
     | f :: _ -> new_exp ~loc:f.vdecl (Lval (var f))

   (*** VARIABLES ***)
   method pVarName fmt v = pp_print_string fmt v

   method private pVarString v =
     Pretty_utils.sfprintf "%a" self#pVar v

   (* variable use *)
   method pVar fmt (v:varinfo) = Format.fprintf fmt "%a" self#pVarName v.vname

   (* variable declaration *)
   method pVDecl fmt (v:varinfo) =
     let stom, rest = separateStorageModifiers v.vattr in
     let fundecl = if isFunctionType v.vtype then Some v else None in
     (* First the storage modifiers *)
     fprintf fmt "%s%a%a%s%a%a"
       (if v.vinline then "__inline " else "")
       d_storage v.vstorage
       self#pAttrs stom
       (if stom = [] then "" else " ")
       (self#pType ?fundecl
          (if v.vname = "" then None else Some (fun fmt -> self#pVar fmt v)))
       v.vtype
       self#pAttrs rest

   (*** L-VALUES ***)
   method pLval fmt (lv:lval) =  (* lval (base is 1st field)  *)
     match lv with
       Var vi, o -> fprintf fmt "%a%a" self#pVar vi self#pOffset o
     | Mem e, Field(fi, o) ->
	 fprintf fmt "%a->%a%a"
	   (self#pExpPrec arrowLevel)  e
	   self#pVarName fi.fname
	   self#pOffset o
     | Mem e, NoOffset ->
	 fprintf fmt "*%a"
	   (self#pExpPrec derefStarLevel) e
     | Mem e, o ->
	 fprintf fmt "(*%a)%a"
	   (self#pExpPrec derefStarLevel) e
	   self#pOffset o

   (** Offsets **)
   method pOffset fmt = function
     | NoOffset -> ()
     | Field (fi, o) ->
	 fprintf fmt ".%a%a"
	   self#pVarName fi.fname
	   self#pOffset o
     | Index (e, o) ->
	 fprintf fmt "[%a]%a"
	   self#pExp e
	   self#pOffset o

   method private pLvalPrec (contextprec: int) fmt lv =
     if getParenthLevel (dummy_exp(Lval(lv))) >= contextprec then
       fprintf fmt "(%a)" self#pLval lv
     else
       self#pLval fmt lv

   (*** EXPRESSIONS ***)
   method pExp fmt (e: exp) =
     let level = getParenthLevel e in
     match (stripInfo e).enode with
     | Info _ -> assert false
     | Const(c) -> d_const fmt c
     | Lval(l) -> self#pLval fmt l

     | UnOp(u,e1,_) ->
         (match u, e1 with
            | Neg, {enode = Const (CInt64 (v, _, _))}
                when My_bigint.ge v My_bigint.zero ->
	        fprintf fmt "-%a" (self#pExpPrec level) e1
            | _ ->
	        fprintf fmt "%a %a" d_unop u (self#pExpPrec level) e1)

     | BinOp(b,e1,e2,_) ->
	 fprintf fmt "@[%a %a %a@]"
	   (self#pExpPrec level) e1
	   d_binop b
	   (self#pExpPrec level) e2

     | CastE(t,e) ->
	 fprintf fmt "(%a)%a"
	   (self#pType None) t
	   (self#pExpPrec level) e

     | SizeOf (t) ->
	 fprintf fmt "sizeof(%a)"
	   (self#pType None) t

     | SizeOfE (e) ->
	 fprintf fmt "sizeof(%a)"
	   self#pExp e

     | SizeOfStr s ->
	 fprintf fmt "sizeof(%a)"
	   d_const (CStr s)

     | AlignOf (t) ->
	 fprintf fmt "__alignof__(%a)"
	   (self#pType None) t
     | AlignOfE (e) ->
	 fprintf fmt "__alignof__(%a)"
	   self#pExp e
     | AddrOf(lv) ->
	 fprintf fmt "& %a"
	   (self#pLvalPrec addrOfLevel) lv

     | StartOf(lv) ->
         if miscState.printCilAsIs then
           fprintf fmt "&(%a[0])" self#pLval lv
         else self#pLval fmt lv

   (* Print an expression, given the precedence of the context in which it
    * appears. *)
   method private pExpPrec (contextprec: int) fmt (e: exp) =
     let thisLevel = getParenthLevel e in
     let needParens =
       if thisLevel >= contextprec then
	 true
       else if contextprec == bitwiseLevel then
	 (* quiet down some GCC warnings *)
	 thisLevel == additiveLevel || thisLevel == comparativeLevel
       else
	 false
     in
     if needParens then
       fprintf fmt "(%a)" self#pExp e
     else
       self#pExp fmt e

   method pInit fmt = function
       SingleInit e -> self#pExp fmt e
     | CompoundInit (t, initl) ->
	 (* We do not print the type of the Compound *)
	 (*
	   let dinit e = d_init () e in
	   dprintf "{@[%a@]}"
	   (docList ~sep:(chr ',' ++ break) dinit) initl
	 *)
	 let printDesignator =
	   if not theMachine.msvcMode then begin
	     (* Print only for union when we do not initialize the first field *)
	     match unrollType t, initl with
	       TComp(ci, _, _), [(Field(f, NoOffset), _)] ->
		 if not (ci.cstruct) && ci.cfields != [] &&
		   (List.hd ci.cfields) != f then
		     true
		 else
		   false
	     | _ -> false
	   end else
	     false
	 in
	 let d_oneInit fmt = function
	     Field(f, NoOffset), i ->
	       if printDesignator then
		 fprintf fmt ".%a = "
		   self#pVarName f.fname;
	       self#pInit fmt i
	   | Index(e, NoOffset), i ->
	       if printDesignator then
		 fprintf fmt "[%a] = "
		   self#pExp e;
	       self#pInit fmt i
	   | _ -> Kernel.fatal "Trying to print malformed initializer"
	 in
	 fprintf fmt  "{@[%a@]}"
	   (Pretty_utils.pp_list ~sep:",@ " d_oneInit) initl


   (** What terminator to print after an instruction. sometimes we want to
       * print sequences of instructions separated by comma *)
   val mutable printInstrTerminator = ";"

   method private setPrintInstrTerminator (term : string) =
     printInstrTerminator <- term

   method private getPrintInstrTerminator () = printInstrTerminator

   (*** INSTRUCTIONS ****)
   method pInstr fmt (i:instr) =       (* imperative instruction *)
     fprintf fmt "%a"
       (self#pLineDirective ~forcefile:false) (Cil_datatype.Instr.loc i);
     match i with
     | Skip _ -> fprintf fmt ";"
     | Set(lv,e,_) -> begin
	 (* Be nice to some special cases *)
	 match e.enode with
	   BinOp((PlusA|PlusPI|IndexPI),
		 {enode = Lval(lv')},
		 {enode=Const(CInt64(one,_,_))},_)
	     when compareLval lv lv' && My_bigint.equal one My_bigint.one
	       && not miscState.printCilAsIs ->
		 fprintf fmt "%a ++%s"
		   (self#pLvalPrec indexLevel) lv
		   printInstrTerminator
	 | BinOp((MinusA|MinusPI),
		 {enode = Lval(lv')},
		 {enode=Const(CInt64(one,_,_))}, _)
	     when compareLval lv lv' && My_bigint.equal one My_bigint.one
	       && not miscState.printCilAsIs ->
	     fprintf fmt "%a --%s"
	       (self#pLvalPrec indexLevel) lv
	       printInstrTerminator

	 | BinOp((PlusA|PlusPI|IndexPI),
		 {enode = Lval(lv')},
		 {enode = Const(CInt64(mone,_,_))},_)
	     when compareLval lv lv' && My_bigint.equal mone My_bigint.minus_one
	       && not miscState.printCilAsIs ->
	     fprintf fmt "%a --%s"
	       (self#pLvalPrec indexLevel) lv
	       printInstrTerminator

	 | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
		      Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
		 {enode = Lval(lv')},e,_) when compareLval lv lv' ->
	     fprintf fmt "%a %a= %a%s"
	       self#pLval  lv
	       d_binop bop
	       self#pExp e
	       printInstrTerminator

	 | _ ->
	     fprintf fmt "%a = %a%s"
	       self#pLval lv
	       self#pExp e
	       printInstrTerminator

       end
	 (* In cabs2cil we have turned the call to builtin_va_arg into a
	  * three-argument call: the last argument is the address of the
	  * destination *)
     | Call(None, {enode = Lval(Var vi, NoOffset)},
	    [dest; {enode = SizeOf t}; adest], (l,_))
	 when vi.vname = "__builtin_va_arg" && not miscState.printCilAsIs ->
	 let destlv = match (stripCasts adest).enode with
	   AddrOf destlv -> destlv
	     (* If this fails, it's likely that an extension interfered
		with the AddrOf *)
	 | _ ->
	     Kernel.fatal ~source:l
	       "Encountered unexpected call to %s with dest %a"
	       vi.vname self#pExp adest
	 in
	 fprintf fmt "%a = __builtin_va_arg (@[%a,@ %a@])%s"
	   self#pLval destlv
	   (* Now the arguments *)
	   self#pExp dest
	   (self#pType None)  t
	   printInstrTerminator

     (* In cabs2cil we have dropped the last argument in the call to
      * __builtin_va_start and __builtin_stdarg_start. *)
     | Call(None, {enode = Lval(Var vi, NoOffset)}, [marker], l)
	 when ((vi.vname = "__builtin_stdarg_start" ||
		   vi.vname = "__builtin_va_start")
	       && not miscState.printCilAsIs) ->
	 begin
	   let last = self#getLastNamedArgument vi.vname in
	   self#pInstr fmt (Call(None,dummy_exp(Lval(Var vi,NoOffset)),
				 [marker; last],l))
	 end

     (* In cabs2cil we have dropped the last argument in the call to
      * __builtin_next_arg. *)
     | Call(res, {enode = Lval(Var vi, NoOffset)}, [ ], l)
	 when vi.vname = "__builtin_next_arg" && not miscState.printCilAsIs ->
	 begin
	   let last = self#getLastNamedArgument vi.vname in
	   self#pInstr fmt (Call(res,dummy_exp(Lval(Var vi,NoOffset)),[last],l))
	 end

     (* In cparser we have turned the call to
      * __builtin_types_compatible_p(t1, t2) into
      * __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
      * represent the types as expressions.
      * Remove the sizeofs when printing. *)
     | Call(dest, {enode = Lval(Var vi, NoOffset)},
	    [{enode = SizeOf t1}; {enode = SizeOf t2}], _)
	 when vi.vname = "__builtin_types_compatible_p"
	   && not miscState.printCilAsIs ->
	 (* Print the destination *)
	 (match dest with
	    None -> ()
	  | Some lv -> fprintf fmt "%a = " self#pLval lv );
	     (* Now the call itself *)
	     fprintf fmt "%a(%a, %a)%s"
	       self#pVarName vi.vname
	       (self#pType None) t1
	       (self#pType None) t2
	       printInstrTerminator
     | Call(_, {enode = Lval(Var vi, NoOffset)}, _, (l,_))
	 when vi.vname = "__builtin_types_compatible_p"
	   && not miscState.printCilAsIs ->
	 Kernel.fatal ~source:l
	   "__builtin_types_compatible_p: cabs2cil should have added sizeof to the arguments."

     | Call(dest,e,args,_) ->
	 (match dest with
	    None -> ()
	  | Some lv ->
	      fprintf fmt "%a = "
		self#pLval lv;
	      (* Maybe we need to print a cast *)
	      (let destt = typeOfLval lv in
	       match unrollType (typeOf e) with
               |  TFun (rt, _, _, _)
		   when (need_cast rt destt) ->
		     fprintf fmt "(%a)"
		       (self#pType None) destt
	       | _ -> ()));
	 (* Now the function name *)
	 (match e.enode with
	    Lval(Var _, _) -> self#pExp fmt e
	  | _ -> fprintf fmt "(%a)"  self#pExp e);
	 (* Now the arguments *)
	 Pretty_utils.pp_flowlist ~left:"(" ~sep:"," ~right:")"
	   self#pExp fmt args ;
	 (* Now the terminator *)
	 fprintf fmt "%s" printInstrTerminator

     | Asm(attrs, tmpls, outs, ins, clobs, l) ->
	 self#pLineDirective fmt l;
	 if theMachine.msvcMode then
	   fprintf fmt "__asm {@[%a@]}%s"
	     (Pretty_utils.pp_list ~sep:"@\n"
		(fun fmt s -> fprintf fmt "%s" s)) tmpls
	     printInstrTerminator
	 else begin
	   fprintf fmt "__asm__%a (@[%a"
	     self#pAttrs attrs
	     (Pretty_utils.pp_list ~sep:"@\n"
		(fun fmt x ->
		  (* [JS 2011/03/11] isn't equivalent to [fprintf fmt "%S" x]?
		   *)
		  fprintf fmt "\"%s\"" (Escape.escape_string x)))
	     tmpls;

	   if outs = [] && ins = [] && clobs = [] then
	     fprintf fmt ":"
	   else
	     fprintf fmt ": %a"
	       (Pretty_utils.pp_list ~sep:",@ "
		  (fun fmt (idopt, c, lv) ->
		     fprintf fmt "%s\"%s\" (%a)"
		       (match idopt with
			  None -> ""
			| Some id -> "[" ^ id ^ "] "
		       )
		       (Escape.escape_string c)
		       self#pLval lv
		  )) outs;

	   if ins <> [] || clobs <> [] then
	     fprintf fmt ": %a"
	       (Pretty_utils.pp_list ~sep:",@ "
		  (fun fmt (idopt, c, e) ->
		     fprintf fmt "%s\"%s\"(%a)"
		       (match idopt with
			  None -> ""
			| Some id -> "[" ^ id ^ "] "
		       )
		       (Escape.escape_string c)
		       self#pExp e))
	       ins;


	   if clobs <> [] then
	     fprintf fmt ": %a"
	       (Pretty_utils.pp_list ~sep:",@ "
		  (fun fmt c -> fprintf fmt "\"%s\"" (Escape.escape_string c)))
	       clobs;

	   fprintf fmt "@])%s" printInstrTerminator
	 end
     | Code_annot (annot, l) ->
	 has_annot <- true;
	 if logic_printer_enabled then
	   begin
	     self#pLineDirective ~forcefile:false fmt l ;
	     Pretty_utils.pp_open_block fmt "/*@@ " ;
	     self#pCode_annot fmt annot ;
	     Pretty_utils.pp_close_block fmt "*/" ;
	   end

   (**** STATEMENTS ****)
   method pStmt fmt (s:stmt) =        (* control-flow statement *)
     self#push_stmt s;
     self#pop_stmt (self#pStmtNext invalidStmt fmt s)

   method pStmtNext (next: stmt) fmt (s: stmt) =
     self#push_stmt s;
     self#pop_stmt (self#pAnnotatedStmt next fmt s)

   method pStmtLabels fmt (s:stmt) =
     (* print the labels. *)
     begin
       let is_simple = function
	 | Instr(Set _ | Call _ | Skip _) -> true
	 | _ -> false
       in
       match s.labels with
       | [] -> ()
       | [l] when is_simple s.skind -> self#pLabel fmt l
       | _ -> List.iter (fprintf fmt "%a@ " self#pLabel) s.labels
     end

   method pAnnotatedStmt (next: stmt) fmt (s: stmt) =
     self#pStmtLabels fmt s ;
     (* print the statement. *)
     if is_skip s.skind && not s.ghost then
       (if verbose || s.labels <> [] then fprintf fmt ";")
     else
       begin
	 if s.ghost then Pretty_utils.pp_open_block fmt "/*@@ ghost ";
	 self#pStmtKind next fmt s.skind ;
	 if s.ghost then Pretty_utils.pp_close_block fmt "*/" ;
       end

   method private pLabel fmt = function
       Label (s, _, true) -> fprintf fmt "%s: " s
     | Label (s, _, false) -> fprintf fmt "%s: /* internal */ " s
     | Case (e, _) -> fprintf fmt "case %a: " self#pExp e
     | Default _ -> fprintf fmt "default: "

   method requireBraces blk =
     match blk.bstmts, blk.battrs, blk.blocals with
     | ([_] | []),[],[] -> false
     | _,_,_::_ -> true
     | _ -> self#has_annot

   method private inlineBlock blk =
     match blk.bstmts with
       | [] | [{skind = (Instr _ | Return _ | Goto _ | Break _ | Continue _ )}]
         -> true
       | _ -> false


   (* The pBlock will put the unalign itself *)
   method pBlock ?(nobrace=true) ?(forcenewline=false) fmt (blk: block) =
     let forcenewline = forcenewline || not (self#inlineBlock blk) in
     let force_paren = (not nobrace) && (verbose || self#requireBraces blk) in
     (* Let the host of the block decide on the alignment. The d_block will
      * pop the alignment as well  *)
     let print_sep fmt =
       if forcenewline then fprintf fmt "@\n" else fprintf fmt "@ "
     in
     let rec iterblock fmt = function
       | [] -> ()
       | [s] ->
	   self#pStmtNext invalidStmt fmt s
       | s_cur :: (s_next :: _ as tail) ->
	   self#pStmtNext s_next fmt s_cur ;
           print_sep fmt;
	   iterblock fmt tail
     in
     if force_paren then fprintf fmt "@[{@[<hov 1>@ ";
     if nobrace then print_sep fmt ;
     if Kernel.debug_atleast 1 then fprintf fmt "@\n/* %a */@\n"
       (Pretty_utils.pp_list
	  ~sep:("," ^^ Pretty_utils.space_sep) self#pVar) blk.blocals;
     if blk.battrs <> [] then self#pAttrsGen true fmt blk.battrs ;
     List.iter
       (fun v -> fprintf fmt "%a;%t" self#pVDecl v print_sep) blk.blocals;
     iterblock fmt blk.bstmts ;
     if force_paren then fprintf fmt "@]@;}@]@\n";

   (* Store here the name of the last file printed in a line number. This is
    * private to the object *)
   val mutable lastFileName = ""
   val mutable lastLineNumber = -1

   (* Make sure that you only call self#pLineDirective on an empty line *)
   method pLineDirective ?(forcefile=false) fmt l =
     CurrentLoc.set l;
     match miscState.lineDirectiveStyle with
     | None -> ()
     | Some _ when (fst l).Lexing.pos_lnum <= 0 -> ()

     (* Do not print lineComment if the same line as above *)
     | Some LineCommentSparse when (fst l).Lexing.pos_lnum = lastLineNumber -> ()

     | Some style  ->
	 let directive =
	   match style with
	   | LineComment | LineCommentSparse -> "//#line "
	   | LinePreprocessorOutput when not theMachine.msvcMode -> "#"
	   | LinePreprocessorOutput | LinePreprocessorInput -> "#line"
	 in
	 lastLineNumber <- (fst l).Lexing.pos_lnum;
	 let filename =
	   if forcefile || (fst l).Lexing.pos_fname <> lastFileName then
	     begin
	       lastFileName <- (fst l).Lexing.pos_fname;
	       " \"" ^ (fst l).Lexing.pos_fname ^ "\""
	     end
	   else
	     ""
	 in
	 fprintf fmt "@<0>\n@<0>%s@<0> @<0>%d@<0> @<0>%s@\n" directive (fst l).Lexing.pos_lnum filename


   method pStmtKind (next: stmt) fmt kind =
     match kind with
     | UnspecifiedSequence seq ->
	 let print_stmt pstmt fmt (stmt, modifies, writes, reads,_) =
	   pstmt fmt stmt ;
	   if verbose then
	     Format.fprintf fmt "@ /*effects: @[(%a)%a@ <-@ %a@]*/"
               (Cilutil.pretty_list (Cilutil.space_sep ",") self#pLval) modifies
	       (Cilutil.pretty_list (Cilutil.space_sep ",") self#pLval) writes
	       (Cilutil.pretty_list (Cilutil.space_sep ",") self#pLval) reads
	 in
	 let rec iterblock fmt = function
	   | [] -> ()
	   | [srw] ->
	       fprintf fmt "@ " ;
	       print_stmt (self#pStmtNext invalidStmt) fmt srw
	   | srw_first :: ((s_next,_,_,_,_) :: _ as tail) ->
	       fprintf fmt "@ " ;
	       print_stmt (self#pStmtNext s_next) fmt srw_first ;
	       iterblock fmt tail
	 in
	 Pretty_utils.pp_open_block fmt "{ /*undefined sequence*/ " ;
	 iterblock fmt seq ;
	 Pretty_utils.pp_close_block fmt "}"

     | Return(None, l) ->
	 self#pLineDirective fmt l;
	 fprintf fmt "return;"

     | Return(Some e, l) ->
	 self#pLineDirective fmt l ;
	 fprintf fmt "return (%a);" self#pExp  e

     | Goto (sref, l) -> begin
	 (* Grab one of the labels *)
	 let rec pickLabel = function
	     [] -> None
	   | Label (lbl, _, _) :: _ -> Some lbl
	   | _ :: rest -> pickLabel rest
	 in
	 match pickLabel !sref.labels with
	   Some lbl ->
             self#pLineDirective fmt l;
             fprintf fmt "goto %s;" (* ^^ " /* %a */" *) lbl
	     (*self#pStmt !sref*)
	 | None ->
	     Kernel.error "Cannot find label for target of goto: %a"
	       self#pStmt !sref;
	     fprintf fmt "goto __invalid_label;"
       end

     | Break l ->
	 self#pLineDirective fmt l;
	 fprintf fmt "break;"

     | Continue l ->
	 self#pLineDirective fmt l;
	 fprintf fmt "continue;"

     | Instr i -> self#pInstr fmt i
	 (* fprintf fmt "@[%a@]" self#pInstr i *)

     | If(be,t,{bstmts=[];battrs=[]},l) when not miscState.printCilAsIs ->
	 self#pLineDirective ~forcefile:false fmt l ;
	 Pretty_utils.pp_open_block fmt "if (%a) {" self#pExp be ;
	 self#pBlock fmt t ;
	 Pretty_utils.pp_close_block fmt "}"

     | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},l)
	 when !gref == next && not miscState.printCilAsIs ->
	 self#pLineDirective ~forcefile:false fmt l ;
	   Pretty_utils.pp_open_block fmt "if (%a) {" self#pExp be ;
	   self#pBlock fmt t ;
	   Pretty_utils.pp_close_block fmt "}"

     | If(be,{bstmts=[];battrs=[]},e,l) when not miscState.printCilAsIs ->
	 self#pLineDirective ~forcefile:false fmt l ;
	 Pretty_utils.pp_open_block fmt "if (%a) {"
	   self#pExp (dummy_exp(UnOp(LNot,be,intType))) ;
	 self#pBlock fmt e ;
	 Pretty_utils.pp_close_block fmt "}"

     | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},e,l)
	 when !gref == next && not miscState.printCilAsIs ->
	 self#pLineDirective ~forcefile:false fmt l ;
	   Pretty_utils.pp_open_block fmt "if (%a) {"
	     self#pExp (dummy_exp(UnOp(LNot,be,intType))) ;
	   self#pBlock fmt e ;
	   Pretty_utils.pp_close_block fmt "}"

     | If(be,t,e,l) ->
	 self#pLineDirective ~forcefile:false fmt l ;
	 Pretty_utils.pp_open_block fmt "if (%a) {" self#pExp be ;
	 self#pBlock fmt t ;
	 Pretty_utils.pp_close_block fmt "}" ;
         let inline_else = self#inlineBlock e in
         if inline_else then
	   fprintf fmt "@ "
         else
	   fprintf fmt "@\n";
	 Pretty_utils.pp_open_block fmt "else {" ;
	 self#pBlock ~forcenewline:(not inline_else) fmt e ;
	 Pretty_utils.pp_close_block fmt "}"

     | Switch(e,b,_,l) ->
	 self#pLineDirective ~forcefile:false fmt l ;
	 Pretty_utils.pp_open_block fmt "switch (%a) {" self#pExp e ;
	 self#pBlock fmt b ;
	 Pretty_utils.pp_close_block fmt "}"

     | Loop(annot, b, l, _, _) ->
	 if logic_printer_enabled && annot <> [] then
	   begin
	     Pretty_utils.pp_open_block fmt "/*@@ " ;
	     Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep
	       self#pCode_annot
	       fmt
	       annot;
	     Pretty_utils.pp_close_block fmt "@ */@\n" ;
	   end ;
	 begin
	   (* Maybe the first thing is a conditional. Turn it into a WHILE *)
	   try
	     let rec skipEmpty = function
		 [] -> []
	       | {skind=Instr (Skip _);labels=[]} as h :: rest
		   when self#may_be_skipped h-> skipEmpty rest
	       | x -> x
	     in
	     let term, bodystmts =
	       (* Bill McCloskey: Do not remove the If if it has labels *)
	       match skipEmpty b.bstmts with
		 {skind=If(e,tb,fb,_)} as to_skip :: rest
		   when
		     not miscState.printCilAsIs && self#may_be_skipped to_skip ->
		       begin
			 match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
			   [], {skind=Break _; labels=[]}::_  -> e, rest
			 | {skind=Break _; labels=[]}::_, []
			     -> dummy_exp (UnOp(LNot, e, intType)), rest
			 | _ -> raise Not_found
		       end
	       | _ -> raise Not_found
	     in
	     let b = match skipEmpty bodystmts with
	       [{ skind=Block b} as s ] when self#may_be_skipped s -> b
	     | _ -> { b with bstmts = bodystmts }
	     in
	     self#pLineDirective fmt l ;
	     Pretty_utils.pp_open_block fmt "while (%a) {" self#pExp term ;
	     self#pBlock fmt b ;
	     Pretty_utils.pp_close_block fmt "}"

	   with Not_found ->
	     self#pLineDirective fmt l ;
	     Pretty_utils.pp_open_block fmt "while (1) {" ;
	     self#pBlock fmt b ;
	     Pretty_utils.pp_close_block fmt "}"

	 end

     | Block b ->
	 if (match b.bstmts with [] | [_] -> true | _ -> false)
	 then self#pBlock ~nobrace:false fmt b
	 else
	   begin
	     if verbose then
               Pretty_utils.pp_open_block fmt "/*block:begin*/@ " ;
	     self#pBlock ~nobrace:false fmt b ;
	     if verbose then Pretty_utils.pp_close_block fmt "/*block:end*/" ;
	   end

     | TryFinally (b, h, l) ->
	 self#pLineDirective fmt l;
	 fprintf fmt "__try {@[%a@]} @[<5>__finally{%a}@]"
           (* NB: eta expansion needed because of optional args of pBlock. *)
	   (fun fmt -> self#pBlock fmt) b
	   (fun fmt -> self#pBlock fmt) h

     | TryExcept (b, (il, e), h, l) ->
	 self#pLineDirective fmt l;
	 fprintf fmt "__try {@[%a@]} @[<5>__except(@\n@["
           (* NB: eta expansion needed because of optional args of pBlock. *)
           (fun fmt -> self#pBlock fmt) b;

	 (* Print the instructions but with a comma at the end, instead of
	  * semicolon *)
	 printInstrTerminator <- ",";
	 Pretty_utils.pp_list ~sep:"@\n" self#pInstr fmt il;
	 printInstrTerminator <- ";";
	 fprintf fmt "%a) @]%a"
           (* NB: eta expansion needed because of optional args of pBlock. *)
	   self#pExp e (fun fmt -> self#pBlock fmt) h

   (*** GLOBALS ***)
   method pGlobal fmt (g:global) =       (* global (vars, types, etc.) *)
     match g with
     | GFun (fundec, l) ->
       if not (is_unused_builtin fundec.svar)
       then begin
	 self#in_current_function fundec.svar;
	 (* If the function has attributes then print a prototype because
	  * GCC cannot accept function attributes in a definition *)
	 let oldattr = fundec.svar.vattr in
	 (* Always pring the file name before function declarations *)
	 (* Prototype first *)
	 if oldattr <> [] then
	   (self#pLineDirective fmt l;
	    fprintf fmt "%a;@\n"
	      self#pVDecl fundec.svar);
	 (* Temporarily remove the function attributes *)
	 fundec.svar.vattr <- [];
	 (* Body now *)
	 self#pLineDirective ~forcefile:true fmt l;
	 self#pFunDecl fmt fundec;
	 fundec.svar.vattr <- oldattr;
	 fprintf fmt "@\n";
	 self#out_current_function
       end

     | GType (typ, l) ->
	 self#pLineDirective ~forcefile:true fmt l;
	 fprintf fmt "typedef %a;@\n"
	   (self#pType (Some (fun fmt -> fprintf fmt "%s" typ.tname))) typ.ttype

     | GEnumTag (enum, l) ->
       self#pLineDirective fmt l;
       if verbose then 
         fprintf fmt "/* Following enum is equivalent to %a */@\n" 
           (self#pType None) 
           (TInt(enum.ekind,[]));
	 fprintf fmt "enum@[ %a {@\n%a@]@\n}%a;@\n"
	   self#pVarName enum.ename
	   (Pretty_utils.pp_list ~sep:",@\n"
	      (fun fmt item ->
		 fprintf fmt "%s = %a"
		   item.einame
		   self#pExp item.eival))
	   enum.eitems
	   self#pAttrs enum.eattr

     | GEnumTagDecl (enum, l) -> (* This is a declaration of a tag *)
	 self#pLineDirective fmt l;
	 fprintf fmt "enum %a;@\n" self#pVarName enum.ename

     | GCompTag (comp, l) -> (* This is a definition of a tag *)
	 let n = comp.cname in
	 let su =
	   if comp.cstruct then "struct"
	   else "union"
	 in
	 let sto_mod, rest_attr = separateStorageModifiers comp.cattr in
	 self#pLineDirective ~forcefile:true fmt l;
	 fprintf fmt "@[<3>%s%a %a {@\n%a@]@\n}%a;@\n"
	   su
	   self#pAttrs sto_mod
	   self#pVarName n
	   (Pretty_utils.pp_list ~sep:"@\n" self#pFieldDecl)
	   comp.cfields
	   self#pAttrs rest_attr

     | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
	 self#pLineDirective fmt l;
	 fprintf fmt "%s;@\n" (compFullName comp)

     | GVar (vi, io, l) ->
       if not (is_unused_builtin vi) then begin
	 self#pLineDirective ~forcefile:true fmt l;
	 fprintf fmt "%a"
	   self#pVDecl  vi;
	 (match io.init with
	    None -> ()
	  | Some i ->
	      fprintf fmt " = ";
	      let islong =
		match i with
		  CompoundInit (_, il) when List.length il >= 8 -> true
		| _ -> false
	      in
	      if islong then
		begin self#pLineDirective fmt l;
		  fprintf fmt "  @[@\n"
		end;
	      self#pInit fmt i;
	      if islong then
		fprintf fmt "@]");
	 fprintf fmt ";@\n"
       end
     (* print global variable 'extern' declarations, and function prototypes *)
     | GVarDecl (funspec, vi, l) ->
       if not (is_unused_builtin vi) then begin
	 if isFunctionType vi.vtype then self#in_current_function vi;
	 self#opt_funspec fmt funspec;
	 if not miscState.printCilAsIs && Builtin_functions.mem vi.vname then
	   begin
	     (* Compiler builtins need no prototypes. Just print them in
		comments. *)
	     fprintf fmt "/* compiler builtin: @\n   %a;   */@\n"
	       self#pVDecl vi
	   end else begin
	     self#pLineDirective fmt l;
	     fprintf fmt "%a;@\n" self#pVDecl vi
	   end;
	 if isFunctionType vi.vtype then self#out_current_function
       end

     | GAsm (s, l) ->
	 self#pLineDirective fmt l;
	 fprintf fmt "__asm__(\"%s\");@\n" (Escape.escape_string s)

     | GPragma (Attr(an, args), l) ->
	 (* sm: suppress printing pragmas that gcc does not understand *)
	 (* assume anything starting with "ccured" is ours *)
	 (* also don't print the 'combiner' pragma *)
	 (* nor 'cilnoremove' *)
	 let suppress =
	   not miscState.print_CIL_Input &&
	     not theMachine.msvcMode &&
	     ((startsWith "box" an) ||
		(startsWith "ccured" an) ||
		(an = "merger") ||
		(an = "cilnoremove"))
	 in
	 self#pLineDirective fmt l;
	 if suppress then fprintf fmt "/* ";
	 fprintf fmt "#pragma ";
	 begin
	   match an, args with
	   | _, [] ->
	       fprintf fmt "%s" an
	   | "weak", [ACons (varinfo, [])] ->
	       fprintf fmt "weak %s" varinfo
	   | "",_ ->
	       fprintf fmt "%a"
		 (Pretty_utils.pp_list ~sep:" " self#pAttrParam) args
	   | _ ->
	       fprintf fmt "%s(%a)"
		 an
		 (Pretty_utils.pp_list ~sep:"," self#pAttrParam) args

	 end;
	 if suppress then  fprintf fmt " */@\n" else fprintf fmt "@\n"

     | GPragma (AttrAnnot _, _) ->
	 assert false
	   (*        self#pLineDirective fmt l;
		     fprintf fmt "/* #pragma %s */@\n" a*)

     | GAnnot (decl,l) ->
	 (*if logic_printer_enabled then*)
	 begin
	   self#pLineDirective fmt l;
	   fprintf fmt "/*@@@ %a@ */@\n"
	     self#pAnnotation decl
	 end

     | GText s  ->
	 if s <> "//" then
	   fprintf fmt "%s@\n" s

   method pFieldDecl fmt fi =
     fprintf fmt "%a %s%a;"
       (self#pType
	  (Some (fun fmt -> if fi.fname <> missingFieldName then fprintf fmt "%s" fi.fname)))
       fi.ftype
       (match fi.fbitfield with
	| None -> ""
	| Some i -> ": " ^ string_of_int i ^ " ")
       self#pAttrs fi.fattr


   method private opt_funspec fmt funspec =
     if logic_printer_enabled && not (is_empty_funspec funspec) then
       fprintf fmt "/*@[@@ %a@]*/@\n" self#pSpec funspec

   method private pFunDecl fmt f =
     (* declaration. *)
     fprintf fmt "%a%a@\n@[<hv 2>{" self#opt_funspec f.sspec self#pVDecl f.svar ;
     (* We take care of locals in blocks. *)
     (*List.iter (fprintf fmt "@\n%a;" self#pVDecl) f.slocals ;*)
     (* body. *)
     currentFormals <- f.sformals ;
     self#pBlock ~forcenewline:true fmt f.sbody ;
     currentFormals <- [] ;
     fprintf fmt "@]@\n}@."

   (***** PRINTING DECLARATIONS and TYPES ****)

   method pType ?fundecl nameOpt
     fmt (t:typ) =
     let pname fmt space = match nameOpt with
       | None -> ()
       | Some d -> Format.fprintf fmt "%s%t" (if space then " " else "") d
     in
     let printAttributes fmt (a: attributes) =
       match nameOpt with
       | None when not miscState.print_CIL_Input && not theMachine.msvcMode ->
	   (* Cannot print the attributes in this case because gcc does not
	    * like them here, except if we are printing for CIL, or for MSVC.
	    * In fact, for MSVC we MUST print attributes such as __stdcall *)
	   (* if pa = nil then nil else
	      text "/*" ++ pa ++ text "*/"*) ()
       | _ ->  self#pAttrs fmt a
     in
     match t with
       TVoid a ->
	 fprintf fmt "void%a%a"
	   self#pAttrs a
	   pname true

     | TInt (ikind,a) ->
	 fprintf fmt "%a%a%a"
	   d_ikind ikind
	   self#pAttrs a
	   pname true

     | TFloat(fkind, a) ->
	 fprintf fmt "%a%a%a"
	   d_fkind fkind
	   self#pAttrs a
	   pname true

     | TComp (comp, _, a) -> (* A reference to a struct *)
	 fprintf fmt
	   "%s %a%a%a"
	   (if comp.cstruct then "struct" else "union")
	   self#pVarName comp.cname
	   self#pAttrs a
	   pname true

     | TEnum (enum, a) ->
	 fprintf fmt "enum %a%a%a"
	   self#pVarName enum.ename
	   self#pAttrs a
	   pname true

     | TPtr (bt, a) ->
	 (* Parenthesize the ( * attr name) if a pointer to a function or an
	  * array. However, on MSVC the __stdcall modifier must appear right
	  * before the pointer constructor "(__stdcall *f)". We push them into
	  * the parenthesis. *)
	 let (paren: (formatter -> unit) option), (bt': typ) =
	   match bt with
	     TFun(rt, args, isva, fa) when theMachine.msvcMode ->
	       let an, af', at = partitionAttributes ~default:AttrType fa in
	       (* We take the af' and we put them into the parentheses *)
	       Some
		 (fun fmt ->
		    fprintf fmt
		      "(%a"
		      printAttributes af'),
	       TFun(rt, args, isva, addAttributes an at)

	   | TFun _ | TArray _ -> (Some (fun fmt -> fprintf fmt "(")), bt

	   | _ -> None, bt
	 in
	 let name' = fun fmt ->
	   fprintf fmt "*%a%a"
	     printAttributes a
	     pname (a <> [])
	 in
	 let name'' =
	   fun fmt ->
	     (* Put the parenthesis *)
	     match paren with
	       Some p -> fprintf fmt "%t%t)" p name'
	     | _ -> fprintf fmt "%t" name'
	 in
	 self#pType
	   (Some name'')
	   fmt
	   bt'

     | TArray (elemt, lo, _, a) ->
	 (* qualifiers attributes are not supposed to be on the TArray,
            but on the base type. (Besides, GCC and Clang do not parse the
            result if the qualifier is misplaced. *)
         let atts_elem, a = splitArrayAttributes a in
         if atts_elem != [] then
           Kernel.failure ~current:true "Found some incorrect attributes for \
              array (%a). Please report." !pd_attrlist atts_elem;
	 let name' fmt =
	   if a = [] then pname fmt false
           else if nameOpt = None then
	     printAttributes fmt a
	   else
	     fprintf fmt "(%a%a)" printAttributes a pname true
	 in
	 self#pType
	   (Some (fun fmt ->
		    fprintf fmt "%t[%t]"
		      name'
		      (fun fmt ->
			 match lo with
			 | None -> ()
			 | Some e -> self#pExp fmt e)
		 ))
	   fmt
	   elemt

     | TFun (restyp, args, isvararg, a) ->
	 let name' fmt =
	   if a = [] then pname fmt false else
 	     if nameOpt = None then
	       printAttributes fmt a
 	     else
 	       fprintf fmt "(%a%a)"
		 printAttributes a
		 pname (a <> [])
	 in
	 let module Args(A:sig type t
			     val args: t list option
			     val pp_args: Format.formatter -> t -> unit
			 end)=
	   struct
	     let pp_prms fmt =
	       fprintf fmt "%t(@[%t@])" name'
		 (fun fmt ->
		    match A.args with
		      | None -> ()
		      | Some [] when isvararg ->
			  fprintf fmt "..."
		      | Some [] -> fprintf fmt "void"
		      | Some args ->
			  Pretty_utils.pp_list ~sep:",@ " A.pp_args
			    fmt args ;
			  if isvararg then fprintf fmt "@ , ...";
		 )
	   end
	 in
	 let pp_prms =
	   match fundecl with
	     | None ->
		  let module Args =
		    Args(struct
			   type t = (string * typ * attributes)
			   let args = args
			   let pp_args fmt (aname,atype,aattr) =
			     let stom, rest = separateStorageModifiers aattr in
			     (* First the storage modifiers *)
			     fprintf fmt
			       "%a%a%a"
			       self#pAttrs stom
			       (self#pType
				  (Some (fun fmt -> fprintf fmt "%s" aname))) atype
                               self#pAttrs rest
			 end)
		  in Args.pp_prms
	      | Some fundecl ->
		  let module Args =
		    Args(struct
			   type t = varinfo
			   let args =
			     (try Some (getFormalsDecl fundecl)
			      with Not_found -> None)
			   let pp_args = self#pVDecl
			 end)
		  in Args.pp_prms
	 in
	 self#pType (Some pp_prms) fmt restyp
     | TNamed (t, a) ->
	 fprintf fmt "%a%a%a"
	   self#pVarName t.tname
	   self#pAttrs a
	   pname true

     | TBuiltin_va_list a ->
	 fprintf fmt "__builtin_va_list%a%a"
	   self#pAttrs a
	   pname true


   (**** PRINTING ATTRIBUTES *********)
   method pAttrs fmt (a: attributes) =
     self#pAttrsGen false fmt a


   (* Print one attribute. Return also an indication whether this attribute
    * should be printed inside the __attribute__ list *)
   method pAttr fmt = function
       Attr(an, args) ->
	 (* Recognize and take care of some known cases *)
	 (match an, args with
	    "const", [] -> fprintf fmt "const"; false
	      (* Put the aconst inside the attribute list *)
	  | "aconst", [] when not theMachine.msvcMode ->
	      fprintf fmt "__const__"; true
	  | "thread", [] when not theMachine.msvcMode ->
	      fprintf fmt "__thread"; false
	      (*
		| "used", [] when not !msvcMode -> text "__attribute_used__", false
	      *)
	  | "volatile", [] -> fprintf fmt "volatile"; false
	  | "restrict", [] -> fprintf fmt "__restrict"; false
	  | "missingproto", [] -> fprintf fmt "/* missing proto */"; false
	  | "cdecl", [] when theMachine.msvcMode -> fprintf fmt "__cdecl"; false
	  | "stdcall", [] when theMachine.msvcMode ->
	      fprintf fmt "__stdcall"; false
	  | "fastcall", [] when theMachine.msvcMode -> fprintf fmt "__fastcall"; false
	  | "declspec", args when theMachine.msvcMode ->
	      fprintf fmt "__declspec(%a)"
		(Pretty_utils.pp_list ~sep:"" self#pAttrParam) args;
	      false
	  | "w64", [] when theMachine.msvcMode -> fprintf fmt "__w64"; false
	  | "asm", args ->
	      fprintf fmt "__asm__(%a)"
		(Pretty_utils.pp_list ~sep:"" self#pAttrParam) args;
	      false
		(* we suppress printing mode(__si__) because it triggers an *)
		(* internal compiler error in all current gcc versions *)
		(* sm: I've now encountered a problem with mode(__hi__)... *)
		(* I don't know what's going on, but let's try disabling all "mode"..*)
	  | "mode", [ACons(tag,[])] ->
	      fprintf fmt "/* mode(%s) */" tag;
	      false

	  (* sm: also suppress "format" because we seem to print it in *)
	  (* a way gcc does not like *)
	  | "format", _ -> fprintf fmt "/* format attribute */";
	      false

	  | "hidden", _ -> (* hidden attribute list *)
	      fprintf fmt ""; false
	  (* sm: here's another one I don't want to see gcc warnings about.. *)
	  | "mayPointToStack", _ when not miscState.print_CIL_Input
	      (* [matth: may be inside another comment.]
		 -> text "/*mayPointToStack*/", false
	      *)
	      -> fprintf fmt ""; false

	  | "arraylen", [a] ->
	      fprintf fmt "/*[%a]*/" self#pAttrParam a;
	      false
	  | "static",_ -> fprintf fmt "/* static */"; false
	  | "", _ ->
	     (fprintf fmt "%a "
		(Pretty_utils.pp_list ~sep:" " self#pAttrParam) args;
	      true)
	  | _ -> (* This is the dafault case *)
	      (* Add underscores to the name *)
	      let an' =
		if theMachine.msvcMode then "__" ^ an else "__" ^ an ^ "__"
	      in
	      if args = [] then
		(fprintf fmt "%s" an';
		 true)
	      else
		(fprintf fmt "%s(%a)"
		   an'
		   (Pretty_utils.pp_list ~sep:"," self#pAttrParam) args;
		 true))
     | AttrAnnot s ->
	 fprintf fmt "%s" (mkAttrAnnot s); false

   method private pAttrPrec (contextprec: int) fmt (a: attrparam) =
     let thisLevel = getParenthLevelAttrParam a in
     let needParens =
       if thisLevel >= contextprec then
	 true
       else if contextprec == bitwiseLevel then
	 (* quiet down some GCC warnings *)
	 thisLevel == additiveLevel || thisLevel == comparativeLevel
       else
	 false
     in
     if needParens then
       fprintf fmt "(%a)" self#pAttrParam a
     else
       self#pAttrParam fmt a


   method pAttrParam fmt a =
     let level = getParenthLevelAttrParam a in
     match a with
     | AInt n -> fprintf fmt "%a" (My_bigint.pretty ~hexa:false) n
     | AStr s -> fprintf fmt "\"%s\"" (Escape.escape_string s)
     | ACons(s, []) -> fprintf fmt "%s" s
     | ACons(s,al) ->
	 fprintf fmt "%s(%a)"
	   s
	   (Pretty_utils.pp_list ~sep:"" self#pAttrParam) al
     | ASizeOfE a -> fprintf fmt "sizeof(%a)" self#pAttrParam a
     | ASizeOf t -> fprintf fmt "sizeof(%a)" (self#pType None) t
     | AAlignOfE a -> fprintf fmt "__alignof__(%a)" self#pAttrParam a
     | AAlignOf t -> fprintf fmt "__alignof__(%a)" (self#pType None) t
     | AUnOp(u,a1) ->
	 fprintf fmt "%a %a"
	   d_unop u
	   (self#pAttrPrec level) a1

     | ABinOp(b,a1,a2) ->
	 fprintf fmt "@[(%a)%a@  (%a) @]"
	   (self#pAttrPrec level) a1
	   d_binop b
	   (self#pAttrPrec level) a2

     | ADot (ap, s) ->
	 fprintf fmt "%a.%s"
	   self#pAttrParam ap
	   s
     | AStar a1 ->
	 fprintf fmt "(*%a)"
	   (self#pAttrPrec derefStarLevel) a1
     | AAddrOf a1 ->
	 fprintf fmt "& %a" (self#pAttrPrec addrOfLevel) a1
     | AIndex (a1, a2) ->
	 fprintf fmt "%a[%a]"
	   self#pAttrParam a1
	   self#pAttrParam a2
     | AQuestion (a1, a2, a3) ->
	 fprintf fmt "%a ? %a : %a"
	   self#pAttrParam a1
	   self#pAttrParam a2
	   self#pAttrParam a3


  (* A general way of printing lists of attributes *)
  method private pAttrsGen (block: bool) fmt (a: attributes) =
    (* Scan all the attributes and separate those that must be printed inside
     * the __attribute__ list *)
    let rec loop (in__attr__: string list) = function
      | [] ->
          if in__attr__ <> [] then
            begin
	      (* sm: added 'forgcc' calls to not comment things out
	       * if CIL is the consumer; this is to address a case
	       * Daniel ran into where blockattribute(nobox) was being
	       * dropped by the merger
	       *)
	      (if block then
                 fprintf fmt " %s __blockattribute__("
		   (forgcc "/*")
	       else
                 fprintf fmt " __attribute__((");
	      Pretty_utils.pp_list ~sep:",@ "
                Format.pp_print_string fmt in__attr__;
	      fprintf fmt ")%s"
                (if block then forgcc "*/" else ")")
            end
      | x :: rest ->
          let buff = Buffer.create 17 in
          let local_fmt = formatter_of_buffer buff in
          let ina = self#pAttr local_fmt x in
          pp_print_flush local_fmt ();
          let dx = Buffer.contents buff in
          if ina then
            loop (dx :: in__attr__) rest
          else begin
            if dx <> "" then fprintf fmt " %s" dx;
            loop in__attr__ rest
          end
    in
    let keep_attr = function
      | Attr (s,_) -> not (List.mem s !reserved_attributes)
      | AttrAnnot _ -> true
    in
    loop [] (List.filter keep_attr a);

   (* Logic annotations printer *)

   method pLogic_type name fmt =
     let pname = match name with
       | Some d -> (fun fmt -> Format.fprintf fmt "@ %t" d)
       | None -> alphaunit
     in
     function
     | Ctype typ -> self#pType name fmt typ
     | Linteger ->
	 let res = 
	   if Kernel.Unicode.get () then Utf8_logic.integer else "integer" 
	 in
	 Format.fprintf fmt "%s%t" res pname
     | Lreal ->
	 let res = 
	   if Kernel.Unicode.get () then Utf8_logic.real else "real" 
	 in
	 Format.fprintf fmt "%s%t" res pname
     | Ltype ({ lt_name = name},[]) when name = Utf8_logic.boolean->
	 let res = 
	   if Kernel.Unicode.get () then Utf8_logic.boolean else "boolean" 
	 in
	 Format.fprintf fmt "%s%t" res pname
     | Ltype (s,l) ->
	 fprintf fmt "%a%a%t" self#pVarName s.lt_name
	   (Cilutil.pretty_list_del (fun fmt -> fprintf fmt "<@[")
	      (fun fmt -> fprintf fmt "@]>@ ")
	      (* the space avoids the issue of list<list<int>> where the double >
		 would be read as a shift. It could be optimized away in most of
		 the cases.
	      *)
	      (Cilutil.space_sep ",") (self#pLogic_type None)) l pname
     | Larrow (args,rt) ->
	 fprintf fmt "@[@[<2>{@ %a@]}@]%a%t"
	   (Cilutil.pretty_list
	      (Cilutil.space_sep ",") (self#pLogic_type None)) args
	   (self#pLogic_type None) rt pname
     | Lvar s -> fprintf fmt "%a%t" self#pVarName s pname

   method private pTermPrec contextprec fmt e =
     let thisLevel = getParenthLevelLogic e.term_node in
     let needParens =
       if thisLevel >= contextprec then
	 true
       else if contextprec == bitwiseLevel then
	 (* quiet down some GCC warnings *)
	 thisLevel == additiveLevel || thisLevel == comparativeLevel
       else
	 false
     in
     if needParens then
       fprintf fmt "@[<hov 2>(%a)@]" self#pTerm e
     else
       self#pTerm fmt e

   val mutable is_debug_type_mode = false
   initializer
     is_debug_type_mode <- false

   method pTerm fmt t =
     if (Kernel.debug_atleast 5) && (not is_debug_type_mode) then
       begin 
	 is_debug_type_mode <- true ;
	 fprintf fmt "/*type:%a*/" (self#pLogic_type None) t.term_type;
	 is_debug_type_mode <- false ;
       end ;
       match t.term_name with
	   [] -> self#pTerm_node fmt t
	 | _ ->
	     fprintf fmt "(@[%a:@ %a@])"
	       (Cilutil.pretty_list
		  (Cilutil.swap fprintf ":@ ") pp_print_string) t.term_name
	       self#pTerm_node t

   (* This instance variable is true the pretty-printed term is not inside 
      an \at. Hence one may not pretty-print useless Here labels. *)
   val mutable current_label = Logic_const.here_label

   method pTerm_node fmt t =
     let current_level = getParenthLevelLogic t.term_node in
      match t.term_node with
     | TConst s -> fprintf fmt "%a" d_logic_const s
     | TDataCons(ci,args) ->
	 fprintf fmt "%a%a" self#pVarName ci.ctor_name
	   (Cilutil.pretty_list_del
	      (Cilutil.swap fprintf "(@[") (Cilutil.swap fprintf "@])")
	      (Cilutil.space_sep ",") self#pTerm) args
     | TLval lv -> fprintf fmt "%a" (self#pTerm_lvalPrec current_level) lv
     | TSizeOf t -> fprintf fmt "sizeof(%a)" (self#pType None) t
     | TSizeOfE e -> fprintf fmt "sizeof(%a)" self#pTerm e
     | TSizeOfStr s -> fprintf fmt "sizeof(%S)" s
     | TAlignOf e -> fprintf fmt "alignof(%a)" (self#pType None) e
     | TAlignOfE e -> fprintf fmt "alignof(%a)" self#pTerm e
     | TUnOp (op,e) -> fprintf fmt "%a%a"
	 d_unop op (self#pTermPrec current_level) e
     | TBinOp (op,l,r) ->
	 fprintf fmt "%a%a%a"
	   (self#pTermPrec current_level) l
	   d_term_binop op
	   (self#pTermPrec current_level) r
     | TCastE (ty,e) ->
	 fprintf fmt "(%a)%a" (self#pType None) ty
	   (self#pTermPrec current_level) e
     | TAddrOf lv -> fprintf fmt "&%a" (self#pTerm_lvalPrec addrOfLevel) lv
     | TStartOf lv -> fprintf fmt "(%a)%a"
	 (self#pLogic_type None) t.term_type
	 (self#pTerm_lvalPrec current_level) lv
     | Tapp (f, labels, tl) -> fprintf fmt "%a%a%a"
	 self#pLogic_info_use f
	   self#pLabels (List.map snd labels)
	   (Cilutil.pretty_list_del
	      (fun fmt -> Format.fprintf fmt "@[(")
	      (fun fmt -> Format.fprintf fmt ")@]")
	      (Cilutil.space_sep ",") self#pTerm) tl
     | Tif (cond,th,el) ->
	 fprintf fmt "@[<2>%a?@;%a:@;%a@]"
	   (self#pTermPrec current_level) cond
	   (self#pTermPrec current_level) th
	   (self#pTermPrec current_level) el
     | Tat (t,StmtLabel sref) ->
	 let rec pickLabel = function
	   | [] -> None
	   | Label (l, _, _) :: _ -> Some l
	   | _ :: rest -> pickLabel rest
	 in let l = match pickLabel !sref.labels with
	     Some l -> l
	   | None -> Kernel.fatal "Cannot find label for \\at@.";
	 in
	   fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]" self#pTerm t l
     | Tat (t,(LogicLabel (_, l) as lab)) ->
       let old_label = current_label in
       current_label <- lab;
       begin
	 if lab = Logic_const.old_label then
	   fprintf fmt "@[\\old(@[%a@])@]" self#pTerm t
	 else
	 fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]" self#pTerm t l
       end;
       current_label <- old_label
	 
     | Toffset (l,t) -> fprintf fmt "\\offset%a(%a)" self#pLabels [l] self#pTerm t
     | Tbase_addr (l,t) -> fprintf fmt "\\base_addr%a(%a)" self#pLabels [l] self#pTerm t
     | Tblock_length (l,t) -> fprintf fmt "\\block_length%a(%a)" self#pLabels [l] self#pTerm t
     | Tnull -> fprintf fmt "\\null"
     | TCoerce (e,ty) ->
	 fprintf fmt "%a@ :>@ %a"
	   (self#pTermPrec current_level) e (self#pType None) ty
     | TCoerceE (e,ce) ->
	 fprintf fmt "%a :> %a"
	   (self#pTermPrec current_level) e (self#pTermPrec current_level) ce
     | TUpdate (t,toff,v) ->
	 fprintf fmt "{%a \\with %a = %a}"
	   self#pTerm t
	   self#pTerm_offset toff
	   self#pTerm v
     | Tlambda(prms,expr) ->
	 fprintf fmt "@[<2>\\lambda@ %a;@ %a@]"
	   self#pQuantifiers prms (self#pTermPrec current_level) expr
     | Ttypeof t -> fprintf fmt "\\typeof(%a)" self#pTerm t
     | Ttype ty -> fprintf fmt "\\type(%a)" (self#pType None) ty
     | Tunion locs ->
	 fprintf fmt "@[<hov 2>\\union(@,%a)@]"
	   (Cilutil.pretty_list (Cilutil.space_sep ",") self#pTerm) locs
     | Tinter locs ->
	 fprintf fmt "@[<hov 2>\\inter(@,%a)@]"
	   (Cilutil.pretty_list (Cilutil.space_sep ",") self#pTerm) locs
     | Tempty_set -> pp_print_string fmt "\\empty"
     | Tcomprehension(lv,quant,pred) ->
	 fprintf fmt "{@[%a@ |@ %a%a@]}"
	   self#pTerm lv self#pQuantifiers quant
	   (Cilutil.pretty_opt (fun fmt p -> fprintf fmt ";@ %a"
				  self#identified_pred p))
	   pred
     | Trange(low,high) ->
	 fprintf fmt "@[%a..@,%a@]"
	   (Cilutil.pretty_opt (self#pTermPrec current_level)) low
	   (Cilutil.pretty_opt (self#pTermPrec current_level)) high
     | Tlet(def,body) ->
	 assert
	   (Kernel.verify (def.l_labels = [])
	      "invalid logic construction: local definition with label");
	 assert
	   (Kernel.verify (def.l_tparams = [])
	      "invalid logic construction: polymorphic local definition");
	 let v = def.l_var_info in
	 let args = def.l_profile in
	 let pp_defn = match def.l_body with
	   | LBterm t -> fun fmt -> self#pTerm fmt t
	   | LBpred p -> fun fmt -> self#pPredicate_named fmt p
	   | LBnone
	   | LBreads _ | LBinductive _ -> fatal "invalid logic local definition"
	 in
	 fprintf fmt "@[\\let@ %a@ =@ %t%t;@ %a@]"
	   self#pLogic_var v
	   (fun fmt -> if args <> [] then
	      fprintf fmt "@[<2>\\lambda@ %a;@]@ " self#pQuantifiers args)
	   pp_defn
	   (self#pTermPrec current_level) body

   method private pTerm_lvalPrec contextprec fmt lv =
     if getParenthLevelLogic (TLval lv) > contextprec then
       fprintf fmt "(%a)" self#pTerm_lval lv
     else
       fprintf fmt "%a" self#pTerm_lval lv

   method pTerm_lval fmt lv = match lv with
   | TVar vi, o -> fprintf fmt "%a%a" self#pLogic_var vi self#pTerm_offset o
   | TResult _, o -> fprintf fmt "\\result%a" self#pTerm_offset o
   | TMem e, TField(fi,o) ->
       fprintf fmt "%a->%a%a" (self#pTermPrec arrowLevel) e
	 self#pVarName fi.fname self#pTerm_offset o
   | TMem e, TNoOffset ->
       fprintf fmt "*%a" (self#pTermPrec derefStarLevel) e
   | TMem e, o ->
       fprintf fmt "(*%a)%a"
	 (self#pTermPrec derefStarLevel) e self#pTerm_offset o

   method pTerm_offset fmt o = match o with
   | TNoOffset -> ()
   | TField (fi,o) ->
       fprintf fmt ".%a%a" self#pVarName fi.fname self#pTerm_offset o
   | TModel (mi,o) ->
       fprintf fmt ".%a%a" self#pVarName mi.mi_name self#pTerm_offset o
   | TIndex(e,o) -> fprintf fmt "[%a]%a" self#pTerm e self#pTerm_offset o

   method pLogic_info_use fmt li = self#pLogic_var fmt li.l_var_info

   method pLogic_var fmt v =
     Format.fprintf fmt "%a" self#pVarName v.lv_name

   method pQuantifiers fmt l =
     Cilutil.pretty_list (Cilutil.space_sep ",")
       (fun fmt lv ->
	  let pvar fmt = self#pLogic_var fmt lv in
	  self#pLogic_type (Some pvar) fmt lv.lv_type)
       fmt l


   method private pPredPrec fmt (contextprec,p) =
     let thisLevel = getParenthLevelPred p in
     let needParens = thisLevel >= contextprec in
     if needParens then fprintf fmt "@[<hov 2>(%a)@]" self#pPredicate p
     else self#pPredicate fmt p

   method private named_pred fmt (parenth, names, content) =
     match names with
     | [] -> self#pPredPrec fmt (parenth,content)
     | _ :: _ ->
       if parenth = upperLevel then
	 fprintf fmt "@[%a:@ %a@]"
	   (Cilutil.pretty_list
	      (Cilutil.swap fprintf ":@ ") pp_print_string) names
	   self#pPredPrec (upperLevel,content)
       else
	 fprintf fmt "(@[%a:@ %a@])"
	   (Cilutil.pretty_list
	      (Cilutil.swap fprintf ":@ ") pp_print_string) names
	   self#pPredPrec (upperLevel,content)

   method private identified_pred fmt p =
     self#named_pred fmt (upperLevel,p.name,p.content)

   method private pPredPrec_named fmt (parenth,p) =
     self#named_pred fmt (parenth,p.name,p.content)

   method pPredicate_named fmt p = self#named_pred fmt (0,p.name,p.content)

   method pIdentified_predicate fmt p =
     (*fprintf fmt "@[IP_LOC:%a@\n@]" Extlib.pretty_position (fst p.ip_loc);*)
     if verbose then
       fprintf fmt "@[//id:%d@\n%a@]"
	 p.ip_id self#named_pred (upperLevel,p.ip_name,p.ip_content)
     else self#named_pred fmt (upperLevel,p.ip_name,p.ip_content)

   method private preds kw fmt l =
     Cilutil.pretty_list_del ignore Cilutil.nl_sep Cilutil.nl_sep
       (fun fmt p ->
	  fprintf fmt "@[%s @[%a@];@]" kw self#pIdentified_predicate p) fmt l

   method pPredicate fmt p =
     let current_level = getParenthLevelPred p in
     let term = self#pTermPrec current_level in
     match p with
     | Pfalse -> fprintf fmt "\\false"
     | Ptrue -> fprintf fmt "\\true"
     | Papp (p,labels,l) -> fprintf fmt "@[%a%a%a@]"
	 self#pLogic_info_use p
	   self#pLabels (List.map snd labels)
	   (Cilutil.pretty_list_del
	      (fun fmt -> Format.fprintf fmt "@[(")
	      (fun fmt -> Format.fprintf fmt ")@]")
	      (Cilutil.space_sep ",") self#pTerm) l
     | Prel (rel,l,r) ->
	 fprintf fmt "@[@[%a@]@ %a@ @[%a@]@]" term l d_relation rel term r
     | Pand (p1, p2) ->
	 fprintf fmt "@[%a@]@ %a@ @[%a@]"
	   self#pPredPrec_named (current_level,p1)
	   d_term_binop LAnd
	   self#pPredPrec_named (current_level,p2)
     | Por (p1, p2) ->
	 fprintf fmt "@[%a@]@ %a@ @[%a@]"
	   self#pPredPrec_named (current_level,p1)
	   d_term_binop LOr
	   self#pPredPrec_named (current_level,p2)
     | Pxor (p1, p2) ->
	 fprintf fmt "@[%a@]@ %s@ @[%a@]"
	   self#pPredPrec_named (current_level,p1)
	   (if Kernel.Unicode.get () then Utf8_logic.x_or else "^^")
	   self#pPredPrec_named (current_level,p2)
     | Pimplies (p1,p2) ->
	 fprintf fmt "@[%a@]@ %s@ @[%a@]"
	   self#pPredPrec_named (current_level,p1)
	   (if Kernel.Unicode.get () then Utf8_logic.implies else "==>")
	   self#pPredPrec_named (current_level,p2)
     | Piff (p1,p2) ->
	 fprintf fmt "@[%a@]@ %s@ @[%a@]"
	   self#pPredPrec_named (current_level,p1)
	   (if Kernel.Unicode.get () then Utf8_logic.iff else "<==>")
	   self#pPredPrec_named (current_level,p2)
     | Pnot a -> fprintf fmt "@[%s@[%a@]@]"
	 (if Kernel.Unicode.get () then Utf8_logic.neg else "!")
	   self#pPredPrec_named (current_level,a)
     | Pif (e, p1, p2) ->
	 fprintf fmt "@[<2>%a?@ %a:@ %a@]"
	   term e
	   self#pPredPrec_named (current_level, p1)
	   self#pPredPrec_named (current_level, p2)
     | Plet (def, p) ->
	 assert
	   (Kernel.verify (def.l_labels = [])
	      "invalid logic construction: local definition with label");
	 assert
	   (Kernel.verify (def.l_tparams = [])
	      "invalid logic construction: polymorphic local definition");
	 let v = def.l_var_info in
	 let args = def.l_profile in
	 let pp_defn = match def.l_body with
	   | LBterm t -> fun fmt -> self#pTerm fmt t
	   | LBpred p -> fun fmt -> self#pPredPrec_named fmt (current_level,p)
	   | LBnone
	   | LBreads _ | LBinductive _ -> fatal "invalid logic local definition"
	 in
	 fprintf fmt "@[\\let@ %a@ =@ %t%t;@ %a@]"
	   self#pLogic_var v
	   (fun fmt ->
	      if args <> [] then
		fprintf fmt "@[<2>\\lambda@ %a;@]@ " self#pQuantifiers args)
	   pp_defn
	   self#pPredPrec_named (current_level,p)
     | Pforall (quant,pred) ->
	 fprintf fmt "@[%s %a;@]@ %a"
	   (if Kernel.Unicode.get () then Utf8_logic.forall else "\\forall")
	   self#pQuantifiers quant self#pPredPrec_named (current_level,pred)
     | Pexists (quant,pred) ->
	 fprintf fmt "@[%s %a;@]@ %a"
	   (if Kernel.Unicode.get () then  Utf8_logic.exists else "\\exists")
	   self#pQuantifiers quant self#pPredPrec_named (current_level,pred)
     | Pfreeable (l,p) ->  fprintf fmt "@[\\freeable%a(@[%a@])@]" self#pLabels [l] self#pTerm p
     | Pallocable (l,p) ->  fprintf fmt "@[\\allocable%a(@[%a@])@]" self#pLabels [l] self#pTerm p
     | Pvalid (l,p) ->  fprintf fmt "@[\\valid%a(@[%a@])@]" self#pLabels [l] self#pTerm p
     | Pvalid_read (l,p) ->  fprintf fmt "@[\\valid_read%a(@[%a@])@]" self#pLabels [l] self#pTerm p
     | Pinitialized (l,p) ->  fprintf fmt "@[\\initialized%a(@[%a@])@]" self#pLabels [l] self#pTerm p
     | Pfresh (l1,l2,e1,e2) -> fprintf fmt "@[\\fresh%a(@[%a@],@[%a@])@]" 
	 self#pLabels [l1;l2] self#pTerm e1 self#pTerm e2
     | Pseparated seps ->
	 fprintf fmt "@[<2>\\separated(@,%a@,)@]"
	   (Cilutil.pretty_list (Cilutil.space_sep ",") self#pTerm) seps
     | Pat (p,StmtLabel sref) ->
       let rec pickLabel = function
	 | [] -> fatal "Cannot find label for \\at@."
	 | Label (l, _, _) :: _ -> l
	 | _ :: rest -> pickLabel rest
       in
       let l = pickLabel !sref.labels in
       fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]"
	 self#pPredPrec_named (upperLevel, p) l
     | Pat(p,(LogicLabel (_, s) as lab)) ->
	 if lab = Logic_const.old_label then
	   fprintf fmt "@[\\old(@[%a@])@]" 
	     self#pPredPrec_named (upperLevel,p)
	 else
	   fprintf fmt "@[\\at(@[@[%a@],@,%s@])@]"
	     self#pPredPrec_named (upperLevel,p) s
     | Psubtype (e,ce) ->
	 fprintf fmt "%a <: %a" term e term ce

   method private pDecrement kw fmt (t, rel) =
     match rel with
       None -> fprintf fmt "@[<2>%s@ %a;@]" kw self#pTerm t
     | Some str ->
	 (*TODO: replace this string with an interpreted variable*)
	 fprintf fmt "@[<2>%s@ %a@ for@ %s;@]" kw self#pTerm t str

   method pDecreases fmt v = self#pDecrement "decreases" fmt v

   method pLoop_variant fmt v = self#pDecrement "loop variant" fmt v

   method pAssumes fmt p =
     fprintf fmt "@[<2>assumes @[%a@];@]" self#pIdentified_predicate p

  method pPost_cond fmt (k,p) =
    let kw = get_termination_kind_name k in
    fprintf fmt "@[<2>%s @[%a@];@]" kw self#pIdentified_predicate p

   method pBehavior fmt b =
     let needed =
       match b with
	 | {b_assigns=WritesAny;b_allocation=FreeAllocAny;
	    b_assumes=[];b_requires=[];b_post_cond=[]} -> false
	 | _ -> true
     in
     if needed
     then begin
       self#set_current_behavior b;
       fprintf fmt "behavior %s:@\n  @[%a%a%a%a%a@]"
	 b.b_name
	 (Cilutil.pretty_list_del ignore Cilutil.nl_sep Cilutil.nl_sep
	    self#pAssumes)
	 b.b_assumes
	 (Cilutil.pretty_list_del ignore Cilutil.nl_sep Cilutil.nl_sep
	    self#pRequires)
	 b.b_requires
	 (Cilutil.pretty_list_del ignore Cilutil.nl_sep Cilutil.nl_sep
	    self#pPost_cond)
	 b.b_post_cond
	 (self#pAllocation ~isloop:false) b.b_allocation
	 (self#pAssignsDeps "assigns") b.b_assigns;
       self#reset_current_behavior ()
     end

   method pRequires fmt p =
     fprintf fmt "@[<2>requires @[%a@];@]"
       self#pIdentified_predicate p

   method pTerminates fmt p =
     fprintf fmt "@[<2>terminates @[%a@];@]"
       self#pIdentified_predicate p

   method pComplete_behaviors fmt p =
     fprintf fmt "@[<2>complete behaviors @[%a@];@]"
       (Cilutil.pretty_list_del
	  ignore
	  ignore
	  (Cilutil.space_sep ",")
	  pp_print_string)
       p

   method pDisjoint_behaviors fmt p =
     fprintf fmt "@[<2>disjoint behaviors @[%a@];@]"
       (Cilutil.pretty_list_del
	  ignore
	  ignore
	  (Cilutil.space_sep ",")
	  pp_print_string)
       p

   method pSpec fmt ({ spec_behavior = behaviors;
		      spec_variant = variant;
		      spec_terminates = terminates;
		      spec_complete_behaviors = complete;
		      spec_disjoint_behaviors = disjoint;
		    } as spec) =
     let default,default_requires,default_assigns,default_allocation,default_post_cond =
       match find_default_behavior spec with
	 | (Some b) as db -> db, b.b_requires,b.b_assigns,b.b_allocation,b.b_post_cond
	 | None -> None,[],WritesAny,FreeAllocAny,[]
     in
     let behaviors =
       List.filter (fun b -> not (is_default_behavior b)) behaviors
     in
     let pretty_maybe_nl needs f fmt x =
       if needs then Cilutil.nl_sep fmt;
       f fmt x
     in
     fprintf fmt "@[";
     let non_empty_default =
       match default with
         | None -> false
         | Some b ->
           self#set_current_behavior b;
           let terminates_needs_nl = default_requires <> [] && terminates <> None in
           let non_empty_prefix = default_requires <> [] || terminates <> None
           in
           let variant_needs_nl = non_empty_prefix && variant <> None in
           let non_empty_prefix = non_empty_prefix || variant <> None
           in
           let post_cond_needs_nl = non_empty_prefix && default_post_cond <> [] in
           let non_empty_prefix = non_empty_prefix || default_post_cond <> [] 
           in
           let allocation_needs_nl = non_empty_prefix && default_allocation<>FreeAllocAny in
           let non_empty_prefix = non_empty_prefix || default_allocation <>FreeAllocAny
           in
           let assigns_needs_nl = non_empty_prefix && default_assigns<>WritesAny in
           let non_empty_prefix = non_empty_prefix || default_assigns<>WritesAny
           in
           fprintf fmt "%a%a%a%a%a%a"
             (Cilutil.pretty_list Cilutil.nl_sep self#pRequires)
	     default_requires
             (Cilutil.pretty_opt
                (pretty_maybe_nl terminates_needs_nl self#pTerminates))
             terminates
             (Cilutil.pretty_opt
                (pretty_maybe_nl variant_needs_nl self#pDecreases)) variant
             (Cilutil.pretty_list Cilutil.nl_sep
                (pretty_maybe_nl post_cond_needs_nl self#pPost_cond))
             default_post_cond
             (pretty_maybe_nl allocation_needs_nl (self#pAllocation ~isloop:false))
             default_allocation
             (pretty_maybe_nl assigns_needs_nl (self#pAssignsDeps "assigns"))
             default_assigns;
	   self#reset_current_behavior ();
           non_empty_prefix
     in
     let behaviors_needs_nl = non_empty_default && behaviors <> [] in
     let non_empty_prefix = non_empty_default || behaviors <> [] in
     let complete_needs_nl = non_empty_prefix && complete <> [] in
     let non_empty_prefix = non_empty_prefix || complete <> [] in
     let disjoint_needs_nl = non_empty_prefix && disjoint <> [] in
     fprintf fmt "%a%a%a@]"
       (pretty_maybe_nl behaviors_needs_nl
	  (Cilutil.pretty_list Cilutil.nl_sep self#pBehavior))
       behaviors
       (pretty_maybe_nl complete_needs_nl
          (Cilutil.pretty_list Cilutil.nl_sep self#pComplete_behaviors))
       complete
       (pretty_maybe_nl disjoint_needs_nl
          (Cilutil.pretty_list Cilutil.nl_sep self#pDisjoint_behaviors))
       disjoint

   method pAllocation ~isloop fmt af = 
     match af with
       | FreeAllocAny -> ()
       | FreeAlloc([],[]) -> 
	   let prefix = if isloop then "loop " else "" in
	     fprintf fmt "@[%sallocates@ \\nothing;@]" prefix
       | FreeAlloc(f,a) ->
	   let prefix = if isloop then "loop " else "" in
	   let pFreeAlloc kw fmt af =
	     match af with
	       | [] -> ()
	       | _ -> Cilutil.pretty_list_del
		   (fun fmt -> fprintf fmt "%s%s@ " prefix kw)
		   (fun fmt -> fprintf fmt ";@\n")
		   (Cilutil.space_sep ",")
		   (fun fmt x -> self#pTerm fmt x.it_content)
		     fmt af
           in fprintf fmt "@[%a%a@]" (pFreeAlloc "frees") f (pFreeAlloc "allocates") a

   method pAssigns kw fmt a =
     match a with
       | WritesAny -> ()
       | Writes [] -> fprintf fmt "@[%s \\nothing;@]" kw
       | Writes l ->
         let without_result =
	   List.filter
	     (function (a,_) -> 
               not (Logic_const.is_result a.it_content ||
                      Logic_const.is_exit_status a.it_content)) 
             l
         in
         Cilutil.pretty_list_del
           (fun fmt -> fprintf fmt "%s@ " kw)
           (fun fmt -> fprintf fmt ";")
	   (Cilutil.space_sep ",")
           (fun fmt (x,_) -> self#pTerm fmt x.it_content)
           fmt without_result

   method private pAssignsDeps kw fmt a =
     self#pAssigns kw fmt a;
     match a with
         WritesAny | Writes [] -> ()
       | Writes [(b,_ as a)] when Logic_const.is_result b.it_content ->
         (* in this case, pAssigns does not write anything. *)
         self#pFrom kw fmt a
       | Writes l ->
         Cilutil.pretty_list
           ignore
           (fun fmt f -> fprintf fmt "@\n%a" (self#pFrom kw) f)
           fmt
           (List.filter (fun (_,f) -> f <> FromAny) l)

   method pFrom kw fmt (base,deps) =
     match deps with
         FromAny -> ()
       | From [] ->
         fprintf fmt "%s@ %a@ \\from \\nothing;" kw
           self#pTerm base.it_content
       | From l ->
         fprintf fmt "%s@ %a@ @[<2>\\from %a@];"
           kw self#pTerm base.it_content
           (Cilutil.pretty_list (Cilutil.space_sep ",")
              (fun fmt x -> self#pTerm fmt x.it_content)) l

   method private pLoop_pragma fmt = function
       | Widen_hints terms -> fprintf fmt "WIDEN_HINTS @[%a@]"
	   (Cilutil.pretty_list_del
	      alphaunit alphaunit
	      (Cilutil.space_sep ",") self#pTerm) terms
       | Widen_variables terms -> fprintf fmt "WIDEN_VARIABLES @[%a@]"
	   (Cilutil.pretty_list_del
	      alphaunit alphaunit
	      (Cilutil.space_sep ",") self#pTerm) terms
       | Unroll_specs terms -> fprintf fmt "UNROLL @[%a@]"
	   (Cilutil.pretty_list_del
	      alphaunit alphaunit
	      (Cilutil.space_sep ",") self#pTerm) terms

   method private pSlice_pragma fmt = function
       SPexpr t ->
	 fprintf fmt "expr @[%a@]" self#pTerm t
     | SPctrl -> pp_print_string fmt "ctrl"
     | SPstmt -> pp_print_string fmt "stmt"

   method private pImpact_pragma fmt = function
   | IPexpr t -> fprintf fmt "expr @[%a@]" self#pTerm t
   | IPstmt -> pp_print_string fmt "stmt"

   (* TODO: add the annot ID in debug mode?*)
   method pCode_annot fmt ca =
     match ca.annot_content with
     | AAssert (behav,p) ->
       fprintf fmt "@[%aassert@ %a;@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   self#identified_pred p
     | APragma (Slice_pragma sp) ->
	 fprintf fmt "@[slice pragma@ %a;@]" self#pSlice_pragma sp
     | APragma (Impact_pragma sp) ->
	 fprintf fmt "@[impact pragma@ %a;@]" self#pImpact_pragma sp
     | APragma (Loop_pragma lp) ->
	 fprintf fmt "@[loop pragma@ %a;@]" self#pLoop_pragma lp
     | AStmtSpec (behav,sp) ->
	 fprintf fmt "@[<2>%a%a@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   self#pSpec sp
     | AAssigns(behav,a) ->
	 fprintf fmt "@[<2>%a%a@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   (self#pAssignsDeps "loop assigns") a
     | AAllocation(behav,af) ->
	 fprintf fmt "@[<2>%a%a@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   (self#pAllocation ~isloop:true) af
    | AInvariant(behav,true, i) ->
	 fprintf fmt "@[<2>%aloop invariant@ %a;@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   self#identified_pred i
     | AInvariant(behav,false,i) -> fprintf fmt "@[<2>%ainvariant@ %a;@]"
	 (Cilutil.pretty_list_del
	    (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	    (Cilutil.space_sep ",") pp_print_string)
	   behav
	   self#identified_pred i
     | AVariant v -> self#pLoop_variant fmt v

   method private pLoopInv fmt p =
     fprintf fmt "@[<2>loop invariant@ %a;@]"
       self#identified_pred p

   method private pLogicPrms fmt arg =
     let pvar fmt = self#pLogic_var fmt arg in
     self#pLogic_type (Some pvar) fmt arg.lv_type

   method private pTypeKernel fmt tvars =
     Cilutil.pretty_list_del
       (fun fmt -> fprintf fmt "<@[") (fun fmt -> fprintf fmt "@]>")
       (Cilutil.space_sep ",") pp_print_string fmt tvars

   method private pLogicLabel fmt lab =
     let s =
       match lab with
	 | LogicLabel (_, s) -> s
	 | StmtLabel sref ->
	     let rec pickLabel = function
		 [] -> None
	       | Label (l, _, _) :: _ -> Some l
	       | _ :: rest -> pickLabel rest
	     in
	     match pickLabel !sref.labels with
		 Some l -> l
	       | None -> "__invalid_label"
     in pp_print_string fmt s

   method private pLabels fmt labels =
     match labels with 
     | [l] when current_label = l -> ()
     | _ -> Cilutil.pretty_list_del
       (fun fmt -> fprintf fmt "{@[") (fun fmt -> fprintf fmt "@]}")
       (Cilutil.space_sep ",") self#pLogicLabel fmt labels

   method pModel_info fmt mfi =
     let print_decl fmt = Format.pp_print_string fmt mfi.mi_name in
     fprintf fmt "@[model %a@ @[<2>{@ %a@ };@]"
       (self#pType None) mfi.mi_base_type
       (self#pLogic_type (Some print_decl)) mfi.mi_field_type

   method pAnnotation fmt = function
     | Dtype_annot (a,_) ->
	 fprintf fmt "@[type invariant @[%a%a=@ %a@,;@]@]@\n"
	   self#pLogic_var a.l_var_info
	   (Cilutil.pretty_list_del
	      (fun fmt -> Format.fprintf fmt "@[(")
	      (fun fmt -> Format.fprintf fmt ")@]@ ")
	      (Cilutil.space_sep ",") self#pLogicPrms) a.l_profile
	   self#identified_pred (pred_body a.l_body)
     | Dmodel_annot (mfi,_) ->
         self#pModel_info fmt mfi
     | Dcustom_annot(_c, n ,_) ->
	 begin
	   fprintf fmt "@[custom %s: <...>@]@\n" n
	 end
    | Dinvariant (pred,_) ->
	 fprintf fmt "@[global@ invariant %a:@[@ %a;@]@]@\n"
	   self#pLogic_var pred.l_var_info
	   self#identified_pred (pred_body pred.l_body)
     | Dlemma(name, is_axiom, labels, tvars, pred,_) ->
	 fprintf fmt "@[%s@ %a%a%a:@[@ %a;@]@]@\n"
	   (if is_axiom then "axiom" else "lemma")
	   self#pVarName name
	   self#pLabels labels
	   self#pTypeKernel tvars
	   self#identified_pred pred
     | Dtype (ti,_) ->
	 fprintf fmt "@[type@ %a%a%a;@]@\n"
	   self#pVarName ti.lt_name self#pTypeKernel ti.lt_params
	   (Cilutil.pretty_opt
	      (fun fmt d -> fprintf fmt "@ =@ @[%a@]" self#pLogic_type_def d))
	   ti.lt_def
      | Dfun_or_pred (li,_) ->
	 begin
	   match li.l_type with
	     | Some rt ->
		 fprintf fmt "@[<hov 2>logic %a"
		   (self#pLogic_type None) rt
	     | None ->
               (match li.l_body with
                   LBinductive _ -> fprintf fmt "@[<hov 2>inductive"
                 | _ -> fprintf fmt "@[<hov 2>predicate")
	 end;
	 fprintf fmt " %a%a%a%a"
	   self#pLogic_var li.l_var_info
	   self#pLabels li.l_labels
	   self#pTypeKernel li.l_tparams
	   (Cilutil.pretty_list_del
	      (fun fmt -> Format.fprintf fmt "@[(")
	      (fun fmt -> Format.fprintf fmt ")@]@ ")
	      (Cilutil.space_sep ",") self#pLogicPrms) li.l_profile;
	 begin
	   match li.l_body with
	     | LBnone ->
		 fprintf fmt ";"
	     | LBreads reads ->
	       (
	       match reads with
		 | [] -> fprintf fmt "@\n@[reads \\nothing;@]"
		 | _ -> 
		   fprintf fmt "%a;"
		     (Cilutil.pretty_list_del
			(fun fmt -> Format.fprintf fmt "@\n@[reads@ ")
			(fun fmt -> Format.fprintf fmt "@]")
			(Cilutil.space_sep ",")
			(fun fmt x -> self#pTerm fmt x.it_content)) reads
	       )
	     | LBpred def ->
		 fprintf fmt "=@ %a;"
		   self#identified_pred def
	     | LBinductive indcases ->
		 fprintf fmt "{@ %a}"
		   (Cilutil.pretty_list_del
		      (fun fmt -> Format.fprintf fmt "@[<v 0>")
		      (fun fmt -> Format.fprintf fmt "@]@\n")
		      Cilutil.nl_sep
		      (fun fmt (id,labels,tvars,p) ->
			 Format.fprintf fmt "case %s%a%a: @[%a@];" id
			   self#pLabels labels
			   self#pTypeKernel tvars
			   self#identified_pred p)) indcases
	     | LBterm def ->
		 fprintf fmt "=@ %a;"
		   self#pTerm def
	 end;
	 fprintf fmt "@]@\n"
     | Dvolatile(tsets,rvi_opt,wvi_opt,_) ->
	 let pp_vol txt fmt = function
	   | None -> () ;
	   | Some vi -> fprintf fmt "@ %s %a" txt self#pVar vi
	 in
	   fprintf fmt "@[<hov 2>volatile@ %a%a%a;@]"
	     (Cilutil.pretty_list (Cilutil.space_sep ",")
              (fun fmt x -> self#pTerm fmt x.it_content)) tsets
	     (pp_vol "reads") rvi_opt
	     (pp_vol "writes") wvi_opt ;
     | Daxiomatic(id,decls,_) ->
 (*
	 Format.eprintf "cil.pAnnotation on axiomatic %s@." id;
 *)
	 fprintf fmt "@[<v 2>axiomatic@ %s {@\n%a}@]@\n" id
	   (Cilutil.pretty_list_del
	      (fun fmt -> Format.fprintf fmt "@[<v 0>")
	      (fun fmt -> Format.fprintf fmt "@]@\n")
	      Cilutil.nl_sep
	      self#pAnnotation)
	   decls

   method pLogic_type_def fmt = function
     | LTsum l ->
	 Cilutil.pretty_list (fun fmt -> fprintf fmt "@ |@ ")
	   (fun fmt info ->
	      fprintf fmt "%s@[%a@]" info.ctor_name
		(Cilutil.pretty_list_del
		   (fun fmt -> fprintf fmt "@[(")
		   (fun fmt -> fprintf fmt ")@]")
		   (Cilutil.space_sep ",")
		   (self#pLogic_type None)) info.ctor_params) fmt l
     | LTsyn typ -> self#pLogic_type None fmt typ

   method pFile fmt file =
     fprintf fmt "@[/* Generated by Frama-C */@\n" ;
     iterGlobals file (fun g -> self#pGlobal fmt g);
     fprintf fmt "@]@."


 end (* class defaultCilPrinterClass *)

 let defaultCilPrinter = new defaultCilPrinterClass

 (* Top-level printing functions *)
 let printType (pp: cilPrinter) fmt (t: typ) =
   pp#pType None fmt t

 let printExp (pp: cilPrinter) fmt (e: exp) =
   pp#pExp fmt e

 let printVar (pp:#cilPrinter) fmt v = pp#pVar fmt v

 let printLval (pp: cilPrinter) fmt (lv: lval) =
   pp#pLval fmt lv

 let printGlobal (pp: cilPrinter) fmt (g: global) =
   pp#pGlobal fmt g

 let printAttr (pp: cilPrinter) fmt (a: attribute) =
   ignore (pp#pAttr fmt a)

 let printAttrs (pp: cilPrinter) fmt (a: attributes) =
   pp#pAttrs fmt a

 let printInstr (pp: cilPrinter) fmt (i: instr) =
   pp#pInstr fmt i

 let printStmt (pp: cilPrinter) fmt (s: stmt) =
   pp#pStmt fmt s

 let printBlock (pp: cilPrinter) fmt (b: block) =
   (* NB: eta expansion needed because of optional args of pBlock. *)
   fprintf fmt "@[%a@]" (fun fmt -> pp#pBlock ~nobrace:false fmt) b

 let printInit (pp: cilPrinter) fmt (i: init) =
   pp#pInit fmt i

 let printTerm_lval pp fmt lv = pp#pTerm_lval fmt lv

 let printLogic_var pp fmt lv = pp#pLogic_var fmt lv

 let printLogic_type pp fmt lv = pp#pLogic_type None fmt lv

 let printTerm pp fmt t = pp#pTerm fmt t

 let printTerm_offset pp fmt o = pp#pTerm_offset fmt o

 let printPredicate_named pp fmt p = pp#pPredicate_named fmt p
 let printIdentified_predicate pp fmt p = pp#pIdentified_predicate fmt p

 let printCode_annotation pp fmt ca = pp#pCode_annot fmt ca

 let printFunspec pp fmt s = pp#pSpec fmt s

 let printBehavior pp fmt b = pp#pBehavior fmt b

 let printModel_info pp fmt mi = pp#pModel_info fmt mi

 let printAnnotation pp fmt a = pp#pAnnotation fmt a
 let printDecreases pp fmt a = pp#pDecreases fmt a
 let printLoop_variant pp fmt a = pp#pLoop_variant fmt a
 let printAssigns pp kw fmt a = pp#pAssigns kw fmt a
 let printAllocation pp ~isloop fmt a = pp#pAllocation ~isloop fmt a
 let printFrom pp kw fmt a = pp#pFrom kw fmt a

 let printFile pp fmt file = pp#pFile fmt file

 (* Now define some short cuts *)
 let () = Cil_datatype.Constant.pretty_ref := d_const
 let d_exp fmt e = printExp defaultCilPrinter fmt e
 let () = Cil_datatype.Exp.pretty_ref := d_exp
 let d_var fmt v = printVar defaultCilPrinter fmt v
 let () = Cil_datatype.Varinfo.pretty_ref := d_var
 let d_lval fmt lv = printLval defaultCilPrinter fmt lv
 let () = Cil_datatype.Lval.pretty_ref := d_lval
 let d_offset fmt off = defaultCilPrinter#pOffset fmt off
 let () = Cil_datatype.Offset.pretty_ref := d_offset
 let d_init fmt i = printInit defaultCilPrinter fmt i
 let d_type fmt t = printType defaultCilPrinter fmt t
 let () = Cil_datatype.pretty_typ_ref := d_type
 let d_global fmt g = printGlobal defaultCilPrinter fmt g
 let d_attrlist fmt a = printAttrs defaultCilPrinter fmt a
 let () = pd_attrlist:=d_attrlist
 let d_attr fmt a = printAttr defaultCilPrinter fmt a
 let () = Cil_datatype.Attribute.pretty_ref := d_attr
 let () = pd_attr:=d_attr
 let d_attrparam fmt e = defaultCilPrinter#pAttrParam fmt e
 let d_label fmt l = defaultCilPrinter#pLabel fmt l
 let d_stmt fmt s = printStmt defaultCilPrinter fmt s
 let () = Cil_datatype.Stmt.pretty_ref := d_stmt
 let d_block fmt b = printBlock defaultCilPrinter fmt b
 let () = Cil_datatype.Block.pretty_ref := d_block
 let d_instr fmt i = printInstr defaultCilPrinter fmt i
 let () = Cil_datatype.Instr.pretty_ref := d_instr

 let d_term_lval fmt lv = printTerm_lval defaultCilPrinter fmt lv
 let d_logic_var fmt lv = printLogic_var defaultCilPrinter fmt lv
 let () = Cil_datatype.Logic_var.pretty_ref := d_logic_var
 let d_model_info fmt mi = printModel_info defaultCilPrinter fmt mi
 let () = Cil_datatype.Model_info.pretty_ref := d_model_info
 let d_logic_type fmt lv = printLogic_type defaultCilPrinter fmt lv
 let () = Cil_datatype.pretty_logic_type_ref := d_logic_type
 let d_term fmt lv = printTerm defaultCilPrinter fmt lv
 let () = Cil_datatype.Term.pretty_ref := d_term
 let d_term_offset fmt lv = printTerm_offset defaultCilPrinter fmt lv

 let d_predicate_named fmt lv = printPredicate_named defaultCilPrinter fmt lv
 let d_identified_predicate fmt p =
   printIdentified_predicate defaultCilPrinter fmt p
 let d_code_annotation fmt lv = printCode_annotation defaultCilPrinter fmt lv
 let () = Cil_datatype.Code_annotation.pretty_ref := d_code_annotation
 let d_funspec fmt lv = printFunspec defaultCilPrinter fmt lv
 let d_behavior fmt b = printBehavior defaultCilPrinter fmt b
 let d_annotation fmt lv = printAnnotation defaultCilPrinter fmt lv
 let d_decreases fmt lv = printDecreases defaultCilPrinter fmt lv
 let d_loop_variant fmt lv = printLoop_variant defaultCilPrinter fmt lv
 let d_from fmt f = printFrom defaultCilPrinter "assigns" fmt f
 let d_assigns fmt a = printAssigns defaultCilPrinter "assigns" fmt a
 let d_allocation fmt a = printAllocation defaultCilPrinter ~isloop:false fmt a
 let d_loop_from fmt f = printFrom defaultCilPrinter "loop assigns" fmt f
 let d_loop_assigns fmt a = printAssigns defaultCilPrinter "loop assigns" fmt a
 let d_loop_allocation fmt a = printAllocation defaultCilPrinter ~isloop:true fmt a
 let d_file fmt f = printFile defaultCilPrinter fmt f


 let () = pd_exp := d_exp
 let () = pd_global := d_global
 let () = pd_type := d_type

 (* sm: given an ordinary CIL object printer, yield one which
  * behaves the same, except it never prints #line directives
  * (this is useful for debugging printfs) *)
 let dn_obj (func: formatter -> 'a -> unit) : (formatter -> 'a -> unit) =
 begin
   (* construct the closure to return *)
   let theFunc fmt (obj:'a) =
   begin
     let prevStyle = miscState.lineDirectiveStyle in
     miscState.lineDirectiveStyle <- None;
     func fmt obj;    (* call underlying printer *)
     miscState.lineDirectiveStyle <- prevStyle
   end in
   theFunc
 end

 (* now define shortcuts for the non-location-printing versions,
  * with the naming prefix "dn_" *)
 let dn_exp       = (dn_obj d_exp)
 let dn_lval      = (dn_obj d_lval)
 (* dn_offset is missing because it has a different interface *)
 let dn_init      = (dn_obj d_init)
 let dn_type      = (dn_obj d_type)
 let dn_global    = (dn_obj d_global)
 let dn_attrlist  = (dn_obj d_attrlist)
 let dn_attr      = (dn_obj d_attr)
 let dn_attrparam = (dn_obj d_attrparam)
 let dn_stmt      = (dn_obj d_stmt)
 let dn_instr     = (dn_obj d_instr)



(* Strip the "const" from the type. It is unfortunate that const variables
 * can only be set in initialization. Once we decided to move all
 * declarations to the top of the functions, we have no way of setting a
 * "const" variable. Furthermore, if the type of the variable is an array or
 * a struct we must recursively strip the "const" from fields and array
 * elements. *)
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


(* And now some shortcuts *)
let d_plainexp fmt e = defaultCilPrinter#pExp fmt e
let d_plaintype fmt t = defaultCilPrinter#pType None fmt t
let d_plaininit fmt i = defaultCilPrinter#pInit fmt i
let d_plainlval fmt l = defaultCilPrinter#pLval fmt l
class type descriptiveCilPrinter = object
  inherit cilPrinter
   method startTemps: unit -> unit
   method stopTemps: unit -> unit
   method pTemps: Format.formatter -> unit
 end

 class descriptiveCilPrinterClass : descriptiveCilPrinter = object (self)
   (** Like defaultCilPrinterClass, but instead of temporary variable
       names it prints the description that was provided when the temp was
       created.  This is usually better for messages that are printed for end
       users, although you may want the temporary names for debugging.  *)
   inherit defaultCilPrinterClass as super

   val mutable temps: (varinfo * string * string option) list = []
   val mutable useTemps: bool = false

   method startTemps () : unit =
     temps <- [];
     useTemps <- true

   method stopTemps () : unit =
     temps <- [];
     useTemps <- false

   method pTemps fmt  =
     if temps = [] then
       ()
     else
       fprintf fmt "@\nWhere:@\n  %a"
	 (Pretty_utils.pp_list ~sep:"\n  "
	 (let f fmt v = match v with
	  | (_, s, Some d) -> fprintf fmt "%s = %s" s  d
	  |(_, s, None) -> fprintf fmt  "%s = " s in f))
	 (List.rev temps)

   method private pVarDescriptive fmt (vi: varinfo) =
     match vi.vdescr with
     | Some vd ->
	 if vi.vdescrpure || not useTemps then
	   fprintf fmt "%s" vd
	 else begin
	 try
	   let _, name, _ = List.find (fun (vi', _, _) -> vi == vi') temps in
	   fprintf fmt "%s" name
	 with Not_found ->
	   let name = "tmp" ^ string_of_int (List.length temps) in
	   temps <- (vi, name, vi.vdescr) :: temps;
	   fprintf fmt "%s" name
	 end
     | None ->
       super#pVar fmt vi

   (* Only substitute temp vars that appear in expressions.
      (Other occurrences of lvalues are the left-hand sides of assignments,
       but we shouldn't substitute there since "foo(a,b) = foo(a,b)"
       would make no sense to the user.)  *)
   method pExp fmt (e:exp) =
     match e.enode with
       Lval (Var vi, o)
     | StartOf (Var vi, o) ->
	 fprintf fmt "%a%a" self#pVarDescriptive vi self#pOffset o
     | AddrOf (Var vi, o) ->
	 (* No parens needed, since offsets have higher precedence than & *)
	 fprintf fmt "& %a%a" self#pVarDescriptive vi self#pOffset o
     | _ -> super#pExp fmt e
 end

 let descriptiveCilPrinter: descriptiveCilPrinter =
   ((new descriptiveCilPrinterClass) :> descriptiveCilPrinter)

 let dd_exp = descriptiveCilPrinter#pExp
 let dd_lval = descriptiveCilPrinter#pLval

 let cvar_to_lvar vi =
   match vi.vlogic_var_assoc with
       None ->
	 let lv =
	   { lv_name = vi.vname;
	     lv_id = vi.vid;
	     lv_type = Ctype vi.vtype ;
	     lv_origin = Some vi}
	 in vi.vlogic_var_assoc <- Some lv; lv
     | Some lv -> lv

 let copyVarinfo (vi: varinfo) (newname: string) : varinfo =
   let vi' = copy_with_new_vid vi in
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

 let makeLocal ?(generated=true) ?(formal=false) fdec name typ =
   (* a helper function *)
   let name = findUniqueName fdec name in
   fdec.smaxid <- 1 + fdec.smaxid;
   let vi = makeVarinfo ~generated false formal name typ in
   vi

 (* Make a local variable and add it to a function *)
 let makeLocalVar fdec ?scope ?(generated=true) ?(insert = true) name typ =
   let typ = stripConstLocalType typ in
   let vi = makeLocal ~generated fdec name typ in
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
   let vi = makeLocalVar fdec ?insert name typ in
   vi.vdescr <- descr;
   vi.vdescrpure <- descrpure;
   vi

 let makePseudoVar =
   let counter = ref 0 in
   function ty ->
     incr counter;
     let name = "@" ^ (string_of_int !counter) in
     makeVarinfo ~logic:true (* global= *)false (* formal= *)false name ty

    (* Set the types of arguments and results as given by the function type
     * passed as the second argument *)
 let setFunctionType (f: fundec) (t: typ) =
   match unrollType t with
     TFun (_rt, Some args, _va, _a) ->
       if List.length f.sformals <> List.length args then
	 Kernel.fatal "setFunctionType: number of arguments differs from the number of formals" ;
       (* Change the function type. *)
       f.svar.vtype <- t;
       (* Change the sformals and we know that indirectly we'll change the
	* function type *)
       List.iter2
	 (fun (_an,at,aa) f ->
	   f.vtype <- at; f.vattr <- aa)
	 args f.sformals

   | _ -> Kernel.fatal "setFunctionType: not a function type"


    (* Set the types of arguments and results as given by the function type
     * passed as the second argument *)
 let setFunctionTypeMakeFormals (f: fundec) (t: typ) =
   match unrollType t with
     TFun (_rt, Some args, _va, _a) ->
       if f.sformals <> [] then
	 Kernel.fatal "setFunctionTypMakeFormals called on function %s with some formals already"
	   f.svar.vname ;
       (* Change the function type. *)
       f.svar.vtype <- t;
       f.sformals <- [];

       f.sformals <- List.map (fun (n,t,_a) -> makeLocal ~formal:true f n t) args;

       setFunctionType f t

   | _ -> Kernel.fatal "setFunctionTypeMakeFormals: not a function type: %a" d_type t


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
	 else Kernel.fatal
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
 let makeGlobalVar ?logic ?generated name typ =
   let vi = makeVarinfo ?logic ?generated true false name typ in
   vi

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
     makeGlobalVar ~generated:false name (TFun(voidType, Some [], false,[]))
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
     inherit genericCilVisitor ~prj:(Project.current ()) (copy_visit())
     method vexpr e = 
       ChangeDoChildrenPost ({e with eid = Eid.next ()}, fun x -> x)
 end

 let copy_exp e = visitCilExpr (new copyVisitExpr) e

 (** A visitor that does constant folding. If "machdep" is true then we do
  * machine dependent simplification (e.g., sizeof) *)
 class constFoldVisitorClass (machdep: bool) : cilVisitor = object
   inherit nopCilVisitor

   method vinst i =
     match i with
       (* Skip two functions to which we add Sizeof to the type arguments.
	  See the comments for these above. *)
       Call(_,({enode = Lval (Var vi,NoOffset)}),_,_)
	 when ((vi.vname = "__builtin_va_arg")
	       || (vi.vname = "__builtin_types_compatible_p")) ->
	   SkipChildren
     | _ -> DoChildren
   method vexpr (e: exp) =
     (* Do it bottom up *)
     ChangeDoChildrenPost (e, constFold machdep)

 end
 let constFoldVisitor (machdep: bool) = new constFoldVisitorClass machdep

 let rec constFoldTermNodeAtTop t =
   match t with
     | TSizeOf typ ->
	 (try integer_lconstant (sizeOf_int typ)
	  with SizeOfError _ -> t)
     | TSizeOfStr str -> integer_lconstant (String.length str + 1)
     | TAlignOf typ -> integer_lconstant (alignOf_int typ)
     | TSizeOfE { term_type= Ctype typ } ->
	 constFoldTermNodeAtTop (TSizeOf typ)
     | TAlignOfE { term_type= Ctype typ }
       -> 	constFoldTermNodeAtTop (TAlignOf typ)
     | TSizeOfE _ | TAlignOfE _ ->
	 assert false (* sizeof/alignof of logic types are rejected
			 by typing anyway. *)
     | _ -> t

 let constFoldTerm machdep t =
   let visitor = object
     inherit nopCilVisitor
     method vterm_node t =
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
	 GVarDecl(_,vi,_) :: _rest when vi.vname = name ->
	   if not (isFunctionType vi.vtype) then
	     fatal
	       "findOrCreateFunc: can't create %s because another global exists with that name."
	       name ;
	   vi
       | _ :: rest -> search rest (* tail recursive *)
       | [] -> (*not found, so create one *)
	   let t' = unrollTypeDeep t in
	   let new_decl = makeGlobalVar ~generated:false name t' in
	   setFormalsDecl new_decl t';
	   f.globals <- GVarDecl(empty_funspec (), new_decl, Location.unknown) :: f.globals;
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
             fatal
	       "You used visitCilFileSameGlobals but the global got changed:\n %a\nchanged to %a\n"
	       d_global g (Pretty_utils.pp_list ~sep:"@\n" d_global) gl ;
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
     Kernel.fatal "You used visitCilFileSameGlobals with a copy visitor. Nothing is done"
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
     Kernel.fatal "You used visitCilFile with a copy visitor. Nothing is done"
   else ignore (visitCilFileCopy vis f)


 let appears_in_expr v e =
   let module M = struct exception Found end in
   let vis = object
     inherit nopCilVisitor
     method vvrbl v' =
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
	     GFun(m, lm) when m.svar.vname = main_name ->
	       (* Prepend a prototype to the global initializer *)
	       fl.globals <- GVarDecl (empty_funspec (),f.svar, lm) :: fl.globals;
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
       | _ -> Kernel.fatal "mapGlobals: globinit is not a function"
   end)

 let dumpFile (pp: cilPrinter) (out : out_channel) (outfile: string) file =
   let fmt = formatter_of_out_channel out in
   pp_set_max_boxes fmt max_int;  (* We don't want ... in the output *)
   pp_set_margin fmt 79;

   Kernel.feedback ~level:2 "printing file %s" outfile ;
   printFile pp fmt file;
   flush out



 (******************
  ******************
  ******************)


 (* Convert an expression into an attribute, if possible. Otherwise raise
  * NotAnAttrParam *)
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
       | TryFinally (b, h, _l) ->
	   peepHole1 doone b.bstmts;
	   peepHole1 doone h.bstmts
       | TryExcept (b, (il, e), h, l) ->
	   peepHole1 doone b.bstmts;
	   peepHole1 doone h.bstmts;
	   s.skind <- TryExcept(b, (doInstrList il, e), h, l);
       | Return _ | Goto _ | Break _ | Continue _ -> ())
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
       | TryFinally (b, h, _l) ->
         b.bstmts <- 
           peepHole2 ~agressive dotwo b.bstmts;
	 b.bstmts <- peepHole2 ~agressive dotwo h.bstmts
       | TryExcept (b, (_il, _e), h, _l) ->
	   b.bstmts <- peepHole2 ~agressive dotwo b.bstmts;
	   h.bstmts <- peepHole2 ~agressive dotwo h.bstmts;
	   () (*s.skind <- TryExcept (b, (doInstrList il, e), h, l)*)

       | UnspecifiedSequence seq ->
	   s.skind <- UnspecifiedSequence (doUnspecifiedStmtList seq)
       | Return _ | Goto _ | Break _ | Continue _ -> ()
   in
   if agressive then List.iter process ss;
   doStmtList [] ss

 let dExp: string -> exp =
   fun d -> new_exp ~loc:Cil_datatype.Location.unknown (Const(CStr(d)))

 let dInstr: string -> location -> instr =
   fun d l -> Asm([], [d], [], [], [], l)

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
   | _ -> new_exp ?loc (AddrOf lval)

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
     | _ -> fatal "splitFunctionType invoked on a non function type %a"
       d_type ftype

 let splitFunctionTypeVI (fvi: varinfo)
     : typ * (string * typ * attributes) list option * bool * attributes =
   match unrollType fvi.vtype with
     TFun (rt, args, isva, a) -> rt, args, isva, a
   | _ -> Kernel.abort "Function %s invoked on a non function type" fvi.vname

 let rec integralPromotion (t : typ) : typ = (* c.f. ISO 6.3.1.1 *)
   match unrollType t with
   | TInt ((IShort|ISChar|IBool), a) -> TInt(IInt, a)
   | TInt (IChar,a) when isSigned IChar -> TInt(IInt, a)
   | TInt (IUChar|IUShort as k, a) -> 
     if bitsSizeOfInt k < bitsSizeOf intType then 
       TInt(IInt, a)
     else
       TInt(IUInt,a)
   | TInt (k,a) ->
     begin match findAttribute "FRAMA_C_BITFIELD_SIZE" a with
     | [AInt size] ->
       (* This attribute always fits in int. *)
       let size = My_bigint.to_int size in
       let sizeofint = bitsSizeOf intType in
       let attrs = dropAttribute "FRAMA_C_BITFIELD_SIZE" a in
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
   | TEnum (ei, a) -> integralPromotion (TInt(ei.ekind, a))
   (* gcc packed enums can be < int *)
   | t -> fatal "integralPromotion: not expecting %a" d_type t

 let arithmeticConversion  t1 t2 = (* c.f. ISO 6.3.1.8 *)
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
        fatal "arithmeticConversion %a -> %a@." d_type t1 d_type t2
  end

let isArrayType t =
   match unrollTypeSkel t with
     TArray _ -> true
   | _ -> false

 let isCharArrayType t =
   match unrollTypeSkel t with
     TArray(tau,_,_,_) when isCharType tau -> true
   | _ -> false

 let isStructOrUnionType t =
   match unrollTypeSkel t with
     TComp _ -> true
   | _ -> false

 let isVariadicListType t =
   match unrollTypeSkel t with
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
	  CInt64 _ | CChr _ | CEnum _ -> true
	| CStr _ | CWStr _ | CReal _ -> false) e

 let getCompField (cinfo:compinfo) (fieldName:string) : fieldinfo =
   (List.find (fun fi -> fi.fname = fieldName) cinfo.cfields)

let mkCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  let loc = e.eloc in
  if not (need_cast oldt newt) then e
  else begin
    let mk_cast exp = (* to new type [newt] *)
      new_exp
        ~loc
        (CastE((type_remove_attributes_for_c_cast newt),exp))
    in
    (* Watch out for constants and cast of cast to pointer *)
    match unrollType newt, e.enode with
        (* In the case were we have a representation for the literal,
           explicitly add the cast. *)
    | TInt(newik, []), Const(CInt64(i, _, None)) -> kinteger64 ~loc newik i
    | TPtr _, CastE (_, e') ->
	(match unrollType (typeOf e') with
           | (TPtr _ as typ'') ->
	       (* Old cast can be removed...*)
               if need_cast newt typ'' then mk_cast e'
	       else (* In fact, both casts can be removed. *) e'
	   | _ -> mk_cast e
	)
    | _ ->   
	(* Do not remove old casts because they are conversions !!! *)
	mk_cast e
  end

 let mkCast ~(e: exp) ~(newt: typ) =
   mkCastT e (typeOf e) newt

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
     else fatal "mkBinOp: %a"
       d_plainexp (dummy_exp(BinOp(op,e1,e2,intType)))
   in
   match op with
       (Mult|Div) -> doArithmetic ()
     | (Mod|BAnd|BOr|BXor|LAnd|LOr) -> doIntegralArithmetic ()
     | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result
                             * has the same type as the left hand side *)
       if theMachine.msvcMode then
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
       Kernel.fatal "mkBinOp: %a"
	 d_plainexp (dummy_exp(BinOp(op,e1,e2,intType)))


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
     (new_exp ~loc:e.eloc
        (BinOp(bop, e, kinteger64 ~loc:e.eloc IULongLong i, et)))

 exception LenOfArray
 let lenOfArray64 eo =
   match eo with
     None -> raise LenOfArray
   | Some e -> begin
       match (constFold true e).enode with
       | Const(CInt64(ni, _, _)) when My_bigint.ge ni My_bigint.zero ->
	   ni
       | _ -> raise LenOfArray
     end
 let lenOfArray eo = My_bigint.to_int (lenOfArray64 eo)

(*** Make an initializer for zeroe-ing a data type ***)
let rec makeZeroInit ~loc (t: typ) : init =
  match unrollType t with
    TInt (ik, _) ->
      SingleInit (new_exp ~loc (Const(CInt64(My_bigint.zero, ik, None))))
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
        | [] -> fatal "Cannot create init for empty union"
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
        match (constFold true len).enode with
          Const(CInt64(n, _, _)) -> My_bigint.to_int n
        | _ -> fatal "Cannot understand length of array"
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
   | x -> fatal "Cannot initialize type: %a" d_type x

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
     TArray(bt, leno, _, _) -> begin
       (* Scan the existing initializer *)
       let part =
	 List.fold_left (fun acc (o, i) -> doinit o i bt acc) acc initl in
       (* See how many more we have to do *)
       match leno with
	 Some lene when implicit -> begin
	   match (constFold true lene).enode with
	     Const(CInt64(i, _, _)) ->
	       let len_array = My_bigint.to_int i in
	       let len_init = List.length initl in
	       if len_array > len_init then
                 (*TODO : find a proper loc*)
                 let loc = Cil_datatype.Location.unknown in
		 let zi = makeZeroInit ~loc bt in
		 let rec loop acc i =
		   if i >= len_array then acc
		   else
		     loop (doinit (Index(integer ~loc i, NoOffset)) zi bt acc)
			  (i + 1)
		 in
		 loop part len_init
	       else
		 part
	   | _ -> fatal
	       "foldLeftCompoundAll: array with initializer and non-constant length"
	 end

       | _ when not implicit -> part

       | _ -> fatal
	   "foldLeftCompoundAll: TArray with initializer and no length"
     end

   | TComp (_comp, _, _) ->
       let getTypeOffset = function
	   Field(f, NoOffset) -> f.ftype
	 | _ -> fatal "foldLeftCompound: malformed initializer"
       in
       List.fold_left
	 (fun acc (o, i) -> doinit o i (getTypeOffset o) acc) acc initl

   | _ -> fatal "Type of Compound is not array or struct or union"




 let rec isCompleteType t =
   match unrollType t with
   | TArray(_t, None, _, _) -> false
   | TArray(_t, Some z, _, _) when isZero z -> false
   | TComp (comp, _, _) -> (* Struct or union *)
       List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields
   | _ -> true


 module A = Alpha

 (** Uniquefy the variable names *)
 let uniqueVarNames (f: file) : unit =
   (* Setup the alpha conversion table for globals *)
   let gAlphaTable: (string,
		     location A.alphaTableData ref) Hashtbl.t = Hashtbl.create 113 in
   (* Keep also track of the global names that we have used. Map them to the
    * variable ID. We do this only to check that we do not have two globals
    * with the same name. *)
   let globalNames: (string, int) Hashtbl.t = Hashtbl.create 113 in
   (* Scan the file and add the global names to the table *)
   iterGlobals f
     (function
	 GVarDecl(_,vi, _)
       | GVar(vi, _, _)
       | GFun({svar = vi}, _) ->
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
	     A.registerAlphaName gAlphaTable None vi.vname (CurrentLoc.get ())
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
	     let newname, oldloc =
	       A.newAlphaName gAlphaTable (Some undolist) v.vname
		 (CurrentLoc.get ())
	     in
	     if false && newname <> v.vname then (* Disable this warning *)
	       Kernel.warning
		 "Changing the name of local %s in %s to %s \
(due to duplicate at %a)"
		 v.vname
		 fdec.svar.vname
		 newname d_loc oldloc ;
	     v.vname <- newname
	   in
	   (* Do the formals first *)
	   List.iter processLocal fdec.sformals;
	   (* Fix the type again *)
	   setFormals fdec fdec.sformals;
	   (* And now the locals *)
	   List.iter processLocal fdec.slocals;
	   (* Undo the changes to the global table *)
	   A.undoAlphaChanges gAlphaTable !undolist;
	   ()
	 end
       | _ -> ());
   ()

let is_case_label l = match l with
  | Case _ | Default _ -> true
  | _ -> false

let init_builtins () =
  if theMachine.msvcMode then
    initMsvcBuiltins ()
  else
    initGccBuiltins ()

let initCIL initLogicBuiltins =
  if not (TheMachine.is_computed ()) then begin
    (* Set the machine *)
    theMachine.theMachine <-
      if theMachine.msvcMode then Machdep.state.Machdep.msvc
      else Machdep.state.Machdep.gcc;
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
        Kernel.fatal "initCIL: cannot find the right ikind for size %d\n" sz
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
	Kernel.fatal "initCIL: cannot find the right ikind for type %s\n" name
    in
    theMachine.upointType <-
      TInt(findIkindSz true theMachine.theMachine.sizeof_ptr, []);
    theMachine.kindOfSizeOf <-
      findIkindName theMachine.theMachine.size_t;
    theMachine.typeOfSizeOf <- TInt(theMachine.kindOfSizeOf, []);
    theMachine.wcharKind <- findIkindName theMachine.theMachine.wchar_t;
    theMachine.wcharType <- TInt(theMachine.wcharKind, []);
    theMachine.ptrdiffKind <- findIkindName theMachine.theMachine.ptrdiff_t;
    theMachine.ptrdiffType <- TInt(theMachine.ptrdiffKind, []);
    theMachine.char_is_unsigned <-
      theMachine.theMachine.Cil_types.char_is_unsigned;
    theMachine.little_endian <- theMachine.theMachine.Cil_types.little_endian;
    theMachine.underscore_name <-
      theMachine.theMachine.Cil_types.underscore_name;
    (* do not use lazy LAND and LOR *)
    theMachine.useLogicalOperators <- false;
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
  method vvrbl (vi: varinfo) =
    pacc := vi :: !pacc;
    SkipChildren

  method vglob = function
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
           g :: (List.fold_left (fun acc v -> GVarDecl(empty_funspec (),v, loc) :: acc)
                                !types vl)
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
    Fe e -> fprintf fmt "Fe(%a)" d_exp e
  | Feo None -> fprintf fmt "Feo(None)"
  | Feo (Some e) -> fprintf fmt "Feo(%a)" d_exp e
  | FE _ -> fprintf fmt "FE()"
  | Fk _ik -> fprintf fmt "Fk()"
  | Fva b -> fprintf fmt "Fva(%b)" b
  | Ff (an, _, _) -> fprintf fmt "Ff(%s)" an
  | FF _ -> fprintf fmt "FF(...)"
  | FA _ -> fprintf fmt "FA(...)"
  | Fu _uo -> fprintf fmt "Fu()"
  | Fb _bo -> fprintf fmt "Fb()"
  | Fv v -> fprintf fmt "Fv(%s)" v.vname
  | Fl l -> fprintf fmt "Fl(%a)" d_lval l
  | Flo None -> fprintf fmt "Flo(None)"
  | Flo (Some l) -> fprintf fmt "Flo(%a)" d_lval l
  | Fo _o -> fprintf fmt "Fo"
  | Fc ci -> fprintf fmt "Fc(%s)" ci.cname
  | Fi _i -> fprintf fmt "Fi(...)"
  | FI _i -> fprintf fmt "FI(...)"
  | Ft t -> fprintf fmt "Ft(%a)" d_type t
  | Fd n -> fprintf fmt "Fd(%d)" n
  | Fg s -> fprintf fmt "Fg(%s)" s
  | Fp _ -> fprintf fmt "Fp(...)"
  | FP _n -> fprintf fmt "FP(...)"
  | Fs _ -> fprintf fmt "FS"
  | FS _ -> fprintf fmt "FS"

  | FX _ -> fprintf fmt "FX()"

let pretty_loc fmt kinstr =
  fprintf fmt "Location: %a" d_loc (Kinstr.loc kinstr)

let pretty_loc_simply fmt kinstr =
  fprintf fmt "%a" d_loc (Kinstr.loc kinstr)

let make_temp_logic_var =
  let counter = ref 0 in
  fun ty ->
    incr counter;
    let name = "__framac_tmp" ^ (string_of_int !counter) in
    make_logic_var name ty

let extract_varinfos_from_exp vexp =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = Varinfo.Set.empty;
    method varinfos = varinfos
    method vvrbl (symb:varinfo) =
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
    method vvrbl (symb:varinfo) =
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
          Kernel.fatal
            "definition of local variable %s is not a term or a predicate"
            d.l_var_info.lv_name
    in
    let fvb =
      free_vars_term (Logic_var.Set.add d.l_var_info bound_vars) b
    in
    Logic_var.Set.union fvd fvb

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
  | Pvalid (_,t) | Pvalid_read (_,t) | Pinitialized (_,t) -> 
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
          Kernel.fatal
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
    method vlogic_label (label:logic_label) =
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
    method vlogic_label (label:logic_label) =
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
    method vlogic_label (label:logic_label) =
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
  method vvrbl v =
    try let v' = Hashtbl.find tbl v.vid in ChangeTo v'
    with Not_found -> DoChildren
  method vlogic_var_use v =
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


(** Provided [s] is a switch, [separate_switch_succs s] returns the
    subset of [s.succs] that correspond to the labels of [s], and an
    optional statement that is [None] if the switch has a default label,
    or [Some s'] where [s'] is the syntactic successor of [s] otherwise *)
let separate_switch_succs s =
  match s.skind with
    | Switch (_, _, cases, _) ->
        let to_set =
          List.fold_left (fun s stmt -> Stmt.Set.add stmt s) Stmt.Set.empty in
        let s_succs = to_set s.succs in
        let s_cases = to_set cases in
        let diff = Stmt.Set.diff s_succs s_cases in
        let cases = Stmt.Set.elements (Stmt.Set.inter s_succs s_cases) in
        (match Stmt.Set.elements diff with
           | [] -> cases, None
           | [s] -> cases, Some s
           | _ :: _ :: _ ->
               fatal "Bad CFG: switch with multiple non-case successors."
        )
    | _ -> raise (Invalid_argument "separate_switch_succs")


module Switch_cases =
  State_builder.Hashtbl
    (Stmt.Hashtbl)
    (Datatype.Pair(Datatype.List(Stmt))(Datatype.Option(Stmt)))
    (struct
       let name = "Switch_cases"
       let dependencies = []
       let size = 49
     end)
let () = add_ast_dependency Switch_cases.self
let () = add_ast_dependency CacheBitsOffset.self
let separate_switch_succs = Switch_cases.memo separate_switch_succs

class dropAttributes ?select () = object
  inherit genericCilVisitor ~prj:(Project.current ()) (copy_visit ())
  method vattr a = 
    match select with
      | None -> ChangeTo []
      | Some l ->
          (match a with
            | (Attr (s,_) | AttrAnnot s) when List.mem s l -> ChangeTo []
            | Attr _ | AttrAnnot _ -> DoChildren)
  method vtype ty =
    match ty with
      | TNamed (ty, attrs) ->
          ChangeDoChildrenPost (typeAddAttributes attrs ty.ttype, fun x -> x)
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

let lastTermOffset = Kernel.deprecated "Cil.lastTermOffset" ~now:"Logic_const.lastTermOffset" Logic_const.lastTermOffset
let addTermOffset = Kernel.deprecated "Cil.addTermOffset" ~now:"Logic_const.addTermOffset" Logic_const.addTermOffset
let addTermOffsetLval = Kernel.deprecated "Cil.addTermOffsetLval" ~now:"Logic_const.addTermOffsetLval" Logic_const.addTermOffsetLval

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
