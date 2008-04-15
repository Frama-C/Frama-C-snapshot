(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003,                                              *)
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
(*  File modified by CEA (Commissariat à l'Énergie Atomique).             *)
(**************************************************************************)

  (*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)

open Escape
module S = Stack
open Cilutil
module Stack = S
open Format
open Trace      (* sm: 'trace' function *)
module C = Cilutil
module E = Errormsg
module H = Hashtbl
module IH = Inthash

(* ************************************************************************* *)
(* Reporting messages *)
(* ************************************************************************* *)

(* A reference to the current location *)
module CurrentLoc = Logic_env.CurrentLoc

(* Some error reporting functions *)
let d_loc fmt loc =
  fprintf fmt "%s:%d" (fst loc).Lexing.pos_fname (fst loc).Lexing.pos_lnum
(*  fprintf fmt "File %S, line %d, characters %d-%d"
    (fst loc).Lexing.pos_fname (fst loc).Lexing.pos_lnum
    ((fst loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol)
    ((snd loc).Lexing.pos_cnum - (fst loc).Lexing.pos_bol)
*)
let rec fprintfList ~sep (f:formatter -> 'a -> unit) fmt l =
  match l with
  | [] -> ()
  | [e] -> f fmt e
  | x::r -> fprintf fmt ("%a" ^^ sep ^^ "%a") f x (fprintfList ~sep f) r

(*Ok for ocaml >= 3.09.3 : fprintf fmt "%a%(%)%a" f x sep (fprintfList ~sep f) r *)

let d_thisloc (fmt: formatter) : unit = d_loc fmt (CurrentLoc.get ())

let generic_report_error msg fmt =
  let f fmt fstring =
    E.hadErrors := true;
    fprintf fmt (fstring ^^ "@]@.")
  in
  kfprintf f fmt "@[%t: %s: " d_thisloc msg

let error_loc (file,line,start_byte,stop_byte) msg =
  let f fmt fstring =
    fprintf fmt (fstring ^^ "@]@.")
  in
  kfprintf f err_formatter "@[File %S, line %d, characters %d-%d: "  file line start_byte stop_byte msg

let error fstring = generic_report_error "Error" err_formatter fstring

let unimp fstring = generic_report_error "Unimplemented" err_formatter fstring

let generic_bug s fstring =
  let f fmt =
    E.hadErrors := true;
    kfprintf (fun _ -> E.showContext (); raise E.Error) fmt (fstring ^^ "@]@.")
  in
  kfprintf f err_formatter "@[%t: %s: " d_thisloc s

let bug fstring = generic_bug "Bug" fstring
let fatal_error fstring = generic_bug "Fatal error" fstring
let fatal_unimp fstring = generic_bug "Fatal unimplemented" fstring

let errorLoc loc fstring =
  let f fmt =
    E.hadErrors := true;
    kfprintf (fun _ -> E.showContext ()) fmt (fstring ^^ "@]@.")
  in
  kfprintf f err_formatter "@[%a: Error: " d_loc loc

let do_not_fprintf fstring =
    kfprintf (fun _ -> ()) (Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())) fstring

let fprintf_to_string fstring =
  let b = Buffer.create 17 in
  let fmt = formatter_of_buffer b in
  kfprintf (fun fmt -> pp_print_flush fmt ();
              Buffer.contents b) fmt fstring

let warn fstring = Messages_manager.emit (CurrentLoc.get ()) `Warning fstring

let warnOpt fstring =
  if !E.warnFlag then
    warn fstring
  else
    do_not_fprintf fstring

let warnContext fstring =
  let f fmt =
    kfprintf (fun _ -> E.showContext ()) fmt (fstring ^^ "@]@.")
  in
  kfprintf f err_formatter "@[%t: Warning: " d_thisloc

let warnContextOpt fstring =
  if !E.warnFlag then
    warn fstring
  else
    do_not_fprintf fstring

let warnLoc loc fstring =  Messages_manager.emit loc `Warning fstring

let logLoc loc fstring = Messages_manager.emit loc `Info fstring

(*let log fstring = Messages_manager.emit (CurrentLoc.get()) `Info fstring*)
let log fstring = Format.eprintf ("@[" ^^ fstring ^^ "@]@.")

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

let print_utf8 = ref true

let () =
  pp_set_margin err_formatter max_int

(* The module Cilversion is generated automatically by Makefile from
 * information in configure.in *)
let cilVersion         = Cilversion.cilVersion
let cilVersionMajor    = Cilversion.cilVersionMajor
let cilVersionMinor    = Cilversion.cilVersionMinor
let cilVersionRevision = Cilversion.cilVersionRev
(*
module Build_BoolRef(X:sig val default:bool val name:string end) =
  Computation.Ref
    (struct include Datatype.Bool let default = X.default end)
    (struct
       let name = Project.Computation.Name.make X.name
       let dependencies = []
     end)

module Build_False
*)

open Cil_types

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])

let charPtrType = TPtr(charType,[])
let charConstPtrType = TPtr(TInt(IChar, [Attr("const", [])]),[])

let voidPtrType = TPtr(voidType, [])
let intPtrType = TPtr(intType, [])
let uintPtrType = TPtr(uintType, [])

let doubleType = TFloat(FDouble, [])

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
      mutable enum_are_signed: bool;
      mutable stringLiteralType: typ;
      mutable upointType: typ;
      mutable wcharKind: ikind; (** An integer type that fits wchar_t. *)
      mutable wcharType: typ;
      mutable ptrdiffKind: ikind; (** An integer type that fits ptrdiff_t. *)
      mutable ptrdiffType: typ;
      mutable typeOfSizeOf: typ; (** An integer type that is the type of
				      sizeof. *)
      mutable kindOfSizeOf: ikind }

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
    enum_are_signed = true;
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
  dst.enum_are_signed <- src.enum_are_signed;
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
module TheMachine =
  Project.Computation.Register
    (Project.Datatype.Imperative
       (struct
	  type t = theMachine
	  let copy x =
	    let m = createMachine () in
	    copyMachine x m;
	    m
	  let name = "theMachine"
	end))
    (struct
       type t = theMachine
       let create = createMachine
       let get () = !theMachineProject
       let set m =
	 theMachineProject := m;
	 copyMachine !theMachineProject theMachine
       let clear m = copyMachine (createMachine ()) m
     end)
    (struct
       let name = "theMachine"
       let dependencies = []
     end)
let () =
  Project.Selection.iter
    (fun k _ -> Project.Computation.add_dependency k TheMachine.self)
    Logic_env.builtin_states

let selfMachine = TheMachine.self

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

module Build_Counter(Name:sig val name:string end) : sig
  val next: unit -> int
  val reset: unit -> unit
  val get: unit -> int
end = struct
  include Computation.Ref
    (struct include Datatype.Int let default () = 0 end)
    (struct
       let dependencies = []
       let name = Name.name
     end)
  let next () =
    let n = get () in
    if n = -1 then
      bug "Too many values for counter %s. Please report.@." Name.name;
    set (succ n);
    get ()
  let reset = clear
end

module Sid = Build_Counter(struct let name = "sid" end)

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


let compareLoc (a: location) (b: location) : int = Pervasives.compare a b
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
  ref (fun _ -> E.s (E.bug "pd_exp not initialized"))
let pd_global : (formatter -> global -> unit) ref =
  ref (fun _ -> E.s (E.bug "pd_global not initialized"))
(*
let d_annotation =
  ref (fun _ _ -> E.s (E.bug "d_annotation not initialized"))
let d_code_annotation =
  ref (fun _ _ -> E.s (E.bug "d_code_annotation not initialized"))
let d_loop_annotation =
  ref (fun _ _ -> E.s (E.bug "d_loop_annotation not initialized"))
let d_funspec =
  ref (fun _ _ -> E.s (E.bug "d_funspec not initialized"))
*)

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc. *)
type 'a visitAction =
    SkipChildren                        (** Do not visit the children. Return
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this
                                            node. Rebuild the node on return
                                            if any of the children changes
                                            (use == test) *)
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
    {
      (* copy mutable structure which are not shared across the AST*)
      cfile: file -> file;
      cinitinfo: initinfo -> initinfo;
      cfundec: fundec -> fundec;
      cblock: block -> block;
      cfunspec: funspec -> funspec;
      cfunbehavior: funbehavior -> funbehavior;
      (* get the copy of a shared value *)
      get_stmt: stmt -> stmt;
      get_compinfo: compinfo -> compinfo;
      get_fieldinfo: fieldinfo -> fieldinfo;
      get_enuminfo: enuminfo -> enuminfo;
      get_enumitem: enumitem -> enumitem;
      get_typeinfo: typeinfo -> typeinfo;
      get_varinfo: varinfo -> varinfo;
      get_logic_info: logic_info -> logic_info;
(*
      get_predicate_info: predicate_info -> predicate_info;
*)
      get_logic_var: logic_var -> logic_var;
      (* get the original value tied to a copy *)
      get_original_stmt: stmt -> stmt;
      get_original_compinfo: compinfo -> compinfo;
      get_original_fieldinfo: fieldinfo -> fieldinfo;
      get_original_enuminfo: enuminfo -> enuminfo;
      get_original_enumitem: enumitem -> enumitem;
      get_original_typeinfo: typeinfo -> typeinfo;
      get_original_varinfo: varinfo -> varinfo;
      get_original_logic_info: logic_info -> logic_info;
(*
      get_original_predicate_info: predicate_info -> predicate_info;
*)
      get_original_logic_var: logic_var -> logic_var;
      (* change a binding... use with care *)
      set_stmt: stmt -> stmt -> unit;
      set_compinfo: compinfo -> compinfo -> unit;
      set_fieldinfo: fieldinfo -> fieldinfo -> unit;
      set_enuminfo: enuminfo -> enuminfo -> unit;
      set_enumitem: enumitem -> enumitem -> unit;
      set_typeinfo: typeinfo -> typeinfo -> unit;
      set_varinfo: varinfo -> varinfo -> unit;
      set_logic_info: logic_info -> logic_info -> unit;
(*
      set_predicate_info: predicate_info -> predicate_info -> unit;
*)
      set_logic_var: logic_var -> logic_var -> unit;
      (* change a reference... use with care *)
      set_orig_stmt: stmt -> stmt -> unit;
      set_orig_compinfo: compinfo -> compinfo -> unit;
      set_orig_fieldinfo: fieldinfo -> fieldinfo -> unit;
      set_orig_enuminfo: enuminfo -> enuminfo -> unit;
      set_orig_enumitem: enumitem -> enumitem -> unit;
      set_orig_typeinfo: typeinfo -> typeinfo -> unit;
      set_orig_varinfo: varinfo -> varinfo -> unit;
      set_orig_logic_info: logic_info -> logic_info -> unit;
(*
      set_orig_predicate_info: predicate_info -> predicate_info -> unit;
*)
      set_orig_logic_var: logic_var -> logic_var -> unit;
      (* copy fields that can referenced in other places of the AST*)
      memo_stmt: stmt -> stmt;
      memo_varinfo: varinfo -> varinfo;
      memo_compinfo: compinfo -> compinfo;
      memo_enuminfo: enuminfo -> enuminfo;
      memo_enumitem: enumitem -> enumitem;
      memo_typeinfo: typeinfo -> typeinfo;
      memo_logic_info: logic_info -> logic_info;
(*
      memo_predicate_info: predicate_info -> predicate_info;
*)
      memo_fieldinfo: fieldinfo -> fieldinfo;
      memo_logic_var: logic_var -> logic_var;
      (* is the behavior a copy behavior *)
      is_copy_behavior: bool;
      (* reset memoizing tables *)
      reset_behavior_varinfo: unit -> unit;
      reset_behavior_compinfo: unit -> unit;
      reset_behavior_enuminfo: unit -> unit;
      reset_behavior_enumitem: unit -> unit;
      reset_behavior_typeinfo: unit -> unit;
      reset_behavior_logic_info: unit -> unit;
(*
      reset_behavior_predicate_info: unit -> unit;
*)
      reset_behavior_fieldinfo: unit -> unit;
      reset_behavior_stmt: unit -> unit;
      reset_logic_var: unit -> unit;
    }

let is_copy_behavior b = b.is_copy_behavior

let reset_behavior_varinfo b = b.reset_behavior_varinfo ()
let reset_behavior_compinfo b = b.reset_behavior_compinfo ()
let reset_behavior_enuminfo b = b.reset_behavior_enuminfo ()
let reset_behavior_enumitem b = b.reset_behavior_enumitem ()
let reset_behavior_typeinfo b = b.reset_behavior_typeinfo ()
let reset_behavior_logic_info b = b.reset_behavior_logic_info ()
(*
let reset_behavior_predicate_info b = b.reset_behavior_predicate_info ()
*)
let reset_behavior_fieldinfo b = b.reset_behavior_fieldinfo ()
let reset_behavior_stmt b = b.reset_behavior_stmt ()
let reset_logic_var b = b.reset_logic_var ()

let get_varinfo b = b.get_varinfo
let get_compinfo b = b.get_compinfo
let get_fieldinfo b = b.get_fieldinfo
let get_enuminfo b = b.get_enuminfo
let get_enumitem b = b.get_enumitem
let get_stmt b = b.get_stmt
let get_typeinfo b = b.get_typeinfo
let get_logic_info b = b.get_logic_info
(*
let get_predicate_info b = b.get_predicate_info
*)
let get_logic_var b = b.get_logic_var

let get_original_varinfo b = b.get_original_varinfo
let get_original_compinfo b = b.get_original_compinfo
let get_original_fieldinfo b = b.get_original_fieldinfo
let get_original_enuminfo b = b.get_original_enuminfo
let get_original_enumitem b = b.get_original_enumitem
let get_original_stmt b = b.get_original_stmt
let get_original_typeinfo b = b.get_original_typeinfo
let get_original_logic_info b = b.get_original_logic_info
(*
let get_original_predicate_info b = b.get_original_predicate_info
*)
let get_original_logic_var b = b.get_original_logic_var

let set_varinfo b = b.set_varinfo
let set_compinfo b = b.set_compinfo
let set_fieldinfo b = b.set_fieldinfo
let set_enuminfo b = b.set_enuminfo
let set_enumitem b = b.set_enumitem
let set_stmt b = b.set_stmt
let set_typeinfo b = b.set_typeinfo
let set_logic_info b = b.set_logic_info
(*
let set_predicate_info b = b.set_predicate_info
*)
let set_logic_var b = b.set_logic_var

let set_orig_varinfo b = b.set_orig_varinfo
let set_orig_compinfo b = b.set_orig_compinfo
let set_orig_fieldinfo b = b.set_orig_fieldinfo
let set_orig_enuminfo b = b.set_orig_enuminfo
let set_orig_enumitem b = b.set_orig_enumitem
let set_orig_stmt b = b.set_orig_stmt
let set_orig_typeinfo b = b.set_orig_typeinfo
let set_orig_logic_info b = b.set_orig_logic_info
(*
let set_orig_predicate_info b = b.set_orig_predicate_info
*)
let set_orig_logic_var b = b.set_orig_logic_var

let inplace_visit () =
  { cfile = (fun x -> x);
    get_compinfo = (fun x -> x);
    get_fieldinfo = (fun x -> x);
    get_enuminfo = (fun x -> x);
    get_enumitem = (fun x -> x);
    get_typeinfo = (fun x -> x);
    get_varinfo = (fun x -> x);
    get_logic_var = (fun x -> x);
    get_stmt = (fun x -> x);
    get_logic_info = (fun x -> x);
(*
    get_predicate_info = (fun x -> x);
*)
    get_original_compinfo = (fun x -> x);
    get_original_fieldinfo = (fun x -> x);
    get_original_enuminfo = (fun x -> x);
    get_original_enumitem = (fun x -> x);
    get_original_typeinfo = (fun x -> x);
    get_original_varinfo = (fun x -> x);
    get_original_logic_var = (fun x -> x);
    get_original_stmt = (fun x -> x);
    get_original_logic_info = (fun x -> x);
(*
    get_original_predicate_info = (fun x -> x);
*)
    cinitinfo = (fun x -> x);
    cfundec = (fun x -> x);
    cblock = (fun x -> x);
    cfunspec = (fun x -> x);
    cfunbehavior = (fun x -> x);
    is_copy_behavior = false;
    memo_varinfo = (fun x -> x);
    memo_compinfo = (fun x -> x);
    memo_enuminfo = (fun x -> x);
    memo_enumitem = (fun x -> x);
    memo_typeinfo = (fun x -> x);
    memo_logic_info = (fun x -> x);
(*
    memo_predicate_info = (fun x -> x);
*)
    memo_stmt = (fun x -> x);
    memo_fieldinfo = (fun x -> x);
    memo_logic_var = (fun x -> x);
    set_varinfo = (fun _ _ -> ());
    set_compinfo = (fun _ _ -> ());
    set_enuminfo = (fun _ _ -> ());
    set_enumitem = (fun _ _ -> ());
    set_typeinfo = (fun _ _ -> ());
    set_logic_info = (fun _ _ -> ());
(*
    set_predicate_info = (fun _ _ -> ());
*)
    set_stmt = (fun _ _ -> ());
    set_fieldinfo = (fun _ _ -> ());
    set_logic_var = (fun _ _ -> ());
    set_orig_varinfo = (fun _ _ -> ());
    set_orig_compinfo = (fun _ _ -> ());
    set_orig_enuminfo = (fun _ _ -> ());
    set_orig_enumitem = (fun _ _ -> ());
    set_orig_typeinfo = (fun _ _ -> ());
    set_orig_logic_info = (fun _ _ -> ());
(*
    set_orig_predicate_info = (fun _ _ -> ());
*)
    set_orig_stmt = (fun _ _ -> ());
    set_orig_fieldinfo = (fun _ _ -> ());
    set_orig_logic_var = (fun _ _ -> ());
    reset_behavior_varinfo = (fun () -> ());
    reset_behavior_compinfo = (fun () -> ());
    reset_behavior_enuminfo = (fun () -> ());
    reset_behavior_enumitem = (fun () -> ());
    reset_behavior_typeinfo = (fun () -> ());
    reset_behavior_logic_info = (fun () -> ());
(*
    reset_behavior_predicate_info = (fun () -> ());
*)
    reset_behavior_fieldinfo = (fun () -> ());
    reset_behavior_stmt = (fun () -> ());
    reset_logic_var = (fun () -> ());
  }

let copy_visit () =
  let varinfos = Inthash.create 103 in
  let compinfos = Inthash.create 17 in
  let enuminfos = Hashtbl.create 17 in
  let enumitems = Hashtbl.create 17 in
  let typeinfos = Hashtbl.create 17 in
  let logic_infos = Hashtbl.create 17 in
(*
  let predicate_infos = Hashtbl.create 17 in
*)
  let fieldinfos = Hashtbl.create 17 in
  let stmts = Inthash.create 103 in
  let logic_vars = Inthash.create 17 in
  let orig_varinfos = Inthash.create 103 in
  let orig_compinfos = Inthash.create 17 in
  let orig_enuminfos = Hashtbl.create 17 in
  let orig_enumitems = Hashtbl.create 17 in
  let orig_typeinfos = Hashtbl.create 17 in
  let orig_logic_infos = Hashtbl.create 17 in
(*
  let orig_predicate_infos = Hashtbl.create 17 in
*)
  let orig_fieldinfos = Hashtbl.create 17 in
  let orig_stmts = Inthash.create 103 in
  let orig_logic_vars = Inthash.create 17 in
  { cfile = (fun x -> { x with fileName = x.fileName });
    get_compinfo =
      (fun x -> try Inthash.find compinfos x.ckey with Not_found -> x);
    get_fieldinfo =
      (fun x -> try Hashtbl.find fieldinfos (x.fname,x.fcomp.ckey)
       with Not_found -> x);
    get_enuminfo =
      (fun x -> try Hashtbl.find enuminfos x.ename with Not_found -> x);
    get_enumitem =
      (fun x -> try Hashtbl.find enumitems x.einame with Not_found -> x);
    get_typeinfo =
      (fun x -> try Hashtbl.find typeinfos x.tname with Not_found -> x);
    get_varinfo =
      (fun x -> try Inthash.find varinfos x.vid with Not_found -> x);
    get_stmt = (fun x -> try Inthash.find stmts x.sid with Not_found -> x);
    get_logic_info =
      (fun x -> try Hashtbl.find logic_infos x.l_name with Not_found -> x);
(*
    get_predicate_info =
      (fun x -> try Hashtbl.find predicate_infos x.p_name with Not_found -> x);
*)
    get_logic_var = (fun x -> try Inthash.find logic_vars x.lv_id
                     with Not_found -> x);
    get_original_compinfo =
      (fun x -> try Inthash.find orig_compinfos x.ckey with Not_found -> x);
    get_original_fieldinfo =
      (fun x -> try Hashtbl.find orig_fieldinfos (x.fname,x.fcomp.ckey)
       with Not_found -> x);
    get_original_enuminfo =
      (fun x -> try Hashtbl.find orig_enuminfos x.ename with Not_found -> x);
    get_original_enumitem =
      (fun x -> try Hashtbl.find orig_enumitems x.einame with Not_found -> x);
    get_original_typeinfo =
      (fun x -> try Hashtbl.find orig_typeinfos x.tname with Not_found -> x);
    get_original_varinfo =
      (fun x -> try Inthash.find orig_varinfos x.vid with Not_found -> x);
    get_original_stmt =
      (fun x -> try Inthash.find orig_stmts x.sid with Not_found -> x);
    get_original_logic_var =
      (fun x -> try Inthash.find orig_logic_vars x.lv_id with Not_found -> x);
    get_original_logic_info =
      (fun x -> try Hashtbl.find orig_logic_infos x.l_name with Not_found -> x);
(*
    get_original_predicate_info =
      (fun x ->
         try Hashtbl.find orig_predicate_infos x.p_name with Not_found -> x);
*)
    cinitinfo = (fun x -> { init = x.init });
    cfundec = ( fun x -> { x with svar = x.svar });
    cblock = (fun x -> { x with battrs = x.battrs });
    cfunspec = (fun x -> { x with spec_requires = x.spec_requires});
    cfunbehavior = (fun x -> { x with b_name = x.b_name});
    is_copy_behavior = true;
    reset_behavior_varinfo =
      (fun () ->  Inthash.clear varinfos; Inthash.clear orig_varinfos);
    reset_behavior_compinfo =
      (fun () -> Inthash.clear compinfos; Inthash.clear orig_compinfos);
    reset_behavior_enuminfo =
      (fun () -> Hashtbl.clear enuminfos; Hashtbl.clear orig_enuminfos);
    reset_behavior_enumitem =
      (fun () -> Hashtbl.clear enumitems; Hashtbl.clear orig_enumitems);
    reset_behavior_typeinfo =
      (fun () -> Hashtbl.clear typeinfos; Hashtbl.clear orig_typeinfos);
    reset_behavior_logic_info =
      (fun () -> Hashtbl.clear logic_infos; Hashtbl.clear orig_logic_infos);
(*
    reset_behavior_predicate_info =
      (fun () ->
         Hashtbl.clear predicate_infos; Hashtbl.clear orig_predicate_infos);
*)
    reset_behavior_fieldinfo =
      (fun () ->Hashtbl.clear fieldinfos; Hashtbl.clear orig_fieldinfos);
    reset_behavior_stmt =
      (fun () -> Inthash.clear stmts; Inthash.clear orig_stmts);
    reset_logic_var =
      (fun () -> Inthash.clear logic_vars; Inthash.clear orig_logic_vars);
    memo_varinfo =
      (fun x ->
         try Inthash.find varinfos x.vid
         with Not_found ->
           let new_x = { x with vid = x.vid } in
           Inthash.add varinfos x.vid new_x;
           Inthash.add orig_varinfos new_x.vid x;
           new_x);
    memo_compinfo =
      (fun x ->
         try Inthash.find compinfos x.ckey
         with Not_found ->
           let new_x = { x with ckey = x.ckey } in
           Inthash.add compinfos x.ckey new_x;
           Inthash.add orig_compinfos new_x.ckey x;
           new_x);
    memo_enuminfo =
      (fun x ->
         try Hashtbl.find enuminfos x.ename
         with Not_found ->
           let new_x = { x with ename = x.ename } in
           Hashtbl.add enuminfos x.ename new_x;
           Hashtbl.add orig_enuminfos new_x.ename x;
           new_x);
    memo_enumitem =
      (fun x ->
         try Hashtbl.find enumitems x.einame
         with Not_found ->
           let new_x = { x with einame = x.einame } in
           Hashtbl.add enumitems x.einame new_x;
           Hashtbl.add orig_enumitems new_x.einame x;
           new_x);
    memo_typeinfo =
      (fun x ->
         try Hashtbl.find typeinfos x.tname
         with Not_found ->
           let new_x = { x with tname = x.tname } in
           Hashtbl.add typeinfos x.tname new_x;
           Hashtbl.add orig_typeinfos new_x.tname x;
           new_x);
    memo_logic_info =
      (fun x ->
         try Hashtbl.find logic_infos x.l_name
         with Not_found ->
           let new_x = { x with l_name = x.l_name } in
           Hashtbl.add logic_infos x.l_name new_x; new_x);
(*
    memo_predicate_info =
      (fun x ->
         try Hashtbl.find predicate_infos x.p_name
         with Not_found ->
           let new_x = { x with p_name = x.p_name } in
           Hashtbl.add predicate_infos x.p_name new_x;
           Hashtbl.add orig_predicate_infos new_x.p_name x;
           new_x);
*)
    memo_stmt =
      (fun x ->
         try Inthash.find stmts x.sid
         with Not_found ->
           let new_x = { x with sid = x.sid } in
           Inthash.add stmts x.sid new_x;
           Inthash.add orig_stmts new_x.sid x;
           new_x);
    memo_fieldinfo =
      (fun x ->
         try Hashtbl.find fieldinfos (x.fname,x.fcomp.ckey)
         with Not_found ->
           let new_x = { x with fname = x.fname } in
           Hashtbl.add fieldinfos (x.fname, x.fcomp.ckey) new_x;
           Hashtbl.add orig_fieldinfos (new_x.fname, new_x.fcomp.ckey) x;
           new_x);
    memo_logic_var =
      (fun x ->
         try Inthash.find logic_vars x.lv_id
         with Not_found ->
           let new_x = { x with lv_id = x.lv_id } in
           Inthash.add logic_vars x.lv_id new_x;
           Inthash.add orig_logic_vars new_x.lv_id x;
           new_x);
    set_varinfo = (fun x y -> Inthash.replace varinfos x.vid y);
    set_compinfo = (fun x y -> Inthash.replace compinfos x.ckey y);
    set_enuminfo = (fun x y -> Hashtbl.replace enuminfos x.ename y);
    set_enumitem = (fun x y -> Hashtbl.replace enumitems x.einame y);
    set_typeinfo = (fun x y -> Hashtbl.replace typeinfos x.tname y);
    set_logic_info = (fun x y -> Hashtbl.replace logic_infos x.l_name y);
(*
    set_predicate_info = (fun x y ->Hashtbl.replace predicate_infos x.p_name y);
*)
    set_stmt = (fun x y -> Inthash.replace stmts x.sid y);
    set_fieldinfo =
      (fun x y -> Hashtbl.replace fieldinfos (x.fname,x.fcomp.ckey) y);
    set_logic_var = (fun x y -> Inthash.replace logic_vars x.lv_id y);
    set_orig_varinfo = (fun x y -> Inthash.replace orig_varinfos x.vid y);
    set_orig_compinfo = (fun x y -> Inthash.replace orig_compinfos x.ckey y);
    set_orig_enuminfo = (fun x y -> Hashtbl.replace orig_enuminfos x.ename y);
    set_orig_enumitem = (fun x y -> Hashtbl.replace orig_enumitems x.einame y);
    set_orig_typeinfo = (fun x y -> Hashtbl.replace orig_typeinfos x.tname y);
    set_orig_logic_info =
      (fun x y -> Hashtbl.replace orig_logic_infos x.l_name y);
(*
    set_orig_predicate_info =
      (fun x y -> Hashtbl.replace orig_predicate_infos x.p_name y);
*)
    set_orig_stmt = (fun x y -> Inthash.replace orig_stmts x.sid y);
    set_orig_fieldinfo =
      (fun x y -> Hashtbl.replace orig_fieldinfos (x.fname,x.fcomp.ckey) y);
    set_orig_logic_var = (fun x y -> Inthash.replace orig_logic_vars x.lv_id y);
  }

(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use
 * one of Change... actions if you want to copy the node *)

(** A visitor interface for traversing CIL trees. Create instantiations of
 * this type by specializing the class {!Cil.nopCilVisitor}. *)
class type cilVisitor = object

  method behavior: visitor_behavior

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

  method current_func: fundec option
  method set_current_func: fundec -> unit
  method reset_current_func: unit -> unit

  (*VP: annotation visitor. *)

  method vlogic_type: logic_type -> logic_type visitAction

  method vtsets_elem: tsets_elem -> tsets_elem visitAction

  method vtsets_lval: tsets_lval -> tsets_lval visitAction

  method vtsets_lhost: tsets_lhost -> tsets_lhost visitAction

  method vtsets_offset: tsets_offset -> tsets_offset visitAction

  method vtsets: tsets -> tsets visitAction

  method vterm: term -> term visitAction

  method vterm_node: term_node -> term_node visitAction

  method vterm_lval: term_lval -> term_lval visitAction

  method vterm_lhost: term_lhost -> term_lhost visitAction

  method vterm_offset: term_offset -> term_offset visitAction

  method vlogic_info_decl: logic_info -> logic_info visitAction

  method vlogic_info_use: logic_info -> logic_info visitAction

  method vlogic_var_use: logic_var -> logic_var visitAction

  method vlogic_var_decl: logic_var -> logic_var visitAction

  method vquantifiers: quantifiers -> quantifiers visitAction

  method vpredicate: predicate -> predicate visitAction

  method vpredicate_named: predicate named -> predicate named visitAction

(*
  method vpredicate_info_decl: predicate_info -> predicate_info visitAction

  method vpredicate_info_use: predicate_info -> predicate_info visitAction
*)

  method vbehavior: funbehavior -> funbehavior visitAction

  method vspec: funspec -> funspec visitAction

  method vassigns:
    identified_tsets assigns -> identified_tsets assigns visitAction

  method vloop_pragma: term loop_pragma -> term loop_pragma visitAction

  method vslice_pragma: term slice_pragma -> term slice_pragma visitAction
  method vimpact_pragma: term impact_pragma -> term impact_pragma visitAction

  method vzone:
    identified_tsets zone -> identified_tsets zone visitAction

  method vcode_annot: code_annotation -> code_annotation visitAction

  method vannotation: global_annotation -> global_annotation visitAction
  method fill_global_tables: unit
  method get_filling_actions: (unit -> unit) Queue.t
  method set_logic_tables: unit -> unit
end

(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class genericCilVisitor ?prj behavior: cilVisitor =
object(self)
  method behavior = behavior

  (* list of things to perform on the new project. Done at the end
     of the analysis in order to minimize the number of project changes.
  *)
  val global_tables_action = Queue.create ()

  method fill_global_tables =
    let prj = match prj with None -> Project.current () | Some prj -> prj in
    Project.on prj
      (fun () -> Queue.iter (fun f -> f()) global_tables_action) ();
    Queue.clear global_tables_action

  method get_filling_actions = global_tables_action

  method set_logic_tables () =
    if is_copy_behavior self#behavior then begin
      Queue.add Logic_env.LogicInfo.clear global_tables_action;
(*
      Queue.add Logic_env.PredicateInfo.clear global_tables_action;
*)
      Logic_env.LogicInfo.iter
        (fun _ x ->
           let x' = self#behavior.memo_logic_info x in
           Queue.add (fun () -> Logic_env.add_logic_function x')
             global_tables_action);
(*
      Logic_env.PredicateInfo.iter
        (fun _ x ->
           let x' = self#behavior.memo_predicate_info x in
           Queue.add (fun () -> Logic_env.add_predicate x')
             global_tables_action);
*)
    end

  method vfile _f = DoChildren
  val current_stmt = Stack.create ()
  method private push_stmt s = Stack.push s current_stmt
  method private pop_stmt _s = ignore (Stack.pop current_stmt)
  method current_stmt =
    try Some (Stack.top current_stmt) with Stack.Empty -> None

  val mutable current_func = None
  method current_func = current_func
  method set_current_func f = current_func <- Some f
  method reset_current_func () = current_func <- None

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

  method vlogic_type _lt = DoChildren

  method vtsets_lhost _ = DoChildren

  method vtsets_elem _ = DoChildren

  method vtsets_lval _ = DoChildren

  method vtsets_offset _ = DoChildren

  method vtsets _l = DoChildren

  method vterm _t = DoChildren

  method vterm_node _tn = DoChildren

  method vterm_lval _tl = DoChildren

  method vterm_lhost _tl = DoChildren

  method vterm_offset _vo = DoChildren

  method vlogic_info_decl _li = DoChildren

  method vlogic_info_use _li = DoChildren

  method vlogic_var_decl _lv = DoChildren

  method vlogic_var_use _lv = DoChildren

  method vquantifiers _q = DoChildren

  method vpredicate _p = DoChildren

  method vpredicate_named _p = DoChildren

(*
  method vpredicate_info_decl _pi = DoChildren

  method vpredicate_info_use _pi = DoChildren
*)

  method vbehavior _b = DoChildren

  method vspec _s = DoChildren

  method vassigns _s = DoChildren

  method vloop_pragma _ = DoChildren

  method vslice_pragma _ = DoChildren
  method vimpact_pragma _ = DoChildren

  method vzone _ = DoChildren

  method vcode_annot _ca = DoChildren

  method vannotation _a = DoChildren

end

class nopCilVisitor = object
  inherit genericCilVisitor (inplace_visit ())
end

let assertEmptyQueue vis =
  if vis#unqueueInstr () <> [] then
    (* Either a visitor inserted an instruction somewhere that it shouldn't
       have (i.e. at the top level rather than inside of a statement), or
       there's a bug in the visitor engine. *)
    E.s (E.bug "Visitor's instruction queue is not empty.\n  You should only use queueInstr inside a function body!");
  ()

(* sm: utility *)
let startsWith prefix s =
  let prefixLen = String.length prefix in
  String.length s >= prefixLen && String.sub s 0 prefixLen = prefix

module VarInfos =
  Computation.Make_Hashtbl
    (Inthash)
    (Cil_datatype.Varinfo)
    (struct
       let name = "VarInfos"
       let dependencies = []
       let size = 17
     end)

let varinfos_self = VarInfos.self
let varinfo_from_vid = VarInfos.find

let set_vid, copy_with_new_vid, copy_with_new_lvid, new_raw_id =
  (* [new_vid] should never be used by foreign functions *)
  let new_vid =
    let module M = Build_Counter(struct let name = "vid" end) in
    M.next
  in
  (fun v ->
     let n = new_vid () in
     v.vid <- n;
     ignore (VarInfos.memo ~change:(fun _ -> assert false) (fun _ -> v) n)),
  (fun v ->
     let n = new_vid () in
     let v' = { v with vid = n } in
     VarInfos.memo ~change:(fun _ -> assert false) (fun _ -> v') n),
  (fun lv -> { lv with lv_id = new_vid () }),
  new_vid

(* The next compindo identifier to use. Counts up. *)
let nextCompinfoKey =
  let module M = Build_Counter(struct let name = "compinfokey" end) in
  M.next

let bytesSizeOfInt (ik: ikind): int =
  match ik with
  | IChar | ISChar | IUChar -> 1
  | IBool | IInt | IUInt -> theMachine.theMachine.sizeof_int
  | IShort | IUShort -> theMachine.theMachine.sizeof_short
  | ILong | IULong -> theMachine.theMachine.sizeof_long
  | ILongLong | IULongLong -> theMachine.theMachine.sizeof_longlong

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

(* Represents an integer as for a given kind.
   Returns a flag saying whether the value was changed
   during truncation (because it was too large to fit in k). *)
let truncateInteger64 (k: ikind) (i: int64) : int64 * bool =
   let nrBits = 8 * (bytesSizeOfInt k) in
  let signed = isSigned k in
    if nrBits = 64 then
    i, false
  else begin
    let i1 = Int64.shift_left i (64 - nrBits) in
    let i2 =
      if signed then Int64.shift_right i1 (64 - nrBits)
      else Int64.shift_right_logical i1 (64 - nrBits)
    in
    let truncated =
      if i2 = i then false
      else
        (* Examine the bits that we chopped off.  If they are all zero, then
         * any difference between i2 and i is due to a simple sign-extension.
         *   e.g. casting the constant 0x80000000 to int makes it
         *        0xffffffff80000000.
         * Suppress the truncation warning in this case.      *)
        let chopped = Int64.shift_right_logical i (64 - nrBits)
        in chopped <> Int64.zero
    in
    i2, truncated
  end

(* Construct an integer constant with possible truncation *)
let kinteger64 (k: ikind) (i: int64) : exp =
  let i', truncated = truncateInteger64 k i in
  if truncated then
    ignore (warnOpt "Truncating integer %s to %s\n"
              (Int64.format "0x%x" i) (Int64.format "0x%x" i'));
(*  Format.printf "kint64:%Lx %Ld@\n" i' i';*)
  Const (CInt64(i' , k,  None))

(* Construct an integer of a given kind. *)
let kinteger (k: ikind) (i: int) = kinteger64 k (Int64.of_int i)

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer (i: int) = Const (CInt64(Int64.of_int i, IInt, None))

let zero      = integer 0
let one       = integer 1
let mone      = integer (-1)

let lconstant ?(loc=locUnknown) v =
  { term_node = TConst (CInt64(v, IInt, None)); term_loc = loc;
    term_name = []; term_type = Ctype (TInt (IInt,[]));}

let lzero ?(loc=locUnknown) () = lconstant ~loc Int64.zero
let lone  ?(loc=locUnknown) () = lconstant ~loc Int64.one
let lmone ?(loc=locUnknown) () = lconstant ~loc (Int64.minus_one)

(** Given the character c in a (CChr c), sign-extend it to 32 bits.
  (This is the official way of interpreting character constants, according to
  ISO C 6.4.4.4.10, which says that character constants are chars cast to ints)
  Returns CInt64(sign-extened c, IInt, None) *)
let charConstToInt (c: char) : constant =
  let c' = Char.code c in
  let value =
    if c' < 128
    then Int64.of_int c'
    else Int64.of_int (c' - 256)
  in
  CInt64(value, IInt, None)


let rec isInteger : exp -> int64 option = function
  | Const(CInt64 (n,_,_)) -> Some n
  | Const(CChr c) -> isInteger (Const (charConstToInt c))  (* sign-extend *)
  | Const(CEnum {eival = v}) -> isInteger v
  | CastE(_, e) -> isInteger e
  | _ -> None

(** Convert a 64-bit int to an OCaml int, or raise an exception if that
    can't be done. *)
let i64_to_int (i: int64) : int =
  let i': int = Int64.to_int i in (* i.e. i' = i mod 2^31 *)
  if i = Int64.of_int i' then i'
  else E.s (E.error "Int constant too large: %Ld\n" i)


let rec isZero (e: exp) : bool = isInteger e = Some Int64.zero

let rec isLogicZero t = match t.term_node with
  | TConst (CInt64 (n,_,_)) -> n = 0L
  | TConst (CChr c) -> Char.code c = 0
  | TCastE(_, t) -> isLogicZero t
  | _ -> false

let isLogicNull t =
  isLogicZero t ||
    (let rec aux t = match t.term_node with
       | Tnull -> true
       | TCastE(_, t) -> aux t
       | _ -> false
     in aux t)

let parseInt (str: string) : exp =
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
  * on Int64 and we take care of overflow. We work only with
  * positive integers since the lexer takes care of the sign *)
  let rec toInt (base: int64) (acc: int64) (idx: int) : int64 =
    (*Format.printf "toInt base=%Ld acc=%Ld idx=%d@." base acc idx;*)
    let doAcc (what: int) =
      let acc' =
        Int64.add (Int64.mul base acc)  (Int64.of_int what) in
      if acc < Int64.zero || (* We clearly overflow since base >= 2
      * *)
      (acc' > Int64.zero && acc' < acc) then
        E.s (unimp "Cannot represent on 64 bits the integer %s\n"
               str)
      else
        toInt base acc' (idx + 1)
    in
    if idx >= l - suffixlen then begin
      acc
    end else
      let ch = String.get str idx in
      if ch >= '0' && ch <= '9' then
        doAcc (Char.code ch - Char.code '0')
      else if  ch >= 'a' && ch <= 'f'  then
        doAcc (10 + Char.code ch - Char.code 'a')
      else if  ch >= 'A' && ch <= 'F'  then
        doAcc (10 + Char.code ch - Char.code 'A')
      else
        E.s (bug "Invalid integer constant: %s" str)
  in
  try
    let i =
      if octalhex then
        if l >= 2 &&
          (let c = String.get str 1 in c = 'x' || c = 'X') then
          toInt (Int64.of_int 16) Int64.zero 2
        else
          toInt (Int64.of_int 8) Int64.zero 1
      else
        toInt (Int64.of_int 10) Int64.zero 0
    in
(*    Format.printf "Got i =%Ld@." i;*)
    (* Construct an integer of the first kinds that fits. i must be
    * POSITIVE  *)
(*    assert (Int64.zero <= i);*)
    let res =
            let rec loop = function
          k::rest ->
            let nrBits =
              let unsignedbits = 8 * (bytesSizeOfInt k) in
              if isSigned k then
                unsignedbits-1
              else
                unsignedbits
            in
            (* Will i fit in nrBits bits? *)
            let bound : int64 = Int64.shift_left 1L nrBits in
            (* toInt has ensured that 0 <= i < 263.
               So if nrBits >=63, i fits *)
            if (nrBits >= 63) || (i < bound) then
              kinteger64 k i
            else
              loop rest
        | [] -> E.s (E.unimp "Cannot represent the integer %s\n"
                       (Int64.to_string i))
      in
(*
let rec loop = function
        | ((IInt | ILong) as k) :: _
                  when i < Int64.shift_left (Int64.of_int 1) 31 ->
                    kinteger64 k i
        | ((IUInt | IULong) as k) :: _
                  when i < Int64.shift_left (Int64.of_int 1) 32
          ->  kinteger64 k i
        | (ILongLong as k) :: _
                 when i  <= Int64.sub (Int64.shift_left
                                              (Int64.of_int 1) 63)
                                          (Int64.of_int 1)
          ->
            kinteger64 k i
        | (IULongLong as k) :: _ -> kinteger64 k i
        | _ :: rest -> loop rest
        | [] -> E.s (E.unimp "Cannot represent the integer %s\n"
                       (Int64.to_string i))
      in
*)
      loop kinds
    in
    res
  with Failure _ as e -> begin
    ignore (log "int_of_string %s (%s)\n" str
              (Printexc.to_string e));
    zero
  end

let mkStmt ?(valid_sid=false) (sk: stmtkind) : stmt =
  { skind = sk;
    labels = [];
    (* It is better to create statements with a valid sid, so that they can
       be safely be used in tables. I only do it when performing Jessie
       analysis, as other plugins rely on specific sid values for their tests
       (e.g. slicing). *)
    sid = if valid_sid then Sid.next () else -1;
    succs = []; preds = [];
    ghost = false}

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


let mkBlock (slst: stmt list) : block =
  let slst = List.filter
    (fun st -> match st.skind with
     | Instr (Skip _) when st.labels = [] -> false
     | _ ->true)
    slst
  in
  { battrs = []; bstmts = slst; }

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

let stmt_of_instr_list ?(loc=locUnknown) = function
  | [] -> Instr (Skip loc)
  | [i] -> Instr i
  | il ->
      let b = mkBlock (List.map (fun i -> mkStmt (Instr i)) il) in
      match b.bstmts with
      | [] -> Instr (Skip loc)
      | [s] when b.battrs = [] -> s.skind
      | _ -> Block b

let mkEmptyStmt ?(loc=locUnknown) () = mkStmt (Instr (Skip loc))
let mkStmtOneInstr (i: instr) = mkStmt (Instr i)

let dummyInstr = Asm([], ["dummy statement!!"], [], [], [], locUnknown)
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

(**** ATTRIBUTES ****)


(* JS: build an attribute annotation from [s]. *)
let mkAttrAnnot s = "/*@ " ^ s ^ " */"

(* JS: *)
let attributeName = function Attr(a, _) | AttrAnnot a -> a

(* Internal attributes. Won't be pretty-printed *)
let reserved_attributes = ["FRAMA_C_KEEP_BLOCK"]

(** Construct sorted lists of attributes ***)
let rec addAttribute
    (Attr(an, _) | AttrAnnot an as a: attribute) (al: attributes) =
  let rec insertSorted = function
      [] -> [a]
    | ((Attr(an0, _) | AttrAnnot an0 as a0) :: rest) as l ->
        if an < an0 then a :: l
        else if equals a a0 then l (* Do not add if already in there *)
        else a0 :: insertSorted rest (* Make sure we see all attributes with
                                      * this name *)
  in
  insertSorted al

(** The second attribute list is sorted *)
and addAttributes al0 (al: attributes) : attributes =
    if al0 == [] then al else
    List.fold_left (fun acc a -> addAttribute a acc) al al0

and dropAttribute (an: string) (al: attributes) =
  List.filter (fun a -> attributeName a <> an) al

and dropAttributes (anl: string list) (al: attributes) =
  List.fold_left (fun acc an -> dropAttribute an acc) al anl

and filterAttributes (s: string) (al: attribute list) : attribute list =
  List.filter (fun a -> attributeName a = s) al

and findAttribute (s: string) (al: attribute list) : attrparam list =
  List.fold_left
    (fun acc -> function
     | Attr (an, param) when an = s -> param @ acc
     | _ -> acc)
    [] al

(* sm: *)
let hasAttribute s al =
  (filterAttributes s al <> [])

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
let attributeHash: (string, attributeClass) H.t =
  let table = H.create 13 in
  List.iter (fun a -> H.add table a (AttrName false))
    [ "section"; "constructor"; "destructor"; "unused"; "used"; "weak";
      "no_instrument_function"; "alias"; "no_check_memory_usage";
      "exception"; "model"; (* "restrict"; *)
      "aconst"; "__asm__" (* Gcc uses this to specifiy the name to be used in
                           * assembly for a global  *)];
  (* Now come the MSVC declspec attributes *)
  List.iter (fun a -> H.add table a (AttrName true))
    [ "thread"; "naked"; "dllimport"; "dllexport";
      "selectany"; "allocate"; "nothrow"; "novtable"; "property";  "noreturn";
      "uuid"; "align" ];
  List.iter (fun a -> H.add table a (AttrFunType false))
    [ "format"; "regparm"; "longcall"; "noinline"; "always_inline" ];
  List.iter (fun a -> H.add table a (AttrFunType true))
    [ "stdcall";"cdecl"; "fastcall" ];
  List.iter (fun a -> H.add table a AttrType)
    [ "const"; "volatile"; "restrict"; "mode" ];
  table

let attributeClass = H.find attributeHash

let registerAttribute = H.add attributeHash
let removeAttribute = H.remove attributeHash

(** Partition the attributes into classes *)
let partitionAttributes
    ~(default:attributeClass)
    (attrs:  attribute list) :
    attribute list * attribute list * attribute list =
  let rec loop (n,f,t) = function
      [] -> n, f, t
    | (Attr(an, _) | AttrAnnot an as a) :: rest ->
        match (try H.find attributeHash an with Not_found -> default) with
          AttrName _ -> loop (addAttribute a n, f, t) rest
        | AttrFunType _ ->
            loop (n, addAttribute a f, t) rest
        | AttrType -> loop (n, f, addAttribute a t) rest
  in
  loop ([], [], []) attrs


(** Get the full name of a comp *)
let compFullName comp =
  (if comp.cstruct then "struct " else "union ") ^ comp.cname


let missingFieldName = "_" (* "___missing_field_name"*)

(** Creates a (potentially recursive) composite type. Make sure you add a
  * GTag for it to the file! **)
let mkCompInfo
      (isstruct: bool)
      (n: string)
      (* fspec is a function that when given a forward
       * representation of the structure type constructs the type of
       * the fields. The function can ignore this argument if not
       * constructing a recursive type.  *)
       (mkfspec: compinfo -> (string * typ * int option * attribute list *
                             location) list)
       (a: attribute list) : compinfo =

  (* make a new name for anonymous structs *)
   if n = "" then
     E.s (E.bug "mkCompInfo: missing structure name\n");
   (* Make a new self cell and a forward reference *)
   let comp =
     { cstruct = isstruct; cname = ""; ckey = 0; cfields = [];
       cattr = a; creferenced = false;
       (* Make this compinfo undefined by default *)
       cdefined = false; }
   in
   comp.cname <- n;
   comp.ckey <- nextCompinfoKey ();
   let flds =
       List.map (fun (fn, ft, fb, fa, fl) ->
          { fcomp = comp;
            ftype = ft;
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

(**** Utility functions ******)

let rec typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TNamed (t, a) -> addAttributes a (typeAttrs t.ttype)
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TComp (comp, a) -> addAttributes comp.cattr a
  | TEnum (enum, a) -> addAttributes enum.eattr a
  | TFun (_, _, _, a) -> a
  | TBuiltin_va_list a -> a


let typeAttr = function
  | TVoid a
  | TInt (_, a)
  | TFloat (_, a)
  | TNamed (_, a)
  | TPtr (_, a)
  | TArray (_, _, a)
  | TComp (_, a)
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
  | TArray (t', l, _) -> TArray(t', l, a)
  | TComp (comp, _) -> TComp (comp, a)
  | TEnum (enum, _) -> TEnum (enum, a)
  | TFun (r, args, v, _) -> TFun(r,args,v,a)
  | TBuiltin_va_list _ -> TBuiltin_va_list a


let typeAddAttributes a0 t =
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
      | TArray (t, l, a) -> TArray (t, l, add a)
      | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
      | TComp (comp, a) -> TComp (comp, add a)
      | TNamed (t, a) -> TNamed (t, add a)
      | TBuiltin_va_list a -> TBuiltin_va_list (add a)
end

let typeRemoveAttributes (anl: string list) t =
  let drop (al: attributes) = dropAttributes anl al in
  match t with
    TVoid a -> TVoid (drop a)
  | TInt (ik, a) -> TInt (ik, drop a)
  | TFloat (fk, a) -> TFloat (fk, drop a)
  | TEnum (enum, a) -> TEnum (enum, drop a)
  | TPtr (t, a) -> TPtr (t, drop a)
  | TArray (t, l, a) -> TArray (t, l, drop a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, drop a)
  | TComp (comp, a) -> TComp (comp, drop a)
  | TNamed (t, a) -> TNamed (t, drop a)
  | TBuiltin_va_list a -> TBuiltin_va_list (drop a)

let unrollType (t: typ) : typ =
  let rec withAttrs (al: attributes) (t: typ) : typ =
    match t with
      TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
    | x -> typeAddAttributes al x
  in
  withAttrs [] t

let rec unrollTypeDeep (t: typ) : typ =
  let rec withAttrs (al: attributes) (t: typ) : typ =
    match t with
      TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
    | TPtr(t, a') -> TPtr(unrollTypeDeep t, addAttributes al a')
    | TArray(t, l, a') -> TArray(unrollTypeDeep t, l, addAttributes al a')
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
  match unrollType t with
    TVoid _ -> true
  | _ -> false
let isVoidPtrType t =
  match unrollType t with
    TPtr(tau,_) when isVoidType tau -> true
  | _ -> false

let isSignedInteger ty =
  match unrollType ty with
    | TInt(ik,_attr) -> isSigned ik
    | TEnum _ -> theMachine.theMachine.Cil_types.enum_are_signed
    | _ -> false

let var vi : lval = (Var vi, NoOffset)
(* let assign vi e = Instrs(Set (var vi, e), lu) *)

let mkString s = Const(CStr s)

let mkWhile ~(guard:exp) ~(body: stmt list) : stmt list =
  (* Do it like this so that the pretty printer recognizes it *)
  [ mkStmt
      (Loop ([],
	     mkBlock
	       (mkStmt
		  (If(guard,
                      mkBlock [ mkEmptyStmt () ],
                      mkBlock [ mkStmt (Break locUnknown)], locUnknown)) ::
                  body), locUnknown, None, None)) ]



let mkFor ~(start: stmt list) ~(guard: exp) ~(next: stmt list)
          ~(body: stmt list) : stmt list =
  (start @
     (mkWhile guard (body @ next)))


let mkForIncr ~(iter : varinfo) ~(first: exp) ~stopat:(past: exp) ~(incr: exp)
    ~(body: stmt list) : stmt list =
      (* See what kind of operator we need *)
  let compop, nextop =
    match unrollType iter.vtype with
      TPtr _ -> Lt, PlusPI
    | _ -> Lt, PlusA
  in
  mkFor
    [ mkStmt (Instr (Set (var iter, first, locUnknown))) ]
    (BinOp(compop, Lval(var iter), past, intType))
    [ mkStmt (Instr (Set (var iter,
                           (BinOp(nextop, Lval(var iter), incr, iter.vtype)),
                           locUnknown)))]
    body

let block_from_unspecified_sequence us =
  { battrs = []; bstmts = List.map (fun (x,_,_) ->x) us }

let rec stripCasts (e: exp) =
  match e with CastE(_, e') -> stripCasts e' | _ -> e

let rec stripInfo (e: exp) =
  match e with Info(e',_) -> stripInfo e' | _ -> e

let rec stripCastsAndInfo (e: exp) =
  match e with Info(e',_) | CastE(_,e') -> stripCastsAndInfo e' | _ -> e

let rec stripCastsButLastInfo (e: exp) =
  match e with
      Info((Info _ | CastE _ as e'),_) | CastE(_,e') -> stripCastsButLastInfo e'
    | _ -> e

let rec stripTermCasts (t: term) =
  match t.term_node with TCastE(_, t') -> stripTermCasts t' | _ -> t

let rec stripTsetsCasts (ts: tsets_elem) =
  match ts with TSCastE(_ty,ts') -> stripTsetsCasts ts' | _ -> ts

let exp_info_of_term t =
  {
    exp_loc = t.term_loc;
    exp_type = t.term_type;
    exp_name = t.term_name;
  }

let term_of_exp_info tnode einfo =
  {
    term_node = tnode;
    term_loc = einfo.exp_loc;
    term_type = einfo.exp_type;
    term_name = einfo.exp_name;
  }

let map_under_info f = function
  | Info(e,einfo) -> Info(f e,einfo)
  | e -> f e

let app_under_info f = function
  | Info(e,_) | e -> f e

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

(* sm: need this value below *)
let mostNeg32BitInt : int64 = (Int64.of_string "-0x80000000")
let mostNeg64BitInt : int64 = (Int64.of_string "-0x8000000000000000")

(* constant *)
let d_const fmt c =
  match c with
    CInt64(_, _, Some s) -> fprintf fmt "%s" s (* Always print the text if there is one *)
  | CInt64(i, ik, None) ->
      (*fprintf fmt "/* %Lx */" i;*)
      (** We must make sure to capture the type of the constant. For some
          * constants this is done with a suffix, for others with a cast prefix.*)
      let suffix : string =
        match ik with
          IUInt -> "U"
        | ILong -> "L"
        | IULong -> "UL"
        | ILongLong -> if theMachine.msvcMode then "L" else "LL"
        | IULongLong -> if theMachine.msvcMode then "UL" else "ULL"
        | _ -> ""
      in
      let prefix : string =
        if suffix <> "" then ""
        else if ik = IInt then ""
        else fprintf_to_string "(%a)" d_ikind ik
      in
      (* Watch out here for negative integers that we should be printing as
       * large positive ones *)
      fprintf fmt "%s"
        (if i < Int64.zero
           && (match ik with
                 IUInt | IULong | IULongLong | IUChar | IUShort -> true | _ -> false) then
             let high = Int64.shift_right i 32 in
             if ik <> IULongLong && ik <> ILongLong && high = Int64.of_int (-1) then
               (* Print only the low order 32 bits *)
               (prefix ^ "0x" ^
                  (Int64.format "%x"
                     (Int64.logand i (Int64.shift_right_logical high 32))
                   ^ suffix))
             else
               (prefix ^ "0x" ^ Int64.format "%x" i ^ suffix)
         else (
           if (i = mostNeg32BitInt) then
             (* sm: quirk here: if you print -2147483648 then this is two tokens *)
             (* in C, and the second one is too large to represent in a signed *)
             (* int.. so we do what's done in limits.h, and print (-2147483467-1); *)
             (* in gcc this avoids a warning, but it might avoid a real problem *)
             (* on another compiler or a 64-bit architecture *)
             (prefix ^ "(-0x7FFFFFFF-1)")
           else if (i = mostNeg64BitInt) then
             (* The same is true of the largest 64-bit negative. *)
             (prefix ^ "(-0x7FFFFFFFFFFFFFFF-1)")
           else
             (prefix ^ (Int64.to_string i ^ suffix))
         ))

  | CStr(s) -> fprintf fmt "\"%s\"" (escape_string s)
  | CWStr(s) ->
      (* text ("L\"" ^ escape_string s ^ "\"")  *)
      fprintf fmt "L";
      List.iter
        (fun elt ->
           if (elt >= Int64.zero &&
                 elt <= (Int64.of_int 255)) then
             fprintf fmt "%S" (escape_char (Char.chr (Int64.to_int elt)))
           else
             fprintf fmt "\"\\x%LX\"" elt;
           fprintf fmt "@ ")
        s;
        (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" --
         * the former has 7 wide characters and the later has 3. *)

  | CChr(c) -> fprintf fmt "'%s'" (escape_char c)
  | CReal(_, _, Some s) -> fprintf fmt "%s" s
  | CReal(f, fsize, None) ->
      fprintf fmt "%s%s" (string_of_float f)
        (match fsize with
           FFloat -> "f"
         | FDouble -> ""
         | FLongDouble -> "L")
  | CEnum {einame = s} -> fprintf fmt "%s" s


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
let questionLevel = 100

let logic_level = 77

let getParenthLevel e = match stripInfo e with
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
  | Tlambda _ -> 90
  (* TODO: remove spurious parentheses with prec mechanism
                         for tsets *)
  | Ttsets _ -> 90
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
  | TStartOf(_) -> addrOfLevel
  | TUnOp((Neg|BNot|LNot),_) -> 30
                                        (* Unary post *)
  | TCoerce _ | TCoerceE _ -> 25

                                        (* Lvals *)
  | TLval(TMem _ , _) -> derefStarLevel
  | TLval(TVar _, (TField _|TIndex _)) -> indexLevel
  | TLval(TResult,(TField _|TIndex _)) -> indexLevel
  | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ -> 20
  | TAlignOf _ | TAlignOfE _ -> 20
      (* VP: I'm not sure I understand why sizeof(x) and f(x) should
         have a separated treatment wrt parentheses. *)
      (* application and applications-like constructions *)
  | Tapp (_, _,_)|TDataCons _
  | Tblock_length _ | Tbase_addr _ | Tat (_, _) | Told _
  | TUpdate _ | Ttypeof _ | Ttype _ -> 10
  | TLval(TVar _, TNoOffset) -> 0        (* Plain variables *)
      (* Constructions that do not require parentheses *)
  | TConst _
  | Tnull | TLval (TResult,TNoOffset) -> 0
  | Tif (_, _, _)  -> logic_level


let getParenthLevelTsetsElem = function
    TSLval(TSMem _,_) -> derefStarLevel
  | TSLval((TSVar _ | TSResult) , (TSField _ | TSIndex _ | TSRange _)) ->
      indexLevel
  | TSAdd_range _ | TSAdd_index _ -> additiveLevel
  | TSCastE _ -> 30
  | TSStartOf _ | TSAddrOf _ -> addrOfLevel
  | TSConst _ | TSLval _ | TSat _ | TSapp _ -> 0

let getParenthLevelAttrParam (a: attrparam) =
  (* Create an expression of the same shape, and use {!getParenthLevel} *)
  match a with
    AInt _ | AStr _ | ACons _ -> 0
  | ASizeOf _ | ASizeOfE _ | ASizeOfS _ -> 20
  | AAlignOf _ | AAlignOfE _ | AAlignOfS _ -> 20
  | AUnOp (uo, _) -> getParenthLevel (UnOp(uo, zero, intType))
  | ABinOp (bo, _, _) -> getParenthLevel (BinOp(bo, zero, zero, intType))
  | AAddrOf _ -> 30
  | ADot _ | AIndex _ | AStar _ -> 20
  | AQuestion _ -> questionLevel


(* Separate out the storage-modifier name attributes *)
let separateStorageModifiers (al: attribute list) =
  let isstoragemod (Attr(an, _) | AttrAnnot an : attribute) : bool =
    try
      match H.find attributeHash an with
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
  match unrollType t with
    | TInt((IChar|ISChar|IUChar),_) -> true
    | _ -> false

let isCharPtrType t =
  match unrollType t with
    TPtr(tau,_) when isCharType tau -> true
  | _ -> false

let isIntegralType t =
  match unrollType t with
    (TInt _ | TEnum _) -> true
  | _ -> false

let isLogicIntegralType t =
  match t with
    | Ctype t -> isIntegralType t
    | Linteger -> true
    | Lreal -> false
    | Lvar _ | Ltype _ | Larrow _ -> false

let isFloatingType t =
  match unrollType t with
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

let isArithmeticType t =
  match unrollType t with
    (TInt _ | TEnum _ | TFloat _) -> true
  | _ -> false

let isLogicArithmeticType t =
  match t with
    | Ctype t -> isArithmeticType t
    | Linteger | Lreal -> true
    | Lvar _ | Ltype _ | Larrow _ -> false

let isPointerType t =
  match unrollType t with
    TPtr _ -> true
  | _ -> false

let isTypeTagType t =
  match t with
      Ltype("typetag",[]) -> true
    | _ -> false

let isFunctionType t =
  match unrollType t with
    TFun _ -> true
  | _ -> false

let getReturnType t =
  match unrollType t with
    | TFun(rt,_,_,_) -> rt
    | _ -> E.s (E.bug "getReturnType: not a function type")

let setReturnTypeVI (v: varinfo) (t: typ) =
  match unrollType v.vtype with
    | TFun (_, args, va, a) ->
	v.vtype <- TFun (t, args, va, a)
    | _ -> E.s (E.bug "setReturnType: not a function type")

let setReturnType (f:fundec) (t:typ) =
  setReturnTypeVI f.svar t

(** Returns the type pointed by the given type. Asserts it is a pointer type *)
let typeOf_pointed typ =
  match unrollType typ with
  | TPtr (typ,_) -> typ
  | _ -> assert false

(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ =
  match stripInfo e with
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

  | Const(CEnum {eihost = ei}) -> TEnum(ei, [])

  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> theMachine.typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> theMachine.typeOfSizeOf
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, _) -> TPtr(t, [])
     | _ -> E.s (E.bug "typeOf: StartOf on a non-array")
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
      | _ -> E.s (bug "typeOfLval: Mem on a non-pointer (%a)" !pd_exp addr)
  end

and typeOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) =
      partitionAttributes ~default:(AttrName false) baseAttrs in
    typeAddAttributes contageous
  in
  function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, baseAttrs) ->
	  let elementType = typeOffset t o in
	  blendAttributes baseAttrs elementType
      | _ -> E.s (E.bug "typeOffset: Index on a non-array")
  end
  | Field (fi, o) ->
      match unrollType basetyp with
        TComp (_, baseAttrs) ->
	  let fieldType = typeOffset fi.ftype o in
	  let typ = blendAttributes baseAttrs fieldType in
          (match fi.fbitfield with
           | Some s ->
               typeAddAttributes [Attr ("FRAMA_C_BITFIELD_SIZE", [AInt s])] typ
           | None -> typ)
      | _ -> E.s (bug "typeOffset: Field on a non-compound")

(**** Compute the type of a term lval ****)
let rec typeOfTermLval = function
    TVar vi, off ->
      let ty = match vi.lv_origin with
	| Some v -> Ctype v.vtype
	| None -> vi.lv_type
      in
      typeTermOffset ty off
  | TResult,_ ->
      begin match !currentGlobal with
	| GFun(f,_loc) -> Ctype (getReturnType f.svar.vtype)
	| _ -> E.s (bug "typeOfTermLval: Can not compute type of result")
      end
  | TMem addr, off -> begin
      match addr.term_type with
	| Ctype typ ->
	    begin match unrollType typ with
		TPtr (t, _) -> typeTermOffset (Ctype t) off
	      | _ -> E.s (bug "typeOfTermLval: Mem on a non-pointer")
	    end
        | Linteger | Lreal ->
            E.s (bug "typeOfTermLval: Mem on a logic type")
	| Ltype (s,_) -> E.s (bug "typeOfTermLval: Mem on a non-C type (%s)" s)
        | Lvar s -> E.s (bug "typeOfTermLval: Mem on a non-C type ('%s)" s)
        | Larrow _ -> E.s (bug "typeOfTermLval: Mem on a function type")
  end

and typeTermOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) =
      partitionAttributes ~default:(AttrName false) baseAttrs in
    function
      | Ctype typ ->
	  Ctype (typeAddAttributes contageous typ)
      | Linteger | Lreal ->
          E.s (bug "typeTermOffset: Attribute on a logic type")
      | Ltype (s,_) ->
	  E.s (bug "typeTermOffset: Attribute on a non-C type (%s)" s)
      | Lvar s ->
	  E.s (bug "typeTermOffset: Attribute on a non-C type ('%s)" s)
      | Larrow _ ->
          E.s (bug "typeTermOffset: Attribute on a function type")
  in
  function
    TNoOffset -> basetyp
  | TIndex (_, o) -> begin
      match basetyp with
	| Ctype typ ->
	    begin match unrollType typ with
		TArray (t, _, baseAttrs) ->
		  let elementType = typeTermOffset (Ctype t) o in
		  blendAttributes baseAttrs elementType
	      | _ -> E.s (E.bug "typeTermOffset: Index on a non-array")
	    end
	 | Linteger | Lreal ->
             E.s (bug "typeTermOffset: Index on a logic type")
         | Ltype (s,_) ->
            E.s (bug "typeTermOffset: Index on a non-C type (%s)" s)
         | Lvar s ->
            E.s (bug "typeTermOffset: Index on a non-C type ('%s)" s)
         | Larrow _ ->
             E.s (bug "typeTermOffset: Index on a function type")
  end
  | TField (fi, o) ->
      match basetyp with
	| Ctype typ ->
	    begin match unrollType typ with
		TComp (_, baseAttrs) ->
		  let fieldType = typeTermOffset (Ctype fi.ftype) o in
		  blendAttributes baseAttrs fieldType
	      | _ -> E.s (bug "typeTermOffset: Field on a non-compound")
	    end
        | Linteger | Lreal ->
            E.s (bug "typeTermOffset: Field on a logic type")
	| Ltype (s,_) ->
            E.s (bug "typeTermOffset: Field on a non-C type (%s)" s)
	| Lvar s -> E.s (bug "typeTermOffset: Field on a non-C type ('%s)" s)
        | Larrow _ -> E.s (bug "typeTermOffset: Field on a function type")

let rec typeOfTsetsLval = function
    TSVar vi, off ->
      let ty = match vi.lv_origin with
	| Some v -> Ctype v.vtype
	| None -> vi.lv_type
      in
      typeTsetsOffset ty off
  | TSResult,_ ->
      begin match !currentGlobal with
	| GFun(f,_loc) -> Ctype (getReturnType f.svar.vtype)
	| _ -> E.s (bug "typeOfTsetsLval: Can not compute type of result")
      end
  | TSMem addr, off -> begin
      match typeOfTsetsElem addr with
	| Ctype typ ->
	    begin match unrollType typ with
		TPtr (t, _) -> typeTsetsOffset (Ctype t) off
	      | _ -> E.s (bug "typeOfTsetsLval: Mem on a non-pointer")
	    end
        | Linteger | Lreal ->
            E.s (bug "typeOfTsetsLval: Mem on a logic type")
	| Ltype (s,_) -> E.s (bug "typeOfTsetsLval: Mem on a non-C type (%s)" s)
        | Lvar s -> E.s (bug "typeOfTsetsLval: Mem on a non-C type ('%s)" s)
        | Larrow _ -> E.s (bug "typeOfTsetsLval: Mem on a function type")
  end

and typeTsetsOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) =
      partitionAttributes ~default:(AttrName false) baseAttrs in
    function
      | Ctype typ ->
	  Ctype (typeAddAttributes contageous typ)
      | Linteger | Lreal ->
          E.s (bug "typeTsetsOffset: Attribute on a logic type")
      | Ltype (s,_) ->
	  E.s (bug "typeTsetsOffset: Attribute on a non-C type (%s)" s)
      | Lvar s ->
	  E.s (bug "typeTsetsOffset: Attribute on a non-C type ('%s)" s)
      | Larrow _ ->
          E.s (bug "typeTsetsOffset: Attribute on a function type")
  in
  function
    TSNoOffset -> basetyp
  | TSIndex (_, o) | TSRange(_,_,o) -> begin
      match basetyp with
	| Ctype typ ->
	    begin match unrollType typ with
		TArray (t, _, baseAttrs) ->
		  let elementType = typeTsetsOffset (Ctype t) o in
		  blendAttributes baseAttrs elementType
	      | _ -> E.s (E.bug "typeTsetsOffset: Index on a non-array")
	    end
	 | Linteger | Lreal ->
             E.s (bug "typeTsetsOffset: Index on a logic type")
         | Ltype (s,_) ->
            E.s (bug "typeTsetsOffset: Index on a non-C type (%s)" s)
         | Lvar s ->
            E.s (bug "typeTsetsOffset: Index on a non-C type ('%s)" s)
         | Larrow _ ->
             E.s (bug "typeTsetsOffset: Index on a function type")
  end
  | TSField (fi, o) ->
      match basetyp with
	| Ctype typ ->
	    begin match unrollType typ with
		TComp (_, baseAttrs) ->
		  let fieldType = typeTsetsOffset (Ctype fi.ftype) o in
		  blendAttributes baseAttrs fieldType
	      | _ -> E.s (bug "typeTsetsOffset: Field on a non-compound")
	    end
        | Linteger | Lreal ->
            E.s (bug "typeTsetsOffset: Field on a logic type")
	| Ltype (s,_) ->
            E.s (bug "typeTsetsOffset: Field on a non-C type (%s)" s)
	| Lvar s -> E.s (bug "typeTsetsOffset: Field on a non-C type ('%s)" s)
        | Larrow _ -> E.s (bug "typeTsetsOffset: Field on a function type")

and typeOfTsetsElem = function
    TSLval lv -> typeOfTsetsLval lv
  | TSStartOf lv ->
      (match typeOfTsetsLval lv with
           Ctype t ->
             (match unrollType t with
                | TArray(t,_,attrs) ->
                    Ctype (TPtr(t,attrs))
                | _ -> E.s (bug "typeOfTsetsElem: TStartOf on non array type"))
         | _ -> E.s (bug "typeOfTsetsElem: TStartOf on non array type"))
  | TSAddrOf lv ->
      (match typeOfTsetsLval lv with
           Ctype t -> Ctype(TPtr(t,[]))
         | _ -> E.s (bug "typeOfTsetsElem: TAddrOf on logical type"))
  | TSConst (CInt64 _ ) -> Linteger
  | TSConst _ -> assert false (* cannot appear in tsets *)
  | TSAdd_index (t,_) | TSAdd_range(t,_,_) -> typeOfTsetsElem t
  | TSCastE(typ,_) -> Ctype typ
  | TSat(elem,_) -> typeOfTsetsElem elem
  | TSapp(f,_,_) ->
      begin
	(* until now, we only have complete applications*)
	match f.l_type with
	  | Some t -> t
	  | None -> assert false (* cannot appear in tsets *)
      end

(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)
exception SizeOfError of string * typ


(* Get the minimum aligment in bytes for a given type *)
let rec alignOf_int = function
  | TInt((IChar|ISChar|IUChar|IBool), _) -> 1
  | TInt((IShort|IUShort), _) -> theMachine.theMachine.alignof_short
  | TInt((IInt|IUInt), _) -> theMachine.theMachine.alignof_int
  | TInt((ILong|IULong), _) -> theMachine.theMachine.alignof_long
  | TInt((ILongLong|IULongLong), _) ->
      theMachine.theMachine.alignof_longlong
  | TEnum _ -> theMachine.theMachine.alignof_enum
  | TFloat(FFloat, _) -> theMachine.theMachine.alignof_float
  | TFloat(FDouble, _) -> theMachine.theMachine.alignof_double
  | TFloat(FLongDouble, _) ->
      theMachine.theMachine.alignof_longdouble
  | TNamed (t, _) -> alignOf_int t.ttype
  | TArray (t, _, _) -> (* Be careful for char[] of Diab-C like compilers. *)
      begin
        match unrollType t with
        | TInt((IChar|ISChar|IUChar),_) ->
            theMachine.theMachine.alignof_char_array
        | _ -> alignOf_int t
      end

  | TPtr _ | TBuiltin_va_list _ ->
      theMachine.theMachine.alignof_ptr

        (* For composite types get the maximum alignment of any field inside *)
  | TComp (c, _) ->
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
          max sofar (alignOf_int f.ftype)) 1 fields
        (* These are some error cases *)
  | TFun _ when not theMachine.msvcMode ->
      theMachine.theMachine.alignof_fun
  | TFun _ as t -> raise (SizeOfError ("function", t))
  | TVoid _ as t -> raise (SizeOfError ("void", t))


let bitsSizeOfInt (ik: ikind): int =
  match ik with
  | IBool | IChar | ISChar | IUChar -> 8
  | IInt | IUInt -> 8 * theMachine.theMachine.sizeof_int
  | IShort | IUShort -> 8 * theMachine.theMachine.sizeof_short
  | ILong | IULong -> 8 * theMachine.theMachine.sizeof_long
  | ILongLong | IULongLong ->
      8 * theMachine.theMachine.sizeof_longlong

let unsignedVersionOf (ik:ikind): ikind =
  match ik with
  | ISChar | IChar -> IUChar
  | IShort -> IUShort
  | IInt -> IUInt
  | ILong -> IULong
  | ILongLong -> IULongLong
  | _ -> ik

(* Represents an integer as for a given kind.
   Returns a flag saying whether the value was changed
   during truncation (because it was too large to fit in k). *)
let truncateInteger64 (k: ikind) (i: int64) : int64 * bool =
  let nrBits = bitsSizeOfInt k in
  let signed = isSigned k in
  if nrBits = 64 then
    i, false
  else begin
    let i1 = Int64.shift_left i (64 - nrBits) in
    let i2 =
      if signed then Int64.shift_right i1 (64 - nrBits)
      else Int64.shift_right_logical i1 (64 - nrBits)
    in
    let truncated =
      if i2 = i then false
      else
        (* Examine the bits that we chopped off.  If they are all zero, then
         * any difference between i2 and i is due to a simple sign-extension.
         *   e.g. casting the constant 0x80000000 to int makes it
         *        0xffffffff80000000.
         * Suppress the truncation warning in this case.      *)
        let chopped = Int64.shift_right_logical i (64 - nrBits)
        in chopped <> Int64.zero
    in
    i2, truncated
  end

(* Construct an integer constant with possible truncation *)
let kinteger64 (k: ikind) (i: int64) : exp =
  let i', truncated = truncateInteger64 k i in
  if truncated && miscState.warnTruncate then
    ignore (warnOpt "Truncating integer %s to %s\n"
              (Int64.format "0x%x" i) (Int64.format "0x%x" i'));
  Const (CInt64(i', k,  None))

(* Construct an integer of a given kind. *)
let kinteger (k: ikind) (i: int) = kinteger64 k (Int64.of_int i)

(* Convert 2 integer constants to integers with the same type, in preparation
   for a binary operation.   See ISO C 6.3.1.8p1 *)
let convertInts (i1:int64) (ik1:ikind) (i2:int64) (ik2:ikind)
  : int64 * int64 * ikind =
  if ik1 = ik2 then (* nothing to do *)
    i1, i2, ik1
  else begin
    let rank : ikind -> int = function
        (* these are just unique numbers representing the integer
           conversion rank. *)
      | IBool | IChar | ISChar | IUChar -> 1
      | IShort | IUShort -> 2
      | IInt | IUInt -> 3
      | ILong | IULong -> 4
      | ILongLong | IULongLong -> 5
    in
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


(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
let rec offsetOfFieldAcc_GCC (fi: fieldinfo)
                             (sofar: offsetAcc) : offsetAcc =
  (* field type *)
  let ftype = unrollType fi.ftype in
  let ftypeAlign = 8 * alignOf_int ftype in
  let ftypeBits = bitsSizeOf ftype in
(*
  if fi.fcomp.cname = "comp2468" ||
     fi.fcomp.cname = "comp2469" ||
     fi.fcomp.cname = "comp2470" ||
     fi.fcomp.cname = "comp2471" ||
     fi.fcomp.cname = "comp2472" ||
     fi.fcomp.cname = "comp2473" ||
     fi.fcomp.cname = "comp2474" ||
     fi.fcomp.cname = "comp2475" ||
     fi.fcomp.cname = "comp2476" ||
     fi.fcomp.cname = "comp2477" ||
     fi.fcomp.cname = "comp2478" then

    ignore (E.log "offsetOfFieldAcc_GCC(%s of %s:%a%a,firstFree=%d,pack=%a)\n"
              fi.fname fi.fcomp.cname
              d_type ftype
              insert
              (match fi.fbitfield with
                None -> nil
              | Some wdthis -> dprintf ":%d" wdthis)
              sofar.oaFirstFree
              insert
              (match sofar.oaPrevBitPack with
                None -> text "None"
              | Some (packstart, _, wdpack) ->
                  dprintf "Some(packstart=%d,wd=%d)"
                    packstart wdpack));
*)
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
  let ftypeAlign = 8 * alignOf_int ftype in
  let ftypeBits = bitsSizeOf ftype in
(*
  ignore (E.log "offsetOfFieldAcc_MSVC(%s of %s:%a%a,firstFree=%d, pack=%a)\n"
            fi.fname fi.fcomp.cname
            d_type ftype
            insert
            (match fi.fbitfield with
              None -> nil
            | Some wdthis -> dprintf ":%d" wdthis)
            sofar.oaFirstFree
            insert
            (match sofar.oaPrevBitPack with
              None -> text "None"
            | Some (prevpack, _, wdpack) -> dprintf "Some(prev=%d,wd=%d)"
                  prevpack wdpack));
*)
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

  | _, Some _, None -> E.s (E.bug "offsetAcc")


and offsetOfFieldAcc ~(fi: fieldinfo)
                     ~(sofar: offsetAcc) : offsetAcc =
  if theMachine.msvcMode then offsetOfFieldAcc_MSVC fi sofar
  else offsetOfFieldAcc_GCC fi sofar

(* The size of a type, in bits. If struct or array then trailing padding is
 * added *)
and bitsSizeOf t =
  if not (TheMachine.is_computed ()) then
    E.s (E.error "You did not call Cil.initCIL before using the CIL library");
  match t with
  | TInt (ik,_) -> 8 * (bytesSizeOfInt ik)
  | TFloat(FDouble, _) -> 8 * theMachine.theMachine.sizeof_double
  | TFloat(FLongDouble, _) ->
      8 * theMachine.theMachine.sizeof_longdouble
  | TFloat _ -> 8 * theMachine.theMachine.sizeof_float
  | TEnum _ -> 8 * theMachine.theMachine.sizeof_enum
  | TPtr _ -> 8 * theMachine.theMachine.sizeof_ptr
  | TBuiltin_va_list _ -> 8 * theMachine.theMachine.sizeof_ptr
  | TNamed (t, _) -> bitsSizeOf t.ttype
  | TComp (comp, _) when comp.cfields == [] -> begin
      (* Empty structs are allowed in msvc mode *)
      if not comp.cdefined && not theMachine.msvcMode then
        raise
	  (SizeOfError
	     (Format.sprintf
		"abstract type: empty struct exist only with MSVC (comp %s)"
		(compFullName comp),
	      t)) (*abstract type*)
      else
        0
  end

  | TComp (comp, _) when comp.cstruct -> (* Struct *)
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
        addTrailing lastoff.oaFirstFree (8 * alignOf_int t)

  | TComp (comp, _) -> (* when not comp.cstruct *)
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
      addTrailing max (8 * alignOf_int t)

   | TArray(bt, Some len, _) -> begin
      match constFold true len with
        Const(CInt64(l,_,_)) ->
          let sz = Int64.mul (Int64.of_int (bitsSizeOf bt)) l in
          let sz' = Int64.to_int sz in
          (* Check for overflow.
             There are other places in these cil.ml that overflow can occur,
             but this multiplication is the most likely to be a problem. *)
          if (Int64.of_int sz') <> sz then
            raise (SizeOfError ("Array is so long that its size can't be "
                                  ^"represented with an OCaml int.", t))
          else
            begin
              sz' (*WAS: addTrailing sz' (8 * alignOf_int t)*)
            end
      | _ -> raise (SizeOfError ("array non-constant length", t))
     end

  | TVoid _ -> 8 * theMachine.theMachine.sizeof_void
  | TFun _ when not theMachine.msvcMode ->
      (* On GCC the size of a function is defined *)
      8 * theMachine.theMachine.sizeof_fun

  | TArray (_, None, _) -> (* it seems that on GCC the size of such an
                            * array is 0 *)
      0

  | TFun _ -> raise (SizeOfError ("function", t))


and addTrailing nrbits roundto =
    (nrbits + roundto - 1) land (lnot (roundto - 1))

and sizeOf_int t = (bitsSizeOf t) lsr 3

and sizeOf t =
  try
    integer ((bitsSizeOf t) lsr 3)
  with SizeOfError _ -> SizeOf(t)


and bitsOffset (baset: typ) (off: offset) : int * int =
  let rec loopOff (baset: typ) (width: int) (start: int) = function
      NoOffset -> start, width
    | Index(e, off) -> begin
        let ei =
          match isInteger e with
            Some i64 -> Int64.to_int i64
          | None -> raise (SizeOfError ("index not constant", baset))
        in
        let bt =
          match unrollType baset with
            TArray(bt, _, _) -> bt
          | _ -> E.s (E.bug "bitsOffset: Index on a non-array")
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
              [] -> E.s (E.bug "bitsOffset: Cannot find field %s in %s\n"
                           f.fname f.fcomp.cname)
            | fi' :: _ when fi' == f -> [fi']
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




(** Do constant folding on an expression. If the first argument is true then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node.*)
and constFold (machdep: bool) (e: exp) : exp =
  match e with
    BinOp(bop, e1, e2, tres) -> constFoldBinOp machdep bop e1 e2 tres
  | UnOp(unop, e1, tres) -> begin
      try
        let tk =
          match unrollType tres with
            TInt(ik, _) -> ik
          | TEnum _ -> IInt
          | _ -> raise Not_found (* probably a float *)
        in
        match constFold machdep e1 with
          Const(CInt64(i,_ik,_)) -> begin
            match unop with
              Neg -> kinteger64 tk (Int64.neg i)
            | BNot -> kinteger64 tk (Int64.lognot i)
            | LNot -> if i = Int64.zero then one else zero
            end
        | e1c -> UnOp(unop, e1c, tres)
      with Not_found -> e
  end
        (* Characters are integers *)
  | Const(CChr c) -> Const(charConstToInt c)
  | Const(CEnum {eival = v}) -> constFold machdep v
  | SizeOf t when machdep -> begin
      try
        let bs = bitsSizeOf t in
        kinteger theMachine.kindOfSizeOf (bs / 8)
      with SizeOfError _ -> e
  end
  | SizeOfE e when machdep -> constFold machdep (SizeOf (typeOf e))
  | SizeOfStr s when machdep ->
      kinteger theMachine.kindOfSizeOf (1 + String.length s)
  | AlignOf t when machdep -> kinteger theMachine.kindOfSizeOf (alignOf_int t)
  | AlignOfE e when machdep -> begin
      (* The alignment of an expression is not always the alignment of its
       * type. I know that for strings this is not true *)
      match e with
        Const (CStr _) when not theMachine.msvcMode ->
          kinteger theMachine.kindOfSizeOf theMachine.theMachine.alignof_str
            (* For an array, it is the alignment of the array ! *)
      | _ -> constFold machdep (AlignOf (typeOf e))
  end

  | CastE(it,
          AddrOf (Mem (CastE(TPtr(bt, _), z)), off))
    when machdep && isZero z -> begin
      try
        let start, _width = bitsOffset bt off in
        if start mod 8 <> 0 then
          E.s (error "Using offset of bitfield\n");
        constFold machdep (CastE(it, (integer (start / 8))))
      with SizeOfError _ -> e
  end


  | CastE (t, e) -> begin
      match constFold machdep e, unrollType t with
        (* Might truncate silently *)
        Const(CInt64(i,_k,_)), TInt(nk,a)
          (* It's okay to drop a cast to const.
             If the cast has any other attributes, leave the cast alone. *)
          when (dropAttributes ["const"] a) = [] ->
          let i', _ = truncateInteger64 nk i in
          Const(CInt64(i', nk, None))
      | e', _ -> CastE (t, e')
  end
  | Lval lv -> Lval (constFoldLval machdep lv)
  | AddrOf lv -> AddrOf (constFoldLval machdep lv)
  | StartOf lv -> StartOf (constFoldLval machdep lv)
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

and constFoldBinOp (machdep: bool) bop e1 e2 tres =
  let e1' = constFold machdep e1 in
  let e2' = constFold machdep e2 in
  if isIntegralType tres then begin
    let newe =
      let rec mkInt = function
          Const(CChr c) -> Const(charConstToInt c)
        | Const(CEnum {eival = v}) -> mkInt v
        | CastE(TInt (ik, ta), e) -> begin
            match mkInt e with
              Const(CInt64(i, _, _)) ->
                let i', _ = truncateInteger64 ik i in
                Const(CInt64(i', ik, None))

            | e' -> CastE(TInt(ik, ta), e')
        end
        | e -> e
      in
      let tk =
        match unrollType tres with
          TInt(ik, _) -> ik
        | TEnum _ -> IInt
        | _ -> E.s (bug "constFoldBinOp")
      in
      (* See if the result is unsigned *)
      let isunsigned typ = not (isSigned typ) in
      let ge (unsigned: bool) (i1: int64) (i2: int64) : bool =
        if unsigned then
          let l1 = Int64.shift_right_logical i1 1 in
          let l2 = Int64.shift_right_logical i2 1 in (* Both positive now *)
          (l1 > l2) || (l1 = l2 &&
                        Int64.logand i1 Int64.one >= Int64.logand i2 Int64.one)
        else i1 >= i2
      in
      let shiftInBounds i2 =
        (* We only try to fold shifts if the second arg is positive and
           less than the size of the type of the first argument.
           Otherwise, the semantics are processor-dependent, so let the compiler sort it out. *)
            (* We only try to fold shifts if the second arg is positive and
            less than the size of the type of the first argument.
            Otherwise, the semantics are processor-dependent, so let the
            compiler sort it out. *)
        if machdep then
          try
            i2 >= Int64.zero && i2 < (Int64.of_int (bitsSizeOf (typeOf e1')))
          with SizeOfError _ -> false
        else false
      in
      (* Assume that the necessary promotions have been done *)
      match bop, mkInt e1', mkInt e2' with
      | PlusA, Const(CInt64(z,_,_)), e2'' when z = Int64.zero -> e2''
      | PlusA, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | PlusPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | IndexPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | MinusPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | PlusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.add i1 i2)
      | MinusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.sub i1 i2)
      | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.mul i1 i2)
      | Mult, Const(CInt64(0L,_,_)), _ -> zero
      | Mult, Const(CInt64(1L,_,_)), e2'' -> e2''
      | Mult, _,    Const(CInt64(0L,_,_)) -> zero
      | Mult, e1'', Const(CInt64(1L,_,_)) -> e1''
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> begin
          try kinteger64 tk (Int64.div i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
      end
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_))
          when bytesSizeOfInt ik1 = bytesSizeOfInt ik2 -> begin
          try kinteger64 tk (Int64.div i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
        end
      | Div, e1'', Const(CInt64(1L,_,_)) -> e1''

      | Mod, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> begin
          try kinteger64 tk (Int64.rem i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
      end
      | BAnd, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.logand i1 i2)
      | BAnd, Const(CInt64(0L,_,_)), _ -> zero
      | BAnd, _, Const(CInt64(0L,_,_)) -> zero
      | BOr, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.logor i1 i2)
      | BOr, _, _ when isZero e1' -> e2'
      | BOr, _, _ when isZero e2' -> e1'
      | BXor, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          kinteger64 tk (Int64.logxor i1 i2)

      | Shiftlt, Const(CInt64(i1,_ik1,_)),Const(CInt64(i2,_,_)) when shiftInBounds i2 ->
          kinteger64 tk (Int64.shift_left i1 (Int64.to_int i2))
      | Shiftlt, Const(CInt64(0L,_,_)), _ -> zero
      | Shiftlt, e1'', Const(CInt64(0L,_,_)) -> e1''

      | Shiftrt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,_,_)) when shiftInBounds i2 ->
          if isunsigned ik1 then
            kinteger64 tk (Int64.shift_right_logical i1 (Int64.to_int i2))
          else
            kinteger64 tk (Int64.shift_right i1 (Int64.to_int i2))
      | Shiftrt, Const(CInt64(0L,_,_)), _ -> zero
      | Shiftrt, e1'', Const(CInt64(0L,_,_)) -> e1''

      | Eq, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
          if i1' = i2' then one else zero
      | Ne, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', _ = convertInts i1 ik1 i2 ik2 in
          if i1' <> i2' then one else zero
      | Le, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', ik' = convertInts i1 ik1 i2 ik2 in
          if ge (isunsigned ik') i2' i1' then one else zero

      | Ge, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', ik' = convertInts i1 ik1 i2 ik2 in
          if ge (isunsigned ik') i1' i2' then one else zero

      | Lt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', ik' = convertInts i1 ik1 i2 ik2 in
          if i1' <> i2' && ge (isunsigned ik') i2' i1' then one else zero

      | Gt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) ->
          let i1', i2', ik' = convertInts i1 ik1 i2 ik2 in
          if i1 <> i2 && ge (isunsigned ik') i1' i2' then one else zero

      (* We rely on the fact that LAnd/LOr appear in global initializers
         and should not have side effects. *)
      | LAnd, _, _ when isZero e1' || isZero e2' -> zero
      | LAnd, _, _ when isInteger e1' <> None -> e2'  (* e1' is TRUE *)
      | LAnd, _, _ when isInteger e2' <> None -> e1'  (* e2' is TRUE *)
      | LOr, _, _ when isZero e1' -> e2'
      | LOr, _, _ when isZero e2' -> e1'
      | LOr, _, _ when isInteger e1' <> None || isInteger e2' <> None ->
          (* One of e1' or e2' is a nonzero constant *)
          one
      | _ -> BinOp(bop, e1', e2', tres)
    in
    if debugConstFold then
      ignore (log "Folded %a to %a\n"
                (!pd_exp) (BinOp(bop, e1', e2', tres)) (!pd_exp) newe);
    newe
  end else
    BinOp(bop, e1', e2', tres)

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
      (CInt64(value,IULong,orig_rep)),(TInt(IULong,[]))
    else
      (CInt64(value,IULongLong,orig_rep)),(TInt(IULongLong,[]))
  end

(*/CEA*)


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
   | Le ->  if !print_utf8 then Utf8_logic.le else "<="
   | Ge -> if !print_utf8 then Utf8_logic.ge else ">="
   | Eq -> if !print_utf8 then Utf8_logic.eq else "=="
   | Ne -> if !print_utf8 then Utf8_logic.neq else "!="
   | BAnd -> "&"
   | BXor -> "^"
   | BOr -> "|"
   | LAnd -> if !print_utf8 then Utf8_logic.conj else "&&"
   | LOr -> if !print_utf8 then Utf8_logic.disj else "||")

let d_relation fmt b =
  fprintf fmt "%s"
    (match b with
     | Rlt -> "<"
     | Rgt -> ">"
     | Rle -> if !print_utf8 then Utf8_logic.le else "<="
     | Rge -> if !print_utf8 then Utf8_logic.ge else ">="
     | Req -> if !print_utf8 then Utf8_logic.eq else "=="
     | Rneq -> if !print_utf8 then Utf8_logic.neq else "!=")

let invalidStmt = mkStmt (Instr (Skip locUnknown))

module BuiltinFunctions =
  Computation.Hashtbl
    (struct type t = string let equal = (=) let hash = Hashtbl.hash end)
    (Project.Datatype.Imperative
       (struct
	  type t = typ * typ list * bool
	  let copy _ = assert false (* TODO *)
	  let name = "builtinFunctions"
	end))
    (struct
       let name = "BuiltinFunctions"
       let dependencies = [ TheMachine.self ]
       let size = 49
     end)

(* Initialize the builtin functions after the machine has been initialized. *)
let initGccBuiltins () : unit =
  if not (TheMachine.is_computed ()) then
    E.s (bug "Call initCIL before initGccBuiltins");
  if BuiltinFunctions.length () <> 0 then
    E.s (bug "builtins already initialized.");
  (* See if we have builtin_va_list *)
  let hasbva = Machdep.state.Machdep.gccHas__builtin_va_list in
  let ulongLongType = TInt(IULongLong, []) in
  let floatType = TFloat(FFloat, []) in
  let longDoubleType = TFloat (FLongDouble, []) in
  let voidConstPtrType = TPtr(TVoid [Attr ("const", [])], []) in
  let sizeType = theMachine.upointType in

  BuiltinFunctions.add "__builtin___fprintf_chk" (intType, [ voidPtrType; intType; charConstPtrType ], true) (* first argument is really FILE*, not void*, but we don't want to build in the definition for FILE *);
  BuiltinFunctions.add "__builtin___memcpy_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___memmove_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___mempcpy_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___memset_chk" (voidPtrType, [ voidPtrType; intType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___printf_chk" (intType, [ intType; charConstPtrType ], true);
  BuiltinFunctions.add "__builtin___snprintf_chk" (intType, [ charPtrType; sizeType; intType; sizeType; charConstPtrType ], true);
  BuiltinFunctions.add "__builtin___sprintf_chk" (intType, [ charPtrType; intType; sizeType; charConstPtrType ], true);
  BuiltinFunctions.add "__builtin___stpcpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin___strcat_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin___strcpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin___strncat_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___strncpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType; sizeType ], false);
  BuiltinFunctions.add "__builtin___vfprintf_chk" (intType, [ voidPtrType; intType; charConstPtrType; TBuiltin_va_list [] ], false) (* first argument is really FILE*, not void*, but we don't want to build in the definition for FILE *);
  BuiltinFunctions.add "__builtin___vprintf_chk" (intType, [ intType; charConstPtrType; TBuiltin_va_list [] ], false);
  BuiltinFunctions.add "__builtin___vsnprintf_chk" (intType, [ charPtrType; sizeType; intType; sizeType; charConstPtrType; TBuiltin_va_list [] ], false);
  BuiltinFunctions.add "__builtin___vsprintf_chk" (intType, [ charPtrType; intType; sizeType; charConstPtrType; TBuiltin_va_list [] ], false);

  BuiltinFunctions.add "__builtin_acos" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_acosf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_acosl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_alloca" (voidPtrType, [ sizeType ], false);

  BuiltinFunctions.add "__builtin_asin" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_asinf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_asinl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_atan" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_atanf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_atanl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_atan2" (doubleType, [ doubleType; doubleType ], false);
  BuiltinFunctions.add "__builtin_atan2f" (floatType, [ floatType; floatType ], false);
  BuiltinFunctions.add "__builtin_atan2l" (longDoubleType, [ longDoubleType;
                                                longDoubleType ], false);

  BuiltinFunctions.add "__builtin_ceil" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_ceilf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_ceill" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_cos" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_cosf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_cosl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_cosh" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_coshf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_coshl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_clz" (intType, [ uintType ], false);
  BuiltinFunctions.add "__builtin_clzl" (intType, [ ulongType ], false);
  BuiltinFunctions.add "__builtin_clzll" (intType, [ ulongLongType ], false);
  BuiltinFunctions.add "__builtin_constant_p" (intType, [ intType ], false);
  BuiltinFunctions.add "__builtin_ctz" (intType, [ uintType ], false);
  BuiltinFunctions.add "__builtin_ctzl" (intType, [ ulongType ], false);
  BuiltinFunctions.add "__builtin_ctzll" (intType, [ ulongLongType ], false);

  BuiltinFunctions.add "__builtin_exp" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_expf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_expl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_expect" (longType, [ longType; longType ], false);

  BuiltinFunctions.add "__builtin_fabs" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_fabsf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_fabsl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_ffs" (intType, [ uintType ], false);
  BuiltinFunctions.add "__builtin_ffsl" (intType, [ ulongType ], false);
  BuiltinFunctions.add "__builtin_ffsll" (intType, [ ulongLongType ], false);
  BuiltinFunctions.add "__builtin_frame_address" (voidPtrType, [ uintType ], false);

  BuiltinFunctions.add "__builtin_floor" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_floorf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_floorl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_huge_val" (doubleType, [], false);
  BuiltinFunctions.add "__builtin_huge_valf" (floatType, [], false);
  BuiltinFunctions.add "__builtin_huge_vall" (longDoubleType, [], false);
  BuiltinFunctions.add "__builtin_inf" (doubleType, [], false);
  BuiltinFunctions.add "__builtin_inff" (floatType, [], false);
  BuiltinFunctions.add "__builtin_infl" (longDoubleType, [], false);
  BuiltinFunctions.add "__builtin_memcpy" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin_mempcpy" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin_memset" (voidPtrType,
                              [ voidPtrType; intType; intType ], false);

  BuiltinFunctions.add "__builtin_fmod" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_fmodf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_fmodl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_frexp" (doubleType, [ doubleType; intPtrType ], false);
  BuiltinFunctions.add "__builtin_frexpf" (floatType, [ floatType; intPtrType  ], false);
  BuiltinFunctions.add "__builtin_frexpl" (longDoubleType, [ longDoubleType;
                                                intPtrType  ], false);

  BuiltinFunctions.add "__builtin_ldexp" (doubleType, [ doubleType; intType ], false);
  BuiltinFunctions.add "__builtin_ldexpf" (floatType, [ floatType; intType  ], false);
  BuiltinFunctions.add "__builtin_ldexpl" (longDoubleType, [ longDoubleType;
                                                intType  ], false);

  BuiltinFunctions.add "__builtin_log" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_logf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_logl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_log10" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_log10f" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_log10l" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_modff" (floatType, [ floatType;
                                          TPtr(floatType,[]) ], false);
  BuiltinFunctions.add "__builtin_modfl" (longDoubleType, [ longDoubleType;
                                               TPtr(longDoubleType, []) ],
                             false);

  BuiltinFunctions.add "__builtin_nan" (doubleType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_nanf" (floatType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_nanl" (longDoubleType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_nans" (doubleType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_nansf" (floatType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_nansl" (longDoubleType, [ charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_next_arg" ((if hasbva then TBuiltin_va_list [] else voidPtrType), [], false) (* When we parse builtin_next_arg we drop the second argument *);
  BuiltinFunctions.add "__builtin_object_size" (sizeType, [ voidPtrType; intType ], false);

  BuiltinFunctions.add "__builtin_parity" (intType, [ uintType ], false);
  BuiltinFunctions.add "__builtin_parityl" (intType, [ ulongType ], false);
  BuiltinFunctions.add "__builtin_parityll" (intType, [ ulongLongType ], false);

  BuiltinFunctions.add "__builtin_popcount" (intType, [ uintType ], false);
  BuiltinFunctions.add "__builtin_popcountl" (intType, [ ulongType ], false);
  BuiltinFunctions.add "__builtin_popcountll" (intType, [ ulongLongType ], false);

  BuiltinFunctions.add "__builtin_powi" (doubleType, [ doubleType; intType ], false);
  BuiltinFunctions.add "__builtin_powif" (floatType, [ floatType; intType ], false);
  BuiltinFunctions.add "__builtin_powil" (longDoubleType, [ longDoubleType; intType ], false);
  BuiltinFunctions.add "__builtin_prefetch" (voidType, [ voidConstPtrType ], true);
  BuiltinFunctions.add "__builtin_return" (voidType, [ voidConstPtrType ], false);
  BuiltinFunctions.add "__builtin_return_address" (voidPtrType, [ uintType ], false);

  BuiltinFunctions.add "__builtin_sin" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_sinf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_sinl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_sinh" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_sinhf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_sinhl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_sqrt" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_sqrtf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_sqrtl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_stpcpy" (charPtrType, [ charPtrType; charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_strchr" (charPtrType, [ charPtrType; intType ], false);
  BuiltinFunctions.add "__builtin_strcmp" (intType, [ charConstPtrType; charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_strcpy" (charPtrType, [ charPtrType; charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_strcspn" (sizeType, [ charConstPtrType; charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_strncat" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin_strncmp" (intType, [ charConstPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin_strncpy" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  BuiltinFunctions.add "__builtin_strspn" (sizeType, [ charConstPtrType; charConstPtrType ], false);
  BuiltinFunctions.add "__builtin_strpbrk" (charPtrType, [ charConstPtrType; charConstPtrType ], false);
  (* When we parse builtin_types_compatible_p, we change its interface *)
  BuiltinFunctions.add "__builtin_types_compatible_p"
    (intType, [ theMachine.typeOfSizeOf;(* Sizeof the type *)
                theMachine.typeOfSizeOf (* Sizeof the type *) ],
                             false);
  BuiltinFunctions.add "__builtin_tan" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_tanf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_tanl" (longDoubleType, [ longDoubleType ], false);

  BuiltinFunctions.add "__builtin_tanh" (doubleType, [ doubleType ], false);
  BuiltinFunctions.add "__builtin_tanhf" (floatType, [ floatType ], false);
  BuiltinFunctions.add "__builtin_tanhl" (longDoubleType, [ longDoubleType ], false);


  if hasbva then begin
    BuiltinFunctions.add "__builtin_va_end" (voidType, [ TBuiltin_va_list [] ], false);
    BuiltinFunctions.add "__builtin_varargs_start"
      (voidType, [ TBuiltin_va_list [] ], false);
    (* When we parse builtin_{va,stdarg}_start, we drop the second argument *)
    BuiltinFunctions.add "__builtin_va_start" (voidType, [ TBuiltin_va_list [] ], false);
    BuiltinFunctions.add "__builtin_stdarg_start" (voidType, [ TBuiltin_va_list []; ],
                                      false);
    (* When we parse builtin_va_arg we change its interface *)
    BuiltinFunctions.add "__builtin_va_arg"
      (voidType, [ TBuiltin_va_list [];
                   theMachine.typeOfSizeOf;(* Sizeof the type *)
                   voidPtrType; (* Ptr to res *) ],
                               false);
    BuiltinFunctions.add "__builtin_va_copy" (voidType, [ TBuiltin_va_list [];
					     TBuiltin_va_list [] ],
                                false);
  end;
  ()

(** Construct a hash with the builtins *)
let initMsvcBuiltins () : unit =
  if not (TheMachine.is_computed ()) then
    E.s (bug "Call initCIL before initGccBuiltins");
  if BuiltinFunctions.length () <> 0 then
    E.s (bug "builtins already initialized.");
  (** Take a number of wide string literals *)
  BuiltinFunctions.add "__annotation" (voidType, [ ], true);
  ()

(** This is used as the location of the prototypes of builtin functions. *)
let builtinLoc: location = locUnknown

let range_loc loc1 loc2 = fst loc1, snd loc2

let pred_body = function
  | LBpred a -> a
  | LBreads _
  | LBinductive _
  | LBterm _ ->
      bug "definition expected in Cil.pred_body"



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

  method current_stmt: stmt option
    (** Returns the stmt being printed *)

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

  method pBlock: ?toplevel:bool -> Format.formatter -> block -> unit
    (** Print a block. *)

  method pGlobal: Format.formatter -> global -> unit
    (** Global (vars, types, etc.). This can be slow and is used only by
     * {!Cil.printGlobal} but by {!Cil.dumpGlobal} for everything else except
     * [GVar] and [GFun]. *)

  method pFieldDecl: Format.formatter -> fieldinfo -> unit
    (** A field declaration *)

  method pType: (Format.formatter -> unit) option -> Format.formatter -> typ -> unit
  (** Use of some type in some declaration. The first argument is used to print
    the declared element, or is None if we are just printing a type with no
    name being declared. Note that for structure/union and enumeration types
    the definition of the composite type is not visited. Use [vglob] to
    visit it.  *)

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

  method pLogic_type: Format.formatter -> logic_type -> unit

  method pTsets_elem: Format.formatter -> tsets_elem -> unit

  method pTsets_lhost: Format.formatter -> tsets_lhost -> unit

  method pTsets_offset: Format.formatter -> tsets_offset -> unit

  method pTsets_lval: Format.formatter -> tsets_lval -> unit

  method pTsets: Format.formatter -> tsets -> unit

  method pTerm: Format.formatter -> term -> unit

  method pTerm_node: Format.formatter -> term -> unit

  method pTerm_lval: Format.formatter -> term_lval -> unit

  method pTerm_offset: Format.formatter -> term_offset -> unit

  method pLogic_info_use: Format.formatter -> logic_info -> unit

  method pLogic_var: Format.formatter -> logic_var -> unit

  method pQuantifiers: Format.formatter -> quantifiers -> unit

  method pPredicate: Format.formatter -> predicate -> unit

  method pPredicate_named: Format.formatter -> predicate named -> unit

(*
  method pPredicate_info_use: Format.formatter -> predicate_info -> unit
*)

  method pBehavior: Format.formatter -> funbehavior -> unit

  method pSpec: Format.formatter -> funspec -> unit

  method pZone: Format.formatter -> identified_tsets zone -> unit

  method pAssigns:
    string -> Format.formatter -> identified_tsets assigns -> unit

  method pStatus : Format.formatter -> Cil_types.annot_status -> unit

  method pCode_annot: Format.formatter -> code_annotation -> unit

  method pAnnotation: Format.formatter -> global_annotation -> unit
end


let is_skip = function Instr (Skip _) -> true | _ -> false

let empty_funspec () =
  {spec_requires = [];
   spec_behavior = [];
   spec_variant = None;
   spec_terminates = None;
   spec_complete_behaviors = [];
   spec_disjoint_behaviors = [];
}

let is_empty_funspec spec =
  spec.spec_requires = [] && spec.spec_behavior = [] &&
  spec.spec_variant = None && spec.spec_terminates = None &&
  spec.spec_complete_behaviors = [] && spec.spec_disjoint_behaviors = []


class defaultCilPrinterClass : cilPrinter = object (self)
  val mutable logic_printer_enabled = true
  val mutable verbose = false

  val current_stmt = Stack.create ()
  val mutable current_function = None

  method private in_current_function vi =
    assert (current_function = None);
    current_function <- Some vi
  method private out_current_function =
    assert (current_function <> None);
    current_function <- None

  method current_function = current_function
  method private push_stmt s = Stack.push s current_stmt
  method private pop_stmt s = ignore (Stack.pop current_stmt); s
  method current_stmt =
    try Some (Stack.top current_stmt) with Stack.Empty -> None

  method may_be_skipped s = s.labels = []

  (** Returns the stmt being printed *)

  val mutable currentFormals : varinfo list = []
  method private getLastNamedArgument (s: string) : exp =
    match List.rev currentFormals with
      f :: _ -> Lval (var f)
    | [] ->
        E.s (warn "Cannot find the last named argument when printing call to %s\n" s)

  (*** VARIABLES ***)
  method pVarName fmt v = pp_print_string fmt v

  method private pVarString v =
    fprintf_to_string "%a" self#pVar v

  (* variable use *)
  method pVar fmt (v:varinfo) = Format.fprintf fmt "%a" self#pVarName v.vname

  (* variable declaration *)
  method pVDecl fmt (v:varinfo) =
    let stom, rest = separateStorageModifiers v.vattr in
    (* First the storage modifiers *)
    fprintf fmt "%s%a%a%a %a"
      (if v.vinline then "__inline " else "")
      d_storage v.vstorage
      self#pAttrs stom
      (self#pType (Some (fun fmt -> self#pVar fmt v))) v.vtype
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
    if getParenthLevel (Lval(lv)) >= contextprec then
      fprintf fmt "(%a)" self#pLval lv
    else
      self#pLval fmt lv

  (*** EXPRESSIONS ***)
  method pExp fmt (e: exp) =
    let level = getParenthLevel e in
    match stripInfo e with
    | Info _ -> assert false
    | Const(c) -> d_const fmt c
    | Lval(l) -> self#pLval fmt l
    | UnOp(u,e1,_) ->
        fprintf fmt "%a %a"
          d_unop u
          (self#pExpPrec level) e1

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

    | StartOf(lv) -> self#pLval fmt lv

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
              TComp(ci, _), [(Field(f, NoOffset), _)] ->
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
          | _ -> E.s (unimp "Trying to print malformed initializer")
        in
        fprintf fmt  "{@[%a@]}"
          (fprintfList ~sep:",@ " d_oneInit) initl


  (** What terminator to print after an instruction. sometimes we want to
      * print sequences of instructions separated by comma *)
  val mutable printInstrTerminator = ";"

  method private setPrintInstrTerminator (term : string) =
    printInstrTerminator <- term

  method private getPrintInstrTerminator () = printInstrTerminator

  (*** INSTRUCTIONS ****)
  method pInstr fmt (i:instr) =       (* imperative instruction *)
    fprintf fmt "%a" (self#pLineDirective ~forcefile:false) (get_instrLoc i);
    match i with
    | Skip _ -> fprintf fmt ";"
    | Set(lv,e,_) -> begin
        (* Be nice to some special cases *)
        match e with
          BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt64(one,_,_)),_)
            when equals lv lv' && one = Int64.one
	      && not miscState.printCilAsIs ->
		fprintf fmt "%a ++%s"
		  (self#pLvalPrec indexLevel) lv
                  printInstrTerminator
        | BinOp((MinusA|MinusPI),Lval(lv'),
                Const(CInt64(one,_,_)), _)
            when equals lv lv' && one = Int64.one
	      && not miscState.printCilAsIs ->
            fprintf fmt "%a --%s"
              (self#pLvalPrec indexLevel) lv
              printInstrTerminator

        | BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt64(mone,_,_)),_)
            when equals lv lv' && mone = Int64.minus_one
              && not miscState.printCilAsIs ->
            fprintf fmt "%a --%s"
              (self#pLvalPrec indexLevel) lv
              printInstrTerminator

        | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
                     Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
                Lval(lv'),e,_) when equals lv lv' ->
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
    | Call(None, Lval(Var vi, NoOffset), [dest; SizeOf t; adest], l)
        when vi.vname = "__builtin_va_arg" && not miscState.printCilAsIs ->
        let destlv = match stripCasts adest with
          AddrOf destlv -> destlv
            (* If this fails, it's likely that an extension interfered
	       with the AddrOf *)
        | _ -> E.s (bug
		      "%a: Encountered unexpected call to %s with dest %a\n"
		      d_loc l vi.vname self#pExp adest)
        in
        fprintf fmt "%a = __builtin_va_arg (@[%a,@ %a@])%s"
          self#pLval destlv
          (* Now the arguments *)
          self#pExp dest
          (self#pType None)  t
          printInstrTerminator

    (* In cabs2cil we have dropped the last argument in the call to
     * __builtin_va_start and __builtin_stdarg_start. *)
    | Call(None, Lval(Var vi, NoOffset), [marker], l)
        when ((vi.vname = "__builtin_stdarg_start" ||
                  vi.vname = "__builtin_va_start")
	      && not miscState.printCilAsIs) ->
	begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr fmt (Call(None,Lval(Var vi,NoOffset),[marker; last],l))
        end

    (* In cabs2cil we have dropped the last argument in the call to
     * __builtin_next_arg. *)
    | Call(res, Lval(Var vi, NoOffset), [ ], l)
        when vi.vname = "__builtin_next_arg" && not miscState.printCilAsIs ->
	begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr fmt (Call(res,Lval(Var vi,NoOffset),[last],l))
        end

    (* In cparser we have turned the call to
     * __builtin_types_compatible_p(t1, t2) into
     * __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
     * represent the types as expressions.
     * Remove the sizeofs when printing. *)
    | Call(dest, Lval(Var vi, NoOffset), [SizeOf t1; SizeOf t2], _)
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
    | Call(_, Lval(Var vi, NoOffset), _, _)
        when vi.vname = "__builtin_types_compatible_p"
	  && not miscState.printCilAsIs ->
        E.s (bug "__builtin_types_compatible_p: cabs2cil should have added sizeof to the arguments.")

    | Call(dest,e,args,_) ->
        (match dest with
           None -> ()
         | Some lv ->
             fprintf fmt "%a = "
               self#pLval lv;
             (* Maybe we need to print a cast *)
             (let destt = typeOfLval lv in
              match unrollType (typeOf e) with
                TFun (rt, _, _, _)
                  when not (equals (!pTypeSig rt)
                              (!pTypeSig destt)) ->
                    fprintf fmt "(%a)"
                      (self#pType None) destt
              | _ -> ()));
        (* Now the function name *)
        (match e with
           Lval(Var _, _) -> self#pExp fmt e
         | _ -> fprintf fmt "(%a)"  self#pExp e);
        fprintf fmt "(@[%a@])%s"
          (* Now the arguments *)
          (fprintfList ~sep:",@ " self#pExp)
          args
          printInstrTerminator


    | Asm(attrs, tmpls, outs, ins, clobs, l) ->
        self#pLineDirective fmt l;
        if theMachine.msvcMode then
          fprintf fmt "__asm {@[%a@]}%s"
            (fprintfList ~sep:"@\n" (fun fmt s -> fprintf fmt "%s" s)) tmpls
            printInstrTerminator
        else begin
          fprintf fmt "__asm__ %a (@[%a"
            self#pAttrs attrs
            (fprintfList ~sep:"@\n" (fun fmt x -> fprintf fmt "\"%s\"" (escape_string x))) tmpls;

          if outs = [] && ins = [] && clobs = [] then
            fprintf fmt ":"
          else
            fprintf fmt ": %a"
              (fprintfList ~sep:",@ "
                 (fun fmt (idopt, c, lv) ->
                    fprintf fmt "%s\"%s\" (%a)"
                      (match idopt with
                         None -> ""
                       | Some id -> "[" ^ id ^ "] "
                      )
                      (escape_string c)
                      self#pLval lv
                 )) outs;

          if ins = [] && clobs = [] then
            ()
          else
            fprintf fmt ": %a"
              (fprintfList ~sep:",@ "
                 (fun fmt (idopt, c, e) ->
                    fprintf fmt "%s\"%s\"(%a)"
                      (match idopt with
                         None -> ""
                       | Some id -> "[" ^ id ^ "] "
                      )
                      (escape_string c)
                      self#pExp e))
              ins;


          if clobs = [] then ()
          else
            fprintf fmt ": %a"
              (fprintfList ~sep:",@ "
                 (fun fmt c -> fprintf fmt "\"%s\"" (escape_string c)))
              clobs;

          fprintf fmt "@])%s" printInstrTerminator
        end
    | Code_annot (annot, l) ->
	if logic_printer_enabled then
          fprintf fmt "%a@[/*@@ %a*/@]"
            (self#pLineDirective ~forcefile:false) l
	    self#pCode_annot annot

  (**** STATEMENTS ****)
  method pStmt fmt (s:stmt) =        (* control-flow statement *)
    self#push_stmt s;
    self#pop_stmt (self#pStmtNext invalidStmt fmt s)

  method pStmtNext (next: stmt) fmt (s: stmt) =
    self#push_stmt s;
    self#pop_stmt (self#pAnnotatedStmt next fmt s)

  method pAnnotatedStmt (next: stmt) fmt (s: stmt) =
    if false then (* CEA: to debug location setting *)
      (let loc = fst (get_stmtLoc s.skind) in
       fprintf fmt "/*Loc=%s:%d*/" loc.Lexing.pos_fname loc.Lexing.pos_lnum);
    (* print the labels *)
    fprintfList ~sep:"@\n" (fun fmt l -> self#pLabel fmt l) fmt s.labels;

    (* print the statement itself. If the labels are non-empty and the
     * statement is empty, print a semicolon  *)
    if is_skip s.skind && not s.ghost then
      (if verbose || s.labels <> [] then fprintf fmt ";")
    else
      (if s.labels <> [] then fprintf fmt "@\n";
       if s.ghost then fprintf fmt "@[/*@@ @[ghost ";
       self#pStmtKind next fmt s.skind ;
       if s.ghost then fprintf fmt "@]*/@]";)
        (* OLD value : if is_skip s.skind && s.labels <> [] then
           text ";" else (if s.labels <> [] then line else nil) ++
           self#pStmtKind next () s.skind *);
    if s.ghost then fprintf fmt "@]*/@]"

  method private pLabel fmt = function
      Label (s, _, true) -> fprintf fmt "%s: " s
    | Label (s, _, false) -> fprintf fmt "%s: /* CIL Label */ " s
    | Case (e, _) -> fprintf fmt "case %a: " self#pExp e
    | Default _ -> fprintf fmt "default: "

  (* The pBlock will put the unalign itself *)
  method pBlock ?(toplevel=true) fmt (blk: block) =
    let force_paren =
      toplevel ||
        match blk.bstmts with
        | [_] | [] ->
            blk.battrs <> []
        | _ -> true
    in
    let rec dofirst () = function
        [] -> ()
      | [x] -> self#pStmtNext invalidStmt fmt x
      | x :: rest -> dorest x rest
    and dorest prev = function
        [] -> self#pStmtNext invalidStmt fmt prev
      | x :: rest ->
          fprintf fmt "%a@\n" (self#pStmtNext x) prev;
          dorest x rest
    in
    (* Let the host of the block decide on the alignment. The d_block will
     * pop the alignment as well  *)
    if force_paren then fprintf fmt "{";
    if blk.battrs <> [] then
      self#pAttrsGen true fmt blk.battrs;
    dofirst () blk.bstmts;
    if force_paren then fprintf fmt "}";
     fprintf fmt "@]@\n"


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
        let print_stmt pstmt fmt (stmt, writes, reads) =
          fprintf fmt "%a%a"
            pstmt stmt
            (print_if verbose)
            (fun fmt () ->
               Format.fprintf fmt "/*@ @[%a@ <-@ %a@]*/"
                 (pretty_list (space_sep ",") self#pLval) writes
                 (pretty_list (space_sep ",") self#pLval) reads)

        in
        let rec dofirst () = function
            [] -> ()
          | [x] -> print_stmt (self#pStmtNext invalidStmt) fmt x
          | x :: rest -> dorest x rest
        and dorest ((p,_,_) as prev) = function
            [] -> print_stmt (self#pStmtNext invalidStmt) fmt prev
          | (s,_,_) as x :: rest ->
              let newline_cond =
                verbose ||
                  not (is_skip p.skind) || p.ghost || p.labels <> []
              in
              fprintf fmt "%a%a" (print_stmt (self#pStmtNext s)) prev
                (print_if newline_cond) newline;
              dorest x rest
        in
        fprintf fmt "@[<1>{%a"
          (print_if verbose)
          (fun fmt () -> fprintf fmt "/*unspecified sequence*/@\n");
        dofirst () seq;
        fprintf fmt "@]}"
    | Return(None, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "return;"

    | Return(Some e, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "return (%a);"
          self#pExp  e

    | Goto (sref, _) -> begin
        (* Grab one of the labels *)
        let rec pickLabel = function
            [] -> None
          | Label (l, _, _) :: _ -> Some l
          | _ :: rest -> pickLabel rest
        in
        match pickLabel !sref.labels with
          Some l -> fprintf fmt "goto %s;" l
        | None ->
            ignore (error "Cannot find label for target of goto\n");
            fprintf fmt "goto __invalid_label;"
      end

    | Break l ->
        self#pLineDirective fmt l;
        fprintf fmt "break;"

    | Continue l ->
        self#pLineDirective fmt l;
        fprintf fmt "continue;"

    | Instr i ->
        fprintf fmt "@[%a@]"
          self#pInstr i

    | If(be,t,{bstmts=[];battrs=[]},l) when not miscState.printCilAsIs ->
        fprintf fmt "%aif@[ (%a) %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp be
          (self#pBlock ~toplevel:true) t

    | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]}];
               battrs=[]},l)
        when !gref == next && not miscState.printCilAsIs ->
        fprintf fmt "%aif@[ (%a) %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp be
          (self#pBlock ~toplevel:true) t

    | If(be,{bstmts=[];battrs=[]},e,l) when not miscState.printCilAsIs ->
        fprintf fmt "%aif@[ (%a) %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp (UnOp(LNot,be,intType))
          (self#pBlock ~toplevel:true) e

    | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]}];
             battrs=[]},e,l)
        when !gref == next && not miscState.printCilAsIs ->
        fprintf fmt "%aif@[ (%a) %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp (UnOp(LNot,be,intType))
          (self#pBlock ~toplevel:true) e

    | If(be,t,e,l) ->
        fprintf fmt "%a@[@[if (%a)@ %aelse %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp be
          (self#pBlock ~toplevel:true) t
          (self#pBlock ~toplevel:true) e

    | Switch(e,b,_,l) ->
        fprintf fmt "%a@[switch (%a) %a"
          (self#pLineDirective ~forcefile:false) l
          self#pExp e
          (self#pBlock ~toplevel:true) b

    | Loop(annot, b, l, _, _) ->
        if logic_printer_enabled then
          pretty_list_del
            (swap fprintf "@[@[<4>/*@@@ ")
            (swap fprintf "@\n@]@ */@]@\n")
            nl_sep self#pCode_annot fmt annot;
        begin
          (* Maybe the first thing is a conditional. Turn it into a WHILE *)
          try
            let term, bodystmts =
              let rec skipEmpty = function
                  [] -> []
                | {skind=Instr (Skip _);labels=[]} :: rest -> skipEmpty rest
                | x -> x
              in
              (* Bill McCloskey: Do not remove the If if it has labels *)
              match skipEmpty b.bstmts with
                {skind=If(e,tb,fb,_)} as to_skip :: rest
                  when
                    not miscState.printCilAsIs && self#may_be_skipped to_skip ->
                      begin
                        match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                          [], {skind=Break _; labels=[]} :: _  -> e, rest
                        | {skind=Break _; labels=[]} :: _, []
                            -> UnOp(LNot, e, intType), rest
                        | _ -> raise Not_found
                      end
              | _ -> raise Not_found
            in
            self#pLineDirective fmt l;
            fprintf fmt "@[<2>while (%a) %a"
              self#pExp term
              (self#pBlock ~toplevel:true) {bstmts=bodystmts; battrs=b.battrs}

          with Not_found ->
            self#pLineDirective fmt l;
            fprintf fmt "@[<2>while (1) %a"
              (self#pBlock ~toplevel:true) b
        end

    | Block b -> fprintf fmt "@[%a" (self#pBlock ~toplevel:false) b

    | TryFinally (b, h, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "__try @[%a @[<5>__finally%a"
          (self#pBlock ~toplevel:true) b
          (self#pBlock ~toplevel:true) h

    | TryExcept (b, (il, e), h, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "__try @[%a __except(@\n@["
          (self#pBlock ~toplevel:true) b;

        (* Print the instructions but with a comma at the end, instead of
         * semicolon *)
        printInstrTerminator <- ",";
        fprintfList ~sep:"@\n" self#pInstr fmt il;
        printInstrTerminator <- ";";
        fprintf fmt "%a) @]%a"
          self#pExp e
          (self#pBlock ~toplevel:true) h

  (*** GLOBALS ***)
  method pGlobal fmt (g:global) =       (* global (vars, types, etc.) *)
    match g with
    | GFun (fundec, l) ->
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


    | GType (typ, l) ->
        self#pLineDirective ~forcefile:true fmt l;
        fprintf fmt "typedef %a;@\n"
          (self#pType (Some (fun fmt -> fprintf fmt "%s" typ.tname))) typ.ttype

    | GEnumTag (enum, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "enum@[ %a {@\n%a@]@\n} %a;@\n"
          self#pVarName enum.ename
          (fprintfList ~sep:",@\n"
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
        fprintf fmt "@[<3>%s %a%a {@\n%a@]@\n}%a;@\n"
          su
          self#pAttrs sto_mod
          self#pVarName n
          (fprintfList ~sep:"@\n" self#pFieldDecl)
          comp.cfields
          self#pAttrs rest_attr

    | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
        self#pLineDirective fmt l;
        fprintf fmt "%s;@\n" (compFullName comp)

    | GVar (vi, io, l) ->
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

    (* print global variable 'extern' declarations, and function prototypes *)
    | GVarDecl (funspec, vi, l) ->
        self#opt_funspec fmt funspec;
        if isFunctionType vi.vtype then self#in_current_function vi;
        if not miscState.printCilAsIs && BuiltinFunctions.mem vi.vname then
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


    | GAsm (s, l) ->
        self#pLineDirective fmt l;
        fprintf fmt "__asm__(\"%s\");@\n" (escape_string s)

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
	  | _ ->
              fprintf fmt "%s(%a)"
                an
                (fprintfList ~sep:"," self#pAttrParam) args

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
       fprintf fmt "/*@[@@ %a@]@\n*/@\n" self#pSpec funspec

  method private pFunDecl fmt f =
    fprintf fmt "%a%a@\n{ @[%a@\n@\n"
      self#opt_funspec f.sspec
      self#pVDecl f.svar
      (* locals. *)
      (fprintfList ~sep:"@\n" (fun fmt vi -> fprintf fmt "%a;" self#pVDecl  vi))
      f.slocals;
    (* the body *)
    (* remember the declaration *)
    currentFormals <- f.sformals;
    self#pBlock ~toplevel:false fmt f.sbody;
    currentFormals <- [];
    fprintf fmt "@\n}"


  (***** PRINTING DECLARATIONS and TYPES ****)

  method pType nameOpt (* Whether we are declaring a name or
                        * we are just printing a type *)
    fmt (t:typ) =       (* use of some type *)
    let name = match nameOpt with None -> (fun _ -> ()) | Some d -> d in
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
        fprintf fmt "void%a %t"
          self#pAttrs a
          name

    | TInt (ikind,a) ->
        fprintf fmt "%a%a %t"
          d_ikind ikind
          self#pAttrs a
          name

    | TFloat(fkind, a) ->
        fprintf fmt "%a%a %t"
          d_fkind fkind
          self#pAttrs a
          name

    | TComp (comp, a) -> (* A reference to a struct *)
        fprintf fmt
          "%s %a %a%t"
          (if comp.cstruct then "struct" else "union")
          self#pVarName comp.cname
          self#pAttrs a
          name

    | TEnum (enum, a) ->
        fprintf fmt "enum %a %a%t"
          self#pVarName enum.ename
          self#pAttrs a
          name

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
          fprintf fmt "*%a%t"
            printAttributes a
            name
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

    | TArray (elemt, lo, a) ->
        (* ignore the const attribute for arrays *)
        let a' = dropAttributes [ "const" ] a in
        let name' = fun fmt ->
          if a' == [] then name fmt else
            if nameOpt == None then
              fprintf fmt
                "%a"
                printAttributes a'
            else
              fprintf fmt
                "(%a%t)"
                printAttributes a'
                name
        in
        self#pType
          (Some (fun fmt ->
                   fprintf fmt "%t[%t]"
                     name'
                     (fun fmt ->
                        match lo with
                        | None -> ()
                        | Some e ->
                            fprintf fmt
                              "%a"
                              self#pExp e)
                ))
          fmt
          elemt

    | TFun (restyp, args, isvararg, a) ->
        let name' fmt =
          if a == [] then name fmt else
            if nameOpt == None then
              fprintf fmt
                "%a"
                printAttributes a
            else
              fprintf fmt
                "(%a%t)"
                printAttributes a
                name
        in
        self#pType
          (Some
             (fun fmt ->
                fprintf fmt
                  "%t(@[%t@])"
                  name'
                  (fun fmt -> if args = Some [] && isvararg then
                     fprintf fmt "..."
                   else
                     (if args = None then ()
                      else if args = Some [] then fprintf fmt "void"
                      else
			let pArg fmt (aname, atype, aattr) =
                          let stom, rest = separateStorageModifiers aattr in
                          (* First the storage modifiers *)
                          fprintf fmt
                            "%a%a %a"
                            self#pAttrs stom
                            (self#pType (Some (fun fmt -> fprintf fmt "%s" aname))) atype
                            self#pAttrs rest
			in
			(fprintfList ~sep:",@ " pArg)
                          fmt
                          (argsToList args) ;
			if isvararg then fprintf fmt "@ , ...";
                     ))))
          fmt
          restyp

    | TNamed (t, a) ->
        fprintf fmt "%a%a %t"
          self#pVarName t.tname
          self#pAttrs a
          name

    | TBuiltin_va_list a ->
        fprintf fmt "__builtin_va_list%a %t"
          self#pAttrs a
          name


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
               (fprintfList ~sep:"" self#pAttrParam) args;
             false
         | "w64", [] when theMachine.msvcMode -> fprintf fmt "__w64"; false
         | "asm", args ->
             fprintf fmt "__asm__(%a)"
               (fprintfList ~sep:"" self#pAttrParam) args;
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

         (* sm: here's another one I don't want to see gcc warnings about.. *)
         | "mayPointToStack", _ when not miscState.print_CIL_Input
             (* [matth: may be inside another comment.]
                -> text "/*mayPointToStack*/", false
             *)
             -> fprintf fmt ""; false

         | "arraylen", [a] ->
             fprintf fmt "/*[%a]*/" self#pAttrParam a;
             false

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
		  (fprintfList ~sep:"," self#pAttrParam) args;
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
    | AInt n -> fprintf fmt "%d" n
    | AStr s -> fprintf fmt "\"%s\"" (escape_string s)
    | ACons(s, []) -> fprintf fmt "%s" s
    | ACons(s,al) ->
        fprintf fmt "%s(%a)"
          s
          (fprintfList ~sep:"" self#pAttrParam) al
    | ASizeOfE a -> fprintf fmt "sizeof(%a)" self#pAttrParam a
    | ASizeOf t -> fprintf fmt "sizeof(%a)" (self#pType None) t
    | ASizeOfS _ts -> fprintf fmt "sizeof(<typsig>)"
    | AAlignOfE a -> fprintf fmt "__alignof__(%a)" self#pAttrParam a
    | AAlignOf t -> fprintf fmt "__alignof__(%a)" (self#pType None) t
    | AAlignOfS _ts -> fprintf fmt "__alignof__(<typsig>)"
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
        [] -> begin
          match in__attr__ with
	    [] -> ()
          | _ :: _->
	      (* sm: added 'forgcc' calls to not comment things out
	       * if CIL is the consumer; this is to address a case
	       * Daniel ran into where blockattribute(nobox) was being
	       * dropped by the merger
	       *)
	      (if block then
                 fprintf fmt " %s __blockattribute__("
		   (forgcc "/*")
	       else
                 fprintf fmt "__attribute__((");
	      fprintfList ~sep:",@ "
                (fun fmt a -> fprintf fmt "%s" a)
                fmt
                in__attr__;
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
          else if dx = "" then
            loop in__attr__ rest
          else
            (fprintf fmt "%s " dx;
             loop in__attr__ rest)
    in
    let a =
      List.filter (function Attr (s,_) -> not (List.mem s reserved_attributes)
                     | AttrAnnot _ -> true) a
    in
    if a <> [] then
      begin
        fprintf fmt " ";
        loop [] a;
        fprintf fmt " "
      end

  (* Logic annotations printer *)

  method pLogic_type fmt = function
    | Ctype typ -> self#pType None fmt typ
    | Linteger -> pp_print_string fmt "integer"
    | Lreal -> pp_print_string fmt "real"
    | Ltype (s,l) ->
        fprintf fmt "%a%a" self#pVarName s
          (pretty_list_del (fun fmt -> fprintf fmt "<@[")
             (fun fmt -> fprintf fmt "@]>@ ")
             (* the space avoids the issue of list<list<int>> where the double >
                would be read as a shift. It could be optimized away in most of
                the cases.
             *)
             (space_sep ",") self#pLogic_type) l
    | Larrow (args,rt) ->
        fprintf fmt "@[@[<2>{@ %a@]@}@]%a"
          (pretty_list (space_sep ",") self#pLogic_type) args
          self#pLogic_type rt
    | Lvar s -> fprintf fmt "%a" self#pVarName s

  method pTsets_lval fmt (h,o) =
    match h,o with
        TSMem t, TSField (f,o) ->
          fprintf fmt "%a->@,%a@,%a"
            (self#pTsets_elemPrec arrowLevel) t
             self#pVarName f.fname self#pTsets_offset o
      | _ -> fprintf fmt "@[%a%a@]" self#pTsets_lhost h self#pTsets_offset o

  method private pTsets_elemPrec contextprec fmt e =
    let thisLevel = getParenthLevelTsetsElem e in
    let needParens =
      if thisLevel >= contextprec then true
      else if contextprec == bitwiseLevel then
        (* quiet down some GCC warnings *)
	thisLevel == additiveLevel || thisLevel == comparativeLevel
      else
	false
    in
    if needParens then
      fprintf fmt "@[<hov 1>(%a)@]" self#pTsets_elem e
    else
      self#pTsets_elem fmt e

  method pTsets_elem fmt lv =
    let level = getParenthLevelTsetsElem lv in
    match lv with
        TSLval lv | TSStartOf lv -> self#pTsets_lval fmt lv
          (*TODO: take into account priority in ts_lvals *)
      | TSAddrOf lv -> fprintf fmt "&%a" self#pTsets_lval lv
      | TSConst s ->  d_const fmt s
      | TSAdd_range(t,low,high) ->
          fprintf fmt "@[%a@ +@ (@[%a..@,%a@])@]"
            (self#pTsets_elemPrec level) t
            (pretty_opt
               (fun fmt t -> fprintf fmt "%a@ " self#pTerm t)) low
            (pretty_opt
               (fun fmt t ->fprintf fmt "@ %a" self#pTerm t)) high
      | TSAdd_index(t,i) ->
          fprintf fmt "@[%a@ +@ %a@]"
            (self#pTsets_elemPrec level) t
            (self#pTermPrec level) i
      | TSCastE(typ,elem) ->
          fprintf fmt "@[(%a)@,%a@]"
            (self#pType None) typ (self#pTsets_elemPrec level) elem
      | TSat(l,lab) ->
	  fprintf fmt "@[<hov 2>\\at(%a,%a)@]"
	    self#pTsets_elem l self#pLogicLabel lab
      | TSapp(f,labs,args) ->
          fprintf fmt "@[<hov 2>%a@,%a@,%a@]"
            self#pLogic_info_use f
            (pretty_list_del
               (fun fmt -> pp_print_string fmt "<")
               (fun fmt -> pp_print_string fmt ">")
               (space_sep ",")
               (fun fmt (_,y) -> self#pLogicLabel fmt y)) labs
            (pretty_list_del
               (fun fmt -> pp_print_string fmt "(")
               (fun fmt -> pp_print_string fmt ")")
               (space_sep ",") self#pTerm) args

  method pTsets_lhost fmt h =
    match h with
      | TSVar lv -> self#pLogic_var fmt lv
      | TSResult -> pp_print_string fmt "\\result"
      | TSMem (TSAdd_index(t,i)) ->
          fprintf fmt "@[<2>%a[%a]@]"
            (self#pTsets_elemPrec derefStarLevel) t
            self#pTerm i
      | TSMem (TSAdd_range(t,low,high)) ->
          fprintf fmt "@[<2>%a[%a..%a]@]"
            (self#pTsets_elemPrec derefStarLevel) t
            (pretty_opt self#pTerm) low
            (pretty_opt self#pTerm) high
      | TSMem t -> fprintf fmt "@[<1>*%a@]"
          (self#pTsets_elemPrec derefStarLevel) t

  method pTsets_offset fmt o =
    match o with
      | TSNoOffset -> ()
      | TSIndex (t,o) ->
          fprintf fmt "@,@[<1>[%a]@]%a" self#pTerm t self#pTsets_offset o
      | TSRange (t1,t2,o) ->
          fprintf fmt "@,@[<1>[%a..%a]@]%a"
            (pretty_opt self#pTerm) t1
            (pretty_opt self#pTerm) t2
            self#pTsets_offset o
      | TSField(f,o) ->
          fprintf fmt "@,@[<1>.%a@]%a"
            self#pVarName f.fname self#pTsets_offset o

  method pTsets fmt loc = (* to be rewritten *)
    match loc with
    | TSSingleton t -> self#pTsets_elem fmt t
    | TSUnion locs ->
        fprintf fmt "@[<hov 2>\\union(@,%a)@]"
          (pretty_list (space_sep ",") self#pTsets) locs
    | TSInter locs ->
        fprintf fmt "@[<hov 2>\\inter(@,%a)@]"
          (pretty_list (space_sep ",") self#pTsets) locs
    | TSEmpty -> pp_print_string fmt "\\empty"
    | TSComprehension(lv,quant,pred) ->
        fprintf fmt "{@[%a@ |@ %a%a@]}"
          self#pTsets lv self#pQuantifiers quant
          (pretty_opt (fun fmt p -> fprintf fmt ";@ %a"
                                 self#pPredicate_named p))
          pred

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

  method pTerm fmt t =
    match t.term_name with
      [] -> self#pTerm_node fmt t
    | _ ->
        fprintf fmt "(@[%a:@ %a@])"
          (pretty_list (swap fprintf ":@ ") pp_print_string) t.term_name
          self#pTerm_node t

  method pTerm_node fmt t =
    let current_level = getParenthLevelLogic t.term_node in
     match t.term_node with
    | TConst s -> fprintf fmt "%a" d_const s
    | TDataCons(ci,args) ->
        fprintf fmt "%a%a" self#pVarName ci.ctor_name
          (pretty_list_del (swap fprintf "(@[") (swap fprintf "@])")
             (space_sep ",") self#pTerm) args
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
    | TStartOf lv -> fprintf fmt "%a" (self#pTerm_lvalPrec current_level) lv
    | Tapp (f, labels, tl) -> fprintf fmt "%a%a%a"
        self#pLogic_info_use f
	  self#pLabels (List.map snd labels)
	  (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]")
             (space_sep ",") self#pTerm) tl
    | Tif (cond,th,el) ->
        fprintf fmt "@[<2>%a?@;%a:@;%a@]"
          (self#pTermPrec current_level) cond
          (self#pTermPrec current_level) th
          (self#pTermPrec current_level) el
    | Told e -> fprintf fmt "\\old(%a)" self#pTerm e
    | Tat (t,lab) ->
        begin
          let rec pickLabel = function
            | [] -> None
            | Label (l, _, _) :: _ -> Some l
            | _ :: rest -> pickLabel rest
          in
          let l = match lab with
	    | LogicLabel s -> s
	    | StmtLabel sref ->
		match pickLabel !sref.labels with
		    Some l -> l
		  | None ->
		      error "Cannot find label for \\at@.";
		      "__invalid_label__"
          in
          fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]" self#pTerm t l
        end
    | Tbase_addr t -> fprintf fmt "\\base_addr(%a)" self#pTerm t
    | Tblock_length t -> fprintf fmt "\\block_length(%a)" self#pTerm t
    | Tnull -> fprintf fmt "\\null"
    | TCoerce (e,ty) ->
	fprintf fmt "%a@ :>@ %a"
          (self#pTermPrec current_level) e (self#pType None) ty
    | TCoerceE (e,ce) ->
	fprintf fmt "%a :> %a"
          (self#pTermPrec current_level) e (self#pTermPrec current_level) ce
    | TUpdate (t,f,v) ->
        fprintf fmt "{%a for %s = %a}"
          self#pTerm t
          f.fname
          self#pTerm v
    | Tlambda(prms,expr) ->
        fprintf fmt "@[<2>\\lambda@ %a;@ %a@]"
          self#pQuantifiers prms (self#pTermPrec current_level) expr
    | Ttypeof t -> fprintf fmt "\\typeof(%a)" self#pTerm t
    | Ttype ty -> fprintf fmt "\\type(%a)" (self#pType None) ty
    | Ttsets tsets ->
        self#pTsets fmt tsets

  method private pTerm_lvalPrec contextprec fmt lv =
    if getParenthLevelLogic (TLval lv) > contextprec then
      fprintf fmt "(%a)" self#pTerm_lval lv
    else
      fprintf fmt "%a" self#pTerm_lval lv

  method pTerm_lval fmt lv = match lv with
  | TVar vi, o -> fprintf fmt "%a%a" self#pLogic_var vi self#pTerm_offset o
  | TResult, o -> fprintf fmt "\\result%a" self#pTerm_offset o
  | TMem ({term_node=TBinOp((PlusPI|IndexPI),base,off)}), o ->
      fprintf fmt "%a[%a]%a"
        (self#pTermPrec derefStarLevel) base
        self#pTerm off
        self#pTerm_offset o
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
  | TIndex(e,o) -> fprintf fmt "[%a]%a" self#pTerm e self#pTerm_offset o

  method pLogic_info_use fmt li = self#pVarName fmt li.l_name

  method pLogic_var fmt v = self#pVarName fmt v.lv_name

  method pQuantifiers fmt l =
    pretty_list (space_sep ",")
      (fun fmt lv ->
         fprintf fmt "%a@ %a" self#pLogic_type lv.lv_type self#pLogic_var lv)
      fmt l

  method pPredicate fmt p =
    (* TODO: get a real priority level for predicates and use it to
       pretty_print subterms.
    *)
    let term = self#pTermPrec logic_level in
    match p with
    | Pfalse -> fprintf fmt "\\false"
    | Ptrue -> fprintf fmt "\\true"
    | Papp (p,labels,l) -> fprintf fmt "@[%a%a%a@]"
        self#pLogic_info_use p
	  self#pLabels (List.map snd labels)
	  (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]")
             (space_sep ",") self#pTerm) l
    | Prel (rel,l,r) ->
        fprintf fmt "@[(@[%a@]@ %a@ @[%a@])@]" term l d_relation rel term r
    | Pand (p1, p2) ->
        fprintf fmt "@[(@[%a@]@ %a@ @[%a@])@]"
          self#pPredicate_named p1
          d_term_binop LAnd
          self#pPredicate_named p2
    | Por (p1, p2) ->
        fprintf fmt "@[(@[%a@]@ %a@ @[%a@])@]"
          self#pPredicate_named p1
          d_term_binop LOr
          self#pPredicate_named p2
    | Pxor (p1, p2) ->
        fprintf fmt "@[(@[%a@]@ %s@ @[%a@])@]"
          self#pPredicate_named p1
	  (if !print_utf8 then Utf8_logic.x_or else "^^")
          self#pPredicate_named p2
    | Pimplies (p1,p2) ->
        fprintf fmt "@[(@[%a@]@ %s@ @[%a@])@]"
          self#pPredicate_named p1
          (if !print_utf8 then Utf8_logic.implies else "==>")
          self#pPredicate_named p2
    | Piff (p1,p2) ->
        fprintf fmt "@[(@[%a@]@ %s@ @[%a@])@]"
          self#pPredicate_named p1
          (if !print_utf8 then Utf8_logic.iff else "<==>")
          self#pPredicate_named p2
    | Pnot a -> fprintf fmt "@[%s(@[%a@])@]"
        (if !print_utf8 then Utf8_logic.neg else "!")
          self#pPredicate_named a
    | Pif (e, p1, p2) ->
        fprintf fmt "@[<2>(%a?@ %a:@ %a)@]"
          term e self#pPredicate_named p1 self#pPredicate_named p2
    | Plet (v, t, p) ->
        fprintf fmt "@[(@[let %a =@]@ @[%a@]@ @[in %a@])@]"
	  self#pLogic_var v term t self#pPredicate_named p
    | Pforall (quant,pred) ->
        fprintf fmt "@[(@[%s %a;@]@ %a)@]"
          (if !print_utf8 then Utf8_logic.forall else "\\forall")
          self#pQuantifiers quant self#pPredicate_named pred
    | Pexists (quant,pred) ->
        fprintf fmt "@[(@[%s %a;@]@ %a)@]"
          (if !print_utf8 then  Utf8_logic.exists else "\\exists")
          self#pQuantifiers quant self#pPredicate_named pred
    | Pold a ->  fprintf fmt "@[\\old(@[%a@])@]" self#pPredicate_named a
    | Pvalid p ->  fprintf fmt "@[\\valid(@[%a@])@]" self#pTsets p
    | Pseparated seps ->
        fprintf fmt "@[<2>\\separated(@,%a@,)@]"
          (pretty_list (space_sep ",") self#pTsets) seps
    | Pat (p,StmtLabel sref) ->
        begin
          let rec pickLabel = function
            | [] -> None
            | Label (l, _, _) :: _ -> Some l
            | _ :: rest -> pickLabel rest
          in
          let l = match pickLabel !sref.labels with
            Some l -> l
          | None ->
              error "Cannot find label for \\at@.";
              assert false

          in
          fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]"
            self#pPredicate_named p l
        end
    | Pat(p,LogicLabel s) ->
	fprintf fmt "@[\\at(@[@[%a@],@,%s@])@]"
          self#pPredicate_named p s
    | Pfresh e -> fprintf fmt "@[\\fresh(@[%a@])@]" self#pTerm e
    | Pvalid_index (e1,e2) ->
        fprintf fmt "@[\\valid_index(@[@[%a@],@,@[%a@]@])@]"
          self#pTerm e1 self#pTerm e2
    | Pvalid_range (e1,e2,e3) ->
        fprintf fmt "@[\\valid_range(@[@[%a@],@,@[%a@],@,@[%a@]@])@]"
          self#pTerm e1 self#pTerm e2 self#pTerm e3
    | Psubtype (e,ce) ->
	fprintf fmt "%a <: %a" term e term ce

  method pPredicate_named fmt p =
    match p.name with
      [] -> self#pPredicate fmt p.content
    | _ ->
        fprintf fmt "(@[%a:@ %a@])"
          (pretty_list (swap fprintf ":@ ") pp_print_string) p.name
          self#pPredicate p.content

(*
  method pPredicate_info_use fmt pi = self#pVarName fmt pi.p_name
*)

  method private preds kw fmt l =
    pretty_list_del ignore nl_sep nl_sep
      (fun fmt p ->
         fprintf fmt "@[%s @[%a@];@]" kw
           self#pPredicate_named
           ({name = p.ip_name; loc = p.ip_loc; content = p.ip_content})) fmt l

  method private pDecrement kw fmt (t, rel) =
    match rel with
      None -> fprintf fmt "@[<2>%s@ %a;@]@\n" kw self#pTerm t
    | Some str ->
        (*TODO: replace this string with an interpreted variable*)
        fprintf fmt "@[<2>%s@ %a@ for@ %s;@]@\n" kw self#pTerm t str

  method pBehavior fmt b =
    fprintf fmt "@[behavior %s:@\n  @[%a%a%a@]@]"
      b.b_name (self#preds "assumes") b.b_assumes
      (self#preds "ensures") b.b_ensures
      (pretty_list (fun _ -> ()) (self#pAssigns "assigns")) b.b_assigns

  method pSpec fmt { spec_requires = requires;
                     spec_behavior = behaviors;
                     spec_variant = variant;
                     spec_terminates = terminates;
                     spec_complete_behaviors = complete;
                     spec_disjoint_behaviors = disjoint;
                   } =
    fprintf fmt "@[%a%a%a%a%a%a@]"
      (self#preds "requires") requires
      (pretty_opt
         (fun fmt p -> fprintf fmt "@[<2>terminates@ %a;@]@\n"
            self#pPredicate_named {name = p.ip_name; loc = p.ip_loc;
                                   content = p.ip_content})) terminates
      (pretty_list nl_sep self#pBehavior) behaviors
      (pretty_list_del nl_sep nl_sep nl_sep
         (pretty_list_del
            (space_sep "complete behaviors")
            (space_sep ";")
            (space_sep ",")
            Format.pp_print_string)) complete
      (pretty_list_del nl_sep nl_sep nl_sep
         (pretty_list_del
            (space_sep "disjoint behaviors")
            (space_sep ";")
            (space_sep ",")
            Format.pp_print_string)) disjoint
      (pretty_opt (self#pDecrement "decreases")) variant

  method private pAssigns kw fmt (base,deps) =
    fprintf fmt "@[%s@ %a%a;@]@\n" kw self#pZone base
      (pretty_list_del (swap fprintf "@ \\from@ ") (fun _ -> ())
         (space_sep ",") self#pZone) deps

  method pZone fmt locs =
    match locs with
    | Nothing ->
        pp_print_string fmt "\\nothing"
    | Location loc -> self#pTsets fmt loc.its_content

  method private pLoop_pragma fmt = function
      | Widen_hints terms -> fprintf fmt "WIDEN_HINTS @[%a@]"
          (pretty_list_del
             (fun _ -> ()) (fun _ -> ())
             (space_sep ",") self#pTerm) terms
      | Widen_variables terms -> fprintf fmt "WIDEN_VARIABLES @[%a@]"
          (pretty_list_del
             (fun _ -> ()) (fun _ -> ())
             (space_sep ",") self#pTerm) terms
      | Unroll_level t -> fprintf fmt "UNROLL @[%a@]" self#pTerm t

  method private pSlice_pragma fmt = function
      SPexpr t ->
        fprintf fmt "expr @[%a@]" self#pTerm t
    | SPctrl -> pp_print_string fmt "ctrl"
    | SPstmt -> pp_print_string fmt "stmt"

  method private pImpact_pragma fmt = function
  | IPexpr t -> fprintf fmt "expr @[%a@]" self#pTerm t
  | IPstmt -> pp_print_string fmt "stmt"

  method pStatus fmt s =
    match s.status with
    | Unknown -> fprintf fmt "No proof attempted@\n"
    | Checked {emitter=s; valid=True} ->
        fprintf fmt "Valid according to %s@\n" s
    | Checked {emitter=s; valid=False} ->
        fprintf fmt "NOT valid according to %s@\n" s
    | Checked {emitter=s; valid=Maybe} ->
        fprintf fmt "Unknown (%s could not decide the status for this property)@\n" s

  (* TODO: add the annot ID in debug mode?*)
  method pCode_annot fmt ca =
    match ca.annot_content with
    | AAssert (behav,p,_status) -> fprintf fmt "@[%aassert@ %a;@]@\n"
	(pretty_list_del
	   (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	   (space_sep ",") pp_print_string)
	  behav
          self#pPredicate_named p
    | AAssume p -> fprintf fmt "@[assume@ %a;@]@\n"
        self#pPredicate_named p
    | APragma (Slice_pragma sp) ->
        fprintf fmt "@[slice pragma@ %a;@]@\n" self#pSlice_pragma sp
    | APragma (Impact_pragma sp) ->
        fprintf fmt "@[impact pragma@ %a;@]@\n" self#pImpact_pragma sp
    | APragma (Loop_pragma lp) ->
        fprintf fmt "@[loop pragma@ %a;@]@\n" self#pLoop_pragma lp
    | AStmtSpec sp -> self#pSpec fmt sp
    | AAssigns a -> self#pAssigns "loop assigns" fmt a
    | AInvariant(behav,true, i) -> fprintf fmt "@[<2>%aloop invariant@ %a;@]@\n"
	(pretty_list_del
	   (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	   (space_sep ",") pp_print_string)
	  behav
          self#pPredicate_named i
    | AInvariant(behav,false,i) -> fprintf fmt "@[<2>%ainvariant@ %a;@]@\n"
	(pretty_list_del
	   (fun fmt -> fprintf fmt "for ") (fun fmt -> fprintf fmt ": ")
	   (space_sep ",") pp_print_string)
	  behav
          self#pPredicate_named i
    | AVariant v -> self#pDecrement "loop variant" fmt v

  method private pLogicPrms fmt arg =
    fprintf fmt "%a@ %a" self#pLogic_type arg.lv_type self#pLogic_var arg

  method private pTypeParameters fmt tvars =
    pretty_list_del
      (fun fmt -> fprintf fmt "<@[") (fun fmt -> fprintf fmt "@]>")
      (space_sep ",") pp_print_string fmt tvars

  method private pLogicLabel fmt lab =
    let s =
      match lab with
	| LogicLabel s -> s
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
    pretty_list_del
      (fun fmt -> fprintf fmt "{@[") (fun fmt -> fprintf fmt "@]}")
      (space_sep ",") self#pLogicLabel fmt labels

  method pAnnotation fmt = function
    | Dtype_annot a ->
        fprintf fmt "@[type invariant @[%a%a=@ %a@,;@]@]@\n"
          self#pVarName a.l_name
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",") self#pLogicPrms) a.l_profile
          self#pPredicate_named (pred_body a.l_body)
    | Dinvariant pred ->
        fprintf fmt "@[global@ invariant %a:@[@ %a;@]@]@\n"
          self#pVarName pred.l_name
          self#pPredicate_named (pred_body pred.l_body)
    | Dlemma(name, is_axiom, labels, tvars, pred) ->
        fprintf fmt "@[%s@ %a%a%a:@[@ %a;@]@]@\n"
	  (if is_axiom then "axiom" else "lemma")
          self#pVarName name
	  self#pLabels labels
	  self#pTypeParameters tvars
          self#pPredicate_named pred
    | Dtype(name,prms) ->
        fprintf fmt "@[logic@ type@ %a%a;@]@\n"
          self#pVarName name self#pTypeParameters prms
    | Dfun_or_pred li ->
	begin
	  match li.l_type with
	    | Some rt ->
		fprintf fmt "@[<hov 2>logic %a"
		  self#pLogic_type rt
	    | None ->
		fprintf fmt "@[<hov 2>predicate"
	end;
	fprintf fmt " %a%a%a%a"
	  self#pVarName li.l_name
	  self#pLabels li.l_labels
	  self#pTypeParameters li.l_tparams
	  (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",") self#pLogicPrms) li.l_profile;
	begin
	  match li.l_body with
	    | LBreads reads ->
		fprintf fmt "%a;"
		  (pretty_list_del
		     (fun fmt -> Format.fprintf fmt "@\n@[reads@ ")
		     (fun fmt -> Format.fprintf fmt "@]")
		     (space_sep ",") self#pTsets) reads
	    | LBpred def ->
		fprintf fmt "=@ %a;"
		  self#pPredicate_named def
(*
	    | LBaxiomatic axioms ->
		fprintf fmt "{@ %a}"
		  (pretty_list_del
		     (fun fmt -> Format.fprintf fmt "@[<v 0>")
		     (fun fmt -> Format.fprintf fmt "@]@\n")
		     nl_sep
		     (fun fmt (id,p) ->
			Format.fprintf fmt "axiom %s: %a;" id
			  self#pPredicate_named p)) axioms
*)
	    | LBinductive indcases ->
		fprintf fmt "{@ %a}"
		  (pretty_list_del
		     (fun fmt -> Format.fprintf fmt "@[<v 0>")
		     (fun fmt -> Format.fprintf fmt "@]@\n")
		     nl_sep
		     (fun fmt (id,labels,tvars,p) ->
			Format.fprintf fmt "case %s%a%a: %a;" id
			  self#pLabels labels
			  self#pTypeParameters tvars
			  self#pPredicate_named p)) indcases
	    | LBterm def ->
		fprintf fmt "=@ %a;"
		  self#pTerm def
	end;
	fprintf fmt "@]@\n"
    | Daxiomatic(id,decls) ->
(*
	Format.eprintf "cil.pAnnotation on axiomatic %s@." id;
*)
	fprintf fmt "@[<v 2>axiomatic@ %s {@\n%a@]}@]@\n" id
	  (pretty_list_del
	     (fun fmt -> Format.fprintf fmt "@[<v 0>")
	     (fun fmt -> Format.fprintf fmt "@]@\n")
	     nl_sep
	     self#pAnnotation)
	  decls


(*
    | Dlogic_reads (name, tvars, args, rt, reads) ->
        fprintf fmt "@[logic %a %a%a%a%a%a;@]@\n"
          self#pLogic_type rt self#pVarName name.l_name
	  self#pLabels name.l_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",") self#pLogicPrms) args
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[reads@ ")
             (fun fmt -> Format.fprintf fmt "@]")
             (space_sep ",") self#pTsets) reads
    | Dlogic_def (name, tvars, args, rt, def) ->
        fprintf fmt "@[logic@[ %a %a%a%a%a=@ %a;@]@]@\n"
          self#pLogic_type rt self#pVarName name.l_name
	  self#pLabels name.l_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",")
             self#pLogicPrms) args
          self#pTerm def
    | Dlogic_axiomatic (name, tvars, args, rt, axioms) ->
        fprintf fmt "@[logic@[ %a %a%a%a%a{@ %a}@]@]@\n"
          self#pLogic_type rt self#pVarName name.l_name
	  self#pLabels name.l_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",")
             self#pLogicPrms) args
          (pretty_list_del
	     (fun fmt -> Format.fprintf fmt "@[<v 0>")
             (fun fmt -> Format.fprintf fmt "@]@\n")
             nl_sep
             (fun fmt (id,p) ->
		Format.fprintf fmt "axiom %s: %a;" id
		self#pPredicate_named p)) axioms
    | Dinductive_def (name, tvars, args, indcases) ->
        fprintf fmt "@[predicate@[ %a%a%a%a{@ %a}@]@]@\n"
          self#pVarName name.p_name
	  self#pLabels name.p_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",")
             self#pLogicPrms) args
          (pretty_list_del
	     (fun fmt -> Format.fprintf fmt "@[<v 0>")
             (fun fmt -> Format.fprintf fmt "@]@\n")
             nl_sep
             (fun fmt (id,p) ->
		Format.fprintf fmt "case %s: %a;" id
		self#pPredicate_named p)) indcases
    | Dpredicate_def (info, tvars, args, def) ->
        fprintf fmt "@[predicate@[ %a%a%a%a=@ %a;@]@]@\n"
          self#pVarName info.p_name
	  self#pLabels info.p_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]@ ")
             (space_sep ",") self#pLogicPrms) args
          self#pPredicate_named def
    | Dpredicate_reads (info, tvars, args, reads) ->
        fprintf fmt "@[predicate %a%a%a%a%a;@]@\n"
          self#pVarName info.p_name
	  self#pLabels info.p_labels
	  self#pTypeParameters tvars
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@[(")
             (fun fmt -> Format.fprintf fmt ")@]")
             (space_sep ",") self#pLogicPrms) args
          (pretty_list_del
             (fun fmt -> Format.fprintf fmt "@\n  @[reads@ ")
             (fun fmt -> Format.fprintf fmt "@]")
             (space_sep ",") self#pTsets) reads
*)
end (* class defaultCilPrinterClass *)

let defaultCilPrinter = new defaultCilPrinterClass

(* Top-level printing functions *)
let printType (pp: cilPrinter) fmt (t: typ) =
  pp#pType None fmt t

let printExp (pp: cilPrinter) fmt (e: exp) =
  pp#pExp fmt e

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
  (* We must add the alignment ourselves, beucase pBlock will pop it *)
  fprintf fmt "@[%a" (pp#pBlock ~toplevel:true) b

let printInit (pp: cilPrinter) fmt (i: init) =
  pp#pInit fmt i

let printTerm_lval pp fmt lv = pp#pTerm_lval fmt lv

let printLogic_var pp fmt lv = pp#pLogic_var fmt lv

let printLogic_type pp fmt lv = pp#pLogic_type fmt lv

let printTerm pp fmt t = pp#pTerm fmt t

let printTerm_offset pp fmt o = pp#pTerm_offset fmt o

let printTsets pp fmt o = pp#pTsets fmt o

let printTsets_elem pp fmt o = pp#pTsets_elem fmt o

let printTsets_lhost pp fmt o = pp#pTsets_lhost fmt o

let printTsets_offset pp fmt o = pp#pTsets_offset fmt o

let printTsets_lval pp fmt o = pp#pTsets_lval fmt o

let printPredicate_named pp fmt p = pp#pPredicate_named fmt p

let printCode_annotation pp fmt ca = pp#pCode_annot fmt ca
let printStatus pp fmt s = pp#pStatus fmt s

let printFunspec pp fmt s = pp#pSpec fmt s

let printAnnotation pp fmt a = pp#pAnnotation fmt a

(* Now define some short cuts *)
let d_exp fmt e = printExp defaultCilPrinter fmt e
let _ = pd_exp := d_exp
let d_lval fmt lv = printLval defaultCilPrinter fmt lv
let d_offset fmt off = defaultCilPrinter#pOffset fmt off
let d_init fmt i = printInit defaultCilPrinter fmt i
let d_type fmt t = printType defaultCilPrinter fmt t
let d_global fmt g = printGlobal defaultCilPrinter fmt g
let _ = pd_global := d_global
let d_attrlist fmt a = printAttrs defaultCilPrinter fmt a
let d_attr fmt a = printAttr defaultCilPrinter fmt a
let d_attrparam fmt e = defaultCilPrinter#pAttrParam fmt e
let d_label fmt l = defaultCilPrinter#pLabel fmt l
let d_stmt fmt s = printStmt defaultCilPrinter fmt s
let d_block fmt b = printBlock defaultCilPrinter fmt b
let d_instr fmt i = printInstr defaultCilPrinter fmt i

let d_term_lval fmt lv = printTerm_lval defaultCilPrinter fmt lv
let d_logic_var fmt lv = printLogic_var defaultCilPrinter fmt lv
let d_logic_type fmt lv = printLogic_type defaultCilPrinter fmt lv
let d_term fmt lv = printTerm defaultCilPrinter fmt lv
let d_term_offset fmt lv = printTerm_offset defaultCilPrinter fmt lv
let d_tsets fmt lv = printTsets defaultCilPrinter fmt lv
let d_tsets_elem fmt lv = printTsets_elem defaultCilPrinter fmt lv
let d_tsets_lhost fmt lv = printTsets_lhost defaultCilPrinter fmt lv
let d_tsets_lval fmt lv = printTsets_lval defaultCilPrinter fmt lv
let d_tsets_offset fmt lv = printTsets_offset defaultCilPrinter fmt lv

let d_status fmt s = printStatus defaultCilPrinter fmt s
let d_predicate_named fmt lv = printPredicate_named defaultCilPrinter fmt lv
let d_code_annotation fmt lv = printCode_annotation defaultCilPrinter fmt lv
let d_funspec fmt lv = printFunspec defaultCilPrinter fmt lv
let d_annotation fmt lv = printAnnotation defaultCilPrinter fmt lv

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


(*
(* Now define a cilPlainPrinter *)
class plainCilPrinterClass =
  (* We keep track of the composite types that we have done to avoid
   * recursion *)
  let donecomps : (int, unit) H.t = H.create 13 in
  object (self)

  inherit defaultCilPrinterClass as super

  (*** PLAIN TYPES ***)
  method pType (dn: doc option) () (t: typ) =
    match dn with
      None -> self#pOnlyType () t
    | Some d -> d ++ text " : " ++ self#pOnlyType () t

 method private pOnlyType () = function
     TVoid a -> dprintf "TVoid(@[%a@])" self#pAttrs a
   | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])"
         d_ikind ikind self#pAttrs a
   | TFloat(fkind, a) ->
       dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind self#pAttrs a
   | TNamed (t, a) ->
       dprintf "TNamed(@[%a,@?%a,@?%a@])"
         self#pVarName t.tname self#pOnlyType t.ttype self#pAttrs a
   | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" self#pOnlyType t self#pAttrs a
   | TArray(t,l,a) ->
       let dl = match l with
         None -> text "None" | Some l -> dprintf "Some(@[%a@])" self#pExp l in
       dprintf "TArray(@[%a,@?%a,@?%a@])"
         self#pOnlyType t insert dl self#pAttrs a
   | TEnum(enum,a) -> dprintf "Enum(%a,@[%a@])"
       self#pVarName enum.ename self#pAttrs a
   | TFun(tr,args,isva,a) ->
       dprintf "TFun(@[%a,@?%a%s,@?%a@])"
         self#pOnlyType tr
         insert
         (if args = None then text "None"
         else (docList ~sep:(chr ',' ++ break)
                 (fun (an,at,aa) ->
                   dprintf "%s: %a" an self#pOnlyType at))
             ()
             (argsToList args))
         (if isva then "..." else "") self#pAttrs a
   | TComp (comp, a) ->
       if H.mem donecomps comp.ckey then
         dprintf "TCompLoop(%s %a, _, %a)"
           (if comp.cstruct then "struct" else "union")
           self#pVarName comp.cname self#pAttrs comp.cattr
       else begin
         H.add donecomps comp.ckey (); (* Add it before we do the fields *)
         dprintf "TComp(@[%s %a,@?%a,@?%a,@?%a@])"
           (if comp.cstruct then "struct" else "union")
           self#pVarName comp.cname
           (docList ~sep:(chr ',' ++ break)
              (fun f -> dprintf "%a : %a"
                 self#pVarName f.fname self#pOnlyType f.ftype))
           comp.cfields
           self#pAttrs comp.cattr
           self#pAttrs a
       end
   | TBuiltin_va_list a ->
       dprintf "TBuiltin_va_list(%a)" self#pAttrs a


  (* Some plain pretty-printers. Unlike the above these expose all the
   * details of the internal representation *)
  method pExp () = function
    Const(c) ->
      let d_plainconst () c =
        match c with
          CInt64(i, ik, so) ->
            dprintf "Int64(%s,%a,%s)"
              (Int64.format "%d" i)
              d_ikind ik
              (match so with Some s -> s | _ -> "None")
        | CStr(s) ->
            text ("CStr(\"" ^ escape_string s ^ "\")")
        | CWStr(s) ->
            dprintf "CWStr(%a)" d_const c

        | CChr(c) -> text ("CChr('" ^ escape_char c ^ "')")
        | CReal(f, fk, so) ->
            dprintf "CReal(%f, %a, %s)"
              f
              d_fkind fk
              (match so with Some s -> s | _ -> "None")
        | CEnum(_, s, _) -> text s
      in
      text "Const(" ++ d_plainconst () c ++ text ")"


  | Lval(lv) ->
      text "Lval("
        ++ (align
              ++ self#pLval () lv
              ++ unalign)
        ++ text ")"

  | CastE(t,e) -> dprintf "CastE(@[%a,@?%a@])" self#pOnlyType t self#pExp e

  | UnOp(u,e1,_) ->
      dprintf "UnOp(@[%a,@?%a@])"
        d_unop u self#pExp e1

  | BinOp(b,e1,e2,_) ->
      let d_plainbinop () b =
        match b with
          PlusA -> text "PlusA"
        | PlusPI -> text "PlusPI"
        | IndexPI -> text "IndexPI"
        | MinusA -> text "MinusA"
        | MinusPP -> text "MinusPP"
        | MinusPI -> text "MinusPI"
        | _ -> d_binop () b
      in
      dprintf "%a(@[%a,@?%a@])" d_plainbinop b
        self#pExp e1 self#pExp e2

  | SizeOf (t) ->
      text "sizeof(" ++ self#pType None () t ++ chr ')'
  | SizeOfE (e) ->
      text "sizeofE(" ++ self#pExp () e ++ chr ')'
  | SizeOfStr (s) ->
      text "sizeofStr(" ++ d_const () (CStr s) ++ chr ')'
  | AlignOf (t) ->
      text "__alignof__(" ++ self#pType None () t ++ chr ')'
  | AlignOfE (e) ->
      text "__alignof__(" ++ self#pExp () e ++ chr ')'

  | StartOf lv -> dprintf "StartOf(%a)" self#pLval lv
  | AddrOf (lv) -> dprintf "AddrOf(%a)" self#pLval lv



  method private d_plainoffset () = function
      NoOffset -> text "NoOffset"
    | Field(fi,o) ->
        dprintf "Field(@[%a:%a,@?%a@])"
          self#pVarName fi.fname self#pOnlyType fi.ftype self#d_plainoffset o
     | Index(e, o) ->
         dprintf "Index(@[%a,@?%a@])" self#pExp e self#d_plainoffset o

  method pInit () = function
      SingleInit e -> dprintf "SI(%a)" d_exp e
    | CompoundInit (t, initl) ->
        let d_plainoneinit (o, i) =
          self#d_plainoffset () o ++ text " = " ++ self#pInit () i
        in
        dprintf "CI(@[%a,@?%a@])" self#pOnlyType t
          (docList ~sep:(chr ',' ++ break) d_plainoneinit) initl
(*
    | ArrayInit (t, len, initl) ->
        let idx = ref (- 1) in
        let d_plainoneinit i =
          incr idx;
          text "[" ++ num !idx ++ text "] = " ++ self#pInit () i
        in
        dprintf "AI(@[%a,%d,@?%a@])" self#pOnlyType t len
          (docList ~sep:(chr ',' ++ break) d_plainoneinit) initl
*)
  method pLval () (lv: lval) =
    match lv with
    | Var vi, o -> dprintf "Var(@[%a,@?%a@])"
        self#pVarName vi.vname self#d_plainoffset o
    | Mem e, o -> dprintf "Mem(@[%a,@?%a@])" self#pExp e self#d_plainoffset o


end
let plainCilPrinter = new plainCilPrinterClass
*)
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
        (fprintfList ~sep:"\n  "
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
    match e with
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

(*
(* zra: this allows pretty printers not in cil.ml to
   be exposed to cilmain.ml *)
let printerForMaincil = ref defaultCilPrinter

let rec d_typsig () = function
    TSArray (ts, eo, al) ->
      dprintf "TSArray(@[%a,@?%a,@?%a@])"
        d_typsig ts
        insert (text (match eo with None -> "None"
                       | Some e -> "Some " ^ Int64.to_string e))
        d_attrlist al
  | TSPtr (ts, al) ->
      dprintf "TSPtr(@[%a,@?%a@])"
        d_typsig ts d_attrlist al
  | TSComp (iss, name, al) ->
      dprintf "TSComp(@[%s %s,@?%a@])"
        (if iss then "struct" else "union") name
        d_attrlist al
  | TSFun (rt, args, isva, al) ->
      dprintf "TSFun(@[%a,@?%a,%b,@?%a@])"
        d_typsig rt
        (docList ~sep:(chr ',' ++ break) (d_typsig ())) args isva
        d_attrlist al
  | TSEnum (n, al) ->
      dprintf "TSEnum(@[%s,@?%a@])"
        n d_attrlist al
  | TSBase t -> dprintf "TSBase(%a)" d_type t
*)

(* Make a varinfo. Used mostly as a helper function below  *)
let makeVarinfo ?(logic=false) global formal name typ =
  (* Strip const from type for locals *)
  let vi =
    { vorig_name = name;
      vname = name;
      vid   = -1;
      vglob = global;
      vdefined = false;
      vformal = formal;
      vtype = if formal || global then typ
      else typeRemoveAttributes ["const"] typ;
      vdecl = locUnknown;
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

let cvar_to_lvar vi =
  (*Format.printf "Converting cvar %s(%d) to logic var@." vi.vname vi.vid;*)
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

let makeLocal ?(formal=false) fdec name typ = (* a helper function *)
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = makeVarinfo false formal name typ in
  vi

(* Make a local variable and add it to a function *)
let makeLocalVar fdec ?(insert = true) name typ =
  let vi = makeLocal fdec name typ in
  if insert then fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec ?(name = "__cil_tmp") ?descr ?(descrpure = true)
                typ : varinfo =
  let rec findUniqueName () : string=
    let n = name ^ (string_of_int (1 + fdec.smaxid)) in
    (* Is this check a performance problem?  We could bring the old
       unchecked makeTempVar back as a separate function that assumes
       the prefix name does not occur in the original program. *)
    if (List.exists (fun vi -> vi.vname = n) fdec.slocals)
      || (List.exists (fun vi -> vi.vname = n) fdec.sformals) then begin
        fdec.smaxid <- 1 + fdec.smaxid;
        findUniqueName ()
      end else
        n
  in
  let name = findUniqueName () in
  let vi = makeLocalVar fdec name typ in
  vi.vdescr <- descr;
  vi.vdescrpure <- descrpure;
  vi

let makePseudoVar =
  let counter = ref 0 in
  function ty ->
    incr counter;
    let name = "@" ^ (string_of_int !counter) in
    makeVarinfo ~logic:true (* global= *)false (* formal= *)false name ty

(* Set the formals and re-create the function name based on the information*)
let setFormals (f: fundec) (forms: varinfo list) =
  List.iter (fun v -> v.vformal <- true) forms;
  f.sformals <- forms; (* Set the formals *)
  match unrollType f.svar.vtype with
    TFun(rt, _, isva, fa) ->
      f.svar.vtype <-
         TFun(rt,
              Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) forms),
              isva, fa)
  | _ -> E.s (E.bug "Set formals. %s does not have function type\n"
                f.svar.vname)

   (* Set the types of arguments and results as given by the function type
    * passed as the second argument *)
let setFunctionType (f: fundec) (t: typ) =
  match unrollType t with
    TFun (_rt, Some args, _va, _a) ->
      if List.length f.sformals <> List.length args then
        E.s (E.bug "setFunctionType: number of arguments differs from the number of formals");
      (* Change the function type. *)
      f.svar.vtype <- t;
      (* Change the sformals and we know that indirectly we'll change the
       * function type *)
      List.iter2
        (fun (_an,at,aa) f ->
          f.vtype <- at; f.vattr <- aa)
        args f.sformals

  | _ -> E.s (E.bug "setFunctionType: not a function type")


   (* Set the types of arguments and results as given by the function type
    * passed as the second argument *)
let setFunctionTypeMakeFormals (f: fundec) (t: typ) =
  match unrollType t with
    TFun (_rt, Some args, _va, _a) ->
      if f.sformals <> [] then
        E.s (E.warn "setFunctionTypMakeFormals called on function %s with some formals already"
               f.svar.vname);
      (* Change the function type. *)
      f.svar.vtype <- t;
      f.sformals <- [];

      f.sformals <- List.map (fun (n,t,_a) -> makeLocal ~formal:true f n t) args;

      setFunctionType f t

  | _ -> E.s (bug "setFunctionTypeMakeFormals: not a function type: %a"
             d_type t)


let setMaxId (f: fundec) =
  f.smaxid <- List.length f.sformals + List.length f.slocals


  (* Make a formal variable for a function. Insert it in both the sformals
   * and the type of the function. You can optionally specify where to insert
   * this one. If where = "^" then it is inserted first. If where = "$" then
   * it is inserted last. Otherwise where must be the name of a formal after
   * which to insert this. By default it is inserted at the end. *)
let makeFormalVar fdec ?(where = "$") name typ : varinfo =
  (* Search for the insertion place *)
  let thenewone = ref fdec.svar in (* Just a placeholder *)
  let makeit () : varinfo =
    let vi = makeLocal ~formal:true fdec name typ in
    thenewone := vi;
    vi
  in
  let rec loopFormals = function
      [] ->
        if where = "$" then [makeit ()]
        else E.s (E.error "makeFormalVar: cannot find insert-after formal %s"
                    where)
    | f :: rest when f.vname = where -> f :: makeit () :: rest
    | f :: rest -> f :: loopFormals rest
  in
  let newformals =
    if where = "^" then makeit () :: fdec.sformals else
    loopFormals fdec.sformals in
  setFormals fdec newformals;
  !thenewone

   (* Make a global variable. Your responsibility to make sure that the name
    * is unique *)
let makeGlobalVar ?logic name typ =
  let vi = makeVarinfo ?logic true false name typ in
  vi

module FormalsDecl =
  Computation.Make_Hashtbl
    (Cilutil.VarinfoHashtbl)
    (Datatype.List(Cil_datatype.Varinfo))
    (struct
       let name = "FormalsDecl"
       let dependencies = [] (* depends on file in Frama-C kernel *)
       let size = 47
     end)

let selfFormalsDecl = FormalsDecl.self

let makeFormalsVarDecl (n,t,a) =
  let vi = makeVarinfo false true n t in
  vi.vattr <- a;
  vi

let setFormalsDecl vi typ =
  match unrollType typ with
  | TFun(_, Some args, _, _) ->
      FormalsDecl.replace vi (List.map makeFormalsVarDecl args)
  | TFun(_,None,_,_) -> ()
  | _ -> E.bug
      "trying to assigns formal parameters to an object which is \
       not a function prototype"

let getFormalsDecl vi = FormalsDecl.find vi

let unsafeSetFormalsDecl vi args = FormalsDecl.replace vi args


(* Make an empty function *)
let emptyFunction name =
  let r = { svar  = makeGlobalVar name (TFun(voidType, Some [], false,[]));
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

let rec lastTermOffset (off: term_offset) : term_offset =
  match off with
  | TNoOffset | TField(_,TNoOffset) | TIndex(_,TNoOffset) -> off
  | TField(_,off) | TIndex(_,off) -> lastTermOffset off

let rec lastTsetsOffset off =
  match off with
  | TSNoOffset | TSField(_,TSNoOffset) | TSIndex(_,TSNoOffset)
  | TSRange(_,_,TSNoOffset) -> off
  | TSField(_,off) | TSIndex(_,off) | TSRange(_,_,off) -> lastTsetsOffset off

let rec addOffset (toadd: offset) (off: offset) : offset =
  match off with
    NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

let rec addTermOffset (toadd: term_offset) (off: term_offset) : term_offset =
  match off with
    TNoOffset -> toadd
  | TField(fid', offset) -> TField(fid', addTermOffset toadd offset)
  | TIndex(t, offset) -> TIndex(t, addTermOffset toadd offset)

let rec addTsetsOffset toadd off =
  match off with
      TSNoOffset -> toadd
    | TSField(fid, offset) -> TSField(fid, addTsetsOffset toadd offset)
    | TSIndex(t,offset) -> TSIndex(t,addTsetsOffset toadd offset)
    | TSRange(t1,t2,offset) -> TSRange(t1,t2,addTsetsOffset toadd offset)

 (* Add an offset at the end of an lv *)
let addOffsetLval toadd (b, off) : lval =
 b, addOffset toadd off

let addTermOffsetLval toadd (b, off) : term_lval =
 b, addTermOffset toadd off

let addTsetsOffsetLval toadd (b, off) : tsets_lval =
 b, addTsetsOffset toadd off

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


(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let doVisit (vis: 'visitor)
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
  | _ -> (* DoChildren and ChangeDoChildrenPost *)
      let nodepre = match action with
        ChangeDoChildrenPost (node', _) -> node'
      | _ -> node'
      in
      let nodepost = children vis nodepre in
      match action with
        ChangeDoChildrenPost (_, f) -> f nodepost
      | _ -> nodepost

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
      let nodespost = mapNoCopy (children vis) nodespre in
      match action with
        ChangeDoChildrenPost (_, f) -> f nodespost
      | _ -> nodespost


let opt_map f o =
  match o with
      None -> o
    | Some x ->
        let x' = f x in if x' != x then Some x' else o
let opt_bind f =
  function
      None -> None
    | Some x as o ->
        match f x with
            None -> None
          | Some x' as o' -> if x != x' then o else o'

let doVisitOption (vis: #cilVisitor as 'visit)
                  (previsit: 'a -> 'a)
                  (startvisit: 'a -> 'a option visitAction)
                  (children: 'visit -> 'a -> 'a)
                  (node: 'a) : 'a option =
  let node' = previsit node in
  let action = startvisit node' in
  match action with
      SkipChildren -> Some node'
    | ChangeTo node' -> node'
    | ChangeToPost (node',f) -> f node'
    | _ ->
        let nodepre = match action with
            ChangeDoChildrenPost(nodepre,_) -> nodepre
          | _ -> Some node'
        in let nodepost = opt_map (children vis) nodepre in
        match action with
            ChangeDoChildrenPost(_,f) -> f nodepost
          | _ -> nodepost

let debugVisit = false

let rec visitCilTerm vis t =
  doVisit vis (fun x-> x) vis#vterm childrenTerm t
and childrenTerm vis t =
  let tn' = visitCilTermNode vis t.term_node in
  let tt' = visitCilLogicType vis t.term_type in
    if tn' != t.term_node || tt' != t.term_type then
      { t with term_node = tn'; term_type = tt' }
    else t
and visitCilTermNode vis tn =
  doVisit vis (fun x -> x) vis#vterm_node childrenTermNode tn
and childrenTermNode vis tn =
  let vTerm t = visitCilTerm vis t in
  let vTermLval tl = visitCilTermLval vis tl in
  let vTyp t = visitCilType vis t in
  let vLogicInfo li =
    doVisit vis vis#behavior.get_logic_info
      vis#vlogic_info_use (fun _ x ->x) li
  in
  match tn with
    | TConst _ -> tn (*enum constants are visited at their declaration site*)
    | TDataCons (ci,args) ->
        let args' = mapNoCopy vTerm args in
        if args != args' then TDataCons(ci,args') else tn
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
        let args' = mapNoCopy vTerm args in
          if li' != li || args' != args then Tapp(li',labels,args') else tn
    | Tif(test,ttrue,tfalse) ->
        let test' = vTerm test in
        let ttrue' = vTerm ttrue in
        let tfalse' = vTerm tfalse in
          if test' != test || ttrue' != ttrue || tfalse' != tfalse then
            Tif(test',ttrue',tfalse')
          else tn
    | Told t ->
        let t' = vTerm t in if t' != t then Told t' else tn
    | Tat(t,s) ->
        let t' = vTerm t in
        let s' = visitCilLogicLabel vis s in
        if t' != t || s' != s then Tat (t',s') else tn
    | Tbase_addr t ->
        let t' = vTerm t in if t' != t then Tbase_addr t' else tn
    | Tblock_length t ->
        let t' = vTerm t in if t' != t then Tblock_length t' else tn
    | Tnull -> tn
    | TCoerce(te,ty) ->
        let ty' = vTyp ty in
        let te' = vTerm te in
        if ty' != ty || te' != te then TCoerce(te',ty') else tn
    | TCoerceE(te,tc) ->
        let tc' = vTerm tc in
        let te' = vTerm te in
        if tc' != tc || te' != te then TCoerceE(te',tc') else tn
    | TUpdate (tc,f,te) ->
        let tc' = vTerm tc in
        let te' = vTerm te in
        if tc' != tc || te' != te then TUpdate(tc',f,te') else tn
    | Tlambda(prms,te) ->
        let prms' = visitCilQuantifiers vis prms in
        let te' = vTerm te in
        if prms' != prms || te' != te then Tlambda(prms',te') else tn
    | Ttypeof t ->
        let t' = vTerm t in if t' != t then Ttypeof t' else tn
    | Ttype ty ->
        let ty' = vTyp ty in if ty' != ty then Ttype ty' else tn
    | Ttsets ts ->
        let ts' = visitCilTsets vis ts in if ts != ts' then Ttsets ts' else tn

and visitCilLogicLabel vis l =
  match l with
      StmtLabel s -> s := vis#behavior.get_stmt !s; l
    | LogicLabel _ -> l

and visitCilTermLval vis tl =
  doVisit vis (fun x -> x) vis#vterm_lval childrenTermLval tl

and childrenTermLval vis ((tlv,toff) as tl)=
  let tlv' = visitCilTermLhost vis tlv in
  let toff' = visitCilTermOffset vis toff in
    if tlv' != tlv || toff' != toff then (tlv',toff') else tl

and visitCilTermLhost vis tl =
  doVisit vis (fun x -> x) vis#vterm_lhost childrenTermLhost tl

and childrenTermLhost vis tl = match tl with
    TVar v ->
      let v' = visitCilLogicVarUse vis v in if v' != v then TVar v' else tl
  | TResult -> tl
  | TMem t ->
      let t' = visitCilTerm vis t in if t' != t then TMem t' else tl

and visitCilTermOffset vis toff =
    doVisit vis (fun x -> x)
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

and visitCilTsets vis loc =
  doVisit vis (fun x -> x) vis#vtsets childrenTsets loc
and childrenTsets vis loc =
  match loc with
      TSSingleton lval ->
        let lval' = visitCilTsetsElem vis lval in
        if lval' != lval then TSSingleton lval' else loc
    | TSUnion locs ->
        let locs' = mapNoCopy (visitCilTsets vis) locs in
        if locs != locs' then TSUnion(locs') else loc
    | TSInter locs ->
        let locs' = mapNoCopy (visitCilTsets vis) locs in
        if locs != locs' then TSInter(locs') else loc
    | TSComprehension(lval,quant,pred) ->
        let quant' = visitCilQuantifiers vis quant in
        let lval' = visitCilTsets vis lval in
        let pred' = (opt_map (visitCilPredicateNamed vis)) pred in
        if lval' != lval || quant' != quant || pred' != pred
        then
          TSComprehension(lval',quant',pred')
        else
          loc
    | TSEmpty -> loc

and visitCilTsetsLhost vis h =
 doVisit vis (fun x -> x) vis#vtsets_lhost childrenTsetsLhost h

and childrenTsetsLhost vis h =
  match h with
    | TSVar v ->
        let v' = visitCilLogicVarUse vis v in if v != v' then TSVar v' else h
    | TSResult -> h
    | TSMem m ->
        let m' = visitCilTsetsElem vis m in if m != m' then TSMem m' else h

and visitCilTsetsElem vis lv =
  doVisit vis (fun x->x) vis#vtsets_elem childrenTsetsElem lv

and childrenTsetsElem vis e =
  match e with
    | TSLval lv ->
        let lv' = visitCilTsetsLval vis lv in
        if lv' != lv then  TSLval lv' else e
    | TSStartOf lv ->
        let lv' = visitCilTsetsLval vis lv in
        if lv' != lv then TSStartOf lv' else e
    | TSAddrOf lv ->
        let lv' = visitCilTsetsLval vis lv in
        if lv' != lv then TSAddrOf lv' else e
    | TSConst _ -> e
    | TSCastE(typ,elem) ->
        let typ' = visitCilType vis typ in
        let elem' = visitCilTsetsElem vis elem in
        if typ' != typ || elem' != elem then TSCastE(typ', elem') else e
    | TSAdd_range(ts,low,high) ->
        let ts' = visitCilTsetsElem vis ts in
        let low' = (opt_map (visitCilTerm vis)) low in
        let high' = (opt_map (visitCilTerm vis)) high in
        if ts' != ts || low' != low || high' != high then
          TSAdd_range(ts',low',high')
        else e
    | TSAdd_index(ts,i) ->
        let ts' = visitCilTsetsElem vis ts in
        let i' = visitCilTerm vis i in
        if ts' != ts || i' != i then TSAdd_index(ts',i') else e
    | TSat(locs,lab) ->
	let locs' = visitCilTsetsElem vis locs in
        let lab' = visitCilLogicLabel vis lab in
	if locs != locs' || lab != lab' then TSat(locs',lab') else e
    | TSapp(f,labs,args) ->
        let f' = visitCilLogicInfo vis f in
        let labs' =
          mapNoCopy (function (x,y) as orig ->
                       let x' = visitCilLogicLabel vis x in
                       let y' = visitCilLogicLabel vis y in
                       if x' != x || y' != y then (x',y') else orig) labs
        in
        let args' = mapNoCopy (visitCilTerm vis) args in
        if f != f' || labs' != labs || args' != args then
          TSapp(f',labs',args')
        else e

and visitCilTsetsLval vis lv =
  doVisit vis (fun x -> x) vis#vtsets_lval childrenTsetsLval lv
and childrenTsetsLval vis (h,o as lv) =
  let h' = visitCilTsetsLhost vis h in
  let o' = visitCilTsetsOffset vis o in
  if h' != h || o' != o then h',o' else  lv

and visitCilTsetsOffset vis o =
  doVisit vis (fun x -> x) vis#vtsets_offset childrenTsetsOffset o

and childrenTsetsOffset vis o =
  match o with
      TSNoOffset -> o
    | TSIndex (i,next) ->
        let i' = visitCilTerm vis i in
        let next' = visitCilTsetsOffset vis next in
        if i != i' || next != next' then TSIndex (i',next') else o
    | TSRange (i1,i2,next) ->
        let i1' = (opt_map (visitCilTerm vis)) i1 in
        let i2' = (opt_map (visitCilTerm vis)) i2 in
        let next' = visitCilTsetsOffset vis next in
        if i1' != i1 || i2' != i2 || next' != next then
          TSRange(i1',i2',next')
        else o
    | TSField (f,next) ->
        let next' = visitCilTsetsOffset vis next in
        let f' = vis#behavior.get_fieldinfo f in
        if next' != next || f != f' then TSField(f',next') else o

and visitCilLogicInfo vis li =
  doVisit
    vis vis#behavior.memo_logic_info
    vis#vlogic_info_decl childrenLogicInfo li

and childrenLogicInfo vis li =
  let lt = opt_map (visitCilLogicType vis) li.l_type in
  let lp = mapNoCopy (visitCilLogicVarDecl vis) li.l_profile in
(*    (fun p ->
       let lt' = visitCilLogicType vis p.lv_type in
         if lt' != p.lv_type then { p with lv_type = lt'} else p)
    li.l_profile

  in
*)
  li.l_type <- lt;
  li.l_profile <- lp;
  li.l_body <-
    begin
      match li.l_body with
	| LBreads ol ->
            let l = mapNoCopy (visitCilTsets vis) ol in
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

and visitCilLogicType vis t =
  doVisit vis (fun x -> x) vis#vlogic_type childrenLogicType t

and childrenLogicType vis ty =
  match ty with
      Ctype t ->
        let t' = visitCilType vis t in
          if t != t' then Ctype t' else ty
    | Linteger | Lreal -> ty
    | Ltype (s,l) ->
        let l' = mapNoCopy (visitCilLogicType vis) l in
        if l' != l then Ltype (s,l') else ty
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
  doVisit vis vis#behavior.memo_logic_var vis#vlogic_var_decl
    childrenLogicVarDecl lv

and childrenLogicVarDecl vis lv =
  lv.lv_type <- visitCilLogicType vis lv.lv_type;
  lv.lv_origin <-
    opt_map (visitCilVarUse vis) lv.lv_origin;
  lv

and visitCilLogicVarUse vis lv =
  doVisit vis vis#behavior.get_logic_var vis#vlogic_var_use
    childrenLogicVarUse lv

and childrenLogicVarUse vis lv =
  lv.lv_origin <- opt_map (visitCilVarUse vis) lv.lv_origin; lv

and visitCilQuantifiers vis lv =
  doVisit vis (fun x -> x) vis#vquantifiers
    (fun vis l -> mapNoCopy (visitCilLogicVarDecl vis) l) lv

and visitCilPredicate vis p =
  doVisit vis (fun x -> x) vis#vpredicate childrenPredicate p

and visitCilPredicateNamed vis p =
  doVisit vis
    (fun x -> x) vis#vpredicate_named childrenPredicateNamed p

and childrenPredicateNamed vis p =
  let content = visitCilPredicate vis p.content in
  if content != p.content then { p with content = content} else p

and childrenPredicate vis p =
  let vPred p = visitCilPredicateNamed vis p in
  let vLogicInfo li =
    doVisit vis vis#behavior.get_logic_info
      vis#vlogic_info_use (fun _ x ->x) li
  in
(*
  let vPredInfo p =
    doVisit vis
      vis#behavior.get_logic_f vis#vpredicate_info_use (fun _ x -> x) p
  in
*)
  let vTerm t = visitCilTerm vis t in
  let vTsets t = visitCilTsets vis t in
  match p with
      Pfalse | Ptrue -> p
    | Papp (pred,labels,args) ->
        let pred' = vLogicInfo pred in
        let args' = mapNoCopy vTerm args in
        if pred' != pred || args' != args then
          Papp(pred',labels,args')
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
    | Plet(var,t,p1) ->
        let var' = visitCilLogicVarDecl vis var in
        let t' = vTerm t in
        let p1' = vPred p1 in
        if var' != var || t' != t || p1' != p1 then
          Plet(var',t',p1')
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
    | Pold p1 ->
        let p1' = vPred p1 in if p1' != p1 then Pold p1' else p
    | Pat(p1,s) ->
        let p1' = vPred p1 in
        let s' = visitCilLogicLabel vis s in
        if p1' != p1 then Pat(p1',s') else p
    | Pvalid t ->
        let t' = vTsets t in if t' != t then Pvalid t' else p
    | Pvalid_index (t1,t2) ->
        let t1' = vTerm t1 in
        let t2' = vTerm t2 in
        if t1' != t1 || t2' != t2 then Pvalid_index (t1',t2') else p
    | Pvalid_range(t1,t2,t3) ->
        let t1' = vTerm t1 in
        let t2' = vTerm t2 in
        let t3' = vTerm t3 in
        if t1' != t1 || t2' != t2 || t3' != t3 then
          Pvalid_range (t1',t2',t3') else p
    | Pseparated seps ->
        let seps' = mapNoCopy vTsets seps in
        if seps' != seps then Pseparated seps' else p
    | Pfresh t ->
        let t' = vTerm t in if t' != t then Pfresh t' else p
(*    | Pnamed(s,p1) ->
        let p1' = vPred p1 in if p1' != p1 then Pnamed (s,p1') else p *)
    | Psubtype(te,tc) ->
        let tc' = vTerm tc in
        let te' = vTerm te in
        if tc' != tc || te' != te then Psubtype(te',tc') else p

(*
and visitCilPredicateInfo vis pi =
  doVisit vis vis#behavior.memo_predicate_info
    vis#vpredicate_info_decl childrenPredicateInfo pi
*)


and visitCilIdLocations vis loc =
  let loc' = visitCilTsets vis loc.its_content in
  if loc' != loc.its_content then { loc with its_content = loc' } else loc

and visitCilZone vis z =
  doVisit vis (fun x -> x) vis#vzone childrenZone z
and childrenZone vis z =
  match z with
      Nothing -> z
    | Location loc ->
        let loc' = visitCilIdLocations vis loc in
        if loc' != loc then Location loc' else z

and visitCilAssigns vis a =
  doVisit vis (fun x -> x) vis#vassigns childrenAssigns a
and childrenAssigns vis ((z,l) as a)=
  let z' = visitCilZone vis z in
  let l' = mapNoCopy (visitCilZone vis) l in
  if z' != z || l' != l then (z',l') else a

and visitCilBehavior vis b =
  doVisit vis vis#behavior.cfunbehavior
    vis#vbehavior childrenBehavior b

and childrenBehavior vis b =
  b.b_assumes <- visitCilPredicates vis b.b_assumes;
  b.b_ensures <- visitCilPredicates vis b.b_ensures;
  b.b_assigns <- mapNoCopy (visitCilAssigns vis) b.b_assigns;
  b

and visitCilPredicates vis ps = mapNoCopy (visitCilIdPredicate vis) ps

and visitCilIdPredicate vis ps =
  let p' = visitCilPredicate vis ps.ip_content in
  if p' != ps.ip_content then { ps with ip_content = p' } else ps

and visitCilBehaviors vis bs = mapNoCopy (visitCilBehavior vis) bs

and visitCilFunspec vis s =
  doVisit vis vis#behavior.cfunspec vis#vspec childrenSpec s

and childrenSpec vis s =
  s.spec_requires <- visitCilPredicates vis s.spec_requires;
  s.spec_behavior <- visitCilBehaviors vis s.spec_behavior;
  s.spec_variant <-
    opt_map (fun x -> (visitCilTerm vis (fst x), snd x)) s.spec_variant;
  s.spec_terminates <-
    opt_map (visitCilIdPredicate vis) s.spec_terminates;
  (* nothing is done now for behaviors names, no need to visit complete and
     disjoint behaviors clauses
   *)
  s

and visitCilSlicePragma vis p =
  doVisit vis (fun x -> x) vis#vslice_pragma childrenSlicePragma p

and childrenSlicePragma vis p =
  match p with
      | SPexpr t ->
          let t' = visitCilTerm vis t in if t' != t then SPexpr t' else p
      | SPctrl | SPstmt -> p

and visitCilImpactPragma vis p =
  doVisit vis (fun x -> x) vis#vimpact_pragma childrenImpactPragma p

and childrenImpactPragma vis p = match p with
  | IPexpr t -> let t' = visitCilTerm vis t in if t' != t then IPexpr t' else p
  | IPstmt -> p

and visitCilLoopPragma vis p =
  doVisit vis
    (fun x -> x) vis#vloop_pragma childrenLoopPragma p

and childrenLoopPragma vis p =
match p with
  | Unroll_level t -> let t' = visitCilTerm vis t in
    if t' != t then Unroll_level t' else p
  | Widen_hints lt -> let lt' = List.map (visitCilTerm vis) lt in
    if lt' != lt then Widen_hints lt' else p
  | Widen_variables lt -> let lt' = List.map (visitCilTerm vis) lt in
    if lt' != lt then Widen_variables lt' else p

and visitCilAnnotation vis a =
  doVisit vis (fun x -> x) vis#vannotation childrenAnnotation a

and visitCilAxiom vis ((id,p) as a) =
  let p' = visitCilPredicateNamed vis p in
  if p' != p then (id,p') else a

and childrenAnnotation vis a =
  match a with
    | Dfun_or_pred li ->
	let li' = visitCilLogicInfo vis li in
	if li' != li then Dfun_or_pred li' else a
(*
      Dpredicate_reads (s, tvars, args,loc) ->
        let s' = visitCilPredicateInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let loc' = mapNoCopy (visitCilTsets vis) loc in
        if s'!=s || args' != args || loc' != loc then
          Dpredicate_reads(s',tvars,args',loc')
        else a
    | Dpredicate_def(s,tvars,args,def) ->
        let s' = visitCilPredicateInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let def' = visitCilPredicateNamed vis def in
        if s' != s || args' != args || def' != def then
          Dpredicate_def(s',tvars,args',def')
        else a
    | Dinductive_def(s,tvars,args,indcases) ->
        let s' = visitCilPredicateInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let indcases' = mapNoCopy (visitCilAxiom vis) indcases in
        if s' != s || args' != args || indcases' != indcases then
          Dinductive_def(s',tvars,args',indcases')
        else a
*)

    | Dtype _ -> a (* nothing to visit here for now *)
(*
    | Dlogic_reads(s,tvars,args,rt,loc) ->
        let s' = visitCilLogicInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let rt' = visitCilLogicType vis rt in
        let loc' = mapNoCopy (visitCilTsets vis) loc in
        if s'!= s || args' != args || rt' != rt || loc' != loc then
          Dlogic_reads(s',tvars,args',rt',loc')
        else a
    | Dlogic_def(s,tvars,args,rt,def) ->
        let s' = visitCilLogicInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let rt' = visitCilLogicType vis rt in
        let def' = visitCilTerm vis def in
        if s != s' || args' != args || rt' != rt || def' != def then
          Dlogic_def(s',tvars,args',rt',def')
        else a
    | Dlogic_axiomatic(s,tvars,args,rt,axioms) ->
        let s' = visitCilLogicInfo vis s in
        let args' = mapNoCopy (visitCilLogicVarDecl vis) args in
        let rt' = visitCilLogicType vis rt in
        let axioms' = mapNoCopy (visitCilAxiom vis) axioms in
        if s != s' || args' != args || rt' != rt || axioms' != axioms then
          Dlogic_axiomatic(s',tvars,args',rt',axioms')
        else a
*)
    | Dlemma(s,is_axiom,labels,tvars,p) ->
        let p' = visitCilPredicateNamed vis p in
        if p' != p then Dlemma(s,is_axiom,labels,tvars,p') else a
    | Dinvariant p ->
        let p' = visitCilLogicInfo vis p in
        if p' != p then Dinvariant p' else a
    | Dtype_annot ta ->
        let ta' = visitCilLogicInfo vis ta in
        if ta' != ta then Dtype_annot ta' else a
    | Daxiomatic(id,l) ->
(*
	Format.eprintf "cil.visitCilAnnotation on axiomatic %s@." id;
*)
	let l' = mapNoCopy (visitCilAnnotation vis) l in
	if l' != l then Daxiomatic(id,l') else a

and visitCilCodeAnnotation vis ca =
  doVisit vis (fun x -> x) vis#vcode_annot childrenCodeAnnot ca

and childrenCodeAnnot vis ca =
  let vPred p = visitCilPredicateNamed vis p in
  let vTerm t = visitCilTerm vis t in
  let vSpec s = visitCilFunspec vis s in
  let change_content annot = { ca with annot_content = annot } in
  match ca.annot_content with
      AAssert (behav,p,_status) ->
        (* TODO: status is reset to Unkown if the predicate changed but
           remain unchanged otherwise. It should be visited. *)
        let p' = vPred p in if p' != p then
	  change_content (AAssert (behav,p',{status=Unknown}))
	else ca
    | AAssume p ->
        let p' = vPred p in if p' != p then change_content (AAssume p') else ca
    | APragma (Impact_pragma t) ->
        let t' = visitCilImpactPragma vis t in
        if t' != t then change_content (APragma (Impact_pragma t')) else ca
    | APragma (Slice_pragma t) ->
        let t' = visitCilSlicePragma vis t in
        if t' != t then change_content (APragma (Slice_pragma t')) else ca
    | APragma (Loop_pragma p) ->
        let p' = visitCilLoopPragma vis p in
        if p' != p then change_content (APragma (Loop_pragma p')) else ca
    | AStmtSpec s ->
        let s' = vSpec s in
        if s' != s then change_content (AStmtSpec s') else ca
    | AInvariant(behav,f,p) ->
        let p' = vPred p in
        if p' != p then change_content (AInvariant (behav,f,p')) else ca
    | AVariant ((t,s)) ->
        let t' = vTerm t in
        if t != t' then change_content (AVariant ((t',s))) else ca
    | AAssigns a ->
        let a' = visitCilAssigns vis a in
        if a != a' then change_content (AAssigns a') else ca

and visitCilExpr (vis: cilVisitor) (e: exp) : exp =
  doVisit vis (fun x -> x) vis#vexpr childrenExp e
and childrenExp (vis: cilVisitor) (e: exp) : exp =
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vLval lv = visitCilLval vis lv in
  match stripInfo e with
  | Info _ -> assert false
  | Const _ -> e (* enum constants are visited at their declaration site *)
  | SizeOf t ->
      let t'= vTyp t in
      if t' != t then SizeOf t' else e
  | SizeOfE e1 ->
      let e1' = vExp e1 in
      if e1' != e1 then SizeOfE e1' else e
  | SizeOfStr _s -> e

  | AlignOf t ->
      let t' = vTyp t in
      if t' != t then AlignOf t' else e
  | AlignOfE e1 ->
      let e1' = vExp e1 in
      if e1' != e1 then AlignOfE e1' else e
  | Lval lv ->
      let lv' = vLval lv in
      if lv' != lv then Lval lv' else e
  | UnOp (uo, e1, t) ->
      let e1' = vExp e1 in let t' = vTyp t in
      if e1' != e1 || t' != t then UnOp(uo, e1', t') else e
  | BinOp (bo, e1, e2, t) ->
      let e1' = vExp e1 in let e2' = vExp e2 in let t' = vTyp t in
      if e1' != e1 || e2' != e2 || t' != t then BinOp(bo, e1',e2',t') else e
  | CastE (t, e1) ->
      let t' = vTyp t in let e1' = vExp e1 in
      if t' != t || e1' != e1 then CastE(t', e1') else e
  | AddrOf lv ->
      let lv' = vLval lv in
      if lv' != lv then AddrOf lv' else e
  | StartOf lv ->
      let lv' = vLval lv in
      if lv' != lv then StartOf lv' else e

and visitCilInit (vis: cilVisitor) (forglob: varinfo)
                 (atoff: offset) (i: init) : init =
  let rec childrenInit (vis: cilVisitor) (i: init) : init =
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
  doVisit vis (fun x -> x) (vis#vinit forglob atoff) childrenInit i

and visitCilLval (vis: cilVisitor) (lv: lval) : lval =
  doVisit vis (fun x -> x) vis#vlval childrenLval lv
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
  doVisit vis (fun x -> x) vis#voffs childrenOffset off
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
  doVisit vis (fun x -> x) vis#vinitoffs childrenOffset off

and visitCilInstr (vis: cilVisitor) (i: instr) : instr list =
  let oldloc = CurrentLoc.get () in
  CurrentLoc.set (get_instrLoc i);
  assertEmptyQueue vis;
  let res =
    doVisitList vis (fun x -> x) vis#vinst childrenInstr i in
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
      let a' = visitCilCodeAnnotation vis a in Code_annot(a',l)


(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis:cilVisitor) (s: stmt) : stmt =
  let oldloc = CurrentLoc.get () in
  CurrentLoc.set (get_stmtLoc s.skind) ;
  vis#push_stmt s; (*(vis#behavior.memo_stmt s);*)
  assertEmptyQueue vis;
  let toPrepend : instr list ref = ref [] in (* childrenStmt may add to this *)
  let res =
    doVisit vis
      vis#behavior.memo_stmt vis#vstmt (childrenStmt toPrepend) s in
  (* Now see if we have saved some instructions *)
  toPrepend := !toPrepend @ vis#unqueueInstr ();
  (match !toPrepend with
    [] -> () (* Return the same statement *)
  | _ ->
      (* Make our statement contain the instructions to prepend *)
      res.skind <-
	Block { battrs = [];
		bstmts = (List.map (fun i -> mkStmt (Instr i)) !toPrepend) @
                         [ mkStmt res.skind ] });
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
            (function (stmt,writes,reads) as orig->
               let stmt' = visitCilStmt vis stmt in
               let writes' = mapNoCopy (visitCilLval vis) writes in
               let reads' = mapNoCopy (visitCilLval vis) reads in
               if stmt' != stmt || writes' != writes || reads' != reads then
                 (stmt',writes',reads')
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
        (* the stmts in b should have cleaned up after themselves.*)
        assertEmptyQueue vis;
        (* Don't do stmts, but we better not change those *)
        if e' != e || b' != b then Switch (e', b', stmts, l) else s.skind
    | Instr i ->
        begin match fInst i with
	  | [i'] when i' == i -> s.skind
	  | il -> stmt_of_instr_list ~loc:(get_instrLoc i) il
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
  doVisit vis vis#behavior.cblock vis#vblock childrenBlock b
and childrenBlock (vis: cilVisitor) (b: block) : block =
  let fStmt s = visitCilStmt vis s in
  let stmts' = mapNoCopy fStmt b.bstmts in
  if stmts' != b.bstmts then { battrs = b.battrs; bstmts = stmts'} else b


and visitCilType (vis : cilVisitor) (t : typ) : typ =
  doVisit vis (fun x -> x) vis#vtype childrenType t
and childrenType (vis : cilVisitor) (t : typ) : typ =
  (* look for types referred to inside t's definition *)
  let fTyp t  = visitCilType vis t in
  let fAttr a = visitCilAttributes vis a in
  match t with
    TPtr(t1, a) ->
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t || a' != a then TPtr(t1', a') else t
  | TArray(t1, None, a) ->
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t || a' != a  then TArray(t1', None, a') else t
  | TArray(t1, Some e, a) ->
      let t1' = fTyp t1 in
      let e' = visitCilExpr vis e in
      let a' = fAttr a in
      if t1' != t || e' != e  || a' != a then TArray(t1', Some e', a') else t

      (* DON'T recurse into the compinfo, this is done in visitCilGlobal.
	 User can iterate over cinfo.cfields manually, if desired.*)
  | TComp(cinfo, a) ->
      let cinfo' = vis#behavior.get_compinfo cinfo in
      let a' = fAttr a in
      if a != a' || cinfo' != cinfo then TComp(cinfo', a') else t

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
  | _ ->  (* other types (TVoid, TInt, TFloat, TEnum, and TBuiltin_va_list)
             don't contain nested types, but they do have attributes. *)
      let a = typeAttrs t in
      let a' = fAttr a in
      if a' != a  then setTypeAttrs t a' else t

(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  doVisit vis vis#behavior.memo_varinfo
    vis#vvdec childrenVarDecl v
and childrenVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  v.vtype <- visitCilType vis v.vtype;
  v.vattr <- visitCilAttributes vis v.vattr;
  v.vlogic_var_assoc <-
    opt_map (visitCilLogicVarDecl vis) v.vlogic_var_assoc;
  v

and visitCilVarUse vis v =
  doVisit vis vis#behavior.get_varinfo vis#vvrbl (fun _ x -> x) v

and visitCilAttributes (vis: cilVisitor) (al: attribute list) : attribute list=
   let al' =
     mapNoCopyList
       (doVisitList vis
          (fun x -> x) vis#vattr childrenAttribute) al in
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
   doVisit vis (fun x -> x) vis#vattrparam childrenAttrparam a
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
    | ASizeOfS _ | AAlignOfS _ ->
        ignore (warn "Visitor inside of a type signature.");
        aa
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
        let stmt1' = opt_map b.get_stmt stmt1 in
        let stmt2' = opt_map b.get_stmt stmt2 in
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
  if debugVisit then ignore
    (E.log "Visiting function %s\n" f.svar.vname);
  assertEmptyQueue vis;
  vis#set_current_func f;
  let f = vis#behavior.cfundec f in
  f.svar <- vis#behavior.memo_varinfo f.svar; (* hit the function name *)
  let f =
    doVisit vis (fun x -> x) (* copy has already been done *)
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
  setFormals f newformals;
  (* Remember any new instructions that were generated while visiting
     variable declarations. *)
  let toPrepend = vis#unqueueInstr () in
  f.sbody <- visitCilBlock vis f.sbody;        (* visit the body *)
  if toPrepend <> [] then
    f.sbody.bstmts <- (List.map (fun i -> mkStmt (Instr i)) toPrepend) @ f.sbody.bstmts;
  f.sspec <- visitCilFunspec vis f.sspec;
  f

let childrenFieldInfo vis fi =
  fi.fcomp <- vis#behavior.get_compinfo fi.fcomp;
  fi.ftype <- visitCilType vis fi.ftype;
  fi.fattr <- visitCilAttributes vis fi.fattr;
  fi

let visitCilFieldInfo vis f =
  doVisit vis vis#behavior.memo_fieldinfo vis#vfieldinfo childrenFieldInfo f

let childrenCompInfo vis comp =
  comp.cfields <- mapNoCopy (visitCilFieldInfo vis) comp.cfields;
  comp.cattr <- visitCilAttributes vis comp.cattr;
  comp

let visitCilCompInfo vis c =
  doVisit vis vis#behavior.memo_compinfo vis#vcompinfo childrenCompInfo c

let childrenEnumItem vis e =
  e.eival <- visitCilExpr vis e.eival;
  e.eihost <- vis#behavior.get_enuminfo e.eihost;
  e

let visitCilEnumItem vis e =
  doVisit vis vis#behavior.memo_enumitem vis#venumitem childrenEnumItem e

let childrenEnumInfo vis e =
  e.eitems <- mapNoCopy (visitCilEnumItem vis) e.eitems;
  e.eattr <- visitCilAttributes vis e.eattr;
  e

let visitCilEnumInfo vis e =
  doVisit vis vis#behavior.memo_enuminfo vis#venuminfo childrenEnumInfo e

let rec visitCilGlobal (vis: cilVisitor) (g: global) : global list =
  (*(trace "visit" (dprintf "visitCilGlobal\n"));*)
  let oldloc = CurrentLoc.get () in
  CurrentLoc.set (get_globalLoc g) ;
  currentGlobal := g;
  let res =
    doVisitList vis (fun x -> x) vis#vglob childrenGlobal g in
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
      if t' != t then GType(t,l) else g
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
      let form' = opt_map (mapNoCopy (visitCilVarDecl vis)) form in
      let spec' =
        if isFunctionType v.vtype then
          visitCilFunspec vis spec
        else begin
          assert (is_empty_funspec spec);
          spec
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
      | _ -> E.s (E.unimp "visitCilAttributes returns more than one attribute")
  end
  | GAnnot (a,l) ->
      let a' = visitCilAnnotation vis a in
        if a' != a then GAnnot(a',l) else g
  | GText _ | GAsm _ -> g


(** A visitor that does constant folding. If "machdep" is true then we do
 * machine dependent simplification (e.g., sizeof) *)
class constFoldVisitorClass (machdep: bool) : cilVisitor = object
  inherit nopCilVisitor

  method vinst i =
    match i with
      (* Skip two functions to which we add Sizeof to the type arguments.
         See the comments for these above. *)
      Call(_,(Lval (Var vi,NoOffset)),_,_)
        when ((vi.vname = "__builtin_va_arg")
              || (vi.vname = "__builtin_types_compatible_p")) ->
          SkipChildren
    | _ -> DoChildren
  method vexpr (e: exp) =
    (* Do it bottom up *)
    ChangeDoChildrenPost (e, constFold machdep)

end
let constFoldVisitor (machdep: bool) = new constFoldVisitorClass machdep

(* Iterate over all globals, including the global initializer *)
let iterGlobals (fl: file)
                (doone: global -> unit) : unit =
  let doone' g =
      CurrentLoc.set (get_globalLoc g);
      doone g
  in
  List.iter doone' fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> doone' (GFun(g, locUnknown)))

(* Fold over all globals, including the global initializer *)
let foldGlobals (fl: file)
                (doone: 'a -> global -> 'a)
                (acc: 'a) : 'a =
  let doone' acc g =
      CurrentLoc.set (get_globalLoc g);
      doone acc g
  in
  let acc' = List.fold_left doone' acc fl.globals in
  (match fl.globinit with
    None -> acc'
  | Some g -> doone' acc' (GFun(g, locUnknown)))

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
            E.s (error ("findOrCreateFunc: can't create %s because another "
                        ^^"global exists with that name.") name);
          vi
      | _ :: rest -> search rest (* tail recursive *)
      | [] -> (*not found, so create one *)
          let t' = unrollTypeDeep t in
	  let new_decl = makeGlobalVar name t' in
          setFormalsDecl new_decl t';
	  f.globals <- GVarDecl(empty_funspec (), new_decl, locUnknown) :: f.globals;
	  new_decl
  in
  search f.globals


let childrenFileSameGlobals vis f =
  let fGlob g = visitCilGlobal vis g in
  iterGlobals f
    (fun g ->
       match fGlob g with
           [g'] when g' == g || equals g' g -> () (* Try to do the pointer check first *)
         | gl ->
             ignore (log "You used visitCilFileSameGlobals but the global got changed:\n %a\nchanged to %a\n" d_global g (fprintfList ~sep:"@\n" d_global) gl);
             ());
  f

let post_file vis f =
  let res = vis#vfile f in
  match res with
      SkipChildren -> ChangeToPost(f, fun res -> vis#fill_global_tables; res)
    | ChangeTo res -> ChangeToPost(res, fun res -> vis#fill_global_tables; res)
    | ChangeToPost (res, f) ->
        ChangeToPost(res, fun res -> vis#fill_global_tables; f res)
    | DoChildren ->
        ChangeDoChildrenPost(f, fun f -> vis#fill_global_tables; f)
    | ChangeDoChildrenPost(f,post) ->
        ChangeDoChildrenPost
          (f,fun f -> let f = post f in vis#fill_global_tables; f)

(* A visitor for the whole file that does not change the globals *)
let visitCilFileSameGlobals (vis : cilVisitor) (f : file) : unit =
  if vis#behavior.is_copy_behavior then
    ignore (log "You used visitCilFileSameGlobals with a copy visitor. \
                 This is an error. Nothing is done")
  else
    ignore
      (doVisit vis vis#behavior.cfile (post_file vis) childrenFileSameGlobals f)

let childrenFileCopy vis f =
  let fGlob g = visitCilGlobal vis g in
  (* Scan the globals. Make sure this is tail recursive. *)
  let rec loop (acc: global list) = function
      [] -> f.globals <- List.rev acc
    | g :: restg ->
        loop ((List.rev (fGlob g)) @ acc) restg
  in
  loop [] f.globals;
  (* the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> f.globinit <- Some (visitCilFunction vis g));
  f

(* Be careful with visiting the whole file because it might be huge. *)
let visitCilFileCopy (vis : cilVisitor) (f : file) : file =
  vis#set_logic_tables ();
  doVisit vis vis#behavior.cfile (post_file vis) childrenFileCopy f

let visitCilFile vis f =
  if vis#behavior.is_copy_behavior then
    ignore (log "You used visitCilFile with a copy visitor. \
                 This is an error. Nothing is done")
  else ignore (visitCilFileCopy vis f)


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
                                     Lval(var f.svar),
                                     [], locUnknown)))
                :: m.sbody.bstmts;
              inserted := true;
              if !E.verboseFlag then
                ignore (E.log "Inserted the globinit\n");
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
      match doone (GFun(g, locUnknown)) with
        GFun(g', _) -> fl.globinit <- Some g'
      | _ -> E.s (E.bug "mapGlobals: globinit is not a function")
  end)



let dumpFile (pp: cilPrinter) (out : out_channel) (outfile: string) file =

  let fmt = formatter_of_out_channel out in
  pp_set_max_boxes fmt max_int;  (* We don't want ... in the output *)
  pp_set_margin fmt 79;

  if !E.verboseFlag then
    ignore (log "printing file %s\n" outfile);

  fprintf fmt
    "/* Generated by CIL v. %s */@\n/* print_CIL_Input is %b */@\n@\n"
    cilVersion
    miscState.print_CIL_Input;
  iterGlobals file (fun g -> printGlobal pp fmt g);

  (* sm: we have to flush the output channel; if we don't then under *)
  (* some circumstances (I haven't figure out exactly when, but it happens *)
  (* more often with big inputs), we get a truncated output file *)
  pp_print_flush fmt ();
  flush out

let d_file (pp: cilPrinter) fmt file =
  fprintf fmt
    "@[/* Generated by CIL v. %s */@\n/* print_CIL_Input is %b */@\n@\n"
    cilVersion
    miscState.print_CIL_Input;
  iterGlobals file (fun g -> printGlobal pp fmt g);
  fprintf fmt "@]@."


(******************
 ******************
 ******************)


(* Convert an expression into an attribute, if possible. Otherwise raise
 * NotAnAttrParam *)
exception NotAnAttrParam of exp
let rec expToAttrParam (e: exp) : attrparam =
  match e with
    Const(CInt64(i,k,_)) ->
      let i', trunc = truncateInteger64 k i in
      if trunc then
        raise (NotAnAttrParam e);
      let i2 = Int64.to_int i' in
      if i' <> Int64.of_int i2 then
        raise (NotAnAttrParam e);
      AInt i2
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
          peepHole1 doone (List.map (fun (x,_,_) -> x) seq)
      | TryFinally (b, h, _l) ->
          peepHole1 doone b.bstmts;
          peepHole1 doone h.bstmts
      | TryExcept (b, (il, e), h, l) ->
          peepHole1 doone b.bstmts;
          peepHole1 doone h.bstmts;
          s.skind <- TryExcept(b, (doInstrList il, e), h, l);
      | Return _ | Goto _ | Break _ | Continue _ -> ())
    ss

let rec peepHole2  (* Process two statements and possibly replace them both *)
                   (dotwo: stmt * stmt -> stmt list option)
                   (ss: stmt list) =
  let rec doStmtList (il: stmt list) : stmt list =
    match il with
      [] -> []
    | [i] -> process i; il
    | (i1 :: ((i2 :: rest) as rest2)) ->
        begin
          match dotwo (i1,i2) with
            None -> process i1; i1 :: doStmtList rest2
          | Some sl -> doStmtList (sl @ rest)
        end
  and doUnspecifiedStmtList il =
    match il with
        [] -> []
      | [ (s,_,_) ] -> process s; il
      | ((i1,w1,r1) as hd)::(((i2,w2,r2)::rest) as rest2) ->
          begin
            match dotwo (i1,i2) with
                None -> process i1; hd :: doUnspecifiedStmtList rest2
              | Some [] -> doUnspecifiedStmtList rest
              | Some (hd::tl) ->
                  let res =
                    (hd, w1 @ w2, r1 @ r2) :: (List.map (fun x -> x,[],[]) tl)
                  in doUnspecifiedStmtList (res @ rest)
          end
  and process s =
    match s.skind with
        Instr _i -> ()
      | If (_e, tb, eb, _) ->
          tb.bstmts <- peepHole2 dotwo tb.bstmts;
          eb.bstmts <- peepHole2 dotwo eb.bstmts
      | Switch (_e, b, _, _) -> b.bstmts <- peepHole2 dotwo b.bstmts
      | Loop (_, b, _l, _, _) -> b.bstmts <- peepHole2 dotwo b.bstmts
      | Block b -> b.bstmts <- doStmtList b.bstmts
      | TryFinally (b, h, _l) -> b.bstmts <- peepHole2 dotwo b.bstmts;
          b.bstmts <- peepHole2 dotwo h.bstmts
      | TryExcept (b, (_il, _e), h, _l) ->
          b.bstmts <- peepHole2 dotwo b.bstmts;
          h.bstmts <- peepHole2 dotwo h.bstmts;
          () (*s.skind <- TryExcept (b, (doInstrList il, e), h, l)*)

      | UnspecifiedSequence seq ->
          s.skind <- UnspecifiedSequence (doUnspecifiedStmtList seq)
      | Return _ | Goto _ | Break _ | Continue _ -> ()
  in List.iter process ss;
  doStmtList ss

(*** Type signatures ***)

(* Helper class for typeSig: replace any types in attributes with typsigs *)
class typeSigVisitor(typeSigConverter: typ->typsig) = object
  inherit nopCilVisitor
  method vattrparam ap =
    match ap with
      | ASizeOf t -> ChangeTo (ASizeOfS (typeSigConverter t))
      | AAlignOf t -> ChangeTo (AAlignOfS (typeSigConverter t))
      | _ -> DoChildren
end

let typeSigAddAttrs a0 t =
  if a0 == [] then t else
  match t with
    TSBase t -> TSBase (typeAddAttributes a0 t)
  | TSPtr (ts, a) -> TSPtr (ts, addAttributes a0 a)
  | TSArray (ts, l, a) -> TSArray(ts, l, addAttributes a0 a)
  | TSComp (iss, n, a) -> TSComp (iss, n, addAttributes a0 a)
  | TSEnum (n, a) -> TSEnum (n, addAttributes a0 a)
  | TSFun(ts, tsargs, isva, a) -> TSFun(ts, tsargs, isva, addAttributes a0 a)

(* Compute a type signature.
    Use ~ignoreSign:true to convert all signed integer types to unsigned,
    so that signed and unsigned will compare the same. *)
let rec typeSigWithAttrs ?(ignoreSign=false) doattr t =
  let typeSig = typeSigWithAttrs ~ignoreSign doattr in
  let attrVisitor = new typeSigVisitor typeSig in
  let doattr al = visitCilAttributes attrVisitor (doattr al) in
  match t with
  | TInt (ik, al) ->
      let ik' =
        if ignoreSign then unsignedVersionOf ik  else ik
      in
      TSBase (TInt (ik', doattr al))
  | TFloat (fk, al) -> TSBase (TFloat (fk, doattr al))
  | TVoid al -> TSBase (TVoid (doattr al))
  | TEnum (enum, a) -> TSEnum (enum.ename, doattr a)
  | TPtr (t, a) -> TSPtr (typeSig t, doattr a)
  | TArray (t,l,a) -> (* We do not want fancy expressions in array lengths.
                       * So constant fold the lengths *)
      let l' =
        match l with
          Some l -> begin
            match constFold true l with
              Const(CInt64(i, _, _)) -> Some i
            | e -> E.s (bug "Invalid length in array type: %a\n"
                          (!pd_exp) e)
          end
        | None -> None
      in
      TSArray(typeSig t, l', doattr a)

  | TComp (comp, a) ->
      TSComp (comp.cstruct, comp.cname, doattr (addAttributes comp.cattr a))
  | TFun(rt,args,isva,a) ->
      TSFun(typeSig rt,
            List.map (fun (_, atype, _) -> (typeSig atype)) (argsToList args),
            isva, doattr a)
  | TNamed(t, a) -> typeSigAddAttrs (doattr a) (typeSig t.ttype)
  | TBuiltin_va_list al -> TSBase (TBuiltin_va_list (doattr al))

let typeSig t =
  typeSigWithAttrs (fun al -> al) t

let _ = pTypeSig := typeSig

(* Remove the attribute from the top-level of the type signature *)
let setTypeSigAttrs (a: attribute list) = function
    TSBase t -> TSBase (setTypeAttrs t a)
  | TSPtr (ts, _) -> TSPtr (ts, a)
  | TSArray (ts, l, _) -> TSArray(ts, l, a)
  | TSComp (iss, n, _) -> TSComp (iss, n, a)
  | TSEnum (n, _) -> TSEnum (n, a)
  | TSFun (ts, tsargs, isva, _) -> TSFun (ts, tsargs, isva, a)


let typeSigAttrs = function
    TSBase t -> typeAttrs t
  | TSPtr (_ts, a) -> a
  | TSArray (_ts, _l, a) -> a
  | TSComp (_iss, _n, a) -> a
  | TSEnum (_n, a) -> a
  | TSFun (_ts, _tsargs, _isva, a) -> a



let dExp: string -> exp =
  fun d -> Const(CStr(d))

let dInstr: string -> location -> instr =
  fun d l -> Asm([], [d], [], [], [], l)

let dGlobal: string -> location -> global =
  fun d l -> GAsm(d, l)

  (* Make an AddrOf. Given an lval of type T will give back an expression of
   * type ptr(T)  *)
let mkAddrOf ((_b, _off) as lval) : exp =
  (* Never take the address of a register variable *)
  (match lval with
    Var vi, _off when vi.vstorage = Register -> vi.vstorage <- NoStorage
  | _ -> ());
  match lval with
    Mem e, NoOffset -> e
  | b, Index(z, NoOffset) when isZero z -> StartOf (b, NoOffset)(* array *)
  | _ -> AddrOf lval


let mkAddrOrStartOf (lv: lval) : exp =
  match unrollType (typeOfLval lv) with
    TArray _ -> StartOf lv
  | _ -> mkAddrOf lv


  (* Make a Mem, while optimizing AddrOf. The type of the addr must be
   * TPtr(t) and the type of the resulting lval is t. Note that in CIL the
   * implicit conversion between a function and a pointer to a function does
   * not apply. You must do the conversion yourself using AddrOf *)
let mkMem ~(addr: exp) ~(off: offset) : lval =
  let res =
    match addr, off with
      AddrOf lv, _ -> addOffsetLval off lv
    | StartOf lv, _ -> (* Must be an array *)
        addOffsetLval (Index(zero, off)) lv
    | _, _ -> Mem addr, off
  in
(*  ignore (E.log "memof : %a:%a\nresult = %a\n"
            d_plainexp addr d_plainoffset off d_plainexp res); *)
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
  | _ -> E.s (bug "splitFunctionType invoked on a non function type %a"
                d_type ftype)

let splitFunctionTypeVI (fvi: varinfo)
    : typ * (string * typ * attributes) list option * bool * attributes =
  match unrollType fvi.vtype with
    TFun (rt, args, isva, a) -> rt, args, isva, a
  | _ -> E.s (bug "Function %s invoked on a non function type" fvi.vname)

let isArrayType t =
  match unrollType t with
    TArray _ -> true
  | _ -> false

let isCharArrayType t =
  match unrollType t with
    TArray(tau,_,_) when isCharType tau -> true
  | _ -> false

let isStructOrUnionType t =
  match unrollType t with
    TComp _ -> true
  | _ -> false


let rec isConstant e = match stripInfo e with
  | Info _ -> assert false
  | Const _ -> true
  | UnOp (_, e, _) -> isConstant e
  | BinOp (_, e1, e2, _) -> isConstant e1 && isConstant e2
  | Lval (Var vi, NoOffset) ->
      (vi.vglob && isArrayType vi.vtype || isFunctionType vi.vtype)
  | Lval _ -> false
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> true
  | CastE (_, e) -> isConstant e
  | AddrOf (Var vi, off) | StartOf (Var vi, off)
        -> vi.vglob && isConstantOffset off
  | AddrOf (Mem e, off) | StartOf(Mem e, off)
        -> isConstant e && isConstantOffset off

and isConstantOffset = function
    NoOffset -> true
  | Field(_fi, off) -> isConstantOffset off
  | Index(e, off) -> isConstant e && isConstantOffset off

let getCompField (cinfo:compinfo) (fieldName:string) : fieldinfo =
  (List.find (fun fi -> fi.fname = fieldName) cinfo.cfields)


let rec mkCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  (* Do not remove old casts because they are conversions !!! *)
  (* TODO: remove ALL attributes including volatile...*)
  if equals
    (typeSig (typeRemoveAttributes ["const"; "FRAMA_C_BITFIELD_SIZE"] oldt))
    (typeSig (typeRemoveAttributes ["const"; "FRAMA_C_BITFIELD_SIZE"] newt)) then begin
    e
  end else begin
    (* Watch out for constants *)
    match newt, e with
      TInt(newik, []), Const(CInt64(i, _, _)) -> kinteger64 newik i
    | _ -> CastE((typeRemoveAttributes ["FRAMA_C_BITFIELD_SIZE"] newt),e)
  end

let mkCast ~(e: exp) ~(newt: typ) =
  mkCastT e (typeOf e) newt

type existsAction =
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are
                                         * looking for but maybe its
                                         * successors are *)
let existsType (f: typ -> existsAction) (t: typ) : bool =
  let memo : (int, unit) H.t = H.create 17 in  (* Memo table *)
  let rec loop t =
    match f t with
      ExistsTrue -> true
    | ExistsFalse -> false
    | ExistsMaybe ->
        (match t with
          TNamed (t', _) -> loop t'.ttype
        | TComp (c, _) -> loopComp c
        | TArray (t', _, _) -> loop t'
        | TPtr (t', _) -> loop t'
        | TFun (rt, args, _, _) ->
            (loop rt || List.exists (fun (_, at, _) -> loop at)
              (argsToList args))
        | _ -> false)
  and loopComp c =
    if H.mem memo c.ckey then
      (* We are looping, the answer must be false *)
      false
    else begin
      H.add memo c.ckey ();
      List.exists (fun f -> loop f.ftype) c.cfields
    end
  in
  loop t


(* Try to do an increment, with constant folding *)
let increm (e: exp) (i: int) =
  let et = typeOf e in
  let bop = if isPointerType et then PlusPI else PlusA in
  constFold false (BinOp(bop, e, integer i, et))

(* Try to do an increment, with constant folding *)
let increm64 (e: exp) (i: int64) =
  let et = typeOf e in
  let bop = if isPointerType et then PlusPI else PlusA in
  constFold false (BinOp(bop, e, kinteger64 IULongLong i, et))

exception LenOfArray
let lenOfArray64 eo =
  match eo with
    None -> raise LenOfArray
  | Some e -> begin
      match constFold true e with
      | Const(CInt64(ni, _, _)) when ni >= Int64.zero ->
          ni
      | _ -> raise LenOfArray
    end
let lenOfArray eo = Int64.to_int (lenOfArray64 eo)


(*** Make an initializer for zeroe-ing a data type ***)
let rec makeZeroInit (t: typ) : init =
  match unrollType t with
    TInt (ik, _) -> SingleInit (Const(CInt64(Int64.zero, ik, None)))
  | TFloat(fk, _) -> SingleInit(Const(CReal(0.0, fk, None)))
  | TEnum _ -> SingleInit zero
  | TComp (comp, _) as t' when comp.cstruct ->
      let inits =
        List.fold_right
          (fun f acc ->
            if f.fname <> missingFieldName then
              (Field(f, NoOffset), makeZeroInit f.ftype) :: acc
            else
              acc)
          comp.cfields []
      in
      CompoundInit (t', inits)

  | TComp (comp, _) when not comp.cstruct ->
      let fstfield, rest =
        match comp.cfields with
          f :: rest -> f, rest
        | [] -> E.s (unimp "Cannot create init for empty union")
      in
      let fieldToInit =
        if theMachine.msvcMode then
          (* ISO C99 [6.7.8.10] says that the first field of the union
             is the one we should initialize. *)
          fstfield
        else begin
          (* gcc initializes the whole union to zero.  So choose the largest
             field, and set that to zero.  Choose the first field if possible.
             MSVC also initializes the whole union, but use the ISO behavior
             for MSVC because it only allows compound initializers to refer
             to the first union field. *)
          let fieldSize f = try bitsSizeOf f.ftype with SizeOfError _ -> 0 in
          let widestField, _widestFieldWidth =
            List.fold_left (fun acc thisField ->
                              let _widestField, widestFieldWidth = acc in
                              let thisSize = fieldSize thisField in
                              if thisSize > widestFieldWidth then
                                thisField, thisSize
                              else
                                acc)
              (fstfield, fieldSize fstfield)
              rest
          in
          widestField
        end
      in
      CompoundInit(t, [(Field(fieldToInit, NoOffset),
                        makeZeroInit fieldToInit.ftype)])

  | TArray(bt, Some len, _) as t' ->
      let n =
        match constFold true len with
          Const(CInt64(n, _, _)) -> Int64.to_int n
        | _ -> E.s (E.unimp "Cannot understand length of array")
      in
      let initbt = makeZeroInit bt in
      let rec loopElems acc i =
        if i < 0 then acc
        else loopElems ((Index(integer i, NoOffset), initbt) :: acc) (i - 1)
      in
      CompoundInit(t', loopElems [] (n - 1))

  | TArray (_bt, None, _at) as t' ->
      (* Unsized array, allow it and fill it in later
       * (see cabs2cil.ml, collectInitializer) *)
      CompoundInit (t', [])

  | TPtr _ as t ->
      SingleInit(if theMachine.insertImplicitCasts then mkCast zero t else zero)
  | x -> E.s (unimp "Cannot initialize type: %a" d_type x)


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
    TArray(bt, leno, _) -> begin
      (* Scan the existing initializer *)
      let part =
        List.fold_left (fun acc (o, i) -> doinit o i bt acc) acc initl in
      (* See how many more we have to do *)
      match leno with
        Some lene when implicit -> begin
          match constFold true lene with
            Const(CInt64(i, _, _)) ->
              let len_array = Int64.to_int i in
              let len_init = List.length initl in
              if len_array > len_init then
                let zi = makeZeroInit bt in
                let rec loop acc i =
                  if i >= len_array then acc
                  else
                    loop (doinit (Index(integer i, NoOffset)) zi bt acc)
                         (i + 1)
                in
                loop part (len_init + 1)
              else
                part
          | _ -> E.s (unimp "foldLeftCompoundAll: array with initializer and non-constant length\n")
        end

      | _ when not implicit -> part

      | _ -> E.s (unimp "foldLeftCompoundAll: TArray with initializer and no length")
    end

  | TComp (_comp, _) ->
      let getTypeOffset = function
          Field(f, NoOffset) -> f.ftype
        | _ -> E.s (bug "foldLeftCompound: malformed initializer")
      in
      List.fold_left
        (fun acc (o, i) -> doinit o i (getTypeOffset o) acc) acc initl

  | _ -> E.s (E.unimp "Type of Compound is not array or struct or union")




let rec isCompleteType t =
  match unrollType t with
  | TArray(_t, None, _) -> false
  | TArray(_t, Some z, _) when isZero z -> false
  | TComp (comp, _) -> (* Struct or union *)
      List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields
  | _ -> true


module A = Alpha


(** Uniquefy the variable names *)
let uniqueVarNames (f: file) : unit =
  (* Setup the alpha conversion table for globals *)
  let gAlphaTable: (string,
                    location A.alphaTableData ref) H.t = H.create 113 in
  (* Keep also track of the global names that we have used. Map them to the
   * variable ID. We do this only to check that we do not have two globals
   * with the same name. *)
  let globalNames: (string, int) H.t = H.create 113 in
  (* Scan the file and add the global names to the table *)
  iterGlobals f
    (function
        GVarDecl(_,vi, _)
      | GVar(vi, _, _)
      | GFun({svar = vi}, _) ->
          (* See if we have used this name already for something else *)
          (try
            let oldid = H.find globalNames vi.vname in
            if oldid <> vi.vid && not vi.vinline then
              ignore (warn "The name %s is used for two distinct globals"
                        vi.vname)
            (* Here if we have used this name already. Go ahead *)
          with Not_found -> begin
            (* Here if this is the first time we define a name *)
            H.add globalNames vi.vname vi.vid;
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
              ignore (warn "uniqueVarNames: Changing the name of local %s in %s to %s (due to duplicate at %a)\n"
                        v.vname
                        fdec.svar.vname
                        newname d_loc oldloc);
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


(* A visitor that makes a deep copy of a function body *)
class copyFunctionVisitor (newname: string) = object
  inherit nopCilVisitor

      (* Keep here a maping from locals to their copies *)
  val map : (string, varinfo) H.t = H.create 113
      (* Keep here a maping from statements to their copies *)
  val stmtmap : (int, stmt) H.t = H.create 113
  (*val sid = ref 0 (* Will have to assign ids to statements *) *)
      (* Keep here a list of statements to be patched *)
  val patches : stmt list ref = ref []

  val argid = ref 0

      (* This is the main function *)
  method vfunc (f: fundec) : fundec visitAction =
    (* We need a map from the old locals/formals to the new ones *)
    H.clear map;
    argid := 0;
     (* Make a copy of the fundec. *)
    let f' = {f with svar = f.svar} in
    let patchfunction (f' : fundec) =
      (* Change the name. Only this late to allow the visitor to copy the
       * svar  *)
      f'.svar.vname <- newname;
      let findStmt (i: int) =
        try H.find stmtmap i
        with Not_found -> E.s (bug "Cannot find the copy of stmt#%d" i)
      in
      let patchstmt (s: stmt) =
        match s.skind with
          Goto (sr, l) ->
            (* Make a copy of the reference *)
            let sr' = ref (findStmt !sr.sid) in
            s.skind <- Goto (sr',l)
        | Switch (e, body, cases, l) ->
            s.skind <- Switch (e, body,
                               List.map (fun cs -> findStmt cs.sid) cases, l)
        | _ -> ()
      in
      List.iter patchstmt !patches;
      f'
    in
    patches := [];
(*    sid := 0; *)
    H.clear stmtmap;
    ChangeDoChildrenPost (f', patchfunction)

  (* We must create a new varinfo for each declaration. Memoize to
   * maintain sharing *)
  method vvdec (v: varinfo) =
    (* Some varinfo have empty names. Give them some name *)
    if v.vname = "" then begin
      v.vname <- "arg" ^ string_of_int !argid; incr argid
    end;
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> begin
      let v' = copy_with_new_vid v in
      H.add map v.vname v';
      ChangeDoChildrenPost (v', fun x -> x)
    end

  (* We must replace references to local variables *)
  method vvrbl (v: varinfo) =
    if v.vglob then SkipChildren else
    try
      ChangeTo (H.find map v.vname)
    with Not_found ->
      E.s (bug "Cannot find the new copy of local variable %s" v.vname)


        (* Replace statements. *)
  method vstmt (s: stmt) : stmt visitAction =
    s.sid <- Sid.next ();
    let s' = {s with sid = s.sid} in
    H.add stmtmap s.sid s'; (* Remember where we copied this *)
    (* if we have a Goto or a Switch remember them to fixup at end *)
    (match s'.skind with
      (Goto _ | Switch _) -> patches := s' :: !patches
    | _ -> ());
    (* Do the children *)
    ChangeDoChildrenPost (s', fun x -> x)

      (* Copy blocks since they are mutable *)
  method vblock (b: block) =
    ChangeDoChildrenPost ({b with bstmts = b.bstmts}, fun x -> x)


  method vglob _ = E.s (bug "copyFunction should not be used on globals")
end

(* We need a function that copies a CIL function. *)
let copyFunction (f: fundec) (newname: string) : fundec =
  visitCilFunction (new copyFunctionVisitor(newname)) f

(********* Compute the CFG ********)

let statements : stmt list ref = ref []
(* Clear all info about the CFG in statements *)
class clear : cilVisitor = object
  inherit nopCilVisitor
  method vstmt s = begin
    s.sid <- Sid.next ();
    statements := s :: !statements;
    s.succs <- [] ;
    s.preds <- [] ;
    DoChildren
  end
  method vexpr _ = SkipChildren
  method vtype _ = SkipChildren
  method vinst _ = SkipChildren
end

let link source dest = begin
  if not (List.mem dest source.succs) then
    source.succs <- dest :: source.succs ;
  if not (List.mem source dest.preds) then
    dest.preds <- source :: dest.preds
end
let trylink source dest_option = match dest_option with
  None -> ()
| Some(dest) -> link source dest


(** Compute the successors and predecessors of a block, given a fallthrough *)
let rec succpred_block b fallthrough =
  let rec handle sl = match sl with
    [] -> ()
  | [a] -> succpred_stmt a fallthrough
  | hd :: ((next :: _) as tl) ->
      succpred_stmt hd (Some next) ;
      handle tl
  in handle b.bstmts


and succpred_stmt s fallthrough =
  match s.skind with
    Instr _ -> trylink s fallthrough
  | Return _ -> ()
  | Goto(dest,_) -> link s !dest
  | Break _
  | Continue _
  | Switch _ ->
    failwith "computeCFGInfo: cannot be called on functions with break, continue or switch statements. Use prepareCFG first to remove them."

  | If(_e1,b1,b2,_) ->
      (match b1.bstmts with
        [] -> trylink s fallthrough
      | hd :: _ -> (link s hd ; succpred_block b1 fallthrough )) ;
      (match b2.bstmts with
        [] -> trylink s fallthrough
      | hd :: _ -> (link s hd ; succpred_block b2 fallthrough ))

  | Loop(_,b,_,_,_) ->
      begin match b.bstmts with
        [] -> failwith "computeCFGInfo: empty loop"
      | hd :: _ ->
          link s hd ;
          succpred_block b (Some(hd))
      end

  | Block(b) -> begin match b.bstmts with
                  [] -> trylink s fallthrough
                | hd :: _ -> link s hd ;
                    succpred_block b fallthrough
                end
  | UnspecifiedSequence (((s1,_,_)::_) as seq) ->
      link s s1;
      succpred_block (block_from_unspecified_sequence seq) fallthrough
  | UnspecifiedSequence [] ->
      trylink s fallthrough
  | TryExcept _ | TryFinally _ ->
      failwith "computeCFGInfo: structured exception handling not implemented"

(* [weimer] Sun May  5 12:25:24 PDT 2002
 * This code was pulled from ext/switch.ml because it looks like we really
 * want it to be part of CIL.
 *
 * Here is the magic handling to
 *  (1) replace switch statements with if/goto
 *  (2) remove "break"
 *  (3) remove "default"
 *  (4) remove "continue"
 *)
let is_case_label l = match l with
  | Case _ | Default _ -> true
  | _ -> false

let switch_count = ref (-1)
let get_switch_count () =
  switch_count := 1 + !switch_count ;
  !switch_count

let switch_label = ref (-1)

let rec xform_switch_stmt
    ?(keepSwitch=false) s break_dest cont_dest label_index = begin
  if not keepSwitch then
    s.labels <- List.map (fun lab -> match lab with
      Label _ -> lab
    | Case(e,l) ->
	let suffix =
	  match isInteger e with
	  | Some value ->
	      if value < Int64.zero then
		"neg_" ^ Int64.to_string (Int64.neg value)
	      else
		Int64.to_string value
	  | None ->
	      incr switch_label;
	      "exp_" ^ string_of_int !switch_label
	in
	let str = Pretty.sprint miscState.lineLength
	  (Pretty.dprintf "switch_%d_%s" label_index suffix) in
	(Label(str,l,false))
    | Default(l) -> (Label(Printf.sprintf
        "switch_%d_default" label_index,l,false))
    ) s.labels ;
  match s.skind with
  | Instr _ | Return _ | Goto _  -> ()
  | Break(l) -> begin try
                  s.skind <- Goto(break_dest (),l)
                with e ->
                  ignore (error "@[prepareCFG: break: %a@\n@]" d_stmt s) ;
                  raise e
                end
  | Continue(l) -> begin try
                  s.skind <- Goto(cont_dest (),l)
                with e ->
                  ignore (error "@[prepareCFG: continue: %a@\n@]" d_stmt s) ;
                  raise e
                end
  | If(_e,b1,b2,_) ->
      xform_switch_block ~keepSwitch b1 break_dest cont_dest label_index ;
      xform_switch_block ~keepSwitch b2 break_dest cont_dest label_index
  | Switch(e,b,sl,l) ->
      if keepSwitch then
	let i = get_switch_count () in
	let break_stmt = mkStmt (Instr (Skip locUnknown)) in
	break_stmt.labels <-
	  [Label((Printf.sprintf "switch_%d_break" i),l,false)] ;
	let switch_stmt = mkStmt s.skind in
	let break_block = mkBlock [ switch_stmt; break_stmt ] in
	s.skind <- Block break_block;
	xform_switch_block ~keepSwitch b (fun () -> ref break_stmt) cont_dest i
      else begin
      (* change
       * switch (se) {
       *   case 0: s0 ;
       *   case 1: s1 ; break;
       *   ...
       * }
       *
       * into:
       *
       * if (se == 0) goto label_0;
       * else if (se == 1) goto label_1;
       * ...
       * else if (0) { // body_block
       *  label_0: s0;
       *  label_1: s1; goto label_break;
       *  ...
       * } else if (0) { // break_block
       *  label_break: ; // break_stmt
       * }
       *)
      let i = get_switch_count () in
      let break_stmt = mkStmt (Instr (Skip locUnknown)) in
      break_stmt.labels <-
				[Label((Printf.sprintf "switch_%d_break" i),l,false)] ;
      let break_block = mkBlock [ break_stmt ] in
      let body_block = b in
      let body_if_stmtkind = (If(zero,body_block,break_block,l)) in

      (* The default case, if present, must be used only if *all*
      non-default cases fail [ISO/IEC 9899:1999, §6.8.4.2, ¶5]. As a
      result, we sort the order in which we handle the labels (but not the
      order in which we print out the statements, so fall-through still
      works as expected). *)
      let compare_choices s1 s2 = match s1.labels, s2.labels with
      | (Default(_) :: _), _ -> 1
      | _, (Default(_) :: _) -> -1
      | _, _ -> 0
      in

      let rec handle_choices sl = match sl with
        [] -> body_if_stmtkind
      | stmt_hd :: stmt_tl -> begin
        let rec handle_labels lab_list = begin
          match lab_list with
            [] -> handle_choices stmt_tl
          | Case(ce,cl) :: lab_tl ->
              let pred = BinOp(Eq,e,ce,intType) in
              let then_block = mkBlock [ mkStmt (Goto(ref stmt_hd,cl)) ] in
              let else_block = mkBlock [ mkStmt (handle_labels lab_tl) ] in
              If(pred,then_block,else_block,cl)
          | Default(dl) :: lab_tl ->
              (* ww: before this was 'if (1) goto label', but as Ben points
              out this might confuse someone down the line who doesn't have
              special handling for if(1) into thinking that there are two
              paths here. The simpler 'goto label' is what we want. *)
              Block(mkBlock [ mkStmt (Goto(ref stmt_hd,dl)) ;
                              mkStmt (handle_labels lab_tl) ])
          | Label(_,_,_) :: lab_tl -> handle_labels lab_tl
        end in
        handle_labels stmt_hd.labels
      end in
      s.skind <- handle_choices (List.sort compare_choices sl) ;
      xform_switch_block ~keepSwitch b (fun () -> ref break_stmt) cont_dest i
    end
  | Loop(a,b,l,_,_) ->
          let i = get_switch_count () in
          let break_stmt = mkStmt (Instr (Skip locUnknown)) in
          break_stmt.labels <-
						[Label((Printf.sprintf "while_%d_break" i),l,false)] ;
          let cont_stmt = mkStmt (Instr (Skip locUnknown)) in
          cont_stmt.labels <-
						[Label((Printf.sprintf "while_%d_continue" i),l,false)] ;
          b.bstmts <- cont_stmt :: b.bstmts ;
          let this_stmt = mkStmt
            (Loop(a,b,l,Some(cont_stmt),Some(break_stmt))) in
          let break_dest () = ref break_stmt in
          let cont_dest () = ref cont_stmt in
          xform_switch_block ~keepSwitch b break_dest cont_dest label_index ;
          break_stmt.succs <- s.succs ;
          let new_block = mkBlock [ this_stmt ; break_stmt ] in
          s.skind <- Block new_block
  | Block b ->
      xform_switch_block ~keepSwitch b break_dest cont_dest label_index
  | UnspecifiedSequence seq ->
      xform_switch_block ~keepSwitch
        (block_from_unspecified_sequence seq) break_dest cont_dest label_index
  | TryExcept _ | TryFinally _ ->
      failwith "xform_switch_statement: structured exception handling not implemented"

end and xform_switch_block
    ?(keepSwitch=false) b break_dest cont_dest label_index =
  try
    let rec link_succs sl = match sl with
    | [] -> ()
    | hd :: tl -> (if hd.succs = [] then hd.succs <- tl) ; link_succs tl
    in
    link_succs b.bstmts ;
    List.iter (fun stmt ->
      xform_switch_stmt ~keepSwitch stmt break_dest cont_dest label_index) b.bstmts ;
  with e ->
    List.iter (fun stmt -> ignore
      (warn "prepareCFG: %a@\n" d_stmt stmt)) b.bstmts ;
    raise e

(* prepare a function for computeCFGInfo by removing break, continue,
 * default and switch statements/labels and replacing them with Ifs and
 * Gotos. *)
let prepareCFG ?(keepSwitch=false) (fd : fundec) : unit =
  xform_switch_block ~keepSwitch fd.sbody
      (fun () -> failwith "prepareCFG: break with no enclosing loop")
      (fun () -> failwith "prepareCFG: continue with no enclosing loop") (-1)

(* make the cfg and return a list of statements *)
let computeCFGInfo (f : fundec) (global_numbering : bool) : unit =
  if not global_numbering then Sid.reset ();
  statements := [];
  let clear_it = new clear in
  ignore (visitCilBlock clear_it f.sbody) ;
  f.smaxstmtid <- Some (Sid.get ()) ;
  succpred_block f.sbody (None);
  let res = List.rev !statements in
  statements := [];
  f.sallstmts <- res;
  ()

let make_logic_var x typ =
  {lv_name = x; lv_id = new_raw_id(); lv_type = typ; lv_origin = None }

let initLogicBuiltins () =
  (* types *)
  Logic_env.add_builtin_logic_type "boolean" { nb_params=0 };
  Logic_env.add_builtin_logic_type "set" { nb_params=1 };
  (* constructors *)
  Logic_env.add_builtin_logic_ctor
    "\\true" { ctor_name = "\\true";
               ctor_type = Ltype("boolean",[]);
               ctor_params = []
             };
  Logic_env.add_builtin_logic_ctor
    "\\false"
    { ctor_name = "\\false";
      ctor_type = Ltype("boolean",[]);
      ctor_params = [] };
  (* functions *)
  let min = make_logic_var "min" Linteger in
  let max = make_logic_var "max" Linteger in
  let f = make_logic_var "f" (Larrow ([Linteger],Linteger)) in
  Logic_env.add_builtin_logic_function
    {
      l_name = "\\sum";
      l_type = Some Linteger;
      l_tparams = []; (* FIXME !!! *)
      l_profile = [ min; max; f ];
      l_labels = [];
      l_body = LBreads
	[ TSSingleton (TSLval (TSVar min,TSNoOffset));
          TSSingleton (TSLval (TSVar max,TSNoOffset));
          TSSingleton (TSLval (TSVar f, TSNoOffset)) ];
    };
  let min = make_logic_var "min" Linteger in
  let max = make_logic_var "max" Linteger in
  let f = make_logic_var "f" (Larrow ([Linteger],Linteger)) in
  Logic_env.add_builtin_logic_function
    {
      l_name = "\\product";
      l_type = Some Linteger;
      l_tparams = []; (* FIXME !!! *)
      l_profile = [ min; max; f ];
      l_labels = [];
      l_body = LBreads
	[ TSSingleton (TSLval (TSVar min,TSNoOffset));
          TSSingleton (TSLval (TSVar max,TSNoOffset));
          TSSingleton (TSLval (TSVar f, TSNoOffset)) ];
    };
  let min = make_logic_var "min" Linteger in
  let max = make_logic_var "max" Linteger in
  let f = make_logic_var "f" (Larrow ([Linteger],Linteger)) in
  Logic_env.add_builtin_logic_function
    {
      l_name = "\\min";
      l_type = Some Linteger;
      l_tparams = []; (* FIXME !!! *)
      l_profile = [ min; max; f ];
      l_labels = [];
      l_body = LBreads
	[ TSSingleton (TSLval (TSVar min,TSNoOffset));
          TSSingleton (TSLval (TSVar max,TSNoOffset));
          TSSingleton (TSLval (TSVar f, TSNoOffset)) ];
    };
  let min = make_logic_var "min" Linteger in
  let max = make_logic_var "max" Linteger in
  let f = make_logic_var "f" (Larrow ([Linteger],Linteger)) in
  Logic_env.add_builtin_logic_function
    {
      l_name = "\\max";
      l_type = Some Linteger;
      l_tparams = []; (* FIXME !!! *)
      l_profile = [ min; max; f ];
      l_labels = [];
      l_body = LBreads
	[ TSSingleton (TSLval (TSVar min,TSNoOffset));
          TSSingleton (TSLval (TSVar max,TSNoOffset));
          TSSingleton (TSLval (TSVar f, TSNoOffset)) ];
    };
  let min = make_logic_var "min" Linteger in
  let max = make_logic_var "max" Linteger in
  let f =
    make_logic_var "f"
      (Larrow ([Linteger],Ltype("boolean",[])))
  in
  Logic_env.add_builtin_logic_function
    {
      l_name = "\\numof";
      l_type = Some Linteger;
      l_tparams = []; (* FIXME !! *)
      l_profile = [ min; max; f ];
      l_labels = [];
      l_body = LBreads
	[ TSSingleton (TSLval (TSVar min,TSNoOffset));
          TSSingleton (TSLval (TSVar max,TSNoOffset));
          TSSingleton (TSLval (TSVar f, TSNoOffset)) ];
    }

let initCIL () =
  if not (TheMachine.is_computed ()) then begin

    (* Set the machine *)
    theMachine.theMachine <-
      if theMachine.msvcMode then Machdep.state.Machdep.msvc
      else Machdep.state.Machdep.gcc;
    (* Pick type for string literals *)
    theMachine.stringLiteralType <-
      if theMachine.theMachine.const_string_literals then
      charConstPtrType
    else
      charPtrType;
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
        E.s(E.unimp "initCIL: cannot find the right ikind for size %d\n" sz)
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
	E.s(E.unimp "initCIL: cannot find the right ikind for type %s\n" name)
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
    theMachine.enum_are_signed <-
      theMachine.theMachine.Cil_types.enum_are_signed;
    (* do not use lazy LAND and LOR *)
    theMachine.useLogicalOperators <- false;
    (*nextGlobalVID <- 1 ;
    nextCompinfoKey <- 1;*)

    (* Have to be mark before calling [init*Builtins] below. *)
    TheMachine.mark_as_computed ();

    if theMachine.msvcMode then
      initMsvcBuiltins ()
    else
      initGccBuiltins ();

    Logic_env.Builtins.extend initLogicBuiltins;

    (* projectify theMachine *)
    copyMachine theMachine !theMachineProject;

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

let hasPrefix p s =
  let pl = String.length p in
  (String.length s >= pl) && String.sub s 0 pl = p

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
  let loc = Instr.loc kinstr in
  fprintf fmt "Location: %a" d_loc loc

let pretty_loc_simply fmt kinstr =
  let loc = Instr.loc kinstr in
  fprintf fmt "%a" d_loc loc

let make_temp_logic_var =
  let counter = ref 0 in
  function ty ->
    incr counter;
    let name = "__framac_tmp" ^ (string_of_int !counter) in
    make_logic_var name ty

let extract_varinfos_from_exp vexp =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = C.VarinfoSet.empty;
    method varinfos = varinfos
    method vvrbl (symb:varinfo) =
      begin
        varinfos <- C.VarinfoSet.add symb varinfos;
        SkipChildren
      end
  end
  in ignore (visitCilExpr (visitor :> nopCilVisitor) vexp) ;
    visitor#varinfos

let extract_varinfos_from_lval vlval =
  let visitor = object
    inherit nopCilVisitor
    val mutable varinfos = C.VarinfoSet.empty;
    method varinfos = varinfos
    method vvrbl (symb:varinfo) =
      begin
        varinfos <- C.VarinfoSet.add symb varinfos;
        SkipChildren
      end
  end
  in ignore (visitCilLval (visitor :> nopCilVisitor) vlval) ;
    visitor#varinfos

let rec free_vars_term bound_vars t = match t.term_node with
  | TConst _   | TSizeOf _
  | TSizeOfStr _ | TAlignOf _
  | Tnull
  | Ttype _
    -> C.LogicVarSet.empty
  | TLval lv
  | TAddrOf lv
  | TStartOf lv
    -> free_vars_lval bound_vars lv
  | TSizeOfE t
  | TAlignOfE t
  | TUnOp (_,t)
  | TCastE (_,t)
  | Told t
  | Tat (t,_)
  | Tbase_addr t
  | Tblock_length t
  | TCoerce (t,_)
  | Ttypeof t
    -> free_vars_term bound_vars t
  | TBinOp (_,t1,t2)
  | TCoerceE (t1,t2)
  | TUpdate (t1,_,t2)
    -> C.LogicVarSet.union
      (free_vars_term bound_vars t1)
        (free_vars_term bound_vars t2)
  | Tif (t1,t2,t3) ->
      C.LogicVarSet.union
        (free_vars_term bound_vars t1)
        (C.LogicVarSet.union
           (free_vars_term bound_vars t2)
           (free_vars_term bound_vars t3))
  | TDataCons(_,t) | Tapp (_,_,t) ->
      List.fold_left
        (fun acc t -> C.LogicVarSet.union (free_vars_term bound_vars t) acc)
        C.LogicVarSet.empty t
  | Tlambda(prms,expr) ->
      let bound_vars =
        List.fold_left (Extlib.swap C.LogicVarSet.add) bound_vars prms
      in free_vars_term bound_vars expr
  | Ttsets ts -> free_vars_tsets bound_vars ts
and free_vars_lval bv (h,o) =
   C.LogicVarSet.union (free_vars_lhost bv h) (free_vars_term_offset bv o)
and free_vars_lhost bv = function
  | TVar log_v -> if C.LogicVarSet.mem log_v bv then C.LogicVarSet.empty else
      C.LogicVarSet.singleton log_v
  | TResult -> C.LogicVarSet.empty
  | TMem t -> free_vars_term bv t
and free_vars_term_offset bv = function
  | TNoOffset -> C.LogicVarSet.empty
  | TField (_,o) -> free_vars_term_offset bv o
  | TIndex (t,o) -> C.LogicVarSet.union (free_vars_term bv t) (free_vars_term_offset bv o)

and free_vars_tsets_offset bv = function
    TSNoOffset -> C.LogicVarSet.empty
  | TSIndex(t,o) ->
      C.LogicVarSet.union (free_vars_term bv t) (free_vars_tsets_offset bv o)
  | TSRange(i1,i2,o) ->
      let fv =
        match i1 with
            None -> C.LogicVarSet.empty
          | Some i -> free_vars_term bv i
      in
      let fv =
        match i2 with
            None -> fv
          | Some i -> C.LogicVarSet.union fv (free_vars_term bv i)
      in C.LogicVarSet.union fv (free_vars_tsets_offset bv o)
  | TSField(_,o) -> free_vars_tsets_offset bv o

and free_vars_tsets_elem bv = function
    TSLval lv | TSStartOf lv | TSAddrOf lv -> free_vars_tsets_lval bv lv
  | TSConst _ -> C.LogicVarSet.empty
  | TSAdd_index(t,i) ->
      C.LogicVarSet.union (free_vars_tsets_elem bv t) (free_vars_term bv i)
  | TSAdd_range(t,i1,i2) ->
      let fv = free_vars_tsets_elem bv t in
      let fv =
        match i1 with
            None -> fv
          | Some i -> C.LogicVarSet.union fv (free_vars_term bv i)
      in (match i2 with
              None -> fv
            | Some i -> C.LogicVarSet.union fv (free_vars_term bv i))
  | TSCastE(_,t) -> free_vars_tsets_elem bv t
  | TSat (t,_) -> free_vars_tsets_elem bv t
  | TSapp(_,_,args) ->
      List.fold_left
        (fun set arg -> C.LogicVarSet.union set (free_vars_term bv arg))
        C.LogicVarSet.empty args
and free_vars_tsets_lval bv (h,o) =
  C.LogicVarSet.union (free_vars_tsets_lhost bv h) (free_vars_tsets_offset bv o)

and free_vars_tsets_lhost bv = function
    TSVar v when C.LogicVarSet.mem v bv -> C.LogicVarSet.empty
  | TSVar v -> C.LogicVarSet.singleton v
  | TSResult -> C.LogicVarSet.empty
  | TSMem t -> free_vars_tsets_elem bv t

and free_vars_tsets bound_vars = function
    TSSingleton t -> free_vars_tsets_elem bound_vars t
  | TSEmpty -> C.LogicVarSet.empty
  | TSUnion l | TSInter l ->
      List.fold_left
        (fun acc t -> C.LogicVarSet.union (free_vars_tsets bound_vars t) acc)
        C.LogicVarSet.empty l
  | TSComprehension(t,q,p) ->
      let new_bv =
        List.fold_left (fun acc v -> C.LogicVarSet.add v acc) bound_vars q
      in
      let fv = free_vars_tsets new_bv t in
      match p with
          None -> fv
        | Some p -> C.LogicVarSet.union fv (free_vars_predicate new_bv p)

and free_vars_predicate bound_vars p = match p.content with
  | Pfalse | Ptrue -> C.LogicVarSet.empty

  | Papp (_,_,tl) ->
      List.fold_left
        (fun acc t -> C.LogicVarSet.union (free_vars_term bound_vars t) acc) C.LogicVarSet.empty tl
  | Pfresh t -> free_vars_term bound_vars t
  | Pvalid(t) -> free_vars_tsets bound_vars t
  | Pseparated seps ->
      List.fold_left
        (fun free_vars tset ->
           C.LogicVarSet.union (free_vars_tsets bound_vars tset) free_vars)
        C.LogicVarSet.empty seps
  | Prel (_,t1,t2)
  | Pvalid_index (t1,t2)
  | Psubtype (t1,t2)
      ->
      C.LogicVarSet.union
        (free_vars_term bound_vars t1)
        (free_vars_term bound_vars t2)
  | Pvalid_range (t1,t2,t3) ->
      C.LogicVarSet.union
        (C.LogicVarSet.union
           (free_vars_term bound_vars t1)
           (free_vars_term bound_vars t2))
        (free_vars_term bound_vars t3)
  | Pand (p1,p2)
  | Por (p1,p2)
  | Pxor (p1,p2)
  | Pimplies (p1,p2)
  | Piff (p1,p2) ->
      C.LogicVarSet.union
        (free_vars_predicate bound_vars p1)
        (free_vars_predicate bound_vars p2)
  | Pnot p
  | Pold p
  | Pat (p,_)
(*  | Pnamed (_,p) *) ->
      free_vars_predicate bound_vars p
  | Pif (t,p1,p2) ->
      C.LogicVarSet.union
        (free_vars_term bound_vars t)
        (C.LogicVarSet.union
           (free_vars_predicate bound_vars p1)
           (free_vars_predicate bound_vars p2))
  | Plet (log_v, t, p) ->
      let new_bv = C.LogicVarSet.add log_v bound_vars in
      C.LogicVarSet.union
        (free_vars_term new_bv t)
        (free_vars_predicate new_bv p)

  | Pforall (lvs,p)
  | Pexists (lvs,p) ->
      let new_bv= List.fold_left (Extlib.swap C.LogicVarSet.add) bound_vars lvs in
      free_vars_predicate new_bv p

let extract_free_logicvars_from_term t =
  free_vars_term C.LogicVarSet.empty t

let extract_free_logicvars_from_predicate p =
  free_vars_predicate C.LogicVarSet.empty p

let close_predicate p =
  let free_vars = free_vars_predicate C.LogicVarSet.empty p in
  if C.LogicVarSet.is_empty free_vars then p
  else
    {name = []; loc = p.loc;
    content = Pforall ((C.LogicVarSet.elements free_vars),p)}

class alpha_conv tbl ltbl =
object
  inherit nopCilVisitor
  method vvrbl v =
    try
      let v' = Hashtbl.find tbl v.vid in ChangeTo v'
    with Not_found -> DoChildren
  method vlogic_var_use v =
    try let v' = Hashtbl.find ltbl v.lv_id in ChangeTo v'
    with Not_found -> DoChildren
end

let create_alpha_renaming old_args new_args =
  let conversion = Hashtbl.create 7 in
  let lconversion = Hashtbl.create 7 in
  List.iter2 (fun old_vi new_vi ->
                Hashtbl.add conversion old_vi.vid new_vi;
                match old_vi.vlogic_var_assoc, new_vi.vlogic_var_assoc with
                    None, _ -> () (* nothing to convert in logic spec. *)
                  | Some old_lv, Some new_lv ->
                      Hashtbl.add lconversion old_lv.lv_id new_lv
                  | Some old_lv, None ->
                      Hashtbl.add lconversion old_lv.lv_id (cvar_to_lvar new_vi)
             )
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


exception Got of string list
let exists_attribute_deep f typ =
  let tbl = Hashtbl.create 7 in
  let rec visitor l = object
    inherit nopCilVisitor
    method vattr a =
      if f a then raise (Got l);
      DoChildren
    method vtype t =
      begin
        match unrollType t with
        | TComp (compinfo,_) ->
            (try Hashtbl.find tbl compinfo.ckey
            with Not_found ->
              Hashtbl.add tbl compinfo.ckey () ;
              List.iter
                (fun finfo -> ignore (visitCilType (visitor (finfo.fname::l)) finfo.ftype))
                compinfo.cfields)
        | _ -> ()
      end;
      DoChildren
  end
  in
  try
    ignore (visitCilType (visitor []) typ);
    None
  with Got l -> Some (List.rev l)

let get_status ca =
 match ca.annot_content with
 | AAssert (_,_,status) -> status
 | _ -> {status=Unknown}

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
