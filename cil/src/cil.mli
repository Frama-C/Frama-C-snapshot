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

(** CIL main API.

    CIL original API documentation is available as
    an html version at http://manju.cs.berkeley.edu/cil.

    @plugin development guide *)

open Cil_types
open Cil_datatype

(* ************************************************************************* *)
(** {2 Builtins management} *)
(* ************************************************************************* *)

(** This module associates the name of a built-in function that might be used
    during elaboration with the corresponding varinfo.  This is done when
    parsing ${FRAMAC_SHARE}/libc/__fc_builtins.h, which is always performed
    before processing the actual list of files provided on the command line (see
    {!File.init_from_c_files}).  Actual list of such built-ins is managed in
    {!Cabs2cil}. *)
module Frama_c_builtins: 
  State_builder.Hashtbl with type key = string and type data = Cil_types.varinfo

val is_builtin: Cil_types.varinfo -> bool
(** @return true if the given variable refers to a Frama-C builtin.
    @since Fluorine-20130401 *)

val is_unused_builtin: Cil_types.varinfo -> bool
(** @return true if the given variable refers to a Frama-C builtin that
    is not used in the current program. Plugins may (and in fact should)
    hide this builtin from their outputs *)

val is_special_builtin: string -> bool
(** @return [true] if the given name refers to a special built-in function.
    A special built-in function can have any number of arguments. It is up to
    the plug-ins to know what to do with it.
    @since Boron-20100401-dev *)

(** register a new special built-in function *)
val add_special_builtin: string -> unit

(** register a new family of special built-in functions.
    @since Carbon-20101201
*)
val add_special_builtin_family: (string -> bool) -> unit

(** initialize the C built-ins. Should be called once per project, after the
    machine has been set. *)
val init_builtins: unit -> unit

(** Description of the machine as seen in GCC and MSVC modes. *)
module type Machdeps = sig
  val gcc : Cil_types.mach 
  val msvc : Cil_types.mach  
end

(** Call this function to perform some initialization, and only after you have
    set [Cil.msvcMode]. [initLogicBuiltins] is the function to call to init
    logic builtins. The [Machdeps] argument is a description of the hardware
    platform and of the compiler used. *)
val initCIL: initLogicBuiltins:(unit -> unit) -> (module Machdeps) -> unit

(* ************************************************************************* *)
(** {2 Customization} *)
(* ************************************************************************* *)

type theMachine = private
    { (** Whether the pretty printer should print output for the MS VC
	  compiler. Default is GCC *)
      mutable msvcMode: bool;
      (** Whether to use the logical operands LAnd and LOr. By default, do not
	  use them because they are unlike other expressions and do not
	  evaluate both of their operands *)
      mutable useLogicalOperators: bool;
      mutable theMachine: mach;
      mutable lowerConstants: bool; (** Do lower constants (default true) *)
      mutable insertImplicitCasts: bool; (** Do insert implicit casts
					     (default true) *)
      (** Whether the compiler generates assembly labels by prepending "_" to
	  the identifier. That is, will function foo() have the label "foo", or
	  "_foo"? *)
      mutable underscore_name: bool;
      mutable stringLiteralType: typ;
      mutable upointKind: ikind (** An unsigned integer type that fits
                                    pointers. *);
      mutable upointType: typ;
      mutable wcharKind: ikind; (** An integer type that fits wchar_t. *)
      mutable wcharType: typ;
      mutable ptrdiffKind: ikind; (** An integer type that fits ptrdiff_t. *)
      mutable ptrdiffType: typ;
      mutable typeOfSizeOf: typ; (** An integer type that is the type of
				      sizeof. *)
      mutable kindOfSizeOf: ikind (** The integer kind of
				      {!Cil.typeOfSizeOf}. *)
    }

val theMachine : theMachine
  (** Current machine description *)

val selfMachine: State.t

val selfMachine_is_computed: ?project:Project.project -> unit -> bool
  (** whether current project has set its machine description. *)

val set_msvcMode: bool -> unit
  (** Must be called before {!Cil.initCIL}. *)

(** Styles of printing line directives *)
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
    { (** How to print line directives *)
      mutable lineDirectiveStyle: lineDirectiveStyle option;
      (** Whether we print something that will only be used as input to our own
	  parser. In that case we are a bit more liberal in what we print *)
      mutable print_CIL_Input: bool;
      (** Whether to print the CIL as they are, without trying to be smart and
	  print nicer code. Normally this is false, in which case the pretty
	  printer will turn the while(1) loops of CIL into nicer loops, will not
	  print empty "else" blocks, etc. These is one case howewer in which if
	  you turn this on you will get code that does not compile: if you use
	  varargs the __builtin_va_arg function will be printed in its internal
	  form. *)
      mutable printCilAsIs: bool;
      (** The length used when wrapping output lines. Setting this variable to
	  a large integer will prevent wrapping and make #line directives more
	  accurate. *)
      mutable lineLength: int;
      (** Emit warnings when truncating integer constants (default true) *)
      mutable warnTruncate: bool }

val miscState: miscState

(** To be able to add/remove features easily, each feature should be package
    as an interface with the following interface. *)
type featureDescr = {
    fd_enabled: bool ref;
    (** The enable flag. Set to default value  *)

    fd_name: string;
    (** This is used to construct an option "--doxxx" and "--dontxxx" that
     * enable and disable the feature  *)

    fd_description: string;
    (** A longer name that can be used to document the new options  *)

    fd_extraopt: (string * Arg.spec * string) list;
    (** Additional command line options *)

    fd_doit: (file -> unit);
    (** This performs the transformation *)

    fd_post_check: bool;
    (** Whether to perform a CIL consistency checking after this stage, if
     * checking is enabled (--check is passed to cilly). Set this to true if
     * your feature makes any changes for the program. *)
}

(* ************************************************************************* *)
(** {2 Values for manipulating globals} *)
(* ************************************************************************* *)

(** Make an empty function from an existing global varinfo.
    @since Nitrogen-20111001 *)
val emptyFunctionFromVI: varinfo -> fundec

(** Make an empty function *)
val emptyFunction: string -> fundec

(** Update the formals of a [fundec] and make sure that the function type
    has the same information. Will copy the name as well into the type. *)
val setFormals: fundec -> varinfo list -> unit

(** Takes as input a function type (or a typename on it) and return its
    return type. *)
val getReturnType: typ -> typ

(** Change the return type of the function passed as 1st argument to be
    the type passed as 2nd argument. *)
val setReturnTypeVI: varinfo -> typ -> unit
val setReturnType: fundec -> typ -> unit

(** Set the types of arguments and results as given by the function type
 * passed as the second argument. Will not copy the names from the function
 * type to the formals *)
val setFunctionType: fundec -> typ -> unit

(** Set the type of the function and make formal arguments for them *)
val setFunctionTypeMakeFormals: fundec -> typ -> unit

(** Update the smaxid after you have populated with locals and formals
 * (unless you constructed those using {!Cil.makeLocalVar} or
 * {!Cil.makeTempVar}. *)
val setMaxId: fundec -> unit

(** Strip const attribute from the type. This is useful for
    any type used as the type of a local variable which may be assigned.
    Note that the type attributes are mutated in place.
    @since Nitrogen-20111001 
*)
val stripConstLocalType : Cil_types.typ -> Cil_types.typ

val selfFormalsDecl: State.t
  (** state of the table associating formals to each prototype. *)

val makeFormalsVarDecl: (string * typ * attributes) -> varinfo
  (** creates a new varinfo for the parameter of a prototype. *)

(** Update the formals of a function declaration from its identifier and its
    type. For a function definition, use {!Cil.setFormals}.
    Do nothing if the type is not a function type or if the list of
    argument is empty.
 *)
val setFormalsDecl: varinfo -> typ -> unit

(** remove a binding from the table.
    @since Oxygen-20120901 *)
val removeFormalsDecl: varinfo -> unit

(** replace to formals of a function declaration with the given
    list of varinfo.
*)
val unsafeSetFormalsDecl: varinfo -> varinfo list -> unit

(** iters the given function on declared prototypes.
    @since Oxygen-20120901 
*)
val iterFormalsDecl: (varinfo -> varinfo list -> unit) -> unit

(** Get the formals of a function declaration registered with
    {!Cil.setFormalsDecl}.
    @raise Not_found if the function is not registered (this is in particular
    the case for prototypes with an empty list of arguments.
    See {!Cil.setFormalsDecl})
*)
val getFormalsDecl: varinfo -> varinfo list

(** A dummy file *)
val dummyFile: file

(** Get the global initializer and create one if it does not already exist.
    When it creates a global initializer it attempts to place a call to it in
    the main function named by the optional argument (default "main").
    @deprecated using this function is incorrect since it modifies the
    current AST (see Plug-in Development Guide, Section "Using Projects"). *)
val getGlobInit: ?main_name:string -> file -> fundec

(** Iterate over all globals, including the global initializer *)
val iterGlobals: file -> (global -> unit) -> unit

(** Fold over all globals, including the global initializer *)
val foldGlobals: file -> ('a -> global -> 'a) -> 'a -> 'a

(** Map over all globals, including the global initializer and change things
    in place *)
val mapGlobals: file -> (global -> global) -> unit

(** Find a function or function prototype with the given name in the file.
  * If it does not exist, create a prototype with the given type, and return
  * the new varinfo.  This is useful when you need to call a libc function
  * whose prototype may or may not already exist in the file.
  *
  * Because the new prototype is added to the start of the file, you shouldn't
  * refer to any struct or union types in the function type.*)
val findOrCreateFunc: file -> string -> typ -> varinfo

module Sid: sig
  val next: unit -> int
end

module Eid: sig
  val next: unit -> int
end

(** creates an expression with a fresh id *)
val new_exp: loc:location -> exp_node -> exp

(** performs a deep copy of an expression (especially, avoid eid sharing).
    @since Nitrogen-20111001
*)
val copy_exp: exp -> exp

(** creates an expression with a dummy id.
    Use with caution, {i i.e.} not on expressions that may be put in the AST.
*)
val dummy_exp: exp_node -> exp

(** Return [true] on case and default labels, [false] otherwise. *)
val is_case_label: label -> bool


(** CIL keeps the types at the beginning of the file and the variables at the
 * end of the file. This function will take a global and add it to the
 * corresponding stack. Its operation is actually more complicated because if
 * the global declares a type that contains references to variables (e.g. in
 * sizeof in an array length) then it will also add declarations for the
 * variables to the types stack *)
val pushGlobal: global -> types: global list ref
                       -> variables: global list ref -> unit

(** An empty statement. Used in pretty printing *)
val invalidStmt: stmt

(** A list of the built-in functions for the current compiler (GCC or
  * MSVC, depending on [!msvcMode]).  Maps the name to the
  * result and argument types, and whether it is vararg.
  * Initialized by {!Cil.initCIL}
  *
  * This map replaces [gccBuiltins] and [msvcBuiltins] in previous
  * versions of CIL.*)
module Builtin_functions :
  State_builder.Hashtbl with type key = string
			and type data = typ * typ list * bool

(** This is used as the location of the prototypes of builtin functions. *)
val builtinLoc: location

(** Returns a location that ranges over the two locations in arguments. *)
val range_loc: location -> location -> location

(* ************************************************************************* *)
(** {2 Values for manipulating initializers} *)
(* ************************************************************************* *)

(** Make a initializer for zero-ing a data type *)
val makeZeroInit: loc:location -> typ -> init

(** Fold over the list of initializers in a Compound (not also the nested
 * ones). [doinit] is called on every present initializer, even if it is of
 * compound type. The parameters of [doinit] are: the offset in the compound
 * (this is [Field(f,NoOffset)] or [Index(i,NoOffset)]), the initializer
 * value, expected type of the initializer value, accumulator. In the case of
 * arrays there might be missing zero-initializers at the end of the list.
 * These are scanned only if [implicit] is true. This is much like
 * [List.fold_left] except we also pass the type of the initializer.

 * This is a good way to use it to scan even nested initializers :
{v
  let rec myInit (lv: lval) (i: init) (acc: 'a) : 'a =
      match i with
        SingleInit e -> ... do something with lv and e and acc ...
      | CompoundInit (ct, initl) ->
         foldLeftCompound ~implicit:false
             ~doinit:(fun off' i' t' acc ->
                        myInit (addOffsetLval lv off') i' acc)
             ~ct:ct
             ~initl:initl
             ~acc:acc
v}
*)
val foldLeftCompound:
    implicit:bool ->
    doinit: (offset -> init -> typ -> 'a -> 'a) ->
    ct: typ ->
    initl: (offset * init) list ->
    acc: 'a -> 'a

(* ************************************************************************* *)
(** {2 Values for manipulating types} *)
(* ************************************************************************* *)

(** void *)
val voidType: typ

(** is the given type "void"? *)
val isVoidType: typ -> bool

(** is the given type "void *"? *)
val isVoidPtrType: typ -> bool

(** int *)
val intType: typ

(** unsigned int *)
val uintType: typ

(** long *)
val longType: typ

(** long long *)
val longLongType: typ

(** unsigned long *)
val ulongType: typ

(** unsigned long long *)
val ulongLongType: typ

(** Any unsigned integer type of size 16 bits.
    It is equivalent to the ISO C uint16_t type but without using the 
    corresponding header. 
    Shall not be called if not such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint16_t: unit -> typ

(** Any unsigned integer type of size 32 bits.
    It is equivalent to the ISO C uint32_t type but without using the 
    corresponding header. 
    Shall not be called if not such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint32_t: unit -> typ

(** Any unsigned integer type of size 64 bits.
    It is equivalent to the ISO C uint64_t type but without using the 
    corresponding header. 
    Shall not be called if no such type exists in the current architecture.
    @since Nitrogen-20111001
*)
val uint64_t: unit -> typ

(** char *)
val charType: typ
val scharType: typ
val ucharType: typ

(** char * *)
val charPtrType: typ
val scharPtrType: typ
val ucharPtrType: typ

(** char const * *)
val charConstPtrType: typ

(** void * *)
val voidPtrType: typ

(** void const * *)
val voidConstPtrType: typ

(** int * *)
val intPtrType: typ

(** unsigned int * *)
val uintPtrType: typ

(** float *)
val floatType: typ

(** double *)
val doubleType: typ

(** long double *)
val longDoubleType: typ

(** @return true if and only if the given type is a signed integer type. *)
val isSignedInteger: typ -> bool

(** @return true if and only if the given type is an unsigned integer type.
    @since Oxygen-20120901 *)
val isUnsignedInteger: typ -> bool


(** Creates a a (potentially recursive) composite type. The arguments are:
 * (1) a boolean indicating whether it is a struct or a union, (2) the name
 * (always non-empty), (3) a function that when given a representation of the
 * structure type constructs the type of the fields recursive type (the first
 * argument is only useful when some fields need to refer to the type of the
 * structure itself), and (4) a list of attributes to be associated with the
 * composite type. The resulting compinfo has the field "cdefined" only if
 * the list of fields is non-empty. *)
val mkCompInfo: bool ->      (* whether it is a struct or a union *)
               string -> (* name of the composite type; cannot be empty *)
               ?norig:string -> (* original name of the composite type, empty when anonymous *)
               (compinfo ->
                  (string * typ * int option * attributes * location) list) ->
               (* a function that when given a forward
                  representation of the structure type constructs the type of
                  the fields. The function can ignore this argument if not
                  constructing a recursive type.  *)
               attributes -> compinfo

(** Makes a shallow copy of a {!Cil_types.compinfo} changing the name and the key.*)
val copyCompInfo: compinfo -> string -> compinfo

(** This is a constant used as the name of an unnamed bitfield. These fields
    do not participate in initialization and their name is not printed. *)
val missingFieldName: string

(** Get the full name of a comp *)
val compFullName: compinfo -> string

(** Returns true if this is a complete type.
   This means that sizeof(t) makes sense.
   Incomplete types are not yet defined
   structures and empty arrays. *)
val isCompleteType: typ -> bool

(** Unroll a type until it exposes a non
 * [TNamed]. Will collect all attributes appearing in [TNamed]!!! *)
val unrollType: typ -> typ

(** Unroll all the TNamed in a type (even under type constructors such as
 * [TPtr], [TFun] or [TArray]. Does not unroll the types of fields in [TComp]
 * types. Will collect all attributes *)
val unrollTypeDeep: typ -> typ

(** Separate out the storage-modifier name attributes *)
val separateStorageModifiers: attribute list -> attribute list * attribute list

(** returns the type of the result of an arithmetic operator applied to
    values of the corresponding input types.
    @since Nitrogen-20111001 (moved from Cabs2cil)
*)
val arithmeticConversion : Cil_types.typ -> Cil_types.typ -> Cil_types.typ

(** performs the usual integral promotions mentioned in C reference manual.
    @since Nitrogen-20111001 (moved from Cabs2cil)
*)
val integralPromotion : Cil_types.typ -> Cil_types.typ

(** True if the argument is a character type (i.e. plain, signed or unsigned) *)
val isCharType: typ -> bool

(** True if the argument is a short type (i.e. signed or unsigned) *)
val isShortType: typ -> bool

(** True if the argument is a pointer to a character type
    (i.e. plain, signed or unsigned) *)
val isCharPtrType: typ -> bool

(** True if the argument is an array of a character type
    (i.e. plain, signed or unsigned) *)
val isCharArrayType: typ -> bool

(** True if the argument is an integral type (i.e. integer or enum) *)
val isIntegralType: typ -> bool

(** True if the argument is an integral type (i.e. integer or enum), either
    C or mathematical one *)
val isLogicIntegralType: logic_type -> bool

(** True if the argument is a floating point type *)
val isFloatingType: typ -> bool

(** True if the argument is a floating point type *)
val isLogicFloatType: logic_type -> bool

(** True if the argument is a C floating point type or logic 'real' type *)
val isLogicRealOrFloatType: logic_type -> bool

(** True if the argument is the logic 'real' type *)
val isLogicRealType: logic_type -> bool

(** True if the argument is an arithmetic type (i.e. integer, enum or
    floating point *)
val isArithmeticType: typ -> bool

(** True if the argument is an arithmetic or pointer type (i.e. integer, enum,
    floating point or pointer *)
val isArithmeticOrPointerType: typ -> bool

(** True if the argument is a logic arithmetic type (i.e. integer, enum or
    floating point, either C or mathematical one *)
val isLogicArithmeticType: logic_type -> bool

(** True if the argument is a pointer type *)
val isPointerType: typ -> bool

(** True if the argument is the type for reified C types *)
val isTypeTagType: logic_type -> bool

(** True if the argument is a function type. *)
val isFunctionType: typ -> bool

(** True if the argument denotes the type of ... in a variadic function.
    @since Nitrogen-20111001 moved from cabs2cil *)
val isVariadicListType: typ -> bool

(** Obtain the argument list ([] if None) *)
val argsToList:
  (string * typ * attributes) list option -> (string * typ * attributes) list

(** True if the argument is an array type *)
val isArrayType: typ -> bool

(** True if the argument is a struct of union type *)
val isStructOrUnionType: typ -> bool

(** Raised when {!Cil.lenOfArray} fails either because the length is [None]
  * or because it is a non-constant expression *)
exception LenOfArray

(** Call to compute the array length as present in the array type, to an
  * integer. Raises {!Cil.LenOfArray} if not able to compute the length, such
  * as when there is no length or the length is not a constant. *)
val lenOfArray: exp option -> int
val lenOfArray64: exp option -> Integer.t

(** Return a named fieldinfo in compinfo, or raise Not_found *)
val getCompField: compinfo -> string -> fieldinfo


(** A datatype to be used in conjunction with [existsType] *)
type existsAction =
    ExistsTrue                          (** We have found it *)
  | ExistsFalse                         (** Stop processing this branch *)
  | ExistsMaybe                         (** This node is not what we are
                                         * looking for but maybe its
                                         * successors are *)

(** Scans a type by applying the function on all elements.
    When the function returns ExistsTrue, the scan stops with
    true. When the function returns ExistsFalse then the current branch is not
    scanned anymore. Care is taken to
    apply the function only once on each composite type, thus avoiding
    circularity. When the function returns ExistsMaybe then the types that
    construct the current type are scanned (e.g. the base type for TPtr and
    TArray, the type of fields for a TComp, etc). *)
val existsType: (typ -> existsAction) -> typ -> bool


(** Given a function type split it into return type,
 * arguments, is_vararg and attributes. An error is raised if the type is not
 * a function type *)
val splitFunctionType:
    typ -> typ * (string * typ * attributes) list option * bool * attributes
(** Same as {!Cil.splitFunctionType} but takes a varinfo. Prints a nicer
 * error message if the varinfo is not for a function *)
val splitFunctionTypeVI:
  varinfo ->
  typ * (string * typ * attributes) list option * bool * attributes


(*********************************************************)
(**  LVALUES *)

(** Make a varinfo. Use this (rarely) to make a raw varinfo. Use other
 * functions to make locals ({!Cil.makeLocalVar} or {!Cil.makeFormalVar} or
 * {!Cil.makeTempVar}) and globals ({!Cil.makeGlobalVar}). Note that this
 * function will assign a new identifier.
 * The [logic] argument defaults to [false]
 * and should be used to create a varinfo such that [varinfo.vlogic=true].
 * The [generated] argument defaults to [true] (in fact, only front-ends have
 * the need to set it to false), and tells whether the variable is generated
 * or comes directly from user input (the [vgenerated] flag).
 * The first unnmamed argument specifies whether the varinfo is for a global and
 * the second is for formals. *)
val makeVarinfo:
  ?logic:bool -> ?generated:bool -> bool -> bool -> string -> typ -> varinfo

(** Make a formal variable for a function declaration. Insert it in both the
    sformals and the type of the function. You can optionally specify where to
    insert this one. If where = "^" then it is inserted first. If where = "$"
    then it is inserted last. Otherwise where must be the name of a formal
    after which to insert this. By default it is inserted at the end.
    A formal var is never generated.
*)
val makeFormalVar: fundec -> ?where:string -> string -> typ -> varinfo

(** Make a local variable and add it to a function's slocals and to the given
    block (only if insert = true, which is the default).
    Make sure you know what you are doing if you set insert=false.
    [generated] is passed to {!Cil.makeVarinfo}.
    The variable is attached to the toplevel block if [scope] is not specified.

    @since Nitrogen-20111001 This function will strip const attributes
    of its type in place in order for local variable to be assignable at
    least once.
*)
val makeLocalVar:
  fundec -> ?scope:block -> ?generated:bool -> ?insert:bool
  -> string -> typ -> varinfo

(** Make a pseudo-variable to use as placeholder in term to expression
    conversions. Its logic field is set. They are always generated. *)
val makePseudoVar: typ -> varinfo

(** Make a temporary variable and add it to a function's slocals. The name of
    the temporary variable will be generated based on the given name hint so
    that to avoid conflicts with other locals.
    Optionally, you can give the variable a description of its contents.
    Temporary variables are always considered as generated variables.
    If [insert] is true (the default), the variable will be inserted
    among other locals of the function. The value for [insert] should
    only be changed if you are completely sure this is not useful.
 *)
val makeTempVar: fundec -> ?insert:bool -> ?name:string -> ?descr:string ->
                 ?descrpure:bool -> typ -> varinfo

(** Make a global variable. Your responsibility to make sure that the name
    is unique. [logic] defaults to [false]. [generated] defaults to [true].*)
val makeGlobalVar: ?logic:bool -> ?generated:bool -> string -> typ -> varinfo

(** Make a shallow copy of a [varinfo] and assign a new identifier.
    If the original varinfo has an associated logic var, it is copied too and
    associated to the copied varinfo
 *)
val copyVarinfo: varinfo -> string -> varinfo

(** Changes the type of a varinfo and of its associated logic var if any.
    @since Neon-20130301 *)
val update_var_type: varinfo -> typ -> unit

(** Is an lvalue a bitfield? *)
val isBitfield: lval -> bool

(** Returns the last offset in the chain. *)
val lastOffset: offset -> offset

(** Add an offset at the end of an lvalue. Make sure the type of the lvalue
 * and the offset are compatible. *)
val addOffsetLval: offset -> lval -> lval

(** [addOffset o1 o2] adds [o1] to the end of [o2]. *)
val addOffset:     offset -> offset -> offset

(** Equivalent to [lastOffset] for terms.
        @deprecated Oxygen-20120901  use Logic_const.addTermOffsetLval *)
val lastTermOffset: term_offset -> term_offset

(** Equivalent to [addOffsetLval] for terms.
        @deprecated Oxygen-20120901  use Logic_const.addTermOffsetLval *)
val addTermOffsetLval: term_offset -> term_lval -> term_lval

(** Equivalent to [addOffset] for terms.
        @deprecated Oxygen-20120901  use Logic_const. *)
val addTermOffset:     term_offset -> term_offset -> term_offset

(** Remove ONE offset from the end of an lvalue. Returns the lvalue with the
 * trimmed offset and the final offset. If the final offset is [NoOffset]
 * then the original [lval] did not have an offset. *)
val removeOffsetLval: lval -> lval * offset

(** Remove ONE offset from the end of an offset sequence. Returns the
 * trimmed offset and the final offset. If the final offset is [NoOffset]
 * then the original [lval] did not have an offset. *)
val removeOffset:   offset -> offset * offset

(** Compute the type of an lvalue *)
val typeOfLval: lval -> typ

(** Compute the type of an lhost (with no offset) *)
val typeOfLhost: lhost -> typ

(** Equivalent to [typeOfLval] for terms. *)
val typeOfTermLval: term_lval -> logic_type

(** Compute the type of an offset from a base type *)
val typeOffset: typ -> offset -> typ

(** Equivalent to [typeOffset] for terms. *)
val typeTermOffset: logic_type -> term_offset -> logic_type

(** Compute the type of an initializer *)
val typeOfInit: init -> typ

(* ************************************************************************* *)
(** {2 Values for manipulating expressions} *)
(* ************************************************************************* *)

(* Construct integer constants *)

(** 0 *)
val zero: loc:Location.t -> exp

(** 1 *)
val one: loc:Location.t -> exp

(** -1 *)
val mone: loc:Location.t -> exp


(** Construct an integer of a given kind, using OCaml's int64 type. If needed
  * it will truncate the integer to be within the representable range for the
  * given kind. The integer can have an optional literal representation. *)
val kinteger64_repr: loc:location -> ikind -> Integer.t -> string option -> exp

(** Construct an integer of a given kind without literal representation. *)
val kinteger64: loc:location -> ikind -> Integer.t -> exp

(** Construct an integer of a given kind. Converts the integer to int64 and
  * then uses kinteger64. This might truncate the value if you use a kind
  * that cannot represent the given integer. This can only happen for one of
  * the Char or Short kinds *)
val kinteger: loc:location -> ikind -> int -> exp

(** Construct an integer of kind IInt. You can use this always since the
    OCaml integers are 31 bits and are guaranteed to fit in an IInt *)
val integer: loc:location -> int -> exp

(** Constructs a floating point constant.
    @since Oxygen-20120901 
*)
val kfloat: loc:location -> fkind -> float -> exp

(** True if the given expression is a (possibly cast'ed)
    character or an integer constant *)
val isInteger: exp -> Integer.t option

(** True if the expression is a compile-time constant *)
val isConstant: exp -> bool

(** True if the expression is a compile-time integer constant *)
val isIntegerConstant: exp -> bool

(** True if the given offset contains only field nanmes or constant indices. *)
val isConstantOffset: offset -> bool

(** True if the given expression is a (possibly cast'ed) integer or character
    constant with value zero *)
val isZero: exp -> bool

(** True if the term is the constant 0 *)
val isLogicZero: term -> bool

(** True if the given term is [\null] or a constant null pointer*)
val isLogicNull: term -> bool

(** gives the value of a wide char literal. *)
val reduce_multichar: Cil_types.typ -> int64 list -> int64

(** gives the value of a char literal. *)
val interpret_character_constant:
  int64 list -> Cil_types.constant * Cil_types.typ

(** Given the character c in a (CChr c), sign-extend it to 32 bits.
  (This is the official way of interpreting character constants, according to
  ISO C 6.4.4.4.10, which says that character constants are chars cast to ints)
  Returns CInt64(sign-extened c, IInt, None) *)
val charConstToInt: char -> constant

(** Do constant folding on an expression. If the first argument is [true] then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!Cil.constFoldVisitor}, which will run constFold on all
    expressions in a given AST node. *)
val constFold: bool -> exp -> exp

(** Do constant folding on an term at toplevel only.
    This uses compiler-dependent informations and will
    remove all sizeof and alignof. *)
val constFoldTermNodeAtTop:  term_node -> term_node

(** Do constant folding on an term at toplevel only.
    If the first argument is true then
    will also compute compiler-dependent expressions such as [sizeof]
    and [alignof]. *)
val constFoldTerm: bool -> term -> term

(** Do constant folding on a binary operation. The bulk of the work done by
    [constFold] is done here. If the second argument is true then
    will also compute compiler-dependent expressions such as [sizeof]. *)
val constFoldBinOp: loc:location -> bool -> binop -> exp -> exp -> typ -> exp

(** [true] if the two constant are equal.
    @since Nitrogen-20111001
*)
val compareConstant: constant -> constant -> bool

(** [true] if the two expressions are syntactically the same. 
    @deprecated Oxygen-20120901 use {!Cil_datatype.ExpStructEq.compare}
*)
val compareExp: exp -> exp -> bool

(** [true] if the two lval are syntactically the same. 
    @deprecated Oxygen-20120901 use {!Cil_datatype.LvalStructEq.compare}
*)
val compareLval: lval -> lval -> bool

(** [true] if the two offsets are syntactically the same. 
    @deprecated Oxygen-20120901 use {!Cil_datatype.OffsetStructEq.compare}
*)
val compareOffset: offset -> offset -> bool

(** Increment an expression. Can be arithmetic or pointer type *)
val increm: exp -> int -> exp

(** Increment an expression. Can be arithmetic or pointer type *)
val increm64: exp -> Integer.t -> exp

(** Makes an lvalue out of a given variable *)
val var: varinfo -> lval

(** Creates an expr representing the variable. 
    @since Nitrogen-20111001
 *)
val evar: ?loc:location -> varinfo -> exp

(** Make an AddrOf. Given an lvalue of type T will give back an expression of
    type ptr(T). It optimizes somewhat expressions like "& v" and "& v[0]"  *)
val mkAddrOf: loc:location -> lval -> exp

(** Creates an expression corresponding to "&v".
    @since Oxygen-20120901 *)
val mkAddrOfVi: varinfo -> exp

(** Like mkAddrOf except if the type of lval is an array then it uses
    StartOf. This is the right operation for getting a pointer to the start
    of the storage denoted by lval. *)
val mkAddrOrStartOf: loc:location -> lval -> exp

(** Make a Mem, while optimizing AddrOf. The type of the addr must be
    TPtr(t) and the type of the resulting lval is t. Note that in CIL the
    implicit conversion between an array and the pointer to the first
    element does not apply. You must do the conversion yourself using
    StartOf *)
val mkMem: addr:exp -> off:offset -> lval

(** makes a binary operation and performs const folding.  Inserts
    casts between arithmetic types as needed, or between pointer
    types, but do not attempt to cast pointer to int or
    vice-versa. Use appropriate binop (PlusPI & friends) for that.  *)
val mkBinOp: loc:location -> binop -> exp -> exp -> exp

(** Equivalent to [mkMem] for terms. *)
val mkTermMem: addr:term -> off:term_offset -> term_lval

(** Make an expression that is a string constant (of pointer type) *)
val mkString: loc:location -> string -> exp

(** [true] if both types are not equivalent. 
    if [force] is [true], returns [true] whenever both types are not equal
    (modulo typedefs). If [force] is [false] (the default), other equivalences
    are considered, in particular between an enum and its representative
    integer type.
    @modify Fluorine-20130401 added [force] argument
*)
val need_cast: ?force:bool -> typ -> typ -> bool

(** Construct a cast when having the old type of the expression. If the new
    type is the same as the old type, then no cast is added, unless [force]
    is [true] (default is [false])
    @modify Fluorine-20130401 add [force] argument
 *)
val mkCastT: ?force:bool -> e:exp -> oldt:typ -> newt:typ -> exp

(** Like {!Cil.mkCastT} but uses typeOf to get [oldt] *)
val mkCast: ?force:bool -> e:exp -> newt:typ -> exp

(** Equivalent to [stripCasts] for terms. *)
val stripTermCasts: term -> term

(** Removes casts from this expression, but ignores casts within
  other expression constructs.  So we delete the (A) and (B) casts from
  "(A)(B)(x + (C)y)", but leave the (C) cast. *)
val stripCasts: exp -> exp

(** Removes info wrappers and return underlying expression *)
val stripInfo: exp -> exp

(** Removes casts and info wrappers and return underlying expression *)
val stripCastsAndInfo: exp -> exp

(** Removes casts and info wrappers,except last info wrapper, and return
    underlying expression *)
val stripCastsButLastInfo: exp -> exp

(** Extracts term information in an expression information *)
val exp_info_of_term: term -> exp_info

(** Constructs a term from a term node and an expression information *)
val term_of_exp_info: location -> term_node -> exp_info -> term

(** Map some function on underlying expression if Info or else on expression *)
val map_under_info: (exp -> exp) -> exp -> exp

(** Apply some function on underlying expression if Info or else on expression *)
val app_under_info: (exp -> unit) -> exp -> unit

val typeOf: exp -> typ
(** Compute the type of an expression. *)

val typeOf_pointed : typ -> typ
(** Returns the type pointed by the given type. Asserts it is a pointer type. *)

val typeOf_array_elem : typ -> typ
(** Returns the type of the array elements of the given type.
    Asserts it is an array type. *)

val is_fully_arithmetic: typ -> bool
  (** Returns [true] whenever the type contains only arithmetic types *)

(** Convert a string representing a C integer literal to an expression.
    Handles the prefixes 0x and 0 and the suffixes L, U, UL, LL, ULL.
*)
val parseInt: string -> Integer.t
val parseIntExp: loc:location -> string -> exp
val parseIntLogic: loc:location -> string -> term

(** Convert a string representing a C integer literal to an expression.
    Handles the prefixes 0x and 0 and the suffixes L, U, UL, LL, ULL *)

val appears_in_expr: varinfo -> exp -> bool
(** @return true if the given variable appears in the expression. *)

(**********************************************)
(** {3 Values for manipulating statements} *)
(**********************************************)

(** Construct a statement, given its kind. Initialize the [sid] field to -1
    if [valid_sid] is false (the default),
    or to a valid sid if [valid_sid] is true,
    and [labels], [succs] and [preds] to the empty list *)
val mkStmt: ?ghost:bool -> ?valid_sid:bool -> stmtkind -> stmt

(* make the [new_stmtkind] changing the CFG relatively to [ref_stmt] *)
val mkStmtCfg: before:bool -> new_stmtkind:stmtkind -> ref_stmt:stmt -> stmt

(** Construct a block with no attributes, given a list of statements *)
val mkBlock: stmt list -> block

(** Construct a block with no attributes, given a list of statements and
    wrap it into the Cfg. *)
val mkStmtCfgBlock: stmt list -> stmt

(** Construct a statement consisting of just one instruction
    See {!Cil.mkStmt} for the signification of the optional args.
 *)
val mkStmtOneInstr: ?ghost:bool -> ?valid_sid:bool -> instr -> stmt

(** Try to compress statements so as to get maximal basic blocks.
 * use this instead of List.@ because you get fewer basic blocks *)
(*val compactStmts: stmt list -> stmt list*)

(** Returns an empty statement (of kind [Instr]). See [mkStmt] for [ghost] and
    [valid_sid] arguments.
    @modify Neon-20130301 adds the [valid_sid] optional argument. *)
val mkEmptyStmt: ?ghost:bool -> ?valid_sid:bool -> ?loc:location -> unit -> stmt

(** A instr to serve as a placeholder *)
val dummyInstr: instr

(** A statement consisting of just [dummyInstr].
    @plugin development guide *)
val dummyStmt: stmt

(** Make a while loop. Can contain Break or Continue *)
val mkWhile: guard:exp -> body:stmt list -> stmt list

(** Make a for loop for(i=start; i<past; i += incr) \{ ... \}. The body
    can contain Break but not Continue. Can be used with i a pointer
    or an integer. Start and done must have the same type but incr
    must be an integer *)
val mkForIncr:  iter:varinfo -> first:exp -> stopat:exp -> incr:exp
                 -> body:stmt list -> stmt list

(** Make a for loop for(start; guard; next) \{ ... \}. The body can
    contain Break but not Continue !!! *)
val mkFor: start:stmt list -> guard:exp -> next: stmt list ->
                                       body: stmt list -> stmt list

(** creates a block with empty attributes from an unspecified sequence. *)
val block_from_unspecified_sequence:
  (stmt * lval list * lval list * lval list * stmt ref list) list -> block

(* ************************************************************************* *)
(** {2 Values for manipulating attributes} *)
(* ************************************************************************* *)

(** Various classes of attributes *)
type attributeClass =
    AttrName of bool
        (** Attribute of a name. If argument is true and we are on MSVC then
            the attribute is printed using __declspec as part of the storage
            specifier  *)
  | AttrFunType of bool
        (** Attribute of a function type. If argument is true and we are on
            MSVC then the attribute is printed just before the function name *)
  | AttrType  (** Attribute of a type *)

val registerAttribute: string -> attributeClass -> unit
  (** Add a new attribute with a specified class *)

val removeAttribute: string -> unit
  (** Remove an attribute previously registered. *)

val attributeClass: string -> attributeClass
  (** Return the class of an attributes. *)

(** Partition the attributes into classes:name attributes, function type,
    and type attributes *)
val partitionAttributes:  default:attributeClass ->
                         attributes -> attribute list * (* AttrName *)
                                       attribute list * (* AttrFunType *)
                                           attribute list   (* AttrType *)

(** Add an attribute. Maintains the attributes in sorted order of the second
    argument *)
val addAttribute: attribute -> attributes -> attributes

(** Add a list of attributes. Maintains the attributes in sorted order. The
    second argument must be sorted, but not necessarily the first *)
val addAttributes: attribute list -> attributes -> attributes

(** Remove all attributes with the given name. Maintains the attributes in
    sorted order.  *)
val dropAttribute: string -> attributes -> attributes

(** Remove all attributes with names appearing in the string list.
 *  Maintains the attributes in sorted order *)
val dropAttributes: string list -> attributes -> attributes

(** Remove attributes whose name appears in the first argument that are
    present anywhere in the fully expanded version of the type.
    @since Oxygen-20120901
*)
val typeDeepDropAttributes: string list -> typ -> typ

(** Remove any attribute appearing somewhere in the fully expanded 
    version of the type.
    @since Oxygen-20120901 
*)
val typeDeepDropAllAttributes: typ -> typ

(** Retains attributes with the given name *)
val filterAttributes: string -> attributes -> attributes

(** True if the named attribute appears in the attribute list. The list of
    attributes must be sorted.  *)
val hasAttribute: string -> attributes -> bool

(** returns the complete name for an attribute annotation. *)
val mkAttrAnnot: string -> string

(** Returns the name of an attribute. *)
val attributeName: attribute -> string

(** Returns the list of parameters associated to an attribute. The list is empty if there
    is no such attribute or it has no parameters at all. *)
val findAttribute: string -> attribute list -> attrparam list

(** Returns all the attributes contained in a type. This requires a traversal
    of the type structure, in case of composite, enumeration and named types *)
val typeAttrs: typ -> attribute list

(** Returns the attributes of a type. *)
val typeAttr: typ -> attribute list

(** Sets the attributes of the type to the given list. Previous attributes
    are discarded. *)
val setTypeAttrs: typ -> attributes -> typ

(** Add some attributes to a type *)
val typeAddAttributes: attribute list -> typ -> typ

(** Remove all attributes with the given names from a type. Note that this
    does not remove attributes from typedef and tag definitions, just from
    their uses *)
val typeRemoveAttributes: string list -> typ -> typ

val typeHasAttributeDeep: string -> typ -> bool
(** Does the type or one of its subtypes have the given attribute. Does
    not recurse through pointer types, nor inside function prototypes.
    @since Oxygen-20120901 *)

(** Remove all attributes relative to const, volatile and restrict attributes
    @since Nitrogen-20111001
 *)
val type_remove_qualifier_attributes: typ -> typ

(** Remove all attributes relative to const, volatile and restrict attributes
    when building a C cast
    @since Oxygen-20120901
 *)
val type_remove_attributes_for_c_cast: typ -> typ

(** Remove all attributes relative to const, volatile and restrict attributes
    when building a logic cast
    @since Oxygen-20120901
 *)
val type_remove_attributes_for_logic_type: typ -> typ

(** retains attributes corresponding to type qualifiers (6.7.3) *)
val filter_qualifier_attributes: attributes -> attributes

(** given some attributes on an array type, split them into those that belong
    to the type of the elements of the array (currently, qualifiers such as
    const and volatile), and those that must remain on the array, in that
    order
    @since Oxygen-20120901 *)
val splitArrayAttributes: attributes -> attributes * attributes

val bitfield_attribute_name: string
(** Name of the attribute that is automatically inserted (with an [AINT size]
    argument when querying the type of a field that is a bitfield *)

(** Convert an expression into an attrparam, if possible. Otherwise raise
    NotAnAttrParam with the offending subexpression *)
val expToAttrParam: exp -> attrparam

exception NotAnAttrParam of exp

(* ************************************************************************* *)
(** {2 The visitor} *)
(* ************************************************************************* *)

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc.
    @plugin development guide *)
type 'a visitAction =
  | SkipChildren (** Do not visit the children. Return the node as it is. 
		     @plugin development guide *)
  | DoChildren (** Continue with the children of this node. Rebuild the node on
		   return if any of the children changes (use == test).
		   @plugin development guide *)
  | DoChildrenPost of ('a -> 'a)
  (** visit the children, and apply the given function to the result.
      @plugin development guide *)
  | JustCopy (** visit the children, but only to make the necessary copies
                 (only useful for copy visitor).
		 @plugin development guide *)
  | JustCopyPost of ('a -> 'a) 
  (** same as JustCopy + applies the given function to the result. 
      @plugin development guide*)
  | ChangeTo of 'a  (** Replace the expression with the given one.
			@plugin development guide *)
  | ChangeToPost of 'a * ('a -> 'a)
  (** applies the expression to the function and gives back the result.
      Useful to insert some actions in an inheritance chain.
      @plugin development guide *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a)
(** First consider that the entire exp is replaced by the first parameter. Then
    continue with the children. On return rebuild the node if any of the
    children has changed and then apply the function on the node. 
    @plugin development guide *)

val mk_behavior :
  ?name:string ->
  ?assumes:('a list) ->
  ?requires:('a list) ->
  ?post_cond:((termination_kind * 'a) list) ->
  ?assigns:('b Cil_types.assigns ) ->
  ?allocation:('b  Cil_types.allocation option) ->
  ?extended:((string * int * 'a list) list) ->
  unit ->
  ('a, 'b) Cil_types.behavior
(** @since Carbon-20101201
    returns a dummy behavior with the default name [Cil.default_behavior_name].
    invariant: [b_assumes] must always be
    empty for behavior named [Cil.default_behavior_name] *)

val default_behavior_name: string
  (** @since Carbon-20101201  *)

val is_default_behavior: ('a,'b) behavior -> bool
val find_default_behavior: funspec -> funbehavior option
  (** @since Carbon-20101201  *)

val find_default_requires: ('a, 'b) behavior list -> 'a list
  (** @since Carbon-20101201  *)

(* ************************************************************************* *)
(** {2 Visitor mechanism} *)
(* ************************************************************************* *)

(** {3 Visitor behavior} *)
type visitor_behavior
  (** How the visitor should behave in front of mutable fields: in
      place modification or copy of the structure. This type is abstract.
      Use one of the two values below in your classes.
      @plugin development guide *)

val inplace_visit: unit -> visitor_behavior
  (** In-place modification. Behavior of the original cil visitor.
      @plugin development guide *)

val copy_visit: Project.t -> visitor_behavior
  (** Makes fresh copies of the mutable structures.
      - preserves sharing for varinfo.
      - makes fresh copy of varinfo only for declarations. Variables that are
      only used in the visited AST are thus still shared with the original
      AST. This allows for instance to copy a function with its
      formals and local variables, and to keep the references to other
      globals in the function's body.
      @plugin development guide *)

(** true iff the behavior is a copy behavior. *)
val is_copy_behavior: visitor_behavior -> bool

val reset_behavior_varinfo: visitor_behavior -> unit
(** resets the internal tables used by the given visitor_behavior.  If you use
    fresh instances of visitor for each round of transformation, this should
    not be needed. In place modifications do not need that at all. *)

val reset_behavior_compinfo: visitor_behavior -> unit
val reset_behavior_enuminfo: visitor_behavior -> unit
val reset_behavior_enumitem: visitor_behavior -> unit
val reset_behavior_typeinfo: visitor_behavior -> unit
val reset_behavior_stmt: visitor_behavior -> unit
val reset_behavior_logic_info: visitor_behavior -> unit
val reset_behavior_logic_type_info: visitor_behavior -> unit
val reset_behavior_fieldinfo: visitor_behavior -> unit
val reset_behavior_model_info: visitor_behavior -> unit
val reset_logic_var: visitor_behavior -> unit
val reset_behavior_kernel_function: visitor_behavior -> unit
val reset_behavior_fundec: visitor_behavior -> unit

val get_varinfo: visitor_behavior -> varinfo -> varinfo
(** retrieve the representative of a given varinfo in the current
    state of the visitor *)

val get_compinfo: visitor_behavior -> compinfo -> compinfo
val get_enuminfo: visitor_behavior -> enuminfo -> enuminfo
val get_enumitem: visitor_behavior -> enumitem -> enumitem
val get_typeinfo: visitor_behavior -> typeinfo -> typeinfo
val get_stmt: visitor_behavior -> stmt -> stmt
(** @plugin development guide *)

val get_logic_info: visitor_behavior -> logic_info -> logic_info
val get_logic_type_info: visitor_behavior -> logic_type_info -> logic_type_info
val get_fieldinfo: visitor_behavior -> fieldinfo -> fieldinfo
val get_model_info: visitor_behavior -> model_info -> model_info
val get_logic_var: visitor_behavior -> logic_var -> logic_var
val get_kernel_function: visitor_behavior -> kernel_function -> kernel_function
(** @plugin development guide *)
  
val get_fundec: visitor_behavior -> fundec -> fundec

val get_original_varinfo: visitor_behavior -> varinfo -> varinfo
  (** retrieve the original representative of a given copy of a varinfo
      in the current state of the visitor. *)

val get_original_compinfo: visitor_behavior -> compinfo -> compinfo
val get_original_enuminfo: visitor_behavior -> enuminfo -> enuminfo
val get_original_enumitem: visitor_behavior -> enumitem -> enumitem
val get_original_typeinfo: visitor_behavior -> typeinfo -> typeinfo
val get_original_stmt: visitor_behavior -> stmt -> stmt
val get_original_logic_info: visitor_behavior -> logic_info -> logic_info
val get_original_logic_type_info:
  visitor_behavior -> logic_type_info -> logic_type_info
val get_original_fieldinfo: visitor_behavior -> fieldinfo -> fieldinfo
val get_original_model_info: visitor_behavior -> model_info -> model_info
val get_original_logic_var: visitor_behavior -> logic_var -> logic_var
val get_original_kernel_function:
  visitor_behavior -> kernel_function -> kernel_function
val get_original_fundec: visitor_behavior -> fundec -> fundec

val set_varinfo: visitor_behavior -> varinfo -> varinfo -> unit
  (** change the representative of a given varinfo in the current
      state of the visitor. Use with care (i.e. makes sure that the old one
      is not referenced anywhere in the AST, or sharing will be lost.
  *)
val set_compinfo: visitor_behavior -> compinfo -> compinfo -> unit
val set_enuminfo: visitor_behavior -> enuminfo -> enuminfo -> unit
val set_enumitem: visitor_behavior -> enumitem -> enumitem -> unit
val set_typeinfo: visitor_behavior -> typeinfo -> typeinfo -> unit
val set_stmt: visitor_behavior -> stmt -> stmt -> unit
val set_logic_info: visitor_behavior -> logic_info -> logic_info -> unit
val set_logic_type_info:
  visitor_behavior -> logic_type_info -> logic_type_info -> unit
val set_fieldinfo: visitor_behavior -> fieldinfo -> fieldinfo -> unit
val set_model_info: visitor_behavior -> model_info -> model_info -> unit
val set_logic_var: visitor_behavior -> logic_var -> logic_var -> unit
val set_kernel_function:
  visitor_behavior -> kernel_function -> kernel_function -> unit
val set_fundec: visitor_behavior -> fundec -> fundec -> unit

val set_orig_varinfo: visitor_behavior -> varinfo -> varinfo -> unit
  (** change the reference of a given new varinfo in the current
      state of the visitor. Use with care
  *)
val set_orig_compinfo: visitor_behavior -> compinfo -> compinfo -> unit
val set_orig_enuminfo: visitor_behavior -> enuminfo -> enuminfo -> unit
val set_orig_enumitem: visitor_behavior -> enumitem -> enumitem -> unit
val set_orig_typeinfo: visitor_behavior -> typeinfo -> typeinfo -> unit
val set_orig_stmt: visitor_behavior -> stmt -> stmt -> unit
val set_orig_logic_info: visitor_behavior -> logic_info -> logic_info -> unit
val set_orig_logic_type_info:
  visitor_behavior -> logic_type_info -> logic_type_info -> unit
val set_orig_fieldinfo: visitor_behavior -> fieldinfo -> fieldinfo -> unit
val set_orig_model_info: visitor_behavior -> model_info -> model_info -> unit
val set_orig_logic_var: visitor_behavior -> logic_var -> logic_var -> unit
val set_orig_kernel_function: 
  visitor_behavior -> kernel_function -> kernel_function -> unit
val set_orig_fundec: visitor_behavior -> fundec -> fundec -> unit

val memo_varinfo: visitor_behavior -> varinfo -> varinfo
  (** finds a binding in new project for the given varinfo, creating one
      if it does not already exists. *)
val memo_compinfo: visitor_behavior -> compinfo -> compinfo
val memo_enuminfo: visitor_behavior -> enuminfo -> enuminfo
val memo_enumitem: visitor_behavior -> enumitem -> enumitem
val memo_typeinfo: visitor_behavior -> typeinfo -> typeinfo
val memo_stmt: visitor_behavior -> stmt -> stmt
val memo_logic_info: visitor_behavior -> logic_info -> logic_info
val memo_logic_type_info: visitor_behavior -> logic_type_info -> logic_type_info
val memo_fieldinfo: visitor_behavior -> fieldinfo -> fieldinfo
val memo_model_info: visitor_behavior -> model_info -> model_info
val memo_logic_var: visitor_behavior -> logic_var -> logic_var
val memo_kernel_function:
  visitor_behavior -> kernel_function -> kernel_function
val memo_fundec: visitor_behavior -> fundec -> fundec

(** [iter_visitor_varinfo vis f] iterates [f] over each pair of 
    varinfo registered in [vis]. Varinfo for the old AST is presented 
    to [f] first.
    @since Oxygen-20120901 
*)
val iter_visitor_varinfo:
  visitor_behavior -> (varinfo -> varinfo -> unit) -> unit
val iter_visitor_compinfo:
  visitor_behavior -> (compinfo -> compinfo -> unit) -> unit
val iter_visitor_enuminfo: 
  visitor_behavior -> (enuminfo -> enuminfo -> unit) -> unit
val iter_visitor_enumitem:
  visitor_behavior -> (enumitem -> enumitem -> unit) -> unit
val iter_visitor_typeinfo:
  visitor_behavior -> (typeinfo -> typeinfo -> unit) -> unit
val iter_visitor_stmt:
  visitor_behavior -> (stmt -> stmt -> unit) -> unit
val iter_visitor_logic_info:
  visitor_behavior -> (logic_info -> logic_info -> unit) -> unit
val iter_visitor_logic_type_info:
  visitor_behavior -> (logic_type_info -> logic_type_info -> unit) -> unit
val iter_visitor_fieldinfo: 
  visitor_behavior -> (fieldinfo -> fieldinfo -> unit) -> unit
val iter_visitor_model_info: 
  visitor_behavior -> (model_info -> model_info -> unit) -> unit
val iter_visitor_logic_var: 
  visitor_behavior -> (logic_var -> logic_var -> unit) -> unit
val iter_visitor_kernel_function:
  visitor_behavior -> (kernel_function -> kernel_function -> unit) -> unit
val iter_visitor_fundec: 
  visitor_behavior -> (fundec -> fundec -> unit) -> unit

(** [fold_visitor_varinfo vis f] folds [f] over each pair of varinfo registered
    in [vis]. Varinfo for the old AST is presented to [f] first.
    @since Oxygen-20120901 
*)
val fold_visitor_varinfo:
  visitor_behavior -> (varinfo -> varinfo -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_compinfo:
  visitor_behavior -> (compinfo -> compinfo -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_enuminfo: 
  visitor_behavior -> (enuminfo -> enuminfo -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_enumitem:
  visitor_behavior -> (enumitem -> enumitem -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_typeinfo:
  visitor_behavior -> (typeinfo -> typeinfo -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_stmt:
  visitor_behavior -> (stmt -> stmt -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_logic_info:
  visitor_behavior -> (logic_info -> logic_info -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_logic_type_info:
  visitor_behavior -> 
  (logic_type_info -> logic_type_info -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_fieldinfo: 
  visitor_behavior -> (fieldinfo -> fieldinfo -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_model_info: 
  visitor_behavior -> (model_info -> model_info -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_logic_var: 
  visitor_behavior -> (logic_var -> logic_var -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_kernel_function:
  visitor_behavior -> 
  (kernel_function -> kernel_function -> 'a -> 'a) -> 'a -> 'a
val fold_visitor_fundec: 
  visitor_behavior -> (fundec -> fundec -> 'a -> 'a) -> 'a -> 'a

(** {3 Visitor class} *)

(** A visitor interface for traversing CIL trees. Create instantiations of
  this type by specializing the class {!nopCilVisitor}. Each of the
  specialized visiting functions can also call the [queueInstr] to specify
  that some instructions should be inserted before the current instruction
  or statement. Use syntax like [self#queueInstr] to call a method
  associated with the current object.
 
  {b Important Note for Frama-C Users:} Unless you really know what you are
  doing, you should probably inherit from the
  {!Visitor.generic_frama_c_visitor} instead of {!genericCilVisitor} or
    {!nopCilVisitor}
    
    @plugin development guide *)
class type cilVisitor = object
  method behavior: visitor_behavior
  (** the kind of behavior expected for the behavior.
      @plugin development guide *)

  method project: Project.t option
    (** Project the visitor operates on. Non-nil for copy visitor.
        @since Oxygen-20120901 *)

  method plain_copy_visitor: cilVisitor
    (** a visitor who only does copies of the nodes according to [behavior] *)

  method vfile: file -> file visitAction
  (** visit a whole file. *)

  method vvdec: varinfo -> varinfo visitAction
    (** Invoked for each variable declaration. The subtrees to be traversed
	are those corresponding to the type and attributes of the variable.
	Note that variable declarations are all the [GVar], [GVarDecl], [GFun],
	all the [varinfo] in formals of function types, and the formals and
	locals for function definitions. This means that the list of formals
	in a function definition will be traversed twice, once as part of the
	function type and second as part of the formals in a function
	definition.
	@plugin development guide *)

  method vvrbl: varinfo -> varinfo visitAction
  (** Invoked on each variable use. Here only the [SkipChildren] and
      [ChangeTo] actions make sense since there are no subtrees. Note that
      the type and attributes of the variable are not traversed for a
      variable use.
      @plugin development guide *)

  method vexpr: exp -> exp visitAction
  (** Invoked on each expression occurrence. The subtrees are the
      subexpressions, the types (for a [Cast] or [SizeOf] expression) or the
      variable use.
      @plugin development guide *)

  method vlval: lval -> lval visitAction
  (** Invoked on each lvalue occurrence *)

  method voffs: offset -> offset visitAction
  (** Invoked on each offset occurrence that is *not* as part of an
      initializer list specification, i.e. in an lval or recursively inside an
      offset.  
      @plugin development guide *)

  method vinitoffs: offset -> offset visitAction
  (** Invoked on each offset appearing in the list of a
      CompoundInit initializer.  *)

  method vinst: instr -> instr list visitAction
  (** Invoked on each instruction occurrence. The [ChangeTo] action can
      replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction
  (** Control-flow statement. The default [DoChildren] action does not create a
      new statement when the components change. Instead it updates the contents
      of the original statement. This is done to preserve the sharing with
      [Goto] and [Case] statements that point to the original statement. If you
      use the [ChangeTo] action then you should take care of preserving that
      sharing yourself.  
      @plugin development guide *)

  method vblock: block -> block visitAction
  (** Block. *)
  
  method vfunc: fundec -> fundec visitAction    
  (** Function definition. Replaced in place. *)

  method vglob: global -> global list visitAction
  (** Global (vars, types, etc.)
      @plugin development guide *)

  method vinit: varinfo -> offset -> init -> init visitAction
  (** Initializers for globals, pass the global where this occurs, and the
      offset *)

  method vtype: typ -> typ visitAction
  (** Use of some type. For typedef, struct, union and enum, the visit is
      done once at the global defining the type. Thus, children of
      [TComp], [TEnum] and [TNamed] are not visited again. *)

  method vcompinfo: compinfo -> compinfo visitAction
    (** declaration of a struct/union *)

  method venuminfo: enuminfo -> enuminfo visitAction
    (** declaration of an enumeration *)

  method vfieldinfo: fieldinfo -> fieldinfo visitAction
    (** visit the declaration of a field of a structure or union *)

  method venumitem: enumitem -> enumitem visitAction
    (** visit the declaration of an enumeration item *)

  method vattr: attribute -> attribute list visitAction
  (** Attribute. Each attribute can be replaced by a list *)

  method vattrparam: attrparam -> attrparam visitAction
  (** Attribute parameters. *)

  method queueInstr: instr list -> unit
  (** Add here instructions while visiting to queue them to preceede the
      current statement or instruction being processed. Use this method only
      when you are visiting an expression that is inside a function body, or a
      statement, because otherwise there will no place for the visitor to place
      your instructions. *)

  (** Gets the queue of instructions and resets the queue. This is done
      automatically for you when you visit statments. *)
  method unqueueInstr: unit -> instr list

  method current_stmt: stmt option
  (** link to the current statement being visited.

      {b NB:} for copy visitor, the stmt is the original one (use
      [get_stmt] to obtain the corresponding copy) *)

  method current_kinstr: kinstr
    (** [Kstmt stmt] when visiting statement stmt, [Kglobal] when called outside
        of a statement.
        @since Carbon-20101201 
	@plugin development guide *)

  method push_stmt : stmt -> unit
  method pop_stmt : stmt -> unit

  method current_func: fundec option
    (** link to the current function being visited.

        {b NB:} for copy visitors, the fundec is the original one. *)
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
  (** @plugin development guide *)

  method vlogic_info_use: logic_info -> logic_info visitAction
  (** @plugin development guide *)

  method vlogic_type_info_decl: logic_type_info -> logic_type_info visitAction
  (** @plugin development guide *)

  method vlogic_type_info_use: logic_type_info -> logic_type_info visitAction
  (** @plugin development guide *)

  method vlogic_type_def: logic_type_def -> logic_type_def visitAction
  method vlogic_ctor_info_decl: logic_ctor_info -> logic_ctor_info visitAction
  (** @plugin development guide *)

  method vlogic_ctor_info_use: logic_ctor_info -> logic_ctor_info visitAction
  (** @plugin development guide *)

  method vlogic_var_decl: logic_var -> logic_var visitAction
  (** @plugin development guide *)

  method vlogic_var_use: logic_var -> logic_var visitAction
  (** @plugin development guide *)

  method vquantifiers: quantifiers -> quantifiers visitAction

  method videntified_predicate:
    identified_predicate -> identified_predicate visitAction
  (**
     @since Fluorine-20130401 
     the child of an identified predicate is treated as a predicate named:
     if you wish to modify names, you only have to override vpredicate_named,
     not both videntified_predicate and vpredicate_named.
  *)

  method vpredicate: predicate -> predicate visitAction
  method vpredicate_named: predicate named -> predicate named visitAction
  method vbehavior: funbehavior -> funbehavior visitAction
  method vspec: funspec -> funspec visitAction
  method vassigns:
    identified_term assigns -> identified_term assigns visitAction

  method vfrees:
    identified_term list -> identified_term list visitAction
  (**	@since Oxygen-20120901 *)

  method vallocates:
    identified_term list -> identified_term list visitAction
  (**	@since Oxygen-20120901 *)

  method vallocation:
    identified_term allocation -> identified_term allocation visitAction
  (**	@since Oxygen-20120901 *)

  method vloop_pragma: term loop_pragma -> term loop_pragma visitAction
  method vslice_pragma: term slice_pragma -> term slice_pragma visitAction
  method vimpact_pragma: term impact_pragma -> term impact_pragma visitAction

  method vdeps: identified_term deps -> identified_term deps visitAction
  method vfrom: identified_term from -> identified_term from visitAction
  method vcode_annot: code_annotation -> code_annotation visitAction
  method vannotation: global_annotation -> global_annotation visitAction

  method fill_global_tables: unit
  (** fill the global environment tables at the end of a full copy in a
      new project.
      @plugin development guide *)

  method get_filling_actions: (unit -> unit) Queue.t
  (** get the queue of actions to be performed at the end of a full copy.
      @plugin development guide *)

end

(**/**)
class internal_genericCilVisitor:
  fundec option ref -> visitor_behavior -> (unit->unit) Queue.t -> cilVisitor
(**/**)

(** generic visitor, parameterized by its copying behavior.
    Traverses the CIL tree without modifying anything *)
class genericCilVisitor: visitor_behavior -> cilVisitor

(** Default in place visitor doing nothing and operating on current project. *)
class nopCilVisitor: cilVisitor

(** {3 Generic visit functions} *)
(** [doVisit vis deepCopyVisitor copy action children node]
    visits a [node]
    (or its copy according to the result of [copy]) and if needed
    its [children]. {b Do not use it if you don't understand Cil visitor
    mechanism}
    @param vis the visitor performing the needed transformations. The open
    type allows for extensions to Cil to be visited by the same mechanisms.
    @param deepCopyVisitor a generator for a visitor of the same type
    of the current one that performs a deep copy of the AST.
    Needed when the visitAction is [SkipChildren] or [ChangeTo] and [vis]
    is a copy visitor (we need to finish the copy anyway)
    @param copy function that may return a copy of the actual node.
    @param action the visiting function for the current node
    @param children what to do on the children of the current node
    @param node the current node
*)
val doVisit:
  'visitor -> 'visitor ->
  ('a -> 'a) ->
  ('a -> 'a visitAction) ->
  ('visitor -> 'a -> 'a) -> 'a -> 'a

(** same as above, but can return a list of nodes *)
val doVisitList:
  'visitor -> 'visitor ->
  ('a -> 'a) ->
  ('a -> 'a list visitAction) ->
  ('visitor -> 'a -> 'a) -> 'a -> 'a list

(* other cil constructs *)

(** {3 Visitor's entry points} *)

(** Visit a file. This will will re-cons all globals TWICE (so that it is
 * tail-recursive). Use {!Cil.visitCilFileSameGlobals} if your visitor will
 * not change the list of globals.
    @plugin development guide *)
val visitCilFileCopy: cilVisitor -> file -> file

(** Same thing, but the result is ignored. The given visitor must thus be
    an inplace visitor. Nothing is done if the visitor is a copy visitor.
    @plugin development guide *)
val visitCilFile: cilVisitor -> file -> unit

(** A visitor for the whole file that does not change the globals (but maybe
 * changes things inside the globals). Use this function instead of
 * {!Cil.visitCilFile} whenever appropriate because it is more efficient for
 * long files.
    @plugin development guide *)
val visitCilFileSameGlobals: cilVisitor -> file -> unit

(** Visit a global *)
val visitCilGlobal: cilVisitor -> global -> global list

(** Visit a function definition *)
val visitCilFunction: cilVisitor -> fundec -> fundec

(* Visit an expression *)
val visitCilExpr: cilVisitor -> exp -> exp

val visitCilEnumInfo: cilVisitor -> enuminfo -> enuminfo

(** Visit an lvalue *)
val visitCilLval: cilVisitor -> lval -> lval

(** Visit an lvalue or recursive offset *)
val visitCilOffset: cilVisitor -> offset -> offset

(** Visit an initializer offset *)
val visitCilInitOffset: cilVisitor -> offset -> offset

(** Visit an instruction *)
val visitCilInstr: cilVisitor -> instr -> instr list

(** Visit a statement *)
val visitCilStmt: cilVisitor -> stmt -> stmt

(** Visit a block *)
val visitCilBlock: cilVisitor -> block -> block

(** Visit a type *)
val visitCilType: cilVisitor -> typ -> typ

(** Visit a variable declaration *)
val visitCilVarDecl: cilVisitor -> varinfo -> varinfo

(** Visit an initializer, pass also the global to which this belongs and the
 * offset. *)
val visitCilInit: cilVisitor -> varinfo -> offset -> init -> init

(** Visit a list of attributes *)
val visitCilAttributes: cilVisitor -> attribute list -> attribute list

val visitCilAnnotation: cilVisitor -> global_annotation -> global_annotation

val visitCilCodeAnnotation: cilVisitor -> code_annotation -> code_annotation

val visitCilDeps:
  cilVisitor -> identified_term deps -> identified_term deps

val visitCilFrom:
  cilVisitor -> identified_term from -> identified_term from

val visitCilAssigns:
  cilVisitor -> identified_term assigns -> identified_term assigns

(** @since Oxygen-20120901
 *)
val visitCilFrees:
  cilVisitor -> identified_term list -> identified_term list

(** @since Oxygen-20120901
 *)
val visitCilAllocates:
  cilVisitor -> identified_term list -> identified_term list

(** @since Oxygen-20120901
 *)
val visitCilAllocation:
  cilVisitor -> identified_term allocation -> identified_term allocation

val visitCilFunspec: cilVisitor -> funspec -> funspec

val visitCilBehavior: cilVisitor -> funbehavior -> funbehavior
val visitCilBehaviors: cilVisitor -> funbehavior list -> funbehavior list

(** visit an extended clause of a behavior.
    @since Nitrogen-20111001
 *)
val visitCilExtended: 
  cilVisitor -> (string * int * identified_predicate list) 
  -> (string * int * identified_predicate list)

val visitCilModelInfo: cilVisitor -> model_info -> model_info

val visitCilLogicType: cilVisitor -> logic_type -> logic_type

val visitCilIdPredicate:
  cilVisitor -> identified_predicate -> identified_predicate

val visitCilPredicate: cilVisitor -> predicate -> predicate

val visitCilPredicateNamed: cilVisitor -> predicate named -> predicate named

val visitCilPredicates:
  cilVisitor -> identified_predicate list -> identified_predicate list

val visitCilTerm: cilVisitor -> term -> term

(** visit identified_term.
    @since Oxygen-20120901
 *)
val visitCilIdTerm: cilVisitor -> identified_term -> identified_term

(** visit term_lval.
    @since Nitrogen-20111001
 *)
val visitCilTermLval: cilVisitor -> term_lval -> term_lval

val visitCilTermLhost: cilVisitor -> term_lhost -> term_lhost

val visitCilTermOffset: cilVisitor -> term_offset -> term_offset

val visitCilLogicInfo: cilVisitor -> logic_info -> logic_info

val visitCilLogicVarUse: cilVisitor -> logic_var -> logic_var

val visitCilLogicVarDecl: cilVisitor -> logic_var -> logic_var

(** {3 Visiting children of a node} *)

val childrenBehavior: cilVisitor -> funbehavior -> funbehavior

(* And some generic visitors. The above are built with these *)

(* ************************************************************************* *)
(** {2 Utility functions} *)
(* ************************************************************************* *)

val is_skip: stmtkind -> bool

(** A visitor that does constant folding. Pass as argument whether you want
 * machine specific simplifications to be done, or not. *)
val constFoldVisitor: bool -> cilVisitor

(** Return the string 's' if we're printing output for gcc, suppres
 *  it if we're printing for CIL to parse back in.  the purpose is to
 *  hide things from gcc that it complains about, but still be able
 *  to do lossless transformations when CIL is the consumer *)
val forgcc: string -> string

(* ************************************************************************* *)
(** {2 Debugging support} *)
(* ************************************************************************* *)

(** A reference to the current location. If you are careful to set this to
 * the current location then you can use some built-in logging functions that
 * will print the location. *)
module CurrentLoc: State_builder.Ref with type data = location

(** Pretty-print [(Cil.CurrentLoc.get ())] *)
val pp_thisloc: Format.formatter -> unit

(** A reference to the current global being visited *)
val currentGlobal: global ref

(** @return a dummy specification *)
val empty_funspec : unit -> funspec

(** @return true if the given spec is empty. *)
val is_empty_funspec: funspec -> bool

(** @return true if the given behavior is empty. *)
val is_empty_behavior: funbehavior -> bool

(* ************************************************************************* *)
(** {2 ALPHA conversion} has been moved to the Alpha module. *)
(* ************************************************************************* *)

(** Assign unique names to local variables. This might be necessary after you
    transformed the code and added or renamed some new variables. Names are not
    used by CIL internally, but once you print the file out the compiler
    downstream might be confused. You might have added a new global that happens
    to have the same name as a local in some function. Rename the local to
    ensure that there would never be confusioin. Or, viceversa, you might have
    added a local with a name that conflicts with a global *)
val uniqueVarNames: file -> unit

(* ************************************************************************* *)
(** {2 Optimization Passes} *)
(* ************************************************************************* *)

(** A peephole optimizer that processes two adjacent statements and possibly
    replaces them both. If some replacement happens and [agressive] is true,
    then the new statements are themselves subject to optimization.  Each
    statement in the list is optimized independently. *)
val peepHole2: 
  agressive:bool -> (stmt * stmt -> stmt list option) -> stmt list -> stmt list

(** Similar to [peepHole2] except that the optimization window consists of
    one statement, not two *)
val peepHole1: (instr -> instr list option) -> stmt list -> unit

(* ************************************************************************* *)
(** {2 Machine dependency} *)
(* ************************************************************************* *)

(** Raised when one of the SizeOf/AlignOf functions cannot compute the size of a
    type. This can happen because the type contains array-length expressions
    that we don't know how to compute or because it is a type whose size is not
    defined (e.g. TFun or an undefined compinfo). The string is an explanation
    of the error *)
exception SizeOfError of string * typ

(** Create a fresh size cache with [Not_Computed] *)
val empty_size_cache : unit -> bitsSizeofTypCache

(** Give the unsigned kind corresponding to any integer kind *)
val unsignedVersionOf : ikind -> ikind

(** The signed integer kind for a given size (unsigned if second argument
 * is true). Raises Not_found if no such kind exists *)
val intKindForSize : int -> bool -> ikind

(** The float kind for a given size. Raises Not_found
 *  if no such kind exists *)
val floatKindForSize : int-> fkind

(** The size of a type, in bits. Trailing padding is added for structs and
 * arrays. Raises {!Cil.SizeOfError} when it cannot compute the size. This
 * function is architecture dependent, so you should only call this after you
 * call {!Cil.initCIL}. Remember that on GCC sizeof(void) is 1! *)
val bitsSizeOf: typ -> int

(** The size of a type, in bytes. Raises {!Cil.SizeOfError} when it cannot
    compute the size. *)
val bytesSizeOf: typ -> int

(** Returns the number of bytes (resp. bits) to represent the given integer
    kind depending on the current machdep. *)
val bytesSizeOfInt: ikind -> int
val bitsSizeOfInt: ikind -> int

(** Returns the signedness of the given integer kind depending
   on the current machdep. *)
val isSigned: ikind -> bool

(** Returns a unique number representing the integer
   conversion rank. *)
val rank: ikind -> int

(** [intTypeIncluded i1 i2] returns [true] iff the range of values
    representable in [i1] is included in the one of [i2] *)
val intTypeIncluded: ikind -> ikind -> bool

(** Returns a unique number representing the floating-point conversion rank.
    @since Oxygen-20120901 *)
val frank: fkind -> int

(** Represents an integer as for a given kind. 
 * Returns a flag saying whether the value was changed
 * during truncation (because it was too large to fit in k). *)
val truncateInteger64: ikind -> Integer.t -> Integer.t * bool

(** Returns the maximal value representable in a signed integer type of the
    given size (in bits)
 *)
val max_signed_number: int -> Integer.t

(** Returns the smallest value representable in a signed integer type of the
    given size (in bits)
 *)
val min_signed_number: int -> Integer.t

(** Returns the maximal value representable in a unsigned integer type of the
    given size (in bits)
 *)
val max_unsigned_number: int -> Integer.t

(** True if the integer fits within the kind's range *)
val fitsInInt: ikind -> Integer.t -> bool

exception Not_representable
(** raised by {!intKindForValue}. *)

(** @return the smallest kind that will hold the integer's value.
    The kind will be unsigned if the 2nd argument is true.
    @raise Not_representable if the bigint is not representable. 
    @modify Neon-20130301 may raise Not_representable. *)
val intKindForValue: Integer.t -> bool -> ikind

(** The size of a type, in bytes. Returns a constant expression or a "sizeof"
 * expression if it cannot compute the size. This function is architecture
 * dependent, so you should only call this after you call {!Cil.initCIL}.  *)
val sizeOf: loc:location -> typ -> exp

(** The minimum alignment (in bytes) for a type. This function is
 * architecture dependent, so you should only call this after you call
 * {!Cil.initCIL}. *)
val bytesAlignOf: typ -> int

(** Give a type of a base and an offset, returns the number of bits from the
 * base address and the width (also expressed in bits) for the subobject
 * denoted by the offset. Raises {!Cil.SizeOfError} when it cannot compute
 * the size. This function is architecture dependent, so you should only call
 * this after you call {!Cil.initCIL}. *)
val bitsOffset: typ -> offset -> int * int

(** Generate an {!Cil_types.exp} to be used in case of errors. *)
val dExp:string -> exp

(** Generate an {!Cil_types.instr} to be used in case of errors. *)
val dInstr: string -> location -> instr

(** Generate a {!Cil_types.global} to be used in case of errors. *)
val dGlobal: string -> location -> global

(** Like map but try not to make a copy of the list *)
val mapNoCopy: ('a -> 'a) -> 'a list -> 'a list

(** same as mapNoCopy for options*)
val optMapNoCopy: ('a -> 'a) -> 'a option -> 'a option

(** Like map but each call can return a list. Try not to make a copy of the
    list *)
val mapNoCopyList: ('a -> 'a list) -> 'a list -> 'a list

(** sm: return true if the first is a prefix of the second string *)
val startsWith: string -> string -> bool

(* ************************************************************************* *)
(** {2 An Interpreter for constructing CIL constructs} *)
(* ************************************************************************* *)

(** The type of argument for the interpreter *)
type formatArg =
    Fe of exp
  | Feo of exp option  (** For array lengths *)
  | Fu of unop
  | Fb of binop
  | Fk of ikind
  | FE of exp list (** For arguments in a function call *)
  | Ff of (string * typ * attributes) (** For a formal argument *)
  | FF of (string * typ * attributes) list (** For formal argument lists *)
  | Fva of bool (** For the ellipsis in a function type *)
  | Fv of varinfo
  | Fl of lval
  | Flo of lval option

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

val d_formatarg : Format.formatter -> formatArg -> unit

(* ************************************************************************* *)
(** {2 Misc} *)
(* ************************************************************************* *)

val stmt_of_instr_list : ?loc:location -> instr list -> stmtkind

(** Convert a C variable into the corresponding logic variable.
    The returned logic variable is unique for a given C variable. *)
val cvar_to_lvar : varinfo -> logic_var

(** Make a temporary variable to use in annotations *)
val make_temp_logic_var: logic_type -> logic_var

(** The constant logic term zero.
    @plugin development guide *)
val lzero : ?loc:location -> unit -> term

(** The constant logic term 1. *)
val lone : ?loc:location -> unit -> term

(** The constant logic term -1. *)
val lmone : ?loc:location -> unit -> term

(** The given constant logic term *)
val lconstant : ?loc:location -> Integer.t -> term

(** Bind all free variables with an universal quantifier *)
val close_predicate : predicate named -> predicate named

(** extract [varinfo] elements from an [exp] *)
val extract_varinfos_from_exp : exp -> Varinfo.Set.t

(** extract [varinfo] elements from an [lval] *)
val extract_varinfos_from_lval : lval -> Varinfo.Set.t

(** extract [logic_var] elements from a [term] *)
val extract_free_logicvars_from_term : term -> Logic_var.Set.t

(** extract [logic_var] elements from a [predicate] *)
val extract_free_logicvars_from_predicate :
  predicate named -> Logic_var.Set.t

(** extract [logic_label] elements from a [code_annotation] *)
val extract_labels_from_annot:
  code_annotation -> Cil_datatype.Logic_label.Set.t 

(** extract [logic_label] elements from a [term] *)
val extract_labels_from_term: term -> Cil_datatype.Logic_label.Set.t 

(** extract [logic_label] elements from a [pred] *)
val extract_labels_from_pred: 
  predicate named -> Cil_datatype.Logic_label.Set.t 

(** extract [stmt] elements from [logic_label] elements *)
val extract_stmts_from_labels:
 Cil_datatype.Logic_label.Set.t -> Cil_datatype.Stmt.Set.t

(** creates a visitor that will replace in place uses of var in the first
    list by their counterpart in the second list.
    @raise Invalid_argument if the lists have different lengths. *)
val create_alpha_renaming: varinfo list -> varinfo list -> cilVisitor

(** Provided [s] is a switch, [separate_switch_succs s] returns the
    subset of [s.succs] that correspond to the Case labels of [s], and a
    "default statement" that either corresponds to the Default label, or to the
    syntactic successor of [s] if there is no default label. Note that this "default
    statement" can thus appear in the returned list. *)
val separate_switch_succs: stmt -> stmt list * stmt

(** Provided [s] is a if, [separate_if_succs s] splits the successors
    of s according to the truth value of the condition. The first
    element of the pair is the successor statement if the condition is
    true, and the second if the condition is false. *)
val separate_if_succs: stmt -> stmt * stmt

(**/**)

val register_ast_dependencies : State.t -> unit
  (** Used to postpone some dependencies on [Ast.self], which is initialized
      afterwards. *)

val pp_typ_ref: (Format.formatter -> typ -> unit) ref
val pp_global_ref: (Format.formatter -> global -> unit) ref
val pp_exp_ref: (Format.formatter -> exp -> unit) ref
val pp_lval_ref: (Format.formatter -> lval -> unit) ref
val pp_ikind_ref: (Format.formatter -> ikind -> unit) ref
val pp_attribute_ref: (Format.formatter -> attribute -> unit) ref
val pp_attributes_ref: (Format.formatter -> attribute list -> unit) ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
