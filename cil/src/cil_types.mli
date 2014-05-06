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

(** The Abstract Syntax of CIL.
    @plugin development guide *)

(**************************** WARNING ***************************************)
(* Remember to reflect any change here into the visitor and pretty-printer  *)
(* in cil.ml. In particular, if a type becomes mutable, it is necessary to  *)
(* adapt the Cil.behavior type and the copy_behavior accordingly.           *)
(* A first test to see if something has been broken by a change is to launch*)
(* ptests.byte -add-options '-files-debug "-check -copy"'                   *)
(* In addition, it is a good idea to add some invariant checks in the       *)
(* check_file class in frama-c/src/file.ml (before lauching the tests)      *)
(****************************************************************************)

(* ************************************************************************* *)
(** {2 Root of the AST} *)
(* ************************************************************************* *)

(** In Frama-C, the whole AST is accessible through {!Ast.get}. *)

(** The top-level representation of a CIL source file (and the result of the
    parsing and elaboration). Its main contents is the list of global
    declarations and definitions. You can iterate over the globals in a
    {!Cil_types.file} using the following iterators: {!Cil.mapGlobals},
    {!Cil.iterGlobals} and {!Cil.foldGlobals}. You can also use the
    {!Cil.dummyFile} when you need a {!Cil_types.file} as a placeholder. For
    each global item CIL stores the source location where it appears (using the
    type {!Cil_types.location}) 
    @plugin development guide *)
type file = { 
  mutable fileName: string;   (** The complete file name *)
  
  mutable globals: global list; 
  (** List of globals as they will appear in the printed file *)
  
  mutable globinit: fundec option;
  (** An optional global initializer function. This is a function where you
      can put stuff that must be executed before the program is
      started. This function, is conceptually at the end of the file,
      although it is not part of the globals list. Use {!Cil.getGlobInit} to
      create/get one. *)

  mutable globinitcalled: bool;
(** Whether the global initialization function is called in main. This
    should always be false if there is no global initializer. When you
    create a global initialization CIL will try to insert code in main to
    call it. *)
}

(** The main type for representing global declarations and definitions. A list
    of these form a CIL file. The order of globals in the file is generally
    important.
    @plugin development guide *)
and global =
  | GType of typeinfo * location
  (** A typedef. All uses of type names (through the [TNamed] constructor)
      must be preceeded in the file by a definition of the name. The string
      is the defined name and always not-empty. *)

  | GCompTag of compinfo * location
  (** Defines a struct/union tag with some fields. There must be one of
      these for each struct/union tag that you use (through the [TComp]
      constructor) since this is the only context in which the fields are
      printed. Consequently nested structure tag definitions must be
      broken into individual definitions with the innermost structure
      defined first. *)

  | GCompTagDecl of compinfo * location
  (** Declares a struct/union tag. Use as a forward declaration. This is
      printed without the fields.  *)

  | GEnumTag of enuminfo * location
  (** Declares an enumeration tag with some fields. There must be one of
      these for each enumeration tag that you use (through the [TEnum]
      constructor) since this is the only context in which the items are
      printed. *)

  | GEnumTagDecl of enuminfo * location
  (** Declares an enumeration tag. Use as a forward declaration. This is
      printed without the items.  *)

  | GVarDecl of funspec * varinfo * location
  (** A variable declaration (not a definition). If the variable has a
      function type then this is a prototype. There can be several
      declarations and at most one definition for a given variable. If both
      forms appear then they must share the same varinfo structure. A
      prototype shares the varinfo with the fundec of the definition. Either
      has storage Extern or there must be a definition in this file *)

  | GVar  of varinfo * initinfo * location
  (** A variable definition. Can have an initializer. The initializer is
      updateable so that you can change it without requiring to recreate the
      list of globals. There can be at most one definition for a variable in an
      entire program. Cannot have storage Extern or function type. *)

  | GFun of fundec * location
  (** A function definition. *)

  | GAsm of string * location
  (** Global asm statement. These ones can contain only a template *)
      
  | GPragma of attribute * location
  (** Pragmas at top level. Use the same syntax as attributes *)

  | GText of string
  (** Some text (printed verbatim) at top level. E.g., this way you can put
      comments in the output.  *)

  | GAnnot of global_annotation * location
(** a global annotation. Can be
    - an axiom or a lemma
    - a predicate declaration or definition
    - a global type invariant
    - a global invariant
    - a logic function declaration or definition. *)

(* ************************************************************************* *)
(** {2 Types} *)
(* ************************************************************************* *)

(** A C type is represented in CIL using the type {!Cil_types.typ}.  Among types
    we differentiate the integral types (with different kinds denoting the sign
    and precision), floating point types, enumeration types, array and pointer
    types, and function types. Every type is associated with a list of
    attributes, which are always kept in sorted order. Use {!Cil.addAttribute}
    and {!Cil.addAttributes} to construct list of attributes. If you want to
    inspect a type, you should use {!Cil.unrollType} or {!Cil.unrollTypeDeep} to
    see through the uses of named types.

    CIL is configured at build-time with the sizes and alignments of the
    underlying compiler (GCC or MSVC). CIL contains functions that can compute
    the size of a type (in bits) {!Cil.bitsSizeOf}, the alignment of a type (in
    bytes) {!Cil.alignOf_int}, and can convert an offset into a start and width
    (both in bits) using the function {!Cil.bitsOffset}. At the moment these
    functions do not take into account the [packed] attributes and pragmas. *)

and typ =
  | TVoid of attributes (** Void type. Also predefined as {!Cil.voidType} *)

  | TInt of ikind * attributes
  (** An integer type. The kind specifies the sign and width. Several useful
      variants are predefined as {!Cil.intType}, {!Cil.uintType},
      {!Cil.longType}, {!Cil.charType}. *)

  | TFloat of fkind * attributes
  (** A floating-point type. The kind specifies the precision. You can also use
      the predefined constant {!Cil.doubleType}. *)

  | TPtr of typ * attributes
  (** Pointer type. Several useful variants are predefined as
      {!Cil.charPtrType}, {!Cil.charConstPtrType} (pointer to a constant
      character), {!Cil.voidPtrType}, {!Cil.intPtrType} *)

  | TArray of typ * exp option * bitsSizeofTypCache * attributes
  (** Array type. It indicates the base type and the array length. *)

  | TFun of typ * (string * typ * attributes) list option * bool * attributes
  (** Function type. Indicates the type of the result, the name, type
      and name attributes of the formal arguments ([None] if no arguments
      were specified, as in a function whose definition or prototype we
      have not seen; [Some \[\]] means void). Use {!Cil.argsToList} to
      obtain a list of arguments. The boolean indicates if it is a
      variable-argument function. If this is the type of a varinfo for
      which we have a function declaration then the information for the
      formals must match that in the function's sformals. Use
      {!Cil.setFormals}, or {!Cil.setFunctionType}, or
      {!Cil.makeFormalVar} for this purpose. *)

  | TNamed of typeinfo * attributes
  (** The use of a named type. All uses of the same type name must share the
      typeinfo. Each such type name must be preceeded in the file by a [GType]
      global. This is printed as just the type name. The actual referred type
      is not printed here and is carried only to simplify processing. To see
      through a sequence of named type references, use {!Cil.unrollType}. The
      attributes are in addition to those given when the type name was
      defined. *)

  | TComp of compinfo * bitsSizeofTypCache * attributes
  (** A reference to a struct or a union type. All references to the
      same struct or union must share the same compinfo among them and
      with a [GCompTag] global that preceeds all uses (except maybe
      those that are pointers to the composite type). The attributes
      given are those pertaining to this use of the type and are in
      addition to the attributes that were given at the definition of
      the type and which are stored in the compinfo.  *)

  | TEnum of enuminfo * attributes
  (** A reference to an enumeration type. All such references must
      share the enuminfo among them and with a [GEnumTag] global that
      preceeds all uses. The attributes refer to this use of the
      enumeration and are in addition to the attributes of the
      enumeration itself, which are stored inside the enuminfo *)

  | TBuiltin_va_list of attributes
(** This is the same as the gcc's type with the same name *)

(** Various kinds of integers *)
and ikind =
    IBool       (** [_Bool] *)
  | IChar       (** [char] *)
  | ISChar      (** [signed char] *)
  | IUChar      (** [unsigned char] *)
  | IInt        (** [int] *)
  | IUInt       (** [unsigned int] *)
  | IShort      (** [short] *)
  | IUShort     (** [unsigned short] *)
  | ILong       (** [long] *)
  | IULong      (** [unsigned long] *)
  | ILongLong   (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft
                    Visual C) *)

(** Various kinds of floating-point numbers*)
and fkind =
    FFloat      (** [float] *)
  | FDouble     (** [double] *)
  | FLongDouble (** [long double] *)

(** This is used to cache the computation of the size of types in bits. *)
and bitsSizeofTyp =
  | Not_Computed
  | Computed of int
  | Not_Computable of (string * typ) (** Explanation of the error *)

and bitsSizeofTypCache = { mutable scache : bitsSizeofTyp}

(* ************************************************************************* *)
(** {2 Attributes} *)
(* ************************************************************************* *)

and attribute =
  | Attr of string * attrparam list
  (** An attribute has a name and some optional parameters. The name should not
      start or end with underscore. When CIL parses attribute names it will
      strip leading and ending underscores (to ensure that the multitude of GCC
      attributes such as const, __const and __const__ all mean the same
      thing.) *)

  | AttrAnnot of string

(** Attributes are lists sorted by the attribute name. Use the functions
    {!Cil.addAttribute} and {!Cil.addAttributes} to insert attributes in an
    attribute list and maintain the sortedness. *)
and attributes = attribute list

(** The type of parameters of attributes *)
and attrparam =
  | AInt of Integer.t                  (** An integer constant *)
  | AStr of string                       (** A string constant *)
  | ACons of string * attrparam list 
  (** Constructed attributes. These are printed [foo(a1,a2,...,an)]. The list
      of parameters can be empty and in that case the parentheses are not
      printed. *)
  | ASizeOf of typ                       (** A way to talk about types *)
  | ASizeOfE of attrparam
  | AAlignOf of typ
  | AAlignOfE of attrparam
  | AUnOp of unop * attrparam
  | ABinOp of binop * attrparam * attrparam
  | ADot of attrparam * string           (** a.foo **)
  | AStar of attrparam                   (** * a *)
  | AAddrOf of attrparam                 (** & a **)
  | AIndex of attrparam * attrparam      (** a1[a2] *)
  | AQuestion of attrparam * attrparam * attrparam (** a1 ? a2 : a3 **)

(* ************************************************************************* *)
(** {2 Structures} *)
(* ************************************************************************* *)

(** The {!Cil_types.compinfo} describes the definition of a structure or union
    type. Each such {!Cil_types.compinfo} must be defined at the top-level using
    the [GCompTag] constructor and must be shared by all references to this type
    (using either the [TComp] type constructor or from the definition of the
    fields.

    If all you need is to scan the definition of each composite type once, you
    can do that by scanning all top-level [GCompTag].

    Constructing a {!Cil_types.compinfo} can be tricky since it must contain
    fields that might refer to the host {!Cil_types.compinfo} and furthermore
    the type of the field might need to refer to the {!Cil_types.compinfo} for
    recursive types.  Use the {!Cil.mkCompInfo} function to create a
    {!Cil_types.compinfo}. You can easily fetch the {!Cil_types.fieldinfo} for a
    given field in a structure with {!Cil.getCompField}. *)

(** The definition of a structure or union type. Use {!Cil.mkCompInfo} to make
    one and use {!Cil.copyCompInfo} to copy one (this ensures that a new key is
    assigned and that the fields have the right pointers to parents.).  
    @plugin development guide *)
and compinfo = {
  mutable cstruct: bool;
  (** [true] if struct, [false] if union *)

  corig_name: string;
  (** Original name as found in C file. Will never be changed *)

  mutable cname: string;
  (** The name. Always non-empty. Use {!Cil.compFullName} to get the full name
      of a comp (along with the struct or union) *)

  mutable ckey: int;
  (** A unique integer. This is assigned by {!Cil.mkCompInfo} using a global
      variable in the Cil module. Thus two identical structs in two different
      files might have different keys. Use {!Cil.copyCompInfo} to copy
      structures so that a new key is assigned. *)

  mutable cfields: fieldinfo list;
  (** Information about the fields. Notice that each fieldinfo has a pointer
      back to the host compinfo. This means that you should not share
      fieldinfo's between two compinfo's *)

  mutable cattr:   attributes;
  (** The attributes that are defined at the same time as the composite
      type. These attributes can be supplemented individually at each
      reference to this [compinfo] using the [TComp] type constructor. *)

  mutable cdefined: bool;
  (** This boolean flag can be used to distinguish between structures
      that have not been defined and those that have been defined but have
      no fields (such things are allowed in gcc). *)

  mutable creferenced: bool;
(** [true] if used. Initially set to [false]. *)
}

(* ************************************************************************* *)
(** {2 Structure fields} *)
(* ************************************************************************* *)

(** The {!Cil_types.fieldinfo} structure is used to describe a structure or
    union field. Fields, just like variables, can have attributes associated
    with the field itself or associated with the type of the field (stored along
    with the type of the field). *)

(** Information about a struct/union field.
    @plugin development guide *)
and fieldinfo = {
  mutable fcomp: compinfo;
  (** The host structure that contains this field. There can be only one
      [compinfo] that contains the field. *)

  forig_name: string;
  (** original name as found in C file. *)

  mutable fname: string;
  (** The name of the field. Might be the value of {!Cil.missingFieldName} in
      which case it must be a bitfield and is not printed and it does not
      participate in initialization *)

  mutable ftype: typ;
  (** The type. If the field is a bitfield, a special attribute
      [FRAMA_C_BITFIELD_SIZE] indicating the width of the bitfield is added. *)

  mutable fbitfield: int option;
  (** If a bitfield then ftype should be an integer type and the width of the
      bitfield must be 0 or a positive integer smaller or equal to the width of
      the integer type. A field of width 0 is used in C to control the alignment
      of fields. *)

  mutable fattr: attributes;
  (** The attributes for this field (not for its type) *)

  mutable floc: location;
  (** The location where this field is defined *)

  mutable faddrof: bool;
  (** Adapted from CIL [vaddrof] field for variables. Only set for non-array
      fields. Variable whose field address is taken is not marked anymore as
      having its own address taken.  True if the address of this field is
      taken. CIL will set these flags when it parses C, but you should make
      sure to set the flag whenever your transformation create [AddrOf]
      expression. *)

  mutable fsize_in_bits: int option;
  (** (Deprecated. Use {!Cil.bitsOffset} instead.) Similar to [fbitfield] for
      all types of fields.
      @deprecated only Jessie uses this *)

  mutable foffset_in_bits: int option;
  (** Offset at which the field starts in the structure. Do not read directly,
      but use {!Cil.bitsOffset} instead. *)

  mutable fpadding_in_bits: int option;
(** (Deprecated.) Store the size of the padding that follows the field, if any.
    @deprecated only Jessie uses this *)
}

(* ************************************************************************* *)
(** {2 Enumerations} *)
(* ************************************************************************* *)

(** Information about an enumeration. This is shared by all references to an
    enumeration. Make sure you have a [GEnumTag] for each of these. *)

(** Information about an enumeration.
    @plugin development guide *)
and enuminfo = {
  eorig_name: string; (** original name as found in C file. *)

  mutable ename: string; (** The name. Always non-empty. *)

  mutable eitems: enumitem list; (** Items. The list must be non-empty *)

  mutable eattr: attributes;
  (** The attributes that are defined at the same time as the enumeration
      type. These attributes can be supplemented individually at each
      reference to this [enuminfo] using the [TEnum] type constructor. *)

  mutable ereferenced: bool; (** [true] if used. Initially set to [false]. *)
  mutable ekind: ikind (** The integer kind used to represent this enum. MSVC
                           always assumes IInt but this is not the case
                           for gcc. See ISO C 6.7.2.2 *)
}

and enumitem = { 
  eiorig_name: string; (** original name as found in C file. *)
  mutable einame: string; (** the name, always non-empty. *)
  mutable eival: exp; (** value of the item. Must be a compile-time constant *)
  mutable eihost: enuminfo; (** the host enumeration in which the item is
				declared. *) 
  eiloc: location;
}

(** Information about a defined type.
    @plugin development guide *)
and typeinfo = {
  torig_name: string; (** original name as found in C file. *)

  mutable tname: string;
  (** The name. Can be empty only in a [GType] when introducing a composite or
      enumeration tag. If empty cannot be refered to from the file *)

  mutable ttype: typ;
  (** The actual type. This includes the attributes that were present in the
      typedef *)

  mutable treferenced: bool; (** [true] if used. Initially set to [false]. *)
}

(* ************************************************************************* *)
(** {2 Variables} *)
(* ************************************************************************* *)

(** Each local or global variable is represented by a unique
    {!Cil_types.varinfo} structure. A global {!Cil_types.varinfo} can be
    introduced with the [GVarDecl] or [GVar] or [GFun] globals. A local varinfo
    can be introduced as part of a function definition {!Cil_types.fundec}.

    All references to a given global or local variable must refer to the same
    copy of the [varinfo]. Each [varinfo] has a globally unique identifier that
    can be used to index maps and hashtables (the name can also be used for this
    purpose, except for locals from different functions). This identifier is
    constructor using a global counter.

    It is very important that you construct [varinfo] structures using only one
    of the following functions:
    - {!Cil.makeGlobalVar} : to make a global variable
    - {!Cil.makeTempVar} : to make a temporary local variable whose name
    will be generated so that to avoid conflict with other locals.
    - {!Cil.makeLocalVar} : like {!Cil.makeTempVar} but you can specify the
    exact name to be used.
    - {!Cil.copyVarinfo}: make a shallow copy of a varinfo assigning a new name
    and a new unique identifier

    A [varinfo] is also used in a function type to denote the list of
    formals. *)

(** Information about a variable.
    @plugin development guide *)
and varinfo = {
  mutable vname: string;
  (** The name of the variable. Cannot be empty. It is primarily your
      responsibility to ensure the uniqueness of a variable name. For local
      variables {!Cil.makeTempVar} helps you ensure that the name is
      unique. *)

  vorig_name: string;
  (** the original name of the variable. Need not be unique. *)

  mutable vtype: typ;
  (** The declared type of the variable. *)

  mutable vattr: attributes;
  (** A list of attributes associated with the variable.*)

  mutable vstorage: storage;
  (** The storage-class *)

  mutable vglob: bool;
  (** True if this is a global variable*)

  mutable vdefined: bool;
  (** True if the variable or function is defined in the file.  Only relevant
      for functions and global variables.  Not used in particular for local
      variables and logic variables. *)

  mutable vformal: bool;
  (** True if the variable is a formal parameter of a function. *)

  mutable vinline: bool;
  (** Whether this varinfo is for an inline function. *)

  mutable vdecl: location;
  (** Location of variable declaration. *)

  mutable vid: int;
  (** A unique integer identifier. This field will be set for you if you use
      one of the {!Cil.makeFormalVar}, {!Cil.makeLocalVar},
      {!Cil.makeTempVar}, {!Cil.makeGlobalVar}, or {!Cil.copyVarinfo}. *)

  mutable vaddrof: bool;
  (** [true] if the address of this variable is taken. CIL will set these
      flags when it parses C, but you should make sure to set the flag
      whenever your transformation create [AddrOf] expression. *)

  mutable vreferenced: bool;
  (** [true] if this variable is ever referenced. This is computed by
      [removeUnusedVars]. It is safe to just initialize this to [false]. *)

  vgenerated: bool;
  (** [true] for temporary variables generated by CIL normalization. [false]
      for variables coming directly from user input.  *)

  mutable vdescr: string option;
  (** For most temporary variables, a description of what the var holds.
      (e.g. for temporaries used for function call results, this string is a
      representation of the function call.) *)

  mutable vdescrpure: bool;           
  (** Indicates whether the vdescr above is a pure expression or call.  True
      for all CIL expressions and Lvals, but false for e.g. function calls.
      Printing a non-pure vdescr more than once may yield incorrect
      results. *)

  mutable vghost: bool; 
  (** Indicates if the variable is declared in ghost code *)

  vlogic: bool;
  (** [false] iff this variable is a C variable. *)

  mutable vlogic_var_assoc: logic_var option
(** logic variable representing this variable in the logic world*)
}

(** Storage-class information *)
and storage =
    NoStorage     (** The default storage. Nothing is printed  *)
  | Static
  | Register
  | Extern

(* ************************************************************************* *)
(** {2 Expressions} *)
(* ************************************************************************* *)

(** The CIL expression language contains only the side-effect free expressions
    of C. They are represented as the type {!Cil_types.exp}. There are several
    interesting aspects of CIL expressions:

    Integer and floating point constants can carry their textual representation.
    This way the integer 15 can be printed as 0xF if that is how it occurred in
    the source.

    CIL uses arbitrary precision integers 
    to represent the integer constants and also stores the
    width of the integer type. Care must be taken to ensure that the constant is
    representable with the given width. Use the functions {!Cil.kinteger},
    {!Cil.kinteger64} and {!Cil.integer} to construct constant expressions. CIL
    predefines the constants {!Cil.zero}, {!Cil.one} and {!Cil.mone} (for -1).

    Use the functions {!Cil.isConstant} and {!Cil.isInteger} to test if an
    expression is a constant and a constant integer respectively.

    CIL keeps the type of all unary and binary expressions. You can think of
    that type qualifying the operator. Furthermore there are different operators
    for arithmetic and comparisons on arithmetic types and on pointers.

    Another unusual aspect of CIL is that the implicit conversion between an
    expression of array type and one of pointer type is made explicit, using the
    [StartOf] expression constructor (which is not printed). If you apply the
    [AddrOf]constructor to an lvalue of type [T] then you will be getting an
    expression of type [TPtr(T)].

    You can find the type of an expression with {!Cil.typeOf}.

    You can perform constant folding on expressions using the function
    {!Cil.constFold}. *)

(** Expressions (Side-effect free)*)
and exp = { 
  eid: int; (** unique identifier *)
  enode: exp_node; (** the expression itself *)
  eloc: location; (** location of the expression. *)
}

and exp_node =
  | Const      of constant              (** Constant *)
  | Lval       of lval                  (** Lvalue *)
  | SizeOf     of typ
  (** sizeof(<type>). Has [unsigned int] type (ISO 6.5.3.4). This is not
      turned into a constant because some transformations might want to change
      types *)

  | SizeOfE    of exp (** sizeof(<expression>) *)

  | SizeOfStr  of string
  (** sizeof(string_literal). We separate this case out because this is the
      only instance in which a string literal should not be treated as having
      type pointer to character. *)

  | AlignOf    of typ
  (** This corresponds to the GCC __alignof_. Has [unsigned int] type *)

  | AlignOfE   of exp

  | UnOp       of unop * exp * typ
  (** Unary operation. Includes the type of the result. *)

  | BinOp      of binop * exp * exp * typ
  (** Binary operation. Includes the type of the result. The arithmetic
      conversions are made explicit for the arguments.
      @plugin development guide *)

  | CastE      of typ * exp
  (** Use {!Cil.mkCast} to make casts.  *)

  | AddrOf     of lval
  (** Always use {!Cil.mkAddrOf} to construct one of these. Apply to an lvalue
      of type [T] yields an expression of type [TPtr(T)] *)

  | StartOf    of lval
  (** Conversion from an array to a pointer to the beginning of the array.
      Given an lval of type [TArray(T)] produces an expression of type
      [TPtr(T)]. In C this operation is implicit, the [StartOf] operator is not
      printed. We have it in CIL because it makes the typing rules simpler. *)

  | Info       of exp * exp_info
(** Additional information on the underlying expression *)

(** Additional information on an expression *)
and exp_info = {
  exp_type : logic_type; (** when used as placeholder for a term *)
  exp_name: string list;
}

(* ************************************************************************* *)
(** {2 Constants} *)
(* ************************************************************************* *)

(** Literal constants *)
and constant =
  | CInt64 of Integer.t * ikind * string option
  (** Integer constant. Give the ikind (see ISO9899 6.1.3.2) and the
      textual representation. Textual representation is always set to Some s
      when it comes from user code. This allows us to print a
      constant as it was represented in the code, for example,
      0xF instead of 15. It is usually None for constant generated by Cil
      itself. Use {!Cil.integer} or {!Cil.kinteger} to create these. *)

  | CStr of string
  (** String constant. The escape characters inside the string have been already
      interpreted. This constant has pointer to character type! The only case
      when you would like a string literal to have an array type is when it is
      an argument to sizeof. In that case you should use SizeOfStr. *)

  | CWStr of int64 list
  (** Wide character string constant. Note that the local interpretation of such
      a literal depends on {!Cil.theMachine.wcharType} and
      {!Cil.theMachine.wcharKind}.  Such a constant has type pointer to
      {!Cil.theMachine.wcharType}. The escape characters in the string have not
      been "interpreted" in the sense that L"A\xabcd" remains "A\xabcd" rather
      than being represented as the wide character list with two elements: 65
      and 43981. That "interpretation" depends on the underlying wide character
      type. *)

  | CChr of char
  (** Character constant.  This has type int, so use charConstToInt to read the
      value in case sign-extension is needed. *)

  | CReal of float * fkind * string option
  (** Floating point constant. Give the fkind (see ISO 6.4.4.2) and also the
      textual representation, if available. *)

  | CEnum of enumitem
(** An enumeration constant. Use [Cillower.lowerEnumVisitor] to replace these
    with integer constants. *)

(** Unary operators *)
and unop =
    Neg   (** Unary minus *)
  | BNot  (** Bitwise complement (~) *)
  | LNot  (** Logical Not (!) *)

(** Binary operations *)
and binop =
    PlusA    (** arithmetic + *)
  | PlusPI   (** pointer + integer *)
  | IndexPI  (** pointer + integer but only when it arises from an expression
                 [e\[i\]] when [e] is a pointer and
                 not an array. This is semantically
                 the same as PlusPI but CCured uses
                 this as a hint that the integer is
                 probably positive. *)
  | MinusA   (** arithmetic - *)
  | MinusPI  (** pointer - integer *)
  | MinusPP  (** pointer - pointer *)
  | Mult     (** * *)
  | Div      (** /      
		 @plugin development guide *)
  | Mod      (** % 
		 @plugin development guide *)
  | Shiftlt  (** shift left *)
  | Shiftrt  (** shift right *)

  | Lt       (** <  (arithmetic comparison) *)
  | Gt       (** >  (arithmetic comparison) *)
  | Le       (** <= (arithmetic comparison) *)
  | Ge       (** >= (arithmetic comparison) *)
  | Eq       (** == (arithmetic comparison) *)
  | Ne       (** != (arithmetic comparison) *)
  | BAnd     (** bitwise and *)
  | BXor     (** exclusive-or *)
  | BOr      (** inclusive-or *)

  | LAnd     (** logical and. Unlike other expressions this one does not always
                 evaluate both operands. If you want
                 to use these, you must set
                 {!Cil.useLogicalOperators}. *)
  | LOr      (** logical or. Unlike other expressions this one does not always
                 evaluate both operands.  If you
                 want to use these, you must set
                 {!Cil.useLogicalOperators}. *)

(* ************************************************************************* *)
(** {2 Left values} *)
(* ************************************************************************* *)

(** Left values (aka Lvalues) are the sublanguage of expressions that can appear
    at the left of an assignment or as operand to the address-of operator.  In C
    the syntax for lvalues is not always a good indication of the meaning of the
    lvalue. For example the C value {v a[0][1][2] v} might involve 1, 2 or 3
    memory reads when used in an expression context, depending on the declared
    type of the variable [a]. If [a] has type [int \[4\]\[4\]\[4\]] then we have
    one memory read from somewhere inside the area that stores the array [a]. On
    the other hand if [a] has type [int ***] then the expression really means [*
    ( * ( * (a + 0) + 1) + 2)], in which case it is clear that it involves three
    separate memory operations.

    An lvalue denotes the contents of a range of memory addresses. This range is
    denoted as a host object along with an offset within the object. The host
    object can be of two kinds: a local or global variable, or an object whose
    address is in a pointer expression. We distinguish the two cases so that we
    can tell quickly whether we are accessing some component of a variable
    directly or we are accessing a memory location through a pointer.  To make
    it easy to tell what an lvalue means CIL represents lvalues as a host object
    and an offset (see {!Cil_types.lval}). The host object (represented as
    {!Cil_types.lhost}) can be a local or global variable or can be the object
    pointed-to by a pointer expression. The offset (represented as
    {!Cil_types.offset}) is a sequence of field or array index designators.

    Both the typing rules and the meaning of an lvalue is very precisely
    specified in CIL.

    The following are a few useful function for operating on lvalues:
    - {!Cil.mkMem} - makes an lvalue of [Mem] kind. Use this to ensure
    that certain equivalent forms of lvalues are canonized.
    For example, [*&x = x].
    - {!Cil.typeOfLval} - the type of an lvalue
    - {!Cil.typeOffset} - the type of an offset, given the type of the
    host.
    - {!Cil.addOffset} and {!Cil.addOffsetLval} - extend sequences
    of offsets.
    - {!Cil.removeOffset} and {!Cil.removeOffsetLval} - shrink sequences
    of offsets.

    The following equivalences hold {v
    Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off
    Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off
    AddrOf (Mem a, NoOffset)        = a
    v} *)

and lval = lhost * offset

(** The host part of an {!Cil_types.lval}. *)
and lhost =
  | Var        of varinfo
  (** The host is a variable. *)

  | Mem        of exp
(** The host is an object of type [T] when the expression has pointer
    [TPtr(T)]. *)


(** The offset part of an {!Cil_types.lval}. Each offset can be applied to
    certain kinds of lvalues and its effect is that it advances the starting
    address of the lvalue and changes the denoted type, essentially focussing
    to some smaller lvalue that is contained in the original one.  
    @plugin development guide *)
and offset =
  | NoOffset
  (** No offset. Can be applied to any lvalue and does not change either the
      starting address or the type.  This is used when the lval consists of just
      a host or as a terminator in a list of other kinds of offsets. *)

  | Field      of fieldinfo * offset
  (** A field offset. Can be applied only to an lvalue that denotes a structure
      or a union that contains the mentioned field. This advances the offset to
      the beginning of the mentioned field and changes the type to the type of
      the mentioned field. *)

  | Index    of exp * offset
(** An array index offset. Can be applied only to an lvalue that denotes an
    array. This advances the starting address of the lval to the beginning of
    the mentioned array element and changes the denoted type to be the type of
    the array element *)

(* ************************************************************************* *)
(** {2 Initializers} *)
(* ************************************************************************* *)

(** A special kind of expressions are those that can appear as initializers for
    global variables (initialization of local variables is turned into
    assignments). The initializers are represented as type
    {!Cil_types.init}. You can create initializers with {!Cil.makeZeroInit} and
    you can conveniently scan compound initializers them with
    {!Cil.foldLeftCompound}. *)

(** Initializers for global variables. *)
and init =
  | SingleInit   of exp   (** A single initializer *)
  | CompoundInit   of typ * (offset * init) list
(** Used only for initializers of structures, unions and arrays.  The offsets
    are all of the form [Field(f, NoOffset)] or [Index(i, NoOffset)] and
    specify the field or the index being initialized. For structures all fields
    must have an initializer (except the unnamed bitfields), in the proper
    order. This is necessary since the offsets are not printed. For arrays the
    list must contain a prefix of the initializers; the rest are 0-initialized.
    For unions there must be exactly one initializer. If the initializer is not
    for the first field then a field designator is printed, so you better be on
    GCC since MSVC does not understand this. You can scan an initializer list
    with {!Cil.foldLeftCompound}. *)

(** We want to be able to update an initializer in a global variable, so we
    define it as a mutable field *)
and initinfo = { mutable init : init option }

(* ************************************************************************* *)
(** {2 Function definitions} *)
(* ************************************************************************* *)

(** A function definition is always introduced with a [GFun] constructor at the
    top level. All the information about the function is stored into a
    {!Cil_types.fundec}. Some of the information (e.g. its name, type, storage,
    attributes) is stored as a {!Cil_types.varinfo} that is a field of the
    [fundec]. To refer to the function from the expression language you must use
    the [varinfo].

    The function definition contains, in addition to the body, a list of all the
    local variables and separately a list of the formals. Both kind of variables
    can be referred to in the body of the function. The formals must also be
    shared with the formals that appear in the function type. For that reason,
    to manipulate formals you should use the provided functions
    {!Cil.makeFormalVar} and {!Cil.setFormals}.  *)

(** Function definitions. *)
and fundec = { 
  mutable svar:     varinfo;
  (** Holds the name and type as a variable, so we can refer to it easily
      from the program. All references to this function either in a function
      call or in a prototype must point to the same [varinfo]. *)

  mutable sformals: varinfo list;
      (** Formals. These must be in the same order and with the same information
          as the formal information in the type of the function.  Use
          {!Cil.setFormals} or {!Cil.setFunctionType} to set these formals and
          ensure that they are reflected in the function type. Do not make
          copies of these because the body refers to them. *)
      
  mutable slocals: varinfo list;
  (** Locals. Does NOT include the sformals. Do not make copies of these
      because the body refers to them. *)

  mutable smaxid: int;
  (** Max local id. Starts at 0. Used for creating the names of new
      temporary variables. Updated by {!Cil.makeLocalVar} and
      {!Cil.makeTempVar}. You can also use {!Cil.setMaxId} to set it after
      you have added the formals and locals. *)

  mutable sbody: block; (** The function body. *)

  mutable smaxstmtid: int option;
  (** max id of a (reachable) statement in this function, if we have
      computed it. range = 0 ...  (smaxstmtid-1). This is computed by
      {!Cfg.computeCFGInfo}. *)

  mutable sallstmts: stmt list; 
  (** After you call {!Cfg.computeCFGInfo} this field is set to contain all
      statements in the function. *)

  mutable sspec: funspec;
}

(** A block is a sequence of statements with the control falling through from
    one element to the next *)
and block = { 
  mutable battrs: attributes; (** Attributes for the block *)

  mutable blocals: varinfo list; 
  (** variables that are local to the block. It is a subset of the slocals of
      the enclosing function. *)

  mutable bstmts: stmt list;  (** The statements comprising the block. *)
}

(* ************************************************************************* *)
(** {2 Statements} *)
(* ************************************************************************* *)

(** CIL statements are the structural elements that make the CFG. They are
    represented using the type {!Cil_types.stmt}. Every statement has a
    (possibly empty) list of labels. The {!Cil_types.stmtkind} field of a
    statement indicates what kind of statement it is.

    Use {!Cil.mkStmt} to make a statement and the fill-in the fields.

    CIL also comes with support for control-flow graphs. The [sid] field in
    [stmt] can be used to give unique numbers to statements, and the [succs] and
    [preds] fields can be used to maintain a list of successors and predecessors
    for every statement. The CFG information is not computed by default. Instead
    you must explicitly use the functions {!Cfg.prepareCFG} and
    {!Cfg.computeCFGInfo} to do it. *)

(** Statements.
    @plugin development guide *)
and stmt = {
  mutable labels: label list;
  (** Whether the statement starts with some labels, case statements or
      default statements. *)

  mutable skind: stmtkind;
  (** The kind of statement *)

  mutable sid: int;
  (** A number (>= 0) that is unique in a function. Filled in only after the
      CFG is computed. *)

  mutable succs: stmt list;
  (** The successor statements. They can always be computed from the skind and
      the context in which this statement appears. Filled in only after the CFG
      is computed. *)

  mutable preds: stmt list;
  (** The inverse of the succs function. *)

  mutable ghost : bool
}

(** Labels *)
and label =
  | Label of string * location * bool
  (** A real label. If the bool is "true", the label is from the input source
      program. If the bool is "false", the label was created by CIL or some
      other transformation *)

  | Case of exp * location
  (** A case statement. This expression is lowered into a constant if
      {!Cil.lowerConstants} is set to [true]. *)

  | Default of location  (** A default statement *)

(* The various kinds of statements *)
and stmtkind =
  | Instr  of instr
  (** An instruction that does not contain control flow. Control implicitly
      falls through. *)

  | Return of exp option * location
  (** The return statement. This is a leaf in the CFG. *)

  | Goto of stmt ref * location
  (** A goto statement. Appears from actual goto's in the code or from goto's
      that have been inserted during elaboration. The reference points to the
      statement that is the target of the Goto. This means that you have to
      update the reference whenever you replace the target statement. The
      target statement MUST have at least a label. *)

  | Break of location
  (** A break to the end of the nearest enclosing Loop or Switch *)

  | Continue of location
  (** A continue to the start of the nearest enclosing [Loop] *)

  | If of exp * block * block * location
  (** A conditional. Two successors, the "then" and the "else" branches. Both
      branches fall-through to the successor of the If statement. *)

  | Switch of exp * block * (stmt list) * location
  (** A switch statement. [exp] is the index of the switch. [block] is
      the body of the switch. [stmt list] contains the set of
      statements whose [labels] are cases of the switch (i.e. for each
      case, the corresponding statement is in [stmt list], a statement
      cannot appear more than once in the list, and statements in
      [stmt list] can have several labels corresponding to several
      cases. *)

  | Loop of 
      code_annotation list * block * location * (stmt option) * (stmt option)
  (** A [while(1)] loop. The termination test is implemented in the body of a
      loop using a [Break] statement. If {!Cfg.prepareCFG} has been called, the
      first stmt option will point to the stmt containing the continue label
      for this loop and the second will point to the stmt containing the break
      label for this loop. *)

  | Block of block
  (** Just a block of statements. Use it as a way to keep some block attributes
      local *)

  | UnspecifiedSequence of (stmt * lval list
                            * lval list * lval list * stmt ref list) list
  (** statements whose order of execution is not specified by
      ISO/C.  This is important for the order of side effects
      during evaluation of expressions. Each statement comes
      together with three list of lval, in this order.
      - lvals that are written during the sequence and whose future
      value depends upon the statement (it is legal to read from them, but
      not to write to them)
      - lvals that are written during the evaluation of the statement itself
      - lval that are read.
      - Function calls in the corresponding statement
      Note that this include only a subset of the affectations
      of the statement.  Namely, the
      temporary variables generated by cil are excluded (i.e.  it
      is assumed that the "compilation" is correct). In addition,
      side effects caused by function applications are not taken
      into account in the list. For a single statement, the written lvals
      are supposed to be ordered (or their order of evaluation doesn't
      matter), so that an alarm should be emitted only if the lvals read by
      a statement overlap with the lvals written (or read) by another
      statement of the sequence.

      At this time this feature is
      experimental and may miss some unspecified sequences.

      In case you do not care about this feature just handle it
      like a block (see {!Cil.block_from_unspecified_sequence})  *)

  | TryFinally of block * block * location
  (** On MSVC we support structured exception handling. This is what you might
      expect. Control can get into the finally block either from the end of the
      body block, or if an exception is thrown. *)

  | TryExcept of block * (instr list * exp) * block * location
(** On MSVC we support structured exception handling. The try/except
     statement is a bit tricky:
    {v         __try \{ blk \}
    __except (e) \{
    handler
    \}
    v}

    The argument to __except  must be an expression. However, we keep a
    list of instructions AND an expression in case you need to make
    function calls. We'll print those as a comma expression. The control
    can get to the __except expression only if an exception is thrown.
    After that, depending on the value of the expression the control
    goes to the handler, propagates the exception, or retries the
    exception. The location corresponds to the try keyword. *)

(** Instructions. They may cause effects directly but may not have control
    flow.*)
and instr =
  | Set of lval * exp * location
  (** An assignment. A cast is present if the exp has different type from
      lval *)

  | Call of lval option * exp * exp list * location
  (** optional: result is an lval. A cast might be necessary if the declared
      result type of the function is not the same as that of the destination.
      Actual arguments must have a type equivalent (i.e. {!Cil.need_cast} must
      return [false]) to the one of the formals of the function.
      If the type of the result variable is not the same as the declared type of
      the function result then an implicit cast exists. 
  *)

  (* See the GCC specification for the meaning of ASM.
    If the source is MS VC then only the templates
    are used.

     [sm] I've added a notes.txt file which contains more
     information on interpreting Asm instructions *)
  | Asm of
      attributes (* Really only const and volatile can appear here *) 
    * string list (* templates (CR-separated) *)
    * (string option * string * lval) list
      (* outputs must be lvals with optional names and constraints.  I would
         like these to be actually variables, but I run into some trouble with
         ASMs in the Linux sources *)
    * (string option * string * exp) list
    (* inputs with optional names and constraints *)
    * string list (* register clobbers *)
    * (stmt ref) list  (* list of statements this asm section may jump to. Destination
			must have a label. *)
    * location
  (** An inline assembly instruction. The arguments are 
      (1) a list of attributes (only const and volatile can appear here and only
      for GCC)
      (2) templates (CR-separated)
      (3) a list of outputs, each of which is an lvalue with optional names and
     constraints.
      (4) a list of input expressions along with constraints
      (5) clobbered registers
      (6) Possible destinations statements
      (7) location information *)

  | Skip of location

  | Code_annot of code_annotation * location

(** Describes a location in a source file *)
and location = Lexing.position * Lexing.position

(** {1 Abstract syntax trees for annotations} *)

and logic_constant =
  | Integer of Integer.t * string option 
  (** Integer constant with a textual representation.  *)
  | LStr of string (** String constant. *)
  | LWStr of int64 list (** Wide character string constant. *)
  | LChr of char (** Character constant. *)
  | LReal of logic_real
  | LEnum of enumitem (** An enumeration constant.*)

(** Real constants. *)
and logic_real = {
  r_literal : string ; (** Initial string representation [s]. *)
  r_nearest : float ;  (** Nearest approximation of [s] in double precision. *)
  r_upper : float ;    (** Smallest double [u] such that [s <= u]. *)
  r_lower : float ;    (** Greatest double [l] such that [l <= s]. *)
}

(** Types of logic terms. *)
and logic_type =
  | Ctype of typ (** a C type *)
  | Ltype of logic_type_info * logic_type list
      (** an user-defined logic type with its parameters *)
  | Lvar of string (** a type variable. *)
  | Linteger (** mathematical integers, {i i.e.} Z *)
  | Lreal    (** mathematical reals, {i i.e.} R *)
  | Larrow of logic_type list * logic_type (** (n-ary) function type *)

(** tsets with an unique identifier.
    Use [Logic_const.new_location] to generate a new id. *)
and identified_term = {
  it_id: int; (** the identifier. *)
  it_content: term (** the term *)
}

(** logic label referring to a particular program point. *)
and logic_label =
  | StmtLabel of stmt ref (** label of a C statement. *)
  | LogicLabel of (stmt option * string) (* [JS 2011/05/13] why a tuple here? *)
(** builtin logic label ({t Here, Pre}, ...) *)

(* ************************************************************************* *)
(** {2 Terms} *)
(* ************************************************************************* *)

(** C Expressions as logic terms follow C constructs (with prefix T) *)

(** Logic terms. *)
and term = {
  term_node : term_node; (** kind of term. *)
  term_loc : Lexing.position * Lexing.position;
  (** position in the source file. *)
  term_type : logic_type; (** type of the term. *)
  term_name: string list; 
   (** names of the term if any. A name can be an arbitrary string, where
     '"' and '\'' are escaped by a \, and which does not end with a \. 
     Hence, "name" and 'name' should be recognized as a unique label by most
     tools. *)
}

(** the various kind of terms. *)
and term_node =
  (* same constructs as exp *)
  | TConst of logic_constant (** a constant. *)
  | TLval of term_lval (** an L-value *)
  | TSizeOf of typ (** size of a given C type. *)
  | TSizeOfE of term (** size of the type of an expression. *)
  | TSizeOfStr of string (** size of a string constant. *)
  | TAlignOf of typ (** alignment of a type. *)
  | TAlignOfE of term (** alignment of the type of an expression. *)
  | TUnOp of unop * term (** unary operator. *)
  | TBinOp of binop * term * term (** binary operators. *)
  | TCastE of typ * term (** cast to a C type. *)
  | TAddrOf of term_lval (** address of a term. *)
  | TStartOf of term_lval (** beginning of an array. *)

  (* additional constructs *)
  | Tapp of logic_info * (logic_label * logic_label) list * term list
      (** application of a logic function. *)
  | Tlambda of quantifiers * term (** lambda abstraction. *)
  | TDataCons of logic_ctor_info * term list
      (** constructor of logic sum-type. *)
  | Tif of term * term * term
      (** conditional operator*)
  | Tat of term * logic_label
      (** term refers to a particular program point. *)
  | Tbase_addr of logic_label * term (** base address of a pointer. *)
  | Toffset of logic_label * term (** offset from the base address of a pointer. *)
  | Tblock_length of logic_label * term (** length of the block pointed to by the term. *)
  | Tnull (** the null pointer. *)
  | TLogic_coerce of logic_type * term
  (** implicit conversion from a C type to a logic type. 
      The logic type must not be a Ctype. In particular, used to denote
      lifting to Linteger and Lreal.
  *)
  | TCoerce of term * typ (** coercion to a given C type. *)
  | TCoerceE of term * term (** coercion to the type of a given term. *)
  | TUpdate of term * term_offset * term
      (** functional update of a field. *)
  | Ttypeof of term (** type tag for a term. *)
  | Ttype of typ (** type tag for a C type. *)
  | Tempty_set (** the empty set. *)
  | Tunion of term list (** union of terms. *)
  | Tinter of term list (** intersection of terms. *)
  | Tcomprehension of
      term * quantifiers * predicate named option
  (** set defined in comprehension ({t \{ t[i] | integer i; 0 <= i < 5\}}) *)
  | Trange of term option * term option (** range of integers. *)
  | Tlet of logic_info * term (** local binding *)

(** lvalue: base address and offset. *)
and term_lval =
    term_lhost * term_offset

(** base address of an lvalue. *)
and term_lhost =
  | TVar of logic_var (** a variable. *)
  | TResult of typ (** value returned by a C function.
                       Only used in post-conditions or assigns
                    *)
  | TMem of term (** memory access. *)

(** model field. *)
and model_info = {
  mi_name: string; (** name *)
  mi_field_type: logic_type; (** type of the field *)
  mi_base_type: typ; (** type to which the field is associated. *)
  mi_decl: location; (** where the field has been declared. *)
}

(** offset of an lvalue. *)
and term_offset =
  | TNoOffset (** no further offset. *)
  | TField of fieldinfo * term_offset
      (** access to the field of a compound type. *)
  | TModel of model_info * term_offset (** access to a model field. *)
  | TIndex of term * term_offset
      (** index. Note that a range is denoted by [TIndex(Trange(i1,i2),ofs)] *)

(** description of a logic function or predicate.
@plugin development guide *)
and logic_info = {
(*
  mutable l_name : string; (** name of the function. *)
*)
  mutable l_var_info : logic_var;
  (** we use only fields lv_name and lv_id of l_var_info
      we should factorize lv_type and l_type+l_profile below *)
  mutable l_labels : logic_label list; (** label arguments of the function. *)
  mutable l_tparams : string list; (** type parameters *)
  mutable l_type : logic_type option; (** return type. None for predicates *)
  mutable l_profile : logic_var list; (** type of the arguments. *)
  mutable l_body : logic_body; (** body of the function. *)
}

and builtin_logic_info = { 
  mutable bl_name: string;
  mutable bl_labels: logic_label list;
  mutable bl_params: string list;
  mutable bl_type: logic_type option;
  mutable bl_profile: (string * logic_type) list;
}

and logic_body =
  | LBnone (** no definition and no reads clause *)
  | LBreads of identified_term list
  (** read accesses performed by a function. *)
  | LBterm of term (** direct definition of a function. *)
  | LBpred of predicate named (** direct definition of a predicate. *)
  | LBinductive of
      (string * logic_label list * string list * predicate named) list
	(** inductive definition *)

(** Description of a logic type.
    @plugin development guide *)
and logic_type_info = { 
  lt_name: string;
  lt_params : string list; (** type parameters*)
  mutable lt_def: logic_type_def option
    (** definition of the type. None for abstract types. *)
}
(* will be expanded when dealing with concrete types *)

and logic_type_def =
  | LTsum of logic_ctor_info list (** sum type with its constructors. *)
  | LTsyn of logic_type (** Synonym of another type. *)

(** origin of a logic variable. *)
and logic_var_kind = 
  | LVGlobal (** global logic function or predicate. *)
  | LVC (** Logic counterpart of a C variable. *)
  | LVFormal (** formal parameter of a logic function / predicate
                 or \lambda abstraction *)
  | LVQuant (** Bound by a quantifier (\exists or \forall) *)
  | LVLocal (** local \let *)

(** description of a logic variable
@plugin development guide *)
and logic_var = {
  mutable lv_name : string; (** name of the variable. *)
  mutable lv_id : int; (** unique identifier *)
  mutable lv_type : logic_type; (** type of the variable. *)
  mutable lv_kind: logic_var_kind; (** kind of the variable *)
  mutable lv_origin : varinfo option
(** when the logic variable stems from a C variable, set to the original C
    variable.  *)
}

(** Description of a constructor of a logic sum-type.
    @plugin development guide *)
and logic_ctor_info =
 { ctor_name: string; (** name of the constructor. *)
   ctor_type: logic_type_info; (** type to which the constructor belongs. *)
   ctor_params: logic_type list 
 (** types of the parameters of the constructor. *)
 }

(* ************************************************************************* *)
(** {2 Predicates} *)
(* ************************************************************************* *)

(** variables bound by a quantifier. *)
and quantifiers = logic_var list

(** comparison relations*)
and relation = 
  | Rlt
  | Rgt
  | Rle
  | Rge
  | Req 
  | Rneq (** @plugin development guide *)


(** predicates *)
and predicate =
  | Pfalse (** always-false predicate. *)
  | Ptrue (** always-true predicate. *)
  | Papp of logic_info * (logic_label * logic_label) list * term list
      (** application of a predicate. *)
  | Pseparated of term list
  | Prel of relation * term * term (** comparison of two terms. *)
  | Pand of predicate named * predicate named (** conjunction *)
  | Por of predicate named * predicate named  (** disjunction. *)
  | Pxor of predicate named * predicate named (** logical xor. *)
  | Pimplies of predicate named * predicate named (** implication. *)
  | Piff of predicate named * predicate named (** equivalence. *)
  | Pnot of predicate named                   (** negation. *)
  | Pif of term * predicate named * predicate named  (** conditional *)
  | Plet of logic_info * predicate named (** definition of a local variable *)
  | Pforall of quantifiers * predicate named (** universal quantification. *)
  | Pexists of quantifiers * predicate named (** existential quantification. *)
  | Pat of predicate named * logic_label
  (** predicate refers to a particular program point. *)
  | Pvalid_read of logic_label * term   (** the given locations are valid for reading. *)
  | Pvalid of logic_label * term   (** the given locations are valid. *)
  (** | Pvalid_index of term * term
      {b deprecated:} Use [Pvalid(TBinOp(PlusPI,p,i))] instead.
          [Pvalid_index(p,i)] indicates that accessing the [i]th element
          of [p] is valid.
      | Pvalid_range of term * term * term
       {b deprecated:} Use [Pvalid(TBinOp(PlusPI(p,Trange(i1,i2))))] instead.
          similar to [Pvalid_index] but for a range of indices.*)
  | Pinitialized of logic_label * term   (** the given locations are initialized. *)
  | Pallocable of logic_label * term   (** the given locations can be allocated. *)
  | Pfreeable of logic_label * term   (** the given locations can be free. *)
  | Pfresh of logic_label * logic_label * term * term
      (** \fresh(pointer, n)
	  A memory block of n bytes is newly allocated to the pointer.*)
  | Psubtype of term * term
      (** First term is a type tag that is a subtype of the second. *)

(** predicate with an unique identifier.  Use [Logic_const.new_predicate] to
    create fresh predicates *)
and identified_predicate = {
  mutable ip_name: string list; (** names given to the predicate if any.*)
  ip_loc: location; (** location in the source code. *)
  ip_id: int; (** identifier *)
  ip_content: predicate; (** the predicate itself*)
}

(*  Polymorphic types shared with parsed trees (Logic_ptree) *)
(** variant of a loop or a recursive function. Type shared with Logic_ptree. *)
and 'term variant = 'term * string option

(** allocates and frees. 
    @since Oxygen-20120901  *)
and 'locs allocation =
  | FreeAlloc of 'locs list * 'locs list (** tsets. Empty list means \nothing. *)
  | FreeAllocAny (** Nothing specified. Semantics depends on where it 
		     is written. *)

(** dependencies of an assigned location. Shared with Logic_ptree. *)
and 'locs deps =
  | From of 'locs list (** tsets. Empty list means \nothing. *)
  | FromAny (** Nothing specified. Any location can be involved. *)

and 'locs from = ('locs * 'locs deps)

(** zone assigned with its dependencies. Type shared with Logic_ptree. *)
and 'locs assigns = 
  | WritesAny (** Nothing specified. Anything can be written. *)
  | Writes of 'locs from list
    (** list of locations that can be written. Empty list means \nothing. *)

(** object that can be named (in particular predicates). *)
and 'a named = { 
  name : string list; (** list of given names *)
  loc : location;     (** position in the source code. *)
  content : 'a;       (** content *)
}

(** Function contract. Type shared with Logic_ptree. *)
and ('term,'pred,'locs) spec = {
  mutable spec_behavior : ('pred,'locs) behavior list;
  (** behaviors *)

  mutable spec_variant : 'term variant option;
  (** variant for recursive functions. *)

  mutable spec_terminates: 'pred option;
  (** termination condition. *)

  mutable spec_complete_behaviors: string list list;
  (** list of complete behaviors.
      It is possible to have more than one set of complete behaviors *)

  mutable spec_disjoint_behaviors: string list list;
  (** list of disjoint behaviors.
     It is possible to have more than one set of disjoint behaviors *)
}

(** Behavior of a function. Type shared with Logic_ptree.
    @since Oxygen-20120901 [b_allocation] has been added.
    @since Carbon-20101201 [b_requires] has been added.
    @modify Boron-20100401 [b_ensures] is replaced by [b_post_cond].
    Old [b_ensures] represent the [Normal] case of [b_post_cond]. *)
and ('pred,'locs) behavior = {
  mutable b_name : string; (** name of the behavior. *)
  mutable b_requires : 'pred list; (** require clauses. *)
  mutable b_assumes : 'pred list; (** assume clauses. *)
  mutable b_post_cond : (termination_kind * 'pred) list; (** post-condition. *)
  mutable b_assigns : 'locs assigns; (** assignments. *)
  mutable b_allocation : 'locs allocation; (** frees, allocates. *)
  mutable b_extended : (string * int * 'pred list) list
(** Grammar extensions *)
}

(** kind of termination a post-condition applies to. See ACSL manual. *)
and termination_kind = Normal | Exits | Breaks | Continues | Returns

(** Pragmas for the value analysis plugin of Frama-C.
    Type shared with Logic_ptree.*)
and 'term loop_pragma =
  | Unroll_specs of 'term list
  | Widen_hints of 'term list
  | Widen_variables of 'term list

(** Pragmas for the slicing plugin of Frama-C. Type shared with Logic_ptree.*)
and 'term slice_pragma =
  | SPexpr of 'term
  | SPctrl
  | SPstmt

(** Pragmas for the impact plugin of Frama-C. Type shared with Logic_ptree.*)
and 'term impact_pragma =
  | IPexpr of 'term
  | IPstmt

(** The various kinds of pragmas. Type shared with Logic_ptree. *)
and 'term pragma =
  | Loop_pragma of 'term loop_pragma
  | Slice_pragma of 'term slice_pragma
  | Impact_pragma of 'term impact_pragma

(** all annotations that can be found in the code.
    Type shared with Logic_ptree. *)
and ('term, 'pred, 'spec_pred, 'locs) code_annot =
  | AAssert of string list * 'pred
  (** assertion to be checked. The list of strings is the list of
      behaviors to which this assertion applies. *)

  | AStmtSpec of string list * ('term, 'spec_pred, 'locs) spec
  (** statement contract eventualy for some behaviors. *)

  | AInvariant of string list * bool * 'pred
  (** loop/code invariant. The list of strings is the list of behaviors to which
      this invariant applies.  The boolean flag is true for normal loop
      invariants and false for invariant-as-assertions. *)

  | AVariant of 'term variant
  (** loop variant. Note that there can be at most one variant associated to a
      given statement *)

  | AAssigns of string list * 'locs assigns
  (** loop assigns.  (see [b_assigns] in the behaviors for other assigns).  At
      most one clause associated to a given (statement, behavior) couple.  *)

  | AAllocation of string list * 'locs allocation
  (** loop allocation clause.  (see [b_allocation] in the behaviors for other
      allocation clauses).
      At most one clause associated to a given (statement, behavior) couple.
      @since Oxygen-20120901 when [b_allocation] has been added.  *)

  | APragma of 'term pragma (** pragma. *)

(** function contract. *)
and funspec = (term, identified_predicate, identified_term) spec

(** code annotation with an unique identifier.
    Use [Logic_const.new_code_annotation] to create new code annotations with
    a fresh id. *)
and code_annotation = { 
  annot_id: int; (** identifier. *) 
  annot_content :
  (term, predicate named, identified_predicate, identified_term) code_annot;
      (** content of the annotation. *)
}

(** behavior of a function. *)
and funbehavior = (identified_predicate,identified_term) behavior

(** global annotations, not attached to a statement or a function. *)
and global_annotation =
  | Dfun_or_pred of logic_info * location
  | Dvolatile of 
      identified_term list * varinfo option * varinfo option * location
  (** associated terms, reading function, writing function *)
  | Daxiomatic of string * global_annotation list * location
  | Dtype of logic_type_info * location (** declaration of a logic type. *)
  | Dlemma of
      string * bool * logic_label list * string list *
        predicate named * location
  (** definition of a lemma. The boolean flag is [true] if the property should
      be taken as an axiom and [false] if it must be proved.  *)
  | Dinvariant of logic_info * location
  (** global invariant. The predicate does not have any argument. *)
  | Dtype_annot of logic_info * location
  (** type invariant. The predicate has exactly one argument. *)
  | Dmodel_annot of model_info * location
  (** Model field for a type t, seen as a logic function with one 
      argument of type t *)
  | Dcustom_annot of custom_tree * string* location
      (*Custom declaration*)

and custom_tree = CustomDummy
(*
  | CustomType of logic_type
  | CustomLexpr of lexpr
  | CustomOther of string * (custom_tree list)
*)

type kinstr =
  | Kstmt of stmt
  | Kglobal

(** Internal representation of decorated C functions *)
type cil_function =
  | Definition of (fundec * location) (** defined function *)
  | Declaration of (funspec * varinfo * varinfo list option * location)
      (** Declaration(spec,f,args,loc) represents a leaf function [f] with
          specification [spec] and arguments [args], at location [loc]. As
          with the [TFun] constructor of {!Cil_types.typ}, the arg list is
          optional, to distinguish [void f()] ([None]) from
          [void f(void)] ([Some []]). *)

(** Except field [fundec], do not use the other fields directly.
    Prefer to use {!Kernel_function.find_return}, {!Annotations.funspec},
    [Annotations.add_*] or [Annotations.remove_*]. *)
type kernel_function = {
  mutable fundec : cil_function;
  mutable return_stmt : stmt option;
  mutable spec : funspec;
}

(* [VP] TODO: VLocal should be attached to a particular block, not a whole
   function. *)
type localisation =
  | VGlobal
  | VLocal of kernel_function
  | VFormal of kernel_function

type mach = {
  version_major: int;     (* Major version number *)
  version_minor: int;     (* Minor version number *)
  version: string;        (* version number *)
  underscore_name: bool;  (* If assembly names have leading underscore *)
  sizeof_short: int;      (* Size of "short" *)
  sizeof_int: int;        (* Size of "int" *)
  sizeof_long: int ;      (* Size of "long" *)
  sizeof_longlong: int;   (* Size of "long long" *)
  sizeof_ptr: int;        (* Size of pointers *)
  sizeof_float: int;      (* Size of "float" *)
  sizeof_double: int;     (* Size of "double" *)
  sizeof_longdouble: int; (* Size of "long double" *)
  sizeof_void: int;       (* Size of "void" *)
  sizeof_fun: int;        (* Size of function *)
  size_t: string;         (* Type of "sizeof(T)" *)
  wchar_t: string;        (* Type of "wchar_t" *)
  ptrdiff_t: string;      (* Type of "ptrdiff_t" *)
  alignof_short: int;     (* Alignment of "short" *)
  alignof_int: int;       (* Alignment of "int" *)
  alignof_long: int;      (* Alignment of "long" *)
  alignof_longlong: int;  (* Alignment of "long long" *)
  alignof_ptr: int;       (* Alignment of pointers *)
  alignof_float: int;     (* Alignment of "float" *)
  alignof_double: int;    (* Alignment of "double" *)
  alignof_longdouble: int;  (* Alignment of "long double" *)
  alignof_str: int;       (* Alignment of strings *)
  alignof_fun: int;       (* Alignment of function *)
  char_is_unsigned: bool; (* Whether "char" is unsigned *)
  const_string_literals: bool; (* Whether string literals have const chars *)
  little_endian: bool; (* whether the machine is little endian *)
  alignof_aligned: int (* Alignment of a type with aligned attribute *);
  has__builtin_va_list: bool (* Whether [__builtin_va_list] is a known type *);
  __thread_is_keyword: bool (* Whether [__thread] is a keyword *);
}

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
