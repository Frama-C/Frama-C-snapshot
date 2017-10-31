(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types

(* Note: these could be made configurable, but since debugging requires frequent
   modifications of settings and recompilation, it may be simpler to just leave
   them here. *)

(* Ideally, these pretty-printers should produce code that is valid OCaml, but
   this is not required. *)

(* print_location is often omitted. *)
let print_locations = false

(* compinfo, fieldinfo, enuminfo, typeinfo and varinfo are shortened by default.
   Setting several to true may result in infinite mutually recursive calls!
 *)
let print_full_compinfo = false
let print_full_fieldinfo = false
let print_full_enuminfo = false
let print_full_typeinfo = false
let print_full_varinfo = false

let pp_list fmt = Pretty_utils.pp_list fmt ~sep:", " ~pre:"[" ~last:"]" ~suf:"" ~empty:"[]"
let pp_option fmt = Pretty_utils.pp_opt ~none:"None" ~pre:"Some(" ~suf:")" fmt
let pp_ref pp fmt r = Format.fprintf fmt "Ref(%a)" pp !r
let pp_pair fmt1 fmt2 = Pretty_utils.pp_pair ~pre:"(" ~sep:", " ~suf:")" fmt1 fmt2
let pp_tuple3
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string ",@,")
    ?(suf=format_of_string "@]")
    fmt1 fmt2 fmt3 fmt (a, b, c) =
  Format.fprintf fmt "%(%)%a%(%)%a%(%)%a%(%)"
    pre fmt1 a sep fmt2 b sep fmt3 c suf
let pp_tuple4
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string ",@,")
    ?(suf=format_of_string "@]")
    fmt1 fmt2 fmt3 fmt4 fmt (a, b, c, d) =
  Format.fprintf fmt "%(%)%a%(%)%a%(%)%a%(%)%a%(%)"
    pre fmt1 a sep fmt2 b sep fmt3 c sep fmt4 d suf
let pp_tuple5
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string ",@,")
    ?(suf=format_of_string "@]")
    fmt1 fmt2 fmt3 fmt4 fmt5 fmt (a, b, c, d, e) =
  Format.fprintf fmt "%(%)%a%(%)%a%(%)%a%(%)%a%(%)%a%(%)"
    pre fmt1 a sep fmt2 b sep fmt3 c sep fmt4 d sep fmt5 e suf

let pp_integer fmt = Format.fprintf fmt "Integer(%a)" (Integer.pretty ~hexa:false)
let pp_int64 fmt i64 = Format.fprintf fmt "Int64(%s)" (Int64.to_string i64)
let pp_string = Format.pp_print_string
let pp_bool = Format.pp_print_bool
let pp_int = Format.pp_print_int
let pp_char = Format.pp_print_char
let pp_float = Format.pp_print_float

let pp_if_list_not_empty prefix suffix pp_l fmt l =
  if l <> [] then Format.fprintf fmt "%s%a%s" prefix pp_l l suffix
  else ()

let pp_variant pp_term = pp_pair pp_term (pp_option pp_string)

let rec pp_allocation fmt = function
  | FreeAlloc(locs_list1,locs_list2) ->
    Format.fprintf fmt "FreeAlloc(%a,%a)"
      (pp_list pp_identified_term) locs_list1 (pp_list pp_identified_term) locs_list2
  | FreeAllocAny -> Format.fprintf fmt "FreeAllocAny"

and pp_deps fmt = function
  | From(locs_list) -> Format.fprintf fmt "From(%a)" (pp_list pp_identified_term) locs_list
  | FromAny -> Format.fprintf fmt "FromAny"

and pp_from fmt = pp_pair pp_identified_term pp_deps fmt

and pp_assigns pp_locs fmt = function
  | WritesAny -> Format.fprintf fmt "WritesAny"
  | Writes(from_list) ->
    Format.fprintf fmt "Writes(%a)" (pp_list pp_locs) from_list


and pp_file fmt file = Format.fprintf fmt "{fileName=%a;globals=%a;globinit=%a;globinitcalled=%a}"
    pp_string file.fileName (pp_list pp_global) file.globals (pp_option pp_fundec) file.globinit pp_bool file.globinitcalled

and pp_global fmt = function
  | GType(typeinfo,location) -> Format.fprintf fmt "GType(%a,%a)"  pp_typeinfo typeinfo  pp_location location
  | GCompTag(compinfo,location) -> Format.fprintf fmt "GCompTag(%a,%a)"  pp_compinfo compinfo  pp_location location
  | GCompTagDecl(compinfo,location) ->
    Format.fprintf fmt "GCompTagDecl(%a,%a)"  pp_compinfo compinfo  pp_location location
  | GEnumTag(enuminfo,location) -> Format.fprintf fmt "GEnumTag(%a,%a)"  pp_enuminfo enuminfo  pp_location location
  | GEnumTagDecl(enuminfo,location) ->
    Format.fprintf fmt "GEnumTagDecl(%a,%a)"  pp_enuminfo enuminfo  pp_location location
  | GVarDecl(varinfo,location) -> Format.fprintf fmt "GVarDecl(%a,%a)"  pp_varinfo varinfo  pp_location location
  | GFunDecl(funspec,varinfo,location) ->
    Format.fprintf fmt "GFunDecl(%a,%a,%a)"  pp_funspec funspec  pp_varinfo varinfo  pp_location location
  | GVar(varinfo,initinfo,location) ->
    Format.fprintf fmt "GVar(%a,%a,%a)"   pp_varinfo varinfo  pp_initinfo initinfo  pp_location location
  | GFun(fundec,location) -> Format.fprintf fmt "GFun(%a,%a)"  pp_fundec fundec  pp_location location
  | GAsm(string,location) -> Format.fprintf fmt "GAsm(%a,%a)"  pp_string string  pp_location location
  | GPragma(attribute,location) -> Format.fprintf fmt "GPragma(%a,%a)"  pp_attribute attribute  pp_location location
  | GText(string) -> Format.fprintf fmt "GText(%a)"  pp_string string
  | GAnnot(global_annotation,location) -> Format.fprintf fmt "GAnnot(%a,%a)"  pp_global_annotation global_annotation  pp_location location

and pp_typ fmt = function
  | TVoid(attributes) -> Format.fprintf fmt "TVoid(%a)"  pp_attributes attributes
  | TInt(ikind,attributes) -> Format.fprintf fmt "TInt(%a,%a)"  pp_ikind ikind  pp_attributes attributes
  | TFloat(fkind,attributes) -> Format.fprintf fmt "TFloat(%a,%a)"  pp_fkind fkind  pp_attributes attributes
  | TPtr(typ,attributes) -> Format.fprintf fmt "TPtr(%a,%a)"  pp_typ typ  pp_attributes attributes
  | TArray(typ,exp_option,bitsSizeofTypCache,attributes) ->
    Format.fprintf fmt "TArray(%a,%a,%a,%a)"  pp_typ typ  (pp_option pp_exp) exp_option
      pp_bitsSizeofTypCache bitsSizeofTypCache  pp_attributes attributes
  | TFun(typ,string_typ_attributes_tuple_list_option,bool,attributes) ->
    Format.fprintf fmt "TFun(%a,%a,%a,%a)"  pp_typ typ
      (pp_option (pp_list (pp_tuple3 pp_string pp_typ pp_attributes))) string_typ_attributes_tuple_list_option
      pp_bool bool  pp_attributes attributes
  | TNamed(typeinfo,attributes) ->
    Format.fprintf fmt "TNamed(%a,%a)"  pp_typeinfo typeinfo  pp_attributes attributes
  | TComp(compinfo,bitsSizeofTypCache,attributes) -> Format.fprintf fmt "TComp(%a,%a,%a)"  pp_compinfo compinfo  pp_bitsSizeofTypCache bitsSizeofTypCache  pp_attributes attributes
  | TEnum(enuminfo,attributes) -> Format.fprintf fmt "TEnum(%a,%a)"  pp_enuminfo enuminfo  pp_attributes attributes
  | TBuiltin_va_list(attributes) -> Format.fprintf fmt "TBuiltin_va_list(%a)"  pp_attributes attributes

and pp_ikind fmt = function
  | IBool -> Format.fprintf fmt "IBool"
  | IChar -> Format.fprintf fmt "IChar"
  | ISChar -> Format.fprintf fmt "ISChar"
  | IUChar -> Format.fprintf fmt "IUChar"
  | IInt -> Format.fprintf fmt "IInt"
  | IUInt -> Format.fprintf fmt "IUInt"
  | IShort -> Format.fprintf fmt "IShort"
  | IUShort -> Format.fprintf fmt "IUShort"
  | ILong -> Format.fprintf fmt "ILong"
  | IULong -> Format.fprintf fmt "IULong"
  | ILongLong -> Format.fprintf fmt "ILongLong"
  | IULongLong -> Format.fprintf fmt "IULongLong"

and pp_fkind fmt = function
  | FFloat -> Format.fprintf fmt "FFloat"
  | FDouble -> Format.fprintf fmt "FDouble"
  | FLongDouble -> Format.fprintf fmt "FLongDouble"

and pp_bitsSizeofTyp fmt = function
  | Not_Computed -> Format.fprintf fmt "Not_Computed"
  | Computed(int) -> Format.fprintf fmt "Computed(%a)"  pp_int int
  | Not_Computable(string,typ) -> Format.fprintf fmt "Not_Computable(%a,%a)"  pp_string string  pp_typ typ

and pp_bitsSizeofTypCache fmt bitsSizeofTypCache = Format.fprintf fmt "{scache=%a}"
    pp_bitsSizeofTyp bitsSizeofTypCache.scache

and pp_attribute fmt = function
  | Attr(string,attrparam_list) ->
    Format.fprintf fmt "Attr(%a,%a)" pp_string string (pp_list pp_attrparam) attrparam_list
  | AttrAnnot(string) -> Format.fprintf fmt "AttrAnnot(%a)"  pp_string string

and pp_attributes fmt attributes = (pp_list pp_attribute) fmt attributes

and pp_attrparam fmt = function
  | AInt(integer) -> Format.fprintf fmt "AInt(%a)"  pp_integer integer
  | AStr(string) -> Format.fprintf fmt "AStr(%a)"  pp_string string
  | ACons(string,attrparam_list) -> Format.fprintf fmt "ACons(%a,%a)"  pp_string string  (pp_list pp_attrparam) attrparam_list
  | ASizeOf(typ) -> Format.fprintf fmt "ASizeOf(%a)"  pp_typ typ
  | ASizeOfE(attrparam) -> Format.fprintf fmt "ASizeOfE(%a)"  pp_attrparam attrparam
  | AAlignOf(typ) -> Format.fprintf fmt "AAlignOf(%a)"  pp_typ typ
  | AAlignOfE(attrparam) -> Format.fprintf fmt "AAlignOfE(%a)"  pp_attrparam attrparam
  | AUnOp(unop,attrparam) -> Format.fprintf fmt "AUnOp(%a,%a)"  pp_unop unop  pp_attrparam attrparam
  | ABinOp(binop,attrparam1,attrparam2) -> Format.fprintf fmt "ABinOp(%a,%a,%a)"  pp_binop binop  pp_attrparam attrparam1  pp_attrparam attrparam2
  | ADot(attrparam,string) -> Format.fprintf fmt "ADot(%a,%a)"  pp_attrparam attrparam  pp_string string
  | AStar(attrparam) -> Format.fprintf fmt "AStar(%a)"  pp_attrparam attrparam
  | AAddrOf(attrparam) -> Format.fprintf fmt "AAddrOf(%a)"  pp_attrparam attrparam
  | AIndex(attrparam1,attrparam2) -> Format.fprintf fmt "AIndex(%a,%a)"  pp_attrparam attrparam1  pp_attrparam attrparam2
  | AQuestion(attrparam1,attrparam2,attrparam3) ->
    Format.fprintf fmt "AQuestion(%a,%a,%a)"  pp_attrparam attrparam1 pp_attrparam attrparam2  pp_attrparam attrparam3

and pp_compinfo fmt compinfo =
  if print_full_compinfo then
    Format.fprintf fmt
      "{\
       cstruct=%a;\
       corig_name=%a;\
       cname=%a;\
       ckey=%a;\
       cfields=%a;\
       cattr=%a;\
       cdefined=%a;\
       creferenced=%a;\
       }"
      pp_bool compinfo.cstruct
      pp_string compinfo.corig_name
      pp_string compinfo.cname
      pp_int compinfo.ckey
      (pp_list pp_fieldinfo) compinfo.cfields
      pp_attributes compinfo.cattr
      pp_bool compinfo.cdefined
      pp_bool compinfo.creferenced
  else
    Format.fprintf fmt
      "{\
       cstruct=%a;\
       corig_name=%a;\
       cname=%a;\
       ckey=%a;\
       cattr=%a;\
       }"
      pp_bool compinfo.cstruct
      pp_string compinfo.corig_name
      pp_string compinfo.cname
      pp_int compinfo.ckey
      pp_attributes compinfo.cattr


and pp_fieldinfo fmt fieldinfo =
  if print_full_fieldinfo then
    Format.fprintf fmt
      "{\
       fcomp=%a;\
       forig_name=%a;\
       fname=%a;\
       ftype=%a;\
       fbitfield=%a;\
       fattr=%a;\
       floc=%a;\
       faddrof=%a;\
       fsize_in_bits=%a;\
       foffset_in_bits=%a;\
       fpadding_in_bits=%a;\
       }"
      pp_compinfo fieldinfo.fcomp
      pp_string fieldinfo.forig_name
      pp_string fieldinfo.fname
      pp_typ fieldinfo.ftype
      (pp_option pp_int) fieldinfo.fbitfield
      pp_attributes fieldinfo.fattr
      pp_location fieldinfo.floc
      pp_bool fieldinfo.faddrof
      (pp_option pp_int) fieldinfo.fsize_in_bits
      (pp_option pp_int) fieldinfo.foffset_in_bits
      (pp_option pp_int) fieldinfo.fpadding_in_bits
  else
    Format.fprintf fmt
      "{\
       forig_name=%a;\
       fname=%a;\
       ftype=%a;\
       fbitfield=%a;\
       fattr=%a;\
       floc=%a;\
       faddrof=%a;\
       }"
      pp_string fieldinfo.forig_name
      pp_string fieldinfo.fname
      pp_typ fieldinfo.ftype
      (pp_option pp_int) fieldinfo.fbitfield
      pp_attributes fieldinfo.fattr
      pp_location fieldinfo.floc
      pp_bool fieldinfo.faddrof

and pp_enuminfo fmt enuminfo =
  if print_full_enuminfo then
    Format.fprintf fmt
      "{\
       eorig_name=%a;\
       ename=%a;\
       eitems=%a;\
       eattr=%a;\
       ereferenced=%a;\
       ekind=%a;\
       }"
      pp_string enuminfo.eorig_name
      pp_string enuminfo.ename
      (pp_list pp_enumitem) enuminfo.eitems
      pp_attributes enuminfo.eattr
      pp_bool enuminfo.ereferenced
      pp_ikind enuminfo.ekind
  else
    Format.fprintf fmt
      "{\
       eorig_name=%a;\
       ename=%a;\
       eattr=%a;\
       ekind=%a;\
       }"
      pp_string enuminfo.eorig_name
      pp_string enuminfo.ename
      pp_attributes enuminfo.eattr
      pp_ikind enuminfo.ekind

and pp_enumitem fmt enumitem =
  Format.fprintf fmt
    "{\
     eiorig_name=%a;\
     einame=%a;\
     eival=%a;\
     eihost=%a;\
     eiloc=%a;\
     }"
    pp_string enumitem.eiorig_name
    pp_string enumitem.einame
    pp_exp enumitem.eival
    pp_enuminfo enumitem.eihost
    pp_location enumitem.eiloc

and pp_typeinfo fmt typeinfo =
  if print_full_typeinfo then
    Format.fprintf fmt
      "{\
       torig_name=%a;\
       tname=%a;\
       ttype=%a;\
       treferenced=%a;\
       }"
      pp_string typeinfo.torig_name
      pp_string typeinfo.tname
      pp_typ typeinfo.ttype
      pp_bool typeinfo.treferenced
  else
    Format.fprintf fmt
      "{\
       torig_name=%a;\
       tname=%a;\
       }"
      pp_string typeinfo.torig_name
      pp_string typeinfo.tname

and pp_varinfo fmt varinfo =
  if print_full_varinfo then
    Format.fprintf fmt
      "{\
       vname=%a;\
       vorig_name=%a;\
       vtype=%a;\
       vattr=%a;\
       vstorage=%a;\
       vglob=%a;\
       vdefined=%a;\
       vformal=%a;\
       vinline=%a;\
       vdecl=%a;\
       vid=%a;\
       vaddrof=%a;\
       vreferenced=%a;\
       vtemp=%a;\
       vdescr=%a;\
       vdescrpure=%a;\
       vghost=%a;\
       vsource=%a;\
       vlogic_var_assoc=%a;\
       }"
      pp_string varinfo.vname
      pp_string varinfo.vorig_name
      pp_typ varinfo.vtype
      pp_attributes varinfo.vattr
      pp_storage varinfo.vstorage
      pp_bool varinfo.vglob
      pp_bool varinfo.vdefined
      pp_bool varinfo.vformal
      pp_bool varinfo.vinline
      pp_location varinfo.vdecl
      pp_int varinfo.vid
      pp_bool varinfo.vaddrof
      pp_bool varinfo.vreferenced
      pp_bool varinfo.vtemp
      (pp_option pp_string) varinfo.vdescr
      pp_bool varinfo.vdescrpure
      pp_bool varinfo.vghost
      pp_bool varinfo.vsource
      (pp_option pp_logic_var) varinfo.vlogic_var_assoc
  else
    Format.fprintf fmt "{vname=%a;vid=%a}"
      pp_string varinfo.vname
      pp_int varinfo.vid

and pp_storage fmt = function
  | NoStorage -> Format.fprintf fmt "NoStorage"
  | Static -> Format.fprintf fmt "Static"
  | Register -> Format.fprintf fmt "Register"
  | Extern -> Format.fprintf fmt "Extern"

and pp_exp fmt exp =
  Format.fprintf fmt "{eid=%a;enode=%a;eloc=%a}"
    pp_int exp.eid pp_exp_node exp.enode pp_location exp.eloc

and pp_exp_node fmt = function
  | Const(constant) -> Format.fprintf fmt "Const(%a)"       pp_constant constant
  | Lval(lval) -> Format.fprintf fmt "Lval(%a)"        pp_lval lval
  | SizeOf(typ) -> Format.fprintf fmt "SizeOf(%a)"      pp_typ typ
  | SizeOfE(exp) -> Format.fprintf fmt "SizeOfE(%a)"     pp_exp exp
  | SizeOfStr(string) -> Format.fprintf fmt "SizeOfStr(%a)"   pp_string string
  | AlignOf(typ) -> Format.fprintf fmt "AlignOf(%a)"     pp_typ typ
  | AlignOfE(exp) -> Format.fprintf fmt "AlignOfE(%a)"    pp_exp exp
  | UnOp(unop,exp,typ) -> Format.fprintf fmt "UnOp(%a,%a,%a)"        pp_unop unop  pp_exp exp  pp_typ typ
  | BinOp(binop,exp1,exp2,typ) ->
    Format.fprintf fmt "BinOp(%a,%a,%a,%a)"       pp_binop binop  pp_exp exp1  pp_exp exp2  pp_typ typ
  | CastE(typ,exp) -> Format.fprintf fmt "CastE(%a,%a)"       pp_typ typ  pp_exp exp
  | AddrOf(lval) -> Format.fprintf fmt "AddrOf(%a)"      pp_lval lval
  | StartOf(lval) -> Format.fprintf fmt "StartOf(%a)"     pp_lval lval
  | Info(exp,exp_info) -> Format.fprintf fmt "Info(%a,%a)"        pp_exp exp  pp_exp_info exp_info

and pp_exp_info fmt _exp_info = Format.fprintf fmt "pp_exp_info_TODO" (*{
  exp_type : logic_type;
  exp_name: string_list;
}*)

and pp_constant fmt = function
  | CInt64(integer,ikind,string_option) ->
    Format.fprintf fmt "CInt64(%a,%a,%a)" pp_integer integer  pp_ikind ikind  (pp_option pp_string) string_option
  | CStr(string) -> Format.fprintf fmt "CStr(%a)"  pp_string string
  | CWStr(int64_list) -> Format.fprintf fmt "CWStr(%a)"  (pp_list pp_int64) int64_list
  | CChr(char) -> Format.fprintf fmt "CChr(%a)"  pp_char char
  | CReal(float,fkind,string_option) ->
    Format.fprintf fmt "CReal(%a,%a,%a)"  pp_float float  pp_fkind fkind  (pp_option pp_string) string_option
  | CEnum(enumitem) -> Format.fprintf fmt "CEnum(%a)"  pp_enumitem enumitem

and pp_unop fmt = function
  | Neg -> Format.fprintf fmt "Neg"
  | BNot -> Format.fprintf fmt "BNot"
  | LNot -> Format.fprintf fmt "LNot"

and pp_binop fmt = function
  | PlusA -> Format.fprintf fmt "PlusA"
  | PlusPI -> Format.fprintf fmt "PlusPI"
  | IndexPI -> Format.fprintf fmt "IndexPI"
  | MinusA -> Format.fprintf fmt "MinusA"
  | MinusPI -> Format.fprintf fmt "MinusPI"
  | MinusPP -> Format.fprintf fmt "MinusPP"
  | Mult -> Format.fprintf fmt "Mult"
  | Div -> Format.fprintf fmt "Div"
  | Mod -> Format.fprintf fmt "Mod"
  | Shiftlt -> Format.fprintf fmt "Shiftlt"
  | Shiftrt -> Format.fprintf fmt "Shiftrt"
  | Lt -> Format.fprintf fmt "Lt"
  | Gt -> Format.fprintf fmt "Gt"
  | Le -> Format.fprintf fmt "Le"
  | Ge -> Format.fprintf fmt "Ge"
  | Eq -> Format.fprintf fmt "Eq"
  | Ne -> Format.fprintf fmt "Ne"
  | BAnd -> Format.fprintf fmt "BAnd"
  | BXor -> Format.fprintf fmt "BXor"
  | BOr -> Format.fprintf fmt "BOr"
  | LAnd -> Format.fprintf fmt "LAnd"
  | LOr -> Format.fprintf fmt "LOr"

and pp_lval fmt = pp_pair pp_lhost pp_offset fmt

and pp_lhost fmt = function
  | Var(varinfo) -> Format.fprintf fmt "Var(%a)"  pp_varinfo varinfo
  | Mem(exp) -> Format.fprintf fmt "Mem(%a)" pp_exp exp

and pp_offset fmt = function
  | NoOffset -> Format.fprintf fmt "NoOffset"
  | Field(fieldinfo,offset) -> Format.fprintf fmt "Field(%a,%a)"  pp_fieldinfo fieldinfo  pp_offset offset
  | Index(exp,offset) -> Format.fprintf fmt "Index(%a,%a)"  pp_exp exp  pp_offset offset

and pp_init fmt = function
  | SingleInit(exp) -> Format.fprintf fmt "SingleInit(%a)"  pp_exp exp
  | CompoundInit(typ,offset_init_pair_list) ->
    Format.fprintf fmt "CompoundInit(%a,%a)"  pp_typ typ
      (pp_list (pp_pair pp_offset pp_init)) offset_init_pair_list

and pp_initinfo fmt _initinfo = Format.fprintf fmt "pp_initinfo_TODO" (*{ mutable init : init_option }*)

and pp_fundec fmt _fundec = Format.fprintf fmt "pp_fundec_TODO" (*{
  mutable svar: varinfo;
  mutable sformals: varinfo_list;
  mutable slocals: varinfo_list;
  mutable smaxid: int;
  mutable sbody: block;
  mutable smaxstmtid: int_option;
  mutable sallstmts: stmt_list;
  mutable sspec: funspec;
}*)

and pp_block fmt _block = Format.fprintf fmt "pp_block_TODO" (*{
  mutable battrs: attributes;
  mutable blocals: varinfo_list;
  mutable bstmts: stmt_list;
}*)

and pp_stmt fmt _stmt = Format.fprintf fmt "pp_stmt_TODO" (*{
  mutable labels: label_list;
  mutable skind: stmtkind;
  mutable sid: int;
  mutable succs: stmt_list;
  mutable preds: stmt_list;
  mutable ghost : bool
}*)

and pp_label fmt = function
  | Label(string,location,bool) ->
    Format.fprintf fmt "Label(%a,%a,%a)"  pp_string string  pp_location location  pp_bool bool
  | Case(exp,location) -> Format.fprintf fmt "Case(%a,%a)"  pp_exp exp  pp_location location
  | Default(location) -> Format.fprintf fmt "Default(%a)"  pp_location location

and pp_stmtkind fmt = function
  | Instr(instr) -> Format.fprintf fmt "Instr(%a)"  pp_instr instr
  | Return(exp_option,location) ->
    Format.fprintf fmt "Return(%a,%a)"  (pp_option pp_exp) exp_option  pp_location location
  | Goto(stmt_ref,location) -> Format.fprintf fmt "Goto(%a,%a)"  (pp_ref pp_stmt) stmt_ref  pp_location location
  | Break(location) -> Format.fprintf fmt "Break(%a)"  pp_location location
  | Continue(location) -> Format.fprintf fmt "Continue(%a)"  pp_location location
  | If(exp,block1,block2,location) ->
    Format.fprintf fmt "If(%a,%a,%a,%a)"  pp_exp exp  pp_block block1  pp_block block2  pp_location location
  | Switch(exp,block,stmt_list,location) ->
    Format.fprintf fmt "Switch(%a,%a,%a,%a)"  pp_exp exp  pp_block block
      (pp_list pp_stmt) stmt_list  pp_location location
  | Loop(code_annotation_list,block,location,stmt_option1,stmt_option2) ->
    Format.fprintf fmt "Loop(%a,%a,%a,%a,%a)"
      (pp_list pp_code_annotation) code_annotation_list  pp_block block  pp_location location
      (pp_option pp_stmt) stmt_option1 (pp_option pp_stmt) stmt_option2
  | Block(block) -> Format.fprintf fmt "Block(%a)"  pp_block block
  | UnspecifiedSequence(stmt_lval_list_lval_list_lval_list_stmt_ref_list_tuple_list) ->
    Format.fprintf fmt "UnspecifiedSequence(%a)"
      (pp_list (pp_tuple5 pp_stmt (pp_list pp_lval) (pp_list pp_lval) (pp_list pp_lval) (pp_list (pp_ref pp_stmt))))
      stmt_lval_list_lval_list_lval_list_stmt_ref_list_tuple_list
  | Throw(exp_typ_pair_option,location) ->
    Format.fprintf fmt "Throw(%a,%a)" (pp_option (pp_pair pp_exp pp_typ)) exp_typ_pair_option pp_location location
  | TryCatch(block,catch_binder_block_pair_list,location) ->
    Format.fprintf fmt "TryCatch(%a,%a,%a)"
      pp_block block (pp_list (pp_pair pp_catch_binder pp_block)) catch_binder_block_pair_list pp_location location
  | TryFinally(block1,block2,location) ->
    Format.fprintf fmt "TryFinally(%a,%a,%a)"  pp_block block1  pp_block block2  pp_location location
  | TryExcept(block1,instr_list_exp_pair,block2,location) ->
    Format.fprintf fmt "TryExcept(%a,%a,%a,%a)"
      pp_block block1
      (pp_pair (pp_list pp_instr) pp_exp) instr_list_exp_pair
      pp_block block2  pp_location location

and pp_catch_binder fmt = function
  | Catch_exn(varinfo,varinfo_block_pair_list) ->
    Format.fprintf fmt "Catch_exn(%a,%a)"  pp_varinfo varinfo
      (pp_list (pp_pair pp_varinfo pp_block)) varinfo_block_pair_list
  | Catch_all -> Format.fprintf fmt "Catch_all"

and pp_constructor_kind fmt = function
  | Constructor -> Format.pp_print_string fmt "Constructor"
  | Plain_func -> Format.pp_print_string fmt "Plain_func"

and pp_local_init fmt = function
  | AssignInit i -> Format.fprintf fmt "AssignInit(%a)" pp_init i
  | ConsInit(f,args,kind) ->
    Format.fprintf fmt "ConsInit(%a,%a,%a)" pp_varinfo f (pp_list pp_exp) args
      pp_constructor_kind kind

and pp_instr fmt = function
  | Set(lval,exp,location) -> Format.fprintf fmt "Set(%a,%a,%a)"  pp_lval lval  pp_exp exp  pp_location location
  | Call(lval_option,exp,exp_list,location) ->
    Format.fprintf fmt "Call(%a,%a,%a,%a)"  (pp_option pp_lval) lval_option  pp_exp exp
      (pp_list pp_exp) exp_list  pp_location location
  | Asm(attributes,string_list,extended_asm_option,location) ->
    Format.fprintf fmt "Asm(%a,%a,%a,%a)"  pp_attributes attributes  (pp_list pp_string) string_list
      (pp_option pp_extended_asm) extended_asm_option   pp_location location
  | Skip(location) -> Format.fprintf fmt "Skip(%a)"  pp_location location
  | Code_annot(code_annotation,location) ->
    Format.fprintf fmt "Code_annot(%a,%a)"  pp_code_annotation code_annotation  pp_location location
  | Local_init(vi,i,location) ->
    Format.fprintf fmt "Local_init(%a,%a,%a)" pp_varinfo vi pp_local_init i pp_location location

and pp_extended_asm fmt _extended_asm = Format.fprintf fmt "pp_extended_asm_TODO" (*{
  {
    asm_outputs: (string_option * string * lval)_list;
    asm_inputs: (string_option * string * exp)_list;
    asm_clobbers: string_list;
    asm_gotos: (stmt ref)_list;
}*)

and pp_lexing_position fmt lexing_position =
  Format.fprintf fmt "{pos_fname=%s;pos_lnum=%d;pos_bol=%d;pos_cnum=%d}"
    lexing_position.Lexing.pos_fname lexing_position.Lexing.pos_lnum
    lexing_position.Lexing.pos_bol lexing_position.Lexing.pos_cnum

and pp_location fmt (pos_start,pos_end) =
  let p = if print_locations then Format.fprintf else Format.ifprintf in
  p fmt "(%a,%a)" pp_lexing_position pos_start pp_lexing_position pos_end

and pp_if_loc_known prefix suffix fmt loc =
  if print_locations && loc <> Cil_datatype.Location.unknown
  then Format.fprintf fmt "%s%a%s" prefix pp_location loc suffix
  else ()

and pp_logic_constant fmt = function
  | Integer(integer,string_option) ->
    Format.fprintf fmt "Integer(%a,%a)"  pp_integer integer  (pp_option pp_string) string_option
  | LStr(string) -> Format.fprintf fmt "LStr(%a)"  pp_string string
  | LWStr(int64_list) -> Format.fprintf fmt "LWStr(%a)"  (pp_list pp_int64) int64_list
  | LChr(char) -> Format.fprintf fmt "LChr(%a)"  pp_char char
  | LReal(logic_real) -> Format.fprintf fmt "LReal(%a)"  pp_logic_real logic_real
  | LEnum(enumitem) -> Format.fprintf fmt "LEnum(%a)"  pp_enumitem enumitem

and pp_logic_real fmt _logic_real = Format.fprintf fmt "pp_logic_real_TODO" (*{
  r_literal : string ;
  r_nearest : float ;
  r_upper : float ;
  r_lower : float ;
}*)

and pp_logic_type fmt = function
  | Ctype(typ) -> Format.fprintf fmt "Ctype(%a)"  pp_typ typ
  | Ltype(logic_type_info,logic_type_list) ->
    Format.fprintf fmt "Ltype(%a,%a)"  pp_logic_type_info logic_type_info  (pp_list pp_logic_type) logic_type_list
  | Lvar(string) -> Format.fprintf fmt "Lvar(%a)"  pp_string string
  | Linteger -> Format.fprintf fmt "Linteger"
  | Lreal -> Format.fprintf fmt "Lreal"
  | Larrow(logic_type_list,logic_type) ->
    Format.fprintf fmt "Larrow(%a,%a)"  (pp_list pp_logic_type) logic_type_list  pp_logic_type logic_type

and pp_identified_term fmt identified_term =
  Format.fprintf fmt "{it_id=%a;it_content=%a}"
    pp_int identified_term.it_id pp_term identified_term.it_content

and pp_logic_label fmt = function
  | StmtLabel(stmt_ref) -> Format.fprintf fmt "StmtLabel(%a)"  (pp_ref pp_stmt) stmt_ref
  | FormalLabel s -> Format.fprintf fmt "FormalLabel %s" s
  | BuiltinLabel l -> Format.fprintf fmt "BuiltinLabel %a" pp_logic_builtin_label l

and pp_logic_builtin_label fmt l =
  let s = match l with
    | Here -> "Here"
    | Old -> "Old"
    | Pre -> "Pre"
    | Post -> "Post"
    | LoopEntry -> "LoopEntry"
    | LoopCurrent -> "LoopCurrent"
    | Init -> "Init"
  in
  pp_string fmt s

and pp_term fmt term =
  Format.fprintf fmt "{term_node=%a;%aterm_type=%a%a}"
    pp_term_node term.term_node
    (pp_if_loc_known "term_loc=" ";") term.term_loc
    pp_logic_type term.term_type
    (pp_if_list_not_empty ";term_name=" "" (pp_list pp_string)) term.term_name

and pp_term_node fmt = function
  | TConst(logic_constant) -> Format.fprintf fmt "TConst(%a)"  pp_logic_constant logic_constant
  | TLval(term_lval) -> Format.fprintf fmt "TLval(%a)"  pp_term_lval term_lval
  | TSizeOf(typ) -> Format.fprintf fmt "TSizeOf(%a)"  pp_typ typ
  | TSizeOfE(term) -> Format.fprintf fmt "TSizeOfE(%a)"  pp_term term
  | TSizeOfStr(string) -> Format.fprintf fmt "TSizeOfStr(%a)"  pp_string string
  | TAlignOf(typ) -> Format.fprintf fmt "TAlignOf(%a)"  pp_typ typ
  | TAlignOfE(term) -> Format.fprintf fmt "TAlignOfE(%a)"  pp_term term
  | TUnOp(unop,term) -> Format.fprintf fmt "TUnOp(%a,%a)"  pp_unop unop  pp_term term
  | TBinOp(binop,term1,term2) ->
    Format.fprintf fmt "TBinOp(%a,%a,%a)"  pp_binop binop  pp_term term1  pp_term term2
  | TCastE(typ,term) -> Format.fprintf fmt "TCastE(%a,%a)"  pp_typ typ  pp_term term
  | TAddrOf(term_lval) -> Format.fprintf fmt "TAddrOf(%a)"  pp_term_lval  term_lval
  | TStartOf(term_lval) -> Format.fprintf fmt "TStartOf(%a)"  pp_term_lval term_lval
  | Tapp(logic_info,logic_label_list,term_list) ->
    Format.fprintf fmt "Tapp(%a,%a,%a)"
      pp_logic_info logic_info
      (pp_list pp_logic_label) logic_label_list
      (pp_list pp_term) term_list
  | Tlambda(quantifiers,term) -> Format.fprintf fmt "Tlambda(%a,%a)"  pp_quantifiers quantifiers  pp_term term
  | TDataCons(logic_ctor_info,term_list) ->
    Format.fprintf fmt "TDataCons(%a,%a)"  pp_logic_ctor_info logic_ctor_info  (pp_list pp_term) term_list
  | Tif(term1,term2,term3) -> Format.fprintf fmt "Tif(%a,%a,%a)"  pp_term term1  pp_term term2  pp_term term3
  | Tat(term,logic_label) ->
    Format.fprintf fmt "Tat(%a,%a)"  pp_term term  pp_logic_label logic_label
  | Tbase_addr(logic_label,term) ->
    Format.fprintf fmt "Tbase_addr(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Toffset(logic_label,term) ->
    Format.fprintf fmt "Toffset(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Tblock_length(logic_label,term) ->
    Format.fprintf fmt "Tblock_length(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Tnull -> Format.fprintf fmt "Tnull"
  | TLogic_coerce(logic_type,term) ->
    Format.fprintf fmt "TLogic_coerce(%a,%a)"  pp_logic_type logic_type  pp_term term
  | TCoerce(term,typ) ->
    Format.fprintf fmt "TCoerce(%a,%a)"  pp_term term  pp_typ typ
  | TCoerceE(term1,term2) ->
    Format.fprintf fmt "TCoerceE(%a,%a)"  pp_term term1  pp_term term2
  | TUpdate(term1,term_offset,term2) ->
    Format.fprintf fmt "TUpdate(%a,%a,%a)"  pp_term term1  pp_term_offset term_offset  pp_term term2
  | Ttypeof(term) -> Format.fprintf fmt "Ttypeof(%a)"  pp_term term
  | Ttype(typ) -> Format.fprintf fmt "Ttype(%a)"  pp_typ typ
  | Tempty_set -> Format.fprintf fmt "Tempty_set"
  | Tunion(term_list) -> Format.fprintf fmt "Tunion(%a)"  (pp_list pp_term) term_list
  | Tinter(term_list) -> Format.fprintf fmt "Tinter(%a)"  (pp_list pp_term) term_list
  | Tcomprehension(term,quantifiers,predicate_option) ->
    Format.fprintf fmt "Tcomprehension(%a,%a,%a)"  pp_term term  pp_quantifiers quantifiers
      (pp_option pp_predicate) predicate_option
  | Trange(term_option1,term_option2) ->
    Format.fprintf fmt "Trange(%a,%a)"  (pp_option pp_term) term_option1  (pp_option pp_term) term_option2
  | Tlet(logic_info,term) -> Format.fprintf fmt "Tlet(%a,%a)"  pp_logic_info logic_info  pp_term term

and pp_term_lval fmt = pp_pair pp_term_lhost pp_term_offset fmt

and pp_term_lhost fmt = function
  | TVar(logic_var) -> Format.fprintf fmt "TVar(%a)"  pp_logic_var logic_var
  | TResult(typ) -> Format.fprintf fmt "TResult(%a)"  pp_typ typ
  | TMem(term) -> Format.fprintf fmt "TMem(%a)"  pp_term term

and pp_model_info fmt _model_info = Format.fprintf fmt "pp_model_info_TODO" (*{
  mi_name: string;
  mi_field_type: logic_type;
  mi_base_type: typ;
  mi_decl: location;
  mutable mi_attr: attributes;

}*)

and pp_term_offset fmt = function
  | TNoOffset -> Format.fprintf fmt "TNoOffset"
  | TField(fieldinfo,term_offset) ->
    Format.fprintf fmt "TField(%a,%a)"  pp_fieldinfo fieldinfo  pp_term_offset term_offset
  | TModel(model_info,term_offset) ->
    Format.fprintf fmt "TModel(%a,%a)"  pp_model_info model_info  pp_term_offset term_offset
  | TIndex(term,term_offset) ->
    Format.fprintf fmt "TIndex(%a,%a)"  pp_term term  pp_term_offset term_offset

and pp_logic_info fmt logic_info =
  Format.fprintf fmt "{l_var_info=%a;%al_tparams=%a;logic_type=%a;TODO}"
    pp_logic_var logic_info.l_var_info
    (pp_if_list_not_empty "l_labels=" ";" (pp_list pp_logic_label)) logic_info.l_labels
    (pp_list pp_string) logic_info.l_tparams
    (pp_option pp_logic_type) logic_info.l_type
(*{
  mutable l_var_info : logic_var;
  mutable l_labels : logic_label_list;
  mutable l_tparams : string_list;
  mutable l_type : logic_type_option;
  mutable l_profile : logic_var_list;
  mutable l_body : logic_body;
}*)

and pp_builtin_logic_info fmt _builtin_logic_info = Format.fprintf fmt "pp_builtin_logic_info_TODO" (*{
  mutable bl_name: string;
  mutable bl_labels: logic_label_list;
  mutable bl_params: string_list;
  mutable bl_type: logic_type_option;
  mutable bl_profile: (string * logic_type)_list;
}*)

and pp_logic_body fmt = function
  | LBnone -> Format.fprintf fmt "LBnone"
  | LBreads(identified_term_list) -> Format.fprintf fmt "LBreads(%a)"  (pp_list pp_identified_term) identified_term_list
  | LBterm(term) -> Format.fprintf fmt "LBterm(%a)"  pp_term term
  | LBpred(predicate) -> Format.fprintf fmt "LBpred(%a)"  pp_predicate predicate
  | LBinductive(string_logic_label_list_string_list_predicate_list) ->
    Format.fprintf fmt "LBinductive(%a)"
      (pp_list (pp_tuple4 pp_string (pp_list pp_logic_label) (pp_list pp_string) pp_predicate))
      string_logic_label_list_string_list_predicate_list

and pp_logic_type_info fmt _logic_type_info = Format.fprintf fmt "pp_logic_type_info_TODO" (*{
  lt_name: string;
  lt_params : string_list;
  mutable lt_def: logic_type_def_option;
  mutable lt_attr: attributes;
}*)

and pp_logic_type_def fmt = function
  | LTsum(logic_ctor_info_list) -> Format.fprintf fmt "LTsum(%a)"  (pp_list pp_logic_ctor_info) logic_ctor_info_list
  | LTsyn(logic_type) -> Format.fprintf fmt "LTsyn(%a)"  pp_logic_type logic_type

and pp_logic_var_kind fmt = function
  | LVGlobal -> Format.fprintf fmt "LVGlobal"
  | LVC -> Format.fprintf fmt "LVC"
  | LVFormal -> Format.fprintf fmt "LVFormal"

  | LVQuant -> Format.fprintf fmt "LVQuant"
  | LVLocal -> Format.fprintf fmt "LVLocal"

and pp_logic_var fmt logic_var =
  Format.fprintf fmt "{lv_name=%a;lv_id=%a;TODO}"
    pp_string logic_var.lv_name pp_int logic_var.lv_id
(*{
  mutable lv_name : string;
  mutable lv_id : int;
  mutable lv_type : logic_type;
  mutable lv_kind: logic_var_kind;
  mutable lv_origin : varinfo_option;
  mutable lv_attr: attributes
}*)

and pp_logic_ctor_info fmt _logic_ctor_info = Format.fprintf fmt "pp_logic_ctor_info_TODO" (*{
   ctor_name: string;
   ctor_type: logic_type_info;
   ctor_params: logic_type_list
}*)

and pp_quantifiers fmt = pp_list pp_logic_var fmt

and pp_relation fmt = function
  | Rlt -> Format.fprintf fmt "Rlt"
  | Rgt -> Format.fprintf fmt "Rgt"
  | Rle -> Format.fprintf fmt "Rle"
  | Rge -> Format.fprintf fmt "Rge"
  | Req -> Format.fprintf fmt "Req"
  | Rneq -> Format.fprintf fmt "Rneq"

and pp_predicate_node fmt = function
  | Pfalse -> Format.fprintf fmt "Pfalse"
  | Ptrue -> Format.fprintf fmt "Ptrue"
  | Papp(logic_info,logic_label_list,term_list) ->
    Format.fprintf fmt "Papp(%a,%a,%a)"
      pp_logic_info logic_info
      (pp_list pp_logic_label) logic_label_list
      (pp_list pp_term) term_list
  | Pseparated(term_list) ->
    Format.fprintf fmt "Pseparated(%a)"  (pp_list pp_term) term_list
  | Prel(relation,term1,term2) ->
    Format.fprintf fmt "Prel(%a,%a,%a)"  pp_relation relation  pp_term term1  pp_term term2
  | Pand(predicate1,predicate2) ->
    Format.fprintf fmt "Pand(%a,%a)"  pp_predicate predicate1  pp_predicate predicate2
  | Por(predicate1,predicate2) ->
    Format.fprintf fmt "Por(%a,%a)"  pp_predicate predicate1  pp_predicate predicate2
  | Pxor(predicate1,predicate2) ->
    Format.fprintf fmt "Pxor(%a,%a)"  pp_predicate predicate1  pp_predicate predicate2
  | Pimplies(predicate1,predicate2) ->
    Format.fprintf fmt "Pimplies(%a,%a)"  pp_predicate predicate1  pp_predicate predicate2
  | Piff(predicate1,predicate2) ->
    Format.fprintf fmt "Piff(%a,%a)"  pp_predicate predicate1  pp_predicate predicate2
  | Pnot(predicate) ->
    Format.fprintf fmt "Pnot(%a)"  pp_predicate predicate
  | Pif(term,predicate1,predicate2) ->
    Format.fprintf fmt "Pif(%a,%a,%a)"  pp_term term  pp_predicate predicate1  pp_predicate predicate2
  | Plet(logic_info,predicate) ->
    Format.fprintf fmt "Plet(%a,%a)"  pp_logic_info logic_info  pp_predicate predicate
  | Pforall(quantifiers,predicate) ->
    Format.fprintf fmt "Pforall(%a,%a)"  pp_quantifiers quantifiers  pp_predicate predicate
  | Pexists(quantifiers,predicate) ->
    Format.fprintf fmt "Pexists(%a,%a)"  pp_quantifiers quantifiers  pp_predicate predicate
  | Pat(predicate,logic_label) ->
    Format.fprintf fmt "Pat(%a,%a)"  pp_predicate predicate  pp_logic_label logic_label
  | Pvalid_read(logic_label,term) ->
    Format.fprintf fmt "Pvalid_read(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pvalid(logic_label,term) ->
    Format.fprintf fmt "Pvalid(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pvalid_function(term) ->
    Format.fprintf fmt "Pvalid_function(%a)"  pp_term term
  | Pinitialized(logic_label,term) ->
    Format.fprintf fmt "Pinitialized(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pdangling(logic_label,term) ->
    Format.fprintf fmt "Pdangling(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pallocable(logic_label,term) ->
    Format.fprintf fmt "Pallocable(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pfreeable(logic_label,term) ->
    Format.fprintf fmt "Pfreeable(%a,%a)"  pp_logic_label logic_label  pp_term term
  | Pfresh(logic_label1,logic_label2,term1,term2) ->
    Format.fprintf fmt "Pfresh(%a,%a,%a,%a)"  pp_logic_label logic_label1  pp_logic_label logic_label2
      pp_term term1  pp_term term2
  | Psubtype(term1,term2) ->
 Format.fprintf fmt "Psubtype(%a,%a)"  pp_term term1  pp_term term2

and pp_identified_predicate fmt identified_predicate =
  Format.fprintf fmt "{ip_id=%d;ip_content=%a}"
    identified_predicate.ip_id pp_predicate identified_predicate.ip_content

and pp_predicate fmt predicate = Format.fprintf fmt "{%a%apred_content=%a}"
    (pp_if_list_not_empty "pred_name=" ";" (pp_list pp_string)) predicate.pred_name
    (pp_if_loc_known "pred_loc=" ";") predicate.pred_loc
    pp_predicate_node predicate.pred_content

and pp_spec fmt _spec = Format.fprintf fmt "pp_spec_TODO" (*{
  mutable spec_behavior : behavior_list;
  mutable spec_variant : term variant_option;
  mutable spec_terminates: identified_predicate_option;
  mutable spec_complete_behaviors: string_list_list;
  mutable spec_disjoint_behaviors: string_list_list;
}*)

and pp_acsl_extension fmt = pp_pair pp_string pp_acsl_extension_kind fmt

and pp_acsl_extension_kind fmt = function
  | Ext_id(int) -> Format.fprintf fmt "Ext_id(%a)"  pp_int int
  | Ext_terms(term_list) -> Format.fprintf fmt "Ext_terms(%a)"  (pp_list pp_term) term_list
  | Ext_preds(predicate_list) -> Format.fprintf fmt "Ext_preds(%a)"  (pp_list pp_predicate) predicate_list

and pp_behavior fmt _behavior = Format.fprintf fmt "pp_behavior_TODO" (*{
  mutable b_name : string;
  mutable b_requires : identified_predicate_list;
  mutable b_assumes : identified_predicate_list;
  mutable b_post_cond : (termination_kind * identified_predicate)_list;
  mutable b_assigns : assigns;
  mutable b_allocation : allocation;
  mutable b_extended : acsl_extension_list
}*)

and pp_termination_kind fmt = function
  | Normal -> Format.fprintf fmt "Normal"
  | Exits -> Format.fprintf fmt "Exits"
  | Breaks -> Format.fprintf fmt "Breaks"
  | Continues -> Format.fprintf fmt "Continues"
  | Returns -> Format.fprintf fmt "Returns"

and pp_loop_pragma pp_term fmt = function
  | Unroll_specs(term_list) -> Format.fprintf fmt "Unroll_specs(%a)" (pp_list pp_term) term_list
  | Widen_hints(term_list) -> Format.fprintf fmt "Widen_hints(%a)" (pp_list pp_term) term_list
  | Widen_variables(term_list) -> Format.fprintf fmt "Widen_variables(%a)" (pp_list pp_term) term_list

and pp_slice_pragma pp_term fmt = function
  | SPexpr(term) -> Format.fprintf fmt "SPexpr(%a)" pp_term term
  | SPctrl -> Format.fprintf fmt "SPctrl"
  | SPstmt -> Format.fprintf fmt "SPstmt"

and pp_impact_pragma pp_term fmt = function
  | IPexpr(term) -> Format.fprintf fmt "IPexpr(%a)" pp_term term
  | IPstmt -> Format.fprintf fmt "IPstmt"

and pp_pragma pp_term fmt = function
  | Loop_pragma(term) -> Format.fprintf fmt "Loop_pragma(%a)" (pp_loop_pragma pp_term) term
  | Slice_pragma(term) -> Format.fprintf fmt "Slice_pragma(%a)" (pp_slice_pragma pp_term) term
  | Impact_pragma(term) -> Format.fprintf fmt "Impact_pragma(%a)" (pp_impact_pragma pp_term) term

and pp_code_annotation_node fmt = function
  | AAssert(string_list,predicate) ->
    Format.fprintf fmt "AAssert(%a,%a)"  (pp_list pp_string) string_list  pp_predicate predicate
  | AStmtSpec(string_list,spec) ->
    Format.fprintf fmt "AStmtSpec(%a,%a)"  (pp_list pp_string) string_list  pp_spec spec
  | AInvariant(string_list,bool,predicate) ->
    Format.fprintf fmt "AInvariant(%a,%a,%a)"  (pp_list pp_string) string_list  pp_bool bool  pp_predicate predicate
  | AVariant(term_variant) ->
    Format.fprintf fmt "AVariant(%a)" (pp_variant pp_term) term_variant
  | AAssigns(string_list,assigns) ->
    Format.fprintf fmt "AAssigns(%a,%a)"  (pp_list pp_string) string_list
      (pp_assigns pp_from) assigns
  | AAllocation(string_list,allocation) ->
    Format.fprintf fmt "AAllocation(%a,%a)"  (pp_list pp_string) string_list
      pp_allocation allocation
  | APragma(pragma) ->
    Format.fprintf fmt "APragma(%a)" (pp_pragma pp_term) pragma
  | AExtended(string_list,acsl_extension) ->
    Format.fprintf fmt "AExtended(%a,%a)"  (pp_list pp_string) string_list  pp_acsl_extension acsl_extension

and pp_funspec fmt _funspec = Format.fprintf fmt "pp_funspec_TODO"

and pp_code_annotation fmt _code_annotation = Format.fprintf fmt "pp_code_annotation_TODO" (*{
  annot_id: int;
  annot_content : code_annotation_node;
}*)

and pp_funbehavior fmt = pp_behavior fmt

and pp_global_annotation fmt = function
  | Dfun_or_pred(logic_info,location) ->
    Format.fprintf fmt "Dfun_or_pred(%a,%a)"  pp_logic_info logic_info  pp_location location
  | Dvolatile(identified_term_list,varinfo_option1,varinfo_option2,attributes,location) ->
    Format.fprintf fmt "Dvolatile(%a,%a,%a,%a,%a)"  (pp_list pp_identified_term) identified_term_list
      (pp_option pp_varinfo) varinfo_option1 (pp_option pp_varinfo) varinfo_option2
      pp_attributes attributes  pp_location location
  | Daxiomatic(string,global_annotation_list,attributes,location) ->
    Format.fprintf fmt "Daxiomatic(%a,%a,%a,%a)"  pp_string string
      (pp_list pp_global_annotation) global_annotation_list
      pp_attributes attributes  pp_location location
  | Dtype(logic_type_info,location) ->
    Format.fprintf fmt "Dtype(%a,%a)"  pp_logic_type_info logic_type_info  pp_location location
  | Dlemma(string,bool,logic_label_list,string_list,predicate,attributes,location) ->
    Format.fprintf fmt "Dlemma(%a,%a,%a,%a,%a,%a,%a)"  pp_string string  pp_bool bool
      (pp_list pp_logic_label) logic_label_list (pp_list pp_string) string_list
      pp_predicate predicate  pp_attributes attributes  pp_location location
  | Dinvariant(logic_info,location) ->
    Format.fprintf fmt "Dinvariant(%a,%a)"  pp_logic_info logic_info  pp_location location
  | Dtype_annot(logic_info,location) ->
    Format.fprintf fmt "Dtype_annot(%a,%a)"  pp_logic_info logic_info  pp_location location
  | Dmodel_annot(model_info,location) ->
    Format.fprintf fmt "Dmodel_annot(%a,%a)"  pp_model_info model_info  pp_location location
  | Dcustom_annot(custom_tree,string,attributes,location) ->
    Format.fprintf fmt "Dcustom_annot(%a,%a,%a,%a)"  pp_custom_tree custom_tree  pp_string string
      pp_attributes attributes  pp_location location

and pp_custom_tree fmt _custom_tree = Format.fprintf fmt "CustomDummy"

let pp_kinstr fmt = function
  | Kstmt(stmt) -> Format.fprintf fmt "Kstmt(%a)"  pp_stmt stmt
  | Kglobal -> Format.fprintf fmt "Kglobal"

let pp_cil_function fmt = function
  | Definition(fundec,location) ->
    Format.fprintf fmt "Definition(%a,%a)"  pp_fundec fundec  pp_location location
  | Declaration(funspec,varinfo,varinfo_list_option,location) ->
    Format.fprintf fmt "Declaration(%a,%a,%a,%a)"  pp_funspec funspec  pp_varinfo varinfo
      (pp_option (pp_list pp_varinfo)) varinfo_list_option  pp_location location

let pp_kernel_function fmt _kernel_function = Format.fprintf fmt "pp_kernel_function_TODO" (*{
  mutable fundec : cil_function;
  mutable spec : funspec;
}*)

let pp_localisation fmt = function
  | VGlobal -> Format.fprintf fmt "VGlobal"
  | VLocal(kernel_function) -> Format.fprintf fmt "VLocal(%a)"  pp_kernel_function kernel_function
  | VFormal(kernel_function) -> Format.fprintf fmt "VFormal(%a)"  pp_kernel_function kernel_function

let pp_mach fmt _mach = Format.fprintf fmt "pp_mach_TODO" (*{
  sizeof_short: int;
  sizeof_int: int;
  sizeof_long: int ;
  sizeof_longlong: int;
  sizeof_ptr: int;
  sizeof_float: int;
  sizeof_double: int;
  sizeof_longdouble: int;
  sizeof_void: int;
  sizeof_fun: int;
  size_t: string;
  wchar_t: string;
  ptrdiff_t: string;
  alignof_short: int;
  alignof_int: int;
  alignof_long: int;
  alignof_longlong: int;
  alignof_ptr: int;
  alignof_float: int;
  alignof_double: int;
  alignof_longdouble: int;
  alignof_str: int;
  alignof_fun: int;
  char_is_unsigned: bool;
  underscore_name: bool;
  const_string_literals: bool;
  little_endian: bool;
  alignof_aligned: int;
  has__builtin_va_list: bool;
  __thread_is_keyword: bool;
  compiler: string;
  cpp_arch_flags: string_list;
  version: string;
}*)
