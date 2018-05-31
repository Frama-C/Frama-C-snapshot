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

(* Type check and elaborate ABS to CIL *)

(* The references to ISO means ANSI/ISO 9899-1999 *)
module A = Cabs
module C = Cabshelper
module V = Cabsvisit
module H = Hashtbl
module IH = Datatype.Int.Hashtbl

open Pretty_utils
open Cabs
open Cabshelper
open Cil
let valid_sid = false
(* All statements generated here must have an invalid sid. Use this variable
   for the valid_sid label of Cil.mkStmt*. *)
open Cil_types
open Cil_datatype
open Lexing

let frama_c_keep_block = "FRAMA_C_KEEP_BLOCK"
let () = Cil_printer.register_shallow_attribute frama_c_keep_block

let fc_stdlib = "fc_stdlib"
let fc_stdlib_generated = "fc_stdlib_generated"
let () = Cil_printer.register_shallow_attribute fc_stdlib
let () = Cil_printer.register_shallow_attribute fc_stdlib_generated

let fc_local_static = "fc_local_static"
let () = Cil_printer.register_shallow_attribute fc_local_static

let frama_c_destructor = "__fc_destructor"
let () = Cil_printer.register_shallow_attribute frama_c_destructor

(** A hook into the code that creates temporary local vars.  By default this
    is the identity function, but you can overwrite it if you need to change the
    types of cabs2cil-introduced temp variables. *)
let typeForInsertedVar: (Cil_types.typ -> Cil_types.typ) ref = ref (fun t -> t)

(** Like [typeForInsertedVar], but for casts.
  * Casts in the source code are exempt from this hook. *)
let typeForInsertedCast:
  (Cil_types.exp -> Cil_types.typ -> Cil_types.typ -> Cil_types.typ) ref =
  ref (fun _ _ t -> t)


let cabs_exp loc node = { expr_loc = loc; expr_node = node }

let bigger_length_args l1 l2 =
  match l1, l2 with
  | None, _ | _, None -> false
  | Some l1, Some l2 -> List.length l1 > List.length l2

let abort_context msg =
  let pos = fst (Cil.CurrentLoc.get ()) in
  let append fmt =
    Format.pp_print_newline fmt ();
    Errorloc.pp_context_from_file fmt pos
  in
  Kernel.abort ~current:true ~append msg

module IgnorePureExpHook =
  Hook.Build (struct type t = string * Cil_types.exp end)

let register_ignore_pure_exp_hook f =
  IgnorePureExpHook.extend (fun (x,z) -> f x z)

module ImplicitPrototypeHook =
  Hook.Build (struct type t = varinfo end)

let register_implicit_prototype_hook f = ImplicitPrototypeHook.extend f

module IncompatibleDeclHook =
  Hook.Build(struct type t = varinfo * varinfo * string end)

let register_incompatible_decl_hook f =
  IncompatibleDeclHook.extend (fun (x,y,z) -> f x y z)

module DifferentDeclHook =
  Hook.Build(struct type t = varinfo * varinfo end)

let register_different_decl_hook f =
  DifferentDeclHook.extend (fun (x,y) -> f x y)

module NewGlobalHook = Hook.Build(struct type t = varinfo * bool end)
let register_new_global_hook f = NewGlobalHook.extend (fun (x, y) -> f x y)

module LocalFuncHook = Hook.Build(struct type t = varinfo end)

let register_local_func_hook = LocalFuncHook.extend

module IgnoreSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * Cil_types.exp end)

let register_ignore_side_effect_hook f =
  IgnoreSideEffectHook.extend (fun (y,z) -> f y z)

module ConditionalSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * Cabs.expression end)

module ForLoopHook =
  Hook.Build(struct 
    type t =
      Cabs.for_clause * Cabs.expression * Cabs.expression * Cabs.statement
  end)

let register_for_loop_all_hook f =
  ForLoopHook.extend (fun (x,y,z,t) -> f x y z t)

let register_for_loop_init_hook f =
  ForLoopHook.extend (fun (x,_,_,_) -> f x)

let register_for_loop_test_hook f =
  ForLoopHook.extend (fun (_,x,_,_) -> f x)

let register_for_loop_incr_hook f =
  ForLoopHook.extend (fun (_,_,x,_) -> f x)

let register_for_loop_body_hook f =
  ForLoopHook.extend (fun (_,_,_,x) -> f x)

let register_conditional_side_effect_hook f =
  ConditionalSideEffectHook.extend (fun (y,z) -> f y z)

(* These symbols are supposed to be macros. It is not possible to
   take their address or to redeclare them outside of the proper header
   in stdlib. See CERT MSC38-C rule.
*)
let no_suppress_function_macro =
  [ "assert"; "setjmp"; "va_arg"; "va_copy"; "va_end"; "va_start" ]

let no_redefine_macro =
  "errno" :: "math_errhandling" :: no_suppress_function_macro

let is_stdlib_function_macro n = List.mem n no_suppress_function_macro

let is_stdlib_macro n = List.mem n no_redefine_macro

let is_bitwise_bop = function
  | A.BAND | A.BOR | A.XOR -> true
  | _ -> false

let is_relational_bop = function
  | EQ | NE | LT | GT | LE | GE -> true
  | _ -> false

let rec stripParen = function { expr_node = A.PAREN e } -> stripParen e | e -> e

let rec is_dangerous_offset = function
    NoOffset -> false
  | Field (fi, o) ->
    Cil.typeHasAttribute "volatile" (Cil.unrollType fi.ftype) ||
    is_dangerous_offset o
  | Index _ -> true

let rec is_dangerous e = match e.enode with
  | Lval lv | AddrOf lv | StartOf lv -> is_dangerous_lval lv
  | UnOp (_,e,_) | CastE(_,e) | Info(e,_) -> is_dangerous e
  | BinOp(_,e1,e2,_) -> is_dangerous e1 || is_dangerous e2
  | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
    false
and is_dangerous_lval = function
  | Var v,_ when 
      (not v.vglob && not v.vformal && not v.vtemp) 
      || Cil.hasAttribute "volatile" v.vattr
      || Cil.typeHasAttribute "volatile" (Cil.unrollType v.vtype)
    -> true
  (* Local might be uninitialized, which will trigger UB,
     but we assume that the variables we generate are correctly initialized.
  *)
  | Var _, o -> is_dangerous_offset o
  | Mem _,_ -> true

class check_no_locals = object
  inherit nopCilVisitor
  method! vlval (h,_) =
    (match h with
     | Var v ->
       if not v.vglob then
         Kernel.error ~once:true ~current:true
           "Forbidden access to local variable %a in static initializer"
           Cil_printer.pp_varinfo v
     | _ -> ());
    DoChildren
end

let rec check_no_locals_in_initializer i =
  match i with
  | SingleInit e ->
    ignore (visitCilExpr (new check_no_locals) e)
  | CompoundInit (ct, initl) ->
    foldLeftCompound ~implicit:false
      ~doinit:(fun _off' i' _ () ->
          check_no_locals_in_initializer i')
      ~ct:ct
      ~initl:initl
      ~acc:()


(* ---------- source error message handling ------------- *)
let cabslu s =
  {Lexing.dummy_pos with pos_fname="Cabs2cil_start"^s},
  {Lexing.dummy_pos with pos_fname="Cabs2cil_end"^s}


(** Keep a list of the variable ID for the variables that were created to
 * hold the result of function calls *)
let callTempVars: unit IH.t = IH.create 13

(* Keep a list of functions that were called without a prototype. *)
let noProtoFunctions : bool IH.t = IH.create 13

(* Check that s starts with the prefix p *)
let prefix p s =
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p


(***** PROCESS PRAGMAS **********)

(* fc_stdlib pragma. Delimits blocks of globals that are declared in
   a given std lib header. By default, they will not be pretty-printed by
   frama-c -print, which will emit #include "header.h" instead
*)
let current_stdheader = ref []

let pop_stdheader () =
  match !current_stdheader with
  | s::l ->
     Kernel.debug ~dkey:Kernel.dkey_typing_pragma "Popping %s %s" fc_stdlib s;
     current_stdheader := l
  | [] -> Kernel.warning "#pragma %s pop does not match a push" fc_stdlib

let push_stdheader s =
  Kernel.debug ~dkey:Kernel.dkey_typing_pragma "Pushing %s %s@." fc_stdlib s;
  current_stdheader := s::!current_stdheader

(* Returns the topmost (latest) header that is not internal to Frama-C,
   unless it is the only one.
   This prevents the pretty-printing function from including Frama-C
   internal files, unless they were directly specified by the user. *)
let get_current_stdheader () =
  let rec aux = function
    | [] -> ""
    | [ s ] -> s
    | s :: l when Extlib.string_prefix ~strict:true "__fc_" s -> aux l
    | s :: _ -> s
  in
  aux !current_stdheader

(* there are several pragmas that we process directly here and remove
   from the globals list, by returning None. We bind their respective
   processing functions with the operator below.
*)
let (>>?) opt f =
  match opt with
  | Some (Attr(name, args)) -> f name args
  | _ -> opt

let process_stdlib_pragma name args =
  if name = fc_stdlib then begin
    match args with
    | [ ACons ("pop",_) ] -> pop_stdheader (); None
    | [ ACons ("push",_); AStr s ] ->
      let base_name = Config.datadir ^ "/libc" in
      let relative_name = Filepath.relativize ~base_name s in
      push_stdheader relative_name;
      None
    | _ -> Some (Attr(name, args))
  end else Some (Attr(name, args))

let fc_stdlib_attribute attrs =
  let s = get_current_stdheader () in
  if s = "" then attrs
  else Cil.addAttribute (Attr (fc_stdlib, [AStr s])) attrs

(* ICC align/noalign pragmas (not supported by GCC/MSVC with this syntax).
   Implemented by translating them to 'aligned' attributes. Currently,
   only default and noalign are supported, not explicit alignment values.
   Cf. www.slac.stanford.edu/grp/cd/soft/rmx/manuals/IC_386.PDF *)
let current_pragma_align = ref (None : bool option)
let pragma_align_by_struct = H.create 17

let process_align_pragma name args =
  let aux pname v =
    (if Cil.msvcMode () || Cil.gccMode ()
     then Kernel.warning ?wkey:None else Kernel.debug ~level:1 ?dkey:None)
      ~current:true "Parsing ICC '%s' pragma." pname;
    match args with
    | [] -> current_pragma_align := Some v
    | l ->
      List.iter
        (function
          | AStr s | ACons (s, _) -> H.replace pragma_align_by_struct s v
          | _ -> Kernel.warning ~current:true
                   "Unsupported '%s' pragma not honored by Frama-C." pname
        ) l
  in
  match name with
  | "align" -> aux "align" true
  | "noalign" -> aux "noalign" false
  | _ -> ()

let align_pragma_for_struct sname =
  try Some (H.find pragma_align_by_struct sname)
  with Not_found -> !current_pragma_align

(* The syntax and semantics for the pack pragmas are GCC's, which emulates most
   of MSVC's behaviors. Some of it has been tested using MSVC 2010.
   Note that #pragma pack directives are emulated by translating them into
   GCC-style attributes, which in turn are not supported by MSVC.
   Therefore some combinations of attributes may be impossible to produce in
   MSVC, which means that Frama-C on an MSVC machdep may accept more programs
   that MSVC would. *)

(* The pack pragma stack *)
let packing_pragma_stack = Stack.create ()

(* The current pack pragma *)
let current_packing_pragma = ref None
let pretty_current_packing_pragma fmt =
  let align =
    Extlib.opt_conv (Integer.of_int theMachine.theMachine.alignof_aligned)
      !current_packing_pragma
  in
  (Integer.pretty ~hexa:false) fmt align

(* Checks if [n] is a valid alignment for #pragma pack, and emits a warning
   if it is not the case. Returns the value to be set as current packing pragma.
   From the MSDN reference
   (msdn.microsoft.com/en-us/library/2e70t5y1(v=vs.100).aspx):
   Valid values are 1, 2, 4, 8, and 16.

   NOTE: GCC seems to consider '#pragma pack(0)' as equivalent to '#pragma pack()',
   but this is not specified in their documentation. To avoid rejecting programs
   with such pragmas, we emulate GCC's current behavior but emit a warning.
   This is the only case when this function returns [None]. *)
let get_valid_pragma_pack_alignment n =
  if Integer.is_zero n && Cil.gccMode () then begin
    Kernel.warning ~current:true "GCC accepts pack(0) but does not specify its \
                                  behavior; considering it equivalent to pack()";
    true, None
  end
  else begin
    let valid = Integer.(equal n one || equal n two || equal n four ||
                         equal n eight || equal n sixteen)
    in
    if not valid then
      Kernel.warning ~current:true "ignoring invalid packing alignment (%a)"
        (Integer.pretty ~hexa:false) n;
    valid, Some n
  end

let process_pack_pragma name args =
  begin match name with
    | "pack" -> begin
        match args with
        | [ACons ("",[])] (*  #pragma pack() *) ->
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "packing pragma: restoring alignment to default (%d)"
            theMachine.theMachine.alignof_aligned;
          current_packing_pragma := None; None
        | [AInt n] (* #pragma pack(n) *) ->
          let is_valid, new_pragma = get_valid_pragma_pack_alignment n in
          if is_valid then begin
            Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
              "packing pragma: setting alignment to %a" (Integer.pretty ~hexa:false) n;
            current_packing_pragma := new_pragma; None
          end else
            Some (Attr (name, args))
        | [ACons ("push",[])] (* #pragma pack(push) *) ->
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "packing pragma: pushing alignment %t" pretty_current_packing_pragma;
          Stack.push !current_packing_pragma packing_pragma_stack; None
        | [ACons ("push",[]); AInt n] (* #pragma pack(push,n) *) ->
          let is_valid, new_pragma = get_valid_pragma_pack_alignment n in
          if is_valid then begin
            Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
              "packing pragma: pushing alignment %t, setting alignment to %a"
              pretty_current_packing_pragma (Integer.pretty ~hexa:false) n;
            Stack.push !current_packing_pragma packing_pragma_stack;
            current_packing_pragma:= new_pragma; None
          end else
            Some (Attr (name, args))
        | [ACons ("pop",[])] (* #pragma pack(pop) *) ->
          begin try
              current_packing_pragma := Stack.pop packing_pragma_stack;
              Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
                "packing pragma: popped alignment %t" pretty_current_packing_pragma;
              None
            with Stack.Empty ->
              (* GCC/Clang/MSVC seem to ignore the directive when a pop() is
                 called with an empty stack, so we emulate their behavior. *)
              Kernel.warning ~current:true
                "ignoring #pragma pack(pop) with empty stack";
              None
          end
        | [ACons ("show",[])] (* #pragma pack(show) *) ->
          Some (Attr (name, args))
        | _ ->
          Kernel.warning ~current:true
            "Unsupported packing pragma not honored by Frama-C: #pragma pack(%a)"
            (Pretty_utils.pp_list ~sep:", " ~empty:"<empty>"
               Cil_printer.pp_attrparam) args;
          Some (Attr (name, args))
      end
    | _ -> Some (Attr (name, args))
  end

let force_packed_attribute a =
  if hasAttribute "packed" a then a
  else addAttribute (Attr("packed",[])) a

let is_power_of_two i = i > 0 && i land (i-1) = 0

(* Computes the numeric value corresponding to an 'aligned' attribute:
   - if 'aligned' (without integer), then use the maximum machine alignment;
   - else, try to const-fold the expression to an integer value.
   Returns [Some n] in case of success, [None] otherwise.
   Note that numeric values that are not powers of two are invalid and
   also return [None]. *)
let eval_aligned_attrparams aps =
  match aps with
  | [] -> Some (Integer.of_int theMachine.theMachine.alignof_aligned)
  | [ap] ->
    begin
      match Cil.intOfAttrparam ap with
      | None -> None
      | Some n -> if is_power_of_two n then Some (Integer.of_int n) else None
    end
  | _ -> (* 'aligned(m,n,...)' is not a valid syntax *) None

let warn_invalid_align_attribute aps =
  Kernel.warning ~current:true ~once:true
    "ignoring invalid aligned attribute: %a"
    Cil_printer.pp_attribute (Attr("aligned", aps))

(* If there is more than one 'aligned' attribute, GCC's behavior is to
   consider the maximum among them. This function computes this value
   and also emits warnings for invalid attributes. *)
let combine_aligned_attributes attrs =
  match filterAttributes "aligned" attrs with
  | [] -> None
  | aligned_attrs ->
    List.fold_left (fun acc attr ->
        match attr with
        | Attr("aligned", aps) ->
          begin
            let align = eval_aligned_attrparams aps in
            if align = None then begin
              warn_invalid_align_attribute aps;
              acc
            end else
              match acc, align with
              | None, a | a, None -> a
              | Some old_n, Some new_n -> Some (Integer.max old_n new_n)
          end
        | _ -> assert false (* attributes were previously filtered by name *)
      ) None aligned_attrs

let warn_incompatible_pragmas_attributes apragma has_attrs =
  if apragma <> None then
    Kernel.warning ~current:true ~once:true
      "ignoring 'align' pragma due to presence of 'pack' pragma.@ \
       No compiler was supposed to accept both syntaxes.";
  if Cil.msvcMode () && has_attrs then
    (* MSVC does not allow attributes *)
    Kernel.warning ~current:true ~once:true
      "field attributes should not be present in MSVC-compatible sources"

(* checks [attrs] for invalid aligned() attributes *)
let check_aligned attrs =
  List.fold_right (fun attr acc ->
      match attr with
      | Attr("aligned", aps) ->
        if eval_aligned_attrparams aps = None then
          (warn_invalid_align_attribute aps; acc)
        else attr :: acc
      | _ -> attr :: acc
    ) attrs []

(* Takes into account the possible effect of '#pragma pack' directives on
   component [ci], and checks the alignment of aligned() attributes.
   This function is complemented by
   [process_pragmas_pack_align_field_attributes]. *)
let process_pragmas_pack_align_comp_attributes ci cattrs =
  match !current_packing_pragma, align_pragma_for_struct ci.corig_name with
  | None, None -> check_aligned cattrs
  | Some n, apragma ->
    warn_incompatible_pragmas_attributes apragma (cattrs <> []);
    let with_aligned_attributes =
      match combine_aligned_attributes cattrs with
      | None ->
        (* No valid aligned attributes in this field.
           - if the composite type has a packed attribute, then add the
             alignment given by the pack pragma;
           - otherwise, no alignment attribute is necessary.
           Drop existing "aligned" attributes, if there are invalid ones. *)
        if Cil.hasAttribute "packed" cattrs then (dropAttribute "aligned" cattrs)
        else begin
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "adding aligned(%a) attribute to comp '%s' due to packing pragma"
            (Integer.pretty ~hexa:false) n ci.cname;
          addAttribute (Attr("aligned",[AInt n])) (dropAttribute "aligned" cattrs)
        end
      | Some local ->
        (* The largest aligned wins with GCC. Don't know
           with other compilers. *)
        let align = Integer.max n local in
        Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
          "setting aligned(%a) attribute to comp '%s' due to packing pragma"
          (Integer.pretty ~hexa:false) align ci.cname;
        addAttribute (Attr("aligned",[AInt align]))
          (dropAttribute "aligned" cattrs)
    in
    force_packed_attribute with_aligned_attributes
  | None, Some true ->
    dropAttribute "aligned" cattrs
  | None, Some false ->
    force_packed_attribute
      (addAttribute
         (Attr("aligned",[AInt Integer.one]))
         (dropAttribute "aligned" cattrs))

(* Takes into account the possible effect of '#pragma pack' directives on
   field [fi], and checks the alignment of aligned() attributes.
   Because we emulate them using GCC attributes, this transformation
   is complex and depends on several factors:
   - if the struct inside the pragma is packed, then ignore alignment constraints
     given by the pragma;
   - otherwise, each struct field should have the alignment given by the pack
     directive, unless that field already has an align attribute, in which case
     the minimum of both is taken into account (note that, in GCC, if a field
     has 2 alignment directives, it is the maximum of those that is taken). *)
let process_pragmas_pack_align_field_attributes fi fattrs cattr =
  match !current_packing_pragma, align_pragma_for_struct fi.forig_name with
  | None, None -> check_aligned fattrs
  | Some n, apragma ->
    begin
      warn_incompatible_pragmas_attributes apragma (fattrs <> []);
      match combine_aligned_attributes fattrs with
      | None ->
        (* No valid aligned attributes in this field.
           - if the composite type has a packed attribute, nothing needs to be
           done (the composite will have the "packed" attribute anyway);
           - otherwise, align on min(n,sizeof(fi.ftyp)).
           Drop existing "aligned" attributes, if there are invalid ones. *)
        if Cil.hasAttribute "packed" cattr then (dropAttribute "aligned" fattrs)
        else begin
          let align = Integer.(min n (of_int (Cil.bytesSizeOf fi.ftype))) in
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "adding aligned(%a) attribute to field '%s.%s' due to packing pragma"
            (Integer.pretty ~hexa:false) align fi.fcomp.cname fi.fname;
          addAttribute (Attr("aligned",[AInt align])) (dropAttribute "aligned" fattrs)
        end
      | Some local ->
        (* There is an alignment attribute in this field. This may be smaller
           than the field type alignment, so we get the maximum of both.
           Then, we apply the pragma pack: the final alignment will be the
           minimum between what we had and [n]. *)
        let align = Integer.min n (Integer.max (Integer.of_int (Cil.bytesSizeOf fi.ftype)) local) in
        Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
          "setting aligned(%a) attribute to field '%s.%s' due to packing pragma"
          (Integer.pretty ~hexa:false) align fi.fcomp.cname fi.fname;
        addAttribute (Attr("aligned",[AInt align]))
          (dropAttribute "aligned" fattrs)
    end
  | None, Some true ->
    dropAttribute "aligned" fattrs
  | None, Some false ->
    (addAttribute
       (Attr("aligned",[AInt Integer.one]))
       (dropAttribute "aligned" fattrs))


(***** COMPUTED GOTO ************)

(* The address of labels are small integers (starting from 0). A computed
 * goto is replaced with a switch on the address of the label. We generate
 * only one such switch and we'll jump to it from all computed gotos. To
 * accomplish this we'll add a local variable to store the target of the
 * goto. *)

(* The local variable in which to put the detonation of the goto and the
 * statement where to jump *)
let gotoTargetData: (varinfo * stmt) option ref = ref None

(* The "addresses" of labels *)
let gotoTargetHash: (string, int) H.t = H.create 13
let gotoTargetNextAddr: int ref = ref 0


(********** TRANSPARENT UNION ******)
(* Check if a type is a transparent union, and return the first field if it
 * is *)
let isTransparentUnion (t: typ) : fieldinfo option =
  match unrollType t with
  | TComp (comp, _, _) when not comp.cstruct ->
    (* Turn transparent unions into the type of their first field *)
    if typeHasAttribute "transparent_union" t then begin
      match comp.cfields with
      | [] ->
        abort_context
          "Empty transparent union: %s" (compFullName comp)
      | f :: _ -> Some f
    end else
      None
  | _ -> None

(* When we process an argument list, remember the argument index which has a
 * transparent union type, along with the original type. We need this to
 * process function definitions *)
let transparentUnionArgs : (int * typ) list ref = ref []

let debugLoc = false
let convLoc (l : cabsloc) =
  if debugLoc then
    Kernel.debug "convLoc at %s: line %d, btye %d\n"
      (fst l).Lexing.pos_fname (fst l).Lexing.pos_lnum (fst l).Lexing.pos_bol;
  l

let isOldStyleVarArgName n =
  if Cil.msvcMode () then n = "va_alist"
  else n = "__builtin_va_alist"

let isOldStyleVarArgTypeName n =
  if Cil.msvcMode () then n = "va_list"  || n = "__ccured_va_list"
  else n = "__builtin_va_alist_t"

(* CERT EXP 46 rule: operands of bitwise operators should not be of type _Bool
   or the result of a comparison.
*)
let check_logical_operand e t =
  let (source,_) = e.expr_loc in
  match Cil.unrollType t with
  | TInt(IBool, _) ->
    Kernel.warning ~wkey:Kernel.wkey_cert_exp_46 ~source
      "operand of bitwise operator has boolean type"
  | _ ->
    let rec aux = function
      | { expr_node = A.PAREN e} -> aux e
      | { expr_node = A.BINARY (bop,_,_); expr_loc = (source, _) }
        when is_relational_bop bop ->
        Kernel.warning ~wkey:Kernel.wkey_cert_exp_46 ~source
          "operand of bitwise operator is a logical relation"
      | _ -> (* EXP 46 does not forbid something like
                (x && y) & z, even though the logical and returns 0 or 1 as
                a relational operator. Maybe this should be clarified. *)
        ()
    in
    aux e

(*** EXPRESSIONS *************)

(* We collect here the program *)
let theFile : global list ref = ref []
let theFileTypes : global list ref = ref []
(* This hashtbl contains the varinfo-indexed globals of theFile.
   They are duplicated here for faster lookup *)
let theFileVars : global Cil_datatype.Varinfo.Hashtbl.t =
  Cil_datatype.Varinfo.Hashtbl.create 13

let findVarInTheFile vi =
  try  List.rev (Cil_datatype.Varinfo.Hashtbl.find_all theFileVars vi)
  with Not_found -> []

let update_fundec_in_theFile vi (f:global -> unit) =
  let rec aux = function
    | [] -> assert false
    | (GFunDecl _ as g) :: _ -> f g
    | _ :: tl -> aux tl
  in
  aux (findVarInTheFile vi)

let update_funspec_in_theFile vi spec =
  let rec aux = function
    | [] -> assert false
    | GFun (f,_) :: _ ->
      Cil.CurrentLoc.set vi.vdecl;
      Logic_utils.merge_funspec f.sspec spec
    | _ :: tl -> aux tl
  in
  aux (findVarInTheFile vi)

let find_existing_behaviors vi =
  let behaviors spec = List.map (fun x -> x.b_name) spec.spec_behavior in
  let aux acc = function
    | GFun(f,_) -> (behaviors f.sspec) @ acc
    | GFunDecl (spec,_,_)  -> behaviors spec @ acc
    | _ -> acc
  in List.fold_left aux [] (findVarInTheFile vi)

let get_formals vi =
  let rec aux = function
    | [] -> assert false
    | GFun(f,_)::_ -> f.sformals
    | _ :: tl -> aux tl
  in aux (findVarInTheFile vi)

let initGlobals () =
  theFile := [];
  theFileTypes := [];
  Cil_datatype.Varinfo.Hashtbl.clear theFileVars;
;;

let cabsPushGlobal (g: global) =
  pushGlobal g ~types:theFileTypes ~variables:theFile;
  (match g with
   | GVar (vi, _, _) | GVarDecl (vi, _)
   | GFun ({svar = vi}, _) | GFunDecl (_, vi, _) ->
     (* Do 'add' and not 'replace' here, as we may store both
        declarations and definitions for the same varinfo *)
     Cil_datatype.Varinfo.Hashtbl.add theFileVars vi g
   | _ -> ()
  );
;;


(* Keep track of some variable ids that must be turned into definitions. We
 * do this when we encounter what appears a definition of a global but
 * without initializer. We leave it a declaration because maybe down the road
 * we see another definition with an initializer. But if we don't see any
 * then we turn the last such declaration into a definition without
 * initializer *)
let mustTurnIntoDef: bool IH.t = IH.create 117

(* Globals that have already been defined. Indexed by the variable name. *)
let alreadyDefined: (string, location) H.t = H.create 117

(* Globals that were created due to static local variables. We chose their
 * names to be distinct from any global encountered at the time. But we might
 * see a global with conflicting name later in the file. *)
let staticLocals: (string, varinfo) H.t = H.create 13


(* Typedefs. We chose their names to be distinct from any global encountered
 * at the time. But we might see a global with conflicting name later in the
 * file *)
let typedefs: (string, typeinfo) H.t = H.create 13

let fileGlobals () =
  let rec revonto (tail: global list) = function
      [] -> tail

    | GVarDecl (vi, _) :: rest when IH.mem mustTurnIntoDef vi.vid ->
      IH.remove mustTurnIntoDef vi.vid;
      (* Use the location of vi instead of the one carried by GVarDecl.
         Maybe we found in the same file a declaration and then a tentative
         definition. In this case, both are GVarDecl, but the location carried
         by [vi] is the location of the tentative definition, which is more
         useful. *)
      if vi.vstorage = Extern then vi.vstorage <- NoStorage;
      vi.vdefined <- true;
      revonto (GVar (vi, {init = None}, vi.vdecl) :: tail) rest

    | x :: rest -> revonto (x :: tail) rest
  in
  revonto (revonto [] !theFile) !theFileTypes


(********* ENVIRONMENTS ***************)

(* The environment is kept in two distinct data structures. A hash table maps
 * each original variable name into a varinfo (for variables, or an
 * enumeration tag, or a type). (Note that the varinfo might contain an
 * alpha-converted name different from that of the lookup name.) The Ocaml
 * hash tables can keep multiple mappings for a single key. Each time the
 * last mapping is returned and upon deletion the old mapping is restored. To
 * keep track of local scopes we also maintain a list of scopes (represented
 * as lists).  *)
type envdata =
    EnvVar of varinfo                   (* The name refers to a variable
                                         * (which could also be a function) *)
  | EnvEnum of enumitem                 (* the name refers to an enum item *)
  | EnvTyp of typ                       (* The name is of the form  "struct
                                         * foo", or "union foo" or "enum foo"
                                         * and refers to a type. Note that
                                         * the name of the actual type might
                                         * be different from foo due to alpha
                                         * conversion *)
  | EnvLabel of string                  (* The name refers to a label. This
                                         * is useful for GCC's locally
                                         * declared labels. The lookup name
                                         * for this category is "label foo" *)

let env : (string, envdata * location) H.t = H.create 307
(* We also keep a global environment. This is always a subset of the env *)
let genv : (string, envdata * location) H.t = H.create 307

(* In the scope we keep the original name, so we can remove them from the
 * hash table easily *)
type undoScope =
    UndoRemoveFromEnv of string
  | UndoResetAlphaCounter of location Alpha.alphaTableData ref *
                             location Alpha.alphaTableData
  | UndoRemoveFromAlphaTable of string * string

let scopes :  undoScope list ref list ref = ref []

(* tries to estimate if the name 's' was declared in the current scope;
   note that this may not work in all cases *)
let declared_in_current_scope s =
  match !scopes with
  | [] -> (* global scope: check if present in genv *) H.mem genv s
  | cur_scope :: _ ->
    let names_declared_in_current_scope =
      Extlib.filter_map
        (fun us ->
           match us with
           | UndoRemoveFromEnv _ | UndoRemoveFromAlphaTable _ -> true
           | UndoResetAlphaCounter _ -> false)
        (fun us ->
           match us with
           | UndoRemoveFromEnv s | UndoRemoveFromAlphaTable (s,_) -> s
           | UndoResetAlphaCounter _ -> assert false (* already filtered *)
        ) !cur_scope
    in
    List.mem s names_declared_in_current_scope

(* When you add to env, you also add it to the current scope *)
let addLocalToEnv (n: string) (d: envdata) =
  (*log "%a: adding local %s to env\n" d_loc !currentLoc n; *)
  H.add env n (d, CurrentLoc.get ());
  (* If we are in a scope, then it means we are not at top level. Add the
   * name to the scope *)
  (match !scopes with
   | [] -> begin
       match d with
       | EnvVar _ ->
         Kernel.fatal ~current:true
           "addLocalToEnv: not in a scope when adding %s!" n
       | _ ->
         H.add genv n (d,CurrentLoc.get()) (* We might add types *)
     end
   | s :: _ ->
     s := (UndoRemoveFromEnv n) :: !s)

let addGlobalToEnv (k: string) (d: envdata) : unit =
  (*  ignore (E.log "%a: adding global %s to env\n" d_loc !currentLoc k); *)
  H.add env k (d, CurrentLoc.get ());
  (* Also add it to the global environment *)
  H.add genv k (d, CurrentLoc.get ())

(* Create a new name based on a given name. The new name is formed from a
 * prefix (obtained from the given name as the longest prefix that ends with
 * a non-digit), followed by a '_' and then by a positive integer suffix. The
 * first argument is a table mapping name prefixes with the largest suffix
 * used so far for that prefix. The largest suffix is one when only the
 * version without suffix has been used. *)
let alphaTable : location Alpha.alphaTable = H.create 307
(* vars and enum tags. For composite types we have names like "struct
 * foo" or "union bar" *)

let fresh_global lookupname =
  fst (Alpha.newAlphaName alphaTable lookupname (CurrentLoc.get ()))

(* To keep different name scopes different, we add prefixes to names
 * specifying the kind of name: the kind can be one of "" for variables or
 * enum tags, "struct" for structures and unions (they share the name space),
 * "enum" for enumerations, or "type" for types *)
let kindPlusName (kind: string)
    (origname: string) : string =
  (* typedefs live in the same namespace as normal identifiers. *) 
  if kind = "" || kind = "type" then origname
  else kind ^ " " ^ origname

let stripKind (kind: string) (kindplusname: string) : string =
  let kind = if kind = "type" then "" else kind in
  let l = 1 + String.length kind in
  if l > 1 then
    String.sub kindplusname l (String.length kindplusname - l)
  else
    kindplusname

let is_same_kind kind info =
  match kind, info with
  | "", EnvEnum _
  | "enum", EnvTyp _
  | "type", EnvTyp _
  | "struct", EnvTyp _
  | "union", EnvTyp _
  | "label", EnvLabel _
  | "", EnvVar _ -> true
  | _, _ -> false

let find_identifier_decl name info =
  match info with
  | UndoRemoveFromEnv name' -> name = name'
  | _ -> false

let newAlphaName (globalscope: bool) (* The name should have global scope *)
    (kind: string)
    (origname: string) : string * location =
  let lookupname = kindPlusName kind origname in
  (* If we are in a scope then it means that we are alpha-converting a local
   * name. Go and add stuff to reset the state of the alpha table but only to
   * the top-most scope (that of the enclosing function) *)
  let rec findEnclosingFun = function
      [] -> (* At global scope *) None
    | [s] -> begin
        let prefix, infix = Alpha.getAlphaPrefix lookupname in
        try
          let infixes = H.find alphaTable prefix in
          let countref = H.find infixes infix in
          s := (UndoResetAlphaCounter (countref, !countref)) :: !s; Some s
        with Not_found ->
          s := (UndoRemoveFromAlphaTable (prefix, infix)) :: !s; Some s;
      end
    | _ :: rest -> findEnclosingFun rest
  in
  let undo_scope =
    if not globalscope then findEnclosingFun !scopes else None
  in
  let newname, oldloc =
    Alpha.newAlphaName alphaTable lookupname (CurrentLoc.get ())
  in
  if newname <> lookupname then begin
    (match undo_scope with
     | None -> ()
     | Some s ->
       let newpre, newinf = Alpha.getAlphaPrefix newname in
       s := (UndoRemoveFromAlphaTable (newpre, newinf)) :: !s);
    try
      let info =
        if !scopes = [] then begin
          fst (H.find genv lookupname)
        end else
        if List.exists (find_identifier_decl lookupname) !(List.hd !scopes)
        then fst (H.find env lookupname)
        else raise Not_found
      in
      if not (Kernel.C11.get () && kind = "type") then
        (* in C11, typedefs can be redefined under some conditions (which are
           checked in doTypedef); this test catches other kinds of errors, such
           as redefined enumeration constants *)
        Kernel.error ~current:true
          "redefinition of '%s'%s in the same scope.@ \
           Previous declaration was at %a"
          origname (if is_same_kind kind info then "" else " with different kind")
          Cil_datatype.Location.pretty oldloc
    with 
    | Not_found -> () (* no clash of identifiers *)
    | Failure _ ->
      Kernel.fatal
        "finding a fresh identifier in local scope with empty scopes stack"
  end;
  stripKind kind newname, oldloc

(*** In order to process GNU_BODY expressions we must record that a given
 *** COMPUTATION is interesting *)
let gnu_body_result : (A.statement * ((exp * typ) option ref)) ref
  = ref ({stmt_ghost = false; stmt_node = A.NOP (cabslu "_NOP")}, ref None)

(*** When we do statements we need to know the current return type *)
let dummy_function = emptyFunction "@dummy@"
let currentReturnType : typ ref = ref (TVoid([]))
let currentFunctionFDEC: fundec ref = ref dummy_function


let lastStructId = ref 0
let anonStructName (k: string) (suggested: string) =
  incr lastStructId;
  "__anon" ^ k ^ (if suggested <> "" then "_"  ^ suggested else "")
  ^ "_" ^ (string_of_int (!lastStructId))


let constrExprId = ref 0


let startFile () =
  H.clear env;
  H.clear genv;
  H.clear alphaTable;
  lastStructId := 0;
;;

(* Lookup a variable name. Return also the location of the definition. Might
 * raise Not_found  *)
let lookupVar (n: string) : varinfo * location =
  match H.find env n with
  | (EnvVar vi), loc -> vi, loc
  | _ -> raise Not_found


let lookupGlobalVar (n: string) : varinfo * location =
  match H.find genv n with
  | (EnvVar vi), loc -> vi, loc
  | _ -> raise Not_found

let _docEnv () =
  let acc : (string * (envdata * location)) list ref = ref [] in
  let doone fmt = function
      EnvVar vi, l ->
      Format.fprintf fmt "Var(%s,global=%b) (at %a)"
        vi.vname vi.vglob Cil_printer.pp_location l
    | EnvEnum (_item), l -> Format.fprintf fmt "Enum (at %a)" Cil_printer.pp_location l
    | EnvTyp _t, _l -> Format.fprintf fmt "typ"
    | EnvLabel l, _ -> Format.fprintf fmt "label %s" l
  in
  H.iter (fun k d -> acc := (k, d) :: !acc) env;
  Pretty_utils.pp_list ~sep:"@\n"
    (fun fmt (k, d) -> Format.fprintf fmt "  %s -> %a" k doone d)
    Format.std_formatter
    !acc



(* Add a new variable. Do alpha-conversion if necessary *)
let alphaConvertVarAndAddToEnv (addtoenv: bool) (vi: varinfo) : varinfo =
(*
  ignore (E.log "%t: alphaConvert(addtoenv=%b) %s" d_thisloc addtoenv vi.vname);
*)
  (* Announce the name to the alpha conversion table *)
  let newname, oldloc = newAlphaName (addtoenv && vi.vglob) "" vi.vname in
  (* Make a copy of the vi if the name has changed. Never change the name for
   * global variables *)
  let newvi =
    if vi.vname = newname then
      vi
    else begin
      if vi.vglob then begin
        (* Perhaps this is because we have seen a static local which happened
         * to get the name that we later want to use for a global. *)
        try
          let static_local_vi = H.find staticLocals vi.vname in
          H.remove staticLocals vi.vname;
          (* Use the new name for the static local *)
          static_local_vi.vname <- newname;
          (* And continue using the last one *)
          vi
        with Not_found -> begin
            (* Or perhaps we have seen a typedef which stole our name. This is
               possible because typedefs use the same name space *)
            try
              let typedef_ti = H.find typedefs vi.vname in
              H.remove typedefs vi.vname;
              (* Use the new name for the typedef instead *)
              typedef_ti.tname <- newname;
              (* And continue using the last name *)
              vi
            with Not_found ->
              abort_context
                "It seems that we would need to rename global %s (to %s) \
                 because of previous occurrence at %a"
                vi.vname newname Cil_printer.pp_location oldloc;
          end
      end else begin
        (* We have changed the name of a local variable. Can we try to detect
         * if the other variable was also local in the same scope? Not for
         * now. *)
        copyVarinfo vi newname
      end
    end
  in
  (* Store all locals in the slocals (in reversed order). *)
  if not vi.vglob && not vi.vformal then
    !currentFunctionFDEC.slocals <- newvi :: !currentFunctionFDEC.slocals;

  (if addtoenv then
     if vi.vglob then
       addGlobalToEnv vi.vname (EnvVar newvi)
     else
       addLocalToEnv vi.vname (EnvVar newvi));
(*
  ignore (E.log "  new=%s\n" newvi.vname);
*)
  (*  ignore (E.log "After adding %s alpha table is: %a\n"
              newvi.vname docAlphaTable alphaTable); *)
  newvi

let constFoldTypeVisitor = object
  inherit nopCilVisitor
  method! vtype t: typ visitAction =
    match t with
    | TArray(bt, Some len, _, a) ->
      let len' = constFold true len in
      ChangeDoChildrenPost (
        TArray(bt, Some len', empty_size_cache (), a),
        (fun x -> x)
      )
    | _ -> DoChildren
end

(* Const-fold any expressions that appear as array lengths in this type *)
let constFoldType (t:typ) : typ =
  visitCilType constFoldTypeVisitor t

let get_temp_name () =
  let undolist = ref [] in
  let data = CurrentLoc.get() in
  let name, _ =
    Alpha.newAlphaName ~alphaTable ~undolist ~lookupname:"tmp" ~data
  in
  let undolist = !undolist in
  Alpha.undoAlphaChanges ~alphaTable ~undolist;
  name

(* Create a new temporary variable *)
let newTempVar descr (descrpure:bool) typ =
  let t' = (!typeForInsertedVar) typ in
  let name = get_temp_name () in 
  let vi = makeVarinfo ~temp:true false false name t' in
  vi.vdescr <- Some descr;
  vi.vdescrpure <- descrpure;
  alphaConvertVarAndAddToEnv false vi

let mkAddrOfAndMark loc ((b, off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  begin match lastOffset off with
    | NoOffset ->
      (match b with
       | Var vi ->
         (* Do not mark arrays as having their address taken. *)
         if not (isArrayType vi.vtype) then
           vi.vaddrof <- true
       | _ -> ())
    | Index _ -> ()
    | Field(fi,_) -> fi.faddrof <- true
  end;
  mkAddrOf ~loc lval

(* Call only on arrays *)
let mkStartOfAndMark loc ((_b, _off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  (* Do not mark arrays as having their address taken.
     (match b with
     | Var vi -> vi.vaddrof <- true
     | _ -> ());
  *)
  let res = new_exp ~loc (StartOf lval) in
  res

(* Keep a set of self compinfo for composite types *)
let compInfoNameEnv : (string, compinfo) H.t = H.create 113
let enumInfoNameEnv : (string, enuminfo) H.t = H.create 113


let lookupTypeNoError (kind: string)
    (n: string) : typ * location =
  let kn = kindPlusName kind n in
  match H.find env kn with
  | EnvTyp t, l -> t, l
  | _ -> raise Not_found

let lookupType (kind: string)
    (n: string) : typ * location =
  try
    lookupTypeNoError kind n
  with Not_found ->
    Kernel.fatal ~current:true "Cannot find type %s (kind:%s)" n kind

(* Create the self ref cell and add it to the map. Return also an indication
 * if this is a new one. *)
let createCompInfo (iss: bool) (n: string) ~(norig: string) : compinfo * bool =
  (* Add to the self cell set *)
  let key = (if iss then "struct " else "union ") ^ n in
  try
    H.find compInfoNameEnv key, false (* Only if not already in *)
  with Not_found -> begin
      (* Create a compinfo. This will have "cdefined" false. *)
      let res = mkCompInfo iss n ~norig (fun _ ->[]) (fc_stdlib_attribute []) in
      H.add compInfoNameEnv key res;
      res, true
    end

(* Create the self ref cell and add it to the map. Return an indication
 * whether this is a new one. *)
let createEnumInfo (n: string) ~(norig:string) : enuminfo * bool =
  (* Add to the self cell set *)
  try
    H.find enumInfoNameEnv n, false (* Only if not already in *)
  with Not_found -> begin
      (* Create a enuminfo *)
      let enum =
        { eorig_name = norig; ename = n; eitems = [];
          eattr = fc_stdlib_attribute []; ereferenced = false; ekind = IInt ; }
      in
      H.add enumInfoNameEnv n enum;
      enum, true
    end


(* kind is either "struct" or "union" or "enum" and n is a name *)
let findCompType (kind: string) (n: string) (a: attributes) =
  let makeForward () =
    (* This is a forward reference, either because we have not seen this
     * struct already or because we want to create a version with different
     * attributes  *)
    if kind = "enum" then
      let enum, isnew = createEnumInfo n n in
      if isnew then
        cabsPushGlobal (GEnumTagDecl (enum, CurrentLoc.get ()));
      TEnum (enum, a)
    else
      let iss = if kind = "struct" then true else false in
      let self, isnew = createCompInfo iss n ~norig:n in
      if isnew then
        cabsPushGlobal (GCompTagDecl (self, CurrentLoc.get ()));
      TComp (self, empty_size_cache (), a)
  in
  try
    let old, _ = lookupTypeNoError kind n in (* already defined  *)
    let olda = typeAttrs old in
    let equal =
      try List.for_all2 Cil_datatype.Attribute.equal olda a
      with Invalid_argument _ -> false
    in
    if equal then old else makeForward ()
  with Not_found -> makeForward ()


(* A simple visitor that searches a statement for labels *)
class canDropStmtClass pRes = object
  inherit nopCilVisitor

  method! vstmt s =
    if s.labels != [] then
      (pRes := false; SkipChildren)
    else
    if !pRes then DoChildren else SkipChildren

  method! vinst _ = SkipChildren
  method! vexpr _ = SkipChildren

end
let canDropStatement (s: stmt) : bool =
  let pRes = ref true in
  let vis = new canDropStmtClass pRes in
  ignore (visitCilStmt vis s);
  !pRes


(******** CASTS *********)

let arithmeticConversion = Cil.arithmeticConversion

let integralPromotion = Cil.integralPromotion

(* C99 6.3.2.1:2: l-values used as r-values lose their qualifier. By default,
   we drop qualifiers, and recover them for the few operators that are
   exceptions, also listed in 6.3.2.1:2 *)
let dropQualifiers = Cil.type_remove_qualifier_attributes

(* true if the expression is known to be a boolean result, i.e. 0 or 1. *)
let rec is_boolean_result e =
  match e.enode with
  | Const _ ->
    (match Cil.isInteger e with
     | Some i ->
       Integer.equal i Integer.zero || Integer.equal i Integer.one
     | None -> false)
  | CastE (_,e) -> is_boolean_result e
  | BinOp((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr),_,_,_) -> true
  | BinOp((PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP | Mult
          | Div | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr),_,_,_) -> false
  | UnOp(LNot,_,_) -> true
  | UnOp ((Neg | BNot),_,_) -> false
  | Lval _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _
  | AlignOfE _ | AddrOf _ | StartOf _ | Info _ -> false

(* Like Cil.mkCastT, but it calls typeForInsertedCast *)
let makeCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  if need_cast oldt newt then
    Cil.mkCastT e oldt (!typeForInsertedCast e oldt newt)
  else e

let makeCast ~(e: exp) ~(newt: typ) =
  makeCastT e (typeOf e) newt

(* A cast that is used for conditional expressions. Pointers are Ok.
   Abort if invalid *)
let checkBool (ot : typ) (_ : exp) =
  match unrollType ot with
  | TInt _
  | TPtr _
  | TEnum _
  | TFloat _ -> ()
  |  _ -> Kernel.fatal ~current:true "castToBool %a" Cil_printer.pp_typ ot

(* Evaluate constants to CTrue (non-zero) or CFalse (zero) *)
let rec isConstTrueFalse c: [ `CTrue | `CFalse ] =
  match c with
  | CInt64 (n,_,_) ->
    if Integer.equal n Integer.zero then `CFalse else `CTrue
  | CChr c ->
    if Char.code c = 0 then `CFalse else `CTrue
  | CStr _ | CWStr _ -> `CTrue
  | CReal(f, _, _) ->
    if f = 0.0 then `CFalse else `CTrue
  | CEnum {eival = e} ->
    match isExpTrueFalse e with
    | `CTrue | `CFalse as r -> r
    | `CUnknown -> Kernel.fatal ~current:true "Non-constant enum"
(* Evaluate expressions to `CTrue, `CFalse or `CUnknown *)
and isExpTrueFalse e: [ `CTrue | `CFalse | `CUnknown ] =
  match e.enode with
  | Const c -> (isConstTrueFalse c :> [ `CTrue | `CFalse | `CUnknown ])
  | CastE _ -> begin (* Do not ignore the cast, because of possible overflows.
                        However, calling constFoldToInt might make some UB disappear... *)
      match Cil.constFoldToInt e with
      | None -> `CUnknown
      | Some i ->
        if Integer.(equal zero i) then `CFalse else `CTrue
    end
  | _ -> `CUnknown

let rec isCabsZeroExp e = match e.expr_node with
  | CAST (_, ie) ->
    (match ie with
     | SINGLE_INIT e -> isCabsZeroExp e
     | NO_INIT | COMPOUND_INIT _ -> false)
  | CONSTANT (CONST_INT i) ->
    Integer.is_zero (Cil.parseInt i)
  | _ -> false

module BlockChunk =
struct
  type chunk = {
    stmts: (stmt * lval list * lval list * lval list * stmt ref list) list;
    (* statements of the chunk.

       This list is built on reverse order.

       Each statements comes with the list of
       pending modified, written and read values.
       The first category represents values which are to be modified during
       the execution of the chunk and whose new value depends on the
       statement (hence, it is legal to read them). They are removed
       syntactically from the list of reads, but we keep them to avoid
       spurious warnings in presence of aliases.
       The order of the write is supposed to be
       fixed at this level.
       We also maintain a list of function calls inside the chunk.
       E.g. for G[i] = j, the written lval is G[i], and the read lval are
       G, i, and j.
    *)

    unspecified_order:bool; (* order of evaluation of statements in the
                               chunk is unspecified.
                            *)
    locals: varinfo list; (* variables that are local to the chunk. *)
    statics: varinfo list; (* static variables whose syntactic scope is the
                              current chunk. *)
    cases: stmt list;                 (* A list of case statements
                                       * (statements with Case labels)
                                       * visible at the outer level *)
  }

  let d_stmt_chunk fmt (s,modified,write,reads,calls) =
    Format.fprintf fmt "@[<v 0>/*@[(%a) %a@ <-@ %a@]@;Calls:@;%a@;*/@;%a@]"
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) modified
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) write
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) reads
      (Pretty_utils.pp_list ~sep:",@ " 
         (fun fmt x -> Cil_printer.pp_stmt fmt !x)) calls
      Cil_printer.pp_stmt s

  let d_chunk fmt (c: chunk) =
    Format.fprintf fmt "@[<v 0>@[%a%a@\n%a@]@;@[<v 2>{%a@]}@]"
      (fun fmt b -> if b then Format.fprintf fmt "/* UNDEFINED ORDER */@\n")
      c.unspecified_order
      (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) c.locals
      (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) c.statics
      (Pretty_utils.pp_list ~sep:";@\n" d_stmt_chunk)
      (List.rev c.stmts)

  let empty =
    { stmts = []; cases = []; locals = []; statics = [];
      unspecified_order = false; }

  let empty_stmts l =
    let rec is_empty_stmt s =
      match s.skind with
      | Instr (Skip _) -> s.labels = []
      | Block b -> b.battrs = [] && List.for_all is_empty_stmt b.bstmts
      | UnspecifiedSequence seq ->
        List.for_all is_empty_stmt (List.map (fun (x,_,_,_,_) -> x) seq)
      | _ -> false
    in
    List.for_all is_empty_stmt (List.map (fun (x,_,_,_,_) -> x) l)

  let isEmpty c = empty_stmts c.stmts

  let isNotEmpty c = not (isEmpty c)

  let i2c (i,m,w,r) =
    let c = match i.skind with
      | Instr(Call _ | Local_init(_, ConsInit _, _)) -> [ref i]
      | _ -> []
    in
    { empty with stmts = [i,m,w,r,c]; }

  (* Keep track of the gotos *)
  let backPatchGotos : (string, stmt ref list ref) H.t = H.create 17
  let addGoto (lname: string) (bref: stmt ref) : unit =
    let gotos =
      try
        H.find backPatchGotos lname
      with Not_found -> begin
          let gotos = ref [] in
          H.add backPatchGotos lname gotos;
          gotos
        end
    in
    gotos := bref :: !gotos

  (* Keep track of the labels *)
  let labelStmt : (string, stmt) H.t = H.create 17
  let initLabels () =
    H.clear backPatchGotos;
    H.clear labelStmt

  let resolveGotos () =
    H.iter
      (fun lname gotos ->
         try
           let dest = H.find labelStmt lname in
           List.iter (fun gref -> gref := dest) !gotos;
           (* Format.eprintf "Label %s associated to %a@." lname d_stmt dest*)
         with Not_found -> begin
             Kernel.error ~once:true ~current:true "Label %s not found" lname
           end)
      backPatchGotos

  module Logic_labels = struct
    (* On the contrary to C, use of labels in the logic
       obeys block scope rules. We keep track of these scopes here.
    *)
    let labels: (string, stmt) H.t = H.create 7
    (* label held by the current statement. *)
    let label_current = ref []
    let add_current_label s = label_current := s::!label_current
    (* Don't remove all current label at once, as there might be some
       labels on nested statements. See bts 1536. *)
    let reset_current_label () =
      label_current:= List.tl !label_current

    let scope = Stack.create ()
    let enter_scope () = Stack.push (ref []) scope
    let exit_scope () =
      let scope_labels = Stack.pop scope in
      List.iter (H.remove labels) !scope_labels

    let add_label l stmt =
      let scope = Stack.top scope in
      scope := l::!scope;
      H.add labels l stmt

    let find_label s =
      try
        ref (H.find labels s)
      with Not_found when List.mem s !label_current ->
        let my_ref =
          ref
            (mkEmptyStmt
              (* just a placeholder that will never be used. no need to
                 check for ghost status here. *)
               ~ghost:false ~valid_sid ~loc:(cabslu "_find_label") ())
        in
        addGoto s my_ref; my_ref
  end

  let add_label l labstmt =
    Logic_labels.add_label l labstmt;
    H.add labelStmt l labstmt

  (* transforms a chunk into a block. Note that if the chunk has its
     unspecified_order flag set, the resulting block contains a single
     UnspecifiedSequence statement.
     If the chunk consists in a single block, this block will get returned
     directly, unless collapse_block is set to false.
     By default, the block is scoping. If force_non_scoping is true
     (and the block does not not declare anything by itself), it is made
     non-scoping.
  *)
  let c2block ~ghost ?(collapse_block=true) ?(force_non_scoping=false) c =
    let declares_var = c.locals <> [] || c.statics <> [] in
    if c.unspecified_order then begin
      let b =
        Cil.mkBlock
          [mkStmt ~ghost ~valid_sid (UnspecifiedSequence (List.rev c.stmts))]
      in
      b.blocals <- c.locals;
      b.bstatics <- c.statics;
      b.bscoping <- declares_var || not force_non_scoping;
      b
    end else
      match c.stmts with
      | [{ skind = Block b } as s,_,_,_,_] when
          collapse_block && s.labels = [] ->
        b.blocals <- c.locals @ b.blocals;
        b.bstatics <- c.statics @ b.bstatics;
        b.bscoping <- b.bscoping || declares_var || not force_non_scoping;
        b
      | stmts ->
        let stmts = List.rev_map (fun (s,_,_,_,_) -> s) stmts in
        let b = Cil.mkBlock stmts in
        b.blocals <- c.locals;
        b.bstatics <- c.statics;
        b.bscoping <- declares_var || not force_non_scoping;
        b

  (* converts a chunk into a statement. *)
  let c2stmt ~ghost ?force_non_scoping c =
    let kind =
      if c.unspecified_order then
        let kind = UnspecifiedSequence (List.rev c.stmts) in
        if c.locals <> [] || c.statics <> [] then begin
          let b = Cil.mkBlock [mkStmt ~ghost ~valid_sid kind] in
          b.blocals <- c.locals;
          b.bstatics <- c.statics;
          Block b
        end else kind
      else
        let block = c2block ~ghost ?force_non_scoping c in Block block
    in
    mkStmt ~ghost ~valid_sid kind

  let merge_effects (m1,w1,r1,c1) (m2,w2,r2,c2) =
    let add_uniq l x =
      if List.exists (Lval.equal x) l then l else x::l
    in
    List.fold_left add_uniq m1 m2,
    List.fold_left add_uniq w1 w2,
    List.fold_left add_uniq r1 r2,
    c1 @ c2

  let get_chunk_effects c =
    List.fold_left
      (fun c (_,x,y,z,t) -> merge_effects c (x,y,z,t)) ([],[],[],[]) c.stmts

  (* make a chunk element from a chunk.  *)
  let c2stmt_effect ~ghost c =
    let modified, writes, reads, calls = get_chunk_effects c in
    let stmt = c2stmt ~ghost ~force_non_scoping:true c in
    (stmt, modified, writes, reads, calls)

  let unspecified_chunk c = (* c *)
    (* to restore previous behavior (where unspecified evaluation order
       was not explicitly marked), comment out the line below and make
       unspecified_chunk the identity function.
    *)
    { c with unspecified_order = true }

  let local_var_chunk c v = { c with locals = v::c.locals }

  let static_var_chunk c v = { c with statics = v :: c.statics }

  let visit_chunk vis c =
    List.iter
      (fun (stmt, _, _, _, _) -> ignore (Cil.visitCilStmt vis stmt))
      c.stmts

  (* if we're about to drop a chunk, clean up locals of current func. *)
  let clean_up_chunk_locals c =
    !currentFunctionFDEC.slocals <-
      List.filter
        (fun x -> not (List.exists (Cil_datatype.Varinfo.equal x) c.locals))
        !currentFunctionFDEC.slocals

  (* Gathers locals of blocks. *)
  class locals_visitor () = object
    inherit Cil.nopCilVisitor

    val mutable locals = []
    method get_locals () = locals

    method !vblock block =
      locals <- block.blocals @ locals;
      Cil.DoChildren
  end

  (* Returns the list of all locals in the chunk [c], including the locals
     of blocks in the list of statements of [c].  *)
  let locals_in_chunk c =
    let locals = c.locals in
    let visitor = new locals_visitor () in
    visit_chunk (visitor :> Cil.cilVisitor) c;
    visitor#get_locals () @ locals

  (* Removes the locals of the chunk [c] (including locals of blocks inside
     the chunk) from the locals of the current function. *)
  let full_clean_up_chunk_locals c =
    let locals = locals_in_chunk c in
    !currentFunctionFDEC.slocals <-
      List.filter
        (fun x -> not (List.exists (Cil_datatype.Varinfo.equal x) locals))
        !currentFunctionFDEC.slocals

  (* removes all labels found in the given chunk from the labels table.
     Use this function when you're about to drop a chunk _and_ you are sure
     that there are no references to such labels outside of the chunk (if there
     are, you should not drop it in the first place). Primarily used for
     dropping side-effects from sizeof of related C expressions, in which
     the only labels that might occur are generated by cabs2cil itself and
     are completely internal.
  *)
  let full_clean_up_chunk_labels c =
    let vis = object
      inherit Cil.nopCilVisitor
      method! vstmt s =
        List.iter
          (function
            | Label (s,_,_) ->
              H.remove labelStmt s;
              H.remove backPatchGotos s
            | Case _ | Default _ -> ())
          s.labels;
        Cil.DoChildren
    end
    in
    visit_chunk vis c

  (* drop the side effects coming from the given expression and takes care
     of cleaning the global environment (labels tables and locals list of
     the current function). First argument is used in the warning to indicate
     which construction is dropping side effects
  *)
  let drop_chunk ctxt c e e' =
    if isNotEmpty c then begin
      Kernel.feedback
        ~once:true ~current:true "Dropping side-effect in %s." ctxt;
      IgnoreSideEffectHook.apply (e, e');
      full_clean_up_chunk_labels c;
      let kept_vars, thrown_vars =
        List.partition (fun x -> Cil.appears_in_expr x e') c.locals
      in
      full_clean_up_chunk_locals {c with locals = thrown_vars};
      { empty with locals = kept_vars }
    end else empty

  (* Add a statement at the end. Never refer to this statement again
   * after you call this *)
  let (+++) (c: chunk) (i,m,w,r) =
    let call = match i.skind with
      | Instr (Call _ | Local_init (_, ConsInit _, _)) -> [ref i]
      | _ -> []
    in
    {c with stmts = (i,m,w,r,call) :: c.stmts; }

  (* Append two chunks. Never refer to the original chunks after you call
   * this. And especially never share c2 with somebody else *)
  let (@@) (c1: chunk) (c2, ghost) =
    let r =
      if (c1.unspecified_order && c2.unspecified_order) ||
         (not c1.unspecified_order && not c2.unspecified_order)
      then
        { stmts = c2.stmts @ c1.stmts;
          cases = c1.cases @ c2.cases;
          locals = c1.locals @ c2.locals;
          statics = c1.statics @ c2.statics;
          unspecified_order = c1.unspecified_order;
        }
      else
        match c2.stmts with
        | [] ->
          (match c2.locals, c2.statics with
           | [],[] -> c1
           | ll, ls ->
             { c1 with
               locals = c1.locals @ ll ;
               statics = c1.statics @ ls })
        | [{skind = UnspecifiedSequence l},_,_,_,_]
          when c1.unspecified_order ->
          { stmts = List.rev_append l c1.stmts;
            cases = c1.cases @ c2.cases;
            locals = c1.locals @ c2.locals;
            statics = c1.statics @ c2.statics;
            unspecified_order = c1.unspecified_order; }
        | [s] ->
          { stmts = s :: c1.stmts;
            cases = c1.cases @ c2.cases;
            locals = c1.locals @ c2.locals;
            statics = c1.statics @ c2.statics;
            unspecified_order = c1.unspecified_order;
          }
        | _ ->
          let locals = c1.locals @ c2.locals in
          let statics = c1.statics @ c2.statics in
          (* the lifespan of the locals is the whole chunk,
             not just c2, which may be transformed artificially
             in a block at this point. Likewise, the syntactic scope of
             static local variables is the whole chunk.
          *)
          let c2 = { c2 with locals = []; statics = [] } in
          { stmts = c2stmt_effect ~ghost c2 :: c1.stmts;
            cases = c1.cases @ c2.cases;
            locals; statics;
            unspecified_order = c1.unspecified_order;
          }
    in
    Kernel.debug ~dkey:Kernel.dkey_typing_chunk
      "Concat:@\n%a@\nWITH@\n%a@\nLEADS TO@\n%a@."
      d_chunk c1 d_chunk c2 d_chunk r;
    r

  let remove_reads lv c =
    Kernel.debug ~dkey:Kernel.dkey_typing_chunk
      "Removing %a from chunk@\n%a@."
      Cil_printer.pp_lval lv d_chunk c;
    let remove_list =
      List.filter (fun x -> not (LvalStructEq.equal lv x))
    in
    let remove_from_reads =
      List.map (fun (s,m,w,r,c) -> (s,lv::m,w,remove_list r,c)) in
    let res =
      { c with stmts = remove_from_reads c.stmts; }
    in
    (* Format.eprintf "Result is@\n%a@." d_chunk res; *)
    res

  let remove_effects_stmt (s,_,_,_,_) = (s,[],[],[],[])

  let remove_effects c =
    { c with stmts = List.map remove_effects_stmt c.stmts }

  (* the chunks below are used in statements translation. Hence,
     their order of evaluation is always specified, and we can forget their
     effects.
  *)

  let skipChunk = empty

  (* return can be ghost but only in ghost functions *)
  let returnChunk ~ghost e (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Return(e, l)),[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let ifChunk ~ghost be (l: location) (t: chunk) (e: chunk) : chunk =
    let effects_t = get_chunk_effects t in
    let effects_e = get_chunk_effects e in
    let (m,r,w,c) = merge_effects effects_t effects_e in
    let stmt =
      mkStmt ~ghost ~valid_sid (If(be, c2block ~ghost t, c2block ~ghost e, l))
    in
    { stmts = [ stmt ,m,r,w,c ];
      cases = t.cases @ e.cases;
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let keepPureExpr ~ghost e loc =
    let fundec = !currentFunctionFDEC in
    let s = Cil.mkPureExpr ~ghost ~fundec ~loc e in
    match s.skind with
    | Block b ->
      { empty with
        stmts = List.map (fun s -> (s,[],[],[],[])) b.bstmts;
        locals = b.blocals }
    | _ ->i2c (s,[],[],[])

  (* We can duplicate a chunk if it has a few simple statements, and if
   * it does not have cases, locals or statics *)
  let duplicateChunk (c: chunk) = (* raises Failure if you should not
                                   * duplicate this chunk *)
    if not (Kernel.AllowDuplication.get ()) then
      raise (Failure "cannot duplicate: disallowed by user");
    if c.locals !=[] then
      raise (Failure "cannot duplicate: has locals");
    if c.statics != [] then
      raise (Failure "cannot duplicate: has static locals");
    if c.cases != [] then raise (Failure "cannot duplicate: has cases") else
      let pCount = ref 0 in
      let duplicate_stmt (s,m,w,r,c) =
        if s.labels != [] then
          raise (Failure "cannot duplicate: has labels");
        (match s.skind with
         | If _ | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
         | TryCatch _ | Throw _ | TryFinally _ | TryExcept _
           ->
           raise (Failure "cannot duplicate: complex stmt")
         | Instr _ | Goto _ | Return _ | Break _ | Continue _ ->
           incr pCount);
        if !pCount > 5 then raise
            (Failure ("cannot duplicate: too many instr"));
        (* We can just copy it because there is nothing to share here.
         * Except maybe for the ref cell in Goto but it is Ok to share
         * that, I think *)
        let s' = { s with sid = s.sid} in
        let c = match s.skind with
          | Instr (Call _ | Local_init (_, ConsInit _, _)) -> [ref s']
          | Instr _ | TryExcept _ | TryFinally _ | TryCatch _ | Throw _
          | UnspecifiedSequence _| Block _| Loop (_, _, _, _, _)
          | Switch (_, _, _, _)| If (_, _, _, _)| Continue _| Break _
          | Goto (_, _)| Return (_, _) -> assert (c = []); []
        in
        (s',m,w,r,c)
      in
      { stmts = List.map duplicate_stmt c.stmts;
        cases = []; unspecified_order = c.unspecified_order;
        locals = []; statics = [];
      }

  (* We can drop a chunk if it does not have labels inside *)
  let canDrop (c: chunk) =
    List.for_all (fun (s,_,_,_,_) -> canDropStatement s) c.stmts

  let loopChunk ~ghost a (body: chunk) : chunk =
    (* Make the statement *)
    let loop =
      mkStmt ~ghost ~valid_sid
        (Loop (a,c2block ~ghost body, CurrentLoc.get (), None, None))
    in
    { stmts = [ loop,[],[],[],[] ];
      cases = body.cases;
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* can be ghost inside a ghost loop *)
  let breakChunk ~ghost (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Break l),[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* can be ghost inside a ghost loop *)
  let continueChunk ~ghost (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Continue l),[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* Get the first statement in a chunk. Might need to change the
   * statements in the chunk *)
  let getFirstInChunk ~ghost ~loc c =
    (* Get the first statement and add the label to it *)
    match c.stmts with
    | [] -> (* Add a statement *)
      let n = mkEmptyStmt ~ghost ~valid_sid ~loc () in
      n, [n,[],[],[],[]]
    | s -> let (st,_,_,_,_) = Extlib.last s in st,s

  (* s2c must not be used during expression translation, as it does not
     take care of the effects of the statement. Use i2c instead.
  *)
  let s2c (s:stmt) : chunk =
    { stmts = [ s,[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  let gotoChunk ~ghost (ln: string) (l: location) : chunk =
    let gref = ref dummyStmt in
    addGoto ln gref;
    { stmts = [ mkStmt ~ghost ~valid_sid (Goto (gref, l)),[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let caseRangeChunk ~ghost el loc (next: chunk) =
    let fst, stmts' = getFirstInChunk ~ghost ~loc next in
    let labels = List.map (fun e -> Case (e, loc)) el in
    fst.labels <- labels @ fst.labels;
    { next with stmts = stmts'; cases = fst :: next.cases;
                unspecified_order = false
    }

  let defaultChunk ~ghost loc (next: chunk) =
    let fst, stmts' = getFirstInChunk ~ghost ~loc next in
    let lb = Default loc in
    fst.labels <- lb :: fst.labels;
    { next with stmts = stmts'; cases = fst :: next.cases;
                unspecified_order = false
    }

  let switchChunk ~ghost (e: exp) (body: chunk) (l: location) =
    (* Make the statement *)
    let defaultSeen = ref false in
    let t = typeOf e in
    let checkForDefaultAndCast lb =
      match lb with
      | Default _ as d ->
        if !defaultSeen then
          Kernel.error ~once:true ~current:true
            "Switch statement at %a has duplicate default entries."
            Cil_printer.pp_location l;
        defaultSeen := true;
        d
      | Label _ as l -> l
      | Case (e, loc) ->
        (* If needed, convert e to type t, and check in case the label
           was too big *)
        let e' = makeCast ~e ~newt:t in
        let constFold = constFold true in
        let e'' = if theMachine.lowerConstants then constFold e' else e' in
        (match constFoldToInt e, constFoldToInt e'' with
         | Some i1, Some i2 when not (Integer.equal i1 i2) ->
           Kernel.feedback ~once:true ~source:(fst e.eloc)
             "Case label %a exceeds range of %a for switch expression. \
              Nothing to worry." 
             Cil_printer.pp_exp e Cil_printer.pp_typ t;
         | _ -> ()
        );
        Case (e'', loc)
    in
    let block = c2block ~ghost body in
    let cases = (* eliminate duplicate entries from body.cases. A statement
                   is added to body.cases for each case label it has. *)
      List.fold_right
        (fun s acc ->
           if List.memq s acc then acc
           else begin
             s.labels <- List.map checkForDefaultAndCast s.labels;
             s::acc
           end)
        body.cases
        []
    in
    let switch = mkStmt ~ghost ~valid_sid (Switch (e, block, cases, l)) in
    { stmts = [ switch,[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  exception Found

  let find_stmt b l s =
    let find = object
      inherit Cil.nopCilVisitor
      method! vstmt s' =
        if s == s' then begin
          (*Format.eprintf "Label %s is in the AST@." l;*)
          raise Found
        end else DoChildren
    end in
    try
      ignore (visitCilBlock find b);
      Kernel.warning ~current:true
        "Inconsistent AST: Statement %a,@ with label %s is not in the AST"
        Cil_printer.pp_stmt s l;
    with Found -> ()

  class cleanUnspecified = object(self)
    inherit nopCilVisitor
    val unspecified_stack = Stack.create ()

    val mutable replace_table = []

    (* we start in a deterministic block. *)
    initializer Stack.push false unspecified_stack

    method private push: 'a.bool->'a->'a visitAction =
      fun flag x ->
        Stack.push flag unspecified_stack;
        ChangeDoChildrenPost
          (x,fun x -> ignore(Stack.pop unspecified_stack); x)


    method! vblock b =
      b.bstmts <-
        List.rev
          (List.fold_left(
              fun res s ->
                match s.skind with
                | Block b when
                    (not (Stack.top unspecified_stack)) &&
                    b.battrs = [] && b.blocals = [] &&
                    s.labels = []
                  -> List.rev_append b.bstmts res
                | _ -> s ::res)
              [] b.bstmts);
      DoChildren

    method! vstmt s =
      let ghost = s.ghost in
      let change_label_stmt s s' =
        List.iter
          (function
            | Label (x,_,_) -> H.replace labelStmt x s'
            | Case _ | Default _ -> replace_table <- (s, s') :: replace_table
          ) s.labels;
        s'.labels <- s.labels @ s'.labels
      in
      match s.skind with
      | UnspecifiedSequence [s',_,_,_,_] ->
        change_label_stmt s s';
        ChangeDoChildrenPost(s', fun x -> x)
      | UnspecifiedSequence [] ->
        let s' = mkEmptyStmt ~ghost ~valid_sid ~loc:(cabslu "_useq") () in
        change_label_stmt s s';
        ChangeTo s';
      | UnspecifiedSequence _ -> self#push true s
      | Block { battrs = []; blocals = []; bstmts = [s']} ->
        change_label_stmt s s';
        ChangeDoChildrenPost (s', fun x -> x)
      | Block _ | If _ | Loop _
      | TryFinally _ | TryExcept _ | Throw _ | TryCatch _ ->
        self#push false s
      | Switch _ ->
        let change_cases stmt =
          match stmt.skind with
          | Switch(e,body,cases,loc) ->
            let newcases =
              List.map
                (fun s ->
                   try List.assq s replace_table
                   with Not_found -> s)
                cases
            in
            stmt.skind <- Switch(e,body,newcases,loc);
            ignore (Stack.pop unspecified_stack);
            stmt
          | _ -> assert false
        in Stack.push false unspecified_stack;
        ChangeDoChildrenPost(s,change_cases)
      | Instr _ | Return _ | Goto _ | Break _
      | Continue _ -> DoChildren
  end

  let mkFunctionBody ~ghost (c: chunk) : block =
    if c.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Switch cases not inside a switch statement\n";
    (* cleanup empty blocks and unspecified sequences.
       This can change some labels (the one attached to removed blocks),
       so it has to be done before resolveGotos. *)
    let res = visitCilBlock (new cleanUnspecified) (c2block ~ghost c) in
    H.iter (find_stmt res) labelStmt; resolveGotos (); initLabels (); res

  let add_reads ~ghost loc r c = match r with
    | [] -> c
    | _ :: _ -> c +++ (mkEmptyStmt ~ghost ~valid_sid ~loc (), [],[], r)

end

open BlockChunk

(* To avoid generating backward gotos, we treat while loops as non-while ones,
 * adding a marker for continue. (useful for Jessie) *)
let doTransformWhile = ref false

let setDoTransformWhile () = doTransformWhile := true

(* To avoid generating forward ingoing gotos, we translate conditionals in
 * an alternate way. (useful for Jessie) *)
let doAlternateConditional = ref false

let setDoAlternateConditional () = doAlternateConditional := true

(************ Labels ***********)
(* Since we turn dowhile and for loops into while we need to take care in
 * processing the continue statement. For each loop that we enter we place a
 * marker in a list saying what kinds of loop it is. When we see a continue
 * for a Non-while loop we must generate a label for the continue *)

type loopstate =
    While of string ref
  | NotWhile of string ref

let continues : loopstate list ref = ref []

(* Sometimes we need to create new label names *)
let newLabelName (base: string) = fst (newAlphaName false "label" base)

let continueOrLabelChunk ~ghost (l: location) : chunk =
  match !continues with
  | [] -> abort_context "continue not in a loop"
  | While lr :: _ ->
    if !doTransformWhile then
      begin
        if !lr = "" then begin
          lr := newLabelName "__Cont"
        end;
        gotoChunk ~ghost !lr l
      end
    else continueChunk ~ghost l
  | NotWhile lr :: _ ->
    if !lr = "" then begin
      lr := newLabelName "__Cont"
    end;
    gotoChunk ~ghost !lr l

(* stack of statements inside which break instruction can be found. *)
let break_env = Stack.create ()

let enter_break_env () = Stack.push () break_env

let breakChunk ~ghost l =
  if Stack.is_empty break_env then
    abort_context "break outside of a loop or switch";
  breakChunk ~ghost l

let exit_break_env () =
  if Stack.is_empty break_env then
    Kernel.fatal ~current:true
      "trying to exit a breakable env without having entered it";
  ignore (Stack.pop break_env)

(* In GCC we can have locally declared labels. *)
let genNewLocalLabel (l: string) =
  (* Call the newLabelName to register the label name in the alpha conversion
   * table. *)
  let l' = newLabelName l in
  (* Add it to the environment *)
  addLocalToEnv (kindPlusName "label" l) (EnvLabel l');
  l'

let lookupLabel (l: string) =
  try
    match H.find env (kindPlusName "label" l) with
    | EnvLabel l', _ -> l'
    | _ -> raise Not_found
  with Not_found ->
    l

class gatherLabelsClass : V.cabsVisitor = object (self)
  inherit V.nopCabsVisitor

  (* We have to know if a label is local to know if it is an error if
   * another label with the same name exists. But a local label can be
   * declared multiple times at different nesting depths. Since a
   * Hashtbl can maintain multiple mappings, we add and remove local
   * labels as we visit their blocks. We map each local label to a
   * location option indicating where it was defined (if it has been).
   * This enables us to raise an error if a local label is defined
   * twice, and we can issue warnings if local labels are declared but
   * never defined. *)
  val localLabels : (string, location option) H.t = H.create 5

  method private addLocalLabels blk =
    List.iter (fun lbl -> H.add localLabels lbl None) blk.blabels
  method private removeLocalLabels blk =
    List.iter
      (fun lbl ->
         if H.find localLabels lbl = None then
           Kernel.warning ~current:true
             "Local label %s declared but not defined" lbl;
         H.remove localLabels lbl)
      blk.blabels

  method! vblock blk =
    (* Add the local labels, process the block, then remove the local labels *)
    self#addLocalLabels blk;
    ChangeDoChildrenPost (blk, fun _ -> (self#removeLocalLabels blk; blk))

  method! vstmt s =
    CurrentLoc.set (get_statementloc s);
    (match s.stmt_node with
     | LABEL (lbl,_,_) ->
       (try
          (match H.find localLabels lbl with
           | Some oldloc ->
             Kernel.error ~once:true ~current:true
               "Duplicate local label '%s' (previous definition was at %a)"
               lbl Cil_printer.pp_location oldloc
           | None ->
             (* Mark this label as defined *)
             H.replace localLabels lbl (Some (CurrentLoc.get())))
        with Not_found -> (* lbl is not a local label *)
          let newname, oldloc = newAlphaName false "label" lbl in
          if newname <> lbl then
            Kernel.error ~once:true ~current:true
              "Duplicate label '%s' (previous definition was at %a)"
              lbl Cil_printer.pp_location oldloc)
     | _ -> ());
    DoChildren
end


(* Enter all the labels into the alpha renaming table to prevent
   duplicate labels when unfolding short-circuiting logical operators
   and when creating labels for (some) continue statements. *)
class registerLabelsVisitor = object
  inherit V.nopCabsVisitor

  method! vstmt s =
    let currentLoc = convLoc (C.get_statementloc s) in
    (match s.stmt_node with
     | A.LABEL (lbl,_,_) ->
       Alpha.registerAlphaName alphaTable (kindPlusName "label" lbl) currentLoc
     | _ -> ());
    DoChildren
end



(* Maps local variables that are variable sized arrays to the expression that
 * denotes their length *)
let varSizeArrays : exp IH.t = IH.create 17

(**** EXP actions ***)
type expAction =
    ADrop                               (* Drop the result. Only the
                                         * side-effect is interesting *)
  | AType                               (* Only the type of the result
                                           is interesting.  *)
  | ASet of bool * lval * lval list * typ
  (* Put the result in a given lval,
   * provided it matches the type. The
   * type is the type of the lval.
   * the flag indicates whether this
   * should be considered in the
   * effects of current
   * chunk.
   * The lval list is the list of location that are read to evaluate
   * the location of the lval.
   * The location of lval is guaranteed
   * not to depend on its own value,
   * e.g. p[p[0]] when p[0] is initially
   * 0, so the location won't change
   * after assignment.
  *)
  | AExp of typ option                  (* Return the exp as usual.
                                         * Optionally we can specify an
                                         * expected type. This is useful for
                                         * constants. The expected type is
                                         * informational only, we do not
                                         * guarantee that the converted
                                         * expression has that type.You must
                                         * use a doCast afterwards to make
                                         * sure. *)
  | AExpLeaveArrayFun                   (* Do it like an expression, but do
                                         * not convert arrays of functions
                                         * into pointers *)


(*** Result of compiling conditional expressions *)
type condExpRes =
    CEExp of chunk * exp (* Do a chunk and then an expression *)
  | CEAnd of condExpRes * condExpRes
  | CEOr  of condExpRes * condExpRes
  | CENot of condExpRes

let rec clean_up_cond_locals =
  function
  | CEAnd(ce1, ce2) | CEOr(ce1, ce2) ->
    clean_up_cond_locals ce1; clean_up_cond_locals ce2
  | CENot ce -> clean_up_cond_locals ce
  | CEExp (c,_) -> clean_up_chunk_locals c

(* We have our own version of addAttributes that does not allow duplicates *)
let cabsAddAttributes al0 (al: attributes) : attributes =
  if al0 == [] then al else
    List.fold_left
      (fun acc (Attr(an, _) | AttrAnnot an as a) ->
         (* See if the attribute is already in there *)
         match filterAttributes an acc with
         | [] -> addAttribute a acc (* Nothing with that name *)
         | a' :: _ ->
           if Cil_datatype.Attribute.equal a a' then
             acc (* Already in *)
           else begin
             Kernel.debug ~level:3
               "Duplicate attribute %a along with %a"
               Cil_printer.pp_attribute a Cil_printer.pp_attribute a' ;
             (* let acc' = dropAttribute an acc in *)
             (** Keep both attributes *)
             addAttribute a acc
           end)
      al
      al0

type combineWhat =
    CombineFundef of bool
  (* The new definition is for a function definition. The old
   * is for a prototype. arg is [true] for an old-style declaration *)
  | CombineFunarg of bool
  (* Comparing a function argument type with an old prototype argument.
     arg is [true] for an old-style declaration, which
     triggers some ad hoc treatment in GCC mode. *)
  | CombineFunret (* Comparing the return of a function with that from an old
                   * prototype *)
  | CombineOther

(* [combineAttributes what olda a] combines the attributes in [olda] and [a]
   according to [what]:
   - if [what == CombineFunarg], then override old attributes;
     this is used to ensure that attributes from formal argument types in a
     function definition are not mixed with attributes from arguments in other
     (compatible, but with different qualifiers) declarations;
   - else, perform the union of old and new attributes. *)
let combineAttributes what olda a =
  match what with
  | CombineFunarg _ -> a (* override old attributes with new ones *)
  | _ -> cabsAddAttributes olda a (* union of attributes *)

(* BY: nothing cabs here, plus seems to duplicate most of Cil.typeAddAttributes *)
(* see [combineAttributes] above for details about the [what] argument *)
let rec cabsTypeCombineAttributes what a0 t =
  let combine = combineAttributes what in
  begin
    match a0 with
    | [] ->
      (* no attributes, keep same type *)
      t
    | _ ->
      (* anything else: add a0 to existing attributes *)
      let add (a: attributes) = combine a0 a in
      match t with
      | TVoid a -> TVoid (add a)
      | TInt (ik, a) ->
        (* Here we have to watch for the mode attribute *)
        (* sm: This stuff is to handle a GCC extension where you can request integers*)
        (* of specific widths using the "mode" attribute syntax; for example:     *)
        (*   typedef int int8_t __attribute__ ((__mode__ (  __QI__ ))) ;          *)
        (* The cryptic "__QI__" defines int8_t to be 8 bits wide, instead of the  *)
        (* 32 bits you'd guess if you didn't know about "mode".  The relevant     *)
        (* testcase is test/small2/mode_sizes.c, and it was inspired by my        *)
        (* /usr/include/sys/types.h.                                              *)
        (*                                                                        *)
        (* A consequence of this handling is that we throw away the mode          *)
        (* attribute, which we used to go out of our way to avoid printing anyway.*)
        let ik', a0' =
          (* Go over the list of new attributes and come back with a
           * filtered list and a new integer kind *)
          List.fold_left
            (fun (ik', a0') a0one ->
               match a0one with
               | Attr("mode", [ACons(mode,[])]) -> begin
                   (* (trace "gccwidth" (dprintf "I see mode %s applied to an int type\n"
                                        mode )); *)
                   (* the cases below encode the 32-bit assumption.. *)
                   match (ik', mode) with
                   | (IInt, "__QI__")      -> (IChar, a0')
                   | (IInt, "__byte__")    -> (IChar, a0')
                   | (IInt, "__HI__")      -> (IShort,  a0')
                   | (IInt, "__SI__")      -> (IInt, a0')   (* same as t *)
                   | (IInt, "__word__")    -> (IInt, a0')
                   | (IInt, "__pointer__") -> (IInt, a0')
                   | (IInt, "__DI__")      -> (ILongLong, a0')

                   | (IUInt, "__QI__")     -> (IUChar, a0')
                   | (IUInt, "__byte__")   -> (IUChar, a0')
                   | (IUInt, "__HI__")     -> (IUShort, a0')
                   | (IUInt, "__SI__")     -> (IUInt, a0')
                   | (IUInt, "__word__")   -> (IUInt, a0')
                   | (IUInt, "__pointer__")-> (IUInt, a0')
                   | (IUInt, "__DI__")     -> (IULongLong, a0')

                   | _ ->
                     Kernel.error ~once:true ~current:true
                       "GCC width mode %s applied to unexpected type, \
                        or unexpected mode"
                       mode;
                     (ik', a0one :: a0')

                 end
               | _ -> (ik', a0one :: a0'))
            (ik, [])
            a0
        in
        TInt (ik', combine a0' a)

      | TFloat (fk, a) -> TFloat (fk, add a)
      | TEnum (enum, a) -> TEnum (enum, add a)
      | TPtr (t, a) -> TPtr (t, add a)
      | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
      | TComp (comp, s, a) -> TComp (comp, s, add a)
      | TNamed (t, a) -> TNamed (t, add a)
      | TBuiltin_va_list a -> TBuiltin_va_list (add a)
      | TArray (t, l, s, a) ->
        let att_elt, att_typ = Cil.splitArrayAttributes a0 in
        TArray (cabsArrayPushAttributes what att_elt t, l, s,
                combineAttributes what att_typ a)
  end
and cabsArrayPushAttributes what al = function
  | TArray (bt, l, s, a) ->
    TArray (cabsArrayPushAttributes what al bt, l, s, a)
  | t -> cabsTypeCombineAttributes what al t

let cabsTypeAddAttributes =
  cabsTypeCombineAttributes CombineOther

exception Cannot_combine of string

(* Do types *)
(* Combine the types. Raises the Cannot_combine exception with an error message.
   [what] is used to recursively deal with function return types and function
   arguments in special ways.
   Note: we cannot force the qualifiers of oldt and t to be the same here,
   because in some cases (e.g. string literals and char pointers) it is
   allowed to have differences, while in others we want to be more strict. *)
let rec combineTypes (what: combineWhat) (oldt: typ) (t: typ) : typ =
  match oldt, t with
  | TVoid olda, TVoid a -> TVoid (combineAttributes what olda a)
  (* allows ignoring a returned value *)
  | _ , TVoid _ when what = CombineFunret -> t
  | TInt (oldik, olda), TInt (ik, a) ->
    let combineIK oldk k =
      if oldk = k then oldk else
        (match what with
         | CombineFunarg b when
             Cil.gccMode () && oldk = IInt
             && bytesSizeOf t <= (bytesSizeOfInt IInt) && b ->
           (* GCC allows a function definition to have a more precise integer
            * type than a prototype that says "int" *)
           k
         | _ ->
           raise (Cannot_combine
                    (Format.asprintf
                       "different integer types:@ '%a' and '%a'"
                       Cil_printer.pp_ikind oldk Cil_printer.pp_ikind k)))
    in
    TInt (combineIK oldik ik, combineAttributes what olda a)
  | TFloat (oldfk, olda), TFloat (fk, a) ->
    let combineFK oldk k =
      if oldk = k then oldk else
        ( match what with
          | CombineFunarg b when
              Cil.gccMode () && oldk = FDouble && k = FFloat && b ->
            (* GCC allows a function definition to have a more precise float
             * type than a prototype that says "double" *)
            k
          | _ ->
            raise (Cannot_combine "different floating point types"))
    in
    TFloat (combineFK oldfk fk, combineAttributes what olda a)
  | TEnum (_, olda), TEnum (ei, a) ->
    TEnum (ei, combineAttributes what olda a)

  (* Strange one. But seems to be handled by GCC *)
  | TEnum (oldei, olda) , TInt(IInt, a) -> TEnum(oldei,
                                                 combineAttributes what olda a)
  (* Strange one. But seems to be handled by GCC *)
  | TInt(IInt, olda), TEnum (ei, a) -> TEnum(ei, combineAttributes what olda a)


  | TComp (oldci, _, olda) , TComp (ci, _, a) ->
    if oldci.cstruct <> ci.cstruct then
      raise (Cannot_combine "different struct/union types");
    let comb_a = combineAttributes what olda a in
    if oldci.cname = ci.cname then
      TComp (oldci, empty_size_cache (), comb_a)
    else
      raise (Cannot_combine (Format.sprintf "%ss with different tags"
                               (if oldci.cstruct then "struct" else "union")))

  | TArray (oldbt, oldsz, _, olda), TArray (bt, sz, _, a) ->
    let newbt = combineTypes CombineOther oldbt bt in
    let newsz =
      match oldsz, sz with
      | None, Some _ -> sz
      | Some _, None -> oldsz
      | None, None -> sz
      | Some oldsz', Some sz' ->
        (* They are not structurally equal. But perhaps they are equal if
         * we evaluate them. Check first machine independent comparison  *)
        let checkEqualSize (machdep: bool) =
          ExpStructEq.equal
            (constFold machdep oldsz')
            (constFold machdep sz')
        in
        if checkEqualSize false then
          oldsz
        else if checkEqualSize true then begin
          Kernel.warning ~current:true
            "Array type comparison succeeds only based on machine-dependent \
             constant evaluation: %a and %a\n"
            Cil_printer.pp_exp oldsz' Cil_printer.pp_exp sz' ;
          oldsz
        end else
          raise (Cannot_combine "different array lengths")

    in
    TArray (newbt, newsz, empty_size_cache (), combineAttributes what olda a)

  | TPtr (oldbt, olda), TPtr (bt, a) ->
    TPtr (combineTypes CombineOther oldbt bt, combineAttributes what olda a)

  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
    let newrt = combineTypes CombineFunret oldrt rt in
    if oldva != va then
      raise (Cannot_combine "different vararg specifiers");
    (* If one does not have arguments, believe the one with the
     * arguments *)
    let newargs, olda' =
      if oldargs = None then args, olda else
      if args = None then oldargs, olda else
        let oldargslist = argsToList oldargs in
        let argslist = argsToList args in
        if List.length oldargslist <> List.length argslist then
          raise (Cannot_combine "different number of arguments")
        else begin
          (* Construct a mapping between old and new argument names. *)
          let map = H.create 5 in
          List.iter2
            (fun (on, _, _) (an, _, _) -> H.replace map on an)
            oldargslist argslist;
          (* Go over the arguments and update the old ones with the
           * adjusted types *)
          (* Format.printf "new type is %a@." Cil_printer.pp_typ t; *)
          let what =
            match what with
            | CombineFundef b -> CombineFunarg b
            | _ -> CombineOther
          in
          Some
            (List.map2
               (fun (on, ot, oa) (an, at, aa) ->
                  (* Update the names. Always prefer the new name. This is
                   * very important if the prototype uses different names than
                   * the function definition. *)
                  let n = if an <> "" then an else on in
                  let t = combineTypes what ot at in
                  let a = addAttributes oa aa in
                  (n, t, a))
               oldargslist argslist),
          olda
        end
    in
    (* Drop missingproto as soon as one of the type is a properly declared one*)
    let olda = 
      if not (Cil.hasAttribute "missingproto" a) then
        Cil.dropAttribute "missingproto" olda'
      else olda'
    in
    let a = 
      if not (Cil.hasAttribute "missingproto" olda') then
        Cil.dropAttribute "missingproto" a
      else a
    in
    TFun (newrt, newargs, oldva, combineAttributes what olda a)

  | TNamed (oldt, olda), TNamed (t, a) when oldt.tname = t.tname ->
    TNamed (oldt, combineAttributes what olda a)

  | TBuiltin_va_list olda, TBuiltin_va_list a ->
    TBuiltin_va_list (combineAttributes what olda a)

  (* Unroll first the new type *)
  | _, TNamed (t, a) ->
    let res = combineTypes what oldt t.ttype in
    cabsTypeCombineAttributes what a res

  (* And unroll the old type as well if necessary *)
  | TNamed (oldt, a), _ ->
    let res = combineTypes what oldt.ttype t in
    cabsTypeCombineAttributes what a res

  | _ -> raise (Cannot_combine
                  (Format.asprintf "different type constructors:@ %a and %a"
                     Cil_printer.pp_typ oldt Cil_printer.pp_typ t))

let get_qualifiers t = Cil.filter_qualifier_attributes (Cil.typeAttrs t)

let equal_qualifiers a1 a2 =
  Cil_datatype.Attributes.equal
    (Cil.filter_qualifier_attributes a1) (Cil.filter_qualifier_attributes a2)

(* precondition: t1 and t2 must be "compatible" as per combineTypes, i.e.
   you must have called [combineTypes t1 t2] before calling this function.
   When [relaxed] is true, qualifier differences are ignored; this is
   an internal parameter used during recursive calls.
   The qualifier compatibility algorithm is:
   - by default, type qualifiers are ignored (e.g. for basic types);
   - when entering a pointer type, stop ignoring type qualifiers;
   - when entering a function type, resume ignoring type qualifiers. *)
let rec have_compatible_qualifiers_deep ?(relaxed=false) t1 t2 =
  match unrollType t1, unrollType t2 with
  | TFun (tres1, Some args1, _, _), TFun (tres2, Some args2, _, _) ->
    have_compatible_qualifiers_deep ~relaxed:true tres1 tres2 &&
    List.for_all2 (fun (_, t1', a1) (_, t2', a2) ->
        have_compatible_qualifiers_deep ~relaxed:true t1' t2' &&
        equal_qualifiers a1 a2)
      args1 args2
  | TPtr (t1', a1), TPtr (t2', a2)
  | TArray (t1', _, _, a1), TArray (t2', _, _, a2) ->
    have_compatible_qualifiers_deep ~relaxed:false t1' t2' &&
    (relaxed || equal_qualifiers a1 a2)
  | _, _ -> relaxed || equal_qualifiers (Cil.typeAttrs t1) (Cil.typeAttrs t2)

let compatibleTypes t1 t2 =
  try
    let r = combineTypes CombineOther t1 t2 in
    (* C99, 6.7.3 §9: "... to be compatible, both shall have the identically
       qualified version of a compatible type;" *)
    if not (have_compatible_qualifiers_deep t1 t2) then
      raise (Cannot_combine "different qualifiers");
    (* Note: different non-qualifier attributes will be silently dropped. *)
    r
  with Cannot_combine _ as e ->
    raise e

let areCompatibleTypes t1 t2 =
  try
    ignore (compatibleTypes t1 t2); true
  with Cannot_combine _ -> false

(* Specify whether the cast is from the source code *)
let rec castTo ?(fromsource=false)
    (ot : typ) (nt : typ) (e : exp) : (typ * exp ) =
  Kernel.debug ~dkey:Kernel.dkey_typing_cast "@[%t: castTo:%s %a->%a@\n@]"
    Cil.pp_thisloc (if fromsource then "(source)" else "")
    Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
  let ot' = unrollType ot in
  let nt' = unrollType nt in
  if not fromsource && not (need_cast ot' nt') then begin
    (* Do not put the cast if it is not necessary, unless it is from the
     * source. *)
    Kernel.debug ~dkey:Kernel.dkey_typing_cast "no cast to perform";
    (ot, e)
  end else begin
    let nt' = if fromsource then nt' else !typeForInsertedCast e ot' nt' in
    let result = (nt', if theMachine.insertImplicitCasts || fromsource then
                    Cil.mkCastT ~force:true ~e ~oldt:ot ~newt:nt' else e)
    in
    let error s =
      if fromsource then abort_context s else Kernel.fatal ~current:true s
    in
    (*  [BM] uncomment the following line to enable attributes static typing
        ignore (check_strict_attributes true ot nt  && check_strict_attributes false nt ot);*)
    Kernel.debug ~dkey:Kernel.dkey_typing_cast
      "@[castTo: ot=%a nt=%a\n  result is %a@\n@]"
      Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
      Cil_printer.pp_exp (snd result);
    (* Now see if we can have a cast here *)
    match ot', nt' with
    | TNamed _, _
    | _, TNamed _ -> Kernel.fatal ~current:true "unrollType failed in castTo"
    | _, TInt(IBool,_) ->
      if is_boolean_result e then result
      else
        nt,
        Cil.mkCastT
          (constFold true
             (new_exp  ~loc:e.eloc
                (BinOp(Ne,e,Cil.integer ~loc:e.eloc 0,intType))))
          ot nt'
    | TInt(_,_), TInt(_,_) ->
      (* We used to ignore attributes on integer-integer casts. Not anymore *)
      (* if ikindo = ikindn then (nt, e) else *)
      result
    | TPtr (TFun (_,args,va,_),_), TPtr(TFun (_,args',va',_),_) ->
      (* Checks on casting from a function type into another one.
         We enforce at least the same number of arguments, and emit a warning
         if types do not match.
      *)
      if va <> va' || bigger_length_args args args' then
        error
          "conversion between function types with \
           different number of arguments:@ %a@ and@ %a"
          Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
      if not (areCompatibleTypes ot nt) then
        Kernel.warning
          ~wkey:Kernel.wkey_incompatible_types_call
          ~current:true
          "implicit conversion between incompatible function types:@ \
           %a@ and@ %a"
          Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
      result

      (* accept converting a ptr to function to/from a ptr to void, even though
         not really accepted by the standard. gcc supports it. though
      *)
    | TPtr (TFun _,_), TPtr (TVoid _, _) -> result
    | TPtr (TVoid _, _), TPtr (TFun _,_) -> result
    (* Taking numerical address or calling an absolute location. Also
       accepted by gcc albeit with a warning. *)
    | TInt _, TPtr (TFun _, _) -> result
    | TPtr (TFun _, _), TInt _ -> result

    (* pointer to potential function type. Note that we do not
       use unrollTypeDeep above in order to avoid needless divergence with
       original type in the sources.
     *)
    | TPtr(TFun _,_), TPtr(TNamed(ti,nattr),pattr) ->
        castTo
          ~fromsource ot (TPtr (Cil.typeAddAttributes nattr ti.ttype, pattr)) e
    | TPtr(TNamed(ti,nattr),pattr), TPtr(TFun _,_) ->
        castTo
          ~fromsource (TPtr (Cil.typeAddAttributes nattr ti.ttype, pattr)) nt e

    (* No other conversion implying a pointer to function
       and a pointer to object are supported. *)
    | TPtr (TFun _,_), TPtr _ ->
      Kernel.warning
        ~wkey:Kernel.wkey_incompatible_pointer_types
        ~current:true
        "casting function to %a" Cil_printer.pp_typ nt;
      result
    | TPtr _, TPtr (TFun _,_) ->
      Kernel.warning
        ~wkey:Kernel.wkey_incompatible_pointer_types
        ~current:true
        "casting function from %a" Cil_printer.pp_typ ot;
      result
    | _, TPtr (TFun _, _) ->
      error "cannot cast %a to function type" Cil_printer.pp_typ ot
    | TPtr _, TPtr _ -> result

    | TInt _, TPtr _ -> result

    | TPtr _, TInt _ -> result

    | TArray _, TPtr _ -> result

    | TArray(t1,_,_,_), TArray(t2,None,_,_)
      when Cil_datatype.Typ.equal t1 t2 -> (nt', e)

    | TPtr _, TArray(_,_,_,_) ->
      error "Cast over a non-scalar type %a" Cil_printer.pp_typ nt';

    | TEnum _, TInt _ -> result
    | TFloat _, (TInt _|TEnum _) -> result
    | (TInt _|TEnum _), TFloat _ -> result
    | TFloat _, TFloat _ -> result
    | TInt (ik,_), TEnum (ei,_) ->
      (match e.enode with
       | Const (CEnum { eihost = ei'})
         when ei.ename = ei'.ename && not fromsource &&
              Cil.bytesSizeOfInt ik = Cil.bytesSizeOfInt ei'.ekind
         -> (nt',e)
       | _ -> result)
    | TEnum _, TEnum _ -> result

    | TEnum _, TPtr _ -> result
    | TBuiltin_va_list _, (TInt _ | TPtr _) ->
      result

    | (TInt _ | TPtr _), TBuiltin_va_list _ ->
      Kernel.debug ~dkey:Kernel.dkey_typing_cast ~current:true
        "Casting %a to __builtin_va_list" Cil_printer.pp_typ ot ;
      result

    | TPtr _, TEnum _ ->
      Kernel.debug ~dkey:Kernel.dkey_typing_cast ~current:true
        "Casting a pointer into an enumeration type" ;
      result

    (* The expression is evaluated for its effects *)
    | (TInt _ | TEnum _ | TPtr _ ), TVoid _ ->
      Kernel.debug ~level:3
        "Casting a value into void: expr is evaluated for side effects";
      result

    (* Even casts between structs are allowed when we are only
     * modifying some attributes *)
    | TComp (comp1, _, _), TComp (comp2, _, _) when comp1.ckey = comp2.ckey ->
      result

    (** If we try to pass a transparent union value to a function
     * expecting a transparent union argument, the argument type would
     * have been changed to the type of the first argument, and we'll
     * see a cast from a union to the type of the first argument. Turn
     * that into a field access *)
    | TComp(_, _, _), _ -> begin
        match isTransparentUnion ot with
        | None ->
          Kernel.fatal ~current:true "castTo %a -> %a" 
            Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
        | Some fstfield -> begin
            (* We do it now only if the expression is an lval *)
            let e' =
              match e.enode with
              | Lval lv ->
                new_exp ~loc:e.eloc
                  (Lval (addOffsetLval (Field(fstfield, NoOffset)) lv))
              | _ ->
                Kernel.fatal ~current:true
                  "castTo: transparent union expression is not an lval: %a\n"
                  Cil_printer.pp_exp e
            in
            (* Continue casting *)
            castTo ~fromsource:fromsource fstfield.ftype nt' e'
          end
      end
    | _ ->
      error "cannot cast from %a to %a@\n" Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
  end

(* Create and cache varinfo's for globals. Starts with a varinfo but if the
 * global has been declared already it might come back with another varinfo.
 * Returns the varinfo to use (might be the old one), and an indication
 * whether the variable exists already in the environment *)
let makeGlobalVarinfo (isadef: bool) (vi: varinfo) : varinfo * bool =
  let res =
    try (* See if already defined, in the global environment. We could also
         * look it up in the whole environment but in that case we might see a
         * local. This can happen when we declare an extern variable with
         * global scope but we are in a local scope. *)
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "makeGlobalVarinfo isadef=%B vi.vname=%s(%d)"
        isadef vi.vname vi.vid;
      (* This may throw an exception Not_found *)
      let oldvi, oldloc = lookupGlobalVar vi.vname in
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "  %s(%d) already in the env at loc %a"
        vi.vname oldvi.vid Cil_printer.pp_location oldloc;
      (* It was already defined. We must reuse the varinfo. But clean up the
       * storage.  *)
      let newstorage = (** See 6.2.2 *)
        match oldvi.vstorage, vi.vstorage with
        | Extern, NoStorage when isadef -> NoStorage
          (* the case above is not strictly C standard, but will not accept
             more program and is more compatible with old implicit
             quasi-invariant that Extern == not defined. *)
        | Extern, (Extern | NoStorage) -> Extern
        | NoStorage, Extern -> if oldvi.vdefined then NoStorage else Extern
        | NoStorage, NoStorage -> NoStorage
        | Static, Extern -> Static (* 6.2.2§4 *)
        | Static, NoStorage when Cil.isFunctionType vi.vtype -> Static
        | _ ->
          if vi.vstorage != oldvi.vstorage then
            Kernel.error ~current:true
              "Inconsistent storage specification for %s. \
               Previous declaration: %a"
              vi.vname Cil_printer.pp_location oldloc;
          vi.vstorage
      in
      (* if _all_ declaration have the inline specifier, and none
         is extern we'll end up with an inline definition which must have
         a special treatment (see C11 6.7.4§7) *)
      oldvi.vinline <- oldvi.vinline && vi.vinline;
      (* If the new declaration has a section attribute, remove any
       * preexisting section attribute. This mimics behavior of gcc that is
       * required to compile the Linux kernel properly. *)
      if hasAttribute "section" vi.vattr then
        oldvi.vattr <- dropAttribute "section" oldvi.vattr;
      (* Before combining attributes, we need to check compatibility between
         qualifiers *)
      begin
        try
          let oldquals = get_qualifiers oldvi.vtype in
          let quals = get_qualifiers vi.vtype in
          if not (Cil_datatype.Attributes.equal oldquals quals) then
            raise (Cannot_combine
                     (Format.asprintf
                        "different qualifiers:@ '%a' and '%a'"
                        Cil_printer.pp_attributes oldquals
                        Cil_printer.pp_attributes quals));
          (* Union the attributes *)
          oldvi.vattr <- cabsAddAttributes oldvi.vattr vi.vattr;
          let what =
            if isadef then
              CombineFundef (hasAttribute "FC_OLDSTYLEPROTO" vi.vattr)
            else CombineOther
          in
          let mytype = combineTypes what oldvi.vtype vi.vtype in
          if not (Cil_datatype.Typ.equal oldvi.vtype vi.vtype)
          then begin
            DifferentDeclHook.apply (oldvi,vi);
            (* note: combineTypes is (purposedly) not very strict, so we
               use compatibleTypes here to perform more strict checks and
               raise Cannot_combine if necessary. However, due to old-style
               prototypes in GCC machdeps, we must support eccentric cases,
               for which we perform no such additional verification. *)
            if not (hasAttribute "FC_OLDSTYLEPROTO" vi.vattr) then
              ignore (compatibleTypes oldvi.vtype vi.vtype)
          end;
          Cil.update_var_type oldvi mytype;
        with Cannot_combine reason ->
          Kernel.debug ~dkey:Kernel.dkey_typing_global
            "old type = %a\nnew type = %a\n"
            Cil_printer.pp_typ oldvi.vtype
            Cil_printer.pp_typ vi.vtype ;
          Kernel.error ~once:true ~current:true
            "Declaration of %s does not match previous declaration from \
 %a (%s)."
            vi.vname Cil_printer.pp_location oldloc reason;
          IncompatibleDeclHook.apply (oldvi,vi,reason)
      end;
      (* Update the storage and vdecl if useful. Do so only after the hooks have
         been applied, as they may need to read those fields *)
      if oldvi.vstorage <> newstorage then begin
        oldvi.vstorage <- newstorage;
        (* Also update the location; [vi.vdecl] is a better
           declaration/definition site for [vi]. *)
        oldvi.vdecl <- vi.vdecl;
      end;
      (* Let's mutate the formals vid's name attribute and type for function
         prototypes. Logic specifications refer to the varinfo in this table. *)
      begin
        match vi.vtype with
        | TFun (_,Some formals , _, _ ) ->
          (try
             let old_formals_env = getFormalsDecl oldvi in
             List.iter2
               (fun old (name,typ,attr) ->
                 if name <> "" then begin
                   Kernel.debug ~dkey:Kernel.dkey_typing_global
                     "replacing formal %s with %s" old.vname name;
                   old.vname <- name;
                   if not oldvi.vdefined || isadef then begin
                     Cil.update_var_type old typ;
                     old.vattr <- attr;
                   end;
                   (match old.vlogic_var_assoc with
                   | None -> ()
                   | Some old_lv -> old_lv.lv_name <- name)
                 end)
               old_formals_env
               formals;
           with
           | Invalid_argument _ ->
             abort_context "Inconsistent formals" ;
           | Not_found ->
             Cil.setFormalsDecl oldvi vi.vtype)
        | _ -> ()
      end ;
      (* if [isadef] is true, [vi] is a definition.  *)
      if isadef then begin
        (* always favor the location of the definition.*)
        oldvi.vdecl <- vi.vdecl;
        oldvi.vdefined <- true;
      end;
      (* notice that [vtemp] is immutable, and cannot be updated. Hopefully,
         temporaries have sufficiently fresh names that this is not a problem *)
      oldvi, true
    with Not_found -> begin (* A new one.  *)
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "  %s not in the env already" vi.vname;
      (* Announce the name to the alpha conversion table. This will not
       * actually change the name of the vi. See the definition of
       * alphaConvertVarAndAddToEnv *)
      let vi = alphaConvertVarAndAddToEnv true vi in
      (* update the field [vdefined] *)
      if isadef then vi.vdefined <- true;
      vi.vattr <- dropAttribute "FC_OLDSTYLEPROTO" vi.vattr;
      vi.vattr <- fc_stdlib_attribute vi.vattr;
      vi, false
    end
  in
  NewGlobalHook.apply res;
  res

type args_or_argtypes = Args of varinfo list | ArgTypes of typ list

(* Register a builtin function *)
let setupBuiltin ?(force_keep=false) name ?spec (resTyp, args_or_argtypes, isva) =
  let funargs, args = match args_or_argtypes with
    | Args args ->
      Some (List.map (fun vi -> (vi.vname, vi.vtype, vi.vattr)) args), args
    | ArgTypes argTypes ->
      let funargs = List.map (fun at -> ("", at, [])) argTypes in
      Some funargs, List.map makeFormalsVarDecl funargs
  in
  let typ = TFun(resTyp, funargs, isva, []) in
  let v = makeGlobalVar name typ in
  ignore (alphaConvertVarAndAddToEnv true v);
  (* Add it to the file as well *)
  let funspec = match spec with
    | None -> empty_funspec ()
    | Some s -> s
  in
  cabsPushGlobal (GFunDecl (funspec, v, Cil.builtinLoc));
  Cil.unsafeSetFormalsDecl v args;
  if force_keep then
    v.vattr <- Cil.addAttribute (Attr ("FC_BUILTIN",[])) v.vattr;
  v
;;

let memoBuiltin ?force_keep ?spec name proto =
  try fst (lookupGlobalVar name)
  with Not_found -> setupBuiltin ?force_keep ?spec name proto

let vla_alloc_fun () =
  let size_arg =
    Cil.makeVarinfo false true "size" theMachine.typeOfSizeOf
  in
  let res_iterm =
    Logic_const.new_identified_term
      (Logic_const.tresult Cil.voidPtrType)
  in
  let behavior =
    Cil.mk_behavior ~assigns:(Writes [(res_iterm, From [])])
      ~allocation:(FreeAlloc ([], [res_iterm])) ()
  in
  let spec = { (Cil.empty_funspec ()) with spec_behavior = [behavior]} in
  memoBuiltin ~force_keep:true "__fc_vla_alloc" ~spec
    (voidPtrType, Args [size_arg], false)

let vla_free_fun () =
  let p_arg = Cil.makeVarinfo false true "p" voidPtrType in
  let p_iterm = Logic_const.new_identified_term
      (Logic_const.tvar (Cil.cvar_to_lvar p_arg))
  in
  let behavior =
    Cil.mk_behavior ~assigns:(Writes [])
      ~allocation:(FreeAlloc ([p_iterm], [])) ()
  in
  let spec = { (Cil.empty_funspec ()) with spec_behavior = [behavior]} in
  memoBuiltin ~force_keep:true ~spec "__fc_vla_free"
    (voidType, Args [p_arg], false)

let conditionalConversion (t2: typ) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    match unrollType t2, unrollType t3 with
    | (TInt _ | TEnum _ | TFloat _), (TInt _ | TEnum _ | TFloat _) ->
      arithmeticConversion t2 t3
    | TComp (comp2,_,_), TComp (comp3,_,_)
      when comp2.ckey = comp3.ckey -> t2
    | TPtr(_, _), TPtr(TVoid _, _) -> t2
    | TPtr(TVoid _, _), TPtr(_, _) -> t3
    | TPtr _, TPtr _ when Cil_datatype.Typ.equal t2 t3 -> t2
    | TPtr _, TInt _  -> t2 (* most likely comparison with 0 *)
    | TInt _, TPtr _ -> t3 (* most likely comparison with 0 *)

    (* When we compare two pointers of different types, we combine them
     * using the same algorithm when combining multiple declarations of
     * a global *)
    | (TPtr _) as t2', (TPtr _ as t3') -> begin
        try combineTypes CombineOther t2' t3'
        with Cannot_combine msg -> begin
            Kernel.warning ~current:true "A.QUESTION: %a does not match %a (%s)"
              Cil_printer.pp_typ (unrollType t2) Cil_printer.pp_typ (unrollType t3) msg;
            t2 (* Just pick one *)
          end
      end
    | _, _ ->
      Kernel.fatal ~current:true "invalid implicit conversion from %a to %a"
        Cil_printer.pp_typ t2 Cil_printer.pp_typ t3
  in
  tresult

let logicConditionalConversion t1 t2 =
  match unrollType t1, unrollType t2 with
  | TPtr _ , TInt _ | TInt _, TPtr _ ->
    Kernel.fatal ~current:true "invalid implicit conversion from %a to %a"
      Cil_printer.pp_typ t2 Cil_printer.pp_typ t1
  | _ -> conditionalConversion t1 t2

(* Some utilities for doing initializers *)

type preInit =
  | NoInitPre
  | SinglePre of exp * Cil_datatype.Lval.Set.t (* lval reads by the expression*)
  | CompoundPre of int ref (* the maximum used index *)
                   * preInit array ref (* an array with initializers *)

(* internal pretty-printing function for debugging purposes *)
let rec _pp_preInit fmt = function
  | NoInitPre -> Format.fprintf fmt "NoInitPre"
  | SinglePre (e,_) -> Format.fprintf fmt "SinglePre(%a)" Cil_printer.pp_exp e
  | CompoundPre (int_ref, preInit_a_ref) ->
    Format.fprintf fmt "CompoundPre(%d,@[%a@])" !int_ref
      (Pretty_utils.pp_array ~sep:",@ "
         (fun fmt index e -> Format.fprintf fmt "@[[%d -> %a]@]" index _pp_preInit e))
      !preInit_a_ref

(* special case for treating GNU extension on empty compound initializers. *)
let empty_preinit() =
  if Cil.gccMode () || Cil.msvcMode () then
    CompoundPre (ref (-1), ref [| |])
  else abort_context "empty initializers only allowed for GCC/MSVC"

(* Set an initializer *)
let rec setOneInit this o preinit =
  match o with
  | NoOffset -> preinit
  | _ ->
    let idx, (* Index in the current comp *)
        restoff (* Rest offset *) =
      match o with
      | Index({enode = Const(CInt64(i,_,_))}, off) -> Integer.to_int i, off
      | Field (f, off) ->
        (* Find the index of the field *)
        let rec loop (idx: int) = function
          | [] ->
            (* We have managed to build a fieldinfo whose fcomp field is a
               compinfo that does not include the corresponding field. This
               is not a typechecking error, but an internal failure of cabs2cil
            *)
            Kernel.fatal ~current:true
              "Cannot find field %s for initialization of type %s"
              f.fname (Cil.compFullName f.fcomp)
          | f' :: _ when f'.fname = f.fname -> idx
          | _ :: restf -> loop (idx + 1) restf
        in
        loop 0 f.fcomp.cfields, off
      | _ -> abort_context "setOneInit: non-constant index"
    in
    let pMaxIdx, pArray =
      match this  with
      | NoInitPre  -> (* No initializer so far here *)
        ref idx, ref (Array.make (max 32 (idx + 1)) NoInitPre)

      | CompoundPre (pMaxIdx, pArray) ->
        if !pMaxIdx < idx then begin
          pMaxIdx := idx;
          (* Maybe we also need to grow the array *)
          let l = Array.length !pArray in
          if l <= idx then begin
            let growBy = max (max 32 (idx + 1 - l)) (l / 2) in
            let newarray = Array.make (growBy + idx) NoInitPre in
            Array.blit !pArray 0 newarray 0 l;
            pArray := newarray
          end
        end;
        pMaxIdx, pArray
      | SinglePre _ ->
        Kernel.fatal ~current:true "Index %d is already initialized" idx
    in
    assert (idx >= 0 && idx < Array.length !pArray);
    let this' = setOneInit !pArray.(idx) restoff preinit in
    !pArray.(idx) <- this';
    CompoundPre (pMaxIdx, pArray)

(* collect a CIL initializer, given the original syntactic initializer
 * 'preInit'; this returns a type too, since initialization of an array
 * with unspecified size actually changes the array's type
 * (ANSI C, 6.7.8, para 22).
 * Finally, we return the set of lvals that are read for the evaluation of
 * the initializer (for unspecified sequences)
 *)
let rec collectInitializer
    reads (* lval already read by the rest of the initializer. *)
    (this: preInit)
    (thistype: typ) ~(parenttype: typ) :
  (init * typ * Cil_datatype.Lval.Set.t) =
  (* parenttype is used to identify a tentative flexible array member
     initialization *)
  let dkey = Kernel.dkey_typing_init in
  let loc = CurrentLoc.get() in
  if this = NoInitPre then begin
    Kernel.debug ~dkey "zero-initializing object of type %a"
      Cil_printer.pp_typ thistype;
    (makeZeroInit ~loc thistype), thistype, reads
  end else
    match unrollType thistype, this with
    | _ , SinglePre (e, r) ->
      Kernel.debug ~dkey "Initializing object of type %a to %a"
        Cil_printer.pp_typ thistype Cil_printer.pp_exp e;
      SingleInit e, thistype, Cil_datatype.Lval.Set.union r reads
    | TArray (bt, leno, _, at), CompoundPre (pMaxIdx, pArray) ->
      Kernel.debug ~dkey
        "Initialization of an array object of type %a with index max %d"
        Cil_printer.pp_typ thistype !pMaxIdx;
      let len, initializer_len_used =
        (* normal case: use array's declared length, newtype=thistype *)
        match leno with
        | Some len -> begin
            match constFoldToInt len with
            | Some ni when Integer.ge ni Integer.zero ->
              (Integer.to_int ni), false
            | _ ->
              Kernel.fatal ~current:true
                "Array length is not a constant expression %a"
                Cil_printer.pp_exp len
          end
        | _ ->
          (* unsized array case, length comes from initializers *)
          (!pMaxIdx + 1), true
      in
      if !pMaxIdx >= len then
        abort_context
          "collectInitializer: too many initializers(%d >= %d)"
          (!pMaxIdx+1) len;
(*
        (* len could be extremely big. So omit the last initializers, if they
         * are many (more than 16). doInit will take care of that by
         * mem-setting everything to 0 in that case.
         *)
        let endAt =
          if len - 1 > !pMaxIdx + 16 then
            !pMaxIdx
          else
            len - 1
        in
        (* Make one zero initializer to be used next *)
        let oneZeroInit = makeZeroInit ~loc bt in
        let rec collect (acc: (offset * init) list) (idx: int) =
          if idx = -1 then acc
          else
            let thisi =
              if idx > !pMaxIdx then oneZeroInit
              else (fst (collectInitializer !pArray.(idx) bt))
            in
            collect ((Index(integer ~loc idx,NoOffset), thisi) :: acc) (idx - 1)
        in
*)
      let collect_one_init v (idx,init,typ,reads,len_used) =
        match v with
        | NoInitPre -> (idx-1,init,typ,reads,len_used)
        | _ ->
          let (vinit,typ', reads') =
            collectInitializer reads v typ ~parenttype:typ
          in
          let len_used =
            len_used || not (Cil_datatype.Typ.equal typ typ')
          in
          (idx-1,
           (Index (integer ~loc idx,NoOffset), vinit)::init,
           typ',
           Cil_datatype.Lval.Set.union reads' reads,
           len_used)
      in
      let (_,init,typ, reads, len_used) =
        Array.fold_right collect_one_init
          !pArray (Array.length !pArray - 1, [], bt, reads,initializer_len_used)
      in
      let newtype =
        (* detect flexible array member initialization *)
        match thistype, Cil.unrollType parenttype with
        | TArray (_, None, _, _), TComp (comp, _, _)
          when comp.cstruct && len > 0 ->
          (* incomplete array type inside a struct => FAM, with
             a non-empty initializer (len > 0)
          *)
          Kernel.debug ~dkey
            "Detected initialization of a flexible array member \
             (length %d, parenttype %a)" len Cil_printer.pp_typ parenttype;
          Kernel.error ~once:true ~current:true
            "static initialization of flexible array members is an \
             unsupported GNU extension";
          TArray (typ, None, empty_size_cache (), at)
        | _ -> (* not a flexible array member *)
          if len = 0 && not (Cil.gccMode() || Cil.msvcMode ()) then
            Kernel.error ~once:true ~current:true
              "arrays of size zero not supported in C99@ \
               (only allowed as compiler extensions)";
          TArray (typ, Some (integer ~loc len), empty_size_cache (), at)
      in
      CompoundInit (newtype, (* collect [] endAt*)init),
      (* If the sizes of the initializers have not been used anywhere,
         we can fold back an eventual typedef. Otherwise, push the
         attributes to the elements of the array *)
      (if len_used then newtype else thistype),
      reads

    | TComp (comp, _, _), CompoundPre (pMaxIdx, pArray) when comp.cstruct ->
      Kernel.debug ~dkey
        "Initialization of an object of type %a with at least %d components"
        Cil_printer.pp_typ thistype !pMaxIdx;
      let rec collect (idx: int) reads = function
          [] -> [], reads
        | f :: restf ->
          if f.fname = missingFieldName then
            collect (idx + 1) reads restf
          else
            let thisi, reads' =
              if idx > !pMaxIdx then
                makeZeroInit ~loc f.ftype, reads
              else
                collectFieldInitializer
                  reads !pArray.(idx) f ~parenttype:thistype
            in
            let rest, reads' = collect (idx+1) reads' restf in
            (Field(f, NoOffset), thisi) :: rest, reads'
      in
      let init, reads = collect 0 reads comp.cfields in
      CompoundInit (thistype, init), thistype, reads

    | TComp (comp, _, _), CompoundPre (pMaxIdx, pArray) when not comp.cstruct ->
      Kernel.debug ~dkey
        "Initialization of an object of type %a with at least %d components"
        Cil_printer.pp_typ thistype !pMaxIdx;
      (* Find the field to initialize *)
      let rec findField (idx: int) = function
        | [] -> abort_context "collectInitializer: union"
        | _ :: rest when idx < !pMaxIdx && !pArray.(idx) = NoInitPre ->
          findField (idx + 1) rest
        | f :: _ when idx = !pMaxIdx ->
          let init, reads =
            collectFieldInitializer reads !pArray.(idx) f ~parenttype:thistype
          in
          (Field(f, NoOffset), init), reads

        | _ ->
          Kernel.fatal ~current:true "Can initialize only one field for union"
      in
      if Cil.msvcMode () && !pMaxIdx != 0 then
        Kernel.warning ~current:true
          "On MSVC we can initialize only the first field of a union";
      let init, reads = findField 0 comp.cfields in
      CompoundInit (thistype, [ init ]), thistype, reads

    | _ -> Kernel.fatal ~current:true "collectInitializer"

and collectFieldInitializer
    reads
    (this: preInit)
    (f: fieldinfo) ~(parenttype: typ) =
  (* collect, and rewrite type *)
  let init,newtype,reads =
    (collectInitializer reads this f.ftype ~parenttype)
  in
  f.ftype <- newtype;
  init, reads

type stackElem =
    InArray of offset * typ * int * int ref (* offset of parent, base type,
                                             * length, current index. If the
                                             * array length is unspecified we
                                             * use Int.max_int  *)
  | InComp  of offset * compinfo * offset list (* offset of parent,
                                                     base comp, current fields *)


(* A subobject is given by its address. The address is read from the end of
 * the list (the bottom of the stack), starting with the current object *)
type subobj = { mutable stack: stackElem list; (* With each stack element we
                                                * store the offset of its
                                                * PARENT  *)
                mutable eof: bool; (* The stack is empty and we reached the
                                    * end *)
                mutable soTyp: typ; (* The type of the subobject. Set using
                                     * normalSubobj after setting stack. *)
                mutable soOff: offset; (* The offset of the subobject. Set
                                        * using normalSubobj after setting
                                        * stack.  *)
                curTyp: typ; (* Type of current object. See ISO for
                              * the definition of the current object *)
                curOff: offset; (* The offset of the current obj *)
                host: varinfo; (* The host that we are initializing.
                                * For error messages *)
              }

(* maps vid to visitor used to perform renaming on function spec when there's
   a spec on a declaration and a definition for the function. This is done after
   typing.
*)
let alpha_renaming = Hashtbl.create 59

let rename_spec = function
  | GFunDecl(spec,v,_) ->
    (try
       let alpha = Hashtbl.find alpha_renaming v.vid in
       ignore (Cil.visitCilFunspec alpha spec)
     with Not_found -> ())
  | _ -> ()

(* Make a subobject iterator *)
let rec makeSubobj
    (host: varinfo)
    (curTyp: typ)
    (curOff: offset) =
  let so =
    { host = host; curTyp = curTyp; curOff = curOff;
      stack = []; eof = false;
      (* The next are fixed by normalSubobj *)
      soTyp = voidType; soOff = NoOffset } in
  normalSubobj so;
  so

(* Normalize a stack so the we always point to a valid subobject. Do not
 * descend into type *)
and normalSubobj (so: subobj) : unit =
  match so.stack with
  | [] -> so.soOff <- so.curOff; so.soTyp <- so.curTyp
  (* The array is over *)
  | InArray (parOff, bt, leno, current) :: rest ->
    if leno = !current then begin (* The array is over *)
      Kernel.debug ~dkey:Kernel.dkey_typing_init "Past the end of array";
      so.stack <- rest;
      advanceSubobj so
    end else begin
      so.soTyp <- bt;
      so.soOff <-
        addOffset
          (Index(integer ~loc:(CurrentLoc.get()) !current, NoOffset))
          parOff
    end

  (* The fields are over *)
  | InComp (parOff, compinfo, nextflds) :: rest ->
    if nextflds == [] then begin (* No more fields here *)
      Kernel.debug ~dkey:Kernel.dkey_typing_init "Past the end of structure";
      so.stack <- rest;
      advanceSubobj so
    end else begin
      let fst = List.hd nextflds
      and baseTyp = TComp (compinfo,empty_size_cache (), []) in
      so.soTyp <- Cil.typeOffset baseTyp fst;
      so.soOff <- addOffset fst parOff
    end

(* Advance to the next subobject. Always apply to a normalized object *)
and advanceSubobj (so: subobj) : unit =
  if so.eof then abort_context "advanceSubobj past end";
  match so.stack with
  | [] ->
    Kernel.debug ~dkey:Kernel.dkey_typing_init "Setting eof to true";
    so.eof <- true
  | InArray (_, _, _, current) :: _ ->
    Kernel.debug ~dkey:Kernel.dkey_typing_init
      "  Advancing to [%d]" (!current + 1);
    (* so.stack <- InArray (parOff, bt, leno, current + 1) :: rest; *)
    incr current;
    normalSubobj so

  (* The fields are over *)
  | InComp (parOff, comp, nextflds) :: rest ->
    let fi, flds' =
      match nextflds with
      | Field (fi,_) :: flds' -> fi, flds'
      | _ -> abort_context "advanceSubobj"
    in
    Kernel.debug ~dkey:Kernel.dkey_typing_init
      "Advancing past .%s" fi.fname;
    so.stack <- InComp(parOff, comp, flds') :: rest;
    normalSubobj so

let anonCompFieldNameId = ref 0
let anonCompFieldName = "__anonCompField"

(* Find the fields to initialize in a composite. *)
let fieldsToInit
    (comp: compinfo)
    (designator: string option)
  : offset list =
  (* Traversal of the comp fields (also goes through anonymous comp)
     the resulting fields are in reverse order *)
  let rec add_comp (offset : offset) (comp : compinfo) acc =
    let in_union = not comp.cstruct in
    add_fields offset in_union comp.cfields acc
  and add_fields (offset : offset) (in_union : bool) (l : fieldinfo list) acc =
    match l with
    | [] -> acc
    | f :: l ->
      let (found, _ as acc) = add_field offset f acc in
      if found && in_union
      then acc (* only consider one field in an union - stop if we found it *)
      else add_fields offset in_union l acc
  and add_field (offset : offset) (f : fieldinfo) (found, loff as acc) =
    (* update current offset *)
    let offset = Cil.addOffset (Field (f, NoOffset)) offset in
    (* if this field is an anonymous comp *)
    if prefix anonCompFieldName f.fname then
      match unrollType f.ftype with
      | TComp (comp, _, _) ->
        add_comp offset comp acc (* go deeper inside *)
      | _ ->
        abort_context "unnamed field type is not a struct/union"
    (* if this field is an anonymous field but not a comp *)
    else if f.fname = missingFieldName then
      acc (* Ignore anonymous non-comp fields *)
    (* if we have already found the designator, just append the current field *)
    else if found then
      found, offset :: loff
   (* we didn't find the designator yet, does this field match ? *)
    else match designator with
      | Some fn when f.fname = fn -> (true, [offset])
      | _ -> acc
  in
  let found, r = add_comp NoOffset comp (designator = None, []) in
  begin if not found then
    let fn = Extlib.the designator in
    Kernel.fatal ~current:true "Cannot find designated field %s" fn;
  end;
  List.rev r

let integerArrayLength (leno: exp option) : int =
  match leno with
  | None -> max_int
  | Some len ->
    try lenOfArray leno
    with LenOfArray ->
      Kernel.fatal ~current:true
        "Initializing non-constant-length array with length=%a"
        Cil_printer.pp_exp len

let find_field_offset cond (fidlist: fieldinfo list) : offset =
  (* Depth first search for the field. This appears to be what GCC does.
   * MSVC checks that there are no ambiguous field names, so it does not
   * matter how we search *)
  let rec search = function
      [] -> raise Not_found
    | fid :: _ when cond fid ->
      Field(fid, NoOffset)
    | fid :: rest when prefix anonCompFieldName fid.fname -> begin
        match unrollType fid.ftype with
        | TComp (ci, _, _) ->
          (try let off = search ci.cfields in Field(fid,off)
           with Not_found -> search rest  (* Continue searching *))
        | _ ->
          abort_context "unnamed field type is not a struct/union"
      end
    | _ :: rest -> search rest
  in
  search fidlist

let findField n comp =
  try
    find_field_offset (fun x -> x.fname = n) comp.cfields
  with Not_found ->
    abort_context "Cannot find field %s in type %s" n (Cil.compFullName comp)

(* Utility ***)
let rec replaceLastInList
    (lst: A.expression list)
    (how: A.expression -> A.expression) : A.expression list=
  match lst with
  | [] -> []
  | [e] -> [how e]
  | h :: t -> h :: replaceLastInList t how

let convBinOp (bop: A.binary_operator) : binop =
  match bop with
  | A.ADD -> PlusA
  | A.SUB -> MinusA
  | A.MUL -> Mult
  | A.DIV -> Div
  | A.MOD -> Mod
  | A.BAND -> BAnd
  | A.BOR -> BOr
  | A.XOR -> BXor
  | A.SHL -> Shiftlt
  | A.SHR -> Shiftrt
  | A.EQ -> Eq
  | A.NE -> Ne
  | A.LT -> Lt
  | A.LE -> Le
  | A.GT -> Gt
  | A.GE -> Ge
  | _ -> Kernel.fatal ~current:true "convBinOp"

(**** PEEP-HOLE optimizations ***)

(* Should we collapse [tmp = f(); lv = tmp;] where the result type of [f]
   is [tf], and the [lv] has type [tlv *)
let allow_return_collapse ~tlv ~tf =
  Cil_datatype.Typ.equal tlv tf ||
  Kernel.DoCollapseCallCast.get () &&
  (match Cil.unrollType tlv, Cil.unrollType tf with
   | TPtr _, TPtr _ -> true (* useful for malloc and others. Could be
                                restricted to void* -> any if needed *)
   | TInt (iklv, _), TInt (ikf, _) ->
     Cil.isSigned iklv = Cil.isSigned ikf &&
     Cil.bitsSizeOfBitfield tlv = Cil.bitsSizeOf tf (* && *)
     (* not (Cil.typeHasQualifier "volatile" tlv) *)
   | TFloat (fklv, _), TFloat (fkf, _) -> fklv = fkf
   | _, _ -> false
  )

let tcallres f =
  match unrollType (typeOf f) with
  | TFun (rt, _, _, _) -> rt
  | _ -> abort_context "Function call to a non-function"

let can_collapse vi vi' destlv cast f =
  let tf = tcallres f in
  not vi.vglob && vi' == vi &&
  String.length vi.vname >= 3 &&
  (* Watch out for the possibility that we have an implied cast in
   * the call *)
  IH.mem callTempVars vi.vid &&
  Cil_datatype.Typ.equal cast (typeOfLval destlv) &&
  (* Depending on circumstances, temp var might either have the type of
     the destination variable or the returned type of f. We collapse in both
     cases. *)
  (Cil_datatype.Typ.equal vi.vtype cast ||
   Cil_datatype.Typ.equal vi.vtype tf)
  &&
  allow_return_collapse ~tf ~tlv:cast

let collapseCallCast (s1,s2) = match s1.skind, s2.skind with
  | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
    Instr (Set(destlv,
               {enode = CastE (newt,
                               {enode = Lval(Var vi', NoOffset)})}, _)) ->
    if can_collapse vi vi' destlv newt f then begin
      s1.skind <- Instr(Call(Some destlv, f, args, l));
      Some [ s1 ]
    end
    else None
  | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
    Instr (Set(destlv, {enode = Lval(Var vi', NoOffset)}, _)) ->
    if can_collapse vi vi' destlv (typeOfLval destlv) f then begin
      s1.skind <- Instr(Call(Some destlv, f, args, l));
      Some [ s1 ]
    end else None
  | Instr (Call (Some (Var vi, NoOffset),
                 ({ enode = Lval (Var f, NoOffset)} as ef), args, l)),
    Instr (
      Local_init(
        destv,
        AssignInit(
          SingleInit
            { enode = CastE(newt, { enode = Lval(Var vi', NoOffset)})}),_))->
    if can_collapse vi vi' (Cil.var destv) newt ef then begin
      s1.skind <- Instr(Local_init(destv, ConsInit(f,args,Plain_func),l));
      Some [s1]
    end else None
  | Instr (Call (Some (Var v1, NoOffset),
                 ({ enode = Lval (Var f, NoOffset)} as ef), args, l)),
    Instr (
      Local_init(
        v2, AssignInit(SingleInit { enode = Lval (Var v1', NoOffset) }),_)) ->
    if can_collapse v1 v1' (Cil.var v2) v2.vtype ef then begin
      s1.skind <- Instr(Local_init(v2, ConsInit(f,args,Plain_func),l));
      Some [ s1 ];
    end else None
  | _ -> None

let afterConversion ~ghost (c: chunk) : chunk =
  (* Now scan the statements and find Instr blocks *)
  (** We want to collapse sequences of the form "tmp = f(); v = tmp". This
      * will help significantly with the handling of calls to malloc, where it
      * is important to have the cast at the same place as the call *)
  let block = c2block ~ghost ~collapse_block:false c in
  let sl =
    if Kernel.DoCollapseCallCast.get () then
      peepHole2 ~aggressive:false collapseCallCast block.bstmts
    else block.bstmts
  in
  (* the call to c2block has taken care of a possible unspecified sequence.
     We do not need to keep track of effects at this level. *)
  let res =
    { c with stmts = (List.rev_map (fun x -> x,[],[],[],[]) sl); }
  in
  (*  Format.eprintf "Before conversion@\n%a@\nAfter conversion@\n%a@\n@."
      d_chunk c d_chunk res;
  *)
  res

(***** Try to suggest a name for the anonymous structures *)
let suggestAnonName (nl: A.name list) =
  match nl with
  | [] -> ""
  | (n, _, _, _) :: _ -> n


(** Optional constant folding of binary operations *)
let optConstFoldBinOp loc machdep bop e1 e2 t =
  if theMachine.lowerConstants then
    constFoldBinOp ~loc machdep bop e1 e2 t
  else
    new_exp ~loc (BinOp(bop, e1, e2, t))

let integral_cast ty t =
  raise
    (Failure
       (Format.asprintf "term %a has type %a, but %a is expected."
          Cil_printer.pp_term t Cil_printer.pp_logic_type Linteger Cil_printer.pp_typ ty))

(* Exception raised by the instance of Logic_typing local to this module.
   See document of [error] below. *)
exception LogicTypeError of location * string

module C_logic_env =
struct
  let nb_loop = ref 0
  let is_loop () = !nb_loop > 0
  let anonCompFieldName = anonCompFieldName
  let conditionalConversion = logicConditionalConversion
  let find_macro _ = raise Not_found
  let find_var x = match H.find env x with
    | EnvVar vi, _ -> cvar_to_lvar vi
    | _ -> raise Not_found
  let find_enum_tag x = match H.find env x with
    | EnvEnum item,_ ->
      dummy_exp (Const (CEnum item)), typeOf item.eival
    | _ -> raise Not_found

  let find_comp_field info s = findField s info

  let find_type namespace s =
    match namespace with
    | Logic_typing.Typedef -> let t,_ = lookupTypeNoError "type" s in t
    | Logic_typing.Union -> findCompType "union" s []
    | Logic_typing.Struct -> findCompType "struct" s []
    | Logic_typing.Enum -> findCompType "enum" s []

  include Logic_labels

  include Logic_env

  let add_logic_function =
    add_logic_function_gen Logic_utils.is_same_logic_profile

  let remove_logic_info =
    remove_logic_info_gen Logic_utils.is_same_logic_profile

  let integral_cast = integral_cast

  (* This function raises a non-recoverable when [-continue-annot-error] is not
     set, and [LogicTypeError] otherwise. This exception must *not* escape
     Cabs2cil. Hence, each call to a function of module [Ltyping] below must
     catch it. *)
  let error loc msg =
    Pretty_utils.ksfprintf (fun e -> raise (LogicTypeError (loc,e))) msg

  let on_error f rollback x =
    try f x with LogicTypeError _ as exn -> rollback(); raise exn

end

module Ltyping = Logic_typing.Make (C_logic_env)

let startLoop iswhile =
  incr C_logic_env.nb_loop;
  continues :=
    (if iswhile then While (ref "") else NotWhile (ref "")) :: !continues;
  enter_break_env ()

let exitLoop () =
  decr C_logic_env.nb_loop;
  exit_break_env ();
  match !continues with
  | [] -> Kernel.error ~once:true ~current:true "exit Loop not in a loop"
  | _ :: rest -> continues := rest

let enterScope () =
  scopes := (ref []) :: !scopes;
  C_logic_env.enter_scope ()

(* Exit a scope and clean the environment. We do not yet delete from
 * the name table *)
let exitScope () =
  let this, rest = match !scopes with
    | [] -> Kernel.fatal ~current:true "Not in a scope"
    | car :: cdr -> car, cdr
  in
  scopes := rest;
  let rec loop = function
      [] -> ()
    | UndoRemoveFromEnv n :: t ->
      H.remove env n; loop t
    | UndoRemoveFromAlphaTable (p,i) :: t ->
      (try
         let h = H.find alphaTable p in
         H.remove h i;
         if H.length h = 0 then H.remove alphaTable p
       with Not_found ->
         Kernel.warning
           "prefix (%s,%s) not in alpha conversion table. \
            undo stack is inconsistent"
           p i); loop t
    | UndoResetAlphaCounter (vref, oldv) :: t ->
      vref := oldv;
      loop t
  in
  loop !this;
  C_logic_env.exit_scope ()

let consLabel ~ghost (l: string) (c: chunk) (loc: location)
    (in_original_program_text : bool) : chunk =
  (* Get the first statement and add the label to it *)
  let labstmt, stmts' = getFirstInChunk ~ghost ~loc c in
  (* Add the label *)
  add_label l labstmt;
  labstmt.labels <- Label (l, loc, in_original_program_text) ::
                    labstmt.labels;
  if c.stmts == stmts' then c else {c with stmts = stmts'}

let consLabContinue ~ghost (c: chunk) =
  match !continues with
  | [] -> Kernel.fatal ~current:true "labContinue not in a loop"
  | While lr :: _ ->
    begin
      assert (!doTransformWhile);
      if !lr = "" then c else consLabel ~ghost !lr c (CurrentLoc.get ()) false
    end
  | NotWhile lr :: _ ->
    if !lr = "" then c else consLabel ~ghost !lr c (CurrentLoc.get ()) false

(* Was a continue instruction used inside the current loop *)
let continueUsed () =
  match !continues with
  | [] -> Kernel.fatal ~current:true "not in a loop"
  | (While lr | NotWhile lr) :: _ -> !lr <> ""


(****** TYPE SPECIFIERS *******)

(* JS: return [Some s] if the attribute string is the attribute annotation [s]
   and [None] if it is not an annotation. *)
let attrAnnot s =
  let r = Str.regexp "/\\*@ \\(.+\\) \\*/" in
  if Str.string_match r s 0 then
    try Some (Str.matched_group 1 s) with Not_found -> assert false
  else
    None


type local_env =
  { authorized_reads: Lval.Set.t;
    known_behaviors: string list;
    is_ghost: bool;
    is_paren: bool; (* true for expressions whose parent is A.PAREN *)
    inner_paren: bool
    (* used during unop/binop traversal to distinguish between
       A.PAREN (A.UNOP(...)) and A.UNOP(A.PAREN(...)) *)
  }

let empty_local_env =
  { authorized_reads = Lval.Set.empty;
    known_behaviors = [];
    is_ghost = false;
    is_paren = false;
    inner_paren = false;
  }

let ghost_local_env ghost = {empty_local_env with is_ghost = ghost }

let paren_local_env env = { env with is_paren = true }
let no_paren_local_env env = { env with is_paren = false }
let inner_paren env = { env with inner_paren = true }
let no_inner_paren env = { env with inner_paren = false }

(* weimer: Sat Dec 8 17:30:47 2001 MSVC NT kernel headers include
 * functions like long convert(x) { __asm { mov eax, x \n cdq } }
 * That set a return value via an ASM statement. As a result, I
 * am changing this so a final ASM statement does not count as
 * "fall through" for the purposes of this warning.  *)
(* matth: But it's better to assume assembly will fall through,
 * since  most such blocks do.  It's probably better to print an
 * unnecessary warning than to break CIL's invariant that
 * return statements are inserted properly.  *)
let rec compute_from_root f = function
    [] -> false

  (* We have a label, perhaps we can jump here *)
  | s :: rest when s.labels <> [] ->
    Kernel.debug ~level:4 "computeFromRoot call f from stmt %a"
      Cil_printer.pp_location (Stmt.loc s);
    f (s :: rest)

  | _ :: rest -> compute_from_root f rest

let instrFallsThrough (i : instr) = match i with
  | Local_init _ -> true
  | Set _ -> true
  | Call (None, {enode = Lval (Var e, NoOffset)}, _, _) ->
    (* See if this is exit, or if it has the noreturn attribute *)
    if e.vname = "exit" then false
    else if hasAttribute "noreturn" e.vattr then false
    else true
  | Call _ -> true
  | Asm _ -> true
  | Skip _ -> true
  | Code_annot _ -> true

let rec stmtFallsThrough (s: stmt) : bool =
  Kernel.debug ~level:4 "stmtFallsThrough stmt %a"
    Cil_printer.pp_location (Stmt.loc s);
  match s.skind with
  | Instr(il) ->
    instrFallsThrough il
  | UnspecifiedSequence seq ->
    blockFallsThrough (block_from_unspecified_sequence seq)
  | Return _ | Break _ | Continue _ | Throw _ -> false
  | Goto _ -> false
  | If (_, b1, b2, _) ->
    blockFallsThrough b1 || blockFallsThrough b2
  | Switch (_e, b, targets, _) ->
    (* See if there is a "default" case *)
    if not
        (List.exists
           (fun s ->
              List.exists (function Default _ -> true | _ -> false)
                s.labels)
           targets)
    then begin
      true (* We fall through because there is no default *)
    end else begin
      (* We must examine all cases. If any falls through,
       * then the switch falls through. *)
      blockFallsThrough b || blockCanBreak b
    end
  | Loop (_,b, _, _, _) ->
    (* A loop falls through if it can break. *)
    blockCanBreak b
  | Block b -> blockFallsThrough b
  | TryCatch (b, l, _) ->
    List.fold_left
      (fun acc (_,b) -> acc || blockFallsThrough b)
      (blockFallsThrough b) l
  | TryFinally (_b, h, _) -> blockFallsThrough h
  | TryExcept (_b, _, _h, _) -> true (* Conservative *)
and stmtListFallsThrough = function
    [] -> true
  | s :: rest ->
    if stmtFallsThrough s then begin
      stmtListFallsThrough rest
    end else begin
      (* If we are not falling through then maybe there
       * are labels who are *)
      compute_from_root stmtListFallsThrough rest
    end
and blockFallsThrough b =
  stmtListFallsThrough b.bstmts

(* will we leave this statement or block with a break command? *)
and stmtCanBreak (s: stmt) : bool =
  Kernel.debug ~level:4 "stmtCanBreak stmt %a"
    Cil_printer.pp_location (Stmt.loc s);
  match s.skind with
  | Instr _ | Return _ | Continue _ | Goto _ | Throw _ -> false
  | Break _ -> true
  | UnspecifiedSequence seq ->
    blockCanBreak (block_from_unspecified_sequence seq)
  | If (_, b1, b2, _) ->
    blockCanBreak b1 || blockCanBreak b2
  | Switch _ | Loop _ ->
    (* switches and loops catch any breaks in their bodies *)
    false
  | Block b -> blockCanBreak b
  | TryCatch (b,l,_) ->
    List.fold_left
      (fun acc (_,b) -> acc || blockCanBreak b)
      (blockCanBreak b)
      l
  | TryFinally (b, h, _) -> blockCanBreak b || blockCanBreak h
  | TryExcept (b, _, h, _) -> blockCanBreak b || blockCanBreak h
and blockCanBreak b =
  let rec aux = function
      [] -> false
    | s::tl ->
      Kernel.debug ~level:4 "blockCanBreak from stmt %a"
        Cil_printer.pp_location (Stmt.loc s);
      stmtCanBreak s ||
      (if stmtFallsThrough s then aux tl
       else compute_from_root aux tl)
  in aux b.bstmts

let chunkFallsThrough c =
  let get_stmt (s,_,_,_,_) = s in
  let stmts = List.rev_map get_stmt c.stmts in
  stmtListFallsThrough stmts

let has_local_init chunk =
  List.exists
    (fun (s,_,_,_,_) ->
       match s.skind with Instr (Local_init _) -> true | _ -> false)
    chunk.stmts

let append_chunk_to_annot ~ghost annot_chunk current_chunk =
  match current_chunk.stmts with
  | [] -> annot_chunk @@ (current_chunk, ghost)
  (* don't forget locals of current_chunk *)

  (* if we have a single statement,
     we can avoid enclosing it into a block. *)
  | [ (_s,_,_,_,_) ] ->
    (*     Format.eprintf "Statement is: %a@." d_stmt _s;  *)
    annot_chunk @@ (current_chunk, ghost)
  (* Make a block, and put labels of the first statement
     on the block itself, so as to respect scoping rules
     for \at in further annotations. *)
  | _ ->
    if has_local_init current_chunk then begin
      (* See if we can collapse the statements of the chunk into a single one.
         Otherwise, we can't handle the combination, as putting the Local_init
         into a new block would change the scope of the local variable, at
         least in the pretty-printed code. Furthermore, the usefulness of
         such annotations is dubious at best.
      *)
      let res =
        match current_chunk.stmts with
        | [(s1, m1, w1, r1, c1); (s2, m2, w2, r2, c2)] ->
          Extlib.swap
            Extlib.opt_bind
            (collapseCallCast (s2,s1)) (* the chunk list is reversed.*)
            (function
              | [ s1' ] -> Some (s1', m1 @ m2, w1 @ w2, r1 @ r2, c1 @ c2)
              | _ -> None (* should not happen. *))
        | _ -> None
      in
      match res with
      | Some s -> annot_chunk @@ ({current_chunk with stmts = [s]}, ghost)
      | None ->
        Kernel.warning ~wkey:Kernel.wkey_annot_error
          "Statement contract and ACSL pragmas over a local definition \
           are not implemented. Ignoring annotation";
        current_chunk
    end else begin
      let b = c2block ~ghost current_chunk in
      (* The statement may contain some local variable
         declarations (but no definitions) coming from userland.
         We have to shift them from the inner block, otherwise they will not
         be accessible in the next statements.
      *)
      let locals = b.blocals in
      b.blocals <- [];
      b.battrs <-
        addAttributes [Attr(frama_c_keep_block,[])] b.battrs;
      let block = mkStmt ~ghost ~valid_sid (Block b) in
      let chunk = s2c block in
      let chunk = { chunk with cases = current_chunk.cases } in
      annot_chunk @@ (List.fold_left
                        local_var_chunk chunk (List.rev locals), ghost)
    end

let default_argument_promotion idx exp =
  let name = "x_" ^ string_of_int idx in
  let arg_type = Cil.typeOf exp in
  let typ =
    match Cil.unrollType arg_type with
    | TVoid _ -> voidType
    | TInt(k,_) when Cil.rank k < Cil.rank IInt ->
      if intTypeIncluded k IInt then intType
      else (* This may happen when char or short have the same size as int *) 
        uintType
    | TInt(k,_) -> TInt(k,[])
    | TFloat(FFloat,_) -> doubleType
    | TFloat(k,_) -> TFloat(k,[])
    | TPtr(t,_) | TArray(t,_,_,_) -> TPtr(t,[])
    | (TFun _) as t -> TPtr(t,[])
    | TComp(ci,_,_) -> TComp(ci,{ scache = Not_Computed },[])
    | TEnum(ei,_) -> TEnum(ei,[])
    | TBuiltin_va_list _ ->
      abort_context "implicit prototype cannot have variadic arguments"
    | TNamed _ -> assert false (* unrollType *)
  in
  (* if we make a promotion, take it explicitly
     into account in the argument itself *)
  let (_,e) = castTo arg_type typ exp in
  (name,typ,[]), e

(* Promote variadic arguments with standard argument promotions.*)
let promote_variadic_arguments (chunk,args) = 
  let args =
    Extlib.mapi 
      (fun i arg -> snd (default_argument_promotion i arg))
      args
  in
  (chunk,args)

let rec evaluate_cond_exp = function
  | CEExp (_,e) ->
    (match Cil.constFoldToInt e with
     | None -> `CUnknown
     | Some z when Integer.is_zero z -> `CFalse
     | Some _ -> `CTrue)
  | CEAnd (e1,e2) ->
    let r = evaluate_cond_exp e1 in
    if r = `CTrue then evaluate_cond_exp e2 else r
  | CEOr(e1,e2) ->
    let r = evaluate_cond_exp e1 in
    if r = `CFalse then evaluate_cond_exp e2 else r
  | CENot e ->
    match evaluate_cond_exp e with
    | `CTrue -> `CFalse
    | `CFalse -> `CTrue
    | `CUnknown -> `CUnknown


(* The way formals are handled now might generate incorrect types, in the
   sense that they refer to a varinfo (in the case of VLA depending on a
   previously declared formal) that exists only during the call to doType.
   We replace them here with the definitive version of the formals' varinfos.
   A global refactoring of cabs2cil would be welcome, though.
*)
let fixFormalsType formals =
  let table = Hashtbl.create 5 in
  let vis =
    object
      inherit Cil.nopCilVisitor
      method! vvrbl v =
        if v.vformal then begin
          try
            ChangeTo (Hashtbl.find table v.vname)
          with Not_found ->
            Kernel.fatal "Formal %a not tied to a varinfo"
              Cil_printer.pp_varinfo v;
        end else SkipChildren
    end
  in
  let treat_one_formal v =
    v.vtype <- Cil.visitCilType vis v.vtype;
    Hashtbl.add table v.vname v;
  in
  List.iter treat_one_formal formals

(* Map from standard int type names like [uint16_t] to their expected sizes,
   and a flag whether the given size is exact (or a lower bound). That is,
   [uint16_t] maps to [(16, true)], and [uint_least16_t] to [(16, false)].
   Used by [checkTypedefSize] below. *)
let stdIntegerSizes = Hashtbl.create 5

(* Initialize the stdIntegerSizes table. *)
let initStdIntegerSizes () =
  let bases = ["int"; "uint"] in
  let sizes = [8; 16; 32; 64] in
  let add_std_type base size =
    let add_variant (variant, exact) =
      let key = base ^ variant ^ (string_of_int size) ^ "_t" in
      Hashtbl.add stdIntegerSizes key (size, exact)
    in
    (* Store exact "normal" variant, inexact "fast" and "least" variants. *)
    List.iter add_variant [("", true); ("_fast", false); ("_least", false)]
  in
  List.iter (fun b -> List.iter (add_std_type b) sizes) bases;
  (* Also store variants of [intptr_t] using the size of [void *], and
     [intmax_t] variants using the size of [long long]. *)
  let add_special_types name size =
    let add base =
      Hashtbl.add stdIntegerSizes (base ^ name ^ "_t") (size, true)
    in
    List.iter add bases
  in
  add_special_types "ptr" (Cil.bitsSizeOf Cil.voidPtrType);
  add_special_types "max" (Cil.bitsSizeOf Cil.longLongType)

(* [checkTypedefSize name typ] checks if [name] is acceptable as a typedef
   name for type [typ]. If [name] is one of the standard integer type names
   like [uint16_t] but [typ] has the wrong bit size, emits a warning. *)
let checkTypedefSize name typ =
  if Hashtbl.length stdIntegerSizes = 0 then
    initStdIntegerSizes ();
  if Cil.isIntegralType typ then begin
    let size = Cil.bitsSizeOf typ in
    try
      let intended_size, exact = Hashtbl.find stdIntegerSizes name in
      if (exact && size <> intended_size) ||
         (not exact && size < intended_size)
      then
        Kernel.warning ~current:true
          "bad type '%a' (%d bits) for typedef '%s';@ \
           check for mismatch between -machdep flag and headers used"
          Typ.pretty typ size name
    with
      (* Not a standard integer type, ignore it. *)
      Not_found -> ()
  end

(* Checks for invalid 'restrict' qualifiers,
   and reports [Kernel.error] if they are found. *)
let rec checkRestrictQualifierDeep t =
  if typeHasQualifier "restrict" t then
    match unrollType t with
    | TArray (bt, _, _, _) | TPtr (bt, _) ->
      if isFunctionType bt then
        Kernel.error ~once:true ~current:true
          "function pointer type does not allow 'restrict' qualifier"
      else
        checkRestrictQualifierDeep bt
    | _ -> Kernel.error ~once:true ~current:true
             "invalid usage of 'restrict' qualifier"
  else
    match unrollType t with
    | TArray (bt, _, _, _) | TPtr (bt, _) ->
      checkRestrictQualifierDeep bt
    | TFun (rt, args, _, _) ->
      checkRestrictQualifierDeep rt;
      begin
        match args with
        | None -> ()
        | Some args ->
          List.iter (fun (_, t, _) -> checkRestrictQualifierDeep t) args
      end
    | _ -> ()

let rec doSpecList ghost (suggestedAnonName: string)
    (* This string will be part of
     * the names for anonymous
     * structures and enums  *)
    (specs: A.spec_elem list)
  (* Returns the base type, the storage, whether it is inline and the
   * (unprocessed) attributes *)
  : typ * storage * bool * A.attribute list =
  (* Do one element and collect the type specifiers *)
  let isinline = ref false in (* If inline appears *)
  (* The storage is placed here *)
  let storage : storage ref = ref NoStorage in

  (* Collect the attributes.  Unfortunately, we cannot treat GCC
   * __attributes__ and ANSI C const/volatile the same way, since they
   * associate with structures differently.  Specifically, ANSI
   * qualifiers never apply to structures (ISO 6.7.3), whereas GCC
   * attributes always do (GCC manual 4.30).  Therefore, they are
   * collected and processed separately. *)
  let attrs : A.attribute list ref = ref [] in      (* __attribute__, etc. *)
  let cvattrs : A.cvspec list ref = ref [] in       (* const/volatile *)

  let doSpecElem (se: A.spec_elem)
      (acc: A.typeSpecifier list)
    : A.typeSpecifier list =
    match se with
    | A.SpecTypedef -> acc
    | A.SpecInline -> isinline := true; acc
    | A.SpecStorage st ->
      if !storage <> NoStorage then
        Kernel.error ~once:true ~current:true "Multiple storage specifiers";
      let sto' =
        match st with
        | A.NO_STORAGE -> NoStorage
        | A.AUTO -> NoStorage
        | A.REGISTER -> Register
        | A.STATIC -> Static
        | A.EXTERN -> Extern
      in
      storage := sto';
      acc

    | A.SpecCV cv -> cvattrs := cv :: !cvattrs; acc
    | A.SpecAttr a -> attrs := a :: !attrs; acc
    | A.SpecType ts -> ts :: acc
    | A.SpecPattern _ -> abort_context "SpecPattern in cabs2cil input"
  in
  (* Now scan the list and collect the type specifiers. Preserve the order *)
  let tspecs = List.fold_right doSpecElem specs [] in

  let tspecs' =
    (* GCC allows a named type that appears first to be followed by things
     * like "short", "signed", "unsigned" or "long". *)
    match tspecs with
    | A.Tnamed _ :: (_ :: _ as rest) when Cil.gccMode () ->
      (* If rest contains "short" or "long" then drop the Tnamed *)
      if List.exists (function A.Tshort -> true
                             | A.Tlong -> true | _ -> false) rest then
        rest
      else
        tspecs

    | _ -> tspecs
  in
  let tspecs'' =
    match specs, List.rev tspecs' with
    | A.SpecTypedef :: _, A.Tnamed _ :: [] ->
      tspecs'
    | A.SpecTypedef :: _, A.Tnamed _ :: rest ->
      List.rev rest
    | _ -> tspecs'
  in
  (* Sort the type specifiers *)
  let sortedspecs =
    let order = function (* Don't change this *)
      | A.Tvoid -> 0
      | A.Tsigned -> 1
      | A.Tunsigned -> 2
      | A.Tchar -> 3
      | A.Tshort -> 4
      | A.Tlong -> 5
      | A.Tint -> 6
      | A.Tint64 -> 7
      | A.Tfloat -> 8
      | A.Tdouble -> 9
      | _ -> 10 (* There should be at most one of the others *)
    in
    List.stable_sort (fun ts1 ts2 ->
        Datatype.Int.compare (order ts1) (order ts2)) tspecs''
  in
  let getTypeAttrs () : A.attribute list =
    (* Partitions the attributes in !attrs.
       Type attributes are removed from attrs and returned, so that they
       can go into the type definition.  Name attributes are left in attrs,
       so they will be returned by doSpecAttr and used in the variable
       declaration.
       Testcase: small1/attr9.c *)
    let an, af, at = cabsPartitionAttributes ghost ~default:AttrType !attrs in
    attrs := an;      (* Save the name attributes for later *)
    if af <> [] then
      Kernel.error ~once:true ~current:true
        "Invalid position for function type attributes.";
    at
  in

  (* And now try to make sense of it. See ISO 6.7.2 *)
  let bt =
    match sortedspecs with
    | [A.Tvoid] -> TVoid []
    | [A.Tchar] -> TInt(IChar, [])
    | [A.Tbool] -> TInt(IBool, [])
    | [A.Tsigned; A.Tchar] -> TInt(ISChar, [])
    | [A.Tunsigned; A.Tchar] -> TInt(IUChar, [])

    | [A.Tshort] -> TInt(IShort, [])
    | [A.Tsigned; A.Tshort] -> TInt(IShort, [])
    | [A.Tshort; A.Tint] -> TInt(IShort, [])
    | [A.Tsigned; A.Tshort; A.Tint] -> TInt(IShort, [])

    | [A.Tunsigned; A.Tshort] -> TInt(IUShort, [])
    | [A.Tunsigned; A.Tshort; A.Tint] -> TInt(IUShort, [])

    | [] -> TInt(IInt, [])
    | [A.Tint] -> TInt(IInt, [])
    | [A.Tsigned] -> TInt(IInt, [])
    | [A.Tsigned; A.Tint] -> TInt(IInt, [])

    | [A.Tunsigned] -> TInt(IUInt, [])
    | [A.Tunsigned; A.Tint] -> TInt(IUInt, [])

    | [A.Tlong] -> TInt(ILong, [])
    | [A.Tsigned; A.Tlong] -> TInt(ILong, [])
    | [A.Tlong; A.Tint] -> TInt(ILong, [])
    | [A.Tsigned; A.Tlong; A.Tint] -> TInt(ILong, [])

    | [A.Tunsigned; A.Tlong] -> TInt(IULong, [])
    | [A.Tunsigned; A.Tlong; A.Tint] -> TInt(IULong, [])

    | [A.Tlong; A.Tlong] -> TInt(ILongLong, [])
    | [A.Tsigned; A.Tlong; A.Tlong] -> TInt(ILongLong, [])
    | [A.Tlong; A.Tlong; A.Tint] -> TInt(ILongLong, [])
    | [A.Tsigned; A.Tlong; A.Tlong; A.Tint] -> TInt(ILongLong, [])

    | [A.Tunsigned; A.Tlong; A.Tlong] -> TInt(IULongLong, [])
    | [A.Tunsigned; A.Tlong; A.Tlong; A.Tint] -> TInt(IULongLong, [])

    (* int64 is to support MSVC *)
    | [A.Tint64] -> TInt(ILongLong, [])
    | [A.Tsigned; A.Tint64] -> TInt(ILongLong, [])

    | [A.Tunsigned; A.Tint64] -> TInt(IULongLong, [])

    | [A.Tfloat] -> TFloat(FFloat, [])
    | [A.Tdouble] -> TFloat(FDouble, [])

    | [A.Tlong; A.Tdouble] -> TFloat(FLongDouble, [])

    (* Now the other type specifiers *)
    | [A.Tnamed "__builtin_va_list"]
      when Cil.theMachine.theMachine.has__builtin_va_list ->
      TBuiltin_va_list []
    | [A.Tnamed "__fc_builtin_size_t"] -> Cil.theMachine.typeOfSizeOf
    | [A.Tnamed n] ->
      (match lookupType "type" n with
       | (TNamed _) as x, _ -> x
       | _ ->
         Kernel.fatal ~current:true "Named type %s is not mapped correctly" n)

    | [A.Tstruct (n, None, _)] -> (* A reference to a struct *)
      if n = "" then
        Kernel.error ~once:true ~current:true "Missing struct tag on incomplete struct";
      findCompType "struct" n []
    | [A.Tstruct (n, Some nglist, extraAttrs)] -> (* A definition of a struct *)
      let n' =
        if n <> "" then n else anonStructName "struct" suggestedAnonName in
      (* Use the (non-cv, non-name) attributes in !attrs now *)
      let a = extraAttrs @ (getTypeAttrs ()) in
      makeCompType ghost true n' ~norig:n nglist (doAttributes ghost a)

    | [A.Tunion (n, None, _)] -> (* A reference to a union *)
      if n = "" then
        Kernel.error ~once:true ~current:true "Missing union tag on incomplete union";
      findCompType "union" n []
    | [A.Tunion (n, Some nglist, extraAttrs)] -> (* A definition of a union *)
      let n' =
        if n <> "" then n else anonStructName "union" suggestedAnonName in
      (* Use the attributes now *)
      let a = extraAttrs @ (getTypeAttrs ()) in
      makeCompType ghost false n' ~norig:n nglist (doAttributes ghost a)

    | [A.Tenum (n, None, _)] -> (* Just a reference to an enum *)
      if n = "" then
        Kernel.error ~once:true ~current:true "Missing enum tag on incomplete enum";
      findCompType "enum" n []

    | [A.Tenum (n, Some eil, extraAttrs)] -> (* A definition of an enum *)
      let n' =
        if n <> "" then n else anonStructName "enum" suggestedAnonName in
      (* make a new name for this enumeration *)
      let n'', _  = newAlphaName true "enum" n' in

      (* Create the enuminfo, or use one that was created already for a
       * forward reference *)
      let enum, _ = createEnumInfo n'' ~norig:n in
      let a = extraAttrs @ (getTypeAttrs ()) in
      enum.eattr <- enum.eattr @ (doAttributes ghost a);
      let res = TEnum (enum, []) in
      let smallest = ref Integer.zero in
      let largest = ref Integer.zero in
      (* Life is fun here. ANSI says: enum constants are ints,
         and there's an implementation-dependent underlying integer
         type for the enum, which must be capable of holding all the
         enum's values.
         For MSVC, we follow these rules and assume the enum's
         underlying type is int.
         GCC allows enum constants that don't fit in int: the enum
         constant's type is the smallest type (but at least int) that
         will hold the value, with a preference for unsigned types.
         The underlying type EI of the enum is picked as follows:
         - let T be the smallest integer type that holds all the enum's
         values; T is signed if any enum value is negative, unsigned otherwise
         - if the enum is packed or sizeof(T) >= sizeof(int), then EI = T
         - otherwise EI = int if T is signed and unsigned int otherwise
         Note that these rules make the enum unsigned if possible *)
      let updateEnum i : ikind =
        if Integer.lt i !smallest then
          smallest := i;
        if Integer.gt i !largest then
          largest := i;
        if Cil.msvcMode () then
          IInt
        else begin
          match Kernel.Enums.get () with
          (* gcc-short-enum will try to pack the enum _type_, not the enum
             constant... *)
          | "" | "help" | "gcc-enums" | "gcc-short-enums" ->
            if fitsInInt IInt i then IInt
            else if fitsInInt IUInt i then IUInt
            else if fitsInInt ILongLong i then ILongLong
            else IULongLong
          | "int" -> IInt
          | s -> Kernel.fatal "Unknown enums representations '%s'" s
        end
      in
      (* as each name,value pair is determined, this is called *)
      let rec processName kname (i: exp) loc rest = begin
        (* add the name to the environment, but with a faked 'typ' field;
         * we don't know the full type yet (since that includes all of the
         * tag values), but we won't need them in here  *)

        (* add this tag to the list so that it ends up in the real
         * environment when we're finished  *)
        let newname, _  = newAlphaName true "" kname in
        let item = { eiorig_name = kname;
                     einame = newname;
                     eival = i;
                     eiloc = loc;
                     eihost = enum }
        in
        addLocalToEnv kname (EnvEnum item);
        (kname, item) :: loop (increm i 1) rest
      end

      and loop i = function
          [] -> []
        | (kname, { expr_node = A.NOTHING}, cloc) :: rest ->
          (* use the passed-in 'i' as the value, since none specified *)
          processName kname i (convLoc cloc) rest

        | (kname, e, cloc) :: rest ->
          (* constant-eval 'e' to determine tag value *)
          let e' = getIntConstExp ghost e in
          let e' = match constFoldToInt e' with
            | None ->
              Kernel.fatal ~current:true
                "Constant initializer %a not an integer"
                Cil_printer.pp_exp e'
            | Some i ->
              let ik = updateEnum i in
              if theMachine.lowerConstants then
                kinteger64 ~loc:e.expr_loc ~kind:ik i
              else 
                e'
          in
          processName kname e' (convLoc cloc) rest
      in

      (*TODO: find a better loc*)
      let fields = loop (zero ~loc:(CurrentLoc.get())) eil in
      (* Now set the right set of items *)
      enum.eitems <- List.map (fun (_, x) -> x) fields;
      (* Pick the enum's kind - see discussion above *)
      begin
        let unsigned = Integer.ge !smallest Integer.zero in
        let smallKind = intKindForValue !smallest unsigned in
        let largeKind = intKindForValue !largest unsigned in
        let real_kind =
          if (bytesSizeOfInt smallKind) > (bytesSizeOfInt largeKind) then
            smallKind
          else
            largeKind
        in
        let ekind =
          match Kernel.Enums.get () with
          | "" | "help" | "gcc-enums" ->
            if hasAttribute "packed" enum.eattr || 
               bytesSizeOfInt real_kind >= bytesSizeOfInt IInt
            then real_kind
            else if unsigned then IUInt else IInt
          | "int" -> IInt
          | "gcc-short-enums" -> real_kind
          | s -> Kernel.fatal "Unknown enum representation '%s'" s
        in
        enum.ekind <- ekind;
      end;
      (* Record the enum name in the environment *)
      addLocalToEnv (kindPlusName "enum" n') (EnvTyp res);
      (* And define the tag *)
      cabsPushGlobal (GEnumTag (enum, CurrentLoc.get ()));
      res

    | [A.TtypeofE e] ->
      let (_, s, e', t) =
        doExp (ghost_local_env ghost) false e AExpLeaveArrayFun
      in
      clean_up_chunk_locals s;
      let t' =
        match e'.enode with
        (* If this is a string literal, then we treat it as in sizeof*)
        | Const (CStr s) -> begin
            match typeOf e' with
            | TPtr(bt, _) -> (* This is the type of array elements *)
              TArray(bt,
                     Some (new_exp ~loc:e'.eloc (SizeOfStr s)),
                     empty_size_cache (),
                     [])
            | _ -> abort_context "The typeOf a string is not a pointer type"
          end
        | _ -> t
      in
      (*
        ignore (E.log "typeof(%a) = %a\n" d_exp e' d_type t');
       *)
      t'

    | [A.TtypeofT (specs, dt)] ->
      doOnlyType ghost specs dt

    | l ->
      Kernel.fatal ~current:true
        "Invalid combination of type specifiers:@ %a"
        (pp_list ~sep:"@ " Cprint.print_type_spec) l;
  in
  bt,!storage,!isinline,List.rev (!attrs @ (convertCVtoAttr !cvattrs))

(* given some cv attributes, convert them into named attributes for
 * uniform processing *)
and convertCVtoAttr (src: A.cvspec list) : A.attribute list =
  match src with
  | [] -> []
  | CV_CONST    :: tl -> ("const",[])    :: (convertCVtoAttr tl)
  | CV_VOLATILE :: tl -> ("volatile",[]) :: (convertCVtoAttr tl)
  | CV_RESTRICT :: tl -> ("restrict",[]) :: (convertCVtoAttr tl)
  | CV_ATTRIBUTE_ANNOT a :: tl -> (mkAttrAnnot a, []) :: convertCVtoAttr tl

and makeVarInfoCabs
    ~(ghost:bool)
    ~(isformal: bool)
    ~(isglobal: bool)
    ?(isgenerated=false)
    (ldecl : location)
    (bt, sto, inline, attrs)
    (n,ndt,a)
  : varinfo =
  let vtype, nattr =
    doType ghost isformal (AttrName false)
      ~allowVarSizeArrays:isformal  (* For locals we handle var-sized arrays
                                       before makeVarInfoCabs; for formals
                                       we do it afterwards *)
      bt (A.PARENTYPE(attrs, ndt, a)) in
  (*Format.printf "Got yp:%a->%a(%a)@." d_type bt d_type vtype d_attrlist nattr;*)

  if inline && not (isFunctionType vtype) then
    Kernel.error ~once:true ~current:true "inline for a non-function: %s" n;
  checkRestrictQualifierDeep vtype;
  (*  log "Looking at %s(%b): (%a)@." n isformal d_attrlist nattr;*)
  let vi = makeVarinfo ~temp:isgenerated isglobal isformal n vtype in
  vi.vstorage <- sto;
  vi.vattr <- nattr;
  vi.vdecl <- ldecl;
  vi.vghost <- ghost;
  vi.vdefined <-
    not (isFunctionType vtype) && isglobal && (sto = NoStorage || sto = Static);

  (*  if false then
      log "Created varinfo %s : %a\n" vi.vname d_type vi.vtype;*)

  vi

(* Process a local variable declaration and allow variable-sized arrays *)
and makeVarSizeVarInfo ghost (ldecl : location)
    spec_res
    (n,ndt,a)
  : varinfo * chunk * exp * bool =
  if not (Cil.msvcMode ()) then
    match isVariableSizedArray ghost ndt with
    | None ->
      makeVarInfoCabs ~ghost ~isformal:false
        ~isglobal:false
        ldecl spec_res (n,ndt,a), empty, zero ~loc:ldecl, false
    | Some (ndt', se, len) ->
      makeVarInfoCabs ~ghost ~isformal:false
        ~isglobal:false
        ldecl spec_res (n,ndt',a), se, len, true
  else
    makeVarInfoCabs ~ghost ~isformal:false
      ~isglobal:false
      ldecl spec_res (n,ndt,a), empty, zero ~loc:ldecl, false

and doAttr ghost (a: A.attribute) : attribute list =
  (* Strip the leading and trailing underscore *)
  let stripUnderscore (n: string) : string =
    let l = String.length n in
    let rec start i =
      if i >= l then
        Kernel.error ~once:true ~current:true "Invalid attribute name %s" n;
      if String.get n i = '_' then start (i + 1) else i
    in
    let st = start 0 in
    let rec finish i =
      (* We know that we will stop at >= st >= 0 *)
      if String.get n i = '_' then finish (i - 1) else i
    in
    let fin = finish (l - 1) in
    String.sub n st (fin - st + 1)
  in
  match a with
  | ("__attribute__", []) -> []  (* An empty list of gcc attributes *)
  | (s, []) ->
    let s = stripUnderscore s in
    [ match attrAnnot s with None -> Attr(s, []) | Some s -> AttrAnnot s ]
  | (s, el) ->

    let rec attrOfExp (strip: bool)
        ?(foldenum=true)
        (a: A.expression) : attrparam =
      let loc = a.expr_loc in
      match a.expr_node with
      | A.VARIABLE n -> begin
          let n' = if strip then stripUnderscore n else n in
          (** See if this is an enumeration *)
          try
            if not foldenum then raise Not_found;

            match H.find env n' with
            | EnvEnum item, _ -> begin
                match constFoldToInt item.eival with
                | Some i64 when theMachine.lowerConstants ->
                  AInt i64
                |  _ -> ACons(n', [])
              end
            | _ -> ACons (n', [])
          with Not_found -> ACons(n', [])
        end
      | A.CONSTANT (A.CONST_STRING s) -> AStr s
      | A.CONSTANT (A.CONST_INT str) -> begin
          match (parseIntExp ~loc str).enode with
          | Const (CInt64 (v64,_,_)) ->
            AInt v64
          | _ ->
            Kernel.fatal ~current:true "Invalid attribute constant: %s" str
        end
      | A.CONSTANT (A.CONST_FLOAT str) ->
        ACons ("__fc_float", [AStr str])
      | A.CALL({expr_node = A.VARIABLE n}, args) -> begin
          let n' = if strip then stripUnderscore n else n in
          let ae' = List.map ae args in
          ACons(n', ae')
        end
      | A.EXPR_SIZEOF e -> ASizeOfE (ae e)
      | A.TYPE_SIZEOF (bt, dt) -> ASizeOf (doOnlyType ghost bt dt)
      | A.EXPR_ALIGNOF e -> AAlignOfE (ae e)
      | A.TYPE_ALIGNOF (bt, dt) -> AAlignOf (doOnlyType ghost bt dt)
      | A.BINARY(A.AND, aa1, aa2) ->
        ABinOp(LAnd, ae aa1, ae aa2)
      | A.BINARY(A.OR, aa1, aa2) ->
        ABinOp(LOr, ae aa1, ae aa2)
      | A.BINARY(A.ASSIGN,aa1,aa2) ->
        (* Bit of a hack to account for OSX specific syntax. *)
        ACons ("__fc_assign", [ae aa1; ae aa2])
      | A.BINARY(abop, aa1, aa2) ->
        ABinOp (convBinOp abop, ae aa1, ae aa2)
      | A.UNARY(A.PLUS, aa) -> ae aa
      | A.UNARY(A.MINUS, aa) -> AUnOp (Neg, ae aa)
      | A.UNARY(A.BNOT, aa) -> AUnOp(BNot, ae aa)
      | A.UNARY(A.NOT, aa) -> AUnOp(LNot, ae aa)
      | A.MEMBEROF (e, s) -> ADot (ae e, s)
      | A.PAREN(e) -> attrOfExp strip ~foldenum:foldenum e
      | A.UNARY(A.MEMOF, aa) -> AStar (ae aa)
      | A.UNARY(A.ADDROF, aa) -> AAddrOf (ae aa)
      | A.MEMBEROFPTR (aa1, s) -> ADot(AStar(ae aa1), s)
      | A.INDEX(aa1, aa2) -> AIndex(ae aa1, ae aa2)
      | A.QUESTION(aa1, aa2, aa3) -> AQuestion(ae aa1, ae aa2, ae aa3)
      | _ ->
        Kernel.fatal ~current:true
          "cabs2cil: invalid expression in attribute: %a"
          Cprint.print_expression a

    and ae (e: A.expression) = attrOfExp false e in

    (* Sometimes we need to convert attrarg into attr *)
    let arg2attr = function
      | ACons (s, args) -> Attr (s, args)
      | a ->
        Kernel.fatal ~current:true
          "Invalid form of attribute: %a"
          Cil_printer.pp_attrparam a;
    in
    if s = "__attribute__" then (* Just a wrapper for many attributes*)
      List.map (fun e -> arg2attr (attrOfExp true ~foldenum:false e)) el
    else if s = "__blockattribute__" then (* Another wrapper *)
      List.map (fun e -> arg2attr (attrOfExp true ~foldenum:false e)) el
    else if s = "__declspec" then
      List.map (fun e -> arg2attr (attrOfExp false ~foldenum:false e)) el
    else
      [Attr(stripUnderscore s, List.map (attrOfExp ~foldenum:false false) el)]

and doAttributes (ghost:bool) (al: A.attribute list) : attribute list =
  List.fold_left (fun acc a -> cabsAddAttributes (doAttr ghost a) acc) [] al

(* A version of Cil.partitionAttributes that works on CABS attributes.
   It would  be better to use Cil.partitionAttributes instead to avoid
   the extra doAttr conversions here, but that's hard to do in doSpecList.*)
and cabsPartitionAttributes
    ghost
    ~(default:attributeClass)
    (attrs:  A.attribute list) :
  A.attribute list * A.attribute list * A.attribute list =
  let rec loop (n,f,t) = function
      [] -> n, f, t
    | a :: rest ->
      let kind = match doAttr ghost a with
        | [] -> default
        | (Attr(an, _) | AttrAnnot an)::_ ->
          (try attributeClass an with Not_found -> default)
      in
      match kind with
      | AttrName _ -> loop (a::n, f, t) rest
      | AttrFunType _ -> loop (n, a::f, t) rest
      | AttrType -> loop (n, f, a::t) rest
  in
  loop ([], [], []) attrs



and doType (ghost:bool) isFuncArg
    (nameortype: attributeClass) (* This is AttrName if we are doing
                                  * the type for a name, or AttrType
                                  * if we are doing this type in a
                                  * typedef *)
    ?(allowZeroSizeArrays=false)
    ?(allowVarSizeArrays=false)
    (bt: typ)                    (* The base type *)
    (dt: A.decl_type)
  (* Returns the new type and the accumulated name (or type attribute
     if nameoftype =  AttrType) attributes *)
  : typ * attribute list =

  (* Now do the declarator type. But remember that the structure of the
   * declarator type is as printed, meaning that it is the reverse of the
   * right one *)
  let rec doDeclType (bt: typ) (acc: attribute list) decl_type =
    checkRestrictQualifierDeep bt;
    match decl_type with
    | A.JUSTBASE -> bt, acc
    | A.PARENTYPE (a1, d, a2) ->
      let a1' = doAttributes ghost a1 in
      let a1n, a1f, a1t = partitionAttributes AttrType a1' in
      let a2' = doAttributes ghost a2 in
      let a2n, a2f, a2t = partitionAttributes nameortype a2' in
      (*Format.printf "doType: @[a1n=%a@\na1f=%a@\na1t=%a@\na2n=%a@\na2f=%a@\na2t=%a@]@\n" d_attrlist a1n d_attrlist a1f d_attrlist a1t d_attrlist a2n d_attrlist a2f d_attrlist a2t;*)
      let bt' = cabsTypeAddAttributes a1t bt in
      (*        log "bt' = %a@." d_type bt';*)

      let bt'', a1fadded =
        match unrollType bt with
        | TFun _ -> cabsTypeAddAttributes a1f bt', true
        | _ -> bt', false
      in
      (* Now recurse *)
      let restyp, nattr = doDeclType bt'' acc d in
      (* Add some more type attributes *)
      let restyp = cabsTypeAddAttributes a2t restyp in
      (* See if we can add some more type attributes *)
      let restyp' =
        match unrollType restyp with
        | TFun _ ->
          if a1fadded then
            cabsTypeAddAttributes a2f restyp
          else
            cabsTypeAddAttributes a2f
              (cabsTypeAddAttributes a1f restyp)
        | TPtr ((TFun _ as tf), ap) when not (Cil.msvcMode ()) ->
          if a1fadded then
            TPtr(cabsTypeAddAttributes a2f tf, ap)
          else
            TPtr(cabsTypeAddAttributes a2f
                   (cabsTypeAddAttributes a1f tf), ap)
        | _ ->
          if a1f <> [] && not a1fadded then
            Kernel.error ~once:true ~current:true
              "Invalid position for (prefix) function type attributes:%a"
              Cil_printer.pp_attributes a1f;
          if a2f <> [] then
            Kernel.error ~once:true ~current:true
              "Invalid position for (post) function type attributes:%a"
              Cil_printer.pp_attributes a2f;
          restyp
      in
      (*        log "restyp' = %a@." d_type restyp';*)

      (* Now add the name attributes and return *)
      restyp', cabsAddAttributes a1n (cabsAddAttributes a2n nattr)

    | A.PTR (al, d) ->
      let al' = doAttributes ghost al in
      let an, af, at = partitionAttributes AttrType al' in
      (* Now recurse *)
      let restyp, nattr = doDeclType (TPtr(bt, at)) acc d in
      (* See if we can do anything with function type attributes *)
      let restyp' =
        match unrollType restyp with
        | TFun _ -> cabsTypeAddAttributes af restyp
        | TPtr((TFun _ as tf), ap) ->
          TPtr(cabsTypeAddAttributes af tf, ap)
        | _ ->
          if af <> [] then
            Kernel.error ~once:true ~current:true
              "Invalid position for function type attributes:%a"
              Cil_printer.pp_attributes af;
          restyp
      in
      (* Now add the name attributes and return *)
      restyp', cabsAddAttributes an nattr

    | A.ARRAY (d, al, len) ->
      if Cil.isFunctionType bt then
        Kernel.error ~once:true ~current:true
          "declaration of array of function type '%a`"
          Cil_printer.pp_typ bt
      else if not (Cil.isCompleteType ~allowZeroSizeArrays:true bt) then
        Kernel.error ~once:true ~current:true
          "declaration of array of incomplete type '%a`"
          Cil_printer.pp_typ bt
      else if not allowZeroSizeArrays &&
         not (Cil.isCompleteType ~allowZeroSizeArrays:false bt)
      then
        (* because we tested previously for incomplete types and now tested again
           forbidding zero-length arrays, bt is necessarily a zero-length array *)
        if Cil.gccMode () || Cil.msvcMode () then
          Kernel.warning ~once:true ~current:true
            "declaration of array of 'zero-length arrays' ('%a`);@ \
             zero-length arrays are a compiler extension"
            Cil_printer.pp_typ bt
        else
          Kernel.error ~once:true ~current:true
            "declaration of array of 'zero-length arrays' ('%a`);@ \
             zero-length arrays are not allowed in C99"
            Cil_printer.pp_typ bt;
      let lo =
        match len.expr_node with
        | A.NOTHING -> None
        | _ ->
            (* Check that len is a constant expression.
               We used to also cast the length to int here, but that's
               theoretically too restrictive on 64-bit machines. *)
            let len' = doPureExp (ghost_local_env ghost) len in
            if not (isIntegralType (typeOf len')) then
              Kernel.error ~once:true ~current:true
                "Array length %a does not have an integral type."
                Cil_printer.pp_exp len';
            if not allowVarSizeArrays then begin
              (* Assert that len' is a constant *)
              let cst = constFold true len' in
              (match cst.enode with
               | Const(CInt64(i, _, _)) ->
                 if Integer.lt i Integer.zero then
                   Kernel.error ~once:true ~current:true 
                     "Length of array is negative"
               | _ ->
                 if isConstant cst then
                   (* e.g., there may be a float constant involved.
                    * We'll leave it to the user to ensure the length is
                    * non-negative, etc.*)
                   Kernel.warning ~once:true ~current:true
                     "Unable to do constant-folding on array length %a. \
                      Some CIL operations on this array may fail."
                     Cil_printer.pp_exp cst
                 else
                   Kernel.error ~once:true ~current:true
                     "Length of array is not a constant: %a"
                     Cil_printer.pp_exp cst)
            end;
            if Cil.isZero len' && not allowZeroSizeArrays &&
               not (Cil.gccMode () || Cil.msvcMode ())
            then
              Kernel.error ~once:true ~current:true
                "zero-length arrays only allowed for GCC/MSVC";
            Some len'
      in
      let al' = doAttributes ghost al in
      if not isFuncArg && hasAttribute "static" al' then
        Kernel.error ~once:true ~current:true
          "static specifier inside array argument is allowed only in \
           function argument";
      doDeclType (TArray(bt, lo, empty_size_cache (), al')) acc d

    | A.PROTO (d, args, isva) ->
      (* Start a scope for the parameter names *)
      enterScope ();
      (* Intercept the old-style use of varargs.h. On GCC this means that
       * we have ellipsis and a last argument "builtin_va_alist:
       * builtin_va_alist_t". On MSVC we do not have the ellipsis and we
       * have a last argument "va_alist: va_list" *)
      let args', isva' =
        if args != [] && Cil.msvcMode () = not isva then begin
          let newisva = ref isva in
          let rec doLast = function
              [([A.SpecType (A.Tnamed atn)], (an, A.JUSTBASE, [], _))]
              when isOldStyleVarArgTypeName atn &&
                   isOldStyleVarArgName an -> begin
                (* Turn it into a vararg *)
                newisva := true;
                (* And forget about this argument *)
                []
              end

            | a :: rest -> a :: doLast rest
            | [] -> []
          in
          let args' = doLast args in
          (args', !newisva)
        end else (args, isva)
      in
      (* Make the argument as for a formal *)
      let doOneArg (s, (n, ndt, a, cloc)) : varinfo =
        let s' = doSpecList ghost n s in
        let vi = makeVarInfoCabs ~ghost ~isformal:true ~isglobal:false
            (convLoc cloc) s' (n,ndt,a) in
        (* Add the formal to the environment, so it can be referenced by
           other formals  (e.g. in an array type, although that will be
           changed to a pointer later, or though typeof).  *)
        addLocalToEnv vi.vname (EnvVar vi);
        vi
      in
      let targs : varinfo list option =
        match List.map doOneArg args'  with
        | [] -> None (* No argument list *)
        | [t] when isVoidType t.vtype ->
          Some []
        | l ->
          Some l
      in
      exitScope ();
      (* Turn [] types into pointers in the arguments and the result type.
       * Turn function types into pointers to respective. This simplifies
       * our life a lot, and is what the standard requires. *)
      let turnArrayIntoPointer (bt: typ)
          (lo: exp option) (a: attributes) : typ =
        let _real_a = dropAttribute "static" a in
        let a' : attributes =
          match lo with
          | None -> []
          | Some l -> begin
              let static = if hasAttribute "static" a then
                  [Attr ("static",[])]
                else []
              in
              (* Transform the length into an attribute expression *)
              try
                let la : attrparam = expToAttrParam l in
                Attr("arraylen", [ la ]) :: static
              with NotAnAttrParam _ -> begin
                  Kernel.warning ~once:true ~current:true
                    "Cannot represent the length '%a'of array as an attribute"
                    Cil_printer.pp_exp l
                  ;
                  static (* Leave unchanged *)
                end
            end
        in
        TPtr(bt, a')
      in
      let rec fixupArgumentTypes (argidx: int) (args: varinfo list) : unit =
        match args with
        | [] -> ()
        | a :: args' ->
          (match unrollType a.vtype with
           | TArray(bt,lo,_,attr) ->
             (* Note that for multi-dimensional arrays we strip off only
                the first TArray and leave bt alone. *)
             let real_type = turnArrayIntoPointer bt lo attr in
             Cil.update_var_type a real_type
           | TFun _ -> Cil.update_var_type a (TPtr(a.vtype, []))
           | TComp (_, _,_) -> begin
               match isTransparentUnion a.vtype with
               | None ->  ()
               | Some fstfield ->
                 transparentUnionArgs :=
                   (argidx, a.vtype) :: !transparentUnionArgs;
                 Cil.update_var_type a fstfield.ftype;
             end
           | _ -> ());
          fixupArgumentTypes (argidx + 1) args'
      in
      let args =
        match targs with
        | None -> None
        | Some argl ->
          fixupArgumentTypes 0 argl;
          Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) argl)
      in
      let tres =
        match unrollType bt with
        | TArray(t,lo,_,attr) -> turnArrayIntoPointer t lo attr
        | _ -> bt
      in
      (* Drop qualifiers on the return type. They are meaningless (qualifiers
         make sense only on l-values), and they make life more complicated:
         the return type of the function is used e.g. for the type of retres,
         and probably in many other places. *)
      let tres = Cil.type_remove_qualifier_attributes tres in
      doDeclType (TFun (tres, args, isva', [])) acc d
  in
  doDeclType bt [] dt

(* If this is a declarator for a variable size array then turn it into a
   pointer type and a length *)
and isVariableSizedArray ghost (dt: A.decl_type)
  : (A.decl_type * chunk * exp) option =
  let res = ref None in
  let rec findArray = function
      ARRAY (JUSTBASE, al, lo) when lo.expr_node != A.NOTHING ->
      (* Checks whether the expression is an integer constant expression,
         that is:
         - it contains no side-effect
         - it can be evaluated at compile-time
         Note that we should not pass true as asconst argument for doExp,
         since we are precisely trying to determine whether the expression
         is a constant or not.
      *)
      let (_, se, e', _) =
        doExp (ghost_local_env ghost) false lo (AExp (Some intType)) in
      if isNotEmpty se || not (isConstant e') then begin
        res := Some (se, e');
        PTR (al, JUSTBASE)
      end else
        ARRAY (JUSTBASE, al, lo)
    | ARRAY (dt, al, lo) -> ARRAY (findArray dt, al, lo)
    | PTR (al, dt) -> PTR (al, findArray dt)
    | JUSTBASE -> JUSTBASE
    | PARENTYPE (prea, dt, posta) -> PARENTYPE (prea, findArray dt, posta)
    | PROTO (dt, f, a) -> PROTO (findArray dt, f, a)
  in
  let dt' = findArray dt in
  match !res with
  | None -> None
  | Some (se, e) -> Some (dt', se, e)

and doOnlyType ghost (specs: A.spec_elem list) (dt: A.decl_type) : typ =
  let bt',sto,inl,attrs = doSpecList ghost "" specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true "Storage or inline specifier in type only";
  let tres, nattr =
    doType ghost false AttrType bt' (A.PARENTYPE(attrs, dt, [])) in
  if nattr <> [] then
    Kernel.error ~once:true ~current:true
      "Name attributes in only_type: %a" Cil_printer.pp_attributes nattr;
  tres


and makeCompType ghost (isstruct: bool)
    (n: string)
    ~(norig: string)
    (nglist: A.field_group list)
    (a: attribute list) =
  (* Make a new name for the structure *)
  let kind = if isstruct then "struct" else "union" in
  let n', _  = newAlphaName true kind n in
  (* Create the self cell for use in fields and forward references. Or maybe
   * one exists already from a forward reference  *)
  let comp, _ = createCompInfo isstruct n' norig in
  let doFieldGroup ~is_first_group ~is_last_group ((s: A.spec_elem list),
                    (nl: (A.name * A.expression option) list)) =
    (* Do the specifiers exactly once *)
    let sugg = match nl with
      | [] -> ""
      | ((n, _, _, _), _) :: _ -> n
    in
    let bt, sto, inl, attrs = doSpecList ghost sugg s in
    (* Do the fields *)
    let makeFieldInfo ~is_first_field ~is_last_field
        (((n,ndt,a,cloc) : A.name), (widtho : A.expression option))
      : fieldinfo =
      if sto <> NoStorage || inl then
        Kernel.error ~once:true ~current:true "Storage or inline not allowed for fields";
      let allowZeroSizeArrays = true in
      let ftype, nattr =
        doType
          ~allowZeroSizeArrays ghost false (AttrName false) bt 
          (A.PARENTYPE(attrs, ndt, a))
      in
      (* check for fields whose type is incomplete. In particular, this rules
         out circularity:
         struct C1 { struct C2 c2; };          //This line is now an error.
         struct C2 { struct C1 c1; int dummy; };
      *)
      if Cil.isFunctionType ftype then
          Kernel.error ~current:true
            "field `%s' declared as a function" n
      else if not (Cil.isCompleteType ~allowZeroSizeArrays ftype)
      then begin
        match Cil.unrollType ftype with
        | TArray(_,None,_,_) when is_last_field ->
          begin
            (* possible flexible array member; check if struct contains at least
               one other field *)
            if is_first_field then (* struct is empty *)
              Kernel.error ~current:true
                "flexible array member '%s' (type %a) \
                 not allowed in otherwise empty struct"
                n Cil_printer.pp_typ ftype
            else (* valid flexible array member *) ()
          end
        | _ ->
          Kernel.error ~current:true
            "field %s is declared with incomplete type %a"
            n Cil_printer.pp_typ ftype
      end;
      let width, ftype =
        match widtho with
        | None -> None, ftype
        | Some w -> begin
            (match unrollType ftype with
             | TInt (_, _) -> ()
             | TEnum _ -> ()
             | _ ->
               Kernel.error ~once:true ~current:true
                 "Base type for bitfield is not an integer type");
            match isIntegerConstant ghost w with
            | None ->
              Kernel.fatal ~current:true
                "bitfield width is not an integer constant"
            | Some s as w ->
              let ftype =
                typeAddAttributes 
                  [Attr (bitfield_attribute_name, [AInt (Integer.of_int s)])]
                  ftype
              in
              w, ftype
          end
      in
      (* If the field is unnamed and its type is a structure of union type
       * then give it a distinguished name  *)
      let n' =
        if n = missingFieldName then begin
          match unrollType ftype with
          | TComp _ -> begin
              if not (Kernel.C11.get ()) then
                Kernel.warning ~once:true ~current:true
                  "unnamed fields are a C11 extension \
                   (use %s to avoid this warning)"
                  Kernel.C11.name;
              incr anonCompFieldNameId;
              anonCompFieldName ^ (string_of_int !anonCompFieldNameId)
            end
          | _ -> n
        end else
          n
      in
      let rec is_circular t =
        match Cil.unrollType t with
        | TArray(bt,_,_,_) -> is_circular bt
        | TComp (comp',_,_) ->
          if Cil_datatype.Compinfo.equal comp comp' then begin
            (* abort and not error, as this circularity could lead
               to infinite recursion... *)
            Kernel.abort
              "type %s %s is circular" 
              (if comp.cstruct then "struct" else "union")
              comp.cname;
          end else
            List.iter (fun f -> is_circular f.ftype) comp'.cfields;
        | _ -> ()
      in
      is_circular ftype;
      { fcomp     =  comp;
        forig_name = n;
        fname     =  n';
        ftype     =  ftype;
        fbitfield =  width;
        fattr     =  nattr;
        floc      =  convLoc cloc;
        faddrof   = false;
        fsize_in_bits = None;
        foffset_in_bits = None;
        fpadding_in_bits = None;
      }
    in
    let rec map_but_last l =
      match l with
      | [] -> []
      | [f] ->
        [makeFieldInfo ~is_first_field:false ~is_last_field:is_last_group f]
      | f::l ->
        let fi = makeFieldInfo ~is_first_field:false ~is_last_field:false f in
        [fi] @ map_but_last l
    in
    match nl with
    | [] -> []
    | [f] ->
      [makeFieldInfo ~is_first_field:is_first_group ~is_last_field:is_last_group f]
    | f::l ->
      let fi =
        makeFieldInfo ~is_first_field:is_first_group ~is_last_field:false f
      in
      [fi] @ map_but_last l
  in

  (* Do regular fields first. *)
  let flds =
    List.filter (function FIELD _ -> true | TYPE_ANNOT _ -> false) nglist in
  let flds =
    List.map (function FIELD (f,g) -> (f,g) | _ -> assert false) flds in
  let last = List.length flds -  1 in
  let doField i = doFieldGroup ~is_first_group:(i=0) ~is_last_group:(i=last) in
  let flds = List.concat (List.mapi doField flds) in

  let fld_table = Cil_datatype.Fieldinfo.Hashtbl.create 17 in
  let check f =
    try
      let oldf = Cil_datatype.Fieldinfo.Hashtbl.find fld_table f in
      let source = fst f.floc in
      Kernel.error ~source
        "field %s occurs multiple times in aggregate %a. \
         Previous occurrence is at line %d."
        f.fname Cil_printer.pp_typ (TComp(comp,{scache = Not_Computed},[]))
        (fst oldf.floc).Lexing.pos_lnum
    with Not_found ->
      (* Do not add unnamed bitfields: they can share the empty name. *)
      if f.fname <> "" then Cil_datatype.Fieldinfo.Hashtbl.add fld_table f f
  in
  List.iter check flds;
  if comp.cfields <> [] then begin
    (* This appears to be a multiply defined structure. This can happen from
     * a construct like "typedef struct foo { ... } A, B;". This is dangerous
     * because at the time B is processed some forward references in { ... }
     * appear as backward references, which could lead to circularity in
     * the type structure. We do a thorough check and then we reuse the type
     * for A *)
    if List.length comp.cfields <> List.length flds
       || (List.exists2 (fun f1 f2 -> not (Cil_datatype.Typ.equal f1.ftype f2.ftype))
             comp.cfields flds)
    then
      Kernel.error ~once:true ~current:true
        "%s seems to be multiply defined" (compFullName comp)
  end else
    begin
      comp.cfields <- flds;
      let fields_with_pragma_attrs =
        List.map (fun fld ->
            (* note: in the call below, we CANNOT use fld.fcomp.cattr because it has not
               been filled in yet, so we need to pass the list of attributes [a] to it *)
            {fld with fattr = (process_pragmas_pack_align_field_attributes fld fld.fattr a)}
          ) comp.cfields
      in
      comp.cfields <- fields_with_pragma_attrs
    end;

  (*  ignore (E.log "makeComp: %s: %a\n" comp.cname d_attrlist a); *)
  let a = Cil.addAttributes comp.cattr a in
  comp.cattr <- process_pragmas_pack_align_comp_attributes comp a;
  let res = TComp (comp,empty_size_cache (), []) in
  (* This compinfo is defined, even if there are no fields *)
  comp.cdefined <- true;
  (* Create a typedef for this one *)
  cabsPushGlobal (GCompTag (comp, CurrentLoc.get ()));

  (* There must be a self cell created for this already *)
  addLocalToEnv (kindPlusName kind n) (EnvTyp res);
  (* Now create a typedef with just this type *)
  res

and preprocessCast ghost (specs: A.specifier)
    (dt: A.decl_type)
    (ie: A.init_expression)
  : A.specifier * A.decl_type * A.init_expression =
  let typ = doOnlyType ghost specs dt in
  (* If we are casting to a union type then we have to treat this as a
   * constructor expression. This is to handle the gcc extension that allows
   * cast from a type of a field to the type of the union  *)
  (* However, it may just be casting of a whole union to its own type.  We
   * will resolve this later, when we'll convert casts to unions. *)
  let ie' =
    match unrollType typ, ie with
    | TComp (c, _, _), A.SINGLE_INIT _ when not c.cstruct ->
      A.COMPOUND_INIT [(A.INFIELD_INIT ("___matching_field",
                                        A.NEXT_INIT),
                        ie)]
    | _, _ -> ie
  in
  (* Maybe specs contains an unnamed composite. Replace with the name so that
   * when we do again the specs we get the right name  *)
  let specs1 =
    match typ with
    | TComp (ci, _, _) ->
      List.map
        (function
            A.SpecType (A.Tstruct ("", _, [])) ->
            A.SpecType (A.Tstruct (ci.cname, None, []))
          | A.SpecType (A.Tunion ("", _, [])) ->
            A.SpecType (A.Tunion (ci.cname, None, []))
          | s -> s) specs
    | _ -> specs
  in
  specs1, dt, ie'

and getIntConstExp ghost (aexp) : exp =
  let loc = aexp.expr_loc in
  let _, c, e, _ = doExp (ghost_local_env ghost) true aexp (AExp None) in
  if not (isEmpty c) then
    Kernel.error ~once:true ~current:true "Constant expression %a has effects" 
      Cil_printer.pp_exp e;
  match e.enode with
  (* first, filter for those Const exps that are integers *)
  | Const (CInt64 _ ) -> e
  | Const (CEnum _) -> e
  | Const (CChr i) -> new_exp ~loc (Const(charConstToIntConstant i))

  (* other Const expressions are not ok *)
  | Const _ ->
    Kernel.fatal ~current:true "Expected integer constant and got %a" 
      Cil_printer.pp_exp e

  (* now, anything else that 'doExp true' returned is ok (provided
     that it didn't yield side effects); this includes, in particular,
     the various sizeof and alignof expression kinds *)
  | _ -> e

and isIntegerConstant ghost (aexp) : int option =
  match doExp (ghost_local_env ghost) true aexp (AExp None) with
  | (_, c, e, _) when isEmpty c -> begin
      match Cil.constFoldToInt e with
      | Some i64 -> Some (Integer.to_int i64)
      | _ -> None
    end
  | _ -> None

(* Process an expression and in the process do some type checking,
 * extract the effects as separate statements.
 * doExp returns the following 4-uple:
 * - a list of read accesses performed for the evaluation of the expression
 * - a chunk representing side-effects occurring during evaluation
 * - the CIL expression
 * - its type.
*)
and doExp local_env
    (asconst: bool)   (* This expression is used as a constant *)
    (e: A.expression)
    (what: expAction)
  =
  let ghost = local_env.is_ghost in
  let loc = e.expr_loc in
  (* will be reset at the end of the compilation of current expression. *)
  let oldLoc = CurrentLoc.get() in
  CurrentLoc.set loc;
  let checkVoidLval e t =
    if (match e.enode with Lval _ -> true | _ -> false) && isVoidType t then
      Kernel.fatal ~current:true
        "lvalue of type void: %a@\n" Cil_printer.pp_exp e
  in
  (* A subexpression of array type is automatically turned into StartOf(e).
   * Similarly an expression of function type is turned into AddrOf. So
   * essentially doExp should never return things of type TFun or TArray *)
  let processArrayFun e t =
    let loc = e.eloc in
    match e.enode, unrollType t with
    | (Lval(lv) | CastE(_, {enode = Lval lv})), TArray(tbase, _, _, a) ->
      mkStartOfAndMark loc lv, TPtr(tbase, a)
    | Lval(Mem _, _), TFun _ -> e, t (* Do not turn pointer function types *)
    | (Lval(lv) | CastE(_, {enode = Lval lv})), TFun _  ->
      mkAddrOfAndMark loc lv, TPtr(t, [])
    | _, (TArray _ | TFun _) ->
      Kernel.fatal ~current:true
        "Array or function expression is not lval: %a@\n"
        Cil_printer.pp_exp e
    | _ -> e, t
  in
  (* Before we return we call finishExp *)
  let finishExp ?(newWhat=what) reads (se: chunk) (e: exp) (t: typ) =
    match newWhat with
    | ADrop
    | AType ->
      let (e', t') = processArrayFun e t in
      (reads, se, e', t')
    | AExpLeaveArrayFun ->
      (reads, se, e, t)
    (* It is important that we do not do "processArrayFun" in
     * this case. We exploit this when we process the typeOf construct *)
    | AExp _ ->
      let (e', t') = processArrayFun e t in
      checkVoidLval e' t';
      (*
        ignore (E.log "finishExp: e'=%a, t'=%a\n"
        Cil_printer.pp_exp e' d_type t');
       *)
      (reads, se, e', t')

    | ASet (is_real_write,lv, r, lvt) -> begin
        (* See if the set was done already *)
        match e.enode with
        | Lval(lv') when lv == lv' ->
          (reads,se, e, t) (* if this is the case, the effects have also been
                              taken into account in the chunk. *)
        | _ ->
          let (e', t') = processArrayFun e t in
          let (t'', e'') = castTo t' lvt e' in
          checkVoidLval e'' t'';
          (*Kernel.debug "finishExp: e = %a\n  e'' = %a\n" Cil_printer.pp_exp e Cil_printer.pp_exp e'';*)
          let writes = if is_real_write then [lv] else [] in
          ([], (* the reads are incorporated in the chunk. *)
           ((unspecified_chunk empty) @@ (remove_reads lv se, ghost)) 
           +++
           (mkStmtOneInstr ~ghost ~valid_sid (Set(lv, e'', CurrentLoc.get ())),
            writes,writes,
            List.filter (fun x -> not (LvalStructEq.equal x lv)) r @ reads),
           e'', t'')

      end
  in
  let result =
    match e.expr_node with
    | A.PAREN e -> doExp (paren_local_env local_env) asconst e what
    | A.NOTHING when what = ADrop ->
      finishExp [] (unspecified_chunk empty) (integer ~loc 0) intType
    | A.NOTHING ->
      let res = new_exp ~loc (Const(CStr "exp_nothing")) in
      finishExp [] (unspecified_chunk empty) res (typeOf res)
    (* Do the potential lvalues first *)
    | A.VARIABLE n -> begin
        if is_stdlib_function_macro n then begin
          (* These must be macros. They can be implemented with a function
             of the same name, but in that case, it is not possible to
             take the address of the function (or do anything else than
             calling the function, which is matched later on). *)
          Kernel.warning ~wkey:Kernel.wkey_cert_msc_38
            "%s is a standard macro. Its definition cannot be suppressed, \
             see CERT C coding rules MSC38-C" n
        end;
        (* Look up in the environment *)
        try
          let envdata = H.find env n in
          match envdata with
          | EnvVar vi, _ ->
            let lval = var vi in
            let reads =
              if
                (* Always allow to read the address of an
                   array, as it will never be written to:
                   no read/write interference is possible. *)
                Cil.isArrayType vi.vtype ||
                Lval.Set.mem lval local_env.authorized_reads
              then []
              else [ lval ]
            in
            (* if isconst &&
               not (isFunctionType vi.vtype) &&
               not (isArrayType vi.vtype)then
               Cil.error "variable appears in constant"; *)
            finishExp
              reads (unspecified_chunk empty)
              (new_exp ~loc (Lval lval)) (dropQualifiers vi.vtype)
          | EnvEnum item, _ ->
            let typ = Cil.typeOf item.eival in
            (*Kernel.debug "Looking for %s got enum %s : %a of type %a"
              n item.einame Cil_printer.pp_exp item.eival
              Cil_printer.pp_typ typ; *)
            if Cil.theMachine.Cil.lowerConstants then
              finishExp [] (unspecified_chunk empty) item.eival typ
            else
              finishExp []
                (unspecified_chunk empty)
                (new_exp ~loc (Const (CEnum item)))
                typ
          | _ -> raise Not_found
        with Not_found -> begin
            if isOldStyleVarArgName n then
              Kernel.fatal ~current:true
                "Cannot resolve variable %s. \
                 This could be a CIL bug due to the handling of old-style variable argument \
                 functions"
                n
            else
              Kernel.fatal ~current:true "Cannot resolve variable %s" n
          end
      end
    | A.INDEX (e1, e2) -> begin
        (* Recall that doExp turns arrays into StartOf pointers *)
        let (r1, se1, e1', t1) =
          doExp (no_paren_local_env local_env) false e1 (AExp None) in
        let (r2,se2, e2', t2) =
          doExp (no_paren_local_env local_env) false e2 (AExp None) in
        let se = se1 @@ (se2, ghost) in
        let (e1'', t1, e2'', tresult) =
          (* Either e1 or e2 can be the pointer *)
          match unrollType t1, unrollType t2 with
          | TPtr(t1e,_), (TInt _|TEnum _) -> e1', t1, e2', t1e
          | (TInt _|TEnum _), TPtr(t2e,_) -> e2', t2, e1', t2e
          | _ ->
            Kernel.fatal ~current:true
              "Expecting exactly one pointer type in array access %a[%a] (%a \
               and %a)"
              Cil_printer.pp_exp e1' Cil_printer.pp_exp e2'
              Cil_printer.pp_typ t1 Cil_printer.pp_typ t2
        in
        (* We have to distinguish the construction based on the type of e1'' *)
        let res =
          match e1''.enode with
          | StartOf array -> (* A real array indexing operation *)
            addOffsetLval (Index(e2'', NoOffset)) array
          | _ -> (* Turn into *(e1 + e2) *)
            mkMem
              (new_exp ~loc:e1''.eloc (BinOp(IndexPI, e1'', e2'', t1)))
              NoOffset
        in
        (* Do some optimization of StartOf *)
        let reads =
          let l = r1 @ r2 in
          if Lval.Set.mem res local_env.authorized_reads
          then l
          else res :: l
        in
        finishExp reads se (new_exp ~loc (Lval res)) (dropQualifiers tresult)
      end
    | A.UNARY (A.MEMOF, e) ->
      if asconst then
        Kernel.warning ~current:true "MEMOF in constant";
      let (r,se, e', t) =
        doExp (no_paren_local_env local_env) false e (AExp None)
      in
      let tresult =
        match unrollType t with
        | TPtr(te, _) -> te
        | _ ->
          Kernel.fatal ~current:true
            "Expecting a pointer type in *. Got %a."
            Cil_printer.pp_typ t
      in
      let res = mkMem e' NoOffset in
      let reads =
        if Lval.Set.mem res local_env.authorized_reads
        then r
        else res :: r
      in
      finishExp reads se (new_exp ~loc (Lval res)) (dropQualifiers tresult)

    (* e.str = (& e + off(str)). If e = (be + beoff) then e.str = (be
     * + beoff + off(str))  *)
    | A.MEMBEROF (e, str) ->
      (* member of is actually allowed if we only take the address *)
      (* if isconst then Cil.error "MEMBEROF in constant";  *)
      let (r,se, e', t') =
        doExp (no_paren_local_env local_env) false e (AExp None)
      in
      let lv =
        match e'.enode with
        | Lval x -> x
        | CastE(_, { enode = Lval x}) -> x
        | _ ->
          Kernel.fatal ~current:true
            "Expected an lval in MEMBEROF (field %s)"
            str
      in
      (* We're not reading the whole lval, just a chunk of it. *)
      let r =
        List.filter (fun x -> not (Lval.equal x lv)) r
      in
      let field_offset =
        match unrollType t' with
        | TComp (comp, _, _) -> findField str comp
        | _ ->
          Kernel.fatal ~current:true "expecting a struct with field %s" str
      in
      let lv' = addOffsetLval field_offset lv in
      let field_type = typeOfLval lv' in
      let reads =
        if Lval.Set.mem lv' local_env.authorized_reads
        then r
        else lv':: r
      in
      finishExp reads se (new_exp ~loc (Lval lv')) (dropQualifiers field_type)

    (* e->str = * (e + off(str)) *)
    | A.MEMBEROFPTR (e, str) ->
      if asconst then Kernel.warning ~current:true "MEMBEROFPTR in constant";
      let (r,se, e', t') =
        doExp (no_paren_local_env local_env) false e (AExp None)
      in
      let pointedt = match unrollType t' with
        | TPtr(t1, _) -> t1
        | TArray(t1,_,_,_) -> t1
        | _ -> Kernel.fatal ~current:true "expecting a pointer to a struct"
      in
      let field_offset = match unrollType pointedt with
        | TComp (comp, _, _) -> findField str comp
        | x ->
          Kernel.fatal ~current:true
            "expecting a struct with field %s. Found %a. t1 is %a"
            str Cil_printer.pp_typ x Cil_printer.pp_typ t'
      in
      let lv' = mkMem e' field_offset in
      let field_type = typeOfLval lv' in
      let reads =
        if Lval.Set.mem lv' local_env.authorized_reads
        then r
        else lv' :: r
      in
      finishExp reads se (new_exp ~loc (Lval lv')) (dropQualifiers field_type)

    | A.CONSTANT ct -> begin
        let hasSuffix str =
          let l = String.length str in
          fun s ->
            let ls = String.length s in
            l >= ls &&
            s =
            Transitioning.String.uppercase_ascii (String.sub str (l - ls) ls)
        in
        match ct with
        | A.CONST_INT str -> begin
            let res = parseIntExp ~loc str in
            finishExp [] (unspecified_chunk empty) res (typeOf res)
          end

        | A.CONST_WSTRING (ws: int64 list) ->
          let res =
            new_exp ~loc
              (Const(CWStr ((* intlist_to_wstring *) ws)))
          in
          finishExp [] (unspecified_chunk empty) res (typeOf res)

        | A.CONST_STRING s ->
          (* Maybe we buried __FUNCTION__ in there *)
          let s' =
            try
              let start = String.index s (Char.chr 0) in
              let l = String.length s in
              let tofind = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" in
              let past = start + String.length tofind in
              if past <= l &&
                 String.sub s start (String.length tofind) = tofind then
                (if start > 0 then String.sub s 0 start else "") ^
                !currentFunctionFDEC.svar.vname ^
                (if past < l then String.sub s past (l - past) else "")
              else
                s
            with Not_found -> s
          in
          let res = new_exp ~loc (Const(CStr s')) in
          finishExp [] (unspecified_chunk empty) res (typeOf res)

        | A.CONST_CHAR char_list ->
          let a, b = (interpret_character_constant char_list) in
          finishExp [] (unspecified_chunk empty) (new_exp ~loc (Const a)) b

        | A.CONST_WCHAR char_list ->
          (* matth: I can't see a reason for a list of more than one char
           * here, since the kinteger64 below will take only the lower 16
           * bits of value.  ('abc' makes sense, because CHAR constants have
           * type int, and so more than one char may be needed to represent
           * the value.  But L'abc' has type wchar, and so is equivalent to
           * L'c').  But gcc allows L'abc', so I'll leave this here in case
           * I'm missing some architecture dependent behavior. *)
          let value = reduce_multichar theMachine.wcharType char_list in
          let result = kinteger64 ~loc ~kind:theMachine.wcharKind
              (Integer.of_int64 value)
          in
          finishExp [] (unspecified_chunk empty) result (typeOf result)

        | A.CONST_FLOAT str -> begin
            (* Maybe it ends in F or L. Strip those *)
            let l = String.length str in
            let hasSuffix = hasSuffix str in
            let baseint, kind =
              if  hasSuffix "L" then
                String.sub str 0 (l - 1), FLongDouble
              else if hasSuffix "F" then
                String.sub str 0 (l - 1), FFloat
              else if hasSuffix "D" then
                String.sub str 0 (l - 1), FDouble
              else
                str, FDouble
            in
            try
              Floating_point.set_round_nearest_even ();
              let open Floating_point in
              let basefloat = parse_kind kind baseint in
              begin
                if basefloat.f_lower <> basefloat.f_upper then
                  Kernel.warning ~wkey:Kernel.wkey_decimal_float
                    ~current:true
                    "Floating-point constant %s is not represented exactly. Will use %a."
                    str (Floating_point.pretty_normal ~use_hex:true) basefloat.f_nearest;
              end ;
              let node = Const(CReal(basefloat.f_nearest, kind, Some str)) in
              finishExp [] (unspecified_chunk empty) (new_exp ~loc node) (TFloat(kind,[]))
            with Failure s -> begin
                Kernel.error ~once:true ~current:true "float_of_string %s (%s)\n" str s;
                let res = new_exp ~loc (Const(CStr "booo CONS_FLOAT")) in
                finishExp [] (unspecified_chunk empty) res (typeOf res)
              end
          end
      end

    | A.TYPE_SIZEOF (bt, dt) ->
      let typ = doOnlyType local_env.is_ghost bt dt in
      finishExp [] (unspecified_chunk empty) (new_exp ~loc (SizeOf(typ)))
        theMachine.typeOfSizeOf

    | A.EXPR_SIZEOF e ->
      (* Allow non-constants in sizeof *)
      (* Do not convert arrays and functions into pointers. *)
      let (_, se, e', lvt) =
        doExp (no_paren_local_env local_env) false e AExpLeaveArrayFun
      in
      if Cil.isFunctionType lvt && Cil.theMachine.theMachine.sizeof_fun < 0 then
        Kernel.abort ~current:true
          "sizeof() called on function";
      let scope_chunk = drop_chunk "sizeof" se e e' in
      let size =
        match e'.enode with
        (* Maybe we are taking the sizeof a variable-sized array *)
        | Lval (Var vi, NoOffset) -> begin
            try
              IH.find varSizeArrays vi.vid
            with Not_found -> new_exp ~loc (SizeOfE e')
          end
        | Const (CStr s) -> new_exp ~loc (SizeOfStr s)
        | _ -> new_exp ~loc (SizeOfE e')
      in
      finishExp [] scope_chunk size theMachine.typeOfSizeOf

    | A.TYPE_ALIGNOF (bt, dt) ->
      let typ = doOnlyType local_env.is_ghost bt dt in
      finishExp [] (unspecified_chunk empty) (new_exp ~loc (AlignOf(typ)))
        theMachine.typeOfSizeOf

    | A.EXPR_ALIGNOF e ->
      let (_, se, e', lvt) =
        doExp (no_paren_local_env local_env) false e AExpLeaveArrayFun
      in
      if Cil.isFunctionType lvt && Cil.theMachine.theMachine.alignof_fun < 0
      then
        Kernel.abort ~current:true "alignof() called on a function.";
      let scope_chunk = drop_chunk "alignof" se e e' in
      let e'' =
        match e'.enode with (* If we are taking the alignof an
                             * array we must drop the StartOf  *)
        | StartOf(lv) -> new_exp ~loc:e'.eloc (Lval(lv))

        | _ -> e'
      in
      finishExp [] scope_chunk (new_exp ~loc (AlignOfE(e'')))
        theMachine.typeOfSizeOf

    | A.CAST ((specs, dt), ie) ->
      let s', dt', ie' = preprocessCast local_env.is_ghost specs dt ie in
      (* We know now that we can do s' and dt' many times *)
      let typ = doOnlyType local_env.is_ghost s' dt' in
      let what' =
        match what with
        | AExp (Some _) -> AExp (Some typ)
        | AExp None -> what
        | ADrop | AType | AExpLeaveArrayFun -> what
        | ASet (_, _, _, lvt) ->
          (* If the cast from typ to lvt would be dropped, then we
           * continue with a Set *)
          if false && Cil_datatype.Typ.equal typ lvt then
            what
          else
            AExp None (* We'll create a temporary *)
      in
      (* Remember here if we have done the Set *)
      let (r,se, e', t'), (needcast: bool) =
        match ie' with
        | A.SINGLE_INIT e ->
          doExp (no_paren_local_env local_env) asconst e what', true

        | A.NO_INIT -> Kernel.fatal ~current:true "missing expression in cast"

        | A.COMPOUND_INIT _ -> begin
            (* Pretend that we are declaring and initializing a brand new
             * variable  *)
            let newvar = "__constr_expr_" ^ string_of_int (!constrExprId) in
            incr constrExprId;
            let spec_res = doSpecList local_env.is_ghost "" s' in
            let se1 =
              if !scopes == [] then begin
                (* This is a global.  Mark the new vars as static *)
                let spec_res' =
                  let t, _, inl, attrs = spec_res in
                  t, Static, inl, attrs
                in
                ignore (createGlobal local_env.is_ghost None spec_res'
                          ((newvar, dt', [], loc), ie'));
                (unspecified_chunk empty)
              end else
                createLocal
                  local_env.is_ghost spec_res ((newvar, dt', [], loc), ie')
            in
            (* Now pretend that e is just a reference to the newly created
             * variable *)
            let v = { expr_node = A.VARIABLE newvar; expr_loc = loc } in
            let r, se, e', t' =
              doExp (no_paren_local_env local_env) asconst v what'
            in
            (* If typ is an array then the doExp above has already added a
             * StartOf. We must undo that now so that it is done once by
             * the finishExp at the end of this case *)
            let e2, t2 =
              match unrollType typ, e'.enode with
              | TArray _, StartOf lv -> new_exp ~loc (Lval lv), typ
              | _, _ -> e', t'
            in
            (* If we are here, then the type t2 is guaranteed to match the
             * type of the expression e2, so we do not need a cast. We have
             * to worry about this because otherwise, we might need to cast
             * between arrays or structures. *)
            (r, se1 @@ (se, ghost), e2, t2), false
          end
      in
      let (t'', e'') =
        match typ with
        | TVoid _ when what' = ADrop -> (t', e') (* strange GNU thing *)
        |  _ ->
          (* Do this to check the cast, unless we are sure that we do not
           * need the check. *)
          let newtyp, newexp =
            if needcast then
              castTo ~fromsource:true t' typ e'
            else
              t', e'
          in
          newtyp, newexp
      in
      finishExp r se e'' t''

    | A.UNARY(A.MINUS, e) ->
      let (r, se, e', t) =
        doExp (no_paren_local_env local_env) asconst e (AExp None)
      in
      if isIntegralType t then
        let tres = integralPromotion t in
        let e'' = new_exp ~loc (UnOp(Neg, makeCastT e' t tres, tres)) in
        finishExp r se e'' tres
      else
      if isArithmeticType t then
        finishExp r se (new_exp ~loc:e'.eloc (UnOp(Neg,e',t))) t
      else
        Kernel.fatal ~current:true "Unary - on a non-arithmetic type"

    | A.UNARY(A.BNOT, e) ->
      let (r, se, e', t) =
        doExp (no_paren_local_env local_env) asconst e (AExp None)
      in
      if isIntegralType t then
        let tres = integralPromotion t in
        let e'' = new_exp ~loc (UnOp(BNot, makeCastT e' t tres, tres)) in
        finishExp r se e'' tres
      else
        Kernel.fatal ~current:true "Unary ~ on a non-integral type"

    | A.UNARY(A.PLUS, e) -> doExp (no_paren_local_env local_env) asconst e what

    | A.UNARY(A.ADDROF, e) ->
      (* some normalization is needed here to remove potential COMMA, QUESTION
         and PAREN. the normalization will take care of setting
         local_env.is_paren as appropriate while removing PAREN. *)
      let action local_env e what =
        match e.expr_node with
        | A.COMMA _ | A.QUESTION _ | A.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | A.VARIABLE s when
            isOldStyleVarArgName s
            && (match !currentFunctionFDEC.svar.vtype with
                  TFun(_, _, true, _) -> true | _ -> false) ->
          (* We are in an old-style variable argument function and we are
           * taking the address of the argument that was removed while
           * processing the function type. We compute the address based on
           * the address of the last real argument *)
          if Cil.msvcMode () then begin
            let rec getLast = function
              | [] ->
                Kernel.fatal ~current:true
                  "old-style variable argument function without real \
                   arguments"
              | [ a ] -> a
              | _ :: rest -> getLast rest
            in
            let last = getLast !currentFunctionFDEC.sformals in
            let res = mkAddrOfAndMark e.expr_loc (var last) in
            let tres = typeOf res in
            let tres', res' = castTo tres (TInt(IULong, [])) res in
            (* Now we must add to this address to point to the next
             * argument. Round up to a multiple of 4  *)
            let sizeOfLast =
              (((bitsSizeOf last.vtype) + 31) / 32) * 4
            in
            let res'' =
              new_exp ~loc
                (BinOp(PlusA, res', kinteger ~loc IULong sizeOfLast, tres'))
            in
            let lv = var last in
            let reads =
              if Lval.Set.mem lv local_env.authorized_reads
              then []
              else [ lv ]
            in
            finishExp reads (unspecified_chunk empty) res'' tres'
          end else begin (* On GCC the only reliable way to do this is to
                          * call builtin_next_arg. If we take the address of
                          * a local we are going to get the address of a copy
                          * of the local ! *)

            doExp local_env asconst
              (cabs_exp loc
                 (A.CALL (cabs_exp loc (A.VARIABLE "__builtin_next_arg"),
                          [cabs_exp loc (A.CONSTANT (A.CONST_INT "0"))])))
              what
          end

        | A.VARIABLE _ | A.UNARY (A.MEMOF, _) (* Regular lvalues *)
        | A.CONSTANT (A.CONST_STRING _) | A.CONSTANT (A.CONST_WSTRING _)
        | A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _
        | A.CAST (_, A.COMPOUND_INIT _) ->
          begin
            let (r, se, e', t) =
              doExp local_env false e (AExp None)
            in
            (* ignore (E.log "ADDROF on %a : %a\n" Cil_printer.pp_exp e'
               Cil_printer.pp_typ t); *)
            match e'.enode with
            | (Lval x | CastE(_, {enode = Lval x})) ->
              let reads =
                match x with
                | Mem _ ,_ -> r (* we're not really reading the
                                     pointed value, just calculating an
                                     offset. *)
                | Var _,_ ->
                  if Lval.Set.mem x local_env.authorized_reads
                  then r
                  else x :: r
              in
              (* Recover type qualifiers that were dropped by dropQualifiers
                 when the l-value was created *)
              let tres = match e'.enode with
                | Lval x -> Cil.typeOfLval x
                | _ -> t
              in
              finishExp reads se (mkAddrOfAndMark loc x) (TPtr(tres, []))

            | StartOf (lv) ->
              let tres = TPtr(typeOfLval lv, []) in (* pointer to array *)
              let reads =
                match lv with
                | Mem _, _ -> r (* see above *)
                | Var _,_ ->
                  if Lval.Set.mem lv local_env.authorized_reads
                  then r
                  else lv :: r
              in
              finishExp reads se (mkAddrOfAndMark loc lv) tres

            | Const (CStr _ | CWStr _) ->
              (* string to array *)
              finishExp r se e' (TPtr(t, []))

            (* Function names are converted into pointers to the function.
             * Taking the address-of again does not change things *)
            | AddrOf (Var v, NoOffset) when isFunctionType v.vtype ->
              finishExp r se e' t

            | _ ->
              Kernel.fatal ~current:true "Expected lval for ADDROF. Got %a"
                Cil_printer.pp_exp e'
          end
        | _ -> Kernel.fatal ~current:true "Unexpected operand for addrof"
      in
      normalize_unop A.ADDROF action false (no_paren_local_env local_env) e what
    | A.UNARY((A.PREINCR|A.PREDECR) as uop, e) ->
      let action local_env e _what =
        match e.expr_node with
        | A.COMMA _ | A.QUESTION _ | A.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
           A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ |
           A.CAST _ (* A GCC extension *)) -> begin
            let uop' = if uop = A.PREINCR then PlusA else MinusA in
            if asconst then
              Kernel.warning ~current:true "PREINCR or PREDECR in constant";
            let (r, se, e', t) = doExp local_env false e (AExp None) in
            let lv =
              match e'.enode with
              | Lval x -> x
              | CastE (_, {enode = Lval x}) -> x
              (* A GCC extension. The operation is
               * done at the cast type. The result
               * is also of the cast type *)
              | _ -> Kernel.fatal ~current:true "Expected lval for ++ or --"
            in
            let se' = remove_reads lv se in
            let r' =
              List.filter (fun x -> not (Lval.equal x lv)) r
            in
            let tresult, result =
              doBinOp loc uop' e' t (one ~loc:e'.eloc) intType
            in
            finishExp []
              (se' +++
               (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                  (Set(lv, makeCastT result tresult t,
                       CurrentLoc.get ())),[],[lv],r'))
              e'
              t
          end
        | _ ->
          Kernel.fatal ~current:true "Unexpected operand for prefix -- or ++"
      in
      normalize_unop uop action asconst (no_paren_local_env local_env) e what

    | A.UNARY((A.POSINCR|A.POSDECR) as uop, e) ->
      let action local_env e what =
        match e.expr_node with
        | A.COMMA _ | A.QUESTION _ | A.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | A.VARIABLE _ | A.UNARY (A.MEMOF, _) (* Regular lvalues *)
        | A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _
        | A.CAST _ (* A GCC extension *) -> begin
            if asconst then
              Kernel.warning ~current:true "POSTINCR or POSTDECR in constant";
            (* If we do not drop the result then we must save the value *)
            let uop' = if uop = A.POSINCR then PlusA else MinusA in
            let (r,se, e', t) = doExp local_env false e (AExp None) in
            let lv =
              match e'.enode with
              | Lval x -> x
              | CastE (_, {enode = Lval x}) -> x
              (* GCC extension. The addition must
               * be be done at the cast type. The
               * result of this is also of the cast
               * type *)
              | _ -> Kernel.fatal ~current:true "Expected lval for ++ or --"
            in
            let se' = remove_reads lv se in
            let r' =
              List.filter (fun x -> not (Lval.equal x lv)) r
            in
            let tresult, opresult =
              doBinOp loc uop' e' t (one ~loc:e'.eloc)
                intType
            in
            let reads, se', result =
              if what <> ADrop && what <> AType then
                let descr =
                  Format.asprintf "%a%s"
                    Cil_descriptive_printer.pp_exp  e'
                    (if uop = A.POSINCR then "++" else "--") in
                let tmp = newTempVar descr true t in
                ([var tmp],
                 local_var_chunk se' tmp +++
                 (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                    (Set(var tmp, e', CurrentLoc.get ())),[],[],[]),
                 (* the tmp variable should not be investigated for
                    unspecified writes: it occurs at the right place in
                    the sequence.
                 *)
                 new_exp ~loc (Lval(var tmp)))
              else
                [],se, e'
            in
            finishExp reads
              (se' +++
               (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                  (Set(lv,
                       makeCastT opresult tresult (typeOfLval lv),
                       CurrentLoc.get ())),
                [],[lv], r'))
              result
              t
          end
        | _ ->
          Kernel.fatal ~current:true "Unexpected operand for suffix ++ or --"
      in
      normalize_unop uop action asconst (no_paren_local_env local_env) e what

    | A.BINARY(A.ASSIGN, e1, e2) ->
      let action local_env asconst e what =
        match e.expr_node with
        | A.COMMA _ | A.QUESTION _ | A.CAST (_,A.SINGLE_INIT _) | A.PAREN _ ->
          Kernel.fatal
            ~current:true "normalization of lval in assignment failed"
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
           A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ ) -> begin
            if asconst then Kernel.warning ~current:true "ASSIGN in constant";
            let se0 = unspecified_chunk empty in
            let (r1,se1, e1', lvt) = doExp local_env false e (AExp None) in
            let lv =
              match e1'.enode with
              | Lval x when Cil.is_modifiable_lval x -> x
              | Lval x ->
                Kernel.abort ~current:true
                  "Cannot assign to non-modifiable lval %a"
                  Cil_printer.pp_lval x
              | StartOf lv ->
                Kernel.abort ~current:true
                  "Cannot assign array %a" Cil_printer.pp_lval lv
              | _ ->
                Kernel.abort ~current:true
                  "Expected lval for assignment. Got %a"
                  Cil_printer.pp_exp e1'
            in
            let se1' = remove_reads lv se1 in
            let r1' = List.filter (fun x -> not (Lval.equal x lv)) r1 in
            let local_env =
              { local_env with
                authorized_reads =
                  Lval.Set.add lv local_env.authorized_reads }
            in
            (*[BM]: is this useful?
              let (_, _, _) = doExp ghost false e2 (ASet(lv, lvt)) in*)
            (* Catch the case of an lval that might depend on itself,
               e.g. p[p[0]] when p[0] == 0.  We need to use a temporary
               here if the result of the expression will be used:
               tmp := e2; lv := tmp; use tmp as the result
               Test: small1/assign.c *)
            let needsTemp =
              not (isBitfield lv) && (* PC: BTS 933, 968 *)
              match what, lv with
              | (ADrop|AType), _ -> false
              | _, (Mem e, off) ->
                not (isConstant e) || not (isConstantOffset off)
              | _, (Var _, off) -> not (isConstantOffset off)
            in
            let r1, tmplv, se3 =
              if needsTemp then
                let descr = 
                  Format.asprintf "%a" Cil_descriptive_printer.pp_lval lv
                in
                let tmp = newTempVar descr true lvt in
                let chunk =
                  i2c
                    (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                       (Set(lv, new_exp ~loc:e1'.eloc (Lval(var tmp)), loc)),
                     [lv],[lv], r1')
                in
                ([],var tmp, local_var_chunk chunk tmp)
              else r1',lv, empty
            in
            let (r2,se2, _, _) =
              doExp local_env false e2 (ASet (not needsTemp, tmplv,  r1, lvt))
            in
            let (@@) s1 s2 = s1 @@ (s2, ghost) in
            (* Format.eprintf "chunk for assigns is %a@." d_chunk se2; *)
            (* r1 is read in the assignment part itself *)
            finishExp r2  ((empty @@ ((se0 @@ se1') @@ se2)) @@ se3)
              (new_exp ~loc (Lval tmplv)) lvt
          end
        | _ -> Kernel.fatal ~current:true "Invalid left operand for ASSIGN"
      in
      normalize_binop
        A.ASSIGN action (no_paren_local_env local_env) asconst e1 e2 what
    | A.BINARY((A.ADD|A.SUB|A.MUL|A.DIV|A.MOD|A.BAND|A.BOR|A.XOR|
                A.SHL|A.SHR|A.EQ|A.NE|A.LT|A.GT|A.GE|A.LE) as bop,
               e1, e2) ->
      let check_bitwise = is_bitwise_bop bop && not local_env.is_paren in
      let se0 = unspecified_chunk empty in
      let bop' = convBinOp bop in
      let (r1,se1, e1', t1) =
        doExp (no_paren_local_env local_env) asconst e1 (AExp None) in
      let (r2,se2, e2', t2) =
        doExp (no_paren_local_env local_env) asconst e2 (AExp None) in
      if check_bitwise then begin
        check_logical_operand e1 t1;
        check_logical_operand e2 t2;
      end;
      let tresult, result = doBinOp loc bop' e1' t1 e2' t2 in
      let (@@) s1 s2 = s1 @@ (s2, ghost) in
      finishExp (r1 @ r2) ((se0 @@ se1) @@ se2) result tresult

    (* assignment operators *)
    | A.BINARY((A.ADD_ASSIGN|A.SUB_ASSIGN|A.MUL_ASSIGN|A.DIV_ASSIGN|
                A.MOD_ASSIGN|A.BAND_ASSIGN|A.BOR_ASSIGN|A.SHL_ASSIGN|
                A.SHR_ASSIGN|A.XOR_ASSIGN) as bop, e1, e2) ->
        let se0 = unspecified_chunk empty in
        let action local_env asconst e _what =
          match e.expr_node with
          | A.COMMA _ | A.QUESTION _ | A.PAREN _ ->
            Kernel.fatal "normalization of lval in compound assignment failed"
          | A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
             A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ |
           A.CAST _ (* GCC extension *) -> begin
            if asconst then
              Kernel.warning ~current:true "op_ASSIGN in constant";
            let bop' = match bop with
              | A.ADD_ASSIGN -> PlusA
              | A.SUB_ASSIGN -> MinusA
              | A.MUL_ASSIGN -> Mult
              | A.DIV_ASSIGN -> Div
              | A.MOD_ASSIGN -> Mod
              | A.BAND_ASSIGN -> BAnd
              | A.BOR_ASSIGN -> BOr
              | A.XOR_ASSIGN -> BXor
              | A.SHL_ASSIGN -> Shiftlt
              | A.SHR_ASSIGN -> Shiftrt
              | _ -> Kernel.fatal ~current:true "binary +="
            in
            let (r1,se1, e1', t1) = doExp local_env false e (AExp None) in
            let lv1 =
              match e1'.enode with
              | Lval x -> x
              | CastE (_, {enode = Lval x}) -> x
              (* GCC extension. The operation and
               * the result are at the cast type  *)
              | _ ->
                Kernel.fatal ~current:true
                  "Expected lval for assignment with arith"
            in
            let se1' = remove_reads lv1 se1 in
            let r1' = List.filter (fun x -> not (Lval.equal x lv1)) r1 in
            let local_env =
              { local_env with
                authorized_reads =
                  Lval.Set.add lv1 local_env.authorized_reads }
            in
            let (r2, se2, e2', t2) = doExp local_env false e2 (AExp None) in
            let se2 = remove_reads lv1 se2 in
            let tresult, result = doBinOp loc bop' e1' t1 e2' t2 in
            (* We must cast the result to the type of the lv1, which may be
             * different than t1 if lv1 was a Cast *)
            let _, result' = castTo tresult (typeOfLval lv1) result in
            (* The type of the result is the type of the left-hand side  *)
            let (@@) s1 s2 = s1 @@ (s2, ghost) in
            finishExp []
              (se0 @@
               (empty @@ (se1' @@ se2) +++
                         (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                            (Set(lv1, result', loc)),
                          [lv1],[lv1], r1' @ r2)))
              e1'
              t1
          end
        | _ ->
          Kernel.fatal ~current:true
            "Unexpected left operand for assignment with arith"
      in
      normalize_binop
        bop action (no_paren_local_env local_env) asconst e1 e2 what
    | A.BINARY((A.AND|A.OR), _, _) | A.UNARY(A.NOT, _) -> begin
        let ce = doCondExp local_env asconst e in
        (* We must normalize the result to 0 or 1 *)
        match ce with
        | CEExp (se, ({enode = Const c;eloc=loc})) ->
          finishExp [] se
            (match isConstTrueFalse c with
             | `CTrue -> one ~loc
             | `CFalse -> zero ~loc)
            intType
        | CEExp (se, ({enode = UnOp(LNot, _, _)} as e)) ->
          (* already normalized to 0 or 1 *)
          finishExp [] se e intType
        | CEExp (se, e) ->
          let e' =
            let te = typeOf e in
            let _, zte = castTo intType te (zero ~loc:e.eloc) in
            new_exp ~loc (BinOp(Ne, e, zte, intType))
          in
          finishExp [] se e' intType
        | _ ->
          let tmp =
            newTempVar "<boolean expression>" true intType
          in
          let condChunk =
            compileCondExp ~ghost ce
              (empty +++
               (mkStmtOneInstr ~ghost ~valid_sid
                  (Set(var tmp, integer ~loc 1,loc)),[],[],[]))
              (empty +++
               (mkStmtOneInstr ~ghost ~valid_sid
                  (Set(var tmp, integer ~loc 0,loc)),[],[],[]))
          in
          finishExp []
            (local_var_chunk condChunk tmp)
            (new_exp ~loc (Lval (var tmp)))
            intType
      end

    | A.CALL(f, args) ->
      let (rf,sf, f', ft') =
        match (stripParen f).expr_node with
        (* Treat the VARIABLE case separate because we might be calling a
         * function that does not have a prototype. In that case assume it
         * takes INTs as arguments  *)
        | A.VARIABLE n -> begin
            try
              (* First look for polymorphic builtins. The typing rule is 
                 luckily always the same one. *)
              let n = match n with 
                | "__sync_add_and_fetch" | "__sync_sub_and_fetch"
                | "__sync_or_and_fetch" | "__sync_and_and_fetch"
                | "__sync_xor_and_fetch" | "__sync_nand_and_fetch"
                | "__sync_fetch_and_add" | "__sync_fetch_and_sub"
                | "__sync_fetch_and_or" | "__sync_fetch_and_and"
                | "__sync_fetch_and_xor" | "__sync_fetch_and_nand"
                | "__sync_bool_compare_and_swap"
                | "__sync_val_compare_and_swap"
                | "__sync_lock_release" | "__sync_lock_test_and_set" -> 
                  begin
                    match args with 
                    | a1::_ -> 
                      (* The available prototypes are
                         typ' f(typ* a1,typ a2,typ a3,...);
                         typ' f(typ* a1,typ a2,...);
                         typ' f(typ* a1,...);
                         Hence we just infer the right type
                         looking at the first argument. *)
                      let _,c,_,t =
                        doExp (no_paren_local_env local_env) false a1 AType
                      in
                      clean_up_chunk_locals c;
                      let t = typeOf_pointed t in
                      Format.sprintf "%s_%sint%d_t"
                        n
                        (if isSignedInteger t then "" else "u")
                        (bitsSizeOf t)
                    | [] -> 
                      Kernel.error ~once:true ~current:true 
                        "Too few arguments for builtin %s" n;
                      n
                  end
                | _ -> n 
              in
              let vi, _ = lookupVar n in
              let reads =
                if Lval.Set.mem
                    (var vi) local_env.authorized_reads
                   ||
                   (vi.vglob && Cil.isFunctionType vi.vtype)
                then []
                else [ var vi ]
              in
              (reads, unspecified_chunk empty,
               new_exp ~loc:f.expr_loc (Lval(var vi)), vi.vtype)
            (* Found. Do not use finishExp. Simulate what = AExp None  *)
            with Not_found -> begin
                Kernel.debug ~level:3
                  "Calling function %s without prototype." n ;
                let ftype = TFun(intType, None, false,
                                 [Attr("missingproto",[])]) in
                (* Add a prototype to the environment *)
                let proto, _ =
                  makeGlobalVarinfo false
                    (makeGlobalVar ~temp:false n ftype) in
                (* Make it EXTERN *)
                proto.vstorage <- Extern;
                IH.add noProtoFunctions proto.vid true;
                proto.vdecl <- f.expr_loc;
                ImplicitPrototypeHook.apply proto;
                (* Add it to the file as well *)
                cabsPushGlobal
                  (GFunDecl (empty_funspec (),proto, f.expr_loc));
                ([var proto],unspecified_chunk empty,
                 new_exp ~loc:f.expr_loc (Lval(var proto)), ftype)
              end
          end
        | _ -> doExp (no_paren_local_env local_env) false f (AExp None)
      in
      (* Get the result type and the argument types *)
      let (resType, argTypes, isvar, f'',attrs) =
        match unrollType ft' with
        | TFun(rt,at,isvar,attrs) -> (rt,at,isvar,f',attrs)
        | TPtr (t, _) -> begin
            match unrollType t with
            | TFun(rt,at,isvar,_) -> (* Make the function pointer
                                      * explicit  *)
              let f'' =
                match f'.enode with
                | AddrOf lv -> new_exp ~loc:f'.eloc (Lval(lv))
                | _ -> 
                  new_exp ~loc:f'.eloc 
                    (Lval (mkMem f' NoOffset))
              in
              (rt,at,isvar, f'',[])
            | x ->
              Kernel.fatal ~current:true
                "Unexpected type of the called function %a: %a"
                Cil_printer.pp_exp f' Cil_printer.pp_typ x
          end
        | x ->
          Kernel.fatal ~current:true
            "Unexpected type of the called function %a: %a"
            Cil_printer.pp_exp f' Cil_printer.pp_typ x
      in
      let argTypesList = argsToList argTypes in
      let warn_no_proto f =
        (* Do not punish twice users of completely undeclared functions. *)
        if not (Cil.typeHasAttribute "missingproto" f.vtype) then
          Kernel.warning ~source:(fst loc) ~wkey:Kernel.wkey_no_proto
            "Calling function %a that is declared without prototype.@ \
             Its formals will be inferred from actual arguments"
            Cil_printer.pp_varinfo f
      in
      (* Drop certain qualifiers from the result type *)
      let resType' = typeRemoveAttributes ["warn_unused_result"] resType in
      (* Before we do the arguments we try to intercept a few builtins. For
       * these we have defined then with a different type, so we do not
       * want to give warnings. We'll just leave the arguments of these
       * functions alone*)
      let isSpecialBuiltin =
        match f''.enode with
        | Lval (Var fv, NoOffset) -> Cil.is_special_builtin fv.vname
        | _ -> false
      in
      let init_chunk = unspecified_chunk empty in
      (* Do the arguments. In REVERSE order !!! Both GCC and MSVC do this *)
      let rec loopArgs = function
        | ([], []) ->
          (match argTypes, f''.enode with
           | None, Lval (Var f,NoOffset) ->
             (* we call a function without prototype with 0 argument.
                Hence, it really has no parameter.
             *)
             if not isSpecialBuiltin then begin
               warn_no_proto f;
               let typ = TFun (resType, Some [], false,attrs) in
               Cil.update_var_type f typ;
             end
           | None, _ (* TODO: treat function pointers. *)
           | Some _, _ -> ()
          );
          (init_chunk, [])

        | _, [] ->
          if not isSpecialBuiltin then
            Kernel.error ~once:true ~current:true
              "Too few arguments in call to %a." Cil_printer.pp_exp f' ;
          (init_chunk, [])

        | ((_, at, _) :: atypes, a :: args) ->
          let (ss, args') = loopArgs (atypes, args) in
          (* Do not cast as part of translating the argument. We let
           * the castTo do this work. This was necessary for
           * test/small1/union5, in which a transparent union is passed
           * as an argument *)
          let (sa, a', att) =
            let (r, c, e, t) =
              doExp (no_paren_local_env local_env) false a (AExp None)
            in
            (add_reads ~ghost:local_env.is_ghost loc r c, e, t)
          in
          let (texpected, a'') = castTo att at a' in
          (* A posteriori check that the argument type was compatible,
             to generate a warning otherwise;
             if a'' = a', no check needs to be done (no cast was introduced).
             Note: this check is conservative (it may not emit warnings when
             it should), and compilers can often detect more errors. *)
          if not (Exp.equal a' a'') &&
             match Cil.isArithmeticType texpected, Cil.isArithmeticType att with
             | true, true -> (* never a problem *) false
             | true, false -> true
             | false, true ->
               (* pointer with no pointer: problematic, except NULL;
                  if expected pointer and got null pointer constant => ok *)
               not (Cil.isPointerType texpected && Ast_info.is_null_expr a')
             | false, false ->
               (* pointers: check compatible modulo void ptr and modulo
                  literal strings (too many warnings otherwise) *)
               let ok1 =
                 (* accept literal strings even when expecting non-const char*;
                    equivalent to GCC's default behavior (-Wno-write-strings) *)
                 (Typ.equal (Cil.unrollType texpected) Cil.charPtrType &&
                  Typ.equal (Cil.unrollType att) Cil.charConstPtrType) ||
                 (* all pointers are convertible to void* *)
                 (Cil.isVoidPtrType texpected && Cil.isPointerType att) ||
                 (* allow implicit void* -> char* conversion *)
                 (Cil.isAnyCharPtrType texpected && Cil.isVoidPtrType att) ||
                 (* always allow null pointers *)
                 (Cil.isPointerType texpected && Ast_info.is_null_expr a') ||
                 areCompatibleTypes texpected att ||
                 (let texpected_no_qualif =
                    Cil.typeRemoveAttributesDeep ["const"] texpected
                  in
                  areCompatibleTypes texpected_no_qualif att)
               in
               let ok =
                 if ok1 then true
                 (* special warning for void* -> any* conversions;
                    this is equivalent to option '-Wc++-compat' in GCC *)
                 else if Cil.isVoidPtrType att && Cil.isPointerType texpected
                 then begin
                   Kernel.warning ~wkey:Kernel.wkey_implicit_conv_void_ptr
                     ~current:true ~once:true
                     "implicit conversion from %a to %a"
                     Cil_printer.pp_typ Cil.voidPtrType
                     Cil_printer.pp_typ texpected;
                   true
                 end else
                   false
               in
               not ok
          then
            Kernel.warning ~wkey:Kernel.wkey_incompatible_types_call
              ~current:true ~once:true
              "expected '%a' but got argument of type '%a': %a"
              Cil_printer.pp_typ texpected Cil_printer.pp_typ att
              Cil_printer.pp_exp a';
          (ss @@ (sa, ghost), a'' :: args')

        | ([], args) -> (* No more types *)
          if not isvar && argTypes != None && not isSpecialBuiltin then
            (* Do not give a warning for functions without a prototype*)
            Kernel.error ~once:true ~current:true
              "Too many arguments in call to %a" Cil_printer.pp_exp f';
          let rec loop = function
              [] -> (init_chunk, [])
            | a :: args ->
              let (ss, args') = loop args in
              let (sa, a', _) =
                let (r, c, e, t) =
                  doExp (no_paren_local_env local_env) false a (AExp None)
                in
                (add_reads ~ghost:local_env.is_ghost loc r c, e, t)
              in
              (ss @@ (sa, ghost), a' :: args')
          in
          let (chunk,args as res) = loop args in
          (match argTypes, f''.enode with
           | Some _,_ ->
             if isvar then begin
               (* use default argument promotion to infer the type of the
                  variadic actuals, see C11:6.5.2.2:7 *)
               promote_variadic_arguments res
             end else
               res
           | None, Lval (Var f, NoOffset)
             when not isSpecialBuiltin ->
             begin
               (* use default argument promotion to infer the type of the
                  function, see 6.5.2.2.6 *)
               assert (not isvar);
               (* No nullary variadics see C11:6.7.6 *)
               warn_no_proto f;
               let (prm_types,args) =
                 List.split
                   (Extlib.mapi default_argument_promotion args)
               in
               let typ = TFun (resType, Some prm_types, false,attrs) in
               Cil.update_var_type f typ;
               Cil.setFormalsDecl f typ;
               (chunk,args)
             end
           | None, _ -> res 
           (* TODO: treat function pointers. 
              The issue is that their origin is more
              difficult to trace than plain variables (e.g. we'd have
              to take into account possible assignments, or update
              accordingly the signature of current function in case
              of a formal.
           *)
          )
      in
      let (sargs, args') = loopArgs (argTypesList, args) in
      (* Setup some pointer to the elements of the call. We may change
       * these below *)
      let s0 = unspecified_chunk empty in
      (* there is a sequence point between evaluations of args
         and the call itself, but we have to check that args wo side-effects
         (thus not appearing anywhere in sargs) are not modified by others...
         The call must thus be in the unspecified chunk
      *)
      let sargs = if isEmpty sargs then empty else sargs in
      let prechunk = ref ((s0 @@ (sf, ghost)) @@ (sargs, ghost)) in
      (* Do we actually have a call, or an expression? *)
      let piscall: bool ref = ref true in

      let pf: exp ref = ref f'' in (* function to call *)
      let pargs: exp list ref = ref args' in (* arguments *)
      let pis__builtin_va_arg: bool ref = ref false in
      let pwhat: expAction ref = ref what in (* what to do with result *)
      let locals = ref [] in

      (* If we do not have a call, this is the result *)
      let pres: exp ref = ref (zero ~loc:e.expr_loc) in

      let prestype: typ ref = ref intType in

      let rec dropCasts e = match e.enode with
        | CastE (_, e) -> dropCasts e
        | _ -> e
      in
      (* Get the name of the last formal *)
      let getNameLastFormal () : string =
        match !currentFunctionFDEC.svar.vtype with
        | TFun(_, Some args, true, _) -> begin
            match List.rev args with
            | (last_par_name, _, _) :: _ -> last_par_name
            | _ -> ""
          end
        | _ -> ""
      in
      (* Try to intercept some builtins *)
      (match (!pf).enode with
       | Lval(Var fv, NoOffset) -> begin
           match fv.vname with
           | "__builtin_va_arg" ->
             begin
               match !pargs with
               | marker :: ({enode = SizeOf resTyp} as size) :: _ -> begin
                   (* Make a variable of the desired type *)
                   let is_real, destlv, r, destlvtyp =
                     match !pwhat with
                     | ASet (is_real,lv, r, lvt) -> is_real, lv, r, lvt
                     | _ ->
                       let v = newTempVar "vararg" true resTyp in
                       locals := v::!locals;
                       false, var v, [], resTyp
                   in
                   pwhat := (ASet (is_real, destlv, r, destlvtyp));
                   pargs := [marker; size;
                             new_exp ~loc
                               (CastE(voidPtrType,
                                      new_exp ~loc (AddrOf destlv)))];
                   pis__builtin_va_arg := true;
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" fv.vname;
             end

           | "__builtin_va_start" ->
             let variad = match (!currentFunctionFDEC).svar.vtype with
               | TFun(_,_,t,_) -> t
               | _ -> assert false
             in
             let name =
               (!currentFunctionFDEC).svar.vname
             in
             begin
               match !pargs with
               | marker :: last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastFormal ()
                     | _ -> false
                   in
                   if not isOk && variad then
                     Kernel.error ~current:true
                       "The last argument in call to __builtin_va_start \
                        should be the last formal argument of %s" name;

                   if not isOk && not variad then
                     Kernel.error ~current:true
                       "Invalid call to __builtin_va_start \
                        in non-variadic function %s"
                       name;

                   (* Check that "lastv" is indeed the last variable in the
                    * prototype and then drop it *)
                   pargs := [ marker ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" name;

                 (* We have to turn uses of __builtin_varargs_start into uses
                  * of __builtin_stdarg_start (because we have dropped the
                  * __builtin_va_alist argument from this function) *)
             end

           | "__builtin_stdarg_start" ->
             let name =
               (!currentFunctionFDEC).svar.vname
             in
             begin
               match !pargs with
               | marker :: last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastFormal ()
                     | _ -> false
                   in
                   if not isOk then
                     Kernel.warning ~current:true
                       "The last argument in call to __builtin_stdarg_start \
                        should be the last formal argument of %s" name;

                   (* Check that "lastv" is indeed the last variable in the
                    * prototype and then drop it *)
                   pargs := [ marker ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" name;

                 (* We have to turn uses of __builtin_varargs_start into uses
                  * of __builtin_stdarg_start (because we have dropped the
                  * __builtin_va_alist argument from this function) *)
             end

           | "__builtin_varargs_start" -> 
             begin
               (* Lookup the prototype for the replacement *)
               let v, _  =
                 try lookupGlobalVar "__builtin_stdarg_start"
                 with Not_found ->
                   abort_context
                     "Cannot find __builtin_stdarg_start to replace %s"
                     fv.vname
               in
               pf := new_exp ~loc (Lval (var v))
             end
           |  "__builtin_next_arg" -> 
             begin
               match !pargs with
               | last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastFormal ()
                     | _ -> false
                   in
                   if not isOk then
                     Kernel.warning ~current:true
                       "The argument in call to %s should be \
                        the last formal argument\n" fv.vname;

                   pargs := [ ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" fv.vname;
             end 
           | "__builtin_va_arg_pack" ->
             begin 
               (match !pargs with
                | [ ] -> begin 
                    piscall := false; 
                    pres := new_exp ~loc:e.expr_loc (SizeOfE !pf);
                    prestype := theMachine.typeOfSizeOf
                  end
                | _ -> 
                  Kernel.warning ~current:true 
                    "Invalid call to builtin_va_arg_pack");
             end
           | "__builtin_constant_p" -> 
             begin
               (* Before emptying the chunk, we remove the corresponding
                  generated labels from the tables. Otherwise, they will
                  be dangling when we iterate over the tables to fix
                  forward gotos, leading to errors. *)
               let remove_label s =
                 let vis = object
                   inherit Cil.nopCilVisitor
                   method! vstmt { labels } =
                     List.iter
                       (function 
                         | Label (l, _, _) -> 
                           H.remove labelStmt l;
                           H.remove backPatchGotos l
                         | _ -> ())
                       labels;
                     DoChildren
                 end
                 in
                 ignore (Cil.visitCilStmt vis s)
               in
               List.iter 
                 (fun (stmt, _, _, _, _) ->
                    remove_label stmt
                 ) !prechunk.stmts;
               clean_up_chunk_locals !prechunk;
               (* Drop the side-effects *)
               prechunk := empty;
               (* Constant-fold the argument and see if it is a constant *)
               (match !pargs with
                | [ arg ] -> begin
                    match (constFold true arg).enode with
                    | Const _ -> piscall := false;
                      pres := integer ~loc:e.expr_loc 1 ;
                      prestype := intType

                    | _ -> piscall := false;
                      pres := integer ~loc:e.expr_loc 0;
                      prestype := intType
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Invalid call to builtin_constant_p")
             end
           | "__builtin_types_compatible_p" -> 
             begin
               (* Constant-fold the argument and see if it is a constant *)
               (match !pargs with 
                | [ {enode = SizeOf t1}; {enode = SizeOf t2}] -> begin
                    (* Drop the side-effects *)
                    prechunk := empty;
                    piscall := false; 
                    let compatible =
                      try ignore(combineTypes CombineOther t1 t2); true
                      with Cannot_combine _ -> false
                    in if compatible then
                      pres := integer ~loc 1
                    else
                      pres := integer ~loc 0;
                    prestype := intType
                  end
                | _ -> 
                  Kernel.warning 
                    ~once:true 
                    ~current:true 
                    "Invalid call to builtin_types_compatible_p");
             end
           | "__builtin_expect" -> 
             begin
               match !pargs with
               | [ arg;_ ] ->
                 (* Keep all side-effects, including those stemming 
                    from the second argument. This is quite strange but 
                    compliant with GCC's behavior. *)
                 piscall := false;
                 pres := arg
               | _ ->
                 Kernel.warning ~once:true ~current:true
                   "Invalid call to builtin_expect"
             end

           (* TODO: Only keep the side effects of the 1st or 2nd argument 
              | "__builtin_choose_expr" -> 
              begin match !pargs with 
              | [ arg; e1; e2 ] -> 
                begin 
                  let constfolded = constFold true arg in
                  match constfolded.enode with 
                  | Const _ -> 
                    piscall := false; 
                    if isZero constfolded then begin
                    (* Keep only 3rd arg side effects *)
                      (*TODO: prechunk := sf @@ (List.nth sargsl 2);*)
                      pres := e2;
                      prestype := typeOf e2
                    end else begin
                    (* Keep only 2nd arg side effects *)
                      (*TODO prechunk := sf @@ (List.nth sargsl 1);*)
                      pres := e1;
                      prestype := typeOf e1
                    end
                  | _ -> Kernel.warning ~once:true ~current:true
                    "builtin_choose_expr expects a constant first argument"
                end
              | _ -> 
              Kernel.warning ~once:true ~current:true
                "Invalid call to builtin_choose_expr: 3 arguments are \
                 expected but %d are provided." 
                (List.length !pargs)
              end*)
           | _ ->
             if asconst then 
               (* last special case: we cannot allow a function call
                  at this point.*)
               begin
                 piscall := false;
                 abort_context
                   "Call to %a in constant." Cil_printer.pp_varinfo fv;
               end
         end
       | _ -> ());

      (* Now we must finish the call *)
      if !piscall then begin
        let addCall ?(is_real_var=true) calldest res t =
          let my_write =
            match calldest with
            | None -> []
            | Some c when is_real_var -> [c]
            | Some _ -> []
          in
          prechunk :=
            (empty @@ (!prechunk, ghost)) +++
            (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
               (Call(calldest,!pf,!pargs,loc)),
             [],my_write, rf);
          pres := res;
          prestype := t
        in
        match !pwhat with
        | ADrop -> addCall None (zero ~loc:e.expr_loc) intType
        | AType -> prestype := resType'
        | ASet(is_real_var, lv, _, vtype) when !pis__builtin_va_arg ->
          (* Make an exception here for __builtin_va_arg *)
          addCall
            ~is_real_var
            None
            (new_exp ~loc:e.expr_loc (Lval(lv)))
            vtype

        | ASet(is_real_var, lv, _, vtype)
          when (allow_return_collapse ~tf:resType' ~tlv:vtype)
          ->
          (* We can assign the result directly to lv *)
          addCall
            ~is_real_var
            (Some lv)
            (new_exp ~loc:e.expr_loc (Lval(lv)))
            vtype

        | _ -> begin
            let restype'' = match !pwhat with
              | AExp (Some t)
                when allow_return_collapse ~tf:resType' ~tlv:t -> t
              | _ -> resType'
            in
            let descr =
              Format.asprintf "%a(%a)"
                Cil_descriptive_printer.pp_exp !pf
                (Pretty_utils.pp_list ~sep:", " 
                   Cil_descriptive_printer.pp_exp) 
                !pargs
            in
            let tmp = newTempVar descr false restype'' in
            tmp.vdecl <- loc;
            locals:=tmp::!locals;
            (* Remember that this variable has been created for this
             * specific call. We will use this in collapseCallCast. *)
            IH.add callTempVars tmp.vid ();
            addCall
              ~is_real_var:false
              (Some (var tmp))
              (new_exp ~loc:e.expr_loc (Lval(var tmp)))
              restype'';
          end
      end;
      List.iter
        (fun v -> prechunk:= local_var_chunk !prechunk v) !locals;
      finishExp [] !prechunk !pres !prestype

    | A.COMMA el ->
      if asconst then Kernel.warning ~current:true "COMMA in constant";
      (* We must ignore AExpLeaveArrayFun (a.k.a. 'do not decay pointers')
         if the expression at hand is a sequence with strictly more than
         one expression, because the exception for sizeof and typeof only
         apply when the expression is directly the argument of the operators.
         See C99 and C11 6.3.2.1§3.)
      *)
      let what =
        if what <> AExpLeaveArrayFun || List.length el = 1
        then what
        else (AExp None)
      in
      let rec loop sofar = function
        | [e] ->
          let (r, se, e', t') =
            doExp (no_paren_local_env local_env) false e what
          in
          (* Pass on the action *)
          (r, sofar @@ (se, ghost), e', t')
        | e :: rest ->
          let (_, se, _, _) =
            doExp (no_paren_local_env local_env) false e ADrop
          in
          loop (sofar @@ (se, ghost)) rest
        | [] -> Kernel.fatal ~current:true "empty COMMA expression"
      in
      loop empty el

    | A.QUESTION (e1, e2, e3) -> begin
        (* Compile the conditional expression *)
        let ghost = local_env.is_ghost in
        let ce1 = doCondExp (no_paren_local_env local_env) asconst e1 in
        let what' = match what with
          | ADrop -> ADrop
          | _ -> AExp None
        in
        (* if we are evaluating a constant expression, e1 is supposed to
           evaluate to either true or false statically, and we can type-check
           only the appropriate branch. In fact, 6.6§3 seems to indicate that
           the dead branch can contain sub-expressions that are normally
           forbidden in a constant expression context, such as function calls.
        *)
        let is_true_cond = evaluate_cond_exp ce1 in
        if asconst && is_true_cond = `CTrue then begin
          match e2.expr_node with
          | A.NOTHING ->
            (match ce1 with
             | CEExp (_,e) -> finishExp [] empty e (Cil.typeOf e)
             | _ ->
               finishExp
                 [] empty (Cil.one ~loc:e2.expr_loc) Cil.intType
                 (* e1 is the result of logic operations that by
                    definition of this branch evaluate to one. *))
          | _ ->
            let _,c2,e2,t2 =
              doExp (no_paren_local_env local_env) asconst e2 what'
            in
            clean_up_chunk_locals c2;
            finishExp [] empty e2 t2
        end else if asconst && is_true_cond = `CFalse then begin
          let _,c3,e3,t3 =
            doExp (no_paren_local_env local_env) asconst e3 what'
          in
          clean_up_chunk_locals c3;
          finishExp [] empty e3 t3
        end else begin
          (* Now we must find the type of both branches, in order to compute
           * the type of the result *)
          let r2, se2, e2'o (* is an option. None means use e1 *), t2 =
            match e2.expr_node with
            | A.NOTHING -> begin (* The same as the type of e1 *)
                match ce1 with
                | CEExp (_, e1') ->
                  [], unspecified_chunk empty, None, typeOf e1'
                (* Do not promote to bool *)
                | _ -> [], unspecified_chunk empty, None, intType
              end
            | _ ->
              let r2, se2, e2', t2 =
                doExp (no_paren_local_env local_env) asconst e2 what'
              in
              r2, se2, Some e2', t2
          in
          (* Do e3 for real *)
          let r3, se3, e3', t3 =
            doExp (no_paren_local_env local_env) asconst e3 what'
          in
          let tresult = conditionalConversion t2 t3 in
          if not (isEmpty se2) then
            ConditionalSideEffectHook.apply (e,e2);
          if not (isEmpty se3) then
            ConditionalSideEffectHook.apply (e,e3);
          match ce1 with
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CFalse && canDrop se2 ->
            clean_up_chunk_locals se2;
            finishExp r3 ((empty @@ (se1, ghost)) @@ (se3, ghost))
              (snd (castTo t3 tresult e3')) tresult
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CTrue && canDrop se3 ->
            begin
              clean_up_chunk_locals se3;
              match e2'o with
              | None -> (* use e1' *)
                finishExp r2
                  ((empty @@ (se1, ghost)) @@ (se2, ghost))
                  (snd (castTo t2 tresult e1')) tresult
              | Some e2' ->
                finishExp r2
                  ((empty @@ (se1, ghost)) @@ (se2, ghost))
                  (snd (castTo t2 tresult e2')) tresult
            end
          | _ when what = ADrop ->
            (* We are not interested by the result, but might want to
               evaluate e2 and e3 if they are dangerous expressions. *)
            (* dummy result, that will be ultimately be dropped *)
            let res = Cil.zero ~loc in
            (match e2'o with
             | None when is_dangerous e3' || not (isEmpty se3) ->
               let descr =
                 Format.asprintf "%a" Cprint.print_expression e1
               in
               let tmp = newTempVar descr true tresult in
               let tmp_var = var tmp in
               let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
               let (r1, se1, _, _) =
                 doExp
                   (no_paren_local_env local_env) asconst e1
                   (ASet(false, tmp_var, [], tresult))
               in
               let se1 = local_var_chunk se1 tmp in
               let dangerous =
                 if is_dangerous e3' then
                   keepPureExpr ~ghost e3' loc
                 else skipChunk
               in
               finishExp (r1@r3)
                 ((empty @@ (se1, ghost)) @@
                  (ifChunk ~ghost tmp_lval loc skipChunk
                     (se3 @@ (dangerous, ghost)), ghost))
                 res
                 tresult
             | None ->
               (* we can drop e3, just keep e1 in case it is dangerous *)
               let (r1,se1,e1,_) =
                 doExp (no_paren_local_env local_env) asconst e1 ADrop
               in
               let dangerous =
                 if is_dangerous e1 then
                   keepPureExpr ~ghost e1 loc
                 else skipChunk
               in
               finishExp
                 (r1@r3) (se1 @@ (dangerous, ghost)) res tresult
             | Some e2'
               when is_dangerous e2' || is_dangerous e3'
                    || not (isEmpty se2) || not (isEmpty se3) ->
               (* we have to keep e1 in order to know which
                  dangerous expression is to be evaluated *)
               let se2 =
                 if is_dangerous e2' then
                   se2 @@
                   (keepPureExpr ~ghost e2' loc, ghost)
                 else se2
               in
               let se3 =
                 if is_dangerous e3' then
                   se3 @@ (keepPureExpr ~ghost e3' loc, ghost)
                 else se3
               in
               let cond = compileCondExp ~ghost ce1 se2 se3 in
               finishExp (r2@r3) cond res tresult
             | Some _ ->
               (* we just keep e1 in case it is dangerous. everything
                  else can be dropped *)
               let (r1,se1,e1,_) =
                 doExp (no_paren_local_env local_env) asconst e1 ADrop
               in
               let dangerous =
                 if is_dangerous e1 then
                   keepPureExpr ~ghost e1 loc
                 else skipChunk
               in
               finishExp
                 (r1@r2@r3) (se1 @@ (dangerous, ghost)) res tresult)
          | _ -> (* Use a conditional *) begin
              match e2'o with
              | None -> (* has form "e1 ? : e3"  *)
                let descr =
                  Format.asprintf "%a" Cprint.print_expression e1
                in
                let tmp = newTempVar descr true tresult in
                let tmp_var = var tmp in
                let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
                let (r1,se1, _, _) =
                  doExp
                    (no_paren_local_env local_env)
                    asconst e1 (ASet(false, tmp_var, [], tresult))
                in
                let se1 = local_var_chunk se1 tmp in
                let newWhat = ASet(false,tmp_var, [], tresult) in
                let r3,se3,_,_ = finishExp ~newWhat r3 se3 e3' t3 in
                finishExp
                  (r1@r3)
                  ((empty @@ (se1, ghost)) @@
                   (ifChunk ~ghost tmp_lval loc skipChunk se3, ghost))
                  tmp_lval
                  tresult
              | Some e2' ->
                let is_real, lv, r, lvt, scope_chunk =
                  match what with
                  | ASet (is_real, lv, r, lvt) ->
                    is_real, lv, r, lvt, empty
                  | _ ->
                    let descr =
                      Format.asprintf "%a?%a:%a"
                        Cprint.print_expression e1
                        Cil_descriptive_printer.pp_exp e2'
                        Cil_descriptive_printer.pp_exp e3'
                    in
                    let tmp = newTempVar descr true tresult in
                    false, var tmp, [], tresult,
                    local_var_chunk empty tmp
                in
                (* Now do e2 and e3 for real *)
                let (r2,se2, _, _) =
                  finishExp ~newWhat:(ASet(is_real,lv,r,lvt))
                    r2 se2 e2' t2
                in
                let (r3, se3, _, _) =
                  finishExp ~newWhat:(ASet(is_real,lv, r, lvt))
                    r3 se3 e3' t3
                in
                let cond = compileCondExp ~ghost ce1 se2 se3 in
                finishExp
                  (r2@r3)
                  (scope_chunk @@ (cond, ghost))
                  (new_exp ~loc (Lval lv)) tresult
            end
        end
      end
    | A.GNU_BODY b -> begin
        (* Find the last A.COMPUTATION and remember it. This one is invoked
         * on the reversed list of statements. *)
        let findLastComputation = function
            s :: _  ->
            let rec findLast st = match st.stmt_node with
              | A.SEQUENCE (_, s, _) -> findLast s
              | CASE (_, s, _) -> findLast s
              | CASERANGE (_, _, s, _) -> findLast s
              | LABEL (_, s, _) -> findLast s
              | A.COMPUTATION _ ->
                begin
                  match local_env.is_ghost,st.stmt_ghost with
                  | true,true | false, false -> st
                  | true, false -> assert false
                  | false, true -> raise Not_found
                end
              | _ -> raise Not_found
            in
            findLast s
          | [] -> raise Not_found
        in
        (* Save the previous data *)
        let old_gnu = ! gnu_body_result in
        let lastComp, isvoidbody =
          match what with
          | ADrop -> (* We are dropping the result *)
            {stmt_ghost = local_env.is_ghost; stmt_node = A.NOP loc}, true
          | _ ->
            try findLastComputation (List.rev b.A.bstmts), false
            with Not_found ->
              Kernel.fatal ~current:true "Cannot find COMPUTATION in GNU.body"
              (*                A.NOP cabslu, true *)
        in
        let loc = Cabshelper.get_statementloc lastComp in
        (* Prepare some data to be filled by doExp ghost *)
        let data : (exp * typ) option ref = ref None in
        gnu_body_result := (lastComp, data);

        let se = doBodyScope local_env b in

        (*Kernel.debug "Body inside expression: %a@." d_chunk se;*)

        gnu_body_result := old_gnu;
        match !data with
        | None when isvoidbody ->
          finishExp [] se (zero ~loc:e.expr_loc) voidType
        | None -> abort_context "Cannot find COMPUTATION in GNU.body"
        | Some (e, t) ->
          let se, e =
            match se.stmts with
            | [ { skind = Block b},_, _, _, _ ] ->
              let vi = newTempVar "GNU.body" true t in
              b.bstmts <-
                b.bstmts @
                [Cil.mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                   (Set (Cil.var vi, e,loc))];
              (local_var_chunk se vi,Cil.new_exp ~loc (Lval (Cil.var vi)))
            | _ -> se,e
          in
          finishExp [] se e t
      end

    | A.LABELADDR l -> begin (* GCC's taking the address of a label *)
        let l = lookupLabel l in (* To support locally declared labels *)
        let addrval =
          try H.find gotoTargetHash l
          with Not_found -> begin
              let res = !gotoTargetNextAddr in
              incr gotoTargetNextAddr;
              H.add gotoTargetHash l res;
              res
            end
        in
        finishExp [] (unspecified_chunk empty)
          (makeCast (integer ~loc addrval) voidPtrType) voidPtrType
      end

    | A.EXPR_PATTERN _ -> abort_context "EXPR_PATTERN in cabs2cil input"

  in
  (*let (_a,b,_c,_d) = result in
    Format.eprintf "doExp ~const:%b ~e:" asconst ;
    Cprint.print_expression e;
    Format.eprintf "@.";
    Format.eprintf "Got: chunk:'%a'@." d_chunk b;*)
  CurrentLoc.set oldLoc;
  result

and normalize_unop unop action asconst local_env e what =
  match e.expr_node with
  | A.COMMA el -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      { e with
        expr_node =
          A.COMMA
            (replaceLastInList el
               (fun e -> { e with expr_node = A.UNARY(unop, e)})) }
      what
  | A.QUESTION (e1, e2, e3) -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      { e with
        expr_node =
          A.QUESTION
            (e1,
             { e2 with expr_node = A.UNARY(unop, e2)},
             { e3 with expr_node = A.UNARY(unop, e3)})}
      what
  | A.PAREN e1 ->
    doExp (inner_paren local_env) asconst
      { e with expr_node = A.UNARY(unop, e1)} what
  | _ ->
    action
      { local_env with
        is_paren = local_env.inner_paren; inner_paren = false }
      e
      what

and normalize_binop binop action local_env asconst le re what =
  match le.expr_node with
  | A.COMMA el -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (A.COMMA
            (replaceLastInList el
               (fun e -> cabs_exp e.expr_loc (A.BINARY(binop, e, re))))))
      what
  | A.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
    (*TODO: prevent duplication of e2: this is incorrect
      if it contains labels *)
    (* let r2,se2,e2,t2 = doExp authorized_reads ghost asconst e2 in*)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (A.QUESTION
            (e1,
             cabs_exp e2q.expr_loc (A.BINARY(binop, e2q, re)),
             cabs_exp e3q.expr_loc (A.BINARY(binop, e3q, re)))))
      what
  | A.CAST (t, A.SINGLE_INIT e) when binop = A.ASSIGN -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (A.CAST
            (t,
             A.SINGLE_INIT
               (cabs_exp e.expr_loc
                  (A.BINARY
                     (binop, e,
                      (cabs_exp re.expr_loc
                         (A.CAST (t, A.SINGLE_INIT re)))))))))
      what
  | A.PAREN e1 ->
    doExp (inner_paren local_env) asconst
      (cabs_exp le.expr_loc (A.BINARY(binop,e1,re))) what
  | _ ->
    action
      { local_env with is_paren = local_env.inner_paren; inner_paren = false }
      asconst le what

(* bop is always the arithmetic version. Change it to the appropriate pointer
 * version if necessary *)
and doBinOp loc (bop: binop) (e1: exp) (t1: typ) (e2: exp) (t2: typ) =
  let doArithmetic () =
    let tres = arithmeticConversion t1 t2 in
    (* Keep the operator since it is arithmetic *)
    tres,
    optConstFoldBinOp loc false bop
      (makeCastT e1 t1 tres) (makeCastT e2 t2 tres) tres
  in
  let doArithmeticComp () =
    let tres = arithmeticConversion t1 t2 in
    (* Keep the operator since it is arithmetic *)
    intType,
    optConstFoldBinOp loc false bop
      (makeCastT e1 t1 tres) (makeCastT e2 t2 tres) intType
  in
  let doIntegralArithmetic () =
    let tres = unrollType (arithmeticConversion t1 t2) in
    match tres with
    | TInt _ ->
      tres,
      optConstFoldBinOp loc false bop
        (makeCastT e1 t1 tres) (makeCastT e2 t2 tres) tres
    | _ ->
      Kernel.fatal ~current:true "%a operator on non-integer type %a"
        Cil_printer.pp_binop bop Cil_printer.pp_typ tres
  in
  (* Invariant: t1 and t2 are pointers types *)
  let pointerComparison e1 t1 e2 t2 =
    if false then Kernel.debug "%a %a %a %a"
        Cil_printer.pp_exp e1 Cil_printer.pp_typ t1
        Cil_printer.pp_exp e2 Cil_printer.pp_typ t2;
    let t1p = Cil.(unrollType (typeOf_pointed t1)) in
    let t2p = Cil.(unrollType (typeOf_pointed t2)) in
    (* We are more lenient than the norm here (C99 6.5.8, 6.5.9), and cast
       arguments with incompatible types to a common type *)
    let e1', e2' =
      if not (areCompatibleTypes t1p t2p) then
        makeCastT e1 t1 Cil.voidPtrType, makeCastT e2 t2 Cil.voidPtrType
      else e1, e2
    in
    intType,
    optConstFoldBinOp loc false bop e1' e2' intType
  in
  let do_shift e1 t1 e2 t2 =
    match e1.enode with
    | StartOf lv ->
      { e1 with enode = AddrOf (addOffsetLval (Index (e2,NoOffset)) lv) }
    | _ ->
      optConstFoldBinOp loc false PlusPI e1
        (makeCastT e2 t2 (integralPromotion t2)) t1
  in
  match bop with
  | (Mult|Div) -> doArithmetic ()
  | (Mod|BAnd|BOr|BXor) -> doIntegralArithmetic ()
  | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result
                          * has the same type as the left hand side *)
    if Cil.msvcMode () then
      (* MSVC has a bug. We duplicate it here *)
      doIntegralArithmetic ()
    else
      let t1' = integralPromotion t1 in
      let t2' = integralPromotion t2 in
      t1',
      optConstFoldBinOp loc false bop
        (makeCastT e1 t1 t1') (makeCastT e2 t2 t2') t1'
  | (PlusA|MinusA)
    when isArithmeticType t1 && isArithmeticType t2 -> doArithmetic ()
  | (Eq|Ne|Lt|Le|Ge|Gt)
    when isArithmeticType t1 && isArithmeticType t2 ->
    doArithmeticComp ()
  | PlusA when isPointerType t1 && isIntegralType t2 ->
    t1, do_shift e1 t1 e2 t2
  | PlusA when isIntegralType t1 && isPointerType t2 ->
    t2, do_shift e2 t2 e1 t1
  | MinusA when isPointerType t1 && isIntegralType t2 ->
    t1,
    optConstFoldBinOp loc false MinusPI e1
      (makeCastT e2 t2 (integralPromotion t2)) t1
  | MinusA when isPointerType t1 && isPointerType t2 ->
    if areCompatibleTypes (* C99 6.5.6:3 *)
        (Cil.type_remove_qualifier_attributes_deep t1)
        (Cil.type_remove_qualifier_attributes_deep t2)
    then
      theMachine.ptrdiffType,
      optConstFoldBinOp loc false MinusPP e1 e2 theMachine.ptrdiffType
    else abort_context "incompatible types in pointer subtraction"

  (* Two special cases for comparisons with the NULL pointer. We are a bit
     more permissive. *)
  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isZero e2 ->
    pointerComparison e1 t1 (makeCast e2 t1) t1
  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t2 && isZero e1 ->
    pointerComparison (makeCast e1 t2) t2 e2 t2

  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
    pointerComparison e1 t1 e2 t2

  | (Eq|Ne|Le|Lt|Ge|Gt) when (isPointerType t1 && isArithmeticType t2 ||
                              isArithmeticType t1 && isPointerType t2 ) ->
    Kernel.fatal ~current:true
      "comparison between pointer and non-pointer: %a"
      Cil_printer.pp_exp (dummy_exp(BinOp(bop,e1,e2,intType)))

  | _ ->
    Kernel.fatal ~current:true
      "doBinOp: %a"
      Cil_printer.pp_exp (dummy_exp(BinOp(bop,e1,e2,intType)))

(* Constant fold a conditional. This is because we want to avoid having
 * conditionals in the initializers. So, we try very hard to avoid creating
 * new statements.
*)
and doCondExp local_env (asconst: bool)
    (** Try to evaluate the conditional expression
     * to TRUE or FALSE, because it occurs in a constant *)
    ?ctxt (* ctxt is used internally to determine if we should apply
             the conditional side effects hook (see above)
             and should not appear (i.e. be None) in toplevel calls. *)
    (e: A.expression) : condExpRes =
  let ghost = local_env.is_ghost in
  let rec addChunkBeforeCE (c0: chunk) ce =
    let c0 = remove_effects c0 in
    match ce with
    | CEExp (c, e) -> CEExp ((empty @@ (c0, ghost)) @@ (c, ghost), e)
    | CEAnd (ce1, ce2) -> CEAnd (addChunkBeforeCE c0 ce1, ce2)
    | CEOr (ce1, ce2) -> CEOr (addChunkBeforeCE c0 ce1, ce2)
    | CENot ce1 -> CENot (addChunkBeforeCE c0 ce1)
  in
  let rec canDropCE = function
      CEExp (c, _e) -> canDrop c
    | CEAnd (ce1, ce2) | CEOr (ce1, ce2) -> canDropCE ce1 && canDropCE ce2
    | CENot (ce1) -> canDropCE ce1
  in
  let rec remove_effects_ce = function
    | CEExp(c,e) -> CEExp(remove_effects c,e)
    | CEAnd(ce1,ce2) -> CEAnd(remove_effects_ce ce1, remove_effects_ce ce2)
    | CEOr(ce1,ce2) -> CEOr(remove_effects_ce ce1, remove_effects_ce ce2)
    | CENot(ce) -> CENot(remove_effects_ce ce)
  in
  let loc = e.expr_loc in
  let result = match e.expr_node with
    | A.BINARY (A.AND, e1, e2) -> begin
        let ce1 = doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 in
        let ce2 = doCondExp (no_paren_local_env local_env) asconst ~ctxt:e e2 in
        let ce1 = remove_effects_ce ce1 in
        match ce1, ce2 with
        | CEExp (se1, ({enode = Const ci1})), _ ->
          (match isConstTrueFalse ci1 with 
           | `CTrue -> addChunkBeforeCE se1 ce2
           | `CFalse ->
             (* se2 might contain labels so we cannot always drop it *)
             if canDropCE ce2 then begin
               clean_up_cond_locals ce2; ce1
             end else CEAnd (ce1, ce2))
        | CEExp(se1, e1'), CEExp (se2, e2') when
            theMachine.useLogicalOperators && isEmpty se1 && isEmpty se2 ->
          CEExp
            (empty,
             new_exp ~loc
               (BinOp(LAnd,
                      makeCast e1' intType, makeCast e2' intType, intType)))
        | _ -> CEAnd (ce1, ce2)
      end

    | A.BINARY (A.OR, e1, e2) -> begin
        let ce1 = doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 in
        let ce2 = doCondExp (no_paren_local_env local_env) asconst ~ctxt:e e2 in
        let ce1 = remove_effects_ce ce1 in
        match ce1, ce2 with
        | CEExp (se1, ({enode = Const ci1})), _ ->
          (match isConstTrueFalse ci1 with
           | `CFalse -> addChunkBeforeCE se1 ce2
           | `CTrue ->
             (* se2 might contain labels so we cannot drop it *)
             if canDropCE ce2 then begin
               clean_up_cond_locals ce2; ce1
             end else CEOr (ce1, ce2))
        | CEExp (se1, e1'), CEExp (se2, e2') when
            theMachine.useLogicalOperators && isEmpty se1 && isEmpty se2 ->
          CEExp
            (empty,
             new_exp ~loc
               (BinOp(LOr,
                      makeCast e1' intType, makeCast e2' intType, intType)))
        | _ -> CEOr (ce1, ce2)
      end

    | A.UNARY(A.NOT, e1) -> begin
        match doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 with
        | CEExp (se1, e) when isEmpty se1 ->
          let t = typeOf e in
          if not ((isPointerType t) || (isArithmeticType t))then
            Kernel.error ~once:true ~current:true "Bad operand to !";
          CEExp (empty, new_exp ~loc (UnOp(LNot, e, intType)))
        | ce1 -> CENot ce1
      end

    | A.PAREN e ->
      doCondExp (paren_local_env local_env) asconst ?ctxt e

    | _ ->
      let (r, se, e', t) =
        doExp local_env asconst e (AExp None)
      in
      (* No need to add reads here: we'll always have a sequence point,
         either because the expression is complete, or because of a logic
         operator. *)
      (match ctxt with
       | None -> ()
       | Some _ when isEmpty se -> ()
       | Some orig ->
         ConditionalSideEffectHook.apply (orig,e));
      ignore (checkBool t e');
      CEExp (add_reads ~ghost e.expr_loc r se,
             if asconst || theMachine.lowerConstants then
               constFold asconst e'
             else e')
  in
  result

and compileCondExp ~ghost ce st sf =
  match ce with
  | CEAnd (ce1, ce2) ->
    let loc = CurrentLoc.get () in
    let (duplicable, sf1, sf2) =
      (* If sf is small then will copy it *)
      try (true, sf, duplicateChunk sf)
      with Failure _ ->
        let lab = newLabelName "_LAND" in
        (false, gotoChunk ~ghost lab loc, consLabel ~ghost lab sf loc false)
    in
    let st' = compileCondExp ~ghost ce2 st sf1 in
    if not duplicable && !doAlternateConditional then
      let st_fall_through = chunkFallsThrough st' in
      (* if st does not fall through, we do not need to add a goto
         after the else part. This prevents spurious falls-through warning
         afterwards. *)
      let sf' = duplicateChunk sf1 in
      let lab = newLabelName "_LAND" in
      let gotostmt =
        if st_fall_through then gotoChunk ~ghost lab loc else skipChunk
      in
      let labstmt =
        if st_fall_through then
          consLabel ~ghost lab empty loc false
        else skipChunk
      in
      let (@@) s1 s2 = s1 @@ (s2, ghost) in
      (compileCondExp ~ghost ce1 st' sf')
      @@ gotostmt @@ sf2 @@ labstmt
    else
      let sf' = sf2 in
      compileCondExp ~ghost ce1 st' sf'

  | CEOr (ce1, ce2) ->
    let loc = CurrentLoc.get () in
    let (duplicable, st1, st2) =
      (* If st is small then will copy it *)
      try (true, st, duplicateChunk st)
      with Failure _ ->
        let lab = newLabelName "_LOR" in
        (false, gotoChunk ~ghost lab loc, consLabel ~ghost lab st loc false)
    in
    if not duplicable && !doAlternateConditional then
      let st' = duplicateChunk st1 in
      let sf' = compileCondExp ~ghost ce2 st1 sf in
      let sf_fall_through = chunkFallsThrough sf' in
      let lab = newLabelName "_LOR" in
      let gotostmt =
        if sf_fall_through then
          gotoChunk ~ghost lab loc
        else skipChunk
      in
      let labstmt =
        if sf_fall_through then
          consLabel ~ghost lab empty (CurrentLoc.get ()) false
        else skipChunk
      in
      let (@@) s1 s2 = s1 @@ (s2, ghost) in
      (compileCondExp ~ghost ce1 st' sf')
      @@ gotostmt @@ st2 @@ labstmt
    else
      let st' = st1 in
      let sf' = compileCondExp ~ghost ce2 st2 sf in
      (*Format.eprintf
        "result:@\nchunk then:@\n  @[%a@]@\nchunk else:  @[%a@]@."
        d_chunk st d_chunk sf;*)
      compileCondExp ~ghost ce1 st' sf'

  | CENot ce1 -> compileCondExp ~ghost ce1 sf st

  | CEExp (se, e) -> begin
      match e.enode with
      | Const(CInt64(i,_,_))
        when (not (Integer.equal i Integer.zero)) && canDrop sf ->
        clean_up_chunk_locals sf;
        se @@ (st, ghost)
      | Const(CInt64(z,_,_))
        when (Integer.equal z Integer.zero) && canDrop st ->
        clean_up_chunk_locals st;
        se @@ (sf, ghost)
      | _ -> (empty @@ (se, ghost)) @@ (ifChunk ~ghost e e.eloc st sf, ghost)
    end


(* A special case for conditionals *)
and doCondition local_env (isconst: bool)
    (* If we are in constants, we do our best to eliminate the conditional *)
    (e: A.expression)
    (st: chunk)
    (sf: chunk) : chunk =
  if isEmpty st && isEmpty sf(*TODO: ignore attribute FRAMA_C_KEEP_BLOCK*) then
    begin
      let (_, se,e,_) = doExp local_env false e ADrop in
      if is_dangerous e then begin
        let ghost = local_env.is_ghost in
        se @@ (keepPureExpr ~ghost e e.eloc, ghost)
      end else begin
        if (isEmpty se) then begin
          let name = !currentFunctionFDEC.svar.vorig_name in
          IgnorePureExpHook.apply (name, e)
        end;
        se
      end
    end else begin
      let ce = doCondExp (no_paren_local_env local_env) isconst e in
      let chunk = compileCondExp ~ghost:local_env.is_ghost ce st sf in
      chunk
    end

and doPureExp local_env (e : A.expression) : exp =
  let (_,se, e', _) = doExp local_env true e (AExp None) in
  if isNotEmpty se then
    Kernel.error ~once:true ~current:true "%a has side-effects" Cprint.print_expression e;
  e'

and doFullExp local_env const e what =
  let (r, se,e,t) = doExp local_env const e what in
  let se' = add_reads ~ghost:local_env.is_ghost e.eloc r se in
  (* there is a sequence point after a full exp *)
  empty @@ (se', local_env.is_ghost),e,t

and doInitializer local_env (vi: varinfo) (inite: A.init_expression)
  (* Return the accumulated chunk, the initializer and the new type (might be
   * different for arrays), together with the lvals read during evaluation of
   * the initializer (for local intialization)
   *)
  : chunk * init * typ * Cil_datatype.Lval.Set.t =

  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "@\nStarting a new initializer for %s : %a@\n"
    vi.vname Cil_printer.pp_typ vi.vtype;
  let acc, preinit, restl =
    let so = makeSubobj vi vi.vtype NoOffset in
    doInit local_env vi.vglob Extlib.nop NoInitPre so
      (unspecified_chunk empty) [ (A.NEXT_INIT, inite) ]
  in
  if restl <> [] then
    Kernel.warning ~current:true "Ignoring some initializers";
  (* sm: we used to do array-size fixups here, but they only worked
   * for toplevel array types; now, collectInitializer does the job,
   * including for nested array types *)
  let typ' = vi.vtype in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "Collecting the initializer for %s@\n" vi.vname;
  let (init, typ'', reads) =
    collectInitializer Cil_datatype.Lval.Set.empty preinit typ' typ'
  in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "Finished the initializer for %s@\n  init=%a@\n  typ=%a@\n  acc=%a@\n"
    vi.vname Cil_printer.pp_init init Cil_printer.pp_typ typ' d_chunk acc;
  empty @@ (acc, local_env.is_ghost), init, typ'', reads

(* Consume some initializers. This is used by both global and local variables
   initialization.
   - local_env is the current environment
   - isconst is used to indicate that expressions must be compile-time constant
     (i.e. we are in a global initializer)
   - add_implicit_ensures is a callback to add an ensures clause to contracts
     above current initialized part when it is partially initialized.
     Does nothing initially. Useful only for initialization of locals
   - preinit corresponds to the initializers seen previously (for globals)
   - so contains the information about the current subobject currently being
     initialized
   - acc is the chunk corresponding to initializations seen previously
     (for locals)
   - initl is the current list of initializers to be processed
doInit returns a triple:
   - chunk performing initialization
   - preinit corresponding to the complete initialization
   - the list of unused initializers if any (should be empty most of the time)
*)
and doInit local_env isconst add_implicit_ensures preinit so acc initl =
  let ghost = local_env.is_ghost in
  let whoami fmt = Cil_printer.pp_lval fmt (Var so.host, so.soOff) in
  let initl1 =
    match initl with
    | (A.NEXT_INIT,
       A.SINGLE_INIT ({ expr_node = A.CAST ((s, dt), ie)} as e)) :: rest ->
      let s', dt', ie' = preprocessCast ghost s dt ie in
      (A.NEXT_INIT,
       A.SINGLE_INIT
         ({expr_node = A.CAST ((s', dt'), ie'); expr_loc = e.expr_loc}))
      :: rest
    | _ -> initl
  in
  (* Sometimes we have a cast in front of a compound (in GCC). This
   * appears as a single initializer. Ignore the cast  *)
  let initl2 =
    match initl1 with
    | (what,
       A.SINGLE_INIT
         ({expr_node = A.CAST ((specs, dt), A.COMPOUND_INIT ci)})) :: rest ->
      let s', dt', _ie' = preprocessCast ghost specs dt (A.COMPOUND_INIT ci) in
      let typ = doOnlyType ghost s' dt' in
      if Typ.equal
          (Cil.typeDeepDropAllAttributes typ)
          (Cil.typeDeepDropAllAttributes so.soTyp)
      then
        (* Drop the cast *)
        (what, A.COMPOUND_INIT ci) :: rest
      else
        (* Keep the cast.  A new var will be created to hold
           the intermediate value.  *)
        initl1
    | _ -> initl1
  in
  let allinitl = initl2 in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "doInit for %t %s (current %a). Looking at: %t" whoami
    (if so.eof then "(eof)" else "")
    Cil_printer.pp_lval (Var so.host, so.curOff)
    (fun fmt ->
       match allinitl with
       | [] -> Format.fprintf fmt "[]@."
       | (what, ie) :: _ ->
         Cprint.print_init_expression fmt (A.COMPOUND_INIT [(what, ie)])
    );
  match unrollType so.soTyp, allinitl with
  (* No more initializers return *)
  | _, [] -> acc, preinit, []
  (* No more subobjects to initialize *)
  | _, (A.NEXT_INIT, _) :: _ when so.eof -> acc, preinit, allinitl
  (* If we are at an array of characters and the initializer is a
   * string literal (optionally enclosed in braces) then explode the
   * string into characters *)
  | TArray(bt, leno, _, _ ),
    (A.NEXT_INIT,
     (A.SINGLE_INIT({ expr_node = A.CONSTANT (A.CONST_STRING s)} as e)|
      A.COMPOUND_INIT
        [(A.NEXT_INIT,
          A.SINGLE_INIT(
            { expr_node =
                A.CONSTANT
                  (A.CONST_STRING s)} as e))]))
    :: restil
    when (match unrollType bt with
        | TInt((IChar|IUChar|ISChar), _) -> true
        | TInt _ ->
          (*Base type is a scalar other than char. Maybe a wchar_t?*)
          Kernel.fatal ~current:true
            "Using a string literal to initialize something other than \
             a character array"
        | _ ->  false (* OK, this is probably an array of strings. Handle *)
      )              (* it with the other arrays below.*)
    ->
    let charinits =
      let init c =
        A.NEXT_INIT,
        A.SINGLE_INIT
          { expr_node = A.CONSTANT (A.CONST_CHAR [c]);
            expr_loc = e.expr_loc }
      in
      let collector =
        (* ISO 6.7.8 para 14: final NUL added only if no size specified, or
         * if there is room for it; btw, we can't rely on zero-init of
         * globals, since this array might be a local variable *)
        if ((not (Extlib.has_some leno)) ||
            ((String.length s) < (integerArrayLength leno)))
        then ref [init Int64.zero]
        else ref []
      in
      for pos = String.length s - 1 downto 0 do
        collector := init (Int64.of_int (Char.code (s.[pos]))) :: !collector
      done;
      !collector
    in
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let leno = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, leno, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      doInit local_env isconst add_implicit_ensures preinit so' acc charinits in
    if initl' <> [] then
      Kernel.warning ~current:true
        "Too many initializers for character array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env isconst add_implicit_ensures preinit' so acc' restil
  (* If we are at an array of WIDE characters and the initializer is a
   * WIDE string literal (optionally enclosed in braces) then explore
   * the WIDE string into characters *)
  (* [weimer] Wed Jan 30 15:38:05 PST 2002
   * Despite what the compiler says, this match case is used and it is
   * important. *)
  | TArray(bt, leno, _, _),
    (A.NEXT_INIT,
     (A.SINGLE_INIT({expr_node = A.CONSTANT (A.CONST_WSTRING s)} as e)|
      A.COMPOUND_INIT
        [(A.NEXT_INIT,
          A.SINGLE_INIT(
            {expr_node =
               A.CONSTANT
                 (A.CONST_WSTRING s)} as e))]))
    :: restil
    when
      (let bt' = unrollType bt in
       match bt' with
       (* compare bt to wchar_t, ignoring signed vs. unsigned *)
       | TInt _ when (bitsSizeOf bt') =
                     (bitsSizeOf theMachine.wcharType) ->
         true
       | TInt _ ->
         (*Base type is a scalar other than wchar_t.
           Maybe a char?*)
         Kernel.fatal ~current:true
           "Using a wide string literal to initialize \
            something other than a wchar_t array"
       | _ -> false
       (* OK, this is probably an array of strings. Handle
          it with the other arrays below.*)
      )
    ->
    let maxWChar =  (*  (2**(bitsSizeOf !wcharType)) - 1  *)
      Int64.sub (Int64.shift_left Int64.one (bitsSizeOf theMachine.wcharType))
        Int64.one in
    let charinits =
      let init c =
        if Int64.compare c maxWChar > 0 then (* if c > maxWChar *)
          Kernel.error ~once:true ~current:true
            "cab2cil:doInit:character 0x%Lx too big." c;
        A.NEXT_INIT,
        A.SINGLE_INIT
          { expr_node = A.CONSTANT (A.CONST_INT (Int64.to_string c));
            expr_loc = e.expr_loc
          }
      in
      (List.map init s) @
      (
        (* ISO 6.7.8 para 14: final NUL added only if no size specified, or
         * if there is room for it; btw, we can't rely on zero-init of
         * globals, since this array might be a local variable *)
        if (not (Extlib.has_some leno)
            || ((List.length s) < (integerArrayLength leno)))
        then [init Int64.zero]
        else [])
    in
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let leno = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, leno, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      doInit local_env isconst add_implicit_ensures preinit so' acc charinits
    in
    if initl' <> [] then
      (* sm: see above regarding ISO 6.7.8 para 14, which is not implemented
       * for wchar_t because, as far as I can tell, we don't even put in
       * the automatic NUL (!) *)
      Kernel.warning ~current:true
        "Too many initializers for wchar_t array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env isconst add_implicit_ensures preinit' so acc' restil
  (* If we are at an array and we see a single initializer then it must
   * be one for the first element *)
  | TArray(bt, leno, _, _), (A.NEXT_INIT, A.SINGLE_INIT _oneinit) :: _restil  ->
    (* Grab the length if there is one *)
    let leno = integerArrayLength leno in
    so.stack <- InArray(so.soOff, bt, leno, ref 0) :: so.stack;
    normalSubobj so;
    (* Start over with the fields *)
    doInit local_env isconst add_implicit_ensures preinit so acc allinitl
  (* An incomplete structure with any initializer is an error. *)
  | TComp (comp, _, _), _ :: restil when not comp.cdefined ->
    Kernel.error ~current:true ~once:true
      "variable `%s' has initializer but incomplete type" so.host.vname;
    doInit local_env isconst add_implicit_ensures preinit so acc restil
  (* If we are at a composite and we see a single initializer of the same
   * type as the composite then grab it all. If the type is not the same
   * then we must go on and try to initialize the fields *)
  | TComp (comp, _, _), (A.NEXT_INIT, A.SINGLE_INIT oneinit) :: restil ->
    let r,se, oneinit', t' =
      doExp (no_paren_local_env local_env) isconst oneinit (AExp None)
    in
    let r = Cil_datatype.Lval.Set.of_list r in
    if (match unrollType t' with
        | TComp (comp', _, _) when comp'.ckey = comp.ckey -> true
        | _ -> false)
    then begin
      (* Initialize the whole struct *)
      let preinit = setOneInit preinit so.soOff (SinglePre (oneinit', r)) in
      (* Advance to the next subobject *)
      advanceSubobj so;
      let se = acc @@ (se, ghost) in
      doInit local_env isconst add_implicit_ensures preinit so se restil
    end else begin (* Try to initialize fields *)
      let toinit = fieldsToInit comp None in
      so.stack <- InComp(so.soOff, comp, toinit) :: so.stack;
      normalSubobj so;
      doInit local_env isconst add_implicit_ensures preinit so acc allinitl
    end

  (* A scalar with a single initializer *)
  | _, (A.NEXT_INIT, A.SINGLE_INIT oneinit) :: restil ->
    let r, se, oneinit', t' =
      doExp (no_paren_local_env local_env) isconst oneinit (AExp(Some so.soTyp))
    in
    let r = Cil_datatype.Lval.Set.of_list r in
    Kernel.debug ~dkey:Kernel.dkey_typing_init "oneinit'=%a, t'=%a, so.soTyp=%a"
      Cil_printer.pp_exp oneinit' Cil_printer.pp_typ t'
      Cil_printer.pp_typ so.soTyp;
    let init_expr =
      if theMachine.insertImplicitCasts then snd (castTo t' so.soTyp oneinit')
      else oneinit'
    in
    let preinit' = setOneInit preinit so.soOff (SinglePre (init_expr,r)) in
    (* Move on *)
    advanceSubobj so;
    let se = acc @@ (se,ghost) in
    doInit local_env isconst add_implicit_ensures preinit' so se restil
  (* An array with a compound initializer. The initializer is for the
   * array elements *)
  | TArray (bt, leno, _, _), (A.NEXT_INIT, A.COMPOUND_INIT initl) :: restil ->
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let len = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, len, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      match initl with
      | [] ->
        (* we must actually indicate that there is some initializer, albeit
           empty, to our parent. This is in particular important if said
           parent is an array of indeterminate size, as the number of
           initializers of its children matters. *)
        let preinit' = setOneInit preinit so'.curOff (empty_preinit()) in
        (* zero initialization will be done anyway,
           no need to change the chunk.*)
        acc, preinit', []
      | _ ->
        doInit
          local_env isconst add_implicit_ensures preinit so' acc initl
    in
    if initl' <> [] then
      Kernel.warning ~current:true
        "Too many initializers for array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env isconst add_implicit_ensures preinit' so acc' restil
  (* We have a designator that tells us to select the matching union field.
   * This is to support a GCC extension *)
  | TComp(ci, _, _) as targ,
    [(A.NEXT_INIT,
      A.COMPOUND_INIT
        [(A.INFIELD_INIT ("___matching_field", A.NEXT_INIT),
          A.SINGLE_INIT oneinit)])]
    when not ci.cstruct ->
    (* Do the expression to find its type *)
    let _, c, _, t' =
      doExp (no_paren_local_env local_env) isconst oneinit (AExp None)
    in
    clean_up_chunk_locals c;
    let t'noattr = Cil.typeDeepDropAllAttributes t' in
    let rec findField = function
      | [] ->
        Kernel.fatal ~current:true "Cannot find matching union field in cast"
      | fi :: _rest when
          Typ.equal (Cil.typeDeepDropAllAttributes fi.ftype) t'noattr -> fi
      | _ :: rest -> findField rest
    in
    (* If this is a cast from union X to union X *)
    if Typ.equal t'noattr (Cil.typeDeepDropAllAttributes targ) then
      doInit
        local_env isconst add_implicit_ensures preinit so acc
        [(A.NEXT_INIT, A.SINGLE_INIT oneinit)]
    else
      (* If this is a GNU extension with field-to-union cast find the field *)
      let fi = findField ci.cfields in
      (* Change the designator and redo *)
      doInit
        local_env isconst add_implicit_ensures preinit so acc
        [A.INFIELD_INIT (fi.fname, A.NEXT_INIT), A.SINGLE_INIT oneinit]

  (* A structure with a composite initializer. We initialize the fields*)
  | TComp (comp, _, _), (A.NEXT_INIT, A.COMPOUND_INIT initl) :: restil ->
    (* Create a separate subobject iterator *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the comp *)
    so'.stack <- [InComp(so'.curOff, comp, fieldsToInit comp None)];
    normalSubobj so';
    let acc', preinit', initl' =
      match initl with
      | [] -> (* empty initializer, a GNU extension to indicate 
                 0-initialization. We must indicate to our parent that we are
                 here, though. *)
        let preinit' = setOneInit preinit so'.curOff (empty_preinit()) in
        acc, preinit', []
      | _ ->
        doInit local_env isconst add_implicit_ensures preinit so' acc initl
    in
    if initl' <> [] then
      Kernel.warning ~current:true "Too many initializers for structure";
    (* Advance past the structure *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env isconst add_implicit_ensures preinit' so acc' restil
  (* A scalar with a initializer surrounded by a number of braces *)
  | t, (A.NEXT_INIT, next) :: restil ->
    begin
      let rec find_one_init c =
        match c with
        | A.COMPOUND_INIT [A.NEXT_INIT,next] -> find_one_init next
        | A.SINGLE_INIT oneinit -> oneinit
        | _ -> raise Not_found
      in
      try
        let oneinit = find_one_init next in
        let r,se, oneinit', t' =
          doExp (no_paren_local_env local_env)
            isconst oneinit (AExp(Some so.soTyp))
        in
        let r = Cil_datatype.Lval.Set.of_list r in
        let init_expr = makeCastT oneinit' t' so.soTyp in
        let preinit' = setOneInit preinit so.soOff (SinglePre (init_expr, r)) in
        (* Move on *)
        advanceSubobj so;
        let se = acc @@ (se, ghost) in
        doInit local_env isconst add_implicit_ensures preinit' so se restil
      with Not_found ->
        abort_context
          "scalar value (of type %a) initialized by compound initializer" 
          Cil_printer.pp_typ t
    end
  (* We have a designator *)
  | _, (what, ie) :: restil when what != A.NEXT_INIT ->
    (* Process a designator and position to the designated subobject *)
    let addressSubobj
        (so: subobj)
        (what: A.initwhat)
        (acc: chunk) : chunk =
      (* Always start from the current element *)
      so.stack <- []; so.eof <- false;
      normalSubobj so;
      let rec address (what: A.initwhat) (acc: chunk)  : chunk =
        match what with
        | A.NEXT_INIT -> acc
        | A.INFIELD_INIT (fn, whatnext) -> begin
            match unrollType so.soTyp with
            | TComp (comp, _, _) ->
              let toinit = fieldsToInit comp (Some fn) in
              so.stack <- InComp(so.soOff, comp, toinit) :: so.stack;
              normalSubobj so;
              address whatnext acc
            | _ ->
              Kernel.fatal ~current:true
                "Field designator %s not in a struct " fn
          end

        | A.ATINDEX_INIT(idx, whatnext) -> begin
            match unrollType so.soTyp with
            | TArray (bt, leno, _, _) ->
              let ilen = integerArrayLength leno in
              let nextidx', doidx =
                let (r,doidx, idxe', _) =
                  doExp
                    (no_paren_local_env local_env) true idx (AExp(Some intType))
                in

                let doidx = add_reads ~ghost idxe'.eloc r doidx in
                match constFoldToInt idxe', isNotEmpty doidx with
                | Some x, false -> Integer.to_int x, doidx
                | _ ->
                  abort_context
                    "INDEX initialization designator is not a constant"
              in
              if nextidx' < 0 || nextidx' >= ilen then
                abort_context "INDEX designator is outside bounds";
              so.stack <-
                InArray(so.soOff, bt, ilen, ref nextidx') :: so.stack;
              normalSubobj so;
              address whatnext (acc @@ (doidx, ghost))

            | _ -> abort_context "INDEX designator for a non-array"
          end

        | A.ATINDEXRANGE_INIT _ -> abort_context "addressSubobj: INDEXRANGE"
      in
      address what acc
    in
    (* First expand the INDEXRANGE by making copies *)
    let rec expandRange (top: A.initwhat -> A.initwhat) = function
      | A.INFIELD_INIT (fn, whatnext) ->
        expandRange (fun what -> top (A.INFIELD_INIT(fn, what))) whatnext
      | A.ATINDEX_INIT (idx, whatnext) ->
        expandRange (fun what -> top (A.ATINDEX_INIT(idx, what))) whatnext
      | A.ATINDEXRANGE_INIT (idxs, idxe) ->
        let (rs, doidxs, idxs', _) =
          doExp (no_paren_local_env local_env) true idxs (AExp(Some intType))
        in
        let (re, doidxe, idxe', _) =
          doExp (no_paren_local_env local_env) true idxe (AExp(Some intType))
        in
        let doidxs = add_reads ~ghost idxs'.eloc rs doidxs in
        let doidxe = add_reads ~ghost idxe'.eloc re doidxe in
        if isNotEmpty doidxs || isNotEmpty doidxe then
          Kernel.fatal ~current:true "Range designators are not constants";
        let first, last =
          match constFoldToInt idxs', constFoldToInt idxe' with
          | Some s, Some e -> Integer.to_int s, Integer.to_int e
          | _ ->
            Kernel.fatal ~current:true
              "INDEX_RANGE initialization designator is not a constant"
        in
        if first < 0 || first > last then
          Kernel.error ~once:true ~current:true
            "start index larger than end index in range initializer";
        let rec loop (i: int) =
          if i > last then restil
          else
            (top (A.ATINDEX_INIT(
                 { expr_node = A.CONSTANT(A.CONST_INT(string_of_int i));
                   expr_loc = fst idxs.expr_loc, snd idxe.expr_loc},
                 A.NEXT_INIT)), ie)
            :: loop (i + 1)
        in
        doInit
          local_env isconst add_implicit_ensures preinit so acc (loop first)
      | A.NEXT_INIT -> (* We have not found any RANGE *)
        let acc' = addressSubobj so what acc in
        doInit
          local_env isconst add_implicit_ensures preinit so acc'
          ((A.NEXT_INIT, ie) :: restil)
    in
    expandRange (fun x -> x) what
  | t, (_what, _ie) :: _ ->
    abort_context "doInit: cases for t=%a" Cil_printer.pp_typ t

(* Create and add to the file (if not already added) a global. Return the
 * varinfo *)
and createGlobal ghost logic_spec ((t,s,b,attr_list) : (typ * storage * bool * A.attribute list))
    (((n,ndt,a,cloc), inite) : A.init_name) : varinfo =
  Kernel.debug ~dkey:Kernel.dkey_typing_global "createGlobal: %s" n;
  (* If the global is a Frama-C builtin, set the generated flag *)
  if is_stdlib_macro n && get_current_stdheader () = "" then begin
    Kernel.warning ~wkey:Kernel.wkey_cert_msc_38
      "Attempt to declare %s as external identifier outside of the stdlib. \
       It is supposed to be a macro name and cannot be declared. See CERT C \
       coding rule MSC38-C" n
  end;
  let is_fc_builtin {A.expr_node=enode} =
    match enode with A.VARIABLE "FC_BUILTIN" -> true | _ -> false
  in
  let isgenerated =
    List.exists (fun (_,el) -> List.exists is_fc_builtin el) a
  in
  (* Make a first version of the varinfo *)
  let vi = makeVarInfoCabs ~ghost ~isformal:false
      ~isglobal:true ~isgenerated (convLoc cloc) (t,s,b,attr_list) (n,ndt,a)
  in
  (* Add the variable to the environment before doing the initializer
   * because it might refer to the variable itself *)
  if isFunctionType vi.vtype then begin
    if inite != A.NO_INIT  then
      Kernel.error ~once:true ~current:true
        "Function declaration with initializer (%s)\n" vi.vname;
  end else if Extlib.has_some logic_spec then begin
    Kernel.warning ~wkey:Kernel.wkey_annot_error ~current:true ~once:true
      "Global variable %s is not a function. It cannot have a contract."
      vi.vname
  end;
  let isadef =
    not (isFunctionType vi.vtype) &&
    (inite != A.NO_INIT
     ||
     (* tentative definition, but definition nevertheless. *)
     vi.vstorage = NoStorage || vi.vstorage = Static)
  in
  let vi, alreadyInEnv = makeGlobalVarinfo isadef vi in
  (* Do the initializer and complete the array type if necessary *)
  let init : init option =
    if inite = A.NO_INIT then
      None
    else
      let se, ie', et, _ = doInitializer (ghost_local_env ghost) vi inite in
      (* Maybe we now have a better type?  Use the type of the
       * initializer only if it really differs from the type of
       * the variable. *)
      if unrollType vi.vtype != unrollType et then
        Cil.update_var_type vi et;
      if isNotEmpty se then begin
        Kernel.error ~once:true ~current:true
          "invalid global initializer @[%a@]" d_chunk se;
      end;
      Some ie'
  in

  try
    let oldloc = H.find alreadyDefined vi.vname in
    if init != None then begin
      (* function redefinition is taken care of elsewhere. *)
      Kernel.error ~once:true ~current:true
        "Global %s was already defined at %a" vi.vname Cil_printer.pp_location oldloc;
    end;
    Kernel.debug ~dkey:Kernel.dkey_typing_global
      " global %s was already defined" vi.vname;
    (* Do not declare it again, but update the spec if any *)
    if isFunctionType vi.vtype then
      begin
        match logic_spec with
        | None -> ()
        | Some (spec,_) ->
          let l1 = get_formals vi in
          let l2 = Cil.getFormalsDecl vi in
          List.iter2
            (fun x y ->
               if x != y then
                 Kernel.fatal
                   "Function %s: formals are not shared between AST and \
                    FormalDecls table" vi.vname)
            l1 l2;
          try
            let known_behaviors = find_existing_behaviors vi in
            let spec =
              Ltyping.funspec
                known_behaviors vi (Some(get_formals vi)) vi.vtype spec
            in
            update_funspec_in_theFile vi spec
          with LogicTypeError ((source,_),msg) ->
            Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
              "%s. Ignoring specification of function %s" msg vi.vname
      end ;
    vi
  with Not_found -> begin
      (* Not already defined *)
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        " first definition for %s(%d)\n" vi.vname vi.vid;
      if init != None then begin
        (* weimer: Sat Dec  8 17:43:34  2001
         * MSVC NT Kernel headers include this lovely line:
         * extern const GUID __declspec(selectany) \
         *  MOUNTDEV_MOUNTED_DEVICE_GUID = { 0x53f5630d, 0xb6bf, 0x11d0, { \
         *  0x94, 0xf2, 0x00, 0xa0, 0xc9, 0x1e, 0xfb, 0x8b } };
         * So we allow "extern" + "initializer" if "const" is
         * around. *)
        (* sm: As I read the ISO spec, in particular 6.9.2 and 6.7.8,
         * "extern int foo = 3" is exactly equivalent to "int foo = 3";
         * that is, if you put an initializer, then it is a definition,
         * and "extern" is redundantly giving the name external linkage.
         * gcc emits a warning, I guess because it is contrary to
         * usual practice, but I think CIL warnings should be about
         * semantic rather than stylistic issues, so I see no reason to
         * even emit a warning. *)
        if vi.vstorage = Extern then
          vi.vstorage <- NoStorage;     (* equivalent and canonical *)

        H.add alreadyDefined vi.vname (CurrentLoc.get ());
        IH.remove mustTurnIntoDef vi.vid;
        cabsPushGlobal (GVar(vi, {init = init}, CurrentLoc.get ()));
        vi
      end else begin
        if not (isFunctionType vi.vtype) &&
           (vi.vstorage = NoStorage || vi.vstorage = Static)
           && not (IH.mem mustTurnIntoDef vi.vid) then
          begin
            IH.add mustTurnIntoDef vi.vid true
          end;
        if not alreadyInEnv then begin (* Only one declaration *)
          (* If it has function type it is a prototype *)
          (* NB: We add the formal prms in the env*)
          if isFunctionType vi.vtype then begin
            if not vi.vdefined then
              setFormalsDecl vi vi.vtype;
            let spec =
              match logic_spec with
              | None -> empty_funspec ()
              | Some (spec,loc) ->
                begin
                  CurrentLoc.set loc;
                  try
                    (* it can not have old behavior names, since this is the
                       first time we see the declaration.
                    *)
                    Ltyping.funspec [] vi None vi.vtype spec
                  with LogicTypeError ((source,_),msg) ->
                    Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
                      "%s. Ignoring specification of function %s" msg vi.vname;
                    empty_funspec ()
                end
            in
            cabsPushGlobal (GFunDecl (spec, vi, CurrentLoc.get ()));
          end
          else
            cabsPushGlobal (GVarDecl (vi, CurrentLoc.get ()));
          vi
        end else begin
          Kernel.debug ~dkey:Kernel.dkey_typing_global
            " already in env %s" vi.vname;
          (match logic_spec with
           | None -> ()
           | Some (spec,loc) ->
             CurrentLoc.set loc;
             let merge_spec = function
               | GFunDecl(old_spec, _, _) ->
                 let behaviors =
                   List.map (fun b -> b.b_name) old_spec.spec_behavior
                 in
                 let spec =
                   try
                     Ltyping.funspec behaviors vi None vi.vtype spec
                   with LogicTypeError ((source,_),msg) ->
                     Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
                       "%s. Ignoring specification of function %s"
                       msg vi.vname;
                     empty_funspec ()
                 in
                 Cil.CurrentLoc.set vi.vdecl;
                 Logic_utils.merge_funspec old_spec spec
               | _ -> assert false
             in
             update_fundec_in_theFile vi merge_spec
          );
          vi
        end
      end
    end
    (*
      ignore (E.log "Env after processing global %s is:@\n%t@\n"
      n docEnv);
      ignore (E.log "Alpha after processing global %s is:@\n%t@\n"
      n docAlphaTable)
    *)

(* it can happen that the variable to be initialized appears in the
   auxiliary statements that contribute to its initialization (and thus
   are meant to occur before the corresponding Local_init statement. In
   that case, this function creates an auxiliary variable that is never
   defined as a placeholder.
   Note that in any case, if the execution attempts to evaluate
   the variable (either original or placeholder), the behavior is undefined.
   There are some cases where the evaluation will succeed, though, e.g. with
   size_t x = sizeof(x) > 6 ? sizeof(x): 6;
*)
and cleanup_autoreference vi chunk =
  let temp = ref None in
  let calls = ref [] in
  let extract_calls () =
    let res = !calls in
    calls := [];
    res
  in
  let vis =
    object(self)
      inherit Cil.nopCilVisitor

      method! vinst = function
        | Call _ | Local_init(_,ConsInit _,_) ->
          calls := ref (Extlib.the self#current_stmt) :: !calls;
          DoChildren
        | _ -> DoChildren

      method! vvrbl v =
        if Cil_datatype.Varinfo.equal v vi then begin
          match !temp with
          | Some v' -> ChangeTo v'
          | None ->
            let v' = newTempVar (vi.vname ^ " initialization") true vi.vtype in
            temp := Some v';
            ChangeTo v'
        end else SkipChildren
    end
  in
  let transform_lvals l = List.map (visitCilLval vis) l in
  let treat_one (s, m, w, r, _) =
    let s' = visitCilStmt vis s in
    let m' = transform_lvals m in
    let w' = transform_lvals w in
    let r' = transform_lvals r in
    let c' = extract_calls () in
    (s', m', w', r', c')
  in
  let stmts = List.map treat_one chunk.stmts in
  match !temp with
  | None -> chunk
  | Some v -> local_var_chunk { chunk with stmts } v

(* Must catch the Static local variables. Make them global *)
and createLocal ghost ((_, sto, _, _) as specs)
    ((((n, ndt, a, cloc) : A.name),
      (inite: A.init_expression)) as init_name)
  : chunk =
  let loc = convLoc cloc in
  (* Check if we are declaring a function *)
  let rec isProto (dt: decl_type) : bool =
    match dt with
    | PROTO (JUSTBASE, _, _) -> true
    | PROTO (x, _, _) -> isProto x
    | PARENTYPE (_, x, _) -> isProto x
    | ARRAY (x, _, _) -> isProto x
    | PTR (_, x) -> isProto x
    | _ -> false
  in
  match ndt with
  (* Maybe we have a function prototype in local scope. Make it global. We
   * do this even if the storage is Static *)
  | _ when isProto ndt ->
    let vi = createGlobal ghost None specs init_name in
    (* Add it to the environment to shadow previous decls *)
    addLocalToEnv n (EnvVar vi);
    LocalFuncHook.apply vi;
    empty

  | _ when sto = Static ->
    Kernel.debug ~dkey:Kernel.dkey_typing_global
      "createGlobal (local static): %s" n;
    (* Now alpha convert it to make sure that it does not conflict with
     * existing globals or locals from this function. *)
    let full_name = !currentFunctionFDEC.svar.vname ^ "_" ^ n in
    let newname, _  = newAlphaName true "" full_name in
    (* Make it global  *)
    let vi = makeVarInfoCabs ~ghost ~isformal:false
        ~isglobal:true
        loc specs (n, ndt, a) in
    vi.vname <- newname;
    vi.vattr <- Cil.addAttribute (Attr (fc_local_static,[])) vi.vattr;
    (* However, we have a problem if a real global appears later with the
     * name that we have happened to choose for this one. Remember these names
     * for later. *)
    H.add staticLocals vi.vname vi;
    (* Add it to the environment as a local so that the name goes out of
     * scope properly *)
    addLocalToEnv n (EnvVar vi);

    (* Maybe this is an array whose length depends on something with local
       scope, e.g. "static char device[ sizeof(local) ]".
       Const-fold the type to fix this. *)
    Cil.update_var_type vi (constFoldType vi.vtype);

    let init : init option =
      if inite = A.NO_INIT then
        None
      else begin
        let se, ie', et, _ = doInitializer (ghost_local_env ghost) vi inite in
        (* Maybe we now have a better type?  Use the type of the
         * initializer only if it really differs from the type of
         * the variable. *)
        if unrollType vi.vtype != unrollType et then
          Cil.update_var_type vi et;
        if isNotEmpty se then
          Kernel.error ~once:true ~current:true "global static initializer";
        (* Check that no locals are referred by the initializer *)
        check_no_locals_in_initializer ie';
        (* Maybe the initializer refers to the function itself.
           Push a prototype for the function, just in case. *)
        cabsPushGlobal
          (GFunDecl (empty_funspec (), !currentFunctionFDEC.svar,
                     CurrentLoc.get ()));
        Cil.setFormalsDecl
          !currentFunctionFDEC.svar !currentFunctionFDEC.svar.vtype;
        Some ie'
      end
    in
    cabsPushGlobal (GVar(vi, {init = init}, CurrentLoc.get ()));
    static_var_chunk empty vi

  (* Maybe we have an extern declaration. Make it a global *)
  | _ when sto = Extern ->
    let vi = createGlobal ghost None specs init_name in
    (* Add it to the local environment to ensure that it shadows previous
     * local variables *)
    addLocalToEnv n (EnvVar vi);
    empty

  | _ ->
    (* Make a variable of potentially variable size. If se0 <> empty then
     * it is a variable size variable *)
    let vi,se0,len,isvarsize =
      makeVarSizeVarInfo ghost loc specs (n, ndt, a) in

    let vi = alphaConvertVarAndAddToEnv true vi in        (* Replace vi *)
    if isvarsize then begin
      let free = vla_free_fun () in
      let destructor = AStr free.vname in
      let attr = Attr (frama_c_destructor, [destructor]) in
      vi.vdefined <- true;
      vi.vattr <- Cil.addAttribute attr vi.vattr;
    end;
    let se1 =
      if isvarsize then begin (* Variable-sized array *)
        (* Make a local variable to keep the length *)
        let savelen =
          makeVarInfoCabs
            ~ghost
            ~isformal:false
            ~isglobal:false
            loc
            (theMachine.typeOfSizeOf, NoStorage, false, [])
            ("__lengthof_" ^ vi.vname,JUSTBASE, [])
        in
        (* Register it *)
        let savelen = alphaConvertVarAndAddToEnv true savelen in
        let se0 = local_var_chunk se0 savelen in
          (* Compute the allocation size *)
          let elt_size = new_exp ~loc (SizeOf (Cil.typeOf_pointed vi.vtype)) in
          let alloca_size =
            new_exp ~loc
              (BinOp(Mult,
                     elt_size,
                     new_exp ~loc (Lval (var savelen)),
                     theMachine.typeOfSizeOf))
          in
        (* Register the length *)
        IH.add varSizeArrays vi.vid alloca_size;
        (* There can be no initializer for this *)
        if inite != A.NO_INIT then
          Kernel.error ~once:true ~current:true
            "Variable-sized array cannot have initializer";
        let se0 = 
        (* add an assertion to ensure the given size is correctly bound:
           assert alloca_bounds: 0 < elt_size * array_size <= max_bounds 
        *)
          (se0 +++ (
            let castloc = CurrentLoc.get () in
            let talloca_size = 
              let telt_size = Logic_utils.expr_to_term ~cast:false elt_size in
              let tlen = Logic_utils.expr_to_term ~cast:false len in
              Logic_const.term (TBinOp (Mult,telt_size,tlen)) telt_size.term_type 
            in
            let pos_size =
              let zero =  Logic_const.tinteger ~loc:castloc 0 in
              Logic_const.prel ~loc:castloc (Rlt, zero, talloca_size)
            in
            let max_size = 
              let szTo = Cil.bitsSizeOf theMachine.typeOfSizeOf in
              let max_bound =  Logic_const.tint ~loc:castloc (Cil.max_unsigned_number szTo) in
              Logic_const.prel ~loc:castloc (Rle, talloca_size, max_bound) 
            in
            let alloca_bounds = Logic_const.pand ~loc:castloc (pos_size, max_size) in
            let alloca_bounds = { alloca_bounds with pred_name = ["alloca_bounds"] } in
            let annot =
              Logic_const.new_code_annotation (AAssert ([], alloca_bounds))
            in
            (mkStmtOneInstr ~ghost ~valid_sid
               (Code_annot (annot, castloc)),
             [],[],[])))                  
        in
        let setlen =  se0 +++
                      (mkStmtOneInstr ~ghost ~valid_sid
                         (Set(var savelen, makeCast len savelen.vtype,
                              CurrentLoc.get ())),
                       [],[],[])
        in
        (* Initialize the variable *)
        let alloca: varinfo = vla_alloc_fun () in
        if Kernel.DoCollapseCallCast.get () then
          (* do it in one step *)
          setlen +++
          (mkStmtOneInstr ~ghost ~valid_sid
             (Local_init (vi, ConsInit(alloca,[ alloca_size ],Plain_func),loc)),
           [],[var vi],[])
        else begin
          (* do it in two *)
          let rt, _, _, _ = splitFunctionType alloca.vtype in
          let tmp =
            newTempVar 
              (Format.asprintf "alloca(%a)" Cil_printer.pp_exp alloca_size)
              false rt
          in
          tmp.vdefined <- true;
          (local_var_chunk setlen tmp)
          +++ (mkStmtOneInstr ~ghost ~valid_sid
                 (Local_init
                    (tmp,ConsInit(alloca,[alloca_size],Plain_func),loc)),
               [],[],[])
          +++ (mkStmtOneInstr ~ghost ~valid_sid
                 (Local_init
                    (vi,AssignInit
                       (SingleInit
                          (makeCast (new_exp ~loc (Lval(var tmp))) vi.vtype)),
                     CurrentLoc.get ())),
               [],[var vi],[var tmp])
        end
      end else empty
    in
    let se1 = local_var_chunk se1 vi in
    if inite = A.NO_INIT then
      se1 (* skipChunk *)
    else begin
      (* TODO: if vi occurs in se4, this is not a real initialization. *)
      vi.vdefined <- true;
      let se4, ie', et, r = doInitializer (ghost_local_env ghost) vi inite in
      let se4 = cleanup_autoreference vi se4 in
      (* Fix the length *)
      (match vi.vtype, ie', et with
       (* We have a length now *)
       | TArray(_,None, _, _), _, TArray(_, Some _, _, _) ->
         Cil.update_var_type vi et
       (* Initializing a local array *)
       | TArray(TInt((IChar|IUChar|ISChar), _) as bt, None, l, a),
         SingleInit({enode = Const(CStr s);eloc=loc}), _ ->
         Cil.update_var_type vi
           (TArray(bt,
                   Some (integer ~loc (String.length s + 1)),
                   l, a))
       | _, _, _ -> ());

      (* Now create assignments instead of the initialization *)
      (se1 @@ (se4, ghost))
      @@
      (i2c
         (Cil.mkStmtOneInstr
            ~ghost ~valid_sid (Local_init(vi,AssignInit ie',loc)),
          [], [(Var vi,NoOffset)], Cil_datatype.Lval.Set.elements r), ghost)
    end

and doAliasFun vtype (thisname:string) (othername:string)
    (sname:single_name) (loc: cabsloc) : unit =
  (* This prototype declares that name is an alias for
     othername, which must be defined in this file *)
  (*   E.log "%s is alias for %s at %a\n" thisname othername  *)
  (*     Cil_printer.pp_location !currentLoc; *)
  let rt, formals, isva, _ = splitFunctionType vtype in
  if isva then Kernel.error ~once:true ~current:true "alias unsupported with varargs";
  let args = List.map
      (fun (n,_,_) -> { expr_loc = loc; expr_node = A.VARIABLE n})
      (argsToList formals) in
  let call = A.CALL ({expr_loc = loc; expr_node = A.VARIABLE othername}, args)
  in
  let stmt = {stmt_ghost = false;
              stmt_node = if isVoidType rt then
                  A.COMPUTATION({expr_loc = loc; expr_node = call}, loc)
                else A.RETURN({expr_loc = loc; expr_node = call}, loc)}
  in
  let body = { A.blabels = []; A.battrs = []; A.bstmts = [stmt] } in
  let fdef = A.FUNDEF (None, sname, body, loc, loc) in
  ignore (doDecl empty_local_env true fdef);
  (* get the new function *)
  let v,_ =
    try lookupGlobalVar thisname
    with Not_found -> abort_context "error in doDecl"
  in
  v.vattr <- dropAttribute "alias" v.vattr


(* Do one declaration *)
and doDecl local_env (isglobal: bool) : A.definition -> chunk = function
  | A.DECDEF (logic_spec, (s, nl), loc) ->
    CurrentLoc.set (convLoc loc);
    (* Do the specifiers exactly once *)
    let sugg =
      match nl with
      | [] -> ""
      | ((n, _, _, _), _) :: _ -> n
    in
    let ghost = local_env.is_ghost in
    let spec_res = doSpecList ghost sugg s in
    (* Do all the variables and concatenate the resulting statements *)
    let doOneDeclarator (acc: chunk) (name: init_name) =
      let (n,ndt,a,l),_ = name in
      if isglobal then begin
        let bt,_,_,attrs = spec_res in
        let vtype, nattr =
          doType local_env.is_ghost false
            (AttrName false) bt (A.PARENTYPE(attrs, ndt, a)) in
        (match filterAttributes "alias" nattr with
         | [] -> (* ordinary prototype. *)
           ignore (createGlobal local_env.is_ghost logic_spec spec_res name)
         (*  E.log "%s is not aliased\n" name *)
         | [Attr("alias", [AStr othername])] ->
           if not (isFunctionType vtype) || local_env.is_ghost then begin
             Kernel.warning ~current:true
               "%a: CIL only supports attribute((alias)) for C functions."
               Cil_printer.pp_location (CurrentLoc.get ());
             ignore (createGlobal ghost logic_spec spec_res name)
           end else
             doAliasFun vtype n othername (s, (n,ndt,a,l)) loc
         | _ ->
           Kernel.error ~once:true ~current:true
             "Bad alias attribute at %a" Cil_printer.pp_location (CurrentLoc.get()));
        acc
      end else
        acc @@ (createLocal ghost spec_res name, ghost)
    in
    let res = List.fold_left doOneDeclarator empty nl in
    if isglobal then res
    else begin
      match logic_spec with
      | None -> res
      | Some (spec,loc) ->
        let loc' = convLoc loc in
        begin
          try
            let spec =
              Ltyping.code_annot loc' local_env.known_behaviors
                (Ctype !currentReturnType) (Logic_ptree.AStmtSpec ([],spec))
            in
            append_chunk_to_annot ~ghost
              (s2c
                 (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (spec,loc'))))
              res
          with LogicTypeError ((source,_),msg) ->
            Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
              "%s. Ignoring code annotation" msg;
            res
        end
    end
  | A.TYPEDEF (ng, loc) ->
    CurrentLoc.set (convLoc loc); doTypedef local_env.is_ghost ng; empty

  | A.ONLYTYPEDEF (s, loc) ->
    CurrentLoc.set (convLoc loc); doOnlyTypedef local_env.is_ghost s; empty

  | A.GLOBASM (s,loc) when isglobal ->
    CurrentLoc.set (convLoc loc);
    cabsPushGlobal (GAsm (s, CurrentLoc.get ())); empty

  | A.PRAGMA (a, loc) when isglobal -> begin
      CurrentLoc.set (convLoc loc);
      match doAttr local_env.is_ghost ("dummy", [a]) with
      | [Attr("dummy", [a'])] ->
        let a'' =
          match a' with
          | ACons (s, args) ->
            process_align_pragma s args;
            process_stdlib_pragma s args >>?
            process_pack_pragma
          | _ -> (* Cil.fatal "Unexpected attribute in #pragma" *)
            Kernel.warning ~current:true "Unexpected attribute in #pragma";
            Some (Attr ("", [a']))
        in
        Extlib.may
          (fun a'' ->
             cabsPushGlobal (GPragma (a'', CurrentLoc.get ())))
          a'';
        empty

      | _ -> Kernel.fatal ~current:true "Too many attributes in pragma"
    end

  | A.FUNDEF (spec,((specs,(n,dt,a, _)) : A.single_name),
              (body : A.block), loc1, loc2) when isglobal ->
    begin
      let ghost = local_env.is_ghost in
      let idloc = loc1 in
      let funloc = fst loc1, snd loc2 in
      let endloc = loc2 in
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "Definition of %s at %a\n" n Cil_printer.pp_location idloc;
      CurrentLoc.set idloc;
      IH.clear callTempVars;

      (* Make the fundec right away, and we'll populate it later. We
       * need this throughout the code to create temporaries. *)
      currentFunctionFDEC :=
        { svar     = makeGlobalVar ~temp:false n voidType;
          slocals  = []; (* For now we'll put here both the locals and
                          * the formals. Then "endFunction" will
                          * separate them *)
          sformals = []; (* Not final yet *)
          smaxid   = 0;
          sbody    = dummy_function.sbody; (* Not final yet *)
          smaxstmtid = None;
          sallstmts = [];
          sspec = empty_funspec ()
        };
      !currentFunctionFDEC.svar.vdecl <- idloc;

      (* Setup the environment. Add the formals to the locals. Maybe
       * they need alpha-conv  *)
      enterScope ();  (* Start the scope *)
      ignore (V.visitCabsBlock (new gatherLabelsClass) body);
      CurrentLoc.set idloc;
      IH.clear varSizeArrays;

      (* Enter all the function's labels into the alpha conversion table *)
      ignore (V.visitCabsBlock (new registerLabelsVisitor) body);
      CurrentLoc.set idloc;

      (* Do not process transparent unions in function definitions.
       * We'll do it later *)
      transparentUnionArgs := [];

      let bt,sto,inl,attrs = doSpecList local_env.is_ghost n specs in
      !currentFunctionFDEC.svar.vinline <- inl;
      let ftyp, funattr =
        doType local_env.is_ghost false
          (AttrName false) bt (A.PARENTYPE(attrs, dt, a)) in
      (* Format.printf "Attrs are %a@." d_attrlist funattr; *)
      Cil.update_var_type !currentFunctionFDEC.svar ftyp;
      !currentFunctionFDEC.svar.vattr <- funattr;
      !currentFunctionFDEC.svar.vstorage <- sto;
      let vi,has_decl =
        makeGlobalVarinfo true !currentFunctionFDEC.svar in
      (* Add the function itself to the environment. Add it before
       * you do the body because the function might be recursive. Add
       * it also before you add the formals to the environment
       * because there might be a formal with the same name as the
       * function and we want it to take precedence. *)
      (* Make a variable out of it and put it in the environment *)
      !currentFunctionFDEC.svar <- vi;

      (* If it is extern inline then we add it to the global
       * environment for the original name as well. This will ensure
       * that all uses of this function will refer to the renamed
       * function *)
      addGlobalToEnv n (EnvVar !currentFunctionFDEC.svar);
      if H.mem alreadyDefined !currentFunctionFDEC.svar.vname then
        Kernel.error ~once:true ~current:true "There is a definition already for %s" n;

      H.add alreadyDefined !currentFunctionFDEC.svar.vname idloc;


          (*
            ignore (E.log "makefunvar:%s@\n type=%a@\n vattr=%a@\n"
            n Cil_printer.pp_typ thisFunctionVI.vtype
            d_attrlist thisFunctionVI.vattr);
          *)

      (* makeGlobalVarinfo might have changed the type of the function
       * (when combining it with the type of the prototype). So get the
       * type only now. *)

      (**** Process the TYPE and the FORMALS ***)
      let _ =
        let (returnType, formals_t, isvararg, funta) =
          splitFunctionTypeVI !currentFunctionFDEC.svar
        in
        (* Record the returnType for doStatement *)
        currentReturnType   := returnType;


        (* Create the formals and add them to the environment. *)
        (* sfg: extract tsets for the formals from dt *)
        let doFormal (loc : location) (fn, ft, fa) =
          let f = makeVarinfo ~temp:false false true fn ft in
          (f.vdecl <- loc;
           f.vattr <- fa;
           alphaConvertVarAndAddToEnv true f)
        in
        let rec doFormals fl' ll' =
          begin
            match (fl', ll') with
            | [], _ -> []

            | fl, [] -> (* no more locs available *)
              List.map (doFormal (CurrentLoc.get ())) fl

            | f::fl, (_,(_,_,_,l))::ll ->
              (* sfg: these lets seem to be necessary to
               *  force the right order of evaluation *)
              let f' = doFormal (convLoc l) f in
              let fl' = doFormals fl ll in
              f' :: fl'
          end
        in
        let fmlocs = (match dt with PROTO(_, fml, _) -> fml | _ -> []) in
        let formals = doFormals (argsToList formals_t) fmlocs in
        (* in case of formals referred to in types of others, doType has
           put dummy varinfos. We need to fix them now that we have proper
           bindings.
           TODO: completely refactor the way formals' typechecking is done.
        *)
        let () = fixFormalsType formals in

        (* Recreate the type based on the formals. *)
        let ftype = TFun(returnType,
                         Some (List.map (fun f ->
                             (f.vname,
                              f.vtype,
                              f.vattr)) formals),
                         isvararg, funta) in

        (*log "Funtype of %s: %a\n" n Cil_printer.pp_typ ftype;*)

        (* Now fix the names of the formals in the type of the function
         * as well *)
        Cil.update_var_type !currentFunctionFDEC.svar ftype;
        !currentFunctionFDEC.sformals <- formals;
        (* we will revisit the spec for the declaration in order
           to change the formals according to the new variables.
        *)
        if has_decl then begin
          try
            Hashtbl.add alpha_renaming
              vi.vid
              (Cil.create_alpha_renaming
                 (Cil.getFormalsDecl vi) formals)
          with Not_found ->
            (* the declaration comes from an
               implicit prototype. We do not have
               any spec anyway. However, we will have a declaration
               in the resulting AST, to which we must attach some
               formals.
            *)
            Cil.unsafeSetFormalsDecl vi formals
        end;
      in
      (* Now change the type of transparent union args back to what it
       * was so that the body type checks. We must do it this late
       * because makeGlobalVarinfo from above might choke if we give
       * the function a type containing transparent unions  *)
      let _ =
        let rec fixbackFormals (idx: int) (args: varinfo list) : unit=
          match args with
          | [] -> ()
          | a :: args' ->
            (* Fix the type back to a transparent union type *)
            (try
               let origtype = List.assq idx !transparentUnionArgs in
               Cil.update_var_type a origtype;
             with Not_found -> ());
            fixbackFormals (idx + 1) args'
        in
        fixbackFormals 0 !currentFunctionFDEC.sformals;
        transparentUnionArgs := [];
      in
      let behaviors = find_existing_behaviors !currentFunctionFDEC.svar in
      (******* Now do the spec *******)
      begin
        match spec with
        | Some (spec,loc) ->
          CurrentLoc.set loc;
          (try
             !currentFunctionFDEC.sspec <-
               Ltyping.funspec behaviors
                 !currentFunctionFDEC.svar
                 (Some !currentFunctionFDEC.sformals)
                 !currentFunctionFDEC.svar.vtype spec
           with LogicTypeError ((source,_),msg) ->
             Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
               "%s. Ignoring logic specification of function %s"
               msg !currentFunctionFDEC.svar.vname)
        | None -> ()
      end;
      (* Merge pre-existing spec if needed. *)
      if has_decl then begin
        let merge_spec = function
          | GFunDecl(old_spec,_,loc) as g ->
            if not (Cil.is_empty_funspec old_spec) then begin
              rename_spec g;
              Cil.CurrentLoc.set loc;
              Logic_utils.merge_funspec
                !currentFunctionFDEC.sspec old_spec;
              Logic_utils.clear_funspec old_spec;
            end
          | _ -> assert false
        in
        update_fundec_in_theFile !currentFunctionFDEC.svar merge_spec
      end;
      (********** Now do the BODY *************)
      let _ =
        let stmts =
          doBody
            { empty_local_env with
              known_behaviors =
                (List.map (fun x -> x.b_name)
                   !currentFunctionFDEC.sspec.spec_behavior)
                @ behaviors;
              is_ghost = local_env.is_ghost
            }
            body
        in
        (* Finish everything *)
        exitScope ();
        (* Now fill in the computed goto statement with cases. Do this
         * before mkFunctionbody which resolves the gotos *)
        (match !gotoTargetData with
         | Some (_switchv, switch) ->
           let switche, loc =
             match switch.skind with
             | Switch (switche, _, _, l) -> switche, l
             | _ ->
               Kernel.fatal ~current:true
                 "the computed goto statement not a switch"
           in
           (* Build a default chunk that segfaults *)
           let default =
             defaultChunk ~ghost
               loc
               (i2c (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                       (Set ((Mem (makeCast (integer ~loc 0) intPtrType),
                              NoOffset),
                             integer ~loc 0, loc)),[],[],[]))
           in
           let bodychunk = ref default in
           H.iter
             (fun lname laddr ->
                bodychunk :=
                  caseRangeChunk ~ghost
                    [integer ~loc laddr] loc
                    (gotoChunk ~ghost lname loc @@ (!bodychunk, ghost)))
             gotoTargetHash;
           (* Now recreate the switch *)
           let newswitch = switchChunk ~ghost switche !bodychunk loc in
           (* We must still share the old switch statement since we
            * have already inserted the goto's *)
           let newswitchkind =
             match newswitch.stmts with
             | [ s, _, _,_,_] when newswitch.cases == []-> s.skind
             | _ ->
               Kernel.fatal ~current:true
                 "Unexpected result from switchChunk"
           in
           switch.skind <- newswitchkind

         | None -> ());
        (* Now finish the body and store it *)
        let body = mkFunctionBody ~ghost stmts in
        !currentFunctionFDEC.sbody <- body;
        (* Reset the global parameters *)
        gotoTargetData := None;
        H.clear gotoTargetHash;
        gotoTargetNextAddr := 0;
      in
      !currentFunctionFDEC.slocals <- (List.rev !currentFunctionFDEC.slocals);
      setMaxId !currentFunctionFDEC;

      (* Now go over the types of the formals and pull out the formals
       * with transparent union type. Replace them with some shadow
       * parameters and then add assignments  *)
      let _ =
        let newformals, newbody =
          List.fold_right (* So that the formals come out in order *)
            (fun f (accform, accbody) ->
               match isTransparentUnion f.vtype with
               | None -> (f :: accform, accbody)
               | Some fstfield ->
                 (* A new shadow to be placed in the formals. Use
                  * makeTempVar to update smaxid and all others but
                    do not insert as a local variable of [f]. *)
                 let loc = CurrentLoc.get () in
                 let shadow =
                   makeTempVar
                     !currentFunctionFDEC ~insert:false
                     fstfield.ftype
                 in
                 (* Now replace it with the current formal. *)
                 (shadow :: accform,
                  mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                    (Set ((Var f, Field(fstfield, NoOffset)),
                          new_exp ~loc (Lval (var shadow)), loc))
                  :: accbody))
            !currentFunctionFDEC.sformals
            ([], !currentFunctionFDEC.sbody.bstmts)
        in
        !currentFunctionFDEC.sbody.bstmts <- newbody;
        (* To make sure sharing with the type is proper *)
        setFormals !currentFunctionFDEC newformals;
      in

      (* Now see whether we can fall through to the end of the function *)
      if blockFallsThrough !currentFunctionFDEC.sbody then begin
        let loc = endloc in
        let protect_return,retval =
          (* Guard the [return] instructions we add with an
             [\assert \false]*)
          let pfalse = Logic_const.unamed ~loc Pfalse in
          let pfalse = { pfalse with pred_name = ["missing_return"] } in
          let assert_false () =
            let annot =
              Logic_const.new_code_annotation (AAssert ([], pfalse))
            in
            Cil.mkStmt ~ghost ~valid_sid (Instr(Code_annot(annot,loc)))
          in
          match unrollType !currentReturnType with
          | TVoid _ -> [], None
          | (TInt _ | TEnum _ | TFloat _ | TPtr _) as rt ->
            let res = Some (makeCastT (zero ~loc) intType rt) in
            if !currentFunctionFDEC.svar.vname = "main" then
              [],res
            else begin
              Kernel.warning ~current:true
                "Body of function %s falls-through. \
                 Adding a return statement"
                !currentFunctionFDEC.svar.vname;
              [assert_false ()], res
            end
          | rt ->
            (* 0 is not an admissible value for the return type.
               On the other hand, *( T* )0 is. We're not supposed
               to get there anyway. *)
            let null_ptr = makeCastT (zero ~loc) intType (TPtr(rt,[])) in
            let res = Some (new_exp ~loc (Lval (mkMem null_ptr NoOffset))) in
            Kernel.warning ~current:true
              "Body of function %s falls-through. \
               Adding a return statement"
              !currentFunctionFDEC.svar.vname;
            [assert_false ()], res
        in
        if not (hasAttribute "noreturn" !currentFunctionFDEC.svar.vattr)
        then
          !currentFunctionFDEC.sbody.bstmts <-
            !currentFunctionFDEC.sbody.bstmts
            @ protect_return @
            [mkStmt ~ghost ~valid_sid (Return(retval, endloc))]
      end;

      (* ignore (E.log "The env after finishing the body of %s:\n%t\n"
         n docEnv); *)
      cabsPushGlobal (GFun (!currentFunctionFDEC, funloc));
      currentFunctionFDEC := dummy_function;
      empty
    end (* FUNDEF *)

  | LINKAGE (n, loc, dl) ->
    CurrentLoc.set (convLoc loc);
    if n <> "C" then
      Kernel.warning ~current:true
        "Encountered linkage specification \"%s\"" n;
    if not isglobal then
      Kernel.error ~once:true ~current:true
        "Encountered linkage specification in local scope";
    (* For now drop the linkage on the floor !!! *)
    List.iter
      (fun d ->
         let s = doDecl local_env isglobal d in
         if isNotEmpty s then
           abort_context "doDecl returns non-empty statement for global")
      dl;
    empty

  | A.GLOBANNOT (decl) when isglobal ->
    begin
      List.iter
        (fun decl  ->
           let loc = convLoc decl.Logic_ptree.decl_loc in
           CurrentLoc.set loc;
           try
             let tdecl = Ltyping.annot decl in
             let attr = fc_stdlib_attribute [] in
             let tdecl =
               List.fold_left
                 (Extlib.swap Logic_utils.add_attribute_glob_annot) tdecl attr
             in
             cabsPushGlobal (GAnnot(tdecl,CurrentLoc.get ()))
           with LogicTypeError ((source,_),msg) ->
             Kernel.warning
               ~wkey:Kernel.wkey_annot_error ~source
               "%s. Ignoring global annotation" msg)
        decl;
    end;
    empty

  | A.CUSTOM (custom, name, location) when isglobal ->
    begin
      let loc = convLoc location in
      CurrentLoc.set loc;
      try
        let tcustom = Ltyping.custom custom in
        let attr = fc_stdlib_attribute [] in
        cabsPushGlobal (GAnnot(Dcustom_annot(tcustom, name, attr,loc),loc))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning
          ~wkey:Kernel.wkey_annot_error ~source
          "%s. Ignoring custom annotation" msg
    end;
    empty
  | A.CUSTOM _ | A.GLOBANNOT _ | A.PRAGMA _ | A.GLOBASM _ | A.FUNDEF _ ->
    Kernel.fatal ~current:true "this form of declaration must be global"

and doTypedef ghost ((specs, nl): A.name_group) =
  (* Do the specifiers exactly once *)
  let bt, sto, inl, attrs = doSpecList ghost (suggestAnonName nl) specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let createTypedef ((n,ndt,a,_) : A.name) =
    (*    E.s (error "doTypeDef") *)
    let newTyp, tattr =
      doType ghost false AttrType bt (A.PARENTYPE(attrs, ndt, a))  in
    checkTypedefSize n newTyp;
    let tattr = fc_stdlib_attribute tattr in
    let newTyp' = cabsTypeAddAttributes tattr newTyp in
    checkRestrictQualifierDeep newTyp';
    if H.mem typedefs n && H.mem env n then
      (* check if type redefinition is allowed (C11 only);
         in all cases, do not create a new type.
         TODO: if local typedef redefinitions are to be supported, then the new type
         must be created if the definition is syntactically valid. *)
      begin
        if !scopes <> [] then
          Kernel.failure ~current:true
            "redefinition of a typedef in a non-global scope is currently unsupported";
        let typeinfo = H.find typedefs n in
        let _, oldloc = lookupType "type" n in
        if areCompatibleTypes typeinfo.ttype newTyp' then
          begin
            let error_conflicting_types () =
              Kernel.error ~current:true
                "redefinition of type '%s' in the same scope with conflicting type.@ \
                 Previous declaration was at %a"
                n Cil_datatype.Location.pretty oldloc
            in
            let error_c11_redefinition () =
              Kernel.error ~current:true
                "redefinition of type '%s' in the same scope is only allowed in C11 \
                 (option %s).@ Previous declaration was at %a" n Kernel.C11.name
                Cil_datatype.Location.pretty oldloc
            in
            (* Tested with GCC+Clang: redefinition of compatible types in same scope:
               - enums are NOT allowed;
               - composite types are allowed only if the composite type itself is
                 not redefined (complex rules; with some extra tag checking performed
                 in compatibleTypesp, we use tags here to detect redefinitions,
                 which are invalid)
               - redefinition via a typedef of a struct/union/enum IS allowed;
               - other types are allowed. *)
            if declared_in_current_scope n then
              begin
                match newTyp' with (* do NOT unroll type here,
                                      redefinitions of typedefs are ok *)
                | TComp (newci, _, _) ->
                  (* Composite types with different tags may be compatible, but here
                     we use the tags to try and detect if the type is being redefined,
                     which is NOT allowed. *)
                  begin
                    match unrollType typeinfo.ttype with
                    | TComp (ci, _, _) ->
                      if ci.cname <> newci.cname then
                        (* different tags => we consider that the type is being redefined *)
                        error_conflicting_types ()
                      else
                        (* redeclaration in same scope valid only in C11 *)
                      if not (Kernel.C11.get ()) then error_c11_redefinition ()
                    | _ -> (* because of the compatibility test, this should not happen *)
                      Kernel.fatal ~current:true "typeinfo.ttype (%a) should be TComp"
                        Cil_printer.pp_typ typeinfo.ttype
                  end
                | TEnum _ -> (* GCC/Clang: "conflicting types" *)
                  error_conflicting_types ()
                | _ -> (* redeclaration in same scope valid only in C11 *)
                  if not (Kernel.C11.get ()) then error_c11_redefinition ()
              end
          end
        else if declared_in_current_scope n then
          Kernel.error ~current:true
            "redefinition of type '%s' in the same scope with incompatible type.@ \
             Previous declaration was at %a" n Cil_datatype.Location.pretty oldloc;
      end
    else (* effectively create new type *) begin
    let n', _  = newAlphaName true "type" n in
    let ti =
      { torig_name = n; tname = n';
        ttype = newTyp'; treferenced = false }
    in
    (* Since we use the same name space, we might later hit a global with
     * the same name and we would want to change the name of the global.
     * It is better to change the name of the type instead. So, remember
     * all types whose names have changed *)
    H.add typedefs n' ti;
    let namedTyp = TNamed(ti, []) in
    (* Register the type. register it as local because we might be in a
     * local context  *)
    addLocalToEnv (kindPlusName "type" n) (EnvTyp namedTyp);
    cabsPushGlobal (GType (ti, CurrentLoc.get ()))
    end
  in
  List.iter createTypedef nl

and doOnlyTypedef ghost (specs: A.spec_elem list) : unit =
  let bt, sto, inl, attrs = doSpecList ghost "" specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let restyp, nattr =
    doType ghost false AttrType bt (A.PARENTYPE(attrs, A.JUSTBASE, []))
  in
  if nattr <> [] then
    Kernel.warning ~current:true "Ignoring identifier attribute";
  (* doSpec will register the type. *)
  (* See if we are defining a composite or enumeration type, and in that
   * case move the attributes from the defined type into the composite type
   * *)
  let isadef =
    List.exists
      (function
          A.SpecType(A.Tstruct(_, Some _, _)) -> true
        | A.SpecType(A.Tunion(_, Some _, _)) -> true
        | A.SpecType(A.Tenum(_, Some _, _)) -> true
        | _ -> false) specs
  in
  match restyp with
  | TComp(ci, _, al) ->
    if isadef then begin
      ci.cattr <- cabsAddAttributes ci.cattr al;
      (* The GCompTag was already added *)
    end else (* Add a GCompTagDecl *)
      cabsPushGlobal (GCompTagDecl(ci, CurrentLoc.get ()))
  | TEnum(ei, al) ->
    if isadef then begin
      ei.eattr <- cabsAddAttributes ei.eattr al;
    end else
      cabsPushGlobal (GEnumTagDecl(ei, CurrentLoc.get ()))
  | _ ->
    Kernel.warning ~current:true
      "Ignoring un-named typedef that does not introduce a struct or \
       enumeration type"

(* Now define the processors for body and statement *)
and doBody local_env (blk: A.block) : chunk =
  let ghost = local_env.is_ghost in
  (* Rename the labels and add them to the environment *)
  List.iter (fun l -> ignore (genNewLocalLabel l)) blk.blabels;
  (* See if we have some attributes *)
  let battrs = doAttributes ghost blk.A.battrs in

  let bodychunk =
    afterConversion ~ghost
      (snd
         (List.fold_left   (* !!! @ evaluates its arguments backwards *)
            (fun ((new_behaviors,keep_block),prev) s ->
               let local_env =
                 { local_env with
                   known_behaviors =
                     new_behaviors @ local_env.known_behaviors
                 }
               in
               (* Format.eprintf "Considering statement: %a@."
                  Cprint.print_statement s; *)
               let res = doStatement local_env s in
               (* Keeps stmts originating from the same source
                  statement in a single block when the statement
                  follows a code annotation, so that the annotation
                  will be attached to the whole result and
                  not to the first Cil statement. This is only needed
                  for statement contracts and pragmas. Other (non-loop, as
                  they have special treatment) annotations operate purely
                  at current point and do not care about what happens to the
                  next statement.
               *)
               let new_behaviors, keep_next =
                 match s.stmt_node with
                 | CODE_ANNOT(Logic_ptree.AStmtSpec (_,s),_)
                 | CODE_SPEC (s,_) ->
                   List.map
                     (fun x -> x.Logic_ptree.b_name)
                     s.Logic_ptree.spec_behavior,
                   true
                 | CODE_ANNOT(Logic_ptree.APragma _,_) -> [], true
                 | _ -> [], false
               in
               (*               Format.eprintf "Done statement %a@." d_chunk res; *)
               let chunk =
                 if keep_block then
                   append_chunk_to_annot ~ghost prev res
                 else prev @@ (res, ghost)
               in ((new_behaviors, keep_next), chunk))
            (([],false),empty)
            blk.A.bstmts))
  in
  if battrs == [] && bodychunk.locals == []
  then begin
    (* keep block marked with FRAMA_C_KEEP_BLOCK or that defines local
          variables as independent blocks whatever happens.
    *)
    bodychunk
  end
  else begin
    let b = c2block ~ghost bodychunk in
    b.battrs <- battrs;
    let res = s2c (mkStmt ~ghost ~valid_sid (Block b)) in
    { res with cases = bodychunk.cases }
  end

and doBodyScope local_env blk =
  enterScope (); let res = doBody local_env blk in exitScope (); res

and doStatement local_env (s : A.statement) : chunk =
  let mk_loop_annot a loc =
    try
      List.map
        (Ltyping.code_annot
           loc local_env.known_behaviors (Ctype !currentReturnType)) a
    with LogicTypeError ((source,_),msg) ->
      Kernel.warning
        ~wkey:Kernel.wkey_annot_error ~source
        "%s. Ignoring loop annotation" msg;
      []
  in
  let ghost = s.stmt_ghost in
  let local_env = { local_env with is_ghost = ghost } in
  match s.stmt_node with
  | A.NOP loc ->
    { empty
      with stmts = [mkEmptyStmt ~ghost ~valid_sid ~loc (), [],[],[],[]]}
  | A.COMPUTATION (e, loc) ->
    CurrentLoc.set (convLoc loc);
    let (lasts, data) = !gnu_body_result in
    if lasts == s then begin      (* This is the last in a GNU_BODY *)
      let (s', e', t') = doFullExp local_env false e (AExp None) in
      data := Some (e', t');      (* Record the result *)
      s'
    end else
      let (s', e', _) = doFullExp local_env false e ADrop in
      (* drop the side-effect free expression unless the whole computation
         is pure and it contains potential threats (i.e. dereference)
      *)
      if isEmpty s' && is_dangerous e'
      then
        s' @@ (keepPureExpr ~ghost e' loc, ghost)
      else
        begin
          if (isEmpty s') then begin
            let name = !currentFunctionFDEC.svar.vorig_name in
            IgnorePureExpHook.apply (name, e');
          end;
          s'
        end

  | A.BLOCK (b, loc,_) ->
    CurrentLoc.set (convLoc loc);
    let c = doBodyScope local_env b in
    let b = c2block ~ghost c in
    b.battrs <- addAttributes [Attr(frama_c_keep_block,[])] b.battrs;
    let res = s2c (mkStmt ~ghost ~valid_sid (Block b)) in
    { res with cases = c.cases }

  | A.SEQUENCE (s1, s2, _) ->
    let c1 = doStatement local_env s1 in
    let c2 = doStatement local_env s2 in
    c1 @@ (c2, ghost)

  | A.IF(e,st,sf,loc) ->
    let st' = doStatement local_env st in
    let sf' = doStatement local_env sf in
    CurrentLoc.set (convLoc loc);
    doCondition local_env false e st' sf'

  | A.WHILE(a,e,s,loc) ->
    startLoop true;
    let a = mk_loop_annot a loc in
    let s' = doStatement local_env s in
    let s' =
      if !doTransformWhile then
        s' @@ (consLabContinue ~ghost skipChunk, ghost)
      else s'
    in
    let loc' = convLoc loc in
    let break_cond = breakChunk ~ghost loc' in
    exitLoop ();
    CurrentLoc.set loc';
    loopChunk ~ghost a
      ((doCondition local_env false e skipChunk break_cond)
       @@ (s', ghost))

  | A.DOWHILE(a, e,s,loc) ->
    startLoop false;
    let a = mk_loop_annot a loc in
    let s' = doStatement local_env s in
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    (* No 'break' instruction can exit the chunk *)
    let no_break chunk =
      List.for_all (fun (s, _, _, _, _) -> not (stmtCanBreak s)) chunk.stmts
    in
    (* Check if we are translating 'do { <s> } while (0)'. If so, translate
       it into '<s>' instead. Only active when -simplify-trivial-loops is
       set (default), as it impact plugins that compare the shape of the
       Cabs and of the Cil files. *)
    if Kernel.SimplifyTrivialLoops.get() &&
       isCabsZeroExp e (* exp is 0 or something equivalent *) &&
       a = [] (* No loop annot *) &&
       not (continueUsed ()) (* no 'continue' inside s *) &&
       no_break s' (* no break that exists s *)
    then (
      exitLoop ();
      s'
    )
    else
      let s'' =
        consLabContinue ~ghost
          (doCondition
             local_env
             false e skipChunk (breakChunk ~ghost loc'))
      in
      exitLoop ();
      loopChunk ~ghost a (s' @@ (s'', ghost))

  | A.FOR(a,fc1,e2,e3,s,loc) -> begin
      let loc' = convLoc loc in
      CurrentLoc.set loc';
      enterScope (); (* Just in case we have a declaration *)
      ForLoopHook.apply (fc1,e2,e3,s);
      let (se1, _, _) , has_decl =
        match fc1 with
        | FC_EXP e1 -> doFullExp local_env false e1 ADrop, false
        | FC_DECL d1 ->
          (doDecl local_env false d1, zero ~loc, voidType), true
      in
      let (se3, _, _) = doFullExp local_env false e3 ADrop in
      startLoop false;
      let a = mk_loop_annot a loc in
      let s' = doStatement local_env s in
      (*Kernel.debug "Loop body : %a" d_chunk s';*)
      CurrentLoc.set loc';
      let s'' = consLabContinue ~ghost se3 in
      let break_cond = breakChunk ~ghost loc' in
      exitLoop ();
      let res =
        match e2.expr_node with
        | A.NOTHING -> (* This means true *)
          se1 @@ (loopChunk ~ghost a (s' @@ (s'', ghost)), ghost)
        | _ ->
          se1 @@
          (loopChunk ~ghost a
             (((doCondition
                  local_env false e2 skipChunk break_cond)
               @@ (s', ghost)) @@ (s'', ghost)), ghost)
      in
      exitScope ();
      if has_decl then begin
        let chunk = s2c (mkStmt ~ghost ~valid_sid (Block (c2block ~ghost res)))
        in
        { chunk with cases = res.cases }
      end else res
    end

  | A.BREAK loc ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    breakChunk ~ghost loc'

  | A.CONTINUE loc ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    continueOrLabelChunk ~ghost loc'

  | A.RETURN ({ expr_node = A.NOTHING}, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    if not (isVoidType !currentReturnType) then
      Kernel.error ~current:true
        "Return statement without a value in function returning %a\n"
        Cil_printer.pp_typ !currentReturnType;
    returnChunk ~ghost None loc'

  | A.RETURN (e, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    (* Sometimes we return the result of a void function call *)
    if isVoidType !currentReturnType then begin
      Kernel.error ~current:true
        "Return statement with a value in function returning void";
      let (se, _, _) = doFullExp local_env false e ADrop in
      se @@ (returnChunk ~ghost None loc', ghost)
    end else begin
      let rt =
        typeRemoveAttributes ["warn_unused_result"] !currentReturnType
      in
      let (se, e', et) =
        doFullExp local_env false e (AExp (Some rt)) in
      let (_, e'') = castTo et rt e' in
      se @@ (returnChunk ~ghost (Some e'') loc', ghost)
    end

  | A.SWITCH (e, s, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    let (se, e', et) = doFullExp local_env false e (AExp None) in
    if not (Cil.isIntegralType et) then
      Kernel.error ~once:true ~current:true "Switch on a non-integer expression.";
    let et' = Cil.integralPromotion et in
    let e' = makeCastT ~e:e' ~oldt:et ~newt:et' in
    enter_break_env ();
    let s' = doStatement local_env s in
    exit_break_env ();
    se @@ (switchChunk ~ghost e' s' loc', ghost)

  | A.CASE (e, s, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    let (se, e', _) = doFullExp local_env true e (AExp None) in
    if isNotEmpty se || not (Cil.isIntegerConstant e') then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let chunk =
      caseRangeChunk ~ghost
        [if theMachine.lowerConstants then constFold false e' else e']
        loc' (doStatement local_env s)
    in
    (* se has no statement, but can contain local variables, in
       particular in the case of a sizeof with side-effects. *)
    se @@ (chunk,ghost)

  | A.CASERANGE (el, eh, s, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc;
    let (sel, el', _) = doFullExp local_env false el (AExp None) in
    let (seh, eh', _) = doFullExp local_env false eh (AExp None) in
    if isNotEmpty sel || isNotEmpty seh then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let il, ih =
      match constFoldToInt el', constFoldToInt eh' with
      | Some il, Some ih -> Integer.to_int il, Integer.to_int ih
      | _ ->
        Kernel.fatal ~current:true
          "Cannot understand the constants in case range"
    in
    if il > ih then Kernel.error ~once:true ~current:true "Empty case range";
    let rec mkAll (i: int) =
      if i > ih then [] else integer ~loc i :: mkAll (i + 1)
    in
    (sel @@ (seh,ghost)) @@ 
    (caseRangeChunk ~ghost (mkAll il) loc' (doStatement local_env s),
     ghost)

  | A.DEFAULT (s, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    defaultChunk ~ghost loc' (doStatement local_env s)
  | A.LABEL (l, s, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    C_logic_env.add_current_label l;
    (* Lookup the label because it might have been locally defined *)
    let chunk =
      consLabel ~ghost (lookupLabel l) (doStatement local_env s) loc' true
    in
    C_logic_env.reset_current_label (); chunk

  | A.GOTO (l, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    (* Maybe we need to rename this label *)
    gotoChunk ~ghost (lookupLabel l) loc'

  | A.COMPGOTO (e, loc) -> begin
      let loc' = convLoc loc in
      CurrentLoc.set loc';
      (* Do the expression *)
      let se, e', _ =
        doFullExp local_env false e (AExp (Some voidPtrType)) in
      match !gotoTargetData with
      | Some (switchv, switch) -> (* We have already generated this one  *)
        (se
         @@ (i2c(mkStmtOneInstr ~ghost ~valid_sid
                   (Set (var switchv, makeCast e' intType, loc')),
                 [],[],[]), ghost))
        @@ (s2c(mkStmt ~ghost ~valid_sid (Goto (ref switch, loc'))), ghost)

      | None -> begin
          (* Make a temporary variable *)
          let vchunk = createLocal
              local_env.is_ghost
              (intType, NoStorage, false, [])
              (("__compgoto", A.JUSTBASE, [], loc), A.NO_INIT)
          in
          if not (isEmpty vchunk) then
            Kernel.fatal ~current:true
              "Non-empty chunk in creating temporary for goto *";
          let switchv, _ =
            try lookupVar "__compgoto"
            with Not_found -> abort_context "Cannot find temporary for goto *";
          in
          (* Make a switch statement. We'll fill in the statements at the
           * end of the function *)
          let switch =
            mkStmt ~ghost ~valid_sid
              (Switch (new_exp ~loc (Lval(var switchv)),
                       mkBlock [], [], loc'))
          in
          (* And make a label for it since we'll goto it *)
          switch.labels <- [Label ("__docompgoto", loc', false)];
          gotoTargetData := Some (switchv, switch);
          (se @@
           (i2c
              (mkStmtOneInstr ~ghost ~valid_sid
                 (Set (var switchv, makeCast e' intType, loc')),[],[],[]),
            ghost))
          @@ (s2c switch, ghost)
        end
    end

  | A.DEFINITION d ->
    doDecl local_env false d

  | A.ASM (asmattr, tmpls, details, loc) ->
    (* Make sure all the outs are variables *)
    let loc' = convLoc loc in
    let attr' = doAttributes local_env.is_ghost asmattr in
    CurrentLoc.set loc';
    let stmts : chunk ref = ref empty in
    let (tmpls', ext_asm) =
      match details with
      | None ->
        let tmpls' =
          if Cil.msvcMode () then tmpls
          else
            let pattern = Str.regexp "%" in
            let escape = Str.global_replace pattern "%%" in
            List.map escape tmpls
        in
        (tmpls', None)
      | Some { aoutputs; ainputs; aclobbers; alabels} ->
        let asm_outputs =
          List.map
            (fun (id, c, e) ->
               let (se, e', _) =
                 doFullExp local_env false e (AExp None)
               in
               let lv =
                 match e'.enode with
                 | Lval lval
                 | StartOf lval -> lval
                 | _ ->
                   Kernel.fatal ~current:true
                     "Expected lval for ASM outputs"
               in
               if not (isEmpty se) then
                 stmts := !stmts @@ (se, ghost);
               (id, c, lv)) aoutputs
        in
        (* Get the side-effects out of expressions *)
        let asm_inputs =
          List.map
            (fun (id, c, e) ->
               let (r, se, e', _) =
                 doExp (no_paren_local_env local_env) false e (AExp None)
               in
               let se = add_reads ~ghost e'.eloc r se in
               if not (isEmpty se) then
                 stmts := !stmts @@ (se, ghost);
               (id, c, e'))
            ainputs
        in
        let asm_clobbers = aclobbers in
        let asm_gotos =
          List.map 
            (fun label -> 
               let label = lookupLabel label in
               let gref = ref dummyStmt in
               addGoto label gref;
               gref) 
            alabels
        in
        (tmpls, Some { asm_outputs; asm_inputs; asm_clobbers; asm_gotos })
    in
    !stmts @@
    (i2c(mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
           (Asm(attr', tmpls', ext_asm, loc')),[],[],[]),
     ghost)
  | THROW (e,loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    (match e with
     | None -> s2c (mkStmt ~ghost ~valid_sid (Throw (None,loc')))
     | Some e ->
       let se,e,t = doFullExp local_env false e (AExp None) in
       se @@
       (s2c (mkStmt ~ghost ~valid_sid (Throw (Some (e,t),loc'))),ghost))
  | TRY_CATCH(stry,l,loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    let chunk_try = doStatement local_env stry in
    let type_one_catch (var,scatch) =
      enterScope();
      let vi =
        match var with
        | None -> Catch_all
        | Some (t,(n,ndt,a,ldecl)) ->
          let spec = doSpecList ghost n t in
          let vi =
            makeVarInfoCabs
              ~ghost ~isformal:false ~isglobal:false ldecl spec (n,ndt,a)
          in
          addLocalToEnv n (EnvVar vi);
          !currentFunctionFDEC.slocals <- vi :: !currentFunctionFDEC.slocals;
          Catch_exn(vi,[])
      in
      let chunk_catch = doStatement local_env scatch in
      exitScope();
      (vi,c2block ~ghost chunk_catch)
    in
    let catches = List.map type_one_catch l in
    s2c
      (mkStmt
         ~ghost ~valid_sid (TryCatch(c2block ~ghost chunk_try,catches,loc')))
  | TRY_FINALLY (b, h, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    let b': chunk = doBodyScope local_env b in
    let h': chunk = doBodyScope local_env h in
    if b'.cases <> [] || h'.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Try statements cannot contain switch cases";
    s2c (mkStmt ~ghost ~valid_sid
           (TryFinally (c2block ~ghost b', c2block ~ghost h', loc')))

  | TRY_EXCEPT (b, e, h, loc) ->
    let loc' = convLoc loc in
    CurrentLoc.set loc';
    let b': chunk = doBodyScope local_env b in
    (* Now do e *)
    let ((se: chunk), e', _) =
      doFullExp local_env false e (AExp None) in
    let h': chunk = doBodyScope local_env h in
    if b'.cases <> [] || h'.cases <> [] || se.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Try statements cannot contain switch cases";
    (* Now take se and try to convert it to a list of instructions. This
     * might not be always possible *)
    let stmt_to_instrs s =
      List.rev_map
        (function (s,_,_,_,_) -> match s.skind with
           | Instr s -> s
           | _ ->
             Kernel.fatal ~current:true
               "Except expression contains unexpected statement")
        s
    in
    let il' = stmt_to_instrs se.stmts in
    s2c (mkStmt ~ghost ~valid_sid
           (TryExcept
              (c2block ~ghost b',(il', e'), c2block ~ghost h', loc')))
  | CODE_ANNOT (a, loc) ->
    let loc' = convLoc loc in
    begin
      try
        let typed_annot =
          Ltyping.code_annot
            loc' local_env.known_behaviors (Ctype !currentReturnType) a
        in
        s2c (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (typed_annot,loc')))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning
          ~wkey:Kernel.wkey_annot_error ~source
          "%s. Ignoring code annotation" msg;
        BlockChunk.empty
    end

  | CODE_SPEC (a, loc) ->
    let loc' = convLoc loc in
    begin
      try
        let spec =
          Ltyping.code_annot loc' local_env.known_behaviors
            (Ctype !currentReturnType) (Logic_ptree.AStmtSpec ([],a))
        in
        s2c (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (spec,loc')))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning
          ~wkey:Kernel.wkey_annot_error ~source
          "%s. Ignoring code annotation" msg;
        BlockChunk.empty
    end

let copy_spec (old_f,new_f) formals_map spec =
  let obj = object
    inherit Cil.genericCilVisitor (Cil.refresh_visit (Project.current()))
    method! vlogic_var_use lv =
      match lv.lv_origin with
      | None -> DoChildren
      | Some v ->
        if Cil_datatype.Varinfo.equal v old_f then
          ChangeTo (Cil.cvar_to_lvar new_f)
        else begin
          try
            let _,new_v =
              List.find
                (fun (x,_) -> Cil_datatype.Varinfo.equal v x) formals_map
            in
            ChangeTo (Cil.cvar_to_lvar new_v)
          with Not_found -> DoChildren
        end
  end
  in
  Cil.visitCilFunspec obj spec

let split_extern_inline_def acc g =
  match g with
  | GFun ( { svar; sformals; sspec }, loc)
    when svar.vinline && svar.vstorage = NoStorage ->
    (* we have an inline definition, which is also an implicit external
       _declaration_ (see C11 6.7.4§7). Just rename its uses in the current
       translation unit, and leave a new, unrelated, external declaration for
       the link phase. If a spec exists, the external declaration will inherit
       it.
    *)
    let new_v = Cil_const.copy_with_new_vid svar in
    svar.vname <- svar.vname ^ "__fc_inline";
    (* inline definition is restricted to this translation unit. *)
    svar.vstorage <- Static;
    let new_formals = List.map Cil_const.copy_with_new_vid sformals in
    Cil.unsafeSetFormalsDecl new_v new_formals;
    let formals_map = List.combine sformals new_formals in
    let new_spec = copy_spec (svar, new_v) formals_map sspec in
    GFunDecl (new_spec, new_v, loc) :: g :: acc
  | GFun ({ svar },_) when svar.vinline && svar.vstorage = Extern ->
    (* The definition is a real external definition. We may as well remove
       the inline specification. *)
    svar.vinline <- false;
    g :: acc
  | _ -> g::acc

(* Translate a file *)
let convFile (fname, f) =
  Errorloc.clear_errors();
  (* Clean up the global types *)
  initGlobals();
  startFile ();
  IH.clear noProtoFunctions;
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear staticLocals;
  H.clear typedefs;
  H.clear alpha_renaming;
  Stack.clear packing_pragma_stack;
  current_packing_pragma := None;
  H.clear pragma_align_by_struct;
  current_pragma_align := None;
  Logic_env.prepare_tables ();
  anonCompFieldNameId := 0;
  Kernel.debug ~level:2 "Converting CABS->CIL" ;
  Cil.Builtin_functions.iter_sorted
    (fun name (resTyp, argTypes, isva) ->
       ignore (setupBuiltin name (resTyp, ArgTypes argTypes, isva)));
  let globalidx = ref 0 in
  let doOneGlobal (ghost,(d: A.definition)) =
    let local_env = ghost_local_env ghost in
    let s = doDecl local_env true d in
    if isNotEmpty s then
      abort_context "doDecl returns non-empty statement for global";
  in
  List.iter doOneGlobal f;
  let globals = fileGlobals () in
  let globals = List.fold_left split_extern_inline_def [] globals in
  let globals = List.rev globals in
  List.iter rename_spec globals;
  Logic_env.prepare_tables ();
  IH.clear noProtoFunctions;
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  H.clear staticLocals;
  H.clear typedefs;
  H.clear env;
  H.clear genv;
  IH.clear callTempVars;
  H.clear alpha_renaming;
  constrExprId := 0;

  if false then Kernel.debug "Cabs2cil converted %d globals" !globalidx;
  (* We are done *)
  { fileName = fname;
    globals;
    globinit = None;
    globinitcalled = false;
  }

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
