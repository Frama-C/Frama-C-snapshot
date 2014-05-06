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

(* Type check and elaborate ABS to CIL *)

(* The references to ISO means ANSI/ISO 9899-1999 *)
module A = Cabs
module C = Cabshelper
module V = Cabsvisit
module H = Hashtbl
module IH = Datatype.Int.Hashtbl
module AL = Alpha

open Pretty_utils
open Cabs
open Cabshelper
open Cil
open Cil_types
open Cil_datatype
open Lexing


let category_global = Kernel.register_category "cabs2cil:createGlobal"
let category_initializer = Kernel.register_category "cabs2cil:initializers"
let category_chunk = Kernel.register_category "cabs2cil:chunk"
let category_cast = Kernel.register_category "cabs2cil:cast"

let continueOnError = false

let frama_c_keep_block = "FRAMA_C_KEEP_BLOCK"
let () = Cil_printer.register_shallow_attribute frama_c_keep_block

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

let rec is_dangerous_offset t = function
    NoOffset -> false
  | Field (_,o) as off ->
    let t_offset = Cil.unrollType (Cil.typeOffset t off) in
    Cil.hasAttribute "volatile" (Cil.typeAttr t_offset) ||
    is_dangerous_offset t_offset o
  | Index _ -> true

let rec is_dangerous e = match e.enode with
    | Lval lv | AddrOf lv | StartOf lv -> is_dangerous_lval lv
    | UnOp (_,e,_) | CastE(_,e) | Info(e,_) -> is_dangerous e
    | BinOp(_,e1,e2,_) -> is_dangerous e1 || is_dangerous e2
    | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
      false
and is_dangerous_lval = function
  | Var v,_ when 
      (not v.vglob && not v.vformal && not v.vgenerated) 
      || Cil.hasAttribute "volatile" v.vattr
      || Cil.hasAttribute "volatile" (Cil.typeAttr (Cil.unrollType v.vtype))
      -> true
  (* Local might be uninitialized, which will trigger UB,
     but we assume that the variables we generate are correctly initialized.
   *)
  | Var v, o -> is_dangerous_offset (Cil.unrollType v.vtype) o
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

(* ICC align/noalign pragmas (not supported by GCC/MSVC with this syntax).
   Implemented by translating them to 'aligned' attributes. Currently,
   only default and noalign are supported, not explicit alignment values.
   Cf. www.slac.stanford.edu/grp/cd/soft/rmx/manuals/IC_386.PDF *)
let current_pragma_align = ref (None : bool option)
let pragma_align_by_struct = H.create 17

let process_align_pragma name args =
  let aux pname v =
    (if theMachine.msvcMode
     then Kernel.warning else Kernel.debug ~level:1 ?dkey:None)
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

(* The syntax and semantics for the pack pragmas are GCC's.
   The MSVC ones seems quite different and specific code should
   be written so support it. *)

(* The pack pragma stack *)
let packing_pragma_stack = Stack.create ()

(* The current pack pragma *)
let current_packing_pragma = ref None
let process_pack_pragma name args =
  begin match name with
  | "pack" -> begin
    if theMachine.msvcMode then
      Kernel.warning ~current:true
        "'pack' pragmas are probably incorrect in MSVC mode. \
          Using GCC like pragmas.";
    match args with
    | [] (*  #pragma pack() *) ->
      current_packing_pragma := None; None
    | [AInt n] (* #pragma pack(n) *) ->
      current_packing_pragma := Some n; None
    | [ACons ("push",[])] (* #pragma pack(push) *) ->
      Stack.push !current_packing_pragma packing_pragma_stack; None
    | [ACons ("push",[AInt n])] (* #pragma pack(push,n) *) ->
      Stack.push !current_packing_pragma packing_pragma_stack;
      current_packing_pragma:= Some n; None
    | [ACons ("pop",[])] (* #pragma pack(pop) *) ->
      begin try
        current_packing_pragma := Stack.pop packing_pragma_stack;
        None
      with Stack.Empty ->
        Kernel.warning ~current:true
          "Inconsistent #pragma pack(pop). Using default packing.";
        current_packing_pragma := None; None
      end
    | [ACons ("show",[])] (* #pragma pack(show) *) ->
      Some (Attr (name, args))
    | _ ->
      Kernel.warning ~current:true
        "Unsupported packing pragma not honored by Frama-C.";
      Some (Attr (name, args))
    end
  | _ -> Some (Attr (name, args))
  end

let force_packed_attribute a =
  if hasAttribute "packed" a then a
  else addAttribute (Attr("packed",[])) a

let add_packing_attributes s a =
  match !current_packing_pragma, align_pragma_for_struct s.corig_name with
    | None, None -> a
    | Some n, _ -> (* ignore 'align' pragma if some 'pack' pragmas are present
                      (no known compiler support both syntaxes) *)
        let with_aligned_attributes =
          match filterAttributes "aligned" a with
            | [] (* no aligned attributes yet. Add the global one. *) ->
                addAttribute (Attr("aligned",[AInt n])) a
            | [Attr("aligned",[AInt local])]
                (* The largest aligned wins with GCC. Don't know
                   with other compilers. *) ->
                addAttribute (Attr("aligned",[AInt (Integer.max local n)]))
                  (dropAttribute "aligned" a)
            | [Attr("aligned",[])] -> (* This one always wins as it is the
                                         biggest available on the plateform. *)
                a
            | _ -> Kernel.warning ~current:true
                "Unknown aligned attribute syntax: keeping it as is and \
                  adding new one.";
                addAttribute (Attr("aligned",[AInt n])) a
        in
        force_packed_attribute with_aligned_attributes

    | None, Some true ->
        dropAttribute "aligned" a
    | None, Some false ->
        force_packed_attribute
          (addAttribute 
	     (Attr("aligned",[AInt Integer.one])) 
	     (dropAttribute "aligned" a))


(***** COMPUTED GOTO ************)

(* The address of labels are small integers (starting from 0). A computed
 * goto is replaced with a switch on the address of the label. We generate
 * only one such switch and we'll jump to it from all computed gotos. To
 * accomplish this we'll add a local variable to store the target of the
 * goto. *)

(* The local variable in which to put the detination of the goto and the
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
    if hasAttribute "transparent_union" (typeAttrs t) then begin
      match comp.cfields with
      | [] ->
        Kernel.abort ~current:true
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
  if theMachine.msvcMode then n = "va_alist"
  else n = "__builtin_va_alist"

let isOldStyleVarArgTypeName n =
  if theMachine.msvcMode then n = "va_list"  || n = "__ccured_va_list"
  else n = "__builtin_va_alist_t"

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
    | (GVarDecl _ as g) :: _ -> f g
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
    | GVarDecl (spec,_,_)  -> behaviors spec @ acc
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

let required_builtins = [ "Frama_C_bzero"; "Frama_C_copy_block" ]

let cabsPushGlobal (g: global) =
  (match g with
    | GFun({ svar = v},_) | GVarDecl(_,v,_)
        when List.mem v.vname required_builtins ->
      ignore (Cil.Frama_c_builtins.memo (fun _ -> v) v.vname)
    | _ -> ());
  pushGlobal g ~types:theFileTypes ~variables:theFile;
  (match g with
     | GVar (vi, _, _) | GVarDecl (_, vi, _) | GFun ({svar = vi}, _) ->
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


(* Typedefs. We chose their names to be distinct from any global encounterd
 * at the time. But we might see a global with conflicting name later in the
 * file *)
let typedefs: (string, typeinfo) H.t = H.create 13

let fileGlobals () =
  let rec revonto (tail: global list) = function
      [] -> tail

    | GVarDecl (_,vi, l) :: rest
      when vi.vstorage != Extern && IH.mem mustTurnIntoDef vi.vid ->
        IH.remove mustTurnIntoDef vi.vid;
        revonto (GVar (vi, {init = None}, l) :: tail) rest

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
  | UndoResetAlphaCounter of location AL.alphaTableData ref *
                             location AL.alphaTableData
  | UndoRemoveFromAlphaTable of string

let scopes :  undoScope list ref list ref = ref []

(* When you add to env, you also add it to the current scope *)
let addLocalToEnv (n: string) (d: envdata) =
  (*log "%a: adding local %s to env\n" d_loc !currentLoc n; *)
  H.add env n (d, CurrentLoc.get ());
    (* If we are in a scope, then it means we are not at top level. Add the
     * name to the scope *)
  (match !scopes with
    [] -> begin
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
let alphaTable : (string, location AL.alphaTableData ref) H.t = H.create 307
        (* vars and enum tags. For composite types we have names like "struct
         * foo" or "union bar" *)

let fresh_global lookupname =
  fst (AL.newAlphaName alphaTable lookupname (CurrentLoc.get ()))

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
      [] -> (* At global scope *)()
    | [s] -> begin
        let prefix = AL.getAlphaPrefix lookupname in
        try
          let countref = H.find alphaTable prefix in
          s := (UndoResetAlphaCounter (countref, !countref)) :: !s
        with Not_found ->
          s := (UndoRemoveFromAlphaTable prefix) :: !s
    end
    | _ :: rest -> findEnclosingFun rest
  in
  if not globalscope then findEnclosingFun !scopes;
  let newname, oldloc =
    AL.newAlphaName alphaTable lookupname (CurrentLoc.get ())
  in
  if newname <> lookupname then begin
    try
      let info =
        if !scopes = [] then begin
          fst (H.find genv lookupname)
        end else
          if List.exists (find_identifier_decl lookupname) !(List.hd !scopes)
          then fst (H.find env lookupname)
          else raise Not_found
      in
      Kernel.error ~current:true 
        "redefinition of '%s'%s in the same scope. \
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
    (EnvVar vi), loc -> vi, loc
  | _ -> raise Not_found


let lookupGlobalVar (n: string) : varinfo * location =
  match H.find genv n with
    (EnvVar vi), loc -> vi, loc
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
            Kernel.abort ~current:true
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
  (* Store all locals in the slocals (in reversed order). We'll reverse them
   * and take out the formals at the end of the function *)
  if not vi.vglob then
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
      TArray(bt, Some len, _, a) ->
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
  let name,_ = AL.newAlphaName ~alphaTable ~undolist ~lookupname:"tmp" ~data in
  let undolist = !undolist in
  AL.undoAlphaChanges ~alphaTable ~undolist;
  name

(* Create a new temporary variable *)
let newTempVar descr (descrpure:bool) typ =
  (* physical equality used on purpose here *)
  if !currentFunctionFDEC == dummy_function then
    Kernel.fatal ~current:true "newTempVar called outside a function" ;
  (*  ignore (E.log "stripConstLocalType(%a) for temporary\n" d_type typ); *)
  let t' = (!typeForInsertedVar) (Cil.stripConstLocalType typ) in
  let name = get_temp_name () in 
  let vi = makeVarinfo false false name t' in
  vi.vdescr <- Some descr;
  vi.vdescrpure <- descrpure;
  (* Rename if clash, but do not add to the environment *)
  let vi = alphaConvertVarAndAddToEnv false vi in
  (*
  (* the temporary is local to the function: the normalization can use it
     wherever it wants.
   *)
  !currentFunctionFDEC.sbody.blocals <-
    vi :: !currentFunctionFDEC.sbody.blocals;
   *)
  vi

let mkAddrOfAndMark loc ((b, off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  begin match lastOffset off with
  | NoOffset ->
      (match b with
	Var vi ->
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
    Var vi -> vi.vaddrof <- true
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
    EnvTyp t, l -> t, l
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
    let res = mkCompInfo iss n ~norig (fun _ -> []) [] in
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
    let enum = { eorig_name = norig; ename = n; eitems = [];
                 eattr = []; ereferenced = false; ekind = IInt ; }
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


(* A simple visitor that searchs a statement for labels *)
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

(* Specify whether the cast is from the source code *)
let rec castTo ?(fromsource=false)
                (ot : typ) (nt : typ) (e : exp) : (typ * exp ) =
  Kernel.debug ~dkey:category_cast "@[%t: castTo:%s %a->%a@\n@]"
    Cil.pp_thisloc (if fromsource then "(source)" else "")
    Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
  let ot' = unrollType ot in
  let nt' = unrollType nt in
  if not fromsource && not (need_cast ot' nt') then begin
    (* Do not put the cast if it is not necessary, unless it is from the
     * source. *)
    Kernel.debug ~dkey:category_cast "no cast to perform";
    (ot, e)
  end else begin
    let nt' = if fromsource then nt' else !typeForInsertedCast e ot' nt' in
    let result = (nt', if theMachine.insertImplicitCasts || fromsource then
                    Cil.mkCastT ~force:true ~e ~oldt:ot ~newt:nt' else e)
    in
    let error s =
      (if fromsource then Kernel.abort else Kernel.fatal) ~current:true s
    in
(*  [BM] uncomment the following line to enable attributes static typing
    ignore (check_strict_attributes true ot nt  && check_strict_attributes false nt ot);*)
    Kernel.debug ~dkey:category_cast 
      "@[castTo: ot=%a nt=%a\n  result is %a@\n@]"
      Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
      Cil_printer.pp_exp (snd result);
    (* Now see if we can have a cast here *)
    match ot', nt' with
      TNamed _, _
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
    | TPtr (_, _), TPtr(_, _) -> result

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
      Kernel.debug ~dkey:category_cast
        "Casting %a to __builtin_va_list" Cil_printer.pp_typ ot ;
      result

    | TPtr _, TEnum _ ->
      Kernel.debug ~dkey:category_cast
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

(* Given an expression that is being coerced to bool,
   is it a nonzero constant? *)
let rec isConstTrue (e:exp): bool =
  match e.enode with
  | Const(CInt64 (n,_,_)) -> not (Integer.equal n Integer.zero)
  | Const(CChr c) -> 0 <> Char.code c
  | Const(CStr _ | CWStr _) -> true
  | Const(CReal(f, _, _)) -> f <> 0.0;
  | CastE(_, e) -> isConstTrue e
  | _ -> false

(* Given an expression that is being coerced to bool, is it zero?
   This is a more general version of Cil.isZero, which only handles integers.
   On constant expressions, either isConstTrue or isConstFalse will hold. *)
let rec isConstFalse (e:exp): bool =
  match e.enode with
  | Const(CInt64 (n,_,_)) -> Integer.equal n Integer.zero
  | Const(CChr c) -> 0 = Char.code c
  | Const(CReal(f, _, _)) -> f = 0.0;
  | CastE(_, e) -> isConstFalse e
  | _ -> false

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
      Format.fprintf fmt "@[<v 0>@[%a%a@]@;@[<v 2>{%a@]}@]"
        (fun fmt b -> if b then Format.fprintf fmt "/* UNDEFINED ORDER */@\n")
        c.unspecified_order
        (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) c.locals
        (Pretty_utils.pp_list ~sep:";@\n" d_stmt_chunk)
        (List.rev c.stmts)

    let empty =
      { stmts = []; cases = []; locals = [];
        unspecified_order = false; }

    let empty_stmts l =
      let rec is_empty_stmt s =
        match s.skind with
            Instr (Skip _) -> s.labels = []
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
          Instr(Call _) -> [ref i]
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
          let my_ref = ref (mkEmptyStmt ~loc:(cabslu "_find_label") ()) in
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
     *)
    let c2block ~ghost ?(collapse_block=true) (c: chunk) : block =
      if c.unspecified_order then
        { battrs = [];
          blocals = c.locals;
          bstmts =
            [mkStmt ~ghost (UnspecifiedSequence (List.rev c.stmts))]; }
      else
        match c.stmts with
          | [{ skind = Block b } as s,_,_,_,_] when
                collapse_block && s.labels = [] ->
              b.blocals <- c.locals @ b.blocals;
              b
          | stmts ->
              (* block has no locals by itself. We must add them now *)
            { blocals = c.locals;
              battrs = [];
              bstmts = List.rev_map (fun (s,_,_,_,_) -> s) stmts;
            }

    (* converts a chunk into a statement. *)
    let c2stmt ~ghost c =
      let kind =
        if c.unspecified_order then
          let kind = UnspecifiedSequence (List.rev c.stmts) in
          if c.locals <> [] then
            Block { battrs = []; blocals = c.locals; 
                    bstmts = [mkStmt ~ghost kind] }
          else kind
        else
          let block = c2block ~ghost c in Block block
      in
      mkStmt ~ghost kind

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

    let c2stmt_effect ~ghost c =
      let modified, writes, reads, calls = get_chunk_effects c
      in (c2stmt ~ghost c, modified, writes, reads, calls)

    let unspecified_chunk c = (* c *)
      (* to restore previous behavior (where unspecified evaluation order
         was not explicitly marked), comment out the line below and make
         unspecified_chunk the identity function.
       *)
      { c with unspecified_order = true }

    let local_var_chunk c v = { c with locals = v::c.locals }

    (* Add a statement at the end. Never refer to this statement again
     * after you call this *)
    let (+++) (c: chunk) (i,m,w,r) =
      let call = match i.skind with
          Instr (Call _) -> [ref i]
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
          unspecified_order = c1.unspecified_order;
        }
      else
        match c2.stmts with
            [] ->
              (match c2.locals with
                | [] -> c1
                | l -> { c1 with locals = c1.locals @ l })
          | [{skind = UnspecifiedSequence l},_,_,_,_]
            when c1.unspecified_order ->
            { stmts = List.rev_append l c1.stmts;
              cases = c1.cases @ c2.cases;
              locals = c1.locals @ c2.locals;
              unspecified_order = c1.unspecified_order; }
          | [s] ->
              { stmts = s :: c1.stmts;
                cases = c1.cases @ c2.cases;
                locals = c1.locals @ c2.locals;
                unspecified_order = c1.unspecified_order;
              }
          | _ -> let locals = c1.locals @ c2.locals in
            (* the lifespan of the locals is the whole chunk,
               not just c2, which may be transformed artificially
               in a block at this point.
             *)
            let c2 = { c2 with locals = [] } in
            { stmts = c2stmt_effect ~ghost c2 :: c1.stmts;
              cases = c1.cases @ c2.cases;
              locals = locals;
              unspecified_order = c1.unspecified_order;
            }
      in
      Kernel.debug ~dkey:category_chunk
        "Concat:@\n%a@\nWITH@\n%a@\nLEADS TO@\n%a@."
        d_chunk c1 d_chunk c2 d_chunk r;
      r

    let remove_reads lv c =
      Kernel.debug ~dkey:category_chunk
        "Removing %a from chunk@\n%a@."
        Cil_printer.pp_lval lv d_chunk c;
      let remove_list =
        List.filter (fun x -> not (Cil.compareLval lv x))
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
      { stmts = [ mkStmt ~ghost (Return(e, l)),[],[],[],[] ];
        cases = [];
        locals = [];
        unspecified_order = false;
      }

    let ifChunk ~ghost be (l: location) (t: chunk) (e: chunk) : chunk =
      let effects_t = get_chunk_effects t in
      let effects_e = get_chunk_effects e in
      let (m,r,w,c) = merge_effects effects_t effects_e in
      let stmt =
        mkStmt ~ghost (If(be, c2block ~ghost t, c2block ~ghost e, l))
      in
      { stmts = [ stmt ,m,r,w,c ];
        cases = t.cases @ e.cases;
        locals = [];
        unspecified_order = false;
      }

    let keepPureExpr ~ghost e l = ifChunk ~ghost e l skipChunk skipChunk

        (* We can duplicate a chunk if it has a few simple statements, and if
         * it does not have cases *)
    let duplicateChunk (c: chunk) = (* raises Failure if you should not
                                     * duplicate this chunk *)
      if not (Kernel.AllowDuplication.get ()) then
        raise (Failure "cannot duplicate: disallowed by user");
      if c.locals !=[] then
	raise (Failure "cannot duplicate: has locals");
      if c.cases != [] then raise (Failure "cannot duplicate: has cases") else
        let pCount = ref 0 in
        let duplicate_stmt (s,m,w,r,c) =
          if s.labels != [] then
            raise (Failure "cannot duplicate: has labels");
          (match s.skind with
            If _ | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
          | TryFinally _ | TryExcept _
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
          | Instr (Call _) -> [ref s']
          | Instr _ | TryExcept (_, _, _, _)| TryFinally (_, _, _)
          | UnspecifiedSequence _| Block _| Loop (_, _, _, _, _)
          | Switch (_, _, _, _)| If (_, _, _, _)| Continue _| Break _
          | Goto (_, _)| Return (_, _) -> assert (c = []); []
          in
          (s',m,w,r,c)
        in
        { stmts = List.map duplicate_stmt c.stmts;
          cases = []; unspecified_order = c.unspecified_order;
          locals = c.locals; (* varinfos must be shared anyway. *)
        }

    (* We can drop a chunk if it does not have labels inside *)
    let canDrop (c: chunk) =
      List.for_all (fun (s,_,_,_,_) -> canDropStatement s) c.stmts

    let loopChunk ~ghost a (body: chunk) : chunk =
      (* Make the statement *)
      let loop =
	mkStmt ~ghost 
          (Loop (a,c2block ~ghost body, CurrentLoc.get (), None, None))
      in
      { stmts = [ loop,[],[],[],[] ];
        cases = body.cases;
        unspecified_order = false;
        locals = [];
      }

    (* can be ghost inside a ghost loop *)
    let breakChunk ~ghost (l: location) : chunk =
      { stmts = [ mkStmt ~ghost (Break l),[],[],[],[] ];
        cases = [];
        unspecified_order = false;
        locals = [];
      }

    (* can be ghost inside a ghost loop *)
    let continueChunk ~ghost (l: location) : chunk =
      { stmts = [ mkStmt ~ghost (Continue l),[],[],[],[] ];
        cases = [];
        unspecified_order = false;
        locals = [];
      }

        (* Get the first statement in a chunk. Might need to change the
         * statements in the chunk *)
    let getFirstInChunk ~ghost ~loc c =
      (* Get the first statement and add the label to it *)
      match c.stmts with
        | [] -> (* Add a statement *)
            let n = mkEmptyStmt ~ghost ~loc () in
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
      }

    let gotoChunk ~ghost (ln: string) (l: location) : chunk =
      let gref = ref dummyStmt in
      addGoto ln gref;
      { stmts = [ mkStmt ~ghost (Goto (gref, l)),[],[],[],[] ];
        cases = [];
        locals = [];
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
          let constFold = constFold false in
          let e'' = if theMachine.lowerConstants then constFold e' else e' in
          (match (constFold e).enode, (constFold e'').enode with
            | Const(CInt64(i1, _, _)), Const(CInt64(i2, _, _))
                when not (Integer.equal i1 i2) ->
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
      let switch = mkStmt ~ghost (Switch (e, block, cases, l)) in
      { stmts = [ switch,[],[],[],[] ];
        cases = [];
        locals = [];
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
                  Block b when
                      (not (Stack.top unspecified_stack)) &&
                        b.battrs = [] && b.blocals = [] &&
			    s.labels = []
			    -> List.rev_append b.bstmts res
                | _ -> s ::res)
             [] b.bstmts);
        DoChildren

      method! vstmt s =
        let change_label_stmt s s' =
          List.iter
            (function
              | Label (x,_,_) -> H.replace labelStmt x s'
              | Case _ | Default _ -> replace_table <- (s, s') :: replace_table
            ) s.labels;
          s'.labels <- s.labels @ s'.labels
        in
        match s.skind with
          UnspecifiedSequence [s',_,_,_,_] ->
            change_label_stmt s s';
            ChangeDoChildrenPost(s', fun x -> x)
        | UnspecifiedSequence [] ->
          let s' = mkEmptyStmt ~loc:(cabslu "_useq") () in
          change_label_stmt s s';
          ChangeTo s';
        | UnspecifiedSequence _ -> self#push true s
        | Block { battrs = []; blocals = []; bstmts = [s']} ->
          change_label_stmt s s';
          ChangeDoChildrenPost (s', fun x -> x)
        | Block _ | If _ | Loop _
        | TryFinally _ | TryExcept _ ->
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

    let add_reads loc r c = match r with
      | [] -> c
      | _ :: _ -> c +++ (mkEmptyStmt ~loc (), [],[], r)

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
  | [] -> Kernel.abort ~current:true "continue not in a loop"
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
    Kernel.abort ~current:true "break outside of a loop or switch";
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
      EnvLabel l', _ -> l'
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
           AL.registerAlphaName alphaTable (kindPlusName "label" lbl) currentLoc
       | _ -> ());
    DoChildren
end


(** ALLOCA ***)
let allocaFun () =
  if theMachine.msvcMode then begin
    let name = "alloca" in
    let fdec = emptyFunction name in
    Cil.update_var_type fdec.svar
      (TFun(voidPtrType,
	    Some [ ("len", theMachine.typeOfSizeOf, []) ], false, []));
    fdec.svar
  end
  else
    (* Use __builtin_alloca where possible, because this can be used
       even when gcc is invoked with -fno-builtin *)
    let alloca, _ = lookupGlobalVar "__builtin_alloca" in
    alloca

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



(* We have our own version of addAttributes that does not allow duplicates *)
let cabsAddAttributes al0 (al: attributes) : attributes =
  if al0 == [] then al else
  List.fold_left
    (fun acc (Attr(an, _) | AttrAnnot an as a) ->
      (* See if the attribute is already in there *)
      match filterAttributes an acc with
        [] -> addAttribute a acc (* Nothing with that name *)
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

(* BY: nothing cabs here, plus seems to duplicate most of Cil.typeAddAttributes *)
let rec cabsTypeAddAttributes a0 t =
  begin
    match a0 with
    | [] ->
        (* no attributes, keep same type *)
          t
    | _ ->
        (* anything else: add a0 to existing attributes *)
          let add (a: attributes) = cabsAddAttributes a0 a in
          match t with
            TVoid a -> TVoid (add a)
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
                      Attr("mode", [ACons(mode,[])]) -> begin
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
              TInt (ik', cabsAddAttributes a0' a)

          | TFloat (fk, a) -> TFloat (fk, add a)
          | TEnum (enum, a) -> TEnum (enum, add a)
          | TPtr (t, a) -> TPtr (t, add a)
          | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
          | TComp (comp, s, a) -> TComp (comp, s, add a)
          | TNamed (t, a) -> TNamed (t, add a)
          | TBuiltin_va_list a -> TBuiltin_va_list (add a)
          | TArray (t, l, s, a) ->
              let att_elt, att_typ = Cil.splitArrayAttributes a0 in
              TArray (cabsArrayPushAttributes att_elt t, l, s,
                      cabsAddAttributes att_typ a)
  end
and cabsArrayPushAttributes al = function
   | TArray (bt, l, s, a) ->
       TArray (cabsArrayPushAttributes al bt, l, s, a)
   | t -> cabsTypeAddAttributes al t


(* Do types *)
    (* Combine the types. Raises the Failure exception with an error message.
     * isdef says whether the new type is for a definition *)
type combineWhat =
    CombineFundef of bool
  (* The new definition is for a function definition. The old
   * is for a prototype. arg is [true] for an old-style declaration *)
  | CombineFunarg of bool
  (* Comparing a function argument type with an old prototype argument.
     arg is [true] for an old-style declaration, which
     triggers some ad'hoc treatment in GCC mode. *)
  | CombineFunret (* Comparing the return of a function with that from an old
                   * prototype *)
  | CombineOther

(* We sometimes want to succeed in combining two structure types that are
 * identical except for the names of the structs. We keep a list of types
 * that are known to be equal *)
let isomorphicStructs : (string * string, bool) H.t = H.create 15

let rec combineTypes (what: combineWhat) (oldt: typ) (t: typ) : typ =
  match oldt, t with
  | TVoid olda, TVoid a -> TVoid (cabsAddAttributes olda a)
  | TInt (oldik, olda), TInt (ik, a) ->
      let combineIK oldk k =
        if oldk = k then oldk else
          (match what with
            | CombineFunarg b when
                not theMachine.msvcMode && oldk = IInt
                && bytesSizeOf t <= (bytesSizeOfInt IInt)
                && b ->
              (* GCC allows a function definition to have a more precise integer
               * type than a prototype that says "int" *)
              k
            | _ ->
              raise (Failure "different integer types"))
      in
      TInt (combineIK oldik ik, cabsAddAttributes olda a)
  | TFloat (oldfk, olda), TFloat (fk, a) ->
      let combineFK oldk k =
        if oldk = k then oldk else
          ( match what with
            | CombineFunarg b when
                not theMachine.msvcMode
                && oldk = FDouble
                && k = FFloat && b
                  ->
              (* GCC allows a function definition to have a more precise float
               * type than a prototype that says "double" *)
              k
            | _ ->
              raise (Failure "different floating point types"))
      in
      TFloat (combineFK oldfk fk, cabsAddAttributes olda a)
  | TEnum (_, olda), TEnum (ei, a) ->
      TEnum (ei, cabsAddAttributes olda a)

        (* Strange one. But seems to be handled by GCC *)
  | TEnum (oldei, olda) , TInt(IInt, a) -> TEnum(oldei,
                                                 cabsAddAttributes olda a)
        (* Strange one. But seems to be handled by GCC *)
  | TInt(IInt, olda), TEnum (ei, a) -> TEnum(ei, cabsAddAttributes olda a)


  | TComp (oldci, _, olda) , TComp (ci, _, a) ->
      if oldci.cstruct <> ci.cstruct then
        raise (Failure "different struct/union types");
      let comb_a = cabsAddAttributes olda a in
      if oldci.cname = ci.cname then
        TComp (oldci, empty_size_cache (), comb_a)
      else
        (* Now maybe they are actually the same *)
        if H.mem isomorphicStructs (oldci.cname, ci.cname) then
          (* We know they are the same *)
          TComp (oldci, empty_size_cache (), comb_a)
        else begin
          (* If one has 0 fields (undefined) while the other has some fields
           * we accept it *)
          let oldci_nrfields = List.length oldci.cfields in
          let ci_nrfields = List.length ci.cfields in
          if oldci_nrfields = 0 then
            TComp (ci, empty_size_cache (), comb_a)
          else if ci_nrfields = 0 then
            TComp (oldci, empty_size_cache (), comb_a)
          else begin
            (* Make sure that at least they have the same number of fields *)
            if  oldci_nrfields <> ci_nrfields then begin
(*
              ignore (E.log "different number of fields: %s had %d and %s had %d\n"
                        oldci.cname oldci_nrfields
                        ci.cname ci_nrfields);
*)
              raise (Failure "different structs(number of fields)");
            end;
            (* Assume they are the same *)
            H.add isomorphicStructs (oldci.cname, ci.cname) true;
            H.add isomorphicStructs (ci.cname, oldci.cname) true;
            (* Check that the fields are isomorphic and watch for Failure *)
            (try
              List.iter2 (fun oldf f ->
                if oldf.fbitfield <> f.fbitfield then
                  raise (Failure "different structs(bitfield info)");
                if oldf.fattr <> f.fattr then
                  raise (Failure "different structs(field attributes)");
                (* Make sure the types are compatible *)
                ignore (combineTypes CombineOther oldf.ftype f.ftype);
                ) oldci.cfields ci.cfields
            with Failure _ as e -> begin
              (* Our assumption was wrong. Forget the isomorphism *)
	      Kernel.debug ~level:2
                "Failed in our assumption that %s and %s are isomorphic"
                oldci.cname ci.cname ;
              H.remove isomorphicStructs (oldci.cname, ci.cname);
              H.remove isomorphicStructs (ci.cname, oldci.cname);
              raise e
            end);
            (* We get here if we succeeded *)
            TComp (oldci, empty_size_cache (), comb_a)
          end
        end

  | TArray (oldbt, oldsz, _, olda), TArray (bt, sz, _, a) ->
      let newbt = combineTypes CombineOther oldbt bt in
      let newsz =
        match oldsz, sz with
          None, Some _ -> sz
        | Some _, None -> oldsz
        | None, None -> sz
        | Some oldsz', Some sz' ->
            (* They are not structurally equal. But perhaps they are equal if
             * we evaluate them. Check first machine independent comparison  *)
           let checkEqualSize (machdep: bool) =
              compareExp
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
              raise (Failure "different array lengths")

      in
      TArray (newbt, newsz, empty_size_cache (), cabsAddAttributes olda a)

  | TPtr (oldbt, olda), TPtr (bt, a) ->
      TPtr (combineTypes CombineOther oldbt bt, cabsAddAttributes olda a)

  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
    let rt_what =
      match what with
        | CombineFundef _ -> CombineFunret
        | _ -> CombineOther
    in
    let newrt = combineTypes rt_what oldrt rt in
    if oldva != va then
      raise (Failure "different vararg specifiers");
      (* If one does not have arguments, believe the one with the
       * arguments *)
    let newargs, olda' =
      if oldargs = None then args, olda else
        if args = None then oldargs, olda else
          let oldargslist = argsToList oldargs in
          let argslist = argsToList args in
          if List.length oldargslist <> List.length argslist then
            raise (Failure "different number of arguments")
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
                  CombineFundef b -> CombineFunarg b
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
    TFun (newrt, newargs, oldva, cabsAddAttributes olda a)

  | TNamed (oldt, olda), TNamed (t, a) when oldt.tname = t.tname ->
      TNamed (oldt, cabsAddAttributes olda a)

  | TBuiltin_va_list olda, TBuiltin_va_list a ->
      TBuiltin_va_list (cabsAddAttributes olda a)

        (* Unroll first the new type *)
  | _, TNamed (t, a) ->
      let res = combineTypes what oldt t.ttype in
      cabsTypeAddAttributes a res

        (* And unroll the old type as well if necessary *)
  | TNamed (oldt, a), _ ->
      let res = combineTypes what oldt.ttype t in
      cabsTypeAddAttributes a res

  | _ -> raise (Failure "different type constructors")

let cleanup_isomorphicStructs () = H.clear isomorphicStructs

let compatibleTypes t1 t2 =
  try
    let r = combineTypes CombineOther t1 t2 in
    cleanup_isomorphicStructs ();
    r
  with Failure _ as e ->
    cleanup_isomorphicStructs ();
    raise e

let compatibleTypesp t1 t2 =
  try
    ignore (combineTypes CombineOther t1 t2);
    cleanup_isomorphicStructs ();
    true
  with Failure _ ->
    cleanup_isomorphicStructs ();
    false

let extInlineSuffRe = Str.regexp "\\(.+\\)__extinline"

(* Create and cache varinfo's for globals. Starts with a varinfo but if the
 * global has been declared already it might come back with another varinfo.
 * Returns the varinfo to use (might be the old one), and an indication
 * whether the variable exists already in the environment *)
let makeGlobalVarinfo (isadef: bool) (vi: varinfo) : varinfo * bool =
  try (* See if already defined, in the global environment. We could also
       * look it up in the whole environment but in that case we might see a
       * local. This can happen when we declare an extern variable with
       * global scope but we are in a local scope. *)

    (* We lookup in the environement. If this is extern inline then the name
     * was already changed to foo__extinline. We lookup with the old name *)
    let lookupname =
      if vi.vstorage = Static then
        if Str.string_match extInlineSuffRe vi.vname 0 then
          let no_extinline_name = Str.matched_group 1 vi.vname in
          if no_extinline_name=vi.vorig_name then no_extinline_name
          else vi.vname
        else
          vi.vname
      else
        vi.vname
    in
    Kernel.debug ~dkey:category_global
      "makeGlobalVarinfo isadef=%b vi.vname=%s (lookup = %s)"
      isadef vi.vname lookupname;
    (* This may throw an exception Not_found *)
    let oldvi, oldloc = lookupGlobalVar lookupname in
    Kernel.debug ~dkey:category_global "  %s(%d) already in the env at loc %a"
      vi.vname oldvi.vid Cil_printer.pp_location oldloc;
    (* It was already defined. We must reuse the varinfo. But clean up the
     * storage.  *)
    let newstorage = (** See 6.2.2 *)
      match oldvi.vstorage, vi.vstorage with
        (* Extern and something else is that thing *)
      | Extern, other
      | other, Extern -> other

      | NoStorage, other
      | other, NoStorage ->  other


      | _ ->
	if vi.vstorage != oldvi.vstorage then
          Kernel.warning ~current:true
	    "Inconsistent storage specification for %s. \
             Previous declaration: %a"
	    vi.vname Cil_printer.pp_location oldloc;
        vi.vstorage
    in
    oldvi.vinline <- oldvi.vinline || vi.vinline;
    oldvi.vstorage <- newstorage;
    (* If the new declaration has a section attribute, remove any
     * preexisting section attribute. This mimics behavior of gcc that is
     * required to compile the Linux kernel properly. *)
    if hasAttribute "section" vi.vattr then
      oldvi.vattr <- dropAttribute "section" oldvi.vattr;
    (* Union the attributes *)
    oldvi.vattr <- cabsAddAttributes oldvi.vattr vi.vattr;
    begin
      try
        let what =
          if isadef then
            CombineFundef (hasAttribute "FC_OLDSTYLEPROTO" vi.vattr)
          else CombineOther
        in
        let mytype = combineTypes what oldvi.vtype vi.vtype in
        if not (Cil_datatype.Typ.equal oldvi.vtype vi.vtype)
        then DifferentDeclHook.apply (oldvi,vi);
        Cil.update_var_type oldvi mytype;
      with Failure reason ->
        Kernel.debug ~dkey:category_global "old type = %a\nnew type = %a\n"
	  Cil_printer.pp_typ oldvi.vtype
          Cil_printer.pp_typ vi.vtype ;
        Kernel.error ~once:true ~current:true
	  "Declaration of %s does not match previous declaration from %a (%s)."
          vi.vname Cil_printer.pp_location oldloc reason;
        IncompatibleDeclHook.apply (oldvi,vi,reason)
    end;

    (* Found an old one. Keep the location always from the definition *)
    if isadef then begin
      oldvi.vdecl <- vi.vdecl;
    end;

    (* Let's mutate the formals vid's name attribute and type for function prototypes.
       Logic specifications refer to the varinfo in this table. *)
    begin
      match vi.vtype with
      | TFun (_,Some formals , _, _ ) ->
          (try
             let old_formals_env = getFormalsDecl oldvi in
             List.iter2
               (fun old (name,typ,attr) ->
                  if name <> "" then begin
                    Kernel.debug ~dkey:category_global
                      "replacing formal %s with %s" old.vname name;
                    old.vname <- name;
                    Cil.update_var_type old typ;
                    old.vattr <- attr;
                    (match old.vlogic_var_assoc with
                         None -> ()
                       | Some old_lv -> old_lv.lv_name <- name)
                  end)
               old_formals_env
               formals;
           with
           | Invalid_argument "List.iter2" ->
                 Kernel.abort "Inconsistent formals" ;
           | Not_found -> 
               Cil.setFormalsDecl oldvi vi.vtype)
      | _ -> ()
    end ;
    (* update the field [vdefined] *)
    if isadef then oldvi.vdefined <- true;
    (* the *immutable* vgenerated field in oldvi cannot be updated. We assume
       that all Frama-C builtins bear the FC_BUILTIN attribute - and thus are
       translated into variables with vgenerated fields at [true]. *)
    oldvi, true
  with Not_found -> begin (* A new one.  *)
    Kernel.debug ~level:2 ~dkey:category_global
      "  %s not in the env already" vi.vname;
    (* Announce the name to the alpha conversion table. This will not
     * actually change the name of the vi. See the definition of
     * alphaConvertVarAndAddToEnv *)
    let vi = alphaConvertVarAndAddToEnv true vi in
    (* update the field [vdefined] *)
    if isadef then vi.vdefined <- true;
    vi.vattr <- dropAttribute "FC_OLDSTYLEPROTO" vi.vattr;
    vi, false
  end

let conditionalConversion (t2: typ) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    match unrollType t2, unrollType t3 with
      (TInt _ | TEnum _ | TFloat _),
      (TInt _ | TEnum _ | TFloat _) ->
        arithmeticConversion t2 t3
    | TComp (comp2,_,_), TComp (comp3,_,_)
          when comp2.ckey = comp3.ckey -> t2
    | TPtr(_, _), TPtr(TVoid _, _) -> t2
    | TPtr(TVoid _, _), TPtr(_, _) -> t3
    | TPtr _, TPtr _ when Cil_datatype.Typ.equal t2 t3 -> t2
    | TPtr _, TInt _  -> t2 (* most likely comparison with 0 *)
    | TInt _, TPtr _ -> t3 (* most likely comparison with 0 *)

          (* When we compare two pointers of diffent type, we combine them
           * using the same algorithm when combining multiple declarations of
           * a global *)
    | (TPtr _) as t2', (TPtr _ as t3') -> begin
        try combineTypes CombineOther t2' t3'
        with Failure msg -> begin
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

(* Some utilitites for doing initializers *)

type preInit =
  | NoInitPre
  | SinglePre of exp
  | CompoundPre of int ref (* the maximum used index *)
                 * preInit array ref (* an array with initializers *)


(* Set an initializer *)
let rec setOneInit (this: preInit)
                   (o: offset) (e: exp) : preInit =
  match o with
    NoOffset -> SinglePre e
  | _ ->
      let idx, (* Index in the current comp *)
          restoff (* Rest offset *) =
        match o with
        | Index({enode = Const(CInt64(i,_,_))}, off) -> Integer.to_int i, off
        | Field (f, off) ->
            (* Find the index of the field *)
            let rec loop (idx: int) = function
              | [] -> Kernel.abort ~current:true "Cannot find field %s" f.fname
              | f' :: _ when f'.fname = f.fname -> idx
              | _ :: restf -> loop (idx + 1) restf
            in
            loop 0 f.fcomp.cfields, off
        | _ -> Kernel.abort ~current:true "setOneInit: non-constant index"
      in
      let pMaxIdx, pArray =
        match this  with
          NoInitPre  -> (* No initializer so far here *)
            ref idx, ref (Array.create (max 32 (idx + 1)) NoInitPre)

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
      let this' = setOneInit !pArray.(idx) restoff e in
      !pArray.(idx) <- this';
      CompoundPre (pMaxIdx, pArray)


(* collect a CIL initializer, given the original syntactic initializer
 * 'preInit'; this returns a type too, since initialization of an array
 * with unspecified size actually changes the array's type
 * (ANSI C, 6.7.8, para 22) *)
let rec collectInitializer
    (this: preInit)
    (thistype: typ) : (init * typ) =
  let loc = CurrentLoc.get() in
  if this = NoInitPre then (makeZeroInit ~loc thistype), thistype
  else
    match unrollType thistype, this with
    | _ , SinglePre e -> SingleInit e, thistype
    | TArray (bt, leno, _, at), CompoundPre (pMaxIdx, pArray) ->
        let len, initializer_len_used =
          (* normal case: use array's declared length, newtype=thistype *)
          match leno with
            Some len -> begin
              match (constFold true len).enode with
              | Const(CInt64(ni, _, _)) when Integer.ge ni Integer.zero ->
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
          Kernel.abort ~current:true
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
        let collect_one_init v (idx,init,typ,len_used) =
          match v with
            | NoInitPre -> (idx-1,init,typ,len_used)
            | _ -> 
                let (vinit,typ') = collectInitializer v typ in
                let len_used =
                  len_used || not (Cil_datatype.Typ.equal typ typ')
                in
                (idx-1,
                 (Index (integer ~loc idx,NoOffset), vinit)::init,
                 typ',
                 len_used)
        in
        let (_,init,typ, len_used) =
          Array.fold_right collect_one_init
            !pArray (Array.length !pArray - 1, [], bt, initializer_len_used)
        in
        let newtype =
          TArray (typ, Some (integer ~loc len), empty_size_cache (), at)
        in
        CompoundInit (newtype, (* collect [] endAt*)init),
        (* If the sizes of the initializers have not been used anywhere,
           we can fold back an eventual typedef. Otherwise, push the
           attributes to the elements of the array *)
        (if len_used then newtype else thistype)

    | TComp (comp, _, _), CompoundPre (pMaxIdx, pArray) when comp.cstruct ->
        let rec collect (idx: int) = function
            [] -> []
          | f :: restf ->
              if f.fname = missingFieldName then
                collect (idx + 1) restf
              else
                let thisi =
                  if idx > !pMaxIdx then
                    makeZeroInit ~loc f.ftype
                  else
                    collectFieldInitializer !pArray.(idx) f
                in
                (Field(f, NoOffset), thisi) :: collect (idx + 1) restf
        in
        CompoundInit (thistype, collect 0 comp.cfields), thistype

    | TComp (comp, _, _), CompoundPre (pMaxIdx, pArray) when not comp.cstruct ->
        (* Find the field to initialize *)
        let rec findField (idx: int) = function
          | [] -> Kernel.abort ~current:true "collectInitializer: union"
          | _ :: rest when idx < !pMaxIdx && !pArray.(idx) = NoInitPre ->
              findField (idx + 1) rest
          | f :: _ when idx = !pMaxIdx ->
              Field(f, NoOffset),
              collectFieldInitializer !pArray.(idx) f
          | _ ->
	    Kernel.fatal ~current:true "Can initialize only one field for union"
        in
        if theMachine.msvcMode && !pMaxIdx != 0 then
          Kernel.warning ~current:true
	    "On MSVC we can initialize only the first field of a union";
        CompoundInit (thistype, [ findField 0 comp.cfields ]), thistype

    | _ -> Kernel.fatal ~current:true "collectInitializer"

and collectFieldInitializer
    (this: preInit)
    (f: fieldinfo) : init =
  (* collect, and rewrite type *)
  let init,newtype = (collectInitializer this f.ftype) in
  f.ftype <- newtype;
  init


type stackElem =
    InArray of offset * typ * int * int ref (* offset of parent, base type,
                                             * length, current index. If the
                                             * array length is unspecified we
                                             * use Int.max_int  *)
  | InComp  of offset * compinfo * fieldinfo list (* offset of parent,
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
    GVarDecl(spec,v,_) ->
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
    [] -> so.soOff <- so.curOff; so.soTyp <- so.curTyp
        (* The array is over *)
  | InArray (parOff, bt, leno, current) :: rest ->
      if leno = !current then begin (* The array is over *)
        Kernel.debug ~dkey:category_initializer "Past the end of array";
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
  | InComp (parOff, _, nextflds) :: rest ->
      if nextflds == [] then begin (* No more fields here *)
        Kernel.debug ~dkey:category_initializer "Past the end of structure";
        so.stack <- rest;
        advanceSubobj so
      end else begin
        let fst = List.hd nextflds in
        so.soTyp <- fst.ftype;
        so.soOff <- addOffset (Field(fst, NoOffset)) parOff
      end

  (* Advance to the next subobject. Always apply to a normalized object *)
and advanceSubobj (so: subobj) : unit =
  if so.eof then Kernel.abort ~current:true "advanceSubobj past end";
  match so.stack with
  | [] ->
      Kernel.debug ~dkey:category_initializer "Setting eof to true";
      so.eof <- true
  | InArray (_, _, _, current) :: _ ->
      Kernel.debug ~dkey:category_initializer
        "  Advancing to [%d]" (!current + 1);
      (* so.stack <- InArray (parOff, bt, leno, current + 1) :: rest; *)
      incr current;
      normalSubobj so

        (* The fields are over *)
  | InComp (parOff, comp, nextflds) :: rest ->
      Kernel.debug ~dkey:category_initializer
        "Advancing past .%s" (List.hd nextflds).fname;
      let flds' =
	try List.tl nextflds
	with Failure _ -> Kernel.abort ~current:true "advanceSubobj"
      in
      so.stack <- InComp(parOff, comp, flds') :: rest;
      normalSubobj so



(* Find the fields to initialize in a composite. *)
let fieldsToInit
    (comp: compinfo)
    (designator: string option)
    : fieldinfo list =
  (* Never look at anonymous fields *)
  let flds1 =
    List.filter (fun f -> f.fname <> missingFieldName) comp.cfields in
  let flds2 =
    match designator with
      None -> flds1
    | Some fn ->
        let rec loop = function
          | [] ->
	    Kernel.fatal ~current:true "Cannot find designated field %s" fn
          | (f :: _) as nextflds when f.fname = fn -> nextflds
          | _ :: rest -> loop rest
        in
        loop flds1
  in
  (* If it is a union we only initialize one field *)
  match flds2 with
    [] -> []
  | (f :: _) as toinit ->
      if comp.cstruct then toinit else [f]


let integerArrayLength (leno: exp option) : int =
  match leno with
  | None -> max_int
  | Some len ->
    try lenOfArray leno
    with LenOfArray ->
      Kernel.fatal ~current:true
	"Initializing non-constant-length array with length=%a"
        Cil_printer.pp_exp len

let anonCompFieldNameId = ref 0
let anonCompFieldName = "__anonCompField"

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
	  Kernel.abort ~current:true "unnamed field type is not a struct/union"
      end
    | _ :: rest -> search rest
  in
  search fidlist

let findField n fidlist =
  try
    find_field_offset (fun x -> x.fname = n) fidlist
  with Not_found ->
    Kernel.abort ~current:true "Cannot find field %s" n

(* Utility ***)
let rec replaceLastInList
    (lst: A.expression list)
    (how: A.expression -> A.expression) : A.expression list=
  match lst with
    [] -> []
  | [e] -> [how e]
  | h :: t -> h :: replaceLastInList t how

let convBinOp (bop: A.binary_operator) : binop =
  match bop with
    A.ADD -> PlusA
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
            Cil.intTypeIncluded ikf iklv

        | TFloat (fklv, _), TFloat (fkf, _) ->
            Cil.frank fklv >= Cil.frank fkf

        | _, _ -> false
    )


let afterConversion ~ghost (c: chunk) : chunk =
  (* Now scan the statements and find Instr blocks *)
  (** We want to collapse sequences of the form "tmp = f(); v = tmp". This
      * will help significantly with the handling of calls to malloc, where it
      * is important to have the cast at the same place as the call *)
  let tcallres f =
    match unrollType (typeOf f) with
      | TFun (rt, _, _, _) -> rt
      | _ -> Kernel.abort ~current:true "Function call to a non-function"
  in
  let collapseCallCast (s1,s2) = match s1.skind, s2.skind with
    | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
      Instr (Set(destlv,
                 {enode = CastE (newt,
                                 {enode = Lval(Var vi', NoOffset)})}, _)) ->
        if (not vi.vglob &&
              vi' == vi &&
              String.length vi.vname >= 3 &&
              (* Watch out for the possibility that we have an implied cast in
               * the call *)
              (let tcallres = tcallres f in
               Cil_datatype.Typ.equal tcallres vi.vtype &&
               Cil_datatype.Typ.equal newt (typeOfLval destlv) &&
               allow_return_collapse ~tf:tcallres ~tlv:newt) &&
              IH.mem callTempVars vi.vid)
        then begin
          s1.skind <- Instr(Call(Some destlv, f, args, l));
          Some [ s1 ]
        end
        else None
  | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
        Instr (Set(destlv, {enode = Lval(Var vi', NoOffset)}, _)) ->
      if (not vi.vglob &&
            vi' == vi &&
            String.length vi.vname >= 3 &&
            (* Watch out for the possibility that we have an implied cast in
             * the call *)
            IH.mem callTempVars vi.vid &&
            Cil_datatype.Typ.equal vi.vtype (typeOfLval destlv) &&
            allow_return_collapse ~tf:(tcallres f) ~tlv:vi.vtype
      )
      then begin
        s1.skind <- Instr(Call(Some destlv, f, args, l));
        Some [ s1 ]
      end else None
  | _ -> None
  in
  let block = c2block ~ghost ~collapse_block:false c in
  let sl =
    if Kernel.DoCollapseCallCast.get () then
      peepHole2 ~agressive:false collapseCallCast block.bstmts
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
    [] -> ""
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
       (Pretty_utils.sfprintf "term %a has type %a, but %a is expected."
          Cil_printer.pp_term t Cil_printer.pp_logic_type Linteger Cil_printer.pp_typ ty))

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
  let find_comp_type ~kind s = findCompType kind s []

  let find_comp_field info s = findField s info.cfields

  let find_type s = let t,_ = lookupTypeNoError "type" s in t

  include Logic_labels

  include Logic_env

  let add_logic_function =
    add_logic_function_gen Logic_utils.is_same_logic_profile

  let integral_cast = integral_cast

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
    [] -> Kernel.error ~once:true ~current:true "exit Loop not in a loop"
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
    | UndoRemoveFromAlphaTable n :: t -> H.remove alphaTable n; loop t
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
      is_ghost: bool }

let empty_local_env =
  { authorized_reads = Lval.Set.empty;
    known_behaviors = [];
    is_ghost = false }

let ghost_local_env ghost = {empty_local_env with is_ghost = ghost }

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
    Set _ -> true
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
      Instr(il) ->
        instrFallsThrough il
    | UnspecifiedSequence seq ->
        blockFallsThrough (block_from_unspecified_sequence seq)
    | Return _ | Break _ | Continue _ -> false
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
      Instr _ | Return _ | Continue _ | Goto _ -> false
    | Break _ -> true
    | UnspecifiedSequence seq ->
        blockCanBreak (block_from_unspecified_sequence seq)
    | If (_, b1, b2, _) ->
        blockCanBreak b1 || blockCanBreak b2
    | Switch _ | Loop _ ->
        (* switches and loops catch any breaks in their bodies *)
        false
    | Block b -> blockCanBreak b
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
      let b = c2block ~ghost current_chunk in
      (* The statement may contain some local variable
         declarations coming from userland. We have to shift
         them from the inner block, otherwise they will not
         be accessible in the next statements.
       *)
      let locals = b.blocals in
      b.blocals <- [];
      b.battrs <-
        addAttributes [Attr(frama_c_keep_block,[])] b.battrs;
      let block = mkStmt ~ghost (Block b) in
      let chunk = s2c block in
      let chunk = { chunk with cases = current_chunk.cases } in
      annot_chunk @@ (List.fold_left
                        local_var_chunk chunk (List.rev locals), ghost)

let ensures_init vi off ini =
  let cast = false in
  let lv = Cil.cvar_to_lvar vi in
  let lo = Logic_utils.offset_to_term_offset ~cast off in
  let lini = Logic_utils.expr_to_term ~cast ini in
  let loc = lini.term_loc in
  let base = (TVar lv, lo) in
  let lval = Logic_const.term ~loc (TLval base) (Cil.typeOfTermLval base) in
  Logic_const.prel ~loc (Req,lval,lini)

let zero_enum ~loc e =
  try
    let ei = List.find (fun e -> Cil.isZero e.eival) e.eitems in
    Cil.new_exp ~loc (Const (CEnum ei))
  with Not_found -> Cil.kinteger ~loc e.ekind 0

(* memset to 0 an entire array. *)
let set_to_zero ~ghost vi off typ =
  let loc = vi.vdecl in
  let bzero =
    try
      Cil.Frama_c_builtins.find "Frama_C_bzero"
    with Not_found ->
      Kernel.fatal
        "Incorrect Cil initialization: cannot find Frama_C_bzero builtin"
  in
  let zone =
    Cil.new_exp ~loc
      (CastE(TPtr(TInt (IUChar,[]),[]),
             Cil.new_exp ~loc (StartOf(Var vi,off))))
  in
  let size =
    Cil.new_exp ~loc
      (CastE (TInt(IULong,[]),
              Cil.new_exp ~loc (SizeOf typ)))
  in
  Cil.mkStmt ~ghost
    (Instr
       (Call
          (None,Cil.evar ~loc bzero,
           [zone; size], loc)))


exception ChangeSize of Cil_types.exp

(* Initialize the first cell of an array, and call Frama_C_copy_block to
   propagate this initialization to the rest of the array.
   Array is located at vi.off, of length len, and cells are of type base_type.
*)
let rec zero_init ~ghost vi off len base_typ =
  let loc = vi.vdecl in
  let copy =
    try
      Cil.Frama_c_builtins.find "Frama_C_copy_block"
    with Not_found ->
      Kernel.fatal
        "Incorrect Cil initialization: cannot find Frama_C_copy_block builtin"
  in
  let zone =
    Cil.new_exp ~loc
      (CastE(TPtr(TInt (IUChar,[]),[]),
             Cil.new_exp ~loc (StartOf(Var vi,off))))
  in
  let size =
     Cil.new_exp ~loc
      (CastE (TInt(IULong,[]),
              Cil.new_exp ~loc (SizeOf base_typ)))
  in
  let len = Cil.kinteger ~loc IULong len in
  let off = Cil.addOffset (Index (Cil.integer ~loc 0, NoOffset)) off in
  let zero_init = zero_init_cell ~ghost vi off base_typ in
  zero_init +++
    (Cil.mkStmt ~ghost
       (Instr
          (Call
             (None, Cil.evar ~loc copy, [zone; size; len], loc))),
     [],[], [(Var vi,off)])

and zero_init_cell ~ghost vi off typ =
  let loc = vi.vdecl in
  match Cil.unrollType typ with
    | TVoid _ -> empty
    | TInt(ikind,_) ->
      let lv = (Var vi,off) in
      s2c
        (Cil.mkStmt ~ghost (Instr (Set (lv, (Cil.kinteger ~loc ikind 0),loc))))

    | TFloat (fkind,_) ->
      let lv = (Var vi,off) in
      s2c (Cil.mkStmt ~ghost (Instr (Set (lv, (Cil.kfloat ~loc fkind 0.),loc))))

    | TPtr _ ->
      let lv = (Var vi,off) in
      let exp = Cil.new_exp ~loc (CastE(typ,Cil.zero ~loc)) in
      s2c (Cil.mkStmt ~ghost (Instr (Set (lv, exp,loc))))

    | TArray(_,None,_,_) ->
      Kernel.fatal ~source:(fst loc)
        "Trying to zero-initialize variable with incomplete type"

    | TArray(typ,Some e,_,_) ->
      let len =
        match Cil.constFold true e with
          | { enode = Const (CInt64 (i,_,_)) } -> Integer.to_int i
          | _ ->
            Kernel.fatal ~source:(fst loc)
              "Trying to zero-initialize variable with incomplete type"
      in
      zero_init ~ghost vi off len typ

    | TFun _ -> Kernel.fatal "Trying to zero-initialize a function"

    | TNamed _ -> assert false (* guarded by unrollType *)

    | TComp (ci,_,_) ->
      let treat_one_field acc fi =
        let off = Cil.addOffset (Field (fi,NoOffset)) off in
        acc @@
          (zero_init_cell ~ghost vi off fi.ftype, ghost)
      in
      if ci.cstruct then
        List.fold_left treat_one_field empty ci.cfields
      else begin
        (* Standard says that zero initializing an union is done by setting
           its first field to 0
        *)
        match ci.cfields with
          | [] -> Kernel.fatal "Union type without fields"
          | fst :: _ -> treat_one_field empty fst
      end

    | TEnum (ei,_) ->
      let lv = (Var vi,off) in
      let zero = zero_enum ~loc ei in
      s2c (mkStmt ~ghost (Instr (Set (lv,zero,loc))))
    
    | TBuiltin_va_list _ ->
      Kernel.fatal "Found builtin varargs in zero-initialization"

let get_implicit_indexes loc vi len known_idx =
  let split_itv i itvs =
    let i = Integer.to_int i in
    let rec aux processed remaining =
      match remaining with
        | [] ->
          Kernel.warning ~current:true
            "Unexpected index in array initialization (bad computed length?)";
          List.rev processed
        | (low,high) as itv :: tl ->
          if i < low then begin
            (* should have been captured by earlier interval*)
            Kernel.warning ~current:true
              "Unexpected index in array initialization \
               (double initialization?)";
            List.rev_append processed remaining
          end
          else if i > high then aux (itv::processed) tl
          else (* split the interval *)
            if i = low then
              if high = low then (* interval is a singleton, just remove it*)
                List.rev_append processed tl
              else (* remove first elt of interval *)
                List.rev_append processed ((low+1,high)::tl)
            else if i = high then (* remove last elt of interval,
                                     which is not singleton *)
              List.rev_append processed ((low,high-1)::tl)
            else (* split interval in two, non empty intervals.  *)
              List.rev_append processed ((low,i-1)::(i+1,high)::tl)
    in
    aux [] itvs
  in
  let unknown_idx =
    Datatype.Big_int.Set.fold split_itv known_idx [0,pred len]
  in
  let one_range acc (low,high) =
    Logic_const.pand ~loc
      (acc,Logic_const.pand ~loc
        (Logic_const.prel ~loc
           (Rle, Logic_const.tinteger ~loc low, Logic_const.tvar vi),
         Logic_const.prel ~loc
           (Rle, Logic_const.tvar vi, Logic_const.tinteger ~loc high)))
  in
  List.fold_left one_range Logic_const.ptrue unknown_idx

let ensures_is_zero_offset loc term typ =
  let rec aux nb_idx term typ =
    let mk_term () =
      Logic_const.term ~loc (TLval term) (Cil.typeOfTermLval term)
    in
    match Cil.unrollType typ with
      | TVoid _ ->
        Kernel.warning "trying to zero-initialize a void value"; Logic_const.ptrue
      | TInt _ ->
        Logic_const.prel(Req,mk_term (),Logic_const.tinteger ~loc 0)
      | TFloat _ ->
        Logic_const.prel (Req,mk_term (),Logic_const.treal ~loc 0.)
      | TPtr _ ->
        Logic_const.prel (Req, mk_term (), Logic_const.term ~loc Tnull (Ctype typ))
      | TArray (t,e,_,_) ->
        let name = "__i" ^ string_of_int nb_idx in
        let vi = Cil_const.make_logic_var_quant name Linteger in
        let idx = Logic_const.tvar ~loc vi in
        let max =
          match e with
            | None -> Logic_const.ptrue
            | Some e ->
              Logic_const.prel ~loc
                (Rlt, idx, Logic_utils.expr_to_term ~cast:false e)
        in
        let pre =
          Logic_const.pand ~loc
            (Logic_const.prel ~loc (Rle, Logic_const.tinteger ~loc 0, idx),max)
        in
        let subterm =
          Logic_const.addTermOffsetLval (TIndex (idx,TNoOffset)) term
        in
        let cond = aux (nb_idx + 1) subterm t in
        Logic_const.pforall ~loc ([vi], Logic_const.pimplies ~loc (pre, cond))
      | TFun _ -> Kernel.fatal "Trying to zero-initialize a function"
      | TNamed _ -> assert false (* protected by unrollType *)
      | TComp (c,_,_) ->
        let treat_one_field acc fi =
          let subterm =
            Logic_const.addTermOffsetLval (TField (fi,TNoOffset)) term
          in
          let cond = aux nb_idx subterm fi.ftype in
          Logic_const.pand ~loc (acc,cond)
        in
        if c.cstruct then
          List.fold_left treat_one_field Logic_const.ptrue c.cfields
        else
          (match c.cfields with
            | [] -> Kernel.fatal "zero-initialize a union with no members"
            | f :: _ -> treat_one_field Logic_const.ptrue f)
      | TEnum (e,_) ->
        let zero = Logic_utils.expr_to_term ~cast:false (zero_enum ~loc e) in
        Logic_const.prel ~loc (Req,mk_term (),zero)
      | TBuiltin_va_list _ ->
        Kernel.fatal "Trying to zero-initialize a vararg list"
  in
  aux 0 term typ

(* Make a contract for a block that performs partial initialization of a local,
   relying on bzero for implicit zero-initialization.
*)
let make_implicit_ensures vi off base_typ len known_idx =
  let loc = vi.vdecl in
  let i = Cil_const.make_logic_var_quant "__i" Linteger in
  let pre = get_implicit_indexes loc i len known_idx in
  let lv = Cil.cvar_to_lvar vi in
  let lo = Logic_utils.offset_to_term_offset ~cast:false off in
  let base = (TVar lv, lo) in
  let term =
    Logic_const.addTermOffsetLval (TIndex (Logic_const.tvar i, TNoOffset)) base
  in
  let res = ensures_is_zero_offset loc term base_typ in
  let cond = Logic_const.pimplies ~loc (pre, res) in
  Logic_const.pforall ~loc ([i],cond)

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
          Kernel.abort ~current:true 
            "implicit prototype cannot have variadic arguments"
      | TNamed _ -> assert false (* unrollType *)
  in
  (* if we make a promotion, take it explicitely 
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
      A.SpecTypedef -> acc
    | A.SpecInline -> isinline := true; acc
    | A.SpecStorage st ->
      if !storage <> NoStorage then
        Kernel.error ~once:true ~current:true "Multiple storage specifiers";
      let sto' =
        match st with
          A.NO_STORAGE -> NoStorage
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
    | A.SpecPattern _ ->
      Kernel.abort ~current:true "SpecPattern in cabs2cil input"
  in
  (* Now scan the list and collect the type specifiers. Preserve the order *)
  let tspecs = List.fold_right doSpecElem specs [] in

  let tspecs' =
    (* GCC allows a named type that appears first to be followed by things
     * like "short", "signed", "unsigned" or "long". *)
    match tspecs with
      A.Tnamed _ :: (_ :: _ as rest) when not theMachine.msvcMode ->
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
      [A.Tvoid] -> TVoid []
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
    | [A.Tnamed n] ->
      if n = "__builtin_va_list" &&
        Cil.theMachine.theMachine.has__builtin_va_list
      then
        TBuiltin_va_list []
      else
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
      enum.eattr <- doAttributes ghost a;
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
	  if theMachine.msvcMode then
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
          let e' = match isInteger (constFold true e') with
            | None ->
	      Kernel.fatal ~current:true
                "Constant initializer %a not an integer"
                Cil_printer.pp_exp e'
            | Some i ->
              let ik = updateEnum i in
	      if theMachine.lowerConstants then
                kinteger64 ~loc:e.expr_loc ik i
              else e'
          in
          processName kname e' (convLoc cloc) rest
      in

      (*TODO: find a better loc*)
      let fields = loop (zero ~loc:(CurrentLoc.get())) eil in
      (* Now set the right set of items *)
      enum.eitems <- List.map (fun (_, x) -> x) fields;
      (* Pick the enum's kind - see discussion above *)
	if not theMachine.msvcMode then begin
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
      let (_, _, e', t) = doExp (ghost_local_env ghost) false e AExpLeaveArrayFun in
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
          | _ ->
	    Kernel.abort ~current:true
	      "The typeOf a string is not a pointer type"
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
  let t =
    if not isglobal && not isformal then begin
      (* Sometimes we call this on the formal argument of a function with no

         * arguments. Don't call stripConstLocalType in that case *)
      (*      ignore (E.log "stripConstLocalType(%a) for %s\n" d_type vtype n); *)
      stripConstLocalType vtype
    end else
      vtype
  in
  (*  log "Looking at %s(%b): (%a)@." n isformal d_attrlist nattr;*)

  let vi = makeVarinfo ~generated:isgenerated isglobal isformal n t in
  vi.vstorage <- sto;
  vi.vattr <- nattr;
  vi.vdecl <- ldecl;
  vi.vghost <- ghost;

  (*  if false then
      log "Created varinfo %s : %a\n" vi.vname d_type vi.vtype;*)

  vi

(* Process a local variable declaration and allow variable-sized arrays *)
and makeVarSizeVarInfo ghost (ldecl : location)
    spec_res
    (n,ndt,a)
    : varinfo * chunk * exp * bool =
  if not theMachine.msvcMode then
    match isVariableSizedArray ghost ndt with
      None ->
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
        A.VARIABLE n -> begin
          let n' = if strip then stripUnderscore n else n in
          (** See if this is an enumeration *)
          try
            if not foldenum then raise Not_found;

            match H.find env n' with
              EnvEnum item, _ -> begin
                match isInteger (constFold true item.eival) with
                  Some i64 when theMachine.lowerConstants ->
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
          [] -> default
        | (Attr(an, _) | AttrAnnot an)::_ ->
          (try attributeClass an with Not_found -> default)
      in
      match kind with
        AttrName _ -> loop (a::n, f, t) rest
      | AttrFunType _ -> loop (n, a::f, t) rest
      | AttrType -> loop (n, f, a::t) rest
  in
  loop ([], [], []) attrs



and doType (ghost:bool) isFuncArg
    (nameortype: attributeClass) (* This is AttrName if we are doing
                                  * the type for a name, or AttrType
                                  * if we are doing this type in a
                                  * typedef *)
    ?(allowVarSizeArrays=false)
    (bt: typ)                    (* The base type *)
    (dt: A.decl_type)
    (* Returns the new type and the accumulated name (or type attribute
       if nameoftype =  AttrType) attributes *)
    : typ * attribute list =

  (* Now do the declarator type. But remember that the structure of the
   * declarator type is as printed, meaning that it is the reverse of the
   * right one *)
  let rec doDeclType (bt: typ) (acc: attribute list) = function
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
          TFun _ -> cabsTypeAddAttributes a1f bt', true
        | _ -> bt', false
      in
      (* Now recurse *)
      let restyp, nattr = doDeclType bt'' acc d in
      (* Add some more type attributes *)
      let restyp = cabsTypeAddAttributes a2t restyp in
      (* See if we can add some more type attributes *)
      let restyp' =
        match unrollType restyp with
          TFun _ ->
            if a1fadded then
              cabsTypeAddAttributes a2f restyp
            else
              cabsTypeAddAttributes a2f
                (cabsTypeAddAttributes a1f restyp)
        | TPtr ((TFun _ as tf), ap) when not theMachine.msvcMode ->
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
          TFun _ -> cabsTypeAddAttributes af restyp
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
      let lo =
        match len.expr_node with
          | A.NOTHING -> None
          | _ -> 
	    try
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
		    else if Integer.equal i Integer.zero then
		      begin 
			Kernel.warning ~once:true ~source:(fst len'.eloc)
			  "Length of array is zero. This GCC extension is unsupported. Assuming length is 1.";
			raise (ChangeSize (Cil.zero ~loc:len'.eloc))
		      end
		  | _ ->
		    if isConstant cst then
                (* e.g., there may be a float constant involved.
                 * We'll leave it to the user to ensure the length is
                 * non-negative, etc.*)
                      Kernel.warning ~current:true
			"Unable to do constant-folding on array length %a. \
 Some CIL operations on this array may fail."
			Cil_printer.pp_exp cst
		    else
                      Kernel.error ~once:true ~current:true
			"Length of array is not a constant: %a"
			Cil_printer.pp_exp cst)
              end;
              Some len'
	    with ChangeSize fixed_len -> Some fixed_len
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
        if args != [] && theMachine.msvcMode = not isva then begin
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
            None -> []
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
          [] -> ()
        | a :: args' ->
          (match unrollType a.vtype with
            TArray(bt,lo,_,attr) ->
              (* Note that for multi-dimensional arrays we strip off only
                 the first TArray and leave bt alone. *)
              let real_type = turnArrayIntoPointer bt lo attr in
              Cil.update_var_type a real_type
          | TFun _ -> Cil.update_var_type a (TPtr(a.vtype, []))
          | TComp (_, _,_) -> begin
            match isTransparentUnion a.vtype with
              None ->  ()
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
          None -> None
        | Some argl ->
          fixupArgumentTypes 0 argl;
          Some (List.map (fun a -> (a.vname, a.vtype, a.vattr)) argl)
      in
      let tres =
        match unrollType bt with
          TArray(t,lo,_,attr) -> turnArrayIntoPointer t lo attr
        | _ -> bt
      in
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
    (* Try to compile the expression to a constant *)
    let (_, se, e', _) =
      doExp (ghost_local_env ghost) true lo (AExp (Some intType)) in
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
    None -> None
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
  let doFieldGroup ((s: A.spec_elem list),
		    (nl: (A.name * A.expression option) list)) =
    (* Do the specifiers exactly once *)
    let sugg = match nl with
	[] -> ""
      | ((n, _, _, _), _) :: _ -> n
    in
    let bt, sto, inl, attrs = doSpecList ghost sugg s in
    (* Do the fields *)
    let makeFieldInfo
        (((n,ndt,a,cloc) : A.name), (widtho : A.expression option))
	: fieldinfo =
      if sto <> NoStorage || inl then
        Kernel.error ~once:true ~current:true "Storage or inline not allowed for fields";
      let ftype, nattr =
        doType ghost false (AttrName false) bt (A.PARENTYPE(attrs, ndt, a)) in
      (* check for fields whose type is an undefined struct.  This rules
         out circularity:
         struct C1 { struct C2 c2; };          //This line is now an error.
         struct C2 { struct C1 c1; int dummy; };
       *)
      (match unrollType ftype with
      | TComp (ci',_,_) when not ci'.cdefined ->
	Kernel.error ~once:true ~current:true "Type of field %s is an undefined struct" n
      | _ -> ());
      let width, ftype =
        match widtho with
	  None -> None, ftype
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
	    TComp _ -> begin
	      incr anonCompFieldNameId;
	      anonCompFieldName ^ (string_of_int !anonCompFieldNameId)
	    end
	  | _ -> n
        end else
          n
      in
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
    List.map makeFieldInfo nl
  in

  (* Do regular fields first. *)
  let flds =
    List.filter (function FIELD _ -> true | TYPE_ANNOT _ -> false) nglist in
  let flds =
    List.map (function FIELD (f,g) -> (f,g) | _ -> assert false) flds in
  let flds = List.concat (List.map doFieldGroup flds) in

  if comp.cfields <> [] then begin
    (* This appears to be a multiply defined structure. This can happen from
     * a construct like "typedef struct foo { ... } A, B;". This is dangerous
     * because at the time B is processed some forward references in { ... }
     * appear as backward references, which could lead to circularity in
     * the type structure. We do a thourough check and then we reuse the type
     * for A *)
    if List.length comp.cfields <> List.length flds
      || (List.exists2 (fun f1 f2 -> not (Cil_datatype.Typ.equal f1.ftype f2.ftype))
            comp.cfields flds)
    then
      Kernel.error ~once:true ~current:true
	"%s seems to be multiply defined" (compFullName comp)
  end else
    comp.cfields <- flds;

  (*  ignore (E.log "makeComp: %s: %a\n" comp.cname d_attrlist a); *)
  comp.cattr <- add_packing_attributes comp a;
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
      TComp (c, _, _), A.SINGLE_INIT _ when not c.cstruct ->
        A.COMPOUND_INIT [(A.INFIELD_INIT ("___matching_field",
                                          A.NEXT_INIT),
                          ie)]
    | _, _ -> ie
  in
  (* Maybe specs contains an unnamed composite. Replace with the name so that
   * when we do again the specs we get the right name  *)
  let specs1 =
    match typ with
      TComp (ci, _, _) ->
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
  | Const (CChr i) -> new_exp ~loc (Const(charConstToInt i))

  (* other Const expressions are not ok *)
  | Const _ ->
    Kernel.fatal ~current:true "Expected integer constant and got %a" 
      Cil_printer.pp_exp e

  (* now, anything else that 'doExp true' returned is ok (provided
     that it didn't yield side effects); this includes, in particular,
     the various sizeof and alignof expression kinds *)
  | _ -> e

(* this is like 'isIntConstExp', but retrieves the actual integer
 * the expression denotes; I have not extended it to work with
 * sizeof/alignof since (for CCured) we can't const-eval those,
 * and it's not clear whether they can be bitfield width specifiers
 * anyway (since that's where this function is used)
 * -- VP 2006-12-20: C99 explicitly says so (par. 6.6.6)
 *)
and isIntegerConstant ghost (aexp) : int option =
  match doExp (ghost_local_env ghost) true aexp (AExp None) with
    (_, c, e, _) when isEmpty c -> begin
      match isInteger (Cil.constFold true e) with
        Some i64 -> Some (Integer.to_int i64)
      | _ -> None
    end
  | _ -> None

(* Process an expression and in the process do some type checking,
 * extract the effects as separate statements.
 * doExp returns the following 4-uple:
 * - a list of read accesses performed for the evaluation of the expression
 * - a chunk representing side-effects occuring during evaluation
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
      ADrop
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
        Lval(lv') when lv == lv' ->
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
           (mkStmtOneInstr ~ghost (Set(lv, e'', CurrentLoc.get ())),
            writes,writes,
            List.filter (fun x -> not (Cil.compareLval x lv)) r @ reads),
         e'', t'')

    end
  in
  let result =
    try
      match e.expr_node with
      | A.PAREN _ -> Kernel.fatal ~current:true "stripParen"
      | A.NOTHING when what = ADrop ->
	finishExp [] (unspecified_chunk empty) (integer ~loc 0) intType
      | A.NOTHING ->
	let res = new_exp ~loc (Const(CStr "exp_nothing")) in
	finishExp [] (unspecified_chunk empty) res (typeOf res)
      (* Do the potential lvalues first *)
      | A.VARIABLE n -> begin
	(* Look up in the environment *)
	try
	  let envdata = H.find env n in
	  match envdata with
	    EnvVar vi, _ ->
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
		(new_exp ~loc (Lval lval)) vi.vtype
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
	  doExp local_env false e1 (AExp None) in
	let (r2,se2, e2', t2) =
	  doExp local_env false e2 (AExp None) in
	let se = se1 @@ (se2, ghost) in
	let (e1'', t1, e2'', tresult) =
	  (* Either e1 or e2 can be the pointer *)
	  match unrollType t1, unrollType t2 with
	    TPtr(t1e,_), (TInt _|TEnum _) -> e1', t1, e2', t1e
	  | (TInt _|TEnum _), TPtr(t2e,_) -> e2', t2, e1', t2e
	  | _ ->
	    Kernel.fatal ~current:true
	      "Expecting a pointer type in index:@\n t1=%a@\nt2=%a"
	      Cil_printer.pp_typ t1 Cil_printer.pp_typ t2
	in
	(* We have to distinguish the construction based on the type of e1'' *)
	let res =
	  match e1''.enode with
	    StartOf array -> (* A real array indexing operation *)
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
	finishExp reads se (new_exp ~loc (Lval res)) tresult
      end
      | A.UNARY (A.MEMOF, e) ->
	if asconst then
	  Kernel.warning ~current:true "MEMOF in constant";
	let (r,se, e', t) = doExp local_env false e (AExp None) in
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
	finishExp reads se (new_exp ~loc (Lval res)) tresult

      (* e.str = (& e + off(str)). If e = (be + beoff) then e.str = (be
       * + beoff + off(str))  *)
      | A.MEMBEROF (e, str) ->
	(* member of is actually allowed if we only take the address *)
	(* if isconst then Cil.error "MEMBEROF in constant";  *)
	let (r,se, e', t') = doExp local_env false e (AExp None) in
	let lv =
	  match e'.enode with
	    Lval x -> x
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
	    TComp (comp, _, _) -> findField str comp.cfields
	  | _ ->
	    Kernel.fatal ~current:true "expecting a struct with field %s" str
	in
	let lv' = addOffsetLval field_offset lv in
	let field_type = typeOf (dummy_exp (Lval lv')) in
	let reads =
	  if Lval.Set.mem lv' local_env.authorized_reads
	  then r
	  else lv':: r
	in
	finishExp reads se (new_exp ~loc (Lval lv')) field_type

		 (* e->str = * (e + off(str)) *)
      | A.MEMBEROFPTR (e, str) ->
	if asconst then Kernel.warning ~current:true "MEMBEROFPTR in constant";
	let (r,se, e', t') = doExp local_env false e (AExp None) in
	let pointedt = match unrollType t' with
	  | TPtr(t1, _) -> t1
	  | TArray(t1,_,_,_) -> t1
	  | _ -> Kernel.fatal ~current:true "expecting a pointer to a struct"
	in
	let field_offset = match unrollType pointedt with
	  | TComp (comp, _, _) -> findField str comp.cfields
	  | x ->
	    Kernel.fatal ~current:true
	      "expecting a struct with field %s. Found %a. t1 is %a"
	      str Cil_printer.pp_typ x Cil_printer.pp_typ t'
	in
	let lv' = mkMem e' field_offset in
	let field_type = typeOf (dummy_exp (Lval lv')) in
	let reads =
	  if Lval.Set.mem lv' local_env.authorized_reads
	  then r
	  else lv' :: r
	in
	finishExp reads se (new_exp ~loc (Lval lv')) field_type

      | A.CONSTANT ct -> begin
	let hasSuffix str =
	  let l = String.length str in
	  fun s ->
	    let ls = String.length s in
	    l >= ls && s = String.uppercase (String.sub str (l - ls) ls)
	in
	match ct with
	  A.CONST_INT str -> begin
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
		     (* Maybe we burried __FUNCTION__ in there *)
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
	  let result = kinteger64 ~loc theMachine.wcharKind
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
	      if basefloat.f_lower <> basefloat.f_upper 
		&& Kernel.WarnDecimalFloat.get() <> "none"
	      then
		let msg =
		  if Kernel.WarnDecimalFloat.get() = "once"
		  then begin
		    Kernel.WarnDecimalFloat.set "none";
		    ". See documentation for option " ^ Kernel.WarnDecimalFloat.name
		  end
		  else (* all *) ""
		in
		Kernel.warning ~current:true
		  "Floating-point constant %s is not represented exactly. Will use %a%s"
		  str (Floating_point.pretty_normal ~use_hex:true) basefloat.f_nearest
		  msg ;
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

      (* Intercept the sizeof("string") *)
      | A.EXPR_SIZEOF ({ expr_node = A.CONSTANT (A.CONST_STRING _)} as e) ->
	begin
	  (* Process the string first *)
	  match doExp local_env asconst e (AExp None) with
	    _, _, {enode = Const(CStr s)}, _ ->
	      finishExp [] (unspecified_chunk empty)
		(new_exp ~loc (SizeOfStr s))
		theMachine.typeOfSizeOf
	  | _ -> Kernel.abort ~current:true "cabs2cil: sizeOfStr"
	end

      | A.EXPR_SIZEOF e ->
	(* Allow non-constants in sizeof *)
	(* Do not convert arrays and functions into pointers. *)
	let (_, se, e', _) =
	  doExp local_env false e AExpLeaveArrayFun in
        (*
          ignore (E.log "sizeof: %a e'=%a, t=%a\n"
          Cil_printer.pp_location !currentLoc Cil_printer.pp_exp e' Cil_printer.pp_typ t);
        *)
        (* !!!! The book says that the expression is not evaluated, so we
         * drop the potential side-effects *)
        let scope_chunk =
          if isNotEmpty se then begin
            Kernel.feedback ~once:true ~current:true
	      "Dropping side-effect in sizeof. \
               Nothing to worry, this is by the book.";
            IgnoreSideEffectHook.apply (e, e');
            let vars =
              List.filter (fun x -> Cil.appears_in_expr x e') se.locals
            in
            List.fold_left local_var_chunk empty vars
          end else empty
        in
        let size =
          match e'.enode with (* If we are taking the sizeof an
                               * array we must drop the StartOf  *)
            StartOf(lv) ->
              new_exp ~loc (SizeOfE (new_exp ~loc:e'.eloc(Lval(lv))))
          (* Maybe we are taking the sizeof a variable-sized array *)
          | Lval (Var vi, NoOffset) -> begin
              try
                IH.find varSizeArrays vi.vid
              with Not_found -> new_exp ~loc (SizeOfE e')
            end
          | _ -> new_exp ~loc (SizeOfE e')
        in
        finishExp [] scope_chunk size theMachine.typeOfSizeOf

    | A.TYPE_ALIGNOF (bt, dt) ->
        let typ = doOnlyType local_env.is_ghost bt dt in
        finishExp [] (unspecified_chunk empty) (new_exp ~loc (AlignOf(typ)))
	  theMachine.typeOfSizeOf

    | A.EXPR_ALIGNOF e ->
        let (_, se, e', _) =
          doExp local_env false e AExpLeaveArrayFun in
        (* !!!! The book says that the expression is not evaluated, so we
         * drop the potential side-effects *)
        if isNotEmpty se then begin
          Kernel.warning ~current:true
	    "Warning: Dropping side-effect in sizeof";
          IgnoreSideEffectHook.apply (e, e')
        end;
        let e'' =
          match e'.enode with (* If we are taking the alignof an
                               * array we must drop the StartOf  *)
            StartOf(lv) -> new_exp ~loc:e'.eloc (Lval(lv))

          | _ -> e'
        in
        finishExp [] (unspecified_chunk empty) (new_exp ~loc (AlignOfE(e'')))
	  theMachine.typeOfSizeOf

    | A.CAST ((specs, dt), ie) ->
        let s', dt', ie' = preprocessCast local_env.is_ghost specs dt ie in
        (* We know now that we can do s' and dt' many times *)
        let typ = doOnlyType local_env.is_ghost s' dt' in
        let what' =
          match what with
            AExp (Some _) -> AExp (Some typ)
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
            A.SINGLE_INIT e ->
              doExp local_env asconst e what', true

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
              let r, se, e', t' = doExp local_env asconst v what' in
              (* If typ is an array then the doExp above has already added a
               * StartOf. We must undo that now so that it is done once by
               * the finishExp at the end of this case *)
              let e2, t2 =
                match unrollType typ, e'.enode with
                  TArray _, StartOf lv -> new_exp ~loc (Lval lv), typ
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
            TVoid _ when what' = ADrop -> (t', e') (* strange GNU thing *)
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
        let (r, se, e', t) = doExp local_env asconst e (AExp None) in
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
        let (r, se, e', t) = doExp local_env asconst e (AExp None) in
        if isIntegralType t then
          let tres = integralPromotion t in
          let e'' = new_exp ~loc (UnOp(BNot, makeCastT e' t tres, tres)) in
          finishExp r se e'' tres
        else
          Kernel.fatal ~current:true "Unary ~ on a non-integral type"

    | A.UNARY(A.PLUS, e) -> doExp local_env asconst e what

    | A.UNARY(A.ADDROF, e) -> begin
        match e.expr_node with
          A.COMMA el -> (* GCC extension *)
            doExp local_env false
              { e with expr_node =
                  A.COMMA (replaceLastInList el
                             (fun e ->
                                { e with expr_node = A.UNARY(A.ADDROF, e)}))
              }
              what
        | A.QUESTION (e1, e2, e3) -> (* GCC extension *)
            doExp local_env false
              { e with expr_node =
                  A.QUESTION (e1,
                              { e2 with expr_node = A.UNARY(A.ADDROF, e2)},
                              { e3 with expr_node = A.UNARY(A.ADDROF, e3)})}
              what
        | A.PAREN e1 ->
            doExp local_env false
              { e with expr_node = A.UNARY(A.ADDROF, e1)} what
        | A.VARIABLE s when
            isOldStyleVarArgName s
            && (match !currentFunctionFDEC.svar.vtype with
                  TFun(_, _, true, _) -> true | _ -> false) ->
            (* We are in an old-style variable argument function and we are
             * taking the address of the argument that was removed while
             * processing the function type. We compute the address based on
             * the address of the last real argument *)
            if theMachine.msvcMode then begin
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
            let (r, se, e', t) = doExp local_env false e (AExp None) in
            (* ignore (E.log "ADDROF on %a : %a\n" Cil_printer.pp_exp e'
               Cil_printer.pp_typ t); *)
            match e'.enode with
              (Lval x | CastE(_, {enode = Lval x})) ->
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
                finishExp reads se (mkAddrOfAndMark loc x) (TPtr(t, []))

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
      end
    | A.UNARY((A.PREINCR|A.PREDECR) as uop, e) -> begin
        match e.expr_node with
          A.COMMA el -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.COMMA
                    (replaceLastInList el
                       (fun e -> cabs_exp e.expr_loc (A.UNARY(uop, e))))))
              what
        | A.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.QUESTION
                    (e1,
                     cabs_exp e2q.expr_loc (A.UNARY(uop, e2q)),
                     cabs_exp e3q.expr_loc (A.UNARY(uop, e3q)))))
              what
        | A.PAREN e1 ->
            doExp local_env asconst (cabs_exp loc (A.UNARY(uop, e1))) what
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
               A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ |
                   A.CAST _ (* A GCC extension *)) -> begin
            let uop' = if uop = A.PREINCR then PlusA else MinusA in
            if asconst then
              Kernel.warning ~current:true "PREINCR or PREDECR in constant";
            let (r, se, e', t) = doExp local_env false e (AExp None) in
            let lv =
              match e'.enode with
                Lval x -> x
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
                 (mkStmtOneInstr ~ghost:local_env.is_ghost
                    (Set(lv, makeCastT result tresult t,
                         CurrentLoc.get ())),[],[lv],r'))
              e'
              t
          end
        | _ ->
	  Kernel.fatal ~current:true "Unexpected operand for prefix -- or ++"
      end

    | A.UNARY((A.POSINCR|A.POSDECR) as uop, e) -> begin
        match e.expr_node with
          A.COMMA el -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.COMMA
                    (replaceLastInList el
                       (fun e -> cabs_exp e.expr_loc (A.UNARY(uop, e))))))
              what
        | A.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.QUESTION
                    (e1,
                     cabs_exp e2q.expr_loc (A.UNARY(uop, e2q)),
                     cabs_exp e3q.expr_loc (A.UNARY(uop, e3q)))))
              what
        | A.PAREN e1 ->
            doExp local_env asconst
              (cabs_exp e1.expr_loc (A.UNARY(uop,e1))) what
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
               A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ |
                   A.CAST _ (* A GCC extension *) ) -> begin
            if asconst then
              Kernel.warning ~current:true "POSTINCR or POSTDECR in constant";
            (* If we do not drop the result then we must save the value *)
            let uop' = if uop = A.POSINCR then PlusA else MinusA in
            let (r,se, e', t) = doExp local_env false e (AExp None) in
            let lv =
              match e'.enode with
                Lval x -> x
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
                  Pretty_utils.sfprintf "%a%s"
                    Cil_descriptive_printer.pp_exp  e'
                    (if uop = A.POSINCR then "++" else "--") in
                let tmp = newTempVar descr true t in
                ([var tmp],
                 local_var_chunk se' tmp +++
                   (mkStmtOneInstr ~ghost:local_env.is_ghost
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
                 (mkStmtOneInstr ~ghost:local_env.is_ghost
                    (Set(lv,
                         makeCastT opresult tresult (typeOfLval lv),
                         CurrentLoc.get ())),
                  [],[lv], r'))
              result
              t
          end
        | _ ->
	  Kernel.fatal ~current:true "Unexpected operand for suffix ++ or --"
      end

    | A.BINARY(A.ASSIGN, e1, e2) -> begin
        match e1.expr_node with
          A.COMMA el -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.COMMA
                    (replaceLastInList el
                       (fun e -> cabs_exp e.expr_loc
                          (A.BINARY(A.ASSIGN, e, e2))))))
              what
        | A.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
            (*TODO: prevent duplication of e2: this is incorrect
              if it contains labels *)
(*            let r2,se2,e2,t2 = doExp authorized_reads ghost asconst e2 in*)
            doExp local_env asconst
              (cabs_exp loc
                 (A.QUESTION
                    (e1,
                     cabs_exp e2q.expr_loc (A.BINARY(A.ASSIGN, e2q, e2)),
                     cabs_exp e3q.expr_loc (A.BINARY(A.ASSIGN, e3q, e2)))))
              what
        | A.CAST (t, A.SINGLE_INIT e) -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.CAST (t,
                          A.SINGLE_INIT
                            (cabs_exp e.expr_loc
                               (A.BINARY
                                  (A.ASSIGN, e,
                                   (cabs_exp e2.expr_loc
                                      (A.CAST (t, A.SINGLE_INIT e2)))))))))
              what
        | A.PAREN e1 ->
            doExp local_env asconst
              (cabs_exp loc (A.BINARY(A.ASSIGN,e1,e2))) what
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
               A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ ) -> begin
          if asconst then Kernel.warning ~current:true "ASSIGN in constant";
          let se0 = unspecified_chunk empty in
          let (r1,se1, e1', lvt) = doExp local_env false e1 (AExp None) in
          let lv =
            match e1'.enode with
            | Lval x -> x
            | _ ->
	      Kernel.fatal ~current:true
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
              (ADrop|AType), _ -> false
            | _, (Mem e, off) ->
		not (isConstant e) || not (isConstantOffset off)
            | _, (Var _, off) -> not (isConstantOffset off)
          in
            let r1, tmplv, se3 =
              if needsTemp then
                let descr = 
		  Pretty_utils.sfprintf "%a" Cil_descriptive_printer.pp_lval lv
		in
                let tmp = newTempVar descr true lvt in
                let chunk =
                  i2c
                    (mkStmtOneInstr ~ghost:local_env.is_ghost
                       (Set(lv, new_exp ~loc:e1'.eloc (Lval(var tmp)), loc)),
                    [lv],[lv], r1')
                in
                ([],var tmp, local_var_chunk chunk tmp)
              else r1',lv, empty
            in
            let (r2,se2, _, _) =
              doExp local_env false e2 (ASet(not needsTemp,tmplv, r1, lvt))
            in
            let (@@) s1 s2 = s1 @@ (s2, ghost) in
            (* Format.eprintf "chunk for assigns is %a@." d_chunk se2; *)
            (* r1 is read in the assignment part itself *)
            finishExp r2  ((empty @@ ((se0 @@ se1') @@ se2)) @@ se3)
              (new_exp ~loc (Lval tmplv)) lvt
          end
        | _ -> Kernel.fatal ~current:true "Invalid left operand for ASSIGN"
      end
    | A.BINARY((A.ADD|A.SUB|A.MUL|A.DIV|A.MOD|A.BAND|A.BOR|A.XOR|
                    A.SHL|A.SHR|A.EQ|A.NE|A.LT|A.GT|A.GE|A.LE) as bop,
               e1, e2) ->
        let se0 = unspecified_chunk empty in
        let bop' = convBinOp bop in
        let (r1,se1, e1', t1) =
          doExp local_env asconst e1 (AExp None) in
        let (r2,se2, e2', t2) =
          doExp local_env asconst e2 (AExp None) in
        let tresult, result = doBinOp loc bop' e1' t1 e2' t2 in
        let (@@) s1 s2 = s1 @@ (s2, ghost) in
        finishExp (r1 @ r2) ((se0 @@ se1) @@ se2) result tresult

    (* assignment operators *)
    | A.BINARY((A.ADD_ASSIGN|A.SUB_ASSIGN|A.MUL_ASSIGN|A.DIV_ASSIGN|
                    A.MOD_ASSIGN|A.BAND_ASSIGN|A.BOR_ASSIGN|A.SHL_ASSIGN|
                        A.SHR_ASSIGN|A.XOR_ASSIGN) as bop, e1, e2) -> begin
        let se0 = unspecified_chunk empty in
        match e1.expr_node with
          A.COMMA el -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.COMMA
                    (replaceLastInList el
                       (fun e -> cabs_exp e.expr_loc (A.BINARY(bop, e, e2))))))
              what
        | A.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
            doExp local_env asconst
              (cabs_exp loc
                 (A.QUESTION
                    (e1,
                     cabs_exp e2q.expr_loc (A.BINARY(bop, e2q, e2)),
                     cabs_exp e3q.expr_loc (A.BINARY(bop, e3q, e2)))))
              what
        | A.PAREN e1 ->
            doExp local_env asconst (cabs_exp loc (A.BINARY(bop,e1,e2))) what
        | (A.VARIABLE _ | A.UNARY (A.MEMOF, _) | (* Regular lvalues *)
               A.INDEX _ | A.MEMBEROF _ | A.MEMBEROFPTR _ |
                   A.CAST _ (* GCC extension *) ) -> begin
            if asconst then
	      Kernel.warning ~current:true "op_ASSIGN in constant";
            let bop' = match bop with
              A.ADD_ASSIGN -> PlusA
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
            let (r1,se1, e1', t1) = doExp local_env false e1 (AExp None) in
            let lv1 =
              match e1'.enode with
                Lval x -> x
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
                    (mkStmtOneInstr ~ghost:local_env.is_ghost
                       (Set(lv1, result', loc)),
                     [lv1],[lv1], r1' @ r2)))
              e1'
              t1
          end
        | _ ->
	  Kernel.fatal ~current:true
	    "Unexpected left operand for assignment with arith"
      end

    | A.BINARY((A.AND|A.OR), _, _) | A.UNARY(A.NOT, _) -> begin
        let ce = doCondExp local_env asconst e in
        (* We must normalize the result to 0 or 1 *)
        match ce with
          CEExp (se, ({enode = Const _;eloc=loc} as c)) ->
            finishExp [] se
              (if isConstTrue c then one ~loc else zero ~loc)
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
              compileCondExp ~ghost false ce
                (empty +++
                   (mkStmtOneInstr ~ghost
                      (Set(var tmp, integer ~loc 1,loc)),[],[],[]))
                (empty +++
                   (mkStmtOneInstr ~ghost
                      (Set(var tmp, integer ~loc 0,loc)),[],[],[]))
            in
            finishExp []
              (local_var_chunk condChunk tmp)
              (new_exp ~loc (Lval (var tmp)))
              intType
      end

    | A.CALL(f, args) ->
      let (rf,sf, f', ft') =
        match f.expr_node with
            (* Treat the VARIABLE case separate because we might be calling a
             * function that does not have a prototype. In that case assume it
             * takes INTs as arguments  *)
            A.VARIABLE n -> begin
              try
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
                    (makeGlobalVar ~generated:false n ftype) in
                (* Make it EXTERN *)
                proto.vstorage <- Extern;
                IH.add noProtoFunctions proto.vid true;
                proto.vdecl <- f.expr_loc;
                ImplicitPrototypeHook.apply proto;
                (* Add it to the file as well *)
                cabsPushGlobal
		  (GVarDecl (empty_funspec (),proto, f.expr_loc));
                ([var proto],unspecified_chunk empty,
                 new_exp ~loc:f.expr_loc (Lval(var proto)), ftype)
              end
            end
          | _ -> doExp local_env false f (AExp None)
      in
        (* Get the result type and the argument types *)
      let (resType, argTypes, isvar, f'',attrs) =
        match unrollType ft' with
            TFun(rt,at,isvar,attrs) -> (rt,at,isvar,f',attrs)
          | TPtr (t, _) -> begin
            match unrollType t with
                TFun(rt,at,isvar,_) -> (* Make the function pointer
                                        * explicit  *)
                  let f'' =
                    match f'.enode with
			AddrOf lv -> new_exp ~loc:f'.eloc (Lval(lv))
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
        (* Drop certain qualifiers from the result type *)
      let resType' = typeRemoveAttributes ["warn_unused_result"] resType in
        (* Before we do the arguments we try to intercept a few builtins. For
         * these we have defined then with a different type, so we do not
         * want to give warnings. We'll just leave the arguments of these
         * functions alone*)
      let isSpecialBuiltin =
        match f''.enode with
            Lval (Var fv, NoOffset) -> Cil.is_special_builtin fv.vname
          | _ -> false
      in
      
      let force_rlarg_eval = Kernel.ForceRLArgEval.get () in
        (** If [force_rlarg_eval], make sure we evaluate args right-to-left. *)
      let force_right_to_left_evaluation (r,c, e, t) =
	  (* If chunk is empty then it is not already evaluated *)
	  (* constants don't need to be pulled out *)
        if force_rlarg_eval && (not (isConstant e)) && not isSpecialBuiltin
        then
	    (* create a temporary *)
	  let tmp =
            newTempVar
              (Pretty_utils.sfprintf "%a" Cil_descriptive_printer.pp_exp e)
	      true
	      t
          in
          let c = local_var_chunk c tmp in
	    (* create an instruction to give the e to the temporary *)
	  let i = mkStmtOneInstr ~ghost:local_env.is_ghost
            (Set(var tmp, e, loc))
          in
	    (* add the instruction to the chunk *)
	    (* change the expression to be the temporary *)
	  (c +++ (i,[],[],[]), new_exp ~loc (Lval(var tmp)), t)
        else
	  (add_reads loc r c, e, t)
      in
      let init_chunk =
        if force_rlarg_eval then empty else unspecified_chunk empty
      in
        (* Do the arguments. In REVERSE order !!! Both GCC and MSVC do this *)
      let rec loopArgs = function
        | ([], []) -> (init_chunk, [])
	  
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
          let (sa, a', att) = force_right_to_left_evaluation
                (doExp local_env false a (AExp None)) in
          let (_, a'') = castTo att at a' in
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
                force_right_to_left_evaluation
                  (doExp local_env false a (AExp None))
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
          CastE (_, e) -> dropCasts e
        | _ -> e
        in
        (* Get the name of the last formal *)
        let getNameLastFormal () : string =
          match !currentFunctionFDEC.svar.vtype with
            TFun(_, Some args, true, _) -> begin
              match List.rev args with
                (last_par_name, _, _) :: _ -> last_par_name
              | _ -> ""
            end
          | _ -> ""
        in

        (* Try to intercept some builtins *)
        (match (!pf).enode with
          | Lval(Var fv, NoOffset) -> begin
            if fv.vname = "__builtin_va_arg" then begin
               match !pargs with
                 marker :: ({enode = SizeOf resTyp} as size) :: _ -> begin
                   (* Make a variable of the desired type *)
                   let is_real, destlv, r, destlvtyp =
                     match !pwhat with
                       ASet (is_real,lv, r, lvt) -> is_real, lv, r, lvt
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
             end else if fv.vname = "__builtin_stdarg_start" ||
               fv.vname = "__builtin_va_start" then begin
                 match !pargs with
                   marker :: last :: [] -> begin
                     let isOk =
                       match (dropCasts last).enode with
                         Lval (Var lastv, NoOffset) ->
                           lastv.vname = getNameLastFormal ()
                       | _ -> false
                     in
                     if not isOk then
                       Kernel.warning ~current:true
                         "The second argument in call to %s \
                           should be the last formal argument" fv.vname;

                     (* Check that "lastv" is indeed the last variable in the
                      * prototype and then drop it *)
                     pargs := [ marker ]
                   end
                 | _ ->
                   Kernel.warning ~current:true "Invalid call to %s\n" fv.vname;

               (* We have to turn uses of __builtin_varargs_start into uses
                * of __builtin_stdarg_start (because we have dropped the
                * __builtin_va_alist argument from this function) *)

               end else if fv.vname = "__builtin_varargs_start" then begin
                 (* Lookup the prototype for the replacement *)
                 let v, _  =
                   try lookupGlobalVar "__builtin_stdarg_start"
                   with Not_found ->
                     Kernel.abort ~current:true
		       "Cannot find __builtin_stdarg_start to replace %s"
		       fv.vname
                 in
                 pf := new_exp ~loc (Lval (var v))
               end else if fv.vname = "__builtin_next_arg" then begin
                 match !pargs with
                   last :: [] -> begin
                     let isOk =
                       match (dropCasts last).enode with
                         Lval (Var lastv, NoOffset) ->
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
               end else if fv.vname = "__builtin_va_arg_pack" then begin 
		 (match !pargs with [  ] -> begin 
                   piscall := false; 
		   pres := new_exp ~loc:e.expr_loc (SizeOfE !pf);
		   prestype := theMachine.typeOfSizeOf
                 end
		   | _ -> 
                     Kernel.warning ~current:true 
		       "Invalid call to builtin_va_arg_pack");
               end else if fv.vname = "__builtin_constant_p" then begin
                 (* Drop the side-effects *)
                 prechunk := empty;
		 
                 (* Constant-fold the argument and see if it is a constant *)
                 (match !pargs with
                    [ arg ] -> begin
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
		 else if fv.vname = "__builtin_types_compatible_p" then begin
		   (* Constant-fold the argument and see if it is a constant *)
		   (match !pargs with 
		       [ {enode = SizeOf t1}; {enode = SizeOf t2}] -> begin
			 (* Drop the side-effects *)
			 prechunk := empty;
			 piscall := false; 
			 let compatible =
			   try ignore(combineTypes CombineOther t1 t2); true
			   with Failure _ -> false
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
		 else if asconst then 
		   (* last special case: we cannot allow a function call
		      at this point.*)
		   begin
		     piscall := false;
		     Kernel.warning
		       ~once:true
		       ~current:true 
		       "Call to %a in constant. Ignoring this call and \
                        returning 0." 
	    Cprint.print_expression f;
		   end
(*TODO: support those nice builtins.
  One needs to translate the code below from 
  Cil to frama-C. 
  
  else if fv.vname = "__builtin_choose_expr" then begin
  
	  (* Constant-fold the argument and see if it is a constant *)
  (match !pargs with 
  [ arg; e1; e2 ] -> begin 
  let constfolded = constFold true arg in
  match constfolded.enode with 
  | (Const _) -> 
  piscall := false; 
  if isZero constfolded then begin
          (* Keep only 3rd arg side effects *)
  prechunk := (fun _ -> sf @@ (List.nth sargsl 2));
  pres := e2;
  prestype := typeOf e2
	              end else begin
          (* Keep only 2nd arg side effects *)
  prechunk := (fun _ -> sf @@ (List.nth sargsl 1));
  pres := e1;
  prestype := typeOf e1
  end
  | _ -> ignore (warn "builtin_choose_expr expects a constant first argument")
  end
  | _ -> 
  Kernel.warning "Invalid call to builtin_choose_expr")
  end
*)
          end
	  | _ -> ());

        (* Now we must finish the call *)
        if !piscall then begin
          let addCall ?(is_real_var=true) calldest res t =
            let my_write =
              match calldest with
                None -> []
              | Some c when is_real_var -> [c]
              | Some _ -> []
            in
            prechunk :=
              (empty @@ (!prechunk, ghost)) +++
                (mkStmtOneInstr ~ghost:local_env.is_ghost
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
                Pretty_utils.sfprintf "%a(%a)"
                  Cil_descriptive_printer.pp_exp !pf
                  (Pretty_utils.pp_list ~sep:", " 
		     Cil_descriptive_printer.pp_exp) 
		  !pargs
              in
              let tmp = newTempVar descr false restype'' in
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
      let rec loop sofar = function
      [e] ->
        let (r, se, e', t') = doExp local_env false e what
        in (* Pass on the action *)
        (r, sofar @@ (se, ghost), e', t')
        | e :: rest ->
          let (_, se, _, _) = doExp local_env false e ADrop in
          loop (sofar @@ (se, ghost)) rest
        | [] -> Kernel.fatal ~current:true "empty COMMA expression"
      in
      loop empty el

    | A.QUESTION (e1, e2, e3) -> begin
        (* Compile the conditional expression *)
        let ghost = local_env.is_ghost in
        let ce1 = doCondExp local_env asconst e1 in
        let what' = match what with
          | ADrop -> ADrop
          | _ -> AExp None
        in
        (* Now we must find the type of both branches, in order to compute
         * the type of the result *)
        let r2, se2, e2'o (* is an option. None means use e1 *), t2 =
          match e2.expr_node with
            A.NOTHING -> begin (* The same as the type of e1 *)
              match ce1 with
                CEExp (_, e1') ->
                  [], unspecified_chunk empty, None, typeOf e1'
                    (* Do not promote to bool *)
              | _ -> [], unspecified_chunk empty, None, intType
            end
          | _ ->
              let r2, se2, e2', t2 = doExp local_env asconst e2 what' in
              r2, se2, Some e2', t2
        in
        (* Do e3 for real *)
        let r3, se3, e3', t3 = doExp local_env asconst e3 what' in
        (* Compute the type of the result. If we are about to drop it,
           it has type void anyway, especially in case of nested ternary
           ops (Cf bts 1503).
         *)
        let tresult = 
          if what = ADrop then Cil.voidType else conditionalConversion t2 t3
        in
        if not (isEmpty se2) then
          ConditionalSideEffectHook.apply (e,e2);
        if not (isEmpty se3) then
          ConditionalSideEffectHook.apply (e,e3);
        match ce1 with
          CEExp (se1, e1') when isConstFalse e1' && canDrop se2 ->
            finishExp r3 ((empty @@ (se1, ghost)) @@ (se3, ghost))
              (snd (castTo t3 tresult e3')) tresult
        | CEExp (se1, e1') when isConstTrue e1' && canDrop se3 ->
            begin
              match e2'o with
                None -> (* use e1' *)
                  finishExp r2
                    ((empty @@ (se1, ghost)) @@ (se2, ghost))
                    (snd (castTo t2 tresult e1')) tresult
              | Some e2' ->
                  finishExp r2
                    ((empty @@ (se1, ghost)) @@ (se2, ghost))
                    (snd (castTo t2 tresult e2')) tresult
            end
        | _ when what = ADrop ->
          (* We are not interested by the result, but might want to evaluate
             e2 and e3 if they are dangerous expressions. *)
          let res = Cil.new_exp ~loc (CastE(Cil.voidType,Cil.zero ~loc)) in
          (match e2'o with
            | None when is_dangerous e3' || not (isEmpty se3) ->
              let descr =
                Pretty_utils.sfprintf "%a" Cprint.print_expression e1
              in
              let tmp = newTempVar descr true tresult in
              let tmp_var = var tmp in
              let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
              let (r1, se1, _, _) =
                 doExp local_env asconst e1 (ASet(false, tmp_var, [], tresult))
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
                Cil.voidType
            | None ->
              (* we can drop e3, just keep e1 in case it is dangerous *)
              let (r1,se1,e1,_) = doExp local_env asconst e1 ADrop in
              let dangerous =
                if is_dangerous e1 then
                  keepPureExpr ~ghost e1 loc
                else skipChunk
              in
              finishExp (r1@r3) (se1 @@ (dangerous, ghost)) res Cil.voidType
            | Some e2'
                when is_dangerous e2' || is_dangerous e3'
                  || not (isEmpty se2) || not (isEmpty se3) ->
              (* we have to keep e1 in order to know which dangerous expression
                 is to be evaluated *)
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
              let cond = compileCondExp ~ghost false ce1 se2 se3 in
              finishExp (r2@r3) cond res Cil.voidType
            | Some _ -> (* we just keep e1 in case it is dangerous. everything
                           else can be dropped *)
              let (r1,se1,e1,_) = doExp local_env asconst e1 ADrop in
              let dangerous =
                if is_dangerous e1 then
                  keepPureExpr ~ghost e1 loc
                else skipChunk
              in
              finishExp (r1@r2@r3) (se1 @@ (dangerous, ghost)) res Cil.voidType)
        | _ -> (* Use a conditional *) begin
            match e2'o with
            | None -> (* has form "e1 ? : e3"  *)
              let descr = Pretty_utils.sfprintf "%a" Cprint.print_expression e1
              in
              let tmp = newTempVar descr true tresult in
              let tmp_var = var tmp in
              let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
              let (r1,se1, _, _) =
                doExp local_env asconst e1 (ASet(false, tmp_var, [], tresult))
              in
              let se1 = local_var_chunk se1 tmp in
              let r3,se3,_,_ =
                finishExp
                  ~newWhat:(ASet(false,tmp_var, [], tresult)) r3 se3 e3' t3
              in
              finishExp
                (r1@r3)
                ((empty @@ (se1, ghost)) @@
                    (ifChunk ~ghost tmp_lval loc skipChunk se3, ghost))
                tmp_lval
                tresult
            | Some e2' ->
                let is_real, lv, r, lvt, scope_chunk =
                  match what with
                  | ASet (is_real, lv, r, lvt) -> is_real, lv, r, lvt, empty
                  | _ ->
                    let descr =
                      Pretty_utils.sfprintf "%a?%a:%a"
                        Cprint.print_expression e1
                        Cil_descriptive_printer.pp_exp e2'
                        Cil_descriptive_printer.pp_exp e3'
                    in
                    let tmp = newTempVar descr true tresult in
                    false, var tmp, [], tresult, local_var_chunk empty tmp
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
                let cond = compileCondExp ~ghost false ce1 se2 se3 in
                finishExp
                  (r2@r3)
                  (scope_chunk @@ (cond, ghost))
                  (new_exp ~loc (Lval lv)) tresult
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
            ADrop -> (* We are dropping the result *)
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

        let se = doBody local_env b in

        (*Kernel.debug "Body inside expression: %a@." d_chunk se;*)

        gnu_body_result := old_gnu;
        match !data with
          None when isvoidbody ->
            finishExp [] se (zero ~loc:e.expr_loc) voidType
        | None ->
	  Kernel.abort ~current:true "Cannot find COMPUTATION in GNU.body"
        | Some (e, t) ->
            let se, e =
              match se.stmts with
                [ { skind = Block b},_, _, _, _ ] ->
                  let vi = newTempVar "GNU.body" true t in
                  b.bstmts <-
                    b.bstmts @
                    [Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
                       (Set (Cil.var vi, e,loc))];
                  (local_var_chunk se vi,Cil.new_exp ~loc (Lval (Cil.var vi)))
              | _ -> se,e
            in
            finishExp [] se e t
      end

    | A.LABELADDR l -> begin (* GCC's taking the address of a label *)
        let l = lookupLabel l in (* To support locallly declared labels *)
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

    | A.EXPR_PATTERN _ ->
      Kernel.abort ~current:true "EXPR_PATTERN in cabs2cil input"

    with _ when Cilmsg.had_errors () && continueOnError -> begin
      Kernel.error ~once:true ~current:true "ignoring expression";
      ([],
       i2c (mkStmtOneInstr ~ghost:local_env.is_ghost
              (dInstr
		 (Pretty_utils.sfprintf "booo_exp(%t)" Cil.pp_thisloc) loc),
            [],[],[]),
       integer ~loc 0, intType)
    end
  in
  (*let (_a,b,_c,_d) = result in
    Format.eprintf "doExp ~const:%b ~e:" asconst ;
    Cprint.print_expression e;
    Format.eprintf "@.";
    Format.eprintf "Got: chunk:'%a'@." d_chunk b;*)
  CurrentLoc.set oldLoc;
  result

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
    (* Keep the operator since it is arithemtic *)
    intType,
    optConstFoldBinOp loc false bop
      (makeCastT e1 t1 tres) (makeCastT e2 t2 tres) intType
  in
  let doIntegralArithmetic () =
    let tres = unrollType (arithmeticConversion t1 t2) in
    match tres with
      TInt _ ->
        tres,
        optConstFoldBinOp loc false bop
          (makeCastT e1 t1 tres) (makeCastT e2 t2 tres) tres
    | _ ->
      Kernel.fatal ~current:true "%a operator on a non-integer type" 
	Cil_printer.pp_binop bop
  in
  let pointerComparison e1 t1 e2 t2 =
    (* Cast both sides to an integer *)
    (* in Frama-C, do not add these non-standard useless casts *)
    let e1', e2' = if false && theMachine.insertImplicitCasts then
      let commontype = theMachine.upointType in
      (makeCastT e1 t1 commontype),
      (makeCastT e2 t2 commontype)
    else
      e1, e2
    in
    intType,
    optConstFoldBinOp loc false bop e1' e2' intType
  in
  let do_shift e1 t1 e2 t2 =
    match e1.enode with
        StartOf lv ->
          { e1 with enode = AddrOf (addOffsetLval (Index (e2,NoOffset)) lv) }
      | _ ->
        optConstFoldBinOp loc false PlusPI e1
          (makeCastT e2 t2 (integralPromotion t2)) t1
  in
  match bop with
    (Mult|Div) -> doArithmetic ()
  | (Mod|BAnd|BOr|BXor) -> doIntegralArithmetic ()
  | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result
                          * has the same type as the left hand side *)
      if theMachine.msvcMode then
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
      let commontype = t1 in
      intType,
      optConstFoldBinOp loc false MinusPP (makeCastT e1 t1 commontype)
        (makeCastT e2 t2 commontype) intType
  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
      pointerComparison e1 t1 e2 t2
  | (Eq|Ne) when isPointerType t1 && isZero e2 ->
      pointerComparison e1 t1 (makeCastT (zero ~loc)theMachine.upointType t1) t1
  | (Eq|Ne) when isPointerType t2 && isZero e1 ->
      pointerComparison (makeCastT (zero ~loc)theMachine.upointType t2) t2 e2 t2

  | (Eq|Ne) when isVariadicListType t1 && isZero e2 ->
    Kernel.debug ~level:3 "Comparison of va_list and zero";
    pointerComparison e1 t1 (makeCastT (zero ~loc)theMachine.upointType t1) t1
  | (Eq|Ne) when isVariadicListType t2 && isZero e1 ->
    Kernel.debug ~level:3 "Comparison of zero and va_list";
    pointerComparison (makeCastT (zero ~loc)theMachine.upointType t2) t2 e2 t2

  | (Eq|Ne|Le|Lt|Ge|Gt) when isPointerType t1 && isArithmeticType t2 ->
    Kernel.debug ~level:3 "Comparison of pointer and non-pointer";
    (* Cast both values to upointType *)
    doBinOp loc bop
      (makeCastT e1 t1 theMachine.upointType) theMachine.upointType
      (makeCastT e2 t2 theMachine.upointType) theMachine.upointType
  | (Eq|Ne|Le|Lt|Ge|Gt) when isArithmeticType t1 && isPointerType t2 ->
    Kernel.debug ~level:3 "Comparison of pointer and non-pointer";
    (* Cast both values to upointType *)
    doBinOp loc
      bop (makeCastT e1 t1 theMachine.upointType) theMachine.upointType
      (makeCastT e2 t2 theMachine.upointType) theMachine.upointType

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
      CEExp (c, e) -> CEExp ((empty @@ (c0, ghost)) @@ (c, ghost), e)
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
    A.BINARY (A.AND, e1, e2) -> begin
      let ce1 = doCondExp local_env asconst ?ctxt e1 in
      let ce2 = doCondExp local_env asconst ~ctxt:e e2 in
      let ce1 = remove_effects_ce ce1 in
      match ce1, ce2 with
        CEExp (se1, ({enode = Const _} as ci1)), _ ->
          if isConstTrue ci1 then addChunkBeforeCE se1 ce2
          else
            (* se2 might contain labels so we cannot always drop it *)
            if canDropCE ce2 then ce1 else CEAnd (ce1, ce2)
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
      let ce1 = doCondExp local_env asconst ?ctxt e1 in
      let ce2 = doCondExp local_env asconst ~ctxt:e e2 in
      let ce1 = remove_effects_ce ce1 in
      match ce1, ce2 with
        CEExp (se1, ({enode = Const(CInt64 _)} as ci1)), _ ->
          if isConstFalse ci1 then addChunkBeforeCE se1 ce2
          else
            (* se2 might contain labels so we cannot drop it *)
            if canDropCE ce2 then ce1 else CEOr (ce1, ce2)
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
      match doCondExp local_env asconst ?ctxt e1 with
        CEExp (se1, ({enode = Const _} as ci1)) ->
          if isConstFalse ci1 then
            CEExp (se1, one e1.expr_loc)
          else
            CEExp (se1, zero e1.expr_loc)
      | CEExp (se1, e) when isEmpty se1 ->
          let t = typeOf e in
          if not ((isPointerType t) || (isArithmeticType t))then
            Kernel.error ~once:true ~current:true "Bad operand to !";
          CEExp (empty, new_exp ~loc (UnOp(LNot, e, intType)))
      | ce1 -> CENot ce1
    end

  | _ ->
      let (r, se, e', t) = doExp local_env asconst e (AExp None) in
      (* No need to add reads here: we'll always have a sequence point,
         either because the expression is complete, or because of a logic
         operator. *)
      (match ctxt with
           None -> ()
         | Some _ when isEmpty se -> ()
         | Some orig ->
             ConditionalSideEffectHook.apply (orig,e));
      ignore (checkBool t e');
      Cabscond.bind e e' ;
      CEExp (add_reads e.expr_loc r se,
             if asconst || theMachine.lowerConstants then
               constFold asconst e'
             else e')
  in
  result

 (* If cabscond is true, then CIL-atomic expressions must be bound with Cabscond. *)

and compileCondExp ~ghost cabscond ce st sf =
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
      let st' = compileCondExp ~ghost cabscond ce2 st sf1 in
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
	(compileCondExp ~ghost cabscond ce1 st' sf')
        @@ gotostmt @@ sf2 @@ labstmt
      else
	let sf' = sf2 in
	compileCondExp ~ghost cabscond ce1 st' sf'

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
	let sf' = compileCondExp ~ghost cabscond ce2 st1 sf in
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
	(compileCondExp ~ghost cabscond ce1 st' sf')
        @@ gotostmt @@ st2 @@ labstmt
      else
	let st' = st1 in
	let sf' = compileCondExp ~ghost cabscond ce2 st2 sf in
        (*Format.eprintf
          "result:@\nchunk then:@\n  @[%a@]@\nchunk else:  @[%a@]@."
          d_chunk st d_chunk sf;*)
	compileCondExp ~ghost cabscond ce1 st' sf'

  | CENot ce1 -> compileCondExp ~ghost cabscond ce1 sf st

  | CEExp (se, e) -> begin
      match e.enode with
      | Const(CInt64(i,_,_))
          when (not (Integer.equal i Integer.zero)) && canDrop sf ->
        se @@ (st, ghost)
      | Const(CInt64(z,_,_))
          when (Integer.equal z Integer.zero) && canDrop st ->
        se @@ (sf, ghost)
      | _ -> (empty @@ (se, ghost)) @@ (ifChunk ~ghost e e.eloc st sf, ghost)
    end


(* A special case for conditionals *)
and doCondition ?info local_env (isconst: bool)
    (* If we are in constants, we do our best to eliminate the conditional *)
    (e: A.expression)
    (st: chunk)
    (sf: chunk) : chunk =
  let cabscond = match info with
  | Some (descr,loc) -> Cabscond.push_condition descr loc e
  | None -> false
  in
  if not cabscond && isEmpty st && isEmpty sf(*TODO: ignore attribute FRAMA_C_KEEP_BLOCK*) then
    let (_, se,_,_) = doExp local_env cabscond e ADrop in se
  else
    let ce = doCondExp local_env isconst e in
    if cabscond then Cabscond.pop_condition () ;
    let chunk = compileCondExp ~ghost:local_env.is_ghost cabscond ce st sf in
  chunk


and doPureExp local_env (e : A.expression) : exp =
  let (_,se, e', _) = doExp local_env true e (AExp None) in
  if isNotEmpty se then
    Kernel.error ~once:true ~current:true "%a has side-effects" Cprint.print_expression e;
  e'

and doFullExp local_env const e what =
  let (r, se,e,t) = doExp local_env const e what in
  let se' = add_reads e.eloc r se in
  (* there is a sequence point after a full exp *)
  empty @@ (se', local_env.is_ghost),e,t

and doInitializer local_env (vi: varinfo) (inite: A.init_expression)
    (* Return the accumulated chunk, the initializer and the new type (might be
     * different for arrays) *)
    : chunk * init * typ =

  (* Setup the pre-initializer *)
  let topPreInit = ref NoInitPre in
  Kernel.debug ~dkey:category_initializer
    "@\nStarting a new initializer for %s : %a@\n"
    vi.vname Cil_printer.pp_typ vi.vtype;
  let topSetupInit (o: offset) (e: exp) =
    Kernel.debug ~dkey:category_initializer " set %a := %a@\n"  
      Cil_printer.pp_lval (Var vi, o) Cil_printer.pp_exp e;
    let newinit = setOneInit !topPreInit o e in
    if newinit != !topPreInit then topPreInit := newinit
  in
  let acc, restl =
    let so = makeSubobj vi vi.vtype NoOffset in
    doInit local_env vi.vglob Extlib.nop topSetupInit so
      (unspecified_chunk empty) [ (A.NEXT_INIT, inite) ]
  in
  if restl <> [] then
    Kernel.warning ~current:true "Ignoring some initializers";
  (* sm: we used to do array-size fixups here, but they only worked
   * for toplevel array types; now, collectInitializer does the job,
   * including for nested array types *)
  let typ' = vi.vtype in
  Kernel.debug ~dkey:category_initializer
    "Collecting the initializer for %s@\n" vi.vname;
  let (init, typ'') = collectInitializer !topPreInit typ' in
  Kernel.debug ~dkey:category_initializer
    "Finished the initializer for %s@\n  init=%a@\n  typ=%a@\n  acc=%a@\n"
    vi.vname Cil_printer.pp_init init Cil_printer.pp_typ typ' d_chunk acc;
  empty @@ (acc, local_env.is_ghost), init, typ''

and blockInitializer local_env vi inite =
  let ghost = local_env.is_ghost in
  let c,init,ty = doInitializer local_env vi inite in c2block ~ghost c, init, ty

(* [VP-2012-03-01] As a matter of fact, this function is not tail-rec, but it seems
   that it's not an issue in practice. *)
(* Consume some initializers. Watch out here. Make sure we use only
 * tail-recursion because these things can be big.  *)
and doInit
    local_env
    (isconst: bool)
    (add_implicit_ensures: predicate named -> unit)
      (* callback to add an ensures clause to contracts
         above current initialized part when it is partially initialized.
         Does nothing initially.
      *)
    (setone: offset -> exp -> unit) (* Use to announce an initializer *)
    (so: subobj)
    (acc: chunk)
    (initl: (A.initwhat * A.init_expression) list)

    (* Return the resulting chunk along with some unused initializers *)
    : chunk * (A.initwhat * A.init_expression) list =

  let whoami fmt = Cil_printer.pp_lval fmt (Var so.host, so.soOff) in

  let initl1 =
    match initl with
    | (A.NEXT_INIT,
       A.SINGLE_INIT ({ expr_node = A.CAST ((s, dt), ie)} as e)) :: rest ->
        let s', dt', ie' = preprocessCast local_env.is_ghost s dt ie in
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
      (what,
       A.SINGLE_INIT
         ({expr_node = A.CAST ((specs, dt), A.COMPOUND_INIT ci)})) :: rest ->
        let s', dt', _ie' =
          preprocessCast local_env.is_ghost specs dt (A.COMPOUND_INIT ci) in
        let typ = doOnlyType local_env.is_ghost s' dt' in
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
  Kernel.debug ~dkey:category_initializer
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
    _, [] -> acc, [] (* No more initializers return *)

  (* No more subobjects *)
  | _, (A.NEXT_INIT, _) :: _ when so.eof -> acc, allinitl


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
    let acc', initl' =
      doInit local_env isconst add_implicit_ensures setone so' acc charinits in
    if initl' <> [] then
      Kernel.warning ~current:true
	"Too many initializers for character array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env isconst add_implicit_ensures setone so acc' restil

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
	             TInt _ when (bitsSizeOf bt') =
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
      let acc', initl' =
        doInit local_env isconst add_implicit_ensures setone so' acc charinits in
      if initl' <> [] then
        (* sm: see above regarding ISO 6.7.8 para 14, which is not implemented
         * for wchar_t because, as far as I can tell, we don't even put in
         * the automatic NUL (!) *)
        Kernel.warning ~current:true
	  "Too many initializers for wchar_t array %t" whoami;
      (* Advance past the array *)
      advanceSubobj so;
      (* Continue *)
      doInit local_env isconst add_implicit_ensures setone so acc' restil

  (* If we are at an array and we see a single initializer then it must
   * be one for the first element *)
  | TArray(bt, leno, _, _), (A.NEXT_INIT, A.SINGLE_INIT _oneinit) :: _restil  ->
      (* Grab the length if there is one *)
      let leno = integerArrayLength leno in
      so.stack <- InArray(so.soOff, bt, leno, ref 0) :: so.stack;
      normalSubobj so;
      (* Start over with the fields *)
      doInit local_env isconst add_implicit_ensures setone so acc allinitl

  (* If we are at a composite and we see a single initializer of the same
   * type as the composite then grab it all. If the type is not the same
   * then we must go on and try to initialize the fields *)
  | TComp (comp, _, _), (A.NEXT_INIT, A.SINGLE_INIT oneinit) :: restil ->
      let r,se, oneinit', t' =
        doExp local_env isconst oneinit (AExp None)
      in
      let se = add_reads oneinit'.eloc r se in
      if (match unrollType t' with
            TComp (comp', _, _) when comp'.ckey = comp.ckey -> true
          | _ -> false)
      then begin
        (* Initialize the whole struct *)
        setone so.soOff oneinit';
        (* Advance to the next subobject *)
        advanceSubobj so;
        let se = acc @@ (se, local_env.is_ghost) in
        doInit local_env isconst add_implicit_ensures setone so se restil
      end else begin (* Try to initialize fields *)
        let toinit = fieldsToInit comp None in
        so.stack <- InComp(so.soOff, comp, toinit) :: so.stack;
        normalSubobj so;
        doInit local_env isconst add_implicit_ensures setone so acc allinitl
      end

  (* A scalar with a single initializer *)
  | _, (A.NEXT_INIT, A.SINGLE_INIT oneinit) :: restil ->
      let r, se, oneinit', t' =
        doExp local_env isconst oneinit (AExp(Some so.soTyp)) in
      let se = add_reads oneinit'.eloc r se in
      Kernel.debug ~dkey:category_initializer "oneinit'=%a, t'=%a, so.soTyp=%a"
        Cil_printer.pp_exp oneinit' Cil_printer.pp_typ t'
        Cil_printer.pp_typ so.soTyp;
      setone so.soOff (if theMachine.insertImplicitCasts then
                         snd (castTo t' so.soTyp oneinit')
                       else oneinit');
      (* Move on *)
      advanceSubobj so;
      let se = acc @@ (se,local_env.is_ghost) in
      doInit local_env isconst add_implicit_ensures setone so se restil


  (* An array with a compound initializer. The initializer is for the
   * array elements *)
  | TArray (bt, leno, _, _), (A.NEXT_INIT, A.COMPOUND_INIT initl) :: restil ->
      (* Create a separate object for the array *)
      let so' = makeSubobj so.host so.soTyp so.soOff in
      (* Go inside the array *)
      let leno = integerArrayLength leno in
      so'.stack <- [InArray(so'.curOff, bt, leno, ref 0)];
      normalSubobj so';
      let acc', initl' =
        doInit
          local_env isconst add_implicit_ensures setone so' acc initl
      in
      if initl' <> [] then
        Kernel.warning ~current:true
	  "Too many initializers for array %t" whoami;
      (* Advance past the array *)
      advanceSubobj so;
      (* Continue *)
      let res =
        doInit local_env isconst add_implicit_ensures setone so acc' restil
      in
      res

  (* We have a designator that tells us to select the matching union field.
   * This is to support a GCC extension *)
  | TComp(ci, _, _) as targ, [(A.NEXT_INIT,
                       A.COMPOUND_INIT [(A.INFIELD_INIT ("___matching_field",
                                                         A.NEXT_INIT),
                                         A.SINGLE_INIT oneinit)])]
      when not ci.cstruct ->
      (* Do the expression to find its type *)
      let _, _, _, t' = doExp local_env isconst oneinit (AExp None) in
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
        doInit local_env isconst add_implicit_ensures setone so acc
          [(A.NEXT_INIT, A.SINGLE_INIT oneinit)]
      else
        (* If this is a GNU extension with field-to-union cast find the field *)
        let fi = findField ci.cfields in
        (* Change the designator and redo *)
        doInit local_env isconst add_implicit_ensures setone so acc
          [A.INFIELD_INIT (fi.fname, A.NEXT_INIT), A.SINGLE_INIT oneinit]

  (* A structure with a composite initializer. We initialize the fields*)
  | TComp (comp, _, _), (A.NEXT_INIT, A.COMPOUND_INIT initl) :: restil ->
      (* Create a separate subobject iterator *)
      let so' = makeSubobj so.host so.soTyp so.soOff in
      (* Go inside the comp *)
      so'.stack <- [InComp(so'.curOff, comp, fieldsToInit comp None)];
      normalSubobj so';
      let acc', initl' =
        doInit local_env isconst add_implicit_ensures setone so' acc initl
      in
      if initl' <> [] then
        Kernel.warning ~current:true "Too many initializers for structure";
      (* Advance past the structure *)
      advanceSubobj so;
      (* Continue *)
      doInit local_env isconst add_implicit_ensures setone so acc' restil


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
            doExp local_env isconst oneinit (AExp(Some so.soTyp))
	  in
          let se = add_reads oneinit'.eloc r se in
          setone so.soOff (makeCastT oneinit' t' so.soTyp);
          (* Move on *)
          advanceSubobj so;
          let se = acc @@ (se, local_env.is_ghost) in
          doInit local_env isconst add_implicit_ensures setone so se restil
	with Not_found ->
	  Kernel.abort ~current:true
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
            A.NEXT_INIT -> acc
          | A.INFIELD_INIT (fn, whatnext) -> begin
              match unrollType so.soTyp with
                TComp (comp, _, _) ->
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
                TArray (bt, leno, _, _) ->
                  let ilen = integerArrayLength leno in
                  let nextidx', doidx =
                    let (r,doidx, idxe', _) =
                      doExp local_env true idx (AExp(Some intType))
                    in
                    let doidx = add_reads idxe'.eloc r doidx in
                    match (constFold true idxe').enode, isNotEmpty doidx with
                      Const(CInt64(x, _, _)), false -> Integer.to_int x, doidx
                    | _ ->
		      Kernel.abort ~current:true
                        "INDEX initialization designator is not a constant"
                  in
                  if nextidx' < 0 || nextidx' >= ilen then
                    Kernel.abort ~current:true
		      "INDEX designator is outside bounds";
                  so.stack <-
                    InArray(so.soOff, bt, ilen, ref nextidx') :: so.stack;
                  normalSubobj so;
                  address whatnext (acc @@ (doidx, local_env.is_ghost))

              | _ ->
		Kernel.abort ~current:true "INDEX designator for a non-array"
            end

          | A.ATINDEXRANGE_INIT _ ->
            Kernel.abort ~current:true "addressSubobj: INDEXRANGE"
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
              doExp local_env true idxs (AExp(Some intType)) in
            let (re, doidxe, idxe', _) =
              doExp local_env true idxe (AExp(Some intType)) in
            let doidxs = add_reads idxs'.eloc rs doidxs in
            let doidxe = add_reads idxe'.eloc re doidxe in
            if isNotEmpty doidxs || isNotEmpty doidxe then
              Kernel.fatal ~current:true "Range designators are not constants";
            let first, last =
              match (constFold true idxs').enode, (constFold true idxe').enode
              with
                Const(CInt64(s, _, _)),
                Const(CInt64(e, _, _)) ->
                  Integer.to_int s, Integer.to_int e
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
            doInit local_env isconst add_implicit_ensures setone so acc (loop first)

        | A.NEXT_INIT -> (* We have not found any RANGE *)
            let acc' = addressSubobj so what acc in
            doInit local_env isconst add_implicit_ensures setone so acc'
              ((A.NEXT_INIT, ie) :: restil)
      in
      expandRange (fun x -> x) what

  | t, (_what, _ie) :: _ ->
    Kernel.abort ~current:true "doInit: cases for t=%a" Cil_printer.pp_typ t


(* Create and add to the file (if not already added) a global. Return the
 * varinfo *)
and createGlobal ghost logic_spec ((t,s,b,attr_list) : (typ * storage * bool * A.attribute list))
    (((n,ndt,a,cloc), inite) : A.init_name) : varinfo =
    try
    Kernel.debug ~dkey:category_global ~level:2 "createGlobal: %s" n;
    (* If the global is a Frama-C builtin, set the generated flag *)
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
      (* sm: if it's a function prototype, and the storage class *)
      (* isn't specified, make it 'extern'; this fixes a problem *)
      (* with no-storage prototype and static definition *)
      if vi.vstorage = NoStorage then
        vi.vstorage <- Extern;
    end else if Extlib.has_some logic_spec then begin
      let emit,msg = 
        if Kernel.ContinueOnAnnotError.get () then
          Kernel.warning, " (ignoring)."
        else Kernel.error, "."
      in
      emit ~current:true ~once:true
        "Global variable %s is not a function. It cannot have a contract%s"
        vi.vname msg
    end;
    let vi, alreadyInEnv = makeGlobalVarinfo (inite != A.NO_INIT) vi in
    (* Do the initializer and complete the array type if necessary *)
    let init : init option =
      if inite = A.NO_INIT then
        None
      else
        let se, ie', et = doInitializer (ghost_local_env ghost) vi inite in
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
      Kernel.debug ~dkey:category_global ~level:2
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
	      Cabshelper.continue_annot
		(cloc)
		(fun () ->
                   let known_behaviors = find_existing_behaviors vi in
                   let spec =
                     Ltyping.funspec
                       known_behaviors vi (Some(get_formals vi)) vi.vtype spec
                   in
                   update_funspec_in_theFile vi spec)
		(fun () -> ())
		"Ignoring specification of function %s" vi.vname
        end ;
      vi
    with Not_found -> begin
      (* Not already defined *)
      Kernel.debug ~dkey:category_global ~level:2
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
        if not (isFunctionType vi.vtype)
          && not (IH.mem mustTurnIntoDef vi.vid) then
            begin
              IH.add mustTurnIntoDef vi.vid true
            end;
        if not alreadyInEnv then begin (* Only one declaration *)
          (* If it has function type it is a prototype *)
          (* NB: We add the formal prms in the env*)
	  if isFunctionType vi.vtype && not vi.vdefined then
            setFormalsDecl vi vi.vtype;
	  let spec =
	    match logic_spec with
            | None -> empty_funspec ()
            | Some (spec,loc) ->
		begin
                  Cabshelper.continue_annot loc
                    (* it can not have old behavior names, since this is the
                       first time we see the declaration.
                     *)
		    (fun () -> Ltyping.funspec [] vi None vi.vtype spec)
		    (empty_funspec)
		    "Ignoring specification of function %s" vi.vname
                end
	  in
          cabsPushGlobal (GVarDecl (spec, vi, CurrentLoc.get ()));
          vi
        end else begin
          Kernel.debug ~dkey:category_global ~level:2
            " already in env %s" vi.vname;
          (match logic_spec with
           | None -> ()
           | Some (spec,loc) ->
               let merge_spec = function
                 | GVarDecl(old_spec, _, _) ->
                     let behaviors =
                       List.map (fun b -> b.b_name) old_spec.spec_behavior
                     in
                     let spec =
                       Cabshelper.continue_annot loc
		         (fun () ->
                            Ltyping.funspec behaviors vi None vi.vtype spec)
		         empty_funspec
		         "Ignoring specification of function %s" vi.vname
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
  with _ when Cilmsg.had_errors () && continueOnError -> begin
    Kernel.error ~once:true ~current:true "skipping global %s" n;
    cabsPushGlobal
      (dGlobal (Pretty_utils.sfprintf "booo - error in global %s (%t)"
                  n Cil.pp_thisloc) (CurrentLoc.get ()));
    dummy_function.svar
  end
    (*
      ignore (E.log "Env after processing global %s is:@\n%t@\n"
      n docEnv);
      ignore (E.log "Alpha after processing global %s is:@\n%t@\n"
      n docAlphaTable)
    *)

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
      Kernel.debug ~dkey:category_global ~level:2
        "createGlobal (local static): %s" n;
      (* Now alpha convert it to make sure that it does not conflict with
       * existing globals or locals from this function. *)
      let newname, _  = newAlphaName true "" n in
      (* Make it global  *)
      let vi = makeVarInfoCabs ~ghost ~isformal:false
        ~isglobal:true
        loc specs (newname, ndt, a) in
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
          let se, ie', et = doInitializer (ghost_local_env ghost) vi inite in
          (* Maybe we now have a better type?  Use the type of the
           * initializer only if it really differs from the type of
           * the variable. *)
          if unrollType vi.vtype != unrollType et then
            Cil.update_var_type vi et;
          if isNotEmpty se then
            Kernel.error ~once:true ~current:true "global static initializer";
          (* Check that no locals are refered by the initializer *)
          check_no_locals_in_initializer ie';
          (* Maybe the initializer refers to the function itself.
             Push a prototype for the function, just in case. *)
          cabsPushGlobal
            (GVarDecl (empty_funspec (), !currentFunctionFDEC.svar,
		       CurrentLoc.get ()));
          Cil.setFormalsDecl
            !currentFunctionFDEC.svar !currentFunctionFDEC.svar.vtype;
          Some ie'
        end
      in
      cabsPushGlobal (GVar(vi, {init = init}, CurrentLoc.get ()));
      empty

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
      let se1 =
        if isvarsize then begin (* Variable-sized array *)
          Kernel.warning ~current:true
	    "Variable-sized local variable %s" vi.vname;
          (* Make a local variable to keep the length *)
          let savelen =
            makeVarInfoCabs
              ~ghost
              ~isformal:false
              ~isglobal:false
	      loc
              (theMachine.typeOfSizeOf, NoStorage, false, [])
              ("__lengthof" ^ vi.vname,JUSTBASE, [])
          in
          (* Register it *)
          let savelen = alphaConvertVarAndAddToEnv true savelen in
	  let se0 = local_var_chunk se0 savelen in
          (* Compute the sizeof *)
          let sizeof =
            new_exp ~loc
              (BinOp(Mult,
                     new_exp ~loc
                       (SizeOfE
                          (new_exp ~loc
                             (Lval
                                (Mem(new_exp ~loc (Lval(var vi))),
                                 NoOffset)))),
                     new_exp ~loc (Lval (var savelen)),
                     theMachine.typeOfSizeOf))
          in
          (* Register the length *)
          IH.add varSizeArrays vi.vid sizeof;
          (* There can be no initializer for this *)
          if inite != A.NO_INIT then
            Kernel.error ~once:true ~current:true
	      "Variable-sized array cannot have initializer";
          let setlen =  se0 +++
            (mkStmtOneInstr ~ghost
               (Set(var savelen, makeCast len savelen.vtype,
		    CurrentLoc.get ())),
             [],[],[])
          in
          (* Initialize the variable *)
          let alloca: varinfo = allocaFun () in
          if Kernel.DoCollapseCallCast.get () then
            (* do it in one step *)
            setlen +++
              (mkStmtOneInstr ~ghost
                 (Call(Some(var vi), new_exp ~loc (Lval(var alloca)),
		       [ sizeof  ], loc)),
	       [],[var vi],[])
          else begin
            (* do it in two *)
            let rt, _, _, _ = splitFunctionType alloca.vtype in
            let tmp =
              newTempVar 
		(Pretty_utils.sfprintf "alloca(%a)" Cil_printer.pp_exp sizeof)
                false rt
            in
            (local_var_chunk setlen tmp)
            +++ (mkStmtOneInstr ~ghost
                   (Call(Some(var tmp), new_exp ~loc (Lval(var alloca)),
			 [ sizeof  ], CurrentLoc.get ())),[],[],[])
            +++ (mkStmtOneInstr ~ghost
                   (Set((var vi),
			makeCast (new_exp ~loc (Lval(var tmp))) vi.vtype,
			CurrentLoc.get ())),
                 [],[var vi],[var tmp])
          end
        end else empty
      in
      let se1 = local_var_chunk se1 vi in
      if inite = A.NO_INIT then
        se1 (* skipChunk *)
      else begin
        let se4, ie', et = doInitializer (ghost_local_env ghost) vi inite in
        (* Fix the length *)
        (match vi.vtype, ie', et with
           (* We have a length now *)
           TArray(_,None, _, _), _, TArray(_, Some _, _, _) -> 
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
        @@ (assignInit ~ghost (Var vi, NoOffset) ie' et empty, ghost)
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
    with Not_found -> Kernel.abort ~current:true "error in doDecl"
  in
  v.vattr <- dropAttribute "alias" v.vattr


(* Do one declaration *)
and doDecl local_env (isglobal: bool) : A.definition -> chunk = function
  | A.DECDEF (logic_spec, (s, nl), loc) ->
      CurrentLoc.set (convLoc loc);
      (* Do the specifiers exactly once *)
      let sugg =
        match nl with
          [] -> ""
        | ((n, _, _, _), _) :: _ -> n
      in
      let ghost = local_env.is_ghost in
      let spec_res = doSpecList local_env.is_ghost sugg s in
      (* Do all the variables and concatenate the resulting statements *)
      let doOneDeclarator (acc: chunk) (name: init_name) =
        let (n,ndt,a,l),_ = name in
        if isglobal then begin
          let bt,_,_,attrs = spec_res in
          let vtype, nattr =
            doType local_env.is_ghost false
              (AttrName false) bt (A.PARENTYPE(attrs, ndt, a)) in
          (match filterAttributes "alias" nattr with
             [] -> (* ordinary prototype. *)
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
          acc @@ (createLocal local_env.is_ghost spec_res name, ghost)
      in
     let res = List.fold_left doOneDeclarator empty nl in
     if isglobal then res
     else begin
       match logic_spec with
         | None -> res
         | Some (spec,loc) ->
           let loc' = convLoc loc in
	   begin
	     Cabshelper.continue_annot loc
	       (fun () ->
                 let spec =
                   Ltyping.code_annot loc' local_env.known_behaviors
                     (Ctype !currentReturnType) (AStmtSpec ([],spec))
                 in
                 append_chunk_to_annot ~ghost
                   (s2c
                      (mkStmtOneInstr
                         ~ghost:local_env.is_ghost (Code_annot (spec,loc'))))
                   res
               )
               (fun () -> res)
	       "Ignoring logic code specification" ;
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
        [Attr("dummy", [a'])] ->
          let a'' =
            match a' with
            | ACons (s, args) ->
                process_align_pragma s args;
                process_pack_pragma s args
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

  (* If there are multiple definitions of extern inline, turn all but the
   * first into a prototype *)
  | A.FUNDEF (spec,((specs,(n,dt,a,loc')) : A.single_name),
              (_body : A.block), loc, _)
      when isglobal && isExtern specs && isInline specs
        && (H.mem genv (n ^ "__extinline")) ->
      CurrentLoc.set (convLoc loc);
          let othervi, _ = lookupVar (n ^ "__extinline") in
          if othervi.vname = n then
            (* The previous entry in the env is also an extern inline version
               of n. *)
            Kernel.warning ~current:true
	      "Duplicate extern inline definition for %s ignored" n
          else begin
            (* Otherwise, the previous entry is an ordinary function that
               happens to be named __extinline.  Renaming n to n__extinline
               would confict with other, so report an error. *)
            Kernel.fatal ~current:true
              ("Trying to rename %s to\n %s__extinline, but %s__extinline"
               ^^ " already exists in the env.\n  \"__extinline\" is"
               ^^ " reserved for CIL.\n") n n n
          end;
          (* Treat it as a prototype *)
          doDecl local_env isglobal
            (A.DECDEF (spec,(specs, [((n,dt,a,loc'), A.NO_INIT)]), loc))

  | A.FUNDEF (spec,((specs,(n,dt,a, _)) : A.single_name),
              (body : A.block), loc1, loc2) when isglobal ->
      begin
        let ghost = local_env.is_ghost in
        let idloc = loc1 in
        let funloc = fst loc1, snd loc2 in
        let endloc = loc2 in
        Kernel.debug ~dkey:category_global ~level:2
          "Definition of %s at %a\n" n Cil_printer.pp_location idloc;
        CurrentLoc.set idloc;
        try
          IH.clear callTempVars;

          (* Make the fundec right away, and we'll populate it later. We
           * need this throughout the code to create temporaries. *)
          currentFunctionFDEC :=
            { svar     = makeGlobalVar ~generated:false n voidType;
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

          constrExprId := 0;
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

          (* Fix the NAME and the STORAGE *)
          let _ =
            let bt,sto,inl,attrs = doSpecList local_env.is_ghost n specs in
            !currentFunctionFDEC.svar.vinline <- inl;

            let ftyp, funattr =
              doType local_env.is_ghost false
                (AttrName false) bt (A.PARENTYPE(attrs, dt, a)) in
            (* Format.printf "Attrs are %a@." d_attrlist funattr; *)
            Cil.update_var_type !currentFunctionFDEC.svar ftyp;
            !currentFunctionFDEC.svar.vattr <- funattr;

            (* If this is the definition of an extern inline then we change
             * its name, by adding the suffix __extinline. We also make it
             * static *)
            let n', sto' =
              let n' = n ^ "__extinline" in
              if inl && sto = Extern then begin
                n', Static
              end else begin
                (* Maybe this is the body of a previous extern inline. Then
                 * we must take that one out of the environment because it
                 * is not used from here on. This will also ensure that
                 * then we make this functions' varinfo we will not think
                 * it is a duplicate definition *)
                (try
                   ignore (lookupVar n'); (* if this succeeds, n' is defined*)
                   let oldvi, _ = lookupVar n in
                   if oldvi.vname = n' then begin
                     (* oldvi is an extern inline function that has been
                        renamed to n ^ "__extinline".  Remove it from the
                        environment. *)
                     H.remove env n; H.remove genv n;
                     H.remove env n'; H.remove genv n'
                   end
                   else
                     (* oldvi is not a renamed extern inline function, and
                        we should do nothing.  The reason the lookup
                        of n' succeeded is probably because there's
                        an ordinary function that happens to be named,
                        n ^ "__extinline", probably as a result of a previous
                        pass through CIL.   See small2/extinline.c*)
                     ()
                 with Not_found -> ());
                n, sto
              end
            in
            (* Now we have the name and the storage *)
            !currentFunctionFDEC.svar.vname <- n';
            !currentFunctionFDEC.svar.vstorage <- sto'
          in
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
	      let f = makeVarinfo ~generated:false false true fn ft in
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
                [] -> ()
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
	        Cabshelper.continue_annot loc
		  (fun () ->
		    !currentFunctionFDEC.sspec <-
		      Ltyping.funspec behaviors
		      !currentFunctionFDEC.svar
		      (Some !currentFunctionFDEC.sformals)
		      !currentFunctionFDEC.svar.vtype spec)
		  (fun () -> ())
		  "ignoring logic specification of function %s"
		  !currentFunctionFDEC.svar.vname
            | None -> ()
          end;
          (* Merge pre-existing spec if needed. *)
          if has_decl then begin
            let merge_spec = function
              | GVarDecl(old_spec,_,loc) as g ->
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
                body in
            (* Finish everything *)
            exitScope ();
            (* Now fill in the computed goto statement with cases. Do this
             * before mkFunctionbody which resolves the gotos *)
            (match !gotoTargetData with
               Some (_switchv, switch) ->
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
                     (i2c (mkStmtOneInstr ~ghost:local_env.is_ghost
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
                     [ s, _, _,_,_] when newswitch.cases == []-> s.skind
                   | _ ->
		     Kernel.fatal ~current:true
		       "Unexpected result from switchChunk"
                 in
                 switch.skind <- newswitchkind

             | None -> ());
            (* Now finish the body and store it *)
            let body = mkFunctionBody ~ghost stmts in
            (* need to add temporary variables created at sbody level. *)
            body.blocals <- !currentFunctionFDEC.sbody.blocals @ body.blocals;
            (*Format.eprintf "Function %a: Temp variables created: %a@."
              Cil_printer.pp_varinfo !currentFunctionFDEC.svar
              (Pretty_utils.pp_list ~sep:Pretty_utils.space_sep Cil_printer.pp_varinfo)
              !currentFunctionFDEC.sbody.blocals; *)
            !currentFunctionFDEC.sbody <- body;
            (* Reset the global parameters *)
            gotoTargetData := None;
            H.clear gotoTargetHash;
            gotoTargetNextAddr := 0;
          in
          let rec dropFormals formals locals =
            match formals, locals with
              [], l -> l
            | f :: formals, l :: locals ->
                if f != l then
                  Kernel.abort ~current:true
		    "formal %s is not in locals (found instead %s)"
                    f.vname
                    l.vname;
                dropFormals formals locals
            | _ -> Kernel.abort ~current:true "Too few locals"
          in
          !currentFunctionFDEC.slocals
          <- dropFormals !currentFunctionFDEC.sformals
            (List.rev !currentFunctionFDEC.slocals);
          setMaxId !currentFunctionFDEC;

          (* Now go over the types of the formals and pull out the formals
           * with transparent union type. Replace them with some shadow
           * parameters and then add assignments  *)
          let _ =
            let newformals, newbody =
              List.fold_right (* So that the formals come out in order *)
                (fun f (accform, accbody) ->
                   match isTransparentUnion f.vtype with
                     None -> (f :: accform, accbody)
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
                        mkStmtOneInstr ~ghost:local_env.is_ghost
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
            let protect_return,retval =
              (* Guard the [return] instructions we add with an
                 [\assert \false]*)
              let assert_false () =
                let annot = Logic_const.new_code_annotation
                  (AAssert ([], Logic_const.unamed ~loc:endloc Pfalse))
                in
                Cil.mkStmt ~ghost:local_env.is_ghost
                  (Instr (Code_annot (annot, endloc)))
              in
              match unrollType !currentReturnType with
                | TVoid _ -> [], None
                | (TInt _ | TEnum _ | TFloat _ | TPtr _) as rt ->
                  let res = Some (makeCastT (zero ~loc:endloc) intType rt) in
                  if !currentFunctionFDEC.svar.vname = "main" then
                    [],res
                  else begin
                    Kernel.warning ~current:true
                      "Body of function %s falls-through. \
                        Adding a return statement"
		      !currentFunctionFDEC.svar.vname;
                    [assert_false ()], res
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Body of function %s falls-through and \
                        cannot find an appropriate return value"
                    !currentFunctionFDEC.svar.vname;
                  [assert_false ()], None
            in
            if not (hasAttribute "noreturn" !currentFunctionFDEC.svar.vattr)
            then
              !currentFunctionFDEC.sbody.bstmts <-
                !currentFunctionFDEC.sbody.bstmts
              @ protect_return @ 
                [mkStmt ~ghost:local_env.is_ghost (Return(retval, endloc))]
          end;

          (* ignore (E.log "The env after finishing the body of %s:\n%t\n"
             n docEnv); *)
          cabsPushGlobal (GFun (!currentFunctionFDEC, funloc));
          empty
        with _ when Cilmsg.had_errors () && continueOnError -> begin
          Kernel.error ~once:true ~current:true
	    "skipping function %s in collectFunction" n;
          cabsPushGlobal (GAsm("error in function " ^ n, CurrentLoc.get ()));
          empty
        end
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
             Kernel.abort ~current:true
	       "doDecl returns non-empty statement for global")
        dl;
      empty

  | A.GLOBANNOT (decl) when isglobal ->
      begin
        List.iter
          (fun decl  ->
	     let loc = convLoc decl.Logic_ptree.decl_loc in
	     CurrentLoc.set loc;
             Cabshelper.continue_annot loc
	       (fun () ->
		  let tdecl = Ltyping.annot decl in
		  cabsPushGlobal (GAnnot(tdecl,CurrentLoc.get ())))
	       (fun () -> ())
	       "Ignoring logic global annotation"
          ) decl;
      end;
      empty

  | A.CUSTOM (custom, name, location) when isglobal ->
      begin
	let loc = convLoc location in
	CurrentLoc.set loc;
        Cabshelper.continue_annot loc
	  (fun () ->
	    let tcustom = Ltyping.custom custom in
	    cabsPushGlobal (GAnnot(Dcustom_annot(tcustom, name, CurrentLoc.get ()),CurrentLoc.get ())))
	  (fun () -> ())
	  "Ignoring custom global annotation";
	Kernel.warning ~current:true "cabs2cil : custom"
      end;
      empty
  | A.CUSTOM _ | A.GLOBANNOT _ | A.PRAGMA _ | A.GLOBASM _ | A.FUNDEF _ ->
     Kernel.fatal ~current:true "this form of declaration must be global"
(* Fragile pattern matching are bad practice
  | _ -> Kernel.fatal ~current:true "unexpected form of declaration"
*)

and doTypedef ghost ((specs, nl): A.name_group) =
  try
    (* Do the specifiers exactly once *)
    let bt, sto, inl, attrs = doSpecList ghost (suggestAnonName nl) specs in
    if sto <> NoStorage || inl then
      Kernel.error ~once:true ~current:true
	"Storage or inline specifier not allowed in typedef";
    let createTypedef ((n,ndt,a,_) : A.name) =
      (*    E.s (error "doTypeDef") *)
      try
        let newTyp, tattr =
          doType ghost false AttrType bt (A.PARENTYPE(attrs, ndt, a))  in
        let newTyp' = cabsTypeAddAttributes tattr newTyp in
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
      with _ when Cilmsg.had_errors () && continueOnError -> begin
        Kernel.error ~once:true ~current:true "skipping typedef";
        cabsPushGlobal (GAsm ("booo_typedef:" ^ n, CurrentLoc.get ()))
      end
    in
    List.iter createTypedef nl
  with _ when Cilmsg.had_errors () && continueOnError -> begin
    Kernel.error ~once:true ~current:true "skipping typedef";
    let fstname =
      match nl with
        [] -> "<missing name>"
      | (n, _, _, _) :: _ -> n
    in
    cabsPushGlobal (GAsm ("booo_typedef: " ^ fstname, CurrentLoc.get ()))
  end

and doOnlyTypedef ghost (specs: A.spec_elem list) : unit =
  try
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
      TComp(ci, _, al) ->
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

  with _ when Cilmsg.had_errors () && continueOnError -> begin
    Kernel.error ~once:true ~current:true "skipping A.ONLYTYPEDEF";
    cabsPushGlobal (GAsm ("booo_typedef", CurrentLoc.get ()))
  end

and assignInit ~ghost (lv: lval)
    ?(has_implicit_init=false)
    ?(explicit_init=(fun _ _ -> ()))
    ?(add_implicit_ensures=(fun _ -> ()))
    (ie: init)
    (iet: typ)
    (acc: chunk) : chunk =
  match ie with
    SingleInit e ->
      let (_, e'') = castTo iet (typeOfLval lv) e in
      explicit_init lv e'';
      acc +++ (mkStmtOneInstr ~ghost
                 (Set(lv, e'', CurrentLoc.get ())),[],[lv],[])
  | CompoundInit (t, initl) ->
    (match t with
      | TArray(bt,len,_,_) ->
        let l = integerArrayLength len in
        if List.length initl < l then begin
          (* For big arrays in local variables,
             the implicit initialization to 0 is not done
             completely. We'll do that ourselves, with
             - a bzero to 0
             - a contract for plugins that do not want to rely on bzero.
             All that is done at the toplevel occurence of implicit 
             initialization.
          *)
          let (curr_host,curr_off) = lv in
          let vi =
            match curr_host with
              | Var vi -> vi
              | _ -> Kernel.fatal "Trying to initialize a anonymous block"
          in
          let ensures = ref [] in
          let known_idx = ref Datatype.Big_int.Set.empty in
          let explicit_init (_,off as lv) v =
            if not has_implicit_init then begin
              (* just add ensures at the toplevel init *)
              let pred = ensures_init vi off v in
              let post_cond = (Normal, Logic_const.new_predicate pred) in
              ensures:= post_cond :: !ensures
            end;
            (* find which index is initialized.
               This is not necessarily the last one in case of array of
               complex structures.
            *)
            let rec aux off =
              let my_off, last_off = Cil.removeOffset off in
              if Cil_datatype.Offset.equal curr_off my_off then begin
                match last_off with
                  | Index(i,_) ->
                      (match Cil.constFold true i with
                        | { enode = Const (CInt64 (v,_,_)) } ->
                            known_idx := Datatype.Big_int.Set.add v !known_idx
                        | _ ->
                            Kernel.abort ~current:true
                              "Non constant index in designator for array \
                               initialization: %a"
                              Cil_printer.pp_exp i)
                  | NoOffset | Field _ ->
                      assert false
                  (* We are supposed to have an array here. *)
              end else
                match last_off with
                  | NoOffset -> ()
                  | _ -> aux my_off
            in
            aux off;
            explicit_init lv v
          in
          let add_implicit_ensures =
            if has_implicit_init then add_implicit_ensures
            else
              fun e ->
                ensures:= (Normal, Logic_const.new_predicate e) :: !ensures
          in
          (* do the initialization of the array only. *)
          let my_init =
            foldLeftCompound
              ~implicit:false
              ~doinit:(fun off i it acc ->
                assignInit ~ghost (addOffsetLval off lv)
                  ~has_implicit_init:true
                  ~explicit_init
                  ~add_implicit_ensures
                  i it acc)
              ~ct:t
              ~initl:initl
              ~acc:empty
          in
          let base_init =
            if has_implicit_init then
              empty 
              (* this location has already been zero-initialized by
                 toplevel implicit init. *)
            else if Kernel.InitializedPaddingLocals.get () then
              s2c (set_to_zero ~ghost vi curr_off t)
              (* use bzero to clear whole region*)
            else
              zero_init ~ghost vi curr_off l bt
              (* zero-init each field, so as to leave padding bits
                 uninitialized. *)
          in
          let init_block = base_init @@ (my_init, ghost) in
          (* lift at toplevel contract implicit zero-initialization. *)
          let my_ensures =
            make_implicit_ensures vi curr_off bt l !known_idx
          in
          add_implicit_ensures my_ensures;
          let annot_chunk =
            if has_implicit_init then empty
            else begin
              let tlv = Logic_utils.lval_to_term_lval ~cast:false lv in
              let loc = vi.vdecl in
              let rec all_zone tlv =
                match Logic_utils.unroll_type (Cil.typeOfTermLval tlv) with
                  | Ctype (TArray (_,len,_,_)) 
                  | Ltype ({ lt_name = "set"},[Ctype(TArray (_,len,_,_))])->
                      let tlen =
                        Extlib.opt_map
                          (Logic_utils.expr_to_term ~cast:false) len
                      in
                      let upper =
                        Extlib.opt_map
                          (fun tlen ->
                            Logic_const.term ~loc
                              (TBinOp(MinusA,tlen,Logic_const.tinteger ~loc 1))
                              Linteger)
                          tlen
                      in
                      let all_range =
                        Logic_const.trange ~loc
                          (Some (Logic_const.tinteger ~loc 0), upper)
                      in
                      all_zone (Logic_const.addTermOffsetLval 
                                  (TIndex (all_range, TNoOffset)) tlv)
                  | t -> Logic_const.term ~loc (TLval tlv) t
              in
              let tlocs = all_zone tlv in
              let assigns =
                Writes [Logic_const.new_identified_term tlocs,FromAny]
              in
              let post_cond = List.rev !ensures in
              let contract =
                { spec_behavior =
                    [Cil.mk_behavior
                        ~name:"Frama_C_implicit_init"
                        ~assigns
                        ~post_cond
                        ()
                    ];
                  spec_variant = None;
                  spec_terminates = None;
                  spec_complete_behaviors = [];
                  spec_disjoint_behaviors = [];
                }
              in
              let code_annot =
                Logic_const.new_code_annotation (AStmtSpec ([],contract))
              in
              s2c (Cil.mkStmt ~ghost
                     (Instr
                        (Code_annot (code_annot,Cabshelper.currentLoc()))))
            end
          in
          let init_chunk =
            append_chunk_to_annot ~ghost annot_chunk init_block
          in
          acc @@ (init_chunk, ghost)
        end else begin
          foldLeftCompound
            ~implicit:false
            ~doinit:
            (fun off i it acc ->
              assignInit ~ghost (addOffsetLval off lv)
                ~has_implicit_init
                ~explicit_init
                ~add_implicit_ensures
                i it acc)
            ~ct:t
            ~initl:initl
            ~acc:acc
        end
      | _ ->
        foldLeftCompound
          ~implicit:false
          ~doinit:
          (fun off i it acc ->
            assignInit ~ghost (addOffsetLval off lv)
              ~has_implicit_init
              ~explicit_init
              ~add_implicit_ensures
              i it acc)
          ~ct:t
          ~initl:initl
          ~acc:acc)

and blockInit ~ghost (lv: lval) (ie: init) (iet: typ) : block =
  c2block ~ghost (assignInit ~ghost lv ie iet empty)

(* Now define the processors for body and statement *)
and doBody local_env (blk: A.block) : chunk =
  let ghost = local_env.is_ghost in
  enterScope ();
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
                  not to the first Cil statement
               *)
               let new_behaviors, keep_next =
                 match s.stmt_node with
                     CODE_ANNOT _  -> [], true
                   | CODE_SPEC (s,_) ->
                       List.map (fun x -> x.b_name) s.spec_behavior, true
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
  exitScope ();
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
    let res = s2c (mkStmt ~ghost (Block b)) in
    { res with cases = bodychunk.cases }
  end

and doStatement local_env (s : A.statement) : chunk =
  let mk_loop_annot a loc =
    Cabshelper.continue_annot loc
      (fun () -> List.map
         (Ltyping.code_annot
            loc local_env.known_behaviors (Ctype !currentReturnType)) a)
      (fun () -> [])
      "Ignoring all logic loop annotations"
  in
  let ghost = s.stmt_ghost in
  let local_env = { local_env with is_ghost = ghost } in
  try
    match s.stmt_node with
      A.NOP loc -> { empty
                     with stmts = [mkEmptyStmt ~ghost ~loc (), [],[],[],[]]}
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
        let c = doBody local_env b in
        let b = c2block ~ghost c in
        b.battrs <- addAttributes [Attr(frama_c_keep_block,[])] b.battrs;
        let res = s2c (mkStmt ~ghost (Block b)) in
        { res with cases = c.cases }

    | A.SEQUENCE (s1, s2, _) ->
        let c1 = doStatement local_env s1 in
        let c2 = doStatement local_env s2 in
        c1 @@ (c2, ghost)

    | A.IF(e,st,sf,loc) ->
        let st' = doStatement local_env st in
        let sf' = doStatement local_env sf in
        CurrentLoc.set (convLoc loc);
        doCondition ~info:(Cabscond.IF,loc) local_env false e st' sf'

    | A.WHILE(a,e,s,loc) ->
        let a = mk_loop_annot a loc in
        startLoop true;
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
          ((doCondition
	      ~info:(Cabscond.WHILE,loc) local_env false e skipChunk break_cond)
           @@ (s', ghost))

    | A.DOWHILE(a, e,s,loc) ->
        let a = mk_loop_annot a loc in
        startLoop false;
        let s' = doStatement local_env s in
        let loc' = convLoc loc in
        CurrentLoc.set loc';
        (* No 'break' instruction can exit the chunk *)
        let no_break chunk =
          List.for_all (fun (s, _, _, _, _) -> not (stmtCanBreak s)) chunk.stmts
        in
        (* Check if we are translating 'do { <s> } while (0)'. If so, translate
           it sito '<s>' instead. Not active for now as it can impact plugins
           that compare the shape of the Cabs and of the Cil files. *)
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
	         ~info:(Cabscond.DOWHILE,loc)
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
            FC_EXP e1 -> doFullExp local_env false e1 ADrop, false
          | FC_DECL d1 ->
              (doDecl local_env false d1, zero ~loc, voidType), true
        in
        let a = mk_loop_annot a loc in
        let (se3, _, _) = doFullExp local_env false e3 ADrop in
        startLoop false;
        let s' = doStatement local_env s in
        (*Kernel.debug "Loop body : %a" d_chunk s';*)
        CurrentLoc.set loc';
        let s'' = consLabContinue ~ghost se3 in
        let break_cond = breakChunk ~ghost loc' in
        exitLoop ();
        let res =
          match e2.expr_node with
            A.NOTHING -> (* This means true *)
              se1 @@ (loopChunk ~ghost a (s' @@ (s'', ghost)), ghost)
          | _ ->
              se1 @@
                (loopChunk ~ghost a
                   (((doCondition
		        ~info:(Cabscond.FOR,loc)
		        local_env false e2 skipChunk break_cond)
                     @@ (s', ghost)) @@ (s'', ghost)), ghost)
        in
        exitScope ();
        if has_decl then begin
          let chunk = s2c (mkStmt ~ghost (Block (c2block ~ghost res)))
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
          Kernel.warning ~current:true
            "Return statement without a value in function returning %a\n"
            Cil_printer.pp_typ !currentReturnType;
        returnChunk ~ghost None loc'

    | A.RETURN (e, loc) ->
        let loc' = convLoc loc in
        CurrentLoc.set loc';
        (* Sometimes we return the result of a void function call *)
        if isVoidType !currentReturnType then begin
          Kernel.warning ~current:true
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
          match (constFold true el').enode, (constFold true eh').enode with
            Const(CInt64(il, _, _)), Const(CInt64(ih, _, _)) ->
              Integer.to_int il, Integer.to_int ih
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
          Some (switchv, switch) -> (* We have already generated this one  *)
            (se
             @@ (i2c(mkStmtOneInstr ~ghost
                       (Set (var switchv, makeCast e' intType, loc')),
                     [],[],[]), ghost))
            @@ (s2c(mkStmt ~ghost (Goto (ref switch, loc'))), ghost)

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
              with Not_found ->
		Kernel.abort ~current:true "Cannot find temporary for goto *";
            in
            (* Make a switch statement. We'll fill in the statements at the
             * end of the function *)
            let switch =
              mkStmt ~ghost
                (Switch (new_exp ~loc (Lval(var switchv)),
                         mkBlock [], [], loc'))
            in
            (* And make a label for it since we'll goto it *)
            switch.labels <- [Label ("__docompgoto", loc', false)];
            gotoTargetData := Some (switchv, switch);
            (se @@
               (i2c
                  (mkStmtOneInstr ~ghost
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
	let (tmpls', outs', ins', clobs', labels') =
	  match details with
	  | None ->
	      let tmpls' =
		if theMachine.msvcMode then tmpls
		else
		  let pattern = Str.regexp "%" in
		  let escape = Str.global_replace pattern "%%" in
		  List.map escape tmpls
	      in
	      (tmpls', [], [], [],[])
	  | Some { aoutputs = outs; ainputs = ins; aclobbers = clobs; alabels = labels } ->
              let outs' =
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
		     stmts := !stmts @@ (se, ghost);
		     (id, c, lv)) outs
              in
	      (* Get the side-effects out of expressions *)
              let ins' =
		List.map
		  (fun (id, c, e) ->
		     let (r, se, e', _) =
                       doExp local_env false e (AExp None)
                     in
                     let se = add_reads e'.eloc r se in
		     stmts := !stmts @@ (se, ghost);
		     (id, c, e'))
		  ins
              in
	      let labels' = 
		List.map 
		  (fun label -> 
		    let label = lookupLabel label in
		    let gref = ref dummyStmt in
		    addGoto label gref;
		    gref) 
		  labels 
	      in
	      (tmpls, outs', ins', clobs, labels')
	in
        !stmts @@
          (i2c(mkStmtOneInstr ~ghost:local_env.is_ghost
                 (Asm(attr', tmpls', outs', ins', clobs', labels', loc')),[],[],[]),
          ghost)

    | TRY_FINALLY (b, h, loc) ->
        let loc' = convLoc loc in
        CurrentLoc.set loc';
        let b': chunk = doBody local_env b in
        let h': chunk = doBody local_env h in
        if b'.cases <> [] || h'.cases <> [] then
          Kernel.error ~once:true ~current:true
	    "Try statements cannot contain switch cases";
        s2c (mkStmt ~ghost 
               (TryFinally (c2block ~ghost b', c2block ~ghost h', loc')))

    | TRY_EXCEPT (b, e, h, loc) ->
        let loc' = convLoc loc in
        CurrentLoc.set loc';
        let b': chunk = doBody local_env b in
        (* Now do e *)
        let ((se: chunk), e', _) =
          doFullExp local_env false e (AExp None) in
        let h': chunk = doBody local_env h in
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
        s2c (mkStmt ~ghost
               (TryExcept
                  (c2block ~ghost b',(il', e'), c2block ~ghost h', loc')))
    | CODE_ANNOT (a, loc) ->
        let loc' = convLoc loc in
        begin
	  Cabshelper.continue_annot loc
	    (fun () ->
               let typed_annot =
                 Ltyping.code_annot
                   loc' local_env.known_behaviors (Ctype !currentReturnType) a
               in
               s2c (mkStmtOneInstr ~ghost (Code_annot (typed_annot,loc'))))
	    (fun () -> BlockChunk.empty)
	    "Ignoring logic code annotation" ;
        end

    | CODE_SPEC (a, loc) ->
        let loc' = convLoc loc in
	begin
	  Cabshelper.continue_annot loc
	    (fun () ->
               let spec =
                 Ltyping.code_annot loc' local_env.known_behaviors
                   (Ctype !currentReturnType) (AStmtSpec ([],a))
               in
               s2c (mkStmtOneInstr ~ghost (Code_annot (spec,loc'))))
	    (fun () -> BlockChunk.empty)
	    "Ignoring logic code specification" ;
        end
  with _ when Cilmsg.had_errors () && continueOnError -> begin
    Kernel.error "Ignoring statement" ;
    consLabel
      ~ghost "booo_statement" empty (convLoc (C.get_statementloc s)) false
  end


let rec stripParenLocal e = match e.expr_node with
  | A.PAREN e2 -> stripParenLocal e2
  | _ -> e

class stripParenClass : V.cabsVisitor = object
  inherit V.nopCabsVisitor

  method! vexpr e = match e.expr_node with
  | A.PAREN e2 -> ChangeDoChildrenPost (stripParenLocal e2,stripParenLocal)
  | _ -> DoChildren
end

let stripParenFile file = V.visitCabsFile (new stripParenClass) file

(* Translate a file *)
let convFile (f : A.file) : Cil_types.file =

  (* remove parentheses from the Cabs *)
  let fname,dl = stripParenFile f in

  (* Clean up the global types *)
  Cilmsg.clear_errors();
  initGlobals();
  startFile ();
  IH.clear noProtoFunctions;
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear staticLocals;
  H.clear typedefs;
  cleanup_isomorphicStructs ();
  H.clear alpha_renaming;
  Stack.clear packing_pragma_stack;
  current_packing_pragma := None;
  H.clear pragma_align_by_struct;
  current_pragma_align := None;
  Logic_env.prepare_tables ();
  anonCompFieldNameId := 0;
  Kernel.debug ~level:2 "Converting CABS->CIL" ;
  (* Setup the built-ins, but do not add their prototypes to the file *)
  let setupBuiltin name (resTyp, argTypes, isva) =
    let v =
      makeGlobalVar name (TFun(resTyp,
                               Some (List.map (fun at -> ("", at, []))
                                       argTypes),
                               isva, [])) in
    ignore (alphaConvertVarAndAddToEnv true v);
    (* Add it to the file as well *)
    cabsPushGlobal (GVarDecl (empty_funspec (), v, Cil.builtinLoc));
    Cil.setFormalsDecl v v.vtype
  in
  Cil.Builtin_functions.iter_sorted setupBuiltin;

  let globalidx = ref 0 in
  let doOneGlobal (ghost,(d: A.definition)) =
    let local_env = ghost_local_env ghost in
    let s = doDecl local_env true d in
    if isNotEmpty s then
      Kernel.abort ~current:true
	"doDecl returns non-empty statement for global";
  in
  List.iter doOneGlobal dl;
  let globals = ref (fileGlobals ()) in

  List.iter rename_spec !globals;
  Logic_env.prepare_tables ();
  IH.clear noProtoFunctions;
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  cleanup_isomorphicStructs ();
  H.clear staticLocals;
  H.clear typedefs;
  H.clear env;
  H.clear genv;
  IH.clear callTempVars;
  H.clear alpha_renaming;

  if false then Kernel.debug "Cabs2cil converted %d globals" !globalidx;
  (* We are done *)
  { fileName = fname;
    globals  = !globals;
    globinit = None;
    globinitcalled = false;
  }

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
