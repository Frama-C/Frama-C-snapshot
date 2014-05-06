(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
open Printer_api
open Format

let debug_logic_types = Kernel.register_category "printer:logic-types"
let debug_logic_coercions = Kernel.register_category "printer:logic-coercions"
let debug_builtins = Kernel.register_category "printer:builtins"
let debug_sid = Kernel.register_category "printer:sid"
let debug_unspecified = Kernel.register_category "printer:unspecified"

module Behavior_extensions = struct
  let printer_tbl = Hashtbl.create 5
  let register name printer =
    Hashtbl.add printer_tbl name printer
  let pp (printer:extensible_printer_type) fmt (name, code, preds) =
    try 
      let pp = Hashtbl.find printer_tbl name in
      Format.fprintf fmt "@[<hov 2>%s %a;@]" name (pp printer) (code, preds)
    with Not_found -> 
      (* default pretty-printer *)
      Format.fprintf fmt "@[<hov 2>%s %a;@]"
	name
	(Pretty_utils.pp_list ~sep:",@ " printer#identified_predicate) 
	preds;

end
let register_behavior_extension = Behavior_extensions.register

(* Internal attributes. Won't be pretty-printed *)
let reserved_attributes = ref []
let register_shallow_attribute s = reserved_attributes:=s::!reserved_attributes

let needs_quote =
  let regex = Str.regexp "^[A-Za-z0-9_]+$" in
  fun s -> not (Str.string_match regex s 0)

let print_as_source source =
  Kernel.Debug.get () = 0 
  &&
  (Kernel.BigIntsHex.is_default () 
   || not (Str.string_match (Str.regexp "^-?[0-9]+$") source 0))


(* This function decides whether to hide Frama-C's own builtins (in
   fc_builtin_for_normalization). *)
let print_var v =
  not (Cil.is_unused_builtin v) || Kernel.is_debug_key_enabled debug_builtins


let pretty_C_constant suffix k fmt i = 
  let nb_signed_bits = 
    Integer.pred (Integer.of_int (8 * (Cil.bytesSizeOfInt k))) 
  in
  let max_strict_signed = Integer.two_power nb_signed_bits in
  let most_neg = Integer.neg max_strict_signed in
  if Integer.equal most_neg i then 
     (* sm: quirk here: if you print -2147483648 then this is two
        tokens in C, and the second one is too large to represent in 
        a signed int.. 
        so we do what's done in limits.h, and print (-2147483467-1); *)
     (* in gcc this avoids a warning, but it might avoid a real 
        problem on another compiler or a 64-bit architecture *)
    Format.fprintf fmt "(-%a-1)" 
      Datatype.Big_int.pretty (Integer.pred max_strict_signed)
  else
    Format.fprintf fmt "%a%s" Datatype.Big_int.pretty i suffix

let pred_body = function
  | LBpred a -> a
  | LBnone
  | LBreads _
  | LBinductive _
  | LBterm _ -> Kernel.fatal "definition expected in Cil.pred_body"

let state =
  { line_directive_style = Some Line_preprocessor_input;
    print_cil_input = false;
    print_cil_as_is = false;
    line_length = 80;
    warn_truncate = true }

(* Parentheses/precedence level. An expression "a op b" is printed
   parenthesized if its parentheses level is >= that that of its context.
   Identifiers have the lowest level and weakly binding operators (e.g. |)
   have the largest level. The correctness criterion is that a smaller level
   MUST correspond to a stronger precedence! *)
module Precedence = struct

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
   | Pnot _ -> 30
   | Psubtype _ -> 75
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

 let getParenthLevel e = match (Cil.stripInfo e).enode with
   | Info _ -> assert false
   | BinOp((LAnd | LOr), _,_,_) -> 80
   (* Bit operations. *)
   | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)
   (* Comparisons *)
   | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_) ->
       comparativeLevel (* 70 *)
   (* Additive. Shifts can have higher level than + or - but I want parentheses
      around them *)
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

 let rec getParenthLevelLogic = function
   | Tlambda _ | Trange _ | Tlet _ -> binderLevel
   | TBinOp((LAnd | LOr), _,_) -> 80
   (* Bit operations. *)
   | TBinOp((BOr|BXor|BAnd),_,_) -> bitwiseLevel (* 75 *)
   (* Comparisons *)
   | TBinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_) ->
     comparativeLevel (* 70 *)
   (* Additive. Shifts can have higher level than + or - but I want parentheses
      around them *)
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
   | TLogic_coerce(_,e) -> (getParenthLevelLogic e.term_node) + 1

 (* Create an expression of the same shape, and use {!getParenthLevel} *)
 let getParenthLevelAttrParam = function
   | AInt _ | AStr _ | ACons _ -> 0
   | ASizeOf _ | ASizeOfE _ -> 20
   | AAlignOf _ | AAlignOfE _ -> 20
   | AUnOp (uo, _) -> 
     getParenthLevel
       (Cil.dummy_exp
	  (UnOp(uo, Cil.zero ~loc:Cil_datatype.Location.unknown, Cil.intType)))
   | ABinOp (bo, _, _) ->
       getParenthLevel
	 (Cil.dummy_exp(BinOp(bo,
                              Cil.zero ~loc:Cil_datatype.Location.unknown,
                              Cil.zero ~loc:Cil_datatype.Location.unknown,
                              Cil.intType)))
   | AAddrOf _ -> 30
   | ADot _ | AIndex _ | AStar _ -> 20
   | AQuestion _ -> questionLevel

end

let get_termination_kind_name = function
  | Normal -> "ensures" | Exits -> "exits" | Breaks -> "breaks"
  | Continues -> "continues" | Returns -> "returns"

(* Grab one of the labels of a statement *)
let rec pickLabel = function
  | [] -> None
  | Label (lbl, _, _) :: _ -> Some lbl
  | _ :: rest -> pickLabel rest

class cil_printer () = object (self)

  val mutable logic_printer_enabled = true

  method reset () = ()

  method without_annot:
    'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
    fun f fmt x ->
      let tmp = logic_printer_enabled in
      logic_printer_enabled <- false;
      let finally () = logic_printer_enabled <- tmp in
      Extlib.try_finally ~finally (f fmt) x;

  val mutable force_brace = false

  method force_brace:
    'a. (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
    fun f fmt x ->
      let tmp = force_brace in
      force_brace <- true;
      let finally () = force_brace <- tmp in
      Extlib.try_finally ~finally f fmt x;

  val mutable verbose = false
  (* Do not add a value that depends on a
     non-constant variable of the kernel here (e.g. [Kernel.Debug.get ()]). Due
     to the way the pretty-printing class is instantiated, this value would be
     evaluated too soon. Override the [reset] method instead. *)
    
  val current_stmt = Stack.create ()

  val mutable current_function = None
  method private current_function = current_function

  method private in_current_function vi =
    assert (current_function = None);
    current_function <- Some vi

  method private out_current_function =
    assert (current_function <> None);
    current_function <- None

  val mutable current_behavior = None
  method private current_behavior = current_behavior

  method private set_current_behavior b =
    assert (current_behavior = None); 
    current_behavior <- Some b

  method private reset_current_behavior () =
    assert (current_behavior <> None); 
    current_behavior <- None

  val mutable has_annot = false
  method private has_annot = has_annot && logic_printer_enabled

  method private push_stmt s = Stack.push s current_stmt
  method private pop_stmt s =
    ignore (Stack.pop current_stmt); 
    has_annot <- false; 
    s

  method private current_stmt =
    try Some (Stack.top current_stmt) with Stack.Empty -> None

  method private may_be_skipped s = s.labels = []

  method location fmt loc = Cil_datatype.Location.pretty fmt loc
    
  (* constant *)
  method constant fmt = function
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
      | ILongLong -> if Cil.theMachine.Cil.msvcMode then "L" else "LL"
      | IULongLong -> if Cil.theMachine.Cil.msvcMode then "UL" else "ULL"
      | IInt | IBool | IShort | IUShort | IChar | ISChar | IUChar -> ""
    in
    let prefix =
      if suffix <> "" then ""
      else if ik = IInt then ""
      else Pretty_utils.sfprintf "(%a)" self#ikind ik
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

  (*** VARIABLES ***)
  method varname fmt v = pp_print_string fmt v

  method private p_varstring v = Pretty_utils.sfprintf "%a" self#varinfo v
    
  (* variable use *)
  method varinfo fmt v = Format.fprintf fmt "%a" self#varname v.vname

  (* variable declaration *)
  method vdecl fmt (v:varinfo) =
    let stom, rest = Cil.separateStorageModifiers v.vattr in
    let fundecl = if Cil.isFunctionType v.vtype then Some v else None in
    (* First the storage modifiers *)
    fprintf fmt "%s%a%a%s%a%a"
      (if v.vinline then "__inline " else "")
      self#storage v.vstorage
      self#attributes stom
      (if stom = [] then "" else " ")
      (self#typ ?fundecl
         (if v.vname = "" then None else Some (fun fmt -> self#varinfo fmt v)))
      v.vtype
      self#attributes rest

  (*** L-VALUES ***)
  method lval fmt (lv:lval) =  (* lval (base is 1st field)  *)
    match lv with
      Var vi, o -> fprintf fmt "%a%a" self#varinfo vi self#offset o
    | Mem e, Field(fi, o) ->
      fprintf fmt "%a->%a%a"
	(self#exp_prec Precedence.arrowLevel)  e
	self#varname fi.fname
	self#offset o
    | Mem e, NoOffset ->
      fprintf fmt "*%a"
	(self#exp_prec Precedence.derefStarLevel) e
    | Mem e, o ->
      fprintf fmt "(*%a)%a"
	(self#exp_prec Precedence.derefStarLevel) e
	self#offset o

  (** Offsets **)

  method field fmt fi = self#varname fmt fi.fname

  method offset fmt = function
  | NoOffset -> ()
  | Field (fi, o) ->
    fprintf fmt ".%a%a"
      self#field fi
      self#offset o
  | Index (e, o) ->
    fprintf fmt "[%a]%a"
      self#exp e
      self#offset o

  method private lval_prec (contextprec: int) fmt lv =
    if Precedence.getParenthLevel (Cil.dummy_exp(Lval(lv))) >= contextprec then
      fprintf fmt "(%a)" self#lval lv
    else
      self#lval fmt lv

  (*** EXPRESSIONS ***)
  method exp fmt (e: exp) =
    let level = Precedence.getParenthLevel e in
    match (Cil.stripInfo e).enode with
    | Info _ -> assert false
    | Const(c) -> self#constant fmt c
    | Lval(l) -> self#lval fmt l

    | UnOp(u,e1,_) ->
      (match u, e1 with
      | Neg, {enode = Const (CInt64 (v, _, _))}
        when Integer.ge v Integer.zero ->
	fprintf fmt "-%a" (self#exp_prec level) e1
      | _ ->
	fprintf fmt "%a %a" self#unop u (self#exp_prec level) e1)

    | BinOp(b,e1,e2,_) ->
      fprintf fmt "@[%a %a %a@]"
	(self#exp_prec level) e1
	self#binop b
	(self#exp_prec level) e2

    | CastE(t,e) ->
      fprintf fmt "(%a)%a" (self#typ None) t (self#exp_prec level) e
    | SizeOf t -> fprintf fmt "sizeof(%a)" (self#typ None) t
    | SizeOfE e -> fprintf fmt "sizeof(%a)" self#exp e
    | SizeOfStr s -> fprintf fmt "sizeof(%a)" self#constant (CStr s)
    | AlignOf t -> fprintf fmt "__alignof__(%a)" (self#typ None) t
    | AlignOfE e -> fprintf fmt "__alignof__(%a)" self#exp e
    | AddrOf lv -> fprintf fmt "& %a" (self#lval_prec Precedence.addrOfLevel) lv
    | StartOf(lv) ->
      if state.print_cil_as_is then fprintf fmt "&(%a[0])" self#lval lv
      else self#lval fmt lv

  method unop fmt u =
    fprintf fmt "%s"
      (match u with
      | Neg -> "-"
      | BNot -> "~"
      | LNot -> "!")

  method binop fmt b =
    fprintf fmt "%s"
      (match b with
      | PlusA | PlusPI | IndexPI -> "+"
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

  (* Print an expression, given the precedence of the context in which it
   * appears. *)
  method private exp_prec (contextprec: int) fmt (e: exp) =
    let thisLevel = Precedence.getParenthLevel e in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == Precedence.bitwiseLevel then
	(* quiet down some GCC warnings *)
	thisLevel == Precedence.additiveLevel 
	|| thisLevel == Precedence.comparativeLevel
      else
	false
    in
    if needParens then fprintf fmt "(%a)" self#exp e else self#exp fmt e

  method init fmt = function
  | SingleInit e -> self#exp fmt e
  | CompoundInit (t, initl) ->
    (* We do not print the type of the Compound *)
    (*
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
      (docList ~sep:(chr ',' ++ break) dinit) initl
     *)
    let designated_init fmt = function
      | Field(f, NoOffset), i ->
	fprintf fmt ".%a = " self#varname f.fname;
	self#init fmt i
      | Index(e, NoOffset), i ->
	fprintf fmt "[%a] = " self#exp e;
	self#init fmt i
      | _ -> Kernel.fatal "Trying to print malformed initializer"
    in
    if not (Cil.isArrayType t) then
      fprintf fmt
        "{@[%a@]}" (Pretty_utils.pp_list ~sep:",@ " designated_init) initl
    else begin
      let print_index prev_index (designator,init as di) =
        let curr_index =
          match designator with
            | Index(e,NoOffset) -> Cil.isInteger (Cil.constFold false e)
            | _ -> None
        in
        let designator_needed =
          match prev_index, curr_index with
            | None, _ | _, None -> true
            | Some p, Some c -> not (Integer.equal (Integer.succ p) c)
        in
        if designator_needed then designated_init fmt di
        else self#init fmt init;
        curr_index
      in
      let print_next_index prev_index di =
        Format.fprintf fmt ",@ "; print_index prev_index di
      in
      Format.fprintf fmt "{@[";
      (match initl with
        | [] -> ()
        | i::tl ->
          let curr_index = print_index (Some Integer.minus_one) i in
          ignore (List.fold_left print_next_index curr_index tl));
      Format.fprintf fmt "@]}"
    end

  (** What terminator to print after an instruction. sometimes we want to
      print sequences of instructions separated by comma *)
  val mutable instr_terminator = ";"

  method private set_instr_terminator (term : string) =
    instr_terminator <- term

  method private get_instr_terminator () = instr_terminator

  (*** INSTRUCTIONS ****)
  method instr fmt (i:instr) = (* imperative instruction *)
    fprintf fmt "%a"
      (self#line_directive ~forcefile:false) (Cil_datatype.Instr.loc i);
    match i with
    | Skip _ -> fprintf fmt ";"
    | Set(lv,e,_) -> begin
      (* Be nice to some special cases *)
      match e.enode with
	BinOp((PlusA|PlusPI|IndexPI),
	      {enode = Lval(lv')},
	      {enode=Const(CInt64(one,_,_))},_)
	  when Cil.compareLval lv lv' && Integer.equal one Integer.one
	    && not state.print_cil_as_is ->
	      fprintf fmt "%a ++%s"
		(self#lval_prec Precedence.indexLevel) lv
		instr_terminator
      | BinOp((MinusA|MinusPI),
	      {enode = Lval(lv')},
	      {enode=Const(CInt64(one,_,_))}, _)
	  when Cil.compareLval lv lv' && Integer.equal one Integer.one
	    && not state.print_cil_as_is ->
	fprintf fmt "%a --%s"
	  (self#lval_prec Precedence.indexLevel) lv
	  instr_terminator

      | BinOp((PlusA|PlusPI|IndexPI),
	      {enode = Lval(lv')},
	      {enode = Const(CInt64(mone,_,_))},_)
	  when Cil.compareLval lv lv' && Integer.equal mone Integer.minus_one
	    && not state.print_cil_as_is ->
	fprintf fmt "%a --%s"
	  (self#lval_prec Precedence.indexLevel) lv
	  instr_terminator

      | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
	  Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
	      {enode = Lval(lv')},e,_) when Cil.compareLval lv lv' ->
	fprintf fmt "%a %a= %a%s"
	  self#lval  lv
	  self#binop bop
	  self#exp e
	  instr_terminator

      | _ ->
	fprintf fmt "%a = %a%s"
	  self#lval lv
	  self#exp e
	  instr_terminator

    end
    (* In cabs2cil we have turned the call to builtin_va_arg into a
       three-argument call: the last argument is the address of the 
       destination *)
    | Call(None, {enode = Lval(Var vi, NoOffset)},
	   [dest; {enode = SizeOf t}; adest], (l,_))
	when vi.vname = "__builtin_va_arg" 
	&& not state.print_cil_as_is ->
      let destlv = match (Cil.stripCasts adest).enode with
	  AddrOf destlv -> destlv
	(* If this fails, it's likely that an extension interfered
	   with the AddrOf *)
	| _ ->
	  Kernel.fatal ~source:l
	    "Encountered unexpected call to %s with dest %a"
	    vi.vname self#exp adest
      in
      fprintf fmt "%a = __builtin_va_arg (@[%a,@ %a@])%s"
	self#lval destlv
	(* Now the arguments *)
	self#exp dest
	(self#typ None)  t
	instr_terminator

    (* In cabs2cil we have dropped the last argument in the call to
       __builtin_va_start and __builtin_stdarg_start. *)
    | Call(None, {enode = Lval(Var vi, NoOffset)}, [marker], l)
	when ((vi.vname = "__builtin_stdarg_start" ||
	    vi.vname = "__builtin_va_start")
	      && not state.print_cil_as_is) ->
      let last = self#getLastNamedArgument () in
      self#instr fmt (Call(None, Cil.dummy_exp(Lval(Var vi,NoOffset)),
			   [marker; last],l))

    (* In cabs2cil we have dropped the last argument in the call to
       __builtin_next_arg. *)
    | Call(res, {enode = Lval(Var vi, NoOffset)}, [ ], l)
	when vi.vname = "__builtin_next_arg" 
	&& not state.print_cil_as_is ->
      let last = self#getLastNamedArgument () in
      self#instr fmt (Call(res,Cil.dummy_exp(Lval(Var vi,NoOffset)),[last],l))

    (* In cparser we have turned the call to
       __builtin_types_compatible_p(t1, t2) into
       __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
       represent the types as expressions.
       Remove the sizeofs when printing. *)
    | Call(dest, {enode = Lval(Var vi, NoOffset)},
	   [{enode = SizeOf t1}; {enode = SizeOf t2}], _)
	when vi.vname = "__builtin_types_compatible_p"
	&& not state.print_cil_as_is ->
      (* Print the destination *)
      (match dest with
	None -> ()
      | Some lv -> fprintf fmt "%a = " self#lval lv );
	  (* Now the call itself *)
	  fprintf fmt "%a(%a, %a)%s"
	    self#varname vi.vname
	    (self#typ None) t1
	    (self#typ None) t2
	    instr_terminator
    | Call(_, {enode = Lval(Var vi, NoOffset)}, _, (l,_))
	when vi.vname = "__builtin_types_compatible_p"
	&& not state.print_cil_as_is ->
      Kernel.fatal ~source:l
	"__builtin_types_compatible_p: cabs2cil should have added sizeof to \
the arguments."

    | Call(dest,e,args,_) ->
      (match dest with
      | None -> ()
      | Some lv ->
	fprintf fmt "%a = " self#lval lv;
	(* Maybe we need to print a cast *)
	(let destt = Cil.typeOfLval lv in
	 match Cil.unrollType (Cil.typeOf e) with
         | TFun(rt, _, _, _) when (Cil.need_cast rt destt) ->
	   fprintf fmt "(%a)" (self#typ None) destt
	 | _ -> ()));
      (* Now the function name *)
      (match e.enode with
      | Lval(Var _, _) -> self#exp fmt e
      | _ -> fprintf fmt "(%a)"  self#exp e);
      (* Now the arguments *)
      Pretty_utils.pp_flowlist ~left:"(" ~sep:"," ~right:")" self#exp fmt args;
      (* Now the terminator *)
      fprintf fmt "%s" instr_terminator

    | Asm(attrs, tmpls, outs, ins, clobs, labels, l) ->
      self#line_directive fmt l;
      let goto = if labels=[] then "" else " goto" in
      if Cil.theMachine.Cil.msvcMode then
	fprintf fmt "__asm%s {@[%a@]}%s"
	  goto
	  (Pretty_utils.pp_list ~sep:"@\n"
	     (fun fmt s -> fprintf fmt "%s" s)) tmpls
	  instr_terminator
      else begin
	fprintf fmt "__asm__%s%a (@[%a"
	  goto
	  self#attributes attrs
	  (Pretty_utils.pp_list ~sep:"@\n"
	     (fun fmt x ->
	       (* [JS 2011/03/11] isn't equivalent to [fprintf fmt "%S" x]? *)
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
		   self#lval lv
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
		   self#exp e))
	    ins;


	if clobs <> [] || labels <> [] then
	  fprintf fmt ": %a"
	    (Pretty_utils.pp_list ~sep:",@ "
	       (fun fmt c -> fprintf fmt "\"%s\"" (Escape.escape_string c)))
	    clobs;

	if labels <> [] then 
	  fprintf fmt ": %a"
	    (Pretty_utils.pp_list ~sep:",@ "
	       (fun fmt r -> 
		 match pickLabel !r.labels with 
		 | Some label -> Format.pp_print_string  fmt label
		 | None ->       
		   Kernel.error "Cannot find label for target of asm goto: %a" 
		     (self#without_annot self#stmt) !r;
		   Format.pp_print_string fmt "__invalid_label"))
	    labels;

	fprintf fmt "@])%s" instr_terminator
      end
    | Code_annot (annot, l) ->
      has_annot <- true;
      if logic_printer_enabled then begin
	self#line_directive ~forcefile:false fmt l;
	Pretty_utils.pp_open_block fmt "/*@@ ";
	self#code_annotation fmt annot ;
	Pretty_utils.pp_close_block fmt "*/";
      end

  (** For variadic calls *)
  method private getLastNamedArgument () =
    match self#current_function with
    | None ->
      Kernel.error ~current:true "Current stmt not positioned";
      Cil_datatype.Exp.dummy
    | Some vi ->
      let formals = Cil.getFormalsDecl vi in
      match List.rev formals with
      | [] -> assert false (* Typing error, this function is variadic and should
                           have at least one argument *)
      | f :: _ -> Cil.new_exp ~loc:f.vdecl (Lval (Cil.var f))

  (**** STATEMENTS ****)
  method stmt fmt (s:stmt) =        (* control-flow statement *)
    self#push_stmt s;
    self#pop_stmt (self#next_stmt Cil.invalidStmt fmt s)

  method next_stmt (next: stmt) fmt (s: stmt) =
    self#push_stmt s;
    self#pop_stmt (self#annotated_stmt next fmt s)

  method stmt_labels fmt (s:stmt) =
    if s.labels <> [] then
      Pretty_utils.pp_list ~sep:"@ " ~suf:"@]@ " self#label fmt s.labels

  method label fmt = function
  | Label (s, _, b) when b || not verbose -> fprintf fmt "@[%s:@]" s
  | Label (s, _, _) -> fprintf fmt "@[%s: /* internal */@]" s
  | Case (e, _) -> fprintf fmt "@[case %a:@]" self#exp e
  | Default _ -> fprintf fmt "@[default:@]"

  (* number of opened ghost code *)
  val mutable is_ghost = false
  method private display_comment () = not is_ghost || verbose

  method annotated_stmt (next: stmt) fmt (s: stmt) =
    pp_open_hvbox fmt 2;
    self#stmt_labels fmt s;
    pp_open_hvbox fmt 0;
    (* print the statement. *)
    if Cil.is_skip s.skind && not s.ghost then begin
      if verbose || s.labels <> [] then fprintf fmt ";"
    end else begin
      let was_ghost = is_ghost in
      let display_ghost = s.ghost && not was_ghost in
      if display_ghost then begin
	is_ghost <- true;
        Pretty_utils.pp_open_block fmt "@[/*@@ ghost "
      end;
      self#stmtkind next fmt s.skind ;
      if display_ghost then begin
	is_ghost <- false;
        Pretty_utils.pp_close_block fmt "*/@]"
      end
    end;
    pp_close_box fmt ();
    pp_close_box fmt ()

  method private require_braces ?(has_annot=self#has_annot) blk = 
    force_brace 
  || verbose || Kernel.is_debug_key_enabled debug_sid
  (* If one the of condition above is true, /* sid:... */ will be printed
     on its own line before s. Braces are needed *)
  ||
      match blk.bstmts, blk.battrs, blk.blocals with
      | _ :: _ :: _, _, _ | _, _, _ :: _ | _, _ :: _, _ -> true
      | [ { skind = Block b } ], _, _ -> has_annot || self#require_braces b
      | _, _, _ -> has_annot

  method private inline_block ?has_annot blk = match blk.bstmts with
  | [] | [ { skind = (Instr _ | Return _ | Goto _ | Break _ | Continue _ ) } ] 
    -> 
    not (self#require_braces ?has_annot blk)
  | [ { skind = Block blk } ] -> self#inline_block blk
  | _ -> false
    
  method private block_is_function blk = match blk.bstmts with
  | [ { skind = Instr (Call _) } ] -> true
  | [ { skind = Block blk } ] -> self#block_is_function blk
  | _ -> false

  method private block_has_dangling_else blk = match blk.bstmts with
  | [ { skind = If(_, { bstmts=[]; battrs=[] }, _, _) 
	  | If(_, {bstmts=[{skind=Goto _; labels=[]}]; battrs=[]}, _, _) 
	  | If(_, _, { bstmts=[]; battrs=[] }, _) 
	  | If(_, _, {bstmts=[{skind=Goto _; labels=[]}]; battrs=[]}, _) } ]
    -> true
  | [ { skind = Block blk | If(_, _, blk, _) } ] -> 
    self#block_has_dangling_else blk
  | _ -> false

  method private vdecl_complete fmt v =
    let display_ghost = v.vghost && not is_ghost in
    Format.fprintf fmt "@[<hov 0>%t%a;%t@]"
      (if display_ghost then fun fmt -> Format.fprintf fmt "/*@@ ghost@ "
       else ignore)
      self#vdecl v
      (if display_ghost then fun fmt -> Format.fprintf fmt "@ */" else ignore)

  (* no box around the block *)
  method private unboxed_block ?(cut=true) ?braces ?has_annot fmt blk =
    let braces = match braces with
      | None -> self#require_braces ?has_annot blk
      | Some b -> b
    in
    let inline = not braces && self#inline_block ?has_annot blk in
    if braces then pp_print_char fmt '{';
    if braces && not inline then pp_print_space fmt ();
    if blk.blocals <> [] && verbose then
      fprintf fmt "@[/* Locals: %a */@]@ "
	(Pretty_utils.pp_list ~sep:",@ " self#varinfo) blk.blocals;
    if blk.battrs <> [] then 
      (* [JS 2012/12/07] could directly call self#attributesGen whenever we are
	 sure than it puts its printing material inside a box *)
      fprintf fmt "@[%a@]" (self#attributesGen true) blk.battrs;
    if blk.blocals <> [] then
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@;" ~suf:"@]@ " 
	self#vdecl_complete fmt blk.blocals;
    let rec iterblock ~cut fmt = function
      | [] -> ()
      | [ s ] ->
	fprintf fmt "";
	if cut && not inline && not braces then pp_print_cut fmt ();
	self#next_stmt Cil.invalidStmt fmt s
      | s_cur :: (s_next :: _ as tail) ->
	Format.fprintf fmt "%a@ %a"
	  (self#next_stmt s_next) s_cur 
	  (iterblock ~cut:false) tail
    in
    let stmts = blk.bstmts in
    if stmts = [] && not braces then fprintf fmt ";" 
    else fprintf fmt "%a" (iterblock ~cut) stmts;
    if braces then Format.fprintf fmt "@;<1 -2>}"

  (* no box around the block *)
  method block ?braces fmt (blk: block) =
    let braces = 
      match braces with None -> self#require_braces blk | Some b -> b
    in
    let open_box =
      if self#inline_block blk then pp_open_hvbox else pp_open_vbox 
    in
    open_box fmt (if braces then 2 else 0);
    if verbose then Pretty_utils.pp_open_block fmt "/*block:begin*/@ ";
    self#unboxed_block ~cut:false ~braces fmt blk;
    if verbose then Pretty_utils.pp_close_block fmt "/*block:end*/";
    pp_close_box fmt ()

  (* Store here the name of the last file printed in a line number. This is
     private to the object *)
  val mutable lastFileName = ""
  val mutable lastLineNumber = -1

  (* Make sure that you only call self#line_directive on an empty line *)
  method line_directive ?(forcefile=false) fmt l =
    Cil.CurrentLoc.set l;
    match state.line_directive_style with
    | None -> ()
    | Some _ when (fst l).Lexing.pos_lnum <= 0 -> ()

    (* Do not print lineComment if the same line as above *)
    | Some Line_comment_sparse when (fst l).Lexing.pos_lnum = lastLineNumber -> 
      ()

    | Some style  ->
      let directive = match style with
	| Line_comment | Line_comment_sparse -> "//#line "
	| Line_preprocessor_output when not Cil.theMachine.Cil.msvcMode -> "#"
	| Line_preprocessor_output | Line_preprocessor_input -> "#line"
      in
      lastLineNumber <- (fst l).Lexing.pos_lnum;
      let filename =
	if forcefile || (fst l).Lexing.pos_fname <> lastFileName then begin
	  lastFileName <- (fst l).Lexing.pos_fname;
	  " \"" ^ (fst l).Lexing.pos_fname ^ "\""
	end else
	  ""
      in
      fprintf fmt "@[@<0>\n@<0>%s@<0> @<0>%d@<0> @<0>%s@]@\n" 
	directive (fst l).Lexing.pos_lnum filename

  method stmtkind (next: stmt) fmt = function
  | UnspecifiedSequence seq ->
    let print_stmt pstmt fmt (stmt, modifies, writes, reads,_) =
      pstmt fmt stmt;
      if verbose || Kernel.is_debug_key_enabled debug_unspecified then
	Format.fprintf fmt "@ /*effects: @[(%a) %a@ <-@ %a@]*/"
          (Pretty_utils.pp_list ~sep:",@ " self#lval) modifies
	  (Pretty_utils.pp_list ~sep:",@ " self#lval) writes
	  (Pretty_utils.pp_list ~sep:",@ " self#lval) reads
    in
    let rec iterblock fmt = function
      | [] -> ()
      | [ srw ] ->
	fprintf fmt "@ " ;
	print_stmt (self#next_stmt Cil.invalidStmt) fmt srw
      | srw_first :: ((s_next,_,_,_,_) :: _ as tail) ->
	fprintf fmt "@ " ;
	print_stmt (self#next_stmt s_next) fmt srw_first ;
	iterblock fmt tail
    in
    fprintf fmt "@[<v 2>{%t%a@;<1 -2>}@]"
      (if self#display_comment () then 
	  fun fmt -> fprintf fmt "@ @[/*sequence*/@]"
       else ignore)
      iterblock seq;

  | Return(None, l) ->
    fprintf fmt "@[%areturn;@]" (fun fmt -> self#line_directive fmt) l

  | Return(Some e, l) ->
    fprintf fmt "@[%a@[<hv 2>return@ %a;@]@]" 
      (fun fmt -> self#line_directive fmt) l
      self#exp e

  | Goto (sref, l) -> begin
    match pickLabel !sref.labels with
    | Some lbl ->
      fprintf fmt "@[%agoto %s;@]" 
	(fun fmt -> self#line_directive fmt) l 
	lbl
    | None ->
      Kernel.error "Cannot find label for target of goto: %a" 
	(self#without_annot self#stmt) !sref;
      fprintf fmt "@[goto@ __invalid_label;@]"
  end

  | Break l ->
    fprintf fmt "@[%a%s@]" (fun fmt -> self#line_directive fmt) l "break;"

  | Continue l ->
    fprintf fmt "@[%a%s@]" (fun fmt -> self#line_directive fmt) l "continue;"

  | Instr i -> 
    self#instr fmt i

  | If(be,t,{bstmts=[];battrs=[]},l) 
      when not state.print_cil_as_is ->
    fprintf fmt "@[<hv>%a@[<v 2>if (%a) %a@]@]"
      (fun fmt -> self#line_directive ~forcefile:false fmt) l
      self#exp be
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) t

  | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},l)
      when !gref == next && not state.print_cil_as_is ->
    fprintf fmt "@[<hv>%a@[<v 2>if (%a) %a@]@]"
      (fun fmt -> self#line_directive ~forcefile:false fmt) l
      self#exp be
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) t

  | If(be,{bstmts=[];battrs=[]},e,l) 
      when not state.print_cil_as_is ->
    fprintf fmt "@[<hv>%a@[<v 2>if (%a) %a@]@]"
      (fun fmt -> self#line_directive ~forcefile:false fmt) l
      self#exp (Cil.dummy_exp(UnOp(LNot,be,Cil.intType)))
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) e

  | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]}]; battrs=[]},e,l)
      when !gref == next && not state.print_cil_as_is ->
    fprintf fmt "@[<hv>%a@[<v 2>if (%a) %a@]@]"
      (fun fmt -> self#line_directive ~forcefile:false fmt) l
      self#exp (Cil.dummy_exp(UnOp(LNot,be,Cil.intType)))
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) e;

  | If(be,t,e,l) ->
    pp_open_hvbox fmt 0;
    self#line_directive fmt l;
    let braces_then = 
      self#require_braces ~has_annot:false t || self#block_has_dangling_else t
    in
    let else_at_newline = 
      braces_then
      || not (self#inline_block ~has_annot:false t)
      || not (self#inline_block ~has_annot:false e)
      || (* call to a function in both branches (for GUI' status bullets) *)
	(force_brace && self#block_is_function t && self#block_is_function e)
    in
    fprintf fmt "@[<v 2>if (%a) %a@]"
      self#exp be
      (fun fmt -> self#unboxed_block ~has_annot:false ~braces:braces_then fmt) 
      t;
    if else_at_newline then fprintf fmt "@\n" else fprintf fmt "@ ";
    fprintf fmt "@[<v 2>else %a@]" 
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) e;
    pp_close_box fmt ()

  | Switch(e,b,_,l) ->
    fprintf fmt "@[%a@[<v 2>switch (%a) %a@]@]"
      (fun fmt -> self#line_directive ~forcefile:false fmt) l
      self#exp e
      (fun fmt -> self#unboxed_block ~has_annot:false fmt) b

  | Loop(a, b, l, _, _) ->
    Format.pp_open_hvbox fmt 0;
    if logic_printer_enabled && a <> [] then begin
      Pretty_utils.pp_open_block fmt "/*@@ " ;
      Pretty_utils.pp_list ~sep:"@\n" self#code_annotation fmt a;
      Pretty_utils.pp_close_block fmt "@ */@\n" ;
    end;
    ((* Maybe the first thing is a conditional. Turn it into a WHILE *)
      try
	let rec skipEmpty = function
	  | [] -> []
	  | {skind=Instr (Skip _);labels=[]} as h :: rest
	      when self#may_be_skipped h-> skipEmpty rest
	  | x -> x
	in
	let term, bodystmts =
	  (* Bill McCloskey: Do not remove the If if it has labels *)
	  match skipEmpty b.bstmts with
	  | { skind = If(e,tb,fb,_) } as to_skip :: rest
	      when not state.print_cil_as_is
		&& self#may_be_skipped to_skip ->
	    (match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
	    | [], [{ skind = Break _; labels = [] } as s]
              when self#may_be_skipped s ->
              e, rest
	    | [{ skind = Break _; labels = [] } as s], []
              when self#may_be_skipped s ->
	      Cil.dummy_exp (UnOp(LNot, e, Cil.intType)), rest
	    | _ -> raise Not_found)
	  | _ -> raise Not_found
	in
	let b = match skipEmpty bodystmts with
	    [{ skind=Block b} as s ] when self#may_be_skipped s -> b
	  | _ -> { b with bstmts = bodystmts }
	in
	Format.fprintf fmt "%a@[<v 2>while (%a) %a@]"
	  (fun fmt -> self#line_directive fmt) l
	  self#exp term
	  (fun fmt -> self#unboxed_block ~has_annot:false fmt) b;
      with Not_found ->
	Format.fprintf fmt "%a@[<v 2>while (1) %a@]"
	  (fun fmt -> self#line_directive fmt) l
	  (fun fmt -> self#unboxed_block ~has_annot:false fmt) b);
    Format.pp_close_box fmt ()

  | Block b ->
    (* We do not want to put extra braces in presence of blocks included in
       another block (that's often the case). So the following line
       specifically limits the number of braces in that case. But that
       assumes that the required braces have already been put before by the
       callers *)
    let braces = 
      b.blocals <> [] || b.battrs <> [] ||
        (Kernel.is_debug_key_enabled debug_sid) || verbose
      || (self#has_annot 
	  && logic_printer_enabled
	  && (* at least two statements inside *) 
	    match b.bstmts with [] | [ _ ] -> false | _ -> true)
    in
    self#block fmt ~braces b

  | TryFinally (b, h, l) ->
    fprintf fmt "@[%a@[<v 2>__try@ %a@]@ @[<v 2>__finally@ %a@]@]"
      (fun fmt -> self#line_directive fmt) l
      (fun fmt -> self#block fmt) b
      (fun fmt -> self#block fmt) h

  | TryExcept (b, (il, e), h, l) ->
    fprintf fmt "@[%a@[<v 2>__try@ %a@]@ @[<v 2>__except(@\n@["
      (fun fmt -> self#line_directive fmt) l
      (fun fmt -> self#block fmt) b;
    (* Print the instructions but with a comma at the end, instead of
     * semicolon *)
    instr_terminator <- ",";
    Pretty_utils.pp_list ~sep:"@\n" self#instr fmt il;
    instr_terminator <- ";";
    fprintf fmt "%a) @]@ %a@]" self#exp e (fun fmt -> self#block fmt) h

  (*** GLOBALS ***)
  method global fmt (g:global) =
    match g with
    | GFun (fundec, l) ->
      if print_var fundec.svar
      then begin
	self#in_current_function fundec.svar;
	(* If the function has attributes then print a prototype because
	 * GCC cannot accept function attributes in a definition *)
	let oldattr = fundec.svar.vattr in
	(* Always pring the file name before function declarations *)
	(* Prototype first *)
	if oldattr <> [] then
	  (self#line_directive fmt l;
	   fprintf fmt "%a;@\n"
	     self#vdecl_complete fundec.svar);
	(* Temporarily remove the function attributes *)
	fundec.svar.vattr <- [];
	(* Body now *)
	self#line_directive ~forcefile:true fmt l;
	self#fundecl fmt fundec;
	fundec.svar.vattr <- oldattr;
	fprintf fmt "@\n";
	self#out_current_function
      end

    | GType (typ, l) ->
      self#line_directive ~forcefile:true fmt l;
      fprintf fmt "typedef %a;@\n"
	(self#typ (Some (fun fmt -> self#varname fmt typ.tname))) typ.ttype

    | GEnumTag (enum, l) ->
      self#line_directive fmt l;
      if verbose then 
        fprintf fmt "/* Following enum is equivalent to %a */@\n" 
          (self#typ None) 
          (TInt(enum.ekind,[]));
      fprintf fmt "enum@[ %a {@\n%a@]@\n}%a;@\n"
	self#varname enum.ename
	(Pretty_utils.pp_list ~sep:",@\n"
	   (fun fmt item ->
	     fprintf fmt "%s = %a"
	       item.einame
	       self#exp item.eival))
	enum.eitems
	self#attributes enum.eattr

    | GEnumTagDecl (enum, l) -> (* This is a declaration of a tag *)
      self#line_directive fmt l;
      fprintf fmt "enum %a;@\n" self#varname enum.ename

    | GCompTag (comp, l) -> (* This is a definition of a tag *)
      let n = comp.cname in
      let su =
	if comp.cstruct then "struct"
	else "union"
      in
      let sto_mod, rest_attr = Cil.separateStorageModifiers comp.cattr in
      self#line_directive ~forcefile:true fmt l;
      fprintf fmt "@[<3>%s%a %a {@\n%a@]@\n}%a;@\n"
	su
	self#attributes sto_mod
	self#varname n
	(Pretty_utils.pp_list ~sep:"@\n" self#fieldinfo)
	comp.cfields
	self#attributes rest_attr

    | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
      self#line_directive fmt l;
      fprintf fmt "%s %a;@\n"
        (if comp.cstruct then "struct" else "union")
        self#varname comp.cname
    | GVar (vi, io, l) ->
      if print_var vi then begin
	self#line_directive ~forcefile:true fmt l;
        if vi.vghost then Format.fprintf fmt "/*@@ ghost@ ";
	self#vdecl fmt vi;
	(match io.init with
	  None -> ()
	| Some i ->
	  fprintf fmt " = ";
	  let islong =
	    match i with
	      CompoundInit (_, il) when List.length il >= 8 -> true
	    | _ -> false
	  in
	  if islong then begin
	    self#line_directive fmt l;
	    fprintf fmt "  @[@\n"
	  end;
	  self#init fmt i;
	  if islong then fprintf fmt "@]");
	fprintf fmt ";%t@\n" 
          (if vi.vghost then fun fmt -> Format.fprintf fmt "@ */" else ignore)
      end
    (* print global variable 'extern' declarations, and function prototypes *)
    | GVarDecl (funspec, vi, l) ->
      if print_var vi then begin
	if Cil.isFunctionType vi.vtype then self#in_current_function vi;
	self#opt_funspec fmt funspec;
	if not state.print_cil_as_is && Cil.Builtin_functions.mem vi.vname 
	then begin
	  (* Compiler builtins need no prototypes. Just print them in
	     comments. *)
	  fprintf fmt "/* compiler builtin: @\n   %a;   */@\n"
	    self#vdecl vi
	end else begin
	  self#line_directive fmt l;
	  fprintf fmt "%a@\n@\n" self#vdecl_complete vi
	end;
	if Cil.isFunctionType vi.vtype then self#out_current_function
      end

    | GAsm (s, l) ->
      self#line_directive fmt l;
      fprintf fmt "__asm__(\"%s\");@\n" (Escape.escape_string s)

    | GPragma (Attr(an, args), l) ->
      (* sm: suppress printing pragmas that gcc does not understand *)
      (* assume anything starting with "ccured" is ours *)
      (* also don't print the 'combiner' pragma *)
      (* nor 'cilnoremove' *)
      let suppress =
	not state.print_cil_input
	&& not Cil.theMachine.Cil.msvcMode
	&& (Cil.startsWith "box" an
	    || Cil.startsWith "ccured" an
	    || an = "merger" 
	      || an = "cilnoremove")
      in
      self#line_directive fmt l;
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
	    (Pretty_utils.pp_list ~sep:" " self#attrparam) args
	| _ ->
	  fprintf fmt "%s(%a)"
	    an
	    (Pretty_utils.pp_list ~sep:"," self#attrparam) args

      end;
      if suppress then  fprintf fmt " */@\n" else fprintf fmt "@\n"

    | GPragma (AttrAnnot _, _) ->
      assert false
    (*        self#line_directive fmt l;
	      fprintf fmt "/* #pragma %s */@\n" a*)

    | GAnnot (decl,l) ->
      self#line_directive fmt l;
      fprintf fmt "/*@@@ %a@ */@\n"
	self#global_annotation decl

    | GText s  ->
      if s <> "//" then
	fprintf fmt "%s@\n" s

  method fieldinfo fmt fi =
    fprintf fmt "%a %s%a;"
      (self#typ
	 (Some (fun fmt -> 
	   if fi.fname <> Cil.missingFieldName then
	     fprintf fmt "%s" fi.fname)))
      fi.ftype
      (match fi.fbitfield with
      | None -> ""
      | Some i -> ": " ^ string_of_int i ^ " ")
      self#attributes fi.fattr

  method private opt_funspec fmt funspec =
    if logic_printer_enabled && not (Cil.is_empty_funspec funspec) then
      fprintf fmt "@[<hv 1>/*@@ %a@ */@]@\n" self#funspec funspec

  method private fundecl fmt f =
    (* declaration. *)
    let was_ghost = is_ghost in
    let entering_ghost = f.svar.vghost && not was_ghost in
    fprintf fmt "@[%t%a@\n@[<v 2>"
      (if entering_ghost then fun fmt -> Format.fprintf fmt "/*@@ ghost@ " 
       else ignore)
      self#vdecl f.svar;
    (* We take care of locals in blocks. *)
    (*List.iter (fprintf fmt "@\n%a;" self#vdecl) f.slocals ;*)
    (* body. *)
    if entering_ghost then is_ghost <- true;
    self#unboxed_block ~has_annot:false ~braces:true fmt f.sbody;
    if entering_ghost then is_ghost <- false;
    fprintf fmt "@]%t@]@."
      (if entering_ghost then fun fmt -> Format.fprintf fmt "@ */" else ignore)

  (***** PRINTING DECLARATIONS and TYPES ****)

  method storage fmt c =
    fprintf fmt "%s"
      (match c with
      | NoStorage -> ""
      | Static ->  "static "
      | Extern -> "extern "
      | Register -> "register ")

  method fkind fmt = function
  | FFloat -> fprintf fmt "float"
  | FDouble -> fprintf fmt "double"
  | FLongDouble -> fprintf fmt "long double"

  method ikind fmt c =
    fprintf fmt "%s"
      (match c with
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
	if Cil.theMachine.Cil.msvcMode then "__int64" else "long long"
      | IULongLong ->
	if Cil.theMachine.Cil.msvcMode then "unsigned __int64"
	else "unsigned long long")

  method typ ?fundecl nameOpt
    fmt (t:typ) =
    let pname fmt space = match nameOpt with
      | None -> ()
      | Some d -> Format.fprintf fmt "%s%t" (if space then " " else "") d
    in
    let printAttributes fmt (a: attributes) =
      match nameOpt with
      | None when not state.print_cil_input
	  && not Cil.theMachine.Cil.msvcMode ->
	()
      (* Cannot print the attributes in this case because gcc does not like them
	 here, except if we are printing for CIL, or for MSVC.  In fact, for
	 MSVC we MUST print attributes such as __stdcall *)
      (* if pa = nil then nil else text "/*" ++ pa ++ text "*/"*)
      | _ ->  self#attributes fmt a
    in
    match t with
    | TVoid a -> fprintf fmt "void%a%a" self#attributes a pname true

    | TInt (ikind,a) ->
      fprintf fmt "%a%a%a" self#ikind ikind self#attributes a pname true

    | TFloat(fkind, a) ->
      fprintf fmt "%a%a%a" self#fkind fkind self#attributes a pname true

    | TComp (comp, _, a) -> (* A reference to a struct *)
      fprintf fmt
	"%s %a%a%a"
	(if comp.cstruct then "struct" else "union")
	self#varname comp.cname
	self#attributes a
	pname true

    | TEnum (enum, a) ->
      fprintf fmt "enum %a%a%a"
	self#varname enum.ename
	self#attributes a
	pname true

    | TPtr (bt, a) ->
      (* Parenthesize the ( * attr name) if a pointer to a function or an
       * array. However, on MSVC the __stdcall modifier must appear right
       * before the pointer constructor "(__stdcall *f)". We push them into
       * the parenthesis. *)
      let (paren: (formatter -> unit) option), (bt': typ) =
	match bt with
	| TFun(rt, args, isva, fa) when Cil.theMachine.Cil.msvcMode ->
	  let an, af', at = Cil.partitionAttributes ~default:Cil.AttrType fa in
	    (* We take the af' and we put them into the parentheses *)
	  Some
	    (fun fmt ->
	      fprintf fmt
		"(%a"
		printAttributes af'),
	  TFun(rt, args, isva, Cil.addAttributes an at)
	| TFun _ | TArray _ -> (Some (fun fmt -> fprintf fmt "(")), bt
	| _ -> None, bt
      in
      let name' = 
	fun fmt -> fprintf fmt "*%a%a" printAttributes a pname (a <> [])
      in
      let name'' =
	fun fmt ->
	  (* Put the parenthesis *)
	  match paren with
	  | Some p -> fprintf fmt "%t%t)" p name'
	  | None -> fprintf fmt "%t" name'
      in
      self#typ (Some name'') fmt bt'

    | TArray (elemt, lo, _, a) ->
      (* qualifiers attributes are not supposed to be on the TArray,
         but on the base type. (Besides, GCC and Clang do not parse the
         result if the qualifier is misplaced. *)
      let atts_elem, a = Cil.splitArrayAttributes a in
      if atts_elem != [] then
        Kernel.failure ~current:true 
	  "Found some incorrect attributes for array (%a). Please report." 
	  self#attributes atts_elem;
      let name' fmt =
	if a = [] then pname fmt false
        else if nameOpt = None then
	  printAttributes fmt a
	else
	  fprintf fmt "(%a%a)" printAttributes a pname true
      in
      self#typ
	(Some (fun fmt ->
	  fprintf fmt "%t[%t]"
	    name'
	    (fun fmt ->
	      match lo with
	      | None -> ()
	      | Some e -> self#exp fmt e)
	 ))
	fmt
	elemt

    | TFun (restyp, args, isvararg, a) ->
      let name' fmt =
	if a = [] then pname fmt false 
	else if nameOpt = None then printAttributes fmt a
 	else fprintf fmt "(%a%a)" printAttributes a pname (a <> [])
      in
      let module Args
	  (A:sig 
	    type t
	    val args: t list option
	    val pp_args: Format.formatter -> t -> unit
	  end)
	  =
	  struct
	    let pp_prms fmt =
	      fprintf fmt "%t(@[%t@])" name'
		(fun fmt ->
		  match A.args with
		  | (None | Some []) when isvararg -> fprintf fmt "..."
		  | None -> ()
		  | Some [] -> fprintf fmt "void"
		  | Some args ->
		    Pretty_utils.pp_list ~sep:",@ " A.pp_args fmt args;
		    if isvararg then fprintf fmt "@ , ...")
	  end
      in
      let pp_prms = match fundecl with
	| None ->
	  let module Args =
		Args(struct
		  type t = (string * typ * attributes)
		  let args = args
		  let pp_args fmt (aname,atype,aattr) =
		    let stom, rest = Cil.separateStorageModifiers aattr in
		    (* First the storage modifiers *)
		    fprintf fmt
		      "%a%a%a"
		      self#attributes stom
		      (self#typ
			 (Some (fun fmt -> fprintf fmt "%s" aname))) 
		      atype
                      self#attributes rest
		end)
	  in 
	  Args.pp_prms
	| Some fundecl ->
	  let module Args =
		Args(struct
		  type t = varinfo
		  let args =
		    try Some (Cil.getFormalsDecl fundecl)
		    with Not_found -> None
		  let pp_args = self#vdecl
		end)
	  in 
	  Args.pp_prms
      in
      self#typ (Some pp_prms) fmt restyp
    | TNamed (t, a) ->
      fprintf fmt "%a%a%a"
	self#varname t.tname
	self#attributes a
	pname true

    | TBuiltin_va_list a ->
      fprintf fmt "__builtin_va_list%a%a"
	self#attributes a
	pname true

  (**** PRINTING ATTRIBUTES *********)
  method attributes fmt a = self#attributesGen false fmt a
    
  (* Print one attribute. Return also an indication whether this attribute
     should be printed inside the __attribute__ list *)
  method attribute fmt = function
  | Attr(an, args) ->
    (* Recognize and take care of some known cases *)
    (match an, args with
    | "const", [] -> fprintf fmt "const"; false
    (* Put the aconst inside the attribute list *)
    | "aconst", [] when not Cil.theMachine.Cil.msvcMode ->
      fprintf fmt "__const__"; true
    | "thread", [] when not Cil.theMachine.Cil.msvcMode ->
      fprintf fmt "__thread"; false
    (*
      | "used", [] when not !msvcMode -> text "__attribute_used__", false
     *)
    | "volatile", [] -> fprintf fmt "volatile"; false
    | "restrict", [] -> fprintf fmt "__restrict"; false
    | "missingproto", [] -> 
      if self#display_comment () then fprintf fmt "/* missing proto */"; 
      false
    | "cdecl", [] when Cil.theMachine.Cil.msvcMode -> 
      fprintf fmt "__cdecl"; false
    | "stdcall", [] when Cil.theMachine.Cil.msvcMode ->
      fprintf fmt "__stdcall"; false
    | "fastcall", [] when Cil.theMachine.Cil.msvcMode -> 
      fprintf fmt "__fastcall"; false
    | "declspec", args when Cil.theMachine.Cil.msvcMode ->
      fprintf fmt "__declspec(%a)"
	(Pretty_utils.pp_list ~sep:"" self#attrparam) args;
      false
    | "w64", [] when Cil.theMachine.Cil.msvcMode -> 
      fprintf fmt "__w64"; false
    | "asm", args ->
      fprintf fmt "__asm__(%a)"
	(Pretty_utils.pp_list ~sep:"" self#attrparam) args;
      false
    (* we suppress printing mode(__si__) because it triggers an
       internal compiler error in all current gcc versions 
       sm: I've now encountered a problem with mode(__hi__)...
       I don't know what's going on, but let's try disabling all "mode". *)
    | "mode", [ACons(tag,[])] ->
      if self#display_comment () then fprintf fmt "/* mode(%s) */" tag;
      false
	
    (* sm: also suppress "format" because we seem to print it in 
       a way gcc does not like *)
    | "format", _ -> 
      if self#display_comment () then fprintf fmt "/* format attribute */";
      false

    | "hidden", _ -> (* hidden attribute list *)
      false
    (* sm: here's another one I don't want to see gcc warnings about.. *)
    | "mayPointToStack", _ when not state.print_cil_input ->
      (* [matth: may be inside another comment.]
	 -> text "/*mayPointToStack*/", false *)
      false
	
    | "arraylen", [a] ->
      if self#display_comment () then fprintf fmt "/*[%a]*/" self#attrparam a;
      false
    | "static",_ -> 
      if self#display_comment () then fprintf fmt "/* static */"; false
    | "", _ ->
      fprintf fmt "%a "
	(Pretty_utils.pp_list ~sep:" " self#attrparam) args;
      true
    | s, _ when s = Cil.bitfield_attribute_name &&
             not Cil.miscState.Cil.printCilAsIs ->
      false
    | _ -> (* This is the dafault case *)
      (* Add underscores to the name *)
      let an' =
	if Cil.theMachine.Cil.msvcMode then "__" ^ an else "__" ^ an ^ "__"
      in
      (match args with
      | [] -> fprintf fmt "%s" an'
      | _ :: _ ->
	fprintf fmt "%s(%a)"
	  an'
	  (Pretty_utils.pp_list ~sep:"," self#attrparam) args);
      true)
  | AttrAnnot s ->
    fprintf fmt "%s" (Cil.mkAttrAnnot s); false

  method private attribute_prec (contextprec: int) fmt (a: attrparam) =
    let thisLevel = Precedence.getParenthLevelAttrParam a in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == Precedence.bitwiseLevel then
	 (* quiet down some GCC warnings *)
	thisLevel == Precedence.additiveLevel
	|| thisLevel == Precedence.comparativeLevel
      else
	false
    in
    if needParens then fprintf fmt "(%a)" self#attrparam a
    else self#attrparam fmt a

  method attrparam fmt a =
    let level = Precedence.getParenthLevelAttrParam a in
    match a with
    | AInt n -> fprintf fmt "%a" Datatype.Big_int.pretty n
    | AStr s -> fprintf fmt "\"%s\"" (Escape.escape_string s)
    | ACons(s, []) -> fprintf fmt "%s" s
    | ACons(s,al) ->
      fprintf fmt "%s(%a)"
	s
	(Pretty_utils.pp_list ~sep:"" self#attrparam) al
    | ASizeOfE a -> fprintf fmt "sizeof(%a)" self#attrparam a
    | ASizeOf t -> fprintf fmt "sizeof(%a)" (self#typ None) t
    | AAlignOfE a -> fprintf fmt "__alignof__(%a)" self#attrparam a
    | AAlignOf t -> fprintf fmt "__alignof__(%a)" (self#typ None) t
    | AUnOp(u,a1) ->
      fprintf fmt "%a %a" self#unop u (self#attribute_prec level) a1
    | ABinOp(b,a1,a2) ->
      fprintf fmt "@[(%a)%a@  (%a) @]"
	(self#attribute_prec level) a1
	self#binop b
	(self#attribute_prec level) a2
    | ADot (ap, s) ->
      fprintf fmt "%a.%s" self#attrparam ap s
    | AStar a1 ->
      fprintf fmt "(*%a)" (self#attribute_prec Precedence.derefStarLevel) a1
    | AAddrOf a1 ->
      fprintf fmt "& %a" (self#attribute_prec Precedence.addrOfLevel) a1
    | AIndex (a1, a2) ->
      fprintf fmt "%a[%a]" self#attrparam a1 self#attrparam a2
    | AQuestion (a1, a2, a3) ->
      fprintf fmt "%a ? %a : %a"
	self#attrparam a1
	self#attrparam a2
	self#attrparam a3

  (* A general way of printing lists of attributes *)
  method private attributesGen (block: bool) fmt (a: attributes) =
    (* Scan all the attributes and separate those that must be printed inside
       the __attribute__ list *)
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
		  (Cil.forgcc "/*")
	     else
                fprintf fmt " __attribute__((");
	    Pretty_utils.pp_list ~sep:",@ "
              Format.pp_print_string fmt in__attr__;
	    fprintf fmt ")%s"
              (if block then Cil.forgcc "*/" else ")")
          end
      | x :: rest ->
        let buff = Buffer.create 17 in
        let local_fmt = formatter_of_buffer buff in
        let ina = self#attribute local_fmt x in
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

  (* ******************************************************************* *)
  (* Logic annotations printer *)
  (* ******************************************************************* *)

  method logic_constant fmt = function
  | Integer(_, Some s) when print_as_source s ->
    fprintf fmt "%s" s (* Always print the text if there is one, unless
                          we want to print it as hexa *)
  | Integer(i, _) ->  Datatype.Big_int.pretty fmt i
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
   (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" -- the former
      has 7 wide characters and the later has 3. *)
  | LChr(c) -> fprintf fmt "'%s'" (Escape.escape_char c)
  | LReal(r) -> fprintf fmt "%s" r.r_literal
  | LEnum {einame = s} -> fprintf fmt "%s" s


  method logic_type name fmt =
    let pname = match name with
      | Some d -> (fun fmt -> Format.fprintf fmt "@ %t" d)
      | None -> fun _ -> ()
    in
    function
    | Ctype typ -> self#typ name fmt typ
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
      fprintf fmt "%a%a%t" self#varname s.lt_name
	((* the space avoids the issue of list<list<int>> where the double >
	    would be read as a shift. It could be optimized away in most of
	    the cases. *)
	  Pretty_utils.pp_list ~pre:"<@[" ~sep:",@ " ~suf:"@]>@ "
	    (self#logic_type None)) l pname
    | Larrow (args,rt) ->
      fprintf fmt "@[@[<2>{@ %a@]}@]%a%t"
	(Pretty_utils.pp_list ~sep:",@ " (self#logic_type None)) args
	(self#logic_type None) rt pname
    | Lvar s -> fprintf fmt "%a%t" self#varname s pname

  method private name fmt s =
    if needs_quote s then Format.fprintf fmt "\"%s\"" s
    else Format.pp_print_string fmt s

  method private term_prec contextprec fmt e =
    let thisLevel = Precedence.getParenthLevelLogic e.term_node in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == Precedence.bitwiseLevel then
	(* quiet down some GCC warnings *)
	thisLevel == Precedence.additiveLevel 
	|| thisLevel == Precedence.comparativeLevel
      else
	false
    in
    if needParens then fprintf fmt "@[<hov 2>(%a)@]" self#term e
    else self#term fmt e

  method identified_term fmt t = self#term fmt t.it_content
    
  method term fmt t =
    if Kernel.is_debug_key_enabled debug_logic_types then begin 
      fprintf fmt "/* type:%a */" (self#logic_type None) t.term_type;
    end;
    match t.term_name with
    | [] -> self#term_node fmt t
    | _ :: _ ->
      fprintf fmt "(@[%a:@ %a@])"
	(Pretty_utils.pp_list ~sep:":@ " self#name) t.term_name
	self#term_node t

  (* This instance variable is true the pretty-printed term is not inside 
     an \at. Hence one may not pretty-print useless Here labels. *)
  val mutable current_label = Logic_const.here_label

  method term_binop fmt b =
    fprintf fmt "%s"
      (match b with
      | PlusA | PlusPI | IndexPI -> "+"
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

  method relation fmt b =
    fprintf fmt "%s"
      (match b with
      | Rlt -> "<"
      | Rgt -> ">"
      | Rle -> if Kernel.Unicode.get () then Utf8_logic.le else "<="
      | Rge -> if Kernel.Unicode.get () then Utf8_logic.ge else ">="
      | Req -> if Kernel.Unicode.get () then Utf8_logic.eq else "=="
      | Rneq -> if Kernel.Unicode.get () then Utf8_logic.neq else "!=")

  method term_node fmt t =
    let current_level = Precedence.getParenthLevelLogic t.term_node in
    match t.term_node with
    | TConst s -> fprintf fmt "%a" self#logic_constant s
    | TDataCons(ci,args) ->
      fprintf fmt "%a%a" self#varname ci.ctor_name
	(Pretty_utils.pp_list ~pre:"(@[" ~suf:"@])" ~sep:",@ " self#term) 
	args
    | TLval lv -> fprintf fmt "%a" (self#term_lval_prec current_level) lv
    | TSizeOf t -> fprintf fmt "sizeof(%a)" (self#typ None) t
    | TSizeOfE e -> fprintf fmt "sizeof(%a)" self#term e
    | TSizeOfStr s -> fprintf fmt "sizeof(%S)" s
    | TAlignOf e -> fprintf fmt "alignof(%a)" (self#typ None) e
    | TAlignOfE e -> fprintf fmt "alignof(%a)" self#term e
    | TUnOp (op,e) -> fprintf fmt "%a%a"
      self#unop op (self#term_prec current_level) e
    | TBinOp (op,l,r) ->
      fprintf fmt "%a%a%a"
	(self#term_prec current_level) l
	self#term_binop op
	(self#term_prec current_level) r
    | TCastE (ty,e) ->
      fprintf fmt "(%a)%a" (self#typ None) ty
	(self#term_prec current_level) e
    | TAddrOf lv -> 
      fprintf fmt "&%a" (self#term_lval_prec Precedence.addrOfLevel) lv
    | TStartOf lv -> fprintf fmt "(%a)%a"
      (self#logic_type None) t.term_type
      (self#term_lval_prec current_level) lv
    | Tapp (f, labels, tl) -> 
      fprintf fmt "%a%a%a"
	self#logic_info f
	self#labels (List.map snd labels)
	(Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ " self#term) tl
    | Tif (cond,th,el) ->
      fprintf fmt "@[<2>%a?@;%a:@;%a@]"
	(self#term_prec current_level) cond
	(self#term_prec current_level) th
	(self#term_prec current_level) el
    | Tat (t,StmtLabel sref) ->
      let rec pickLabel = function
	| [] -> None
	| Label (l, _, _) :: _ -> Some l
	| _ :: rest -> pickLabel rest
      in 
      let l = match pickLabel !sref.labels with
	| Some l -> l
	| None -> Kernel.fatal "Cannot find label for \\at";
      in
      fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]" self#term t l
    | Tat (t,(LogicLabel (_, l) as lab)) ->
      let old_label = current_label in
      current_label <- lab;
      begin
	if lab = Logic_const.old_label then
	  fprintf fmt "@[\\old(@[%a@])@]" self#term t
	else
	  fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]" self#term t l
      end;
      current_label <- old_label
	
    | Toffset (l,t) -> 
      fprintf fmt "\\offset%a(%a)" self#labels [l] self#term t
    | Tbase_addr (l,t) -> 
      fprintf fmt "\\base_addr%a(%a)" self#labels [l] self#term t
    | Tblock_length (l,t) -> 
      fprintf fmt "\\block_length%a(%a)" self#labels [l] self#term t
    | Tnull -> fprintf fmt "\\null"
    | TCoerce (e,ty) ->
      fprintf fmt "%a@ :>@ %a"
	(self#term_prec current_level) e (self#typ None) ty
    | TCoerceE (e,ce) ->
      fprintf fmt "%a :> %a"
	(self#term_prec current_level) e (self#term_prec current_level) ce
    | TUpdate (t,toff,v) ->
      fprintf fmt "{%a \\with %a = %a}"
	self#term t
	self#term_offset toff
	self#term v
    | Tlambda(prms,expr) ->
      fprintf fmt "@[<2>\\lambda@ %a;@ %a@]"
	self#quantifiers prms (self#term_prec current_level) expr
    | Ttypeof t -> fprintf fmt "\\typeof(%a)" self#term t
    | Ttype ty -> fprintf fmt "\\type(%a)" (self#typ None) ty
    | Tunion locs ->
      fprintf fmt "@[<hov 2>\\union(@,%a)@]"
	(Pretty_utils.pp_list ~sep:",@ " self#term) locs
    | Tinter locs ->
      fprintf fmt "@[<hov 2>\\inter(@,%a)@]"
	(Pretty_utils.pp_list ~sep:",@ " self#term) locs
    | Tempty_set -> pp_print_string fmt "\\empty"
    | Tcomprehension(lv,quant,pred) ->
      fprintf fmt "{@[%a@ |@ %a%a@]}"
	self#term lv self#quantifiers quant
	(Pretty_utils.pp_opt
	   (fun fmt p -> fprintf fmt ";@ %a" self#identified_pred p))
	pred
    | Trange(low,high) ->
      let pp_term = self#term_prec current_level in
      fprintf fmt "@[%a..%a@]"
	(Pretty_utils.pp_opt
           (fun fmt v -> Format.fprintf fmt "%a " pp_term v)) low
	(Pretty_utils.pp_opt
           (fun fmt v -> Format.fprintf fmt "@ %a" pp_term v)) high;
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
	| LBterm t -> fun fmt -> self#term fmt t
	| LBpred p -> fun fmt -> self#predicate_named fmt p
	| LBnone
	| LBreads _ | LBinductive _ -> 
	  Kernel.fatal "invalid logic local definition"
      in
      fprintf fmt "@[\\let@ %a@ =@ %t%t;@ %a@]"
	self#logic_var v
	(fun fmt -> if args <> [] then
	    fprintf fmt "@[<2>\\lambda@ %a;@]@ " self#quantifiers args)
	pp_defn
	(self#term_prec current_level) body
    | TLogic_coerce(ty,t) ->
      if Kernel.is_debug_key_enabled debug_logic_coercions then
        fprintf fmt "/* coercion to:@[%a@] */" (self#logic_type None) ty;
      self#term_prec current_level fmt t
	
  method private term_lval_prec contextprec fmt lv =
    if Precedence.getParenthLevelLogic (TLval lv) > contextprec then
      fprintf fmt "(%a)" self#term_lval lv
    else
      fprintf fmt "%a" self#term_lval lv

  method term_lval fmt lv = match lv with
  | TVar vi, o -> fprintf fmt "%a%a" self#logic_var vi self#term_offset o
  | TResult _, o -> fprintf fmt "\\result%a" self#term_offset o
  | TMem e, TField(fi,o) ->
    fprintf fmt "%a->%a%a" (self#term_prec Precedence.arrowLevel) e
      self#varname fi.fname self#term_offset o
  | TMem e, TNoOffset ->
    fprintf fmt "*%a" (self#term_prec Precedence.derefStarLevel) e
  | TMem e, o ->
    fprintf fmt "(*%a)%a"
      (self#term_prec Precedence.derefStarLevel) e self#term_offset o
      
  method model_field fmt mi = self#varname fmt mi.mi_name

  method term_offset fmt o = match o with
  | TNoOffset -> ()
  | TField (fi,o) ->
    fprintf fmt ".%a%a" self#field fi self#term_offset o
  | TModel (mi,o) ->
    fprintf fmt ".%a%a" self#model_field mi self#term_offset o
  | TIndex(e,o) -> fprintf fmt "[%a]%a" self#term e self#term_offset o
    
  method logic_info fmt li = self#logic_var fmt li.l_var_info
  method logic_var fmt v = self#varname fmt v.lv_name

  method quantifiers fmt l =
    Pretty_utils.pp_list ~sep:",@ "
      (fun fmt lv ->
	let pvar fmt = self#logic_var fmt lv in
	self#logic_type (Some pvar) fmt lv.lv_type)
      fmt l

  method private pred_prec fmt (contextprec,p) =
    let thisLevel = Precedence.getParenthLevelPred p in
    let needParens = thisLevel >= contextprec in
    if needParens then fprintf fmt "@[<hov 2>(%a)@]" self#predicate p
    else self#predicate fmt p
      
  method private named_pred fmt (parenth, names, content) =
    match names with
    | [] -> self#pred_prec fmt (parenth,content)
    | _ :: _ ->
      if parenth = Precedence.upperLevel then
	fprintf fmt "@[<hv 2>%a:@ %a@]"
	  (Pretty_utils.pp_list ~sep:":@ " self#name) names
	  self#pred_prec (Precedence.upperLevel, content)
      else
	fprintf fmt "(@[<hv 2>%a:@ %a@])"
	  (Pretty_utils.pp_list ~sep:":@ " self#name) names
	  self#pred_prec (Precedence.upperLevel, content)
	  
  method private identified_pred fmt p =
    self#named_pred fmt (Precedence.upperLevel, p.name, p.content)

  method private pred_prec_named fmt (parenth,p) =
    self#named_pred fmt (parenth,p.name,p.content)

  method predicate_named fmt p =
    self#named_pred fmt (Precedence.upperLevel, p.name, p.content)

  method identified_predicate fmt p =
    if verbose then fprintf fmt "/* ip:%d */" p.ip_id;
    self#predicate_named fmt (Logic_const.pred_of_id_pred p)

  method private preds kw fmt l =
    Pretty_utils.pp_list ~suf:"@]@\n" ~sep:"@\n"
      (fun fmt p ->
	fprintf fmt "@[%s %a;@]" kw self#identified_predicate p) fmt l
      
  method predicate fmt p =
    let current_level = Precedence.getParenthLevelPred p in
    let term = self#term_prec current_level in
    match p with
    | Pfalse -> pp_print_string fmt "\\false"
    | Ptrue -> pp_print_string fmt "\\true"
    | Papp (p,labels,l) -> 
      fprintf fmt "@[%a%a%a@]"
	self#logic_info p
	self#labels (List.map snd labels)
	(Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ " self#term) l
    | Prel (rel,l,r) ->
      fprintf fmt "@[%a@ %a@ %a@]" term l self#relation rel term r
    | Pand (p1, p2) ->
      fprintf fmt "@[%a@ %a@ %a@]"
	self#pred_prec_named (current_level,p1)
	self#term_binop LAnd
	self#pred_prec_named (current_level,p2)
    | Por (p1, p2) ->
      fprintf fmt "@[%a@ %a@ %a@]"
	self#pred_prec_named (current_level,p1)
	self#term_binop LOr
	self#pred_prec_named (current_level,p2)
    | Pxor (p1, p2) ->
      fprintf fmt "@[%a@ %s@ %a@]"
	self#pred_prec_named (current_level,p1)
	(if Kernel.Unicode.get () then Utf8_logic.x_or else "^^")
	self#pred_prec_named (current_level,p2)
    | Pimplies (p1,p2) ->
      fprintf fmt "@[%a@ %s@ %a@]"
	self#pred_prec_named (current_level,p1)
	(if Kernel.Unicode.get () then Utf8_logic.implies else "==>")
	self#pred_prec_named (current_level,p2)
    | Piff (p1,p2) ->
      fprintf fmt "@[%a@ %s@ %a@]"
	self#pred_prec_named (current_level,p1)
	(if Kernel.Unicode.get () then Utf8_logic.iff else "<==>")
	self#pred_prec_named (current_level,p2)
    | Pnot a -> fprintf fmt "@[%s%a@]"
      (if Kernel.Unicode.get () then Utf8_logic.neg else "!")
      self#pred_prec_named (current_level,a)
    | Pif (e, p1, p2) ->
      fprintf fmt "@[<hv 2>%a?@ %a:@ %a@]"
	term e
	self#pred_prec_named (current_level, p1)
	self#pred_prec_named (current_level, p2)
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
	| LBterm t -> fun fmt -> self#term fmt t
	| LBpred p -> fun fmt -> self#pred_prec_named fmt (current_level,p)
	| LBnone
	| LBreads _ | LBinductive _ -> 
	  Kernel.fatal "invalid logic local definition"
      in
      fprintf fmt "@[\\let@ %a@ =@ %t%t;@ %a@]"
	self#logic_var v
	(fun fmt ->
	  if args <> [] then
	    fprintf fmt "@[<hv 2>\\lambda@ %a;@]@ " self#quantifiers args)
	pp_defn
	self#pred_prec_named (current_level,p)
    | Pforall (quant,pred) ->
      fprintf fmt "@[<hv 2>@[%s %a;@]@ %a@]"
	(if Kernel.Unicode.get () then Utf8_logic.forall else "\\forall")
	self#quantifiers quant self#pred_prec_named (current_level,pred)
    | Pexists (quant,pred) ->
      fprintf fmt "@[<hv 2>@[%s %a;@]@ %a@]"
	(if Kernel.Unicode.get () then  Utf8_logic.exists else "\\exists")
	self#quantifiers quant self#pred_prec_named (current_level,pred)
    | Pfreeable (l,p) ->
      fprintf fmt "@[\\freeable%a(@[%a@])@]" self#labels [l] self#term p
    | Pallocable (l,p) ->
      fprintf fmt "@[\\allocable%a(@[%a@])@]" self#labels [l] self#term p
    | Pvalid (l,p) -> 
      fprintf fmt "@[\\valid%a(@[%a@])@]" self#labels [l] self#term p
    | Pvalid_read (l,p) ->
      fprintf fmt "@[\\valid_read%a(@[%a@])@]" self#labels [l] self#term p
    | Pinitialized (l,p) ->
      fprintf fmt "@[\\initialized%a(@[%a@])@]" self#labels [l] self#term p
    | Pfresh (l1,l2,e1,e2) -> 
      fprintf fmt "@[\\fresh%a(@[%a@],@[%a@])@]" 
	self#labels [l1;l2] self#term e1 self#term e2
    | Pseparated seps ->
      fprintf fmt "@[<hv 2>\\separated(@,%a@,)@]"
	(Pretty_utils.pp_list ~sep:",@ " self#term) seps
    | Pat (p,StmtLabel sref) ->
      let rec pickLabel = function
	| [] -> Kernel.fatal "Cannot find label for \\at"
	| Label (l, _, _) :: _ -> l
	| _ :: rest -> pickLabel rest
      in
      let l = pickLabel !sref.labels in
      fprintf fmt "@[\\at(@[@[%a@],@,@[%s@]@])@]"
	self#pred_prec_named (Precedence.upperLevel, p) l
    | Pat(p,(LogicLabel (_, s) as lab)) ->
      if lab = Logic_const.old_label then
	fprintf fmt "@[\\old(@[%a@])@]" 
	  self#pred_prec_named (Precedence.upperLevel,p)
      else
	fprintf fmt "@[\\at(@[@[%a@],@,%s@])@]"
	  self#pred_prec_named (Precedence.upperLevel,p) s
    | Psubtype (e,ce) ->
      fprintf fmt "@[%a@ <:@ %a@]" term e term ce
	
  method private decrement kw fmt (t, rel) = match rel with
  | None -> fprintf fmt "@[<2>%s@ %a;@]" kw self#term t
  | Some str ->
    (*TODO: replace this string with an interpreted variable*)
    fprintf fmt "@[<2>%s@ %a@ for@ %s;@]" kw self#term t str

  method decreases fmt v = self#decrement "decreases" fmt v
  method variant fmt v = self#decrement "loop variant" fmt v

  method assumes fmt p =
    fprintf fmt "@[<hov 2>assumes@ %a;@]" self#identified_predicate p
      
  method requires fmt p =
    fprintf fmt "@[<hov 2>requires@ %a;@]" self#identified_predicate p
      
  method post_cond fmt (k,p) =
    let kw = get_termination_kind_name k in
    fprintf fmt "@[<hov 2>%s@ %a;@]" kw self#identified_predicate p

  method terminates fmt p =
    fprintf fmt "@[<hov 2>terminates@ %a;@]" self#identified_predicate p
      
  method private cd_behaviors fmt kind p =
    fprintf fmt "@[%s behaviors %a;@]"
      kind
      (Pretty_utils.pp_list ~pre:"@[<hv 0>" ~sep:",@ " pp_print_string)
      p

  method complete_behaviors fmt p = self#cd_behaviors fmt "complete" p
  method disjoint_behaviors fmt p = self#cd_behaviors fmt "disjoint" p

  method allocation ~isloop fmt = function
  | FreeAllocAny -> ()
  | FreeAlloc([],[]) -> 
    fprintf fmt "@[%sallocates@ \\nothing;@]" (if isloop then "loop " else "")
  | FreeAlloc(f,a) ->
    let pFreeAlloc kw fmt = function
      | [] -> ()
      | _ :: _ as af -> 
	fprintf fmt "@[%s%s@ %a;@]"
	  (if isloop then "loop " else "")
	  kw
	  (Pretty_utils.pp_list ~sep:",@ " self#identified_term) af
    in
    fprintf fmt "@[<v>%a%(%)%a@]" 
      (pFreeAlloc "frees") f
      (if f != [] && a != [] then format_of_string "@ " else "")
      (pFreeAlloc "allocates") a
      
  method assigns kw fmt = function
  | WritesAny -> ()
  | Writes [] -> fprintf fmt "@[%s \\nothing;@]" kw
  | Writes l ->
    let without_result =
      List.filter
	(function (a,_) -> not (Logic_const.is_exit_status a.it_content))
        l
    in
    fprintf fmt "@[<h>%s%a@]"
      (if without_result == [] then "" else kw ^ " ")
      (Pretty_utils.pp_list ~sep:",@ " ~suf:";@]"
	 (fun fmt (t, _) -> self#identified_term fmt t))
      without_result
      
  method private assigns_deps kw fmt = function
  | WritesAny -> ()
  | Writes [] as a -> self#assigns kw fmt a
  | Writes l as a ->
    fprintf fmt "@[<v>%a%a@]"
      (self#assigns kw) a
      (Pretty_utils.pp_list ~pre:"@ @[" ~sep:"@\n" (self#from kw))
      (List.filter (fun (_, f) -> f <> FromAny) l);
    
  method from kw fmt (base,deps) = match deps with
  | FromAny -> ()
  | From [] ->
    fprintf fmt "@[<hv 2>@[<h>%s@ %a@]@ @[<h>\\from \\nothing@];@]" 
      kw
      self#identified_term base
  | From l ->
    fprintf fmt "@[<hv 2>@[%s@ %a@]@ @[<h>\\from %a@];@]"
      kw 
      self#identified_term base
      (Pretty_utils.pp_list ~sep:",@ " self#identified_term) l
      
  (* not enclosed in a box *)
  method private terminates_decreases ~extra_nl nl fmt (terminates, variant) =
    let nl_terminates = nl || variant != None in
    let pp_opt nl fmt = 
      let suf = if nl then format_of_string "@]@\n" else "@]" in
      Pretty_utils.pp_opt ~suf fmt 
    in
    fprintf fmt "%a%a%(%)"
      (pp_opt nl_terminates self#terminates) terminates
      (pp_opt nl self#decreases) variant
      (format_of_string 
	 (if extra_nl && nl && (variant != None || terminates != None) 
	  then format_of_string "@\n" 
	  else ""))
      
  (* not enclosed in a box *)
  method private behavior_contents ~extra_nl nl ?terminates ?variant fmt b =
    self#set_current_behavior b;
    (* Template for correct line breaks:
       let nl_line_n = nb_line_(n+1) || is_empty clause_line_(n+1) *)
    let nl_assigns = nl || b.b_allocation != FreeAllocAny in
    let nl_extended = nl_assigns || b.b_assigns != WritesAny in
    let nl_ensures = nl_extended || b.b_extended != [] in
    let nl_decreases = nl_extended || b.b_post_cond != [] in
    let nl_requires = nl_decreases || variant != None || terminates != None in
    let nl_assumes = nl_requires || b.b_requires != [] in
    let pp_list nl fmt = 
      let suf = if nl then format_of_string "@]@\n" else "@]" in
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@\n" ~suf fmt 
    in
    fprintf fmt "%a%a%a%a%a%a%(%)%a%(%)%(%)"
      (pp_list nl_assumes self#assumes) b.b_assumes
      (pp_list nl_requires self#requires) b.b_requires
      (self#terminates_decreases ~extra_nl:false nl_decreases) 
        (terminates, variant)
      (pp_list nl_ensures self#post_cond) b.b_post_cond
      (pp_list nl_extended 
	 (Behavior_extensions.pp (self:>extensible_printer_type))) b.b_extended
      (self#assigns_deps "assigns") b.b_assigns
      (format_of_string 
	 (if nl_assigns && b.b_assigns != WritesAny
          then format_of_string "@\n" else ""))
      (self#allocation ~isloop:false) b.b_allocation
      (format_of_string 
	 (if nl && b.b_allocation != FreeAllocAny
          then format_of_string "@\n" else ""))
      (format_of_string
	 (if extra_nl && (nl_assumes || b.b_assumes != [])
          then format_of_string "@\n" else ""));
    self#reset_current_behavior ()
      
  method behavior fmt b =
    fprintf fmt "@[<v>behavior %s:@;<1 2>@[%a@]@]" 
      b.b_name
      (self#behavior_contents ~extra_nl:false false
	 ?terminates:None ?variant:None) 
      b
      
  method funspec fmt ({ spec_behavior = behaviors;
			spec_variant = variant;
			spec_terminates = terminates;
			spec_complete_behaviors = complete;
			spec_disjoint_behaviors = disjoint } as spec) =
    let pp_list ?(extra_nl=false) nl fmt = 
      let suf = 
        if nl then
          if extra_nl then format_of_string "@]@\n@\n" else "@]@\n"
        else "@]"
      in
      let sep = if extra_nl then format_of_string "@\n@\n" else "@\n" in
      Pretty_utils.pp_list ~pre:"@[<v>" ~sep ~suf fmt 
    in
    fprintf fmt "@[<v>";
    let default_bhv = Cil.find_default_behavior spec in
    let other_bhvs =
      List.filter (fun b -> not (Cil.is_default_behavior b)) behaviors
    in
    let nl_complete = disjoint != [] in
    let nl_other_bhvs = nl_complete || complete != [] in
    let nl_default = nl_other_bhvs || other_bhvs != [] in
    (match default_bhv with
    | None -> 
      self#terminates_decreases ~extra_nl:nl_default nl_default fmt
	(terminates, variant)
    | Some b
	when b.b_assumes == [] && b.b_requires == [] && b.b_post_cond == []
	  && b.b_extended == [] 
	  && b.b_allocation == FreeAllocAny && b.b_assigns == WritesAny ->
      self#terminates_decreases ~extra_nl:nl_default nl_default fmt
	(terminates, variant)
    | Some b -> 
      self#behavior_contents 
	~extra_nl:nl_default nl_default ?terminates ?variant fmt b);
    fprintf fmt "%a%a%a@]"
      (pp_list ~extra_nl:true nl_other_bhvs self#behavior) other_bhvs
      (pp_list nl_complete self#complete_behaviors) complete
      (pp_list false self#disjoint_behaviors) disjoint

  method private loop_pragma fmt = function
  | Widen_hints terms -> 
    fprintf fmt "WIDEN_HINTS @[%a@]"
      (Pretty_utils.pp_list ~sep:",@ " self#term) terms
  | Widen_variables terms -> 
    fprintf fmt "WIDEN_VARIABLES @[%a@]"
      (Pretty_utils.pp_list ~sep:",@ " self#term) terms
  | Unroll_specs terms -> 
    fprintf fmt "UNROLL @[%a@]"
      (Pretty_utils.pp_list ~sep:",@ " self#term) terms
      
  method private slice_pragma fmt = function
  |SPexpr t -> fprintf fmt "expr @[%a@]" self#term t
  | SPctrl -> pp_print_string fmt "ctrl"
  | SPstmt -> pp_print_string fmt "stmt"

  method private impact_pragma fmt = function
  | IPexpr t -> fprintf fmt "expr @[%a@]" self#term t
  | IPstmt -> pp_print_string fmt "stmt"
    
  (* TODO: add the annot ID in debug mode?*)
  method code_annotation fmt ca = 
    let pp_for_behavs fmt l =
      Pretty_utils.pp_list ~pre:"for @[" ~suf:"@]:@ " ~sep:",@ "
	pp_print_string
	fmt
	l
    in
    match ca.annot_content with
    | AAssert (behav,p) ->
      fprintf fmt "@[%aassert@ %a;@]"
	pp_for_behavs behav
	self#identified_pred p
    | APragma (Slice_pragma sp) ->
      fprintf fmt "@[slice pragma@ %a;@]" self#slice_pragma sp
    | APragma (Impact_pragma sp) ->
      fprintf fmt "@[impact pragma@ %a;@]" self#impact_pragma sp
    | APragma (Loop_pragma lp) ->
      fprintf fmt "@[loop pragma@ %a;@]" self#loop_pragma lp
    | AStmtSpec(for_bhv, spec) ->
      fprintf fmt "@[<hv 2>%a%a@]"
	pp_for_behavs for_bhv
	self#funspec spec
    | AAssigns(behav,a) ->
      fprintf fmt "@[<2>%a%a@]"
	pp_for_behavs behav
	(self#assigns_deps "loop assigns") a
    | AAllocation(behav,af) ->
      fprintf fmt "@[<2>%a%a@]"
	pp_for_behavs behav
	(self#allocation ~isloop:true) af
    | AInvariant(behav,true, i) ->
      fprintf fmt "@[<2>%aloop invariant@ %a;@]"
	pp_for_behavs behav
	self#identified_pred i
    | AInvariant(behav,false,i) -> 
      fprintf fmt "@[<2>%ainvariant@ %a;@]"
	pp_for_behavs behav
	self#identified_pred i
    | AVariant v -> 
      self#variant fmt v
	
  method private loopInv fmt p =
    fprintf fmt "@[<2>loop invariant@ %a;@]" self#identified_pred p

  method private logicPrms fmt arg =
    let pvar fmt = self#logic_var fmt arg in
    self#logic_type (Some pvar) fmt arg.lv_type
      
  method private typeKernel fmt tvars =
    Pretty_utils.pp_list ~pre:"<@[" ~suf:"@]>" ~sep:",@ "
      pp_print_string fmt tvars
      
  method logic_label fmt lab =
    let s =
      match lab with
      | LogicLabel (_, s) -> s
      | StmtLabel sref ->
	let rec pickLabel = function
	  | [] -> None
	  | Label (l, _, _) :: _ -> Some l
	  | _ :: rest -> pickLabel rest
	in
	match pickLabel !sref.labels with
	| Some l -> l
	| None -> "__invalid_label"
    in 
    pp_print_string fmt s

  method private labels fmt labels =
    match labels with 
    | [ l ] when current_label = l -> ()
    | _ ->
      Pretty_utils.pp_list ~pre:"{@[" ~suf:"@]}" ~sep:",@ "
	self#logic_label fmt labels
	
  method model_info fmt mfi =
    let print_decl fmt = self#model_field fmt mfi in
    fprintf fmt "@[model %a@ @[<2>{@ %a@ };@]"
      (self#typ None) mfi.mi_base_type
      (self#logic_type (Some print_decl)) mfi.mi_field_type

  method global_annotation fmt = function
  | Dtype_annot (a,_) ->
    fprintf fmt "@[type invariant @[%a%a=@ %a@,;@]@]@\n"
      self#logic_var a.l_var_info
      (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]@ " ~sep:",@ " 
	 self#logicPrms) a.l_profile
      self#identified_pred (pred_body a.l_body)
  | Dmodel_annot (mfi,_) ->
    self#model_info fmt mfi
  | Dcustom_annot(_c, n ,_) ->
    fprintf fmt "@[custom %s: <...>@]@\n" n
  | Dinvariant (pred,_) ->
    fprintf fmt "@[global@ invariant %a:@[@ %a;@]@]@\n"
      self#logic_var pred.l_var_info
      self#identified_pred (pred_body pred.l_body)
  | Dlemma(name, is_axiom, labels, tvars, pred,_) ->
    fprintf fmt "@[%s@ %a%a%a:@[@ %a;@]@]@\n"
      (if is_axiom then "axiom" else "lemma")
      self#varname name
      self#labels labels
      self#typeKernel tvars
      self#identified_pred pred
  | Dtype (ti,_) ->
    fprintf fmt "@[type@ %a%a%a;@]@\n"
      self#varname ti.lt_name self#typeKernel ti.lt_params
      (Pretty_utils.pp_opt
	 (fun fmt d -> fprintf fmt "@ =@ @[%a@]" self#logic_type_def d))
      ti.lt_def
  | Dfun_or_pred (li,_) ->
    (match li.l_type with
    | Some rt ->
      fprintf fmt "@[<hov 2>logic %a"
	(self#logic_type None) rt
    | None ->
      (match li.l_body with
      | LBinductive _ -> fprintf fmt "@[<hov 2>inductive"
      | _ -> fprintf fmt "@[<hov 2>predicate"));
    fprintf fmt " %a%a%a%a"
      self#logic_var li.l_var_info
      self#labels li.l_labels
      self#typeKernel li.l_tparams
      (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]@ " ~sep:",@ "
	 self#logicPrms) 
      li.l_profile;
    (match li.l_body with
    | LBnone ->
      fprintf fmt ";"
    | LBreads reads ->
      (match reads with
      | [] -> fprintf fmt "@\n@[reads \\nothing;@]"
      | _ -> 
	fprintf fmt "%a;"
	  (Pretty_utils.pp_list
	     ~pre:"@\n@[reads@ "
	     ~sep:",@ "
	     (fun fmt x -> self#term fmt x.it_content)) reads)
    | LBpred def ->
      fprintf fmt "=@ %a;"
	self#identified_pred def
    | LBinductive indcases ->
      fprintf fmt "{@ %a}"
	(Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]@\n" ~sep:"@\n"
	   (fun fmt (id,labels,tvars,p) ->
	     Format.fprintf fmt "case %s%a%a: @[%a@];" id
	       self#labels labels
	       self#typeKernel tvars
	       self#identified_pred p)) indcases
    | LBterm def ->
      fprintf fmt "=@ %a;"
	self#term def);
    fprintf fmt "@]@\n"
  | Dvolatile(tsets,rvi_opt,wvi_opt,_) ->
    let pp_vol txt fmt = function
      | None -> () ;
      | Some vi -> fprintf fmt "@ %s %a" txt self#varinfo vi
    in
    fprintf fmt "@[<hov 2>volatile@ %a%a%a;@]"
      (Pretty_utils.pp_list ~sep:",@ "
	 (fun fmt x -> self#term fmt x.it_content)) 
      tsets
      (pp_vol "reads") rvi_opt
      (pp_vol "writes") wvi_opt ;
  | Daxiomatic(id,decls,_) ->
    fprintf fmt "@[<v 2>axiomatic@ %s {@\n%a}@]@\n" id
      (Pretty_utils.pp_list ~pre:"@[<v 0>" ~suf:"@]@\n" ~sep:"@\n"
	 self#global_annotation)
      decls
      
  method logic_type_def fmt = function
  | LTsum l ->
    Pretty_utils.pp_list ~sep:"@ |@ "
      (fun fmt info ->
	fprintf fmt "%s@[%a@]" info.ctor_name
	  (Pretty_utils.pp_list ~pre:"@[(" ~suf:")@]" ~sep:",@ "
	     (self#logic_type None)) info.ctor_params) fmt l
  | LTsyn typ -> 
    self#logic_type None fmt typ

  method file fmt file =
    fprintf fmt "@[/* Generated by Frama-C */@\n" ;
    Cil.iterGlobals file (fun g -> self#global fmt g);
    fprintf fmt "@]@."

end (* class cil_printer *)

include Printer_builder.Make(struct class printer = cil_printer end)

(* initializing Cil's forward references *)
let () = Cil.pp_typ_ref := pp_typ
let () = Cil.pp_global_ref := pp_global
let () = Cil.pp_exp_ref := pp_exp
let () = Cil.pp_lval_ref := pp_lval
let () = Cil.pp_ikind_ref := pp_ikind
let () = Cil.pp_attribute_ref := pp_attribute
let () = Cil.pp_attributes_ref := pp_attributes

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
