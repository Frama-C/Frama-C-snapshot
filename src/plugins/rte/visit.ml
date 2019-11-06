(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Runtime Error annotation generation plugin *)

open Cil_types
open Cil_datatype

(* AST inplace visitor for runtime annotation generation *)

(** [kf]: function to annotate
    [flags]: which RTE to generate.
    [register]: the action to perform on each RTE alarm *)
class annot_visitor kf flags on_alarm = object (self)

  inherit Visitor.frama_c_inplace

  val mutable skip_set = Exp.Set.empty
  val mutable skip_initialized_set = Lval.Set.empty

  method private mark_to_skip exp = skip_set <- Exp.Set.add exp skip_set
  method private must_skip exp = Exp.Set.mem exp skip_set

  method private mark_to_skip_initialized lv =
    skip_initialized_set <- Lval.Set.add lv skip_initialized_set

  method private must_skip_initialized lv =
    (* Will return true only once per mark_to_skip_initialized
       for the same lval *)
    let r = Lval.Set.mem lv skip_initialized_set in
    if r then skip_initialized_set <- Lval.Set.remove lv skip_initialized_set;
    r

  method private do_initialized () =
    flags.Flags.initialized && not (Generator.Initialized.is_computed kf)

  method private do_mem_access () =
    flags.Flags.mem_access && not (Generator.Mem_access.is_computed kf)

  method private do_div_mod () =
    flags.Flags.div_mod && not (Generator.Div_mod.is_computed kf)

  method private do_shift () =
    flags.Flags.shift && not (Generator.Shift.is_computed kf)

  method private do_left_shift_negative () =
    flags.Flags.left_shift_negative
    && not (Generator.Left_shift_negative.is_computed kf)

  method private do_right_shift_negative () =
    flags.Flags.right_shift_negative
    && not (Generator.Right_shift_negative.is_computed kf)

  method private do_signed_overflow () =
    flags.Flags.signed_overflow
    && not (Generator.Signed_overflow.is_computed kf)

  method private do_unsigned_overflow () =
    flags.Flags.unsigned_overflow
    && not (Generator.Unsigned_overflow.is_computed kf)

  method private do_signed_downcast () =
    flags.Flags.signed_downcast
    && not (Generator.Signed_downcast.is_computed kf)

  method private do_unsigned_downcast () =
    flags.Flags.unsigned_downcast
    && not (Generator.Unsigned_downcast.is_computed kf)

  method private do_float_to_int () =
    flags.Flags.float_to_int && not (Generator.Float_to_int.is_computed kf)

  method private do_finite_float () =
    flags.Flags.finite_float && not (Generator.Finite_float.is_computed kf)

  method private do_pointer_call () =
    flags.Flags.pointer_call && not (Generator.Pointer_call.is_computed kf)

  method private do_bool_value () =
    flags.Flags.bool_value && not (Generator.Bool_value.is_computed kf)

  method private queue_stmt_spec spec =
    let stmt = Extlib.the (self#current_stmt) in
    Queue.add
      (fun () ->
         let annot = Logic_const.new_code_annotation (AStmtSpec ([], spec)) in
         Annotations.add_code_annot Generator.emitter ~kf stmt annot)
      self#get_filling_actions

  method private generate_assertion: 'a. 'a Rte.alarm_gen -> 'a -> unit =
    fun fgen ->
    let curr_stmt = self#current_stmt in
    let on_alarm ~invalid a =
      match curr_stmt with
      | None -> Options.warning ~current:true
                  "Alarm generated outside any statement:@ %a"
                  Alarms.pretty a
      | Some stmt -> on_alarm stmt ~invalid a
    in
    fgen ~remove_trivial:flags.Flags.remove_trivial ~on_alarm

  (* Do not visit variable declarations, as no alarm should be emitted here,
     and there is no statement to emit an alarm anyway ([generate_assertion]
     or [Alarms.register] would then crash). *)
  method !vvdec _ = Cil.SkipChildren

  method! vstmt s = match s.skind with
    | UnspecifiedSequence l ->
      (* UnspecifiedSequences may contain lvals for side-effects, that
         give rise to spurious assertions *)
      let no_lval = List.map (fun (s, _, _, _, sref) -> s, [], [], [], sref) l in
      let s' = { s with skind = UnspecifiedSequence no_lval } in
      Cil.ChangeDoChildrenPost (s', fun _ -> s)
    | _ -> Cil.DoChildren

  method private treat_call ret_opt =
    match ret_opt, self#do_mem_access () with
    | None, _ | Some _, false -> ()
    | Some ret, true ->
      Options.debug "lval %a: validity of potential mem access checked\n"
        Printer.pp_lval ret;
      self#generate_assertion
        (Rte.lval_assertion ~read_only:Alarms.For_writing) ret


  method private check_uchar_assign dest src =
    if self#do_mem_access () then begin
      Options.debug "lval %a: validity of potential mem access checked\n"
        Printer.pp_lval dest;
      self#generate_assertion
        (Rte.lval_assertion ~read_only:Alarms.For_writing)
        dest
    end;
    begin match src.enode with
      | Lval src_lv ->
        let typ1 = Cil.typeOfLval src_lv in
        let typ2 = Cil.typeOfLval dest in
        let isUChar t = Cil.isUnsignedInteger t && Cil.isAnyCharType t in
        if isUChar typ1 && isUChar typ2 then
          self#mark_to_skip_initialized src_lv
      | _ -> ()
    end ;
    Cil.DoChildren

  (* assigned left values are checked for valid access *)
  method! vinst = function
    | Set (lval,exp,_) -> self#check_uchar_assign lval exp
    | Call (ret_opt,funcexp,argl,_) ->
      (* Do not emit alarms on Eva builtins such as Frama_C_show_each, that should
         have no effect on analyses. *)
      let is_builtin, is_va_start =
        match funcexp.enode with
        | Lval (Var vinfo, NoOffset) ->
          let kf = Globals.Functions.get vinfo in
          let frama_b = Ast_info.is_frama_c_builtin (Kernel_function.get_name kf)
          in
          let va_start = Kernel_function.get_name kf = "__builtin_va_start" in
          (frama_b, va_start)
        | _ -> (false, false)
      in
      if is_va_start then begin
        match (List.nth argl 0).enode with
        | Lval lv -> self#mark_to_skip_initialized lv
        | _ -> ()
      end ;
      if is_builtin
      then Cil.SkipChildren
      else begin
        self#treat_call ret_opt;
        (* Alarm if the call is through a pointer. Done in DoChildrenPost to get a
           more pleasant ordering of annotations. *)
        let do_ptr () =
          if self#do_pointer_call () then
            match funcexp.enode with
            | Lval (Mem e, _) -> self#generate_assertion Rte.pointer_call (e, argl)
            | _ -> ()
        in
        Cil.DoChildrenPost (fun res -> do_ptr (); res)
      end
    | Local_init (v,ConsInit(f,args,kind),loc) ->
      let do_call lv _e _args _loc = self#treat_call lv in
      Cil.treat_constructor_as_func do_call v f args kind loc;
      Cil.DoChildren
    | Local_init (v,AssignInit (SingleInit exp),_) ->
      self#check_uchar_assign (Cil.var v) exp
    | Local_init (_,AssignInit _,_)
    | Asm _ | Skip _ | Code_annot _ -> Cil.DoChildren

  method! vexpr exp =
    Options.debug "considering exp %a\n" Printer.pp_exp exp;
    match exp.enode with
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _ -> Cil.SkipChildren
    | _ ->
      let generate () =
        match exp.enode with
        | BinOp((Div | Mod) as op, lexp, rexp, ty) ->
          (match Cil.unrollType ty with
           | TInt(kind,_) ->
             (* add assertion "divisor not zero" *)
             if self#do_div_mod () then
               self#generate_assertion Rte.divmod_assertion rexp;
             if self#do_signed_overflow () && op = Div && Cil.isSigned kind then
               (* treat the special case of signed division overflow
                  (no signed modulo overflow) *)
               self#generate_assertion Rte.signed_div_assertion (exp, lexp, rexp)
           | TFloat(fkind,_) when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp);
           | _ -> ())

        | BinOp((Shiftlt | Shiftrt) as op, lexp, rexp,ttype ) ->
          (match Cil.unrollType ttype with
           | TInt(kind,_) ->
             (* 0 <= rexp <= width *)
             if self#do_shift () then begin
               let typ = Cil.unrollType (Cil.typeOf exp) in
               (* Not really a problem of overflow, but almost a similar to self#do_div_mod *)
               self#generate_assertion Rte.shift_width_assertion (rexp, typ);
             end;
             let signed = Cil.isSigned kind in
             (* 0 <= lexp *)
             if signed &&
                (op = Shiftlt && self#do_left_shift_negative ()
                 || op = Shiftrt && self#do_right_shift_negative ())
             then self#generate_assertion Rte.shift_negative_assertion lexp;
             (* Signed or unsigned overflow. *)
             if self#do_signed_overflow () && signed
             || self#do_unsigned_overflow () && not signed
             then
               self#generate_assertion
                 (Rte.shift_overflow_assertion ~signed) (exp, op, lexp, rexp)
           | _ -> ())

        | BinOp((PlusA |MinusA | Mult) as op, lexp, rexp, ttype) ->
          (* may be skipped if the enclosing expression is a downcast to a signed
             type *)
          (match Cil.unrollType ttype with
           | TInt(kind,_) when Cil.isSigned kind ->
             if self#do_signed_overflow () && not (self#must_skip exp) then
               self#generate_assertion
                 (Rte.mult_sub_add_assertion ~signed:true)
                 (exp, op, lexp, rexp)
           | TInt(kind,_) when not (Cil.isSigned kind) ->
             if self#do_unsigned_overflow () then
               self#generate_assertion
                 (Rte.mult_sub_add_assertion ~signed:false)
                 (exp, op, lexp, rexp)
           | TFloat(fkind,_) when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp)
           | _ -> ())

        | UnOp(Neg, exp, ty) ->
          (* Note: if unary minus on unsigned integer is to be understood as
             "subtracting the promoted value from the largest value
             of the promoted type and adding one",
             the result is always representable: so no overflow *)
          (match Cil.unrollType ty with
           | TInt(kind,_) when Cil.isSigned kind ->
             if self#do_signed_overflow () then
               self#generate_assertion Rte.uminus_assertion exp;
           | TFloat(fkind,_) when self#do_finite_float () ->
             self#generate_assertion Rte.finite_float_assertion (fkind,exp)
           | _ -> ())

        | Lval lval ->
          (match Cil.(unrollType (typeOfLval lval)) with
           | TInt (IBool,_) when self#do_bool_value () ->
             self#generate_assertion Rte.bool_value lval
           | _ -> ());
          (* left values are checked for valid access *)
          if self#do_mem_access () then begin
            Options.debug
              "exp %a is an lval: validity of potential mem access checked"
              Printer.pp_exp exp;
            self#generate_assertion
              (Rte.lval_assertion ~read_only:Alarms.For_reading) lval
          end;
          if self#do_initialized () && not (self#must_skip_initialized lval) then begin
            Options.debug
              "exp %a is an lval: initialization of potential mem access checked"
              Printer.pp_exp exp;
            self#generate_assertion
              Rte.lval_initialized_assertion lval
          end ;
        | CastE (ty, e) ->
          (match Cil.unrollType ty, Cil.unrollType (Cil.typeOf e) with
           (* to , from *)
           | TInt(kind,_), TInt (_, _) ->
             if Cil.isSigned kind then begin
               if self#do_signed_downcast () then begin
                 self#generate_assertion Rte.signed_downcast_assertion (ty, e);
                 self#mark_to_skip e;
               end
             end
             else if self#do_unsigned_downcast () then
               self#generate_assertion Rte.unsigned_downcast_assertion (ty, e)

           | TInt _, TFloat _ ->
             if self#do_float_to_int () then
               self#generate_assertion Rte.float_to_int_assertion (ty, e)

           | TFloat (to_fkind,_), TFloat (from_fkind,_) when
               self#do_finite_float () && Cil.frank to_fkind < Cil.frank from_fkind ->
             self#generate_assertion Rte.finite_float_assertion (to_fkind,exp)
           | _ -> ());
        | Const (CReal(f,fkind,_)) when self#do_finite_float () ->
          begin match classify_float f with
            | FP_normal
            | FP_subnormal
            | FP_zero -> ()
            | FP_infinite
            | FP_nan ->
              self#generate_assertion Rte.finite_float_assertion (fkind,exp)
          end
        | StartOf _
        | AddrOf _
        | Info _
        | UnOp _
        | Const _
        | BinOp _ -> ()
        | SizeOf _
        | SizeOfE _
        | SizeOfStr _
        | AlignOf _
        | AlignOfE _ -> assert false
      in
      (* Use Cil.DoChildrenPost so that inner expression and lvals are
         checked first. The order of resulting assertions will be better. *)
      Cil.DoChildrenPost (fun new_e -> generate (); new_e)

end

(** {2 Iterate over Alarms on Cil elements} *)

type on_alarm = kernel_function -> stmt -> invalid:bool -> Alarms.alarm -> unit

let filter = function None -> Flags.default () | Some flags -> flags

let iter_alarms visit ?flags (on_alarm:on_alarm) kf stmt element =
  let visitor = object (self)
    inherit annot_visitor kf (filter flags) (on_alarm kf)
    initializer self#push_stmt stmt
  end in
  ignore (visit (visitor :> Cil.cilVisitor) element)

type 'a iterator =
  ?flags:Flags.t -> on_alarm ->
  Kernel_function.t -> Cil_types.stmt -> 'a -> unit

let iter_lval : lval iterator = iter_alarms Cil.visitCilLval
let iter_exp : exp iterator = iter_alarms Cil.visitCilExpr
let iter_instr : instr iterator = iter_alarms Cil.visitCilInstr
let iter_stmt : stmt iterator = iter_alarms Cil.visitCilStmt

(** {2 Regitration} *)

let status ~invalid =
  if invalid then Some Property_status.False_if_reachable else None

let register emitter kf stmt ~invalid alarm =
  let status = status ~invalid in
  Alarms.register emitter ~kf (Kstmt stmt) ?status alarm

(* -------------------------------------------------------------------------- *)
(* --- List Code Annotations                                              --- *)
(* -------------------------------------------------------------------------- *)

let collector () =
  let pool = ref [] in
  let on_alarm stmt ~invalid:_ alarm =
    let ca, _ = Alarms.to_annot (Kstmt stmt) alarm in
    pool := ca :: !pool ;
  in pool , on_alarm

let get_annotations_kf ?flags kf =
  match kf.fundec with
  | Declaration _ -> []
  | Definition(f, _) ->
    let pool,on_alarm = collector () in
    let visitor = new annot_visitor kf (filter flags) on_alarm in
    ignore (Visitor.visitFramacFunction visitor f) ; !pool

let collect from flags kf stmt elt =
  let pool,on_alarm = collector () in
  let visitor = object (self)
    inherit annot_visitor kf (filter flags) on_alarm
    initializer self#push_stmt stmt
  end in
  ignore (from (visitor :> Cil.cilVisitor) elt); !pool

let get_annotations_stmt ?flags kf stmt =
  collect Cil.visitCilStmt flags kf stmt stmt

let get_annotations_exp ?flags kf stmt exp =
  collect Cil.visitCilExpr flags kf stmt exp

let get_annotations_lval ?flags kf stmt lv =
  collect Cil.visitCilLval flags kf stmt lv

(** {2 Annotations of kernel_functions for a given type of RTE} *)

(* generates annotation for function kf on the basis of [flags] *)
let annotate ?flags kf =
  let flags = filter flags in
  Options.debug "annotating function %a" Kernel_function.pretty kf;
  match kf.fundec with
  | Declaration _ -> ()
  | Definition(f, _) ->
    (* This reference contains all the RTE statuses that should be positioned
       once this function has been annotated. *)
    let to_update = ref [] in
    (* Check whether there is something to compute + lists all the statuses
       that will be ultimately updated *)
    let comp (_name, set, is_computed) should_compute =
      if should_compute && not (is_computed kf) then begin
        to_update := (fun () -> set kf true) :: !to_update;
        true
      end
      else false
    in
    (* Strict version of ||, because [comp] has side-effects *)
    let (|||) a b = a || b in
    let open Generator in
    let open Flags in
    if comp Initialized.accessor flags.initialized |||
       comp Mem_access.accessor flags.mem_access |||
       comp Pointer_call.accessor flags.pointer_call |||
       comp Div_mod.accessor flags.div_mod |||
       comp Shift.accessor flags.shift |||
       comp Left_shift_negative.accessor flags.left_shift_negative |||
       comp Right_shift_negative.accessor flags.right_shift_negative |||
       comp Signed_overflow.accessor flags.signed_overflow |||
       comp Signed_downcast.accessor flags.signed_downcast |||
       comp Unsigned_overflow.accessor flags.unsigned_overflow |||
       comp Unsigned_downcast.accessor flags.unsigned_downcast |||
       comp Float_to_int.accessor flags.float_to_int |||
       comp Finite_float.accessor flags.finite_float |||
       comp Bool_value.accessor flags.bool_value
    then begin
      Options.feedback "annotating function %a" Kernel_function.pretty kf;
      let warn = Options.Warn.get () in
      let on_alarm stmt ~invalid alarm =
        let ca, _ = register Generator.emitter kf stmt ~invalid alarm in
        if warn && invalid then
          Options.warn "@[guaranteed RTE:@ %a@]"
            Printer.pp_code_annotation ca
      in
      let vis = new annot_visitor kf flags on_alarm in
      let nkf = Visitor.visitFramacFunction vis f in
      assert(nkf == f);
      List.iter (fun f -> f ()) !to_update;
    end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
