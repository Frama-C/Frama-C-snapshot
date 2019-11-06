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

open Cil_types

let category = File.register_code_transformation_category "asm contracts"

let emitter =
  Emitter.(
    create "asm_contracts"
      [ Code_annot; Property_status ]
      ~correctness:[]
      ~tuning:[Kernel.AsmContractsAutoValidate.parameter])

let find_out_lval l =
  let treat_one_lval (output, input) (_,constr, lv) =
    let tlv = Logic_utils.lval_to_term_lval ~cast:false lv in
    match constr with
      | "" -> tlv :: output, input
      | _ ->
        (* '+' indicates that the lval is used both as input and as output. 
           GNU syntax allows it only at the beginning of the constraint, but
           actual implementation is more liberal and emits only a warning.
        *)
        if String.contains constr '+' then begin
          if constr.[0] <> '+' then
            Kernel.warning
              "output constraint '+' is not at the beginning in output operand";
          tlv::output,
          (* avoid sharing ids *)
          Visitor.visitFramacTermLval
            (new Visitor.frama_c_refresh(Project.current())) tlv
          :: input
        end else tlv::output, input
  in
  let output, input =
    List.fold_left treat_one_lval ([],[]) l
  in
  List.rev output, List.rev input

let extract_term_lval acc (_,_,e) =
  let res = ref acc in
  let vis =
    object
      inherit Visitor.frama_c_inplace
      method! vlval lv =
        res := Logic_utils.lval_to_term_lval ~cast:false lv :: !res;
        Cil.SkipChildren
    end
  in
  ignore (Visitor.visitFramacExpr vis e);
  !res

let find_input_lval l =
  (* constraints on input are not interesting for us. They only concern
     the placement of the given expression (register, memory, ...) *)
  List.rev (List.fold_left extract_term_lval [] l)

let access_ptr_elts ~loc tlv =
  let range = Logic_const.trange ~loc (None, None) in
  let basetype = Cil.typeOfTermLval tlv in
  let base = Logic_const.term ~loc (TLval tlv) basetype in
  let base, basetype =
    if Logic_utils.isLogicVoidPointerType basetype then begin
      let typ = Ctype Cil.charPtrType in
      Logic_const.term ~loc (TCastE(Cil.charPtrType,base)) typ, typ
    end else base, basetype
  in
  let offset = Logic_const.term ~loc (TBinOp (PlusPI, base, range)) basetype in
  TMem offset, TNoOffset

let access_elts ~loc ?size tlv =
  let range =
    match size with
    | None -> Logic_const.trange ~loc (None, None)
    | Some l ->
      Logic_const.trange
        ~loc
        (Some (Logic_const.tinteger ~loc 0),
         Some (Logic_const.tint ~loc (Integer.pred l)))
  in
  Logic_const.addTermOffsetLval (TIndex(range,TNoOffset)) tlv

let extract_mem_term ~loc acc tlv =
  match Logic_utils.unroll_type (Cil.typeOfTermLval tlv) with
  | Ctype (TPtr _ ) -> access_ptr_elts ~loc tlv :: acc
  | Ctype (TArray(_,e,_,_)) ->
    let size = Extlib.opt_bind (Cil.constFoldToInt ~machdep:true) e in
    access_elts ~loc ?size tlv :: acc
  | _ -> acc

let extract_mem_terms ~loc l =
  List.rev (List.fold_left (extract_mem_term ~loc) [] l)

class visit_assembly =
object(self)
  inherit Visitor.frama_c_inplace

  method! vinst i =
    let stmt = Extlib.the self#current_stmt in
    let kf = Extlib.the self#current_kf in
    match i with
      | Asm(_, _, Some { asm_outputs; asm_inputs; asm_clobbers }, loc) ->
          let lv_out, lv_from = find_out_lval asm_outputs in
          let lv_from = lv_from @ find_input_lval asm_inputs in
          let mem_output = extract_mem_terms ~loc lv_from in
          let lv_out = lv_out @ mem_output in
          let lv_from = lv_from @ mem_output in
          let lv_from =
            List.filter
              (fun lv ->
                 not (Logic_utils.isLogicArrayType (Cil.typeOfTermLval lv)))
              lv_from
          in
          (* the only interesting information for clobbers is the
             presence of the "memory" keyword, which indicates that
             memory may have been accessed (read or write) outside of
             the locations explicitly referred to as output or
             input. We can't do much more than emitting a warning and
             considering that nothing is touched beyond normally
             specified outputs and inputs. *)
          let mem_clobbered = List.mem "memory" asm_clobbers in
          if  mem_clobbered then begin
            let source = fst (Cil_datatype.Instr.loc i) in
            let once = true in
            Kernel.warning
              ~once ~source
              "Clobber list contains \"memory\" argument. Assuming no \
               side effects beyond those mentioned in operands."
          end;
          let to_id_term lv =
            Logic_const.new_identified_term
              (Logic_const.term ~loc (TLval lv) (Cil.typeOfTermLval lv))
          in
          let to_id_from lv =
            let typ = Cil.typeOfTermLval lv in
            let base_term = Logic_const.term ~loc (TLval lv) typ in
            let term =
              if Logic_utils.isLogicPointerType typ ||
                 Logic_utils.isLogicArrayType typ
              then { base_term with term_name = ["indirect"] }
              else base_term
            in
            Logic_const.new_identified_term term
          in
          let assigns () =
            Writes
              (List.map
                 (fun x -> (to_id_term x, From (List.map to_id_from lv_from)))
                 lv_out)
          in
          let filter ca =
            match ca.annot_content with
                (* search for a statement contract that applies to all cases. *)
              | AStmtSpec ([],_) -> true
              | _ -> false
          in
          let contracts = Annotations.code_annot ~filter stmt in
          (match contracts with
           | [] ->
                let assigns = assigns () in
                let bhv = Cil.mk_behavior ~assigns () in
                let spec = Cil.empty_funspec () in
                spec.spec_behavior <- [ bhv ];
                let ca =
                  Logic_const.new_code_annotation (AStmtSpec ([],spec))
                in
                Annotations.add_code_annot emitter ~kf stmt ca;
                if not mem_clobbered && Kernel.AsmContractsAutoValidate.get()
                then begin
                  let active = [] in
                  let ip_assigns =
                    Property.ip_assigns_of_behavior kf (Kstmt stmt) ~active bhv in
                  let ip_from =
                    Property.ip_from_of_behavior kf (Kstmt stmt) ~active bhv in
                  List.iter
                    Property_status.(
                      fun x -> emit emitter ~hyps:[] x True)
                    (Extlib.list_of_opt ip_assigns @ ip_from)
                end
           | [ { annot_content = AStmtSpec ([], spec) } ] ->
                (* Already existing contracts. Just add assigns clause for
                   behaviors that do not already have one. *)
             List.iter
               (fun bhv ->
                  match bhv.b_assigns with
                  | WritesAny ->
                    let behavior = bhv.b_name in
                    let assigns = assigns () in
                    let keep_empty = false in
                    Annotations.add_assigns
                      ~keep_empty emitter kf ~stmt ~behavior assigns;
                  | Writes _ -> ())
               spec.spec_behavior
           | _ ->
             Kernel.fatal "Several contracts found for the same statement %a"
               Printer.pp_stmt stmt
          );
          Cil.SkipChildren
      | Asm(_,_,None,_) ->
        Kernel.feedback
          ~dkey:Kernel.dkey_asm_contracts "Ignoring basic assembly instruction";
        Cil.SkipChildren
      | _ -> Cil.SkipChildren
end

let transform file =
  if Kernel.AsmContractsGenerate.get() then
    Visitor.visitFramacFileSameGlobals (new visit_assembly) file

let () =
  File.add_code_transformation_after_cleanup 
    ~deps:[(module Kernel.AsmContractsGenerate);
           (module Kernel.AsmContractsAutoValidate) ]
    category
    transform
