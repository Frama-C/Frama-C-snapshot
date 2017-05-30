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

(* Print a warning message when an undefined behavior may occurs in an
   unspecified sequence, i.e. two writes or a write and a read. See full
   doc in .mli. *)
let check_sequences file =
  (* checks whether offsets starting from the same base might overlap *)
  let rec may_overlap_offset offs1 offs2 =
    match offs1, offs2 with
    | NoOffset,_ | _, NoOffset -> true
    | Field (f1,offs1), Field(f2,offs2) ->
      (* it's probably a bit overkill to check if any of the field is in
         an union, as the types of offs1 and offs2 are very probably 
         identical, but I don't have a Coq proof of that fact at the moment. *)
      (not f1.fcomp.cstruct || not f2.fcomp.cstruct) ||
      (f1.fname = f2.fname &&
       f1.fcomp.ckey = f2.fcomp.ckey &&
       may_overlap_offset offs1 offs2)
    | Index(i1,offs1), Index(i2,offs2) ->
      (match Cil.constFoldToInt i1, Cil.constFoldToInt i2 with
       | Some c1, Some c2 ->
         Integer.equal c1 c2 &&
         may_overlap_offset offs1 offs2
       | None, _ | _, None ->
         may_overlap_offset offs1 offs2
      )
    | (Index _|Field _), (Index _|Field _) ->
      (* A bit strange, but we're not immune against some ugly cast.
         Let's play safe here. *)
      true
  in
  (* checks whether two lval may overlap *)
  let may_overlap_lval (base1,offs1)(base2,offs2) =
    match (base1,offs1), (base2,offs2) with
    | (Mem _,_),(Mem _,_) -> true
    | (Var v,_),(Mem _,_) | (Mem _,_), (Var v,_)->
      v.vaddrof (* if the address of v is not taken,
                   it cannot be aliased*)
    | (Var v1,offs1),(Var v2,offs2) ->
      v1.vid = v2.vid && may_overlap_offset offs1 offs2
  in
  (* checks whether some element of the first list may overlap with some
     element of the second one. *)
  let may_overlap l1 l2 =
    Extlib.product_fold (fun f e1 e2 -> f || may_overlap_lval e1 e2)
      false l1 l2
  in
  let check_unspec = object
    inherit Cil.nopCilVisitor
    method! vstmt s =
      (match s.skind with
       | UnspecifiedSequence [] | UnspecifiedSequence [ _ ] -> ()
       | UnspecifiedSequence seq ->
         (* We have more than one side-effect in an unspecified sequence.
            For each statement, we check whether its side effects may overlap
            with the others, or with the reads. *)
         let my_stmt_print = object(self)
           inherit Cil_printer.extensible_printer () as super
           method! stmt fmt = function
             | {skind = UnspecifiedSequence seq } ->
               Pretty_utils.pp_list ~sep:"@\n"
                 (fun fmt (s,m,w,r,_) ->
                    Format.fprintf fmt
                      "/*@ %t%a@ <-@ %a@ */@\n%a"
                      (fun fmt -> if (Kernel.debug_atleast 2) then
                          Pretty_utils.pp_list
                            ~pre:"@[("
                            ~suf:")@]"
                            ~sep:"@ "
                            self#lval fmt m)
                      (Pretty_utils.pp_list ~sep:"@ " self#lval) w
                      (Pretty_utils.pp_list ~sep:"@ " self#lval) r
                      self#stmt s)
                 fmt
                 seq
             | s -> super#stmt fmt s
         end in
         (* when checking for overlaps, we do not consider temporaries
            introduced by the normalization. In other words,
            we assume that the normalization itself is correct. *)
         let remove_mod m l =
           List.filter
             (fun x -> not (List.exists (Cil_datatype.Lval.equal x) m)) l
         in
         (* l1 contains two lists: the first one is the temporaries we
            do not want to consider, the second one are locations that
            are read by a given statement. l2 contains locations that are
            written by another statement. *)
         let may_overlap_modified l1 l2 =
           List.fold_left
             (fun flag (m,r) -> flag || may_overlap (remove_mod m l2) r)
             false l1
         in
         let warn,_,_ =
           List.fold_left
             (fun ((warn,writes,reads) as res) (_,m,w,r,_) ->
                (* the accumulator contains the lists of written 
                   and read locations from the previous statements.
                   We check for overlapping between the following pairs:
                   - w vs writes
                   - r vs writes (modulo temporaries m as explained above).
                   - reads vs w (id. )
                   As soon as we have identified a potential overlap, we
                   output the whole unspecified sequence.
                *)
                if warn then res else begin
                  let new_writes = w @ writes in
                  let new_reads = (m,r)::reads in
                  let new_warn =
                    warn || may_overlap writes w ||
                    may_overlap (remove_mod m writes) r ||
                    may_overlap_modified reads w
                  in
                  new_warn,new_writes,new_reads
                end)
             (false, [], []) seq
         in
         if warn then
           Kernel.warning ~current:true ~once:true
             "Unspecified sequence with side effect:@\n%a@\n"
             (my_stmt_print#without_annot my_stmt_print#stmt) s
       | _ -> ());
      Cil.DoChildren
  end
  in
  Cil.visitCilFileSameGlobals check_unspec file
