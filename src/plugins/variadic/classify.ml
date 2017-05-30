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
open Format_types
open Va_types
open Options
module Typ = Extends.Typ


(* ************************************************************************ *)
(* Variadic classes builders                                                *)
(* ************************************************************************ *)

let find_function env s =
  try
    Some (Environment.find_function env s)
  with Not_found ->
    Self.warning
      "Unable to locate function %s which should be in the Frama-C LibC."
      s;
    None

let mk_overload env names =
  let vis = Extends.List.filter_map (find_function env) names in
  let overload = List.map (fun vi -> Typ.params_types vi.vtype, vi) vis in
  Overload overload

let mk_aggregator env fun_name a_pos pname a_type =
  match find_function env fun_name with
  | None -> Misc
  | Some vi ->
      try
        (* Get the list of arguments *)
        let params = Typ.params vi.vtype in

        (* Check that pos is a valid position in the list *)
        assert (a_pos >= 0);
        if a_pos >= List.length params then begin
          Self.warning ~current:true
            "The standard function %s should have at least %d parameters."
            fun_name
            (a_pos + 1);
            raise Exit
        end;

        (* Get the aggregate type of elements *)
        let _,ptyp,_ = List.nth params a_pos in
        let a_param = pname, match ptyp with
        | TArray (typ,_,_,_)
        | TPtr (typ, _) -> typ
        | _ ->
            Self.warning ~current:true
              "The parameter %d of standard function %s should be \
               of array type."
              (a_pos + 1)
              fun_name;
            raise Exit
        in

        Aggregator {a_target = vi; a_pos; a_type; a_param}

        (* In case of failure return Misc (apply generic translation) *)
      with Exit -> Misc

let mk_format_fun ~buffer ?(additional=[]) ~format f_kind =
  FormatFun { f_kind; f_buffer = buffer ; f_format_pos = format ;
  f_additional_args = additional; }


(* ************************************************************************ *)
(* Classification                                                           *)
(* ************************************************************************ *)

let classify_std env vi = match vi.vname with
  (* fcntl.h - Overloads of functions *)
  | "fcntl" -> mk_overload env
      ["__va_fcntl_void" ; "__va_fcntl_int" ; "__va_fcntl_flock"]
  | "open" -> mk_overload env
      ["__va_open_void" ; "__va_open_mode_t"]
  | "openat" -> mk_overload env
      ["__va_openat_void" ; "__va_openat_mode_t"]

  (* unistd.h *)
  | "execl"   -> mk_aggregator env "execv" 1 "argv" EndedByNull
  | "execle"  -> mk_aggregator env "execve" 1 "argv" EndedByNull
  | "execlp"  -> mk_aggregator env "execvp" 1 "argv" EndedByNull
  | "syscall" -> Misc

  (* stdio.h *)
  | "fprintf"  -> mk_format_fun ~buffer:(Stream 0) ~format:1 PrintfLike
  | "printf"   -> mk_format_fun ~buffer:(StdIO) ~format:0 PrintfLike
  | "sprintf"  -> mk_format_fun ~buffer:(Arg 0) ~format:1 PrintfLike
  | "snprintf" -> mk_format_fun ~buffer:(Arg 0) ~additional:[1] ~format:2 PrintfLike
  | "dprintf"  -> mk_format_fun ~buffer:(File 0) ~format:1 PrintfLike
  | "fscanf"   -> mk_format_fun ~buffer:(Stream 0) ~format:1 ScanfLike
  | "scanf"    -> mk_format_fun ~buffer:(StdIO) ~format:0 ScanfLike
  | "sscanf"   -> mk_format_fun ~buffer:(Arg 0) ~format:1 ScanfLike

  (* syslog.h *)
  | "syslog"   -> mk_format_fun ~buffer:(Syslog) ~format:1 PrintfLike

  (* wchar.h *)
  | "fwprintf" -> mk_format_fun ~buffer:(Stream 0) ~format:1 PrintfLike
  | "swprintf" -> mk_format_fun ~buffer:(Arg 0) ~format:1 PrintfLike
  | "wprintf"  -> mk_format_fun ~buffer:(StdIO) ~format:0 PrintfLike
  | "fwscanf"  -> mk_format_fun ~buffer:(Stream 0) ~format:1 ScanfLike
  | "swscanf"  -> mk_format_fun ~buffer:(StdIO) ~format:0 ScanfLike
  | "wscanf"   -> mk_format_fun ~buffer:(Arg 0) ~format:1 ScanfLike

  (* Anything else *)
  | _ -> Unknown


let classify env vi =
  if Extends.Cil.is_variadic_function vi then begin
    Self.result ~level:2 ~current:true
      "Declaration of variadic function %s." vi.vname;
    Some {
      vf_decl = vi;
      vf_original_type = vi.vtype;
      vf_class = if vi.vdefined then Defined else classify_std env vi;
      vf_specialization_count = 0
    }
  end else
    None

