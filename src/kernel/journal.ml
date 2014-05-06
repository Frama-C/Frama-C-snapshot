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

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

(* Disclaimer
   ----------
   This module uses very unsafe caml features (module Obj).
   Modify it at your own risk.
   Sometimes the caml type system does not help you here.
   Introducing a bug here may introduce some "segmentation faults" in Frama-C *)

(* ****************************************************************************)
(* ****************************************************************************)
(* ****************************************************************************)

open Cmdline.Kernel_log

(** Journalization of functions *)

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

(* [started] prevents journalization of function call
   inside another one. It is [true] iff a journalized function is being
   applied. *)
let started = ref false

module Sentences = struct

  type t =
      { sentence: Format.formatter -> unit;
        raise_exn: bool }

  let sentences : t Queue.t = Queue.create ()

  let add print exn =
    Queue.add { sentence = print; raise_exn = exn } sentences

  let write fmt =
    let finally_raised = ref false in
    (* printing the sentences *)
    Queue.iter
      (fun s -> s.sentence fmt; finally_raised := s.raise_exn)
      sentences;
    (* if any, re-raised the exception raised by the last sentence *)
    Format.fprintf fmt "@[%s@]"
      (if !finally_raised then "raise (Exception (Printexc.to_string exn))"
       else "()");
    (* closing the box opened when catching exception *)
    Queue.iter
      (fun s -> if s.raise_exn then Format.fprintf fmt "@]@]@]@;end")
      sentences

  let journal_copy = ref (Queue.create ())
  let save () =  journal_copy := Queue.copy sentences
  let restore () =
    Queue.clear sentences;
    Queue.transfer !journal_copy sentences

end

module Abstract_modules = struct
  let tbl: (string, string) Hashtbl.t = Hashtbl.create 7
  let () = Type.add_abstract_types := Hashtbl.replace tbl
  let write fmt =
    Hashtbl.iter
      (fun k v -> 
	Format.fprintf fmt
	  "@[<hv 2>let module %s=@;@[<hv 0>Type.Abstract\
(struct let name = %S end) in@]@]@;"
	  k v)
      tbl
  let tbl_copy = ref (Hashtbl.create 7)
  let save () = tbl_copy := Hashtbl.copy tbl
  let restore () =
    Hashtbl.clear tbl;
    Hashtbl.iter (fun k v -> Hashtbl.add tbl k v) !tbl_copy
end

let save () =
  Sentences.save ();
  Abstract_modules.save ()

let restore () =
  Sentences.restore ();
  Abstract_modules.restore ()

let now () = Unix.localtime (Unix.time ())

let default_filename = "frama_c_journal.ml"
let filename = ref default_filename
let get_session_file = ref (fun _ -> assert false)
let get_name () = 
  let f = !filename in
  if f == default_filename then !get_session_file f else f

let set_name s = filename := s

let print_header fmt =
  let time = now () in
  Format.pp_open_hvbox fmt 0; (* the outermost box *)
  Format.fprintf fmt
    "@[(* Frama-C journal generated at %02d:%02d the %02d/%02d/%d *)@]@;@;"
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_mday
    (time.Unix.tm_mon+1)
    (time.Unix.tm_year + 1900);
  Format.fprintf fmt "@[exception Unreachable@]@;";
  Format.fprintf fmt "@[exception Exception of string@]@;@;";
  Format.fprintf fmt (* open two boxes for start *)
    "(* Run the user commands *)@;@[<hv 2>let run () =@;@[<hv 0>"

let print_trailer fmt =
  Format.fprintf fmt "@[(* Main *)@]@\n";
  Format.fprintf fmt "@[<hv 2>let main () =@;";
  Format.fprintf fmt
    "@[<hv 0>@[<hv 2>Journal.keep_file@;\"%s\";@]@;"
    (get_name  ());
  Format.fprintf fmt "try run ()@;";
  Format.fprintf fmt "@[<v>with@;@[<hv 2>| Unreachable ->@ ";
  Format.fprintf fmt
    "@[<hv 2>Kernel.fatal@;\"Journal reaches an assumed dead code\"@;@]@]@;";
  Format.fprintf fmt "@[<hv 2>| Exception s ->@ ";
  Format.fprintf fmt
    "@[<hv 2>Kernel.log@;\"Journal re-raised the exception %%S\"@;s@]@]@;";
  Format.fprintf fmt "@[<hv 2>| exn ->@ ";
  Format.fprintf fmt
    "@[<hv 2>Kernel.fatal@;\"Journal raised an unexpected exception: %%s\"@;";
  Format.fprintf fmt "(Printexc.to_string exn)@]@]@]@]@]@\n@\n";
  Format.fprintf fmt "@[(* Registering *)@]@\n";
  Format.fprintf fmt
    "@[<hv 2>let main : unit -> unit =@;@[<hv 2>Dynamic.register@;~plugin:%S@;\"main\"@;"
    (String.capitalize (Filename.basename (get_name ())));
  Format.fprintf fmt
    "@[<hv 2>(Datatype.func@;Datatype.unit@;Datatype.unit)@]@;";
  Format.fprintf fmt "~journalize:false@;main@]@]@\n@\n";
  Format.fprintf fmt "@[(* Hooking *)@]@\n";
  Format.fprintf fmt "@[<hv 2>let () =@;";
  Format.fprintf fmt
    "@[<hv 2>Cmdline.run_after_loading_stage@;main;@]@;";
  Format.fprintf fmt "@[<hv 2>Cmdline.is_going_to_load@;()@]@]@.";
  (* close the outermost box *)
  Format.pp_close_box fmt ()

let preserved_files = ref []
let keep_file s = preserved_files := s :: !preserved_files

let get_filename =
  let cpt = ref 0 in
  let rec get_filename first =
    let name = get_name () in
    if (not first && Sys.file_exists name) || List.mem name !preserved_files
    then begin
      incr cpt;
      let suf = "_" ^ string_of_int !cpt in
      (try
         let n =
           Str.search_backward
	     (Str.regexp "_[0-9]+") 
	     name
	     (String.length name - 1)
         in
         filename := Str.string_before name n ^ suf
       with Not_found ->
         filename := name ^ suf);
      get_filename false
    end else
      name
  in
  fun () -> get_filename true

let write () =
  let write fmt =
    print_header fmt;
    Abstract_modules.write fmt;
    Sentences.write fmt;
    Format.fprintf fmt "@]@]@;@;";
    print_trailer fmt;
    Format.pp_print_flush fmt ()
  in
  let error msg s = error "cannot %s journal (%s)." msg s in
  let filename = get_filename () in
  feedback "writing journal in file `%s'." filename;
  try
    let cout = open_out filename in
    let fmt = Format.formatter_of_out_channel cout in
    Format.pp_set_margin fmt 78 (* line length *);
    (try write fmt with Sys_error s -> error "write into" s);
    try close_out cout with Sys_error s -> error "close" s
  with Sys_error s ->
    error "create" s

let () =
  (* write the journal iff it is enable and
     - either an error occurs;
     - or the user explicitly wanted it. *)
  if Cmdline.journal_enable then begin
    Cmdline.at_error_exit (fun _ -> write ());
    if Cmdline.journal_isset then Cmdline.at_normal_exit write
  end

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

module Binding: sig
  val add: 'a Type.t -> 'a -> string -> unit
    (** [add ty v var] binds the value [v] to the variable name [var].  Thus,
        [pp ty v] prints [var] and not use the standard pretty printer.  Very
        useful to pretty print values with no associated pretty printer. *)
  exception Name_already_exists of string
  val add_once: 'a Type.t -> 'a -> string -> unit
    (** Same as function [add] above but raise the exception [Already_exists]
        if the binding previously exists *)
  val find: 'a Type.t -> 'a -> string
  val iter: ('a Type.t -> 'a -> string -> unit) -> unit
end = struct

  let bindings : string Type.Obj_tbl.t = Type.Obj_tbl.create ()

  let add ty v var =
    Type.Obj_tbl.add bindings ty v var (* eta-expansion required *)

  (* add bindings for [Format.std_formatter] and [Format.err_formatter] *)
  let () =
    add Datatype.formatter Format.std_formatter "Format.std_formatter";
    add Datatype.formatter Format.err_formatter "Format.err_formatter"

  exception Name_already_exists of string
  let check_name s =
    let error () =
      Format.eprintf "[Type] A value of name %s already exists@." s;
      raise (Name_already_exists s)
    in
    Type.Obj_tbl.iter bindings (fun _ _ s' -> if s = s' then error ())

  let add_once ty x s =
    check_name s;
    add ty x s

  let find ty v = Type.Obj_tbl.find bindings ty v (* eta-expansion required *)
  let iter f = Type.Obj_tbl.iter bindings f (* eta-expansion required *)

  (* predefined bindings *)
  let () = 
    add Datatype.formatter Format.std_formatter "Format.std_formatter";
    add Datatype.formatter Format.err_formatter "Format.err_formatter"

end

(* JS 2012/02/07: useful only for BM introspection testing ;-) *)
module Reverse_binding = struct
  module Tbl = Type.String_tbl(struct type 'a t = 'a end)
  exception Unbound_value = Tbl.Unbound_value
  exception Incompatible_type = Tbl.Incompatible_type

  let tbl = Tbl.create 97
  let fill () = Binding.iter (fun ty v name -> Tbl.add tbl name ty v)
  let find name ty = Tbl.find tbl name ty
  let iter f = Tbl.iter f tbl

  let pretty fmt () =
    iter
      (fun name ty v -> 
	Format.fprintf fmt "%s --> %a@." name (Datatype.pretty ty) v)

end

exception Not_writable of string
let never_write name f =
  if Cmdline.journal_enable && Cmdline.use_type then
    if Obj.tag (Obj.repr f) = Obj.closure_tag then
      Obj.magic
        (fun y -> if !started then Obj.magic f y else raise (Not_writable name))
    else
      invalid_arg ("[Journal.never_write] " ^ name ^ " is not a closure")
  else
    f

let pp (type t) (ty: t Type.t) fmt (x:t) =
  assert Cmdline.use_type;
  try Format.fprintf fmt "%s" (Binding.find ty x);
  with Not_found ->
    let pp_error msg =
      Format.fprintf fmt "@[<hov 2>(failwith @[<hov 2>\"%s:@ running the journal will fail.\"@])@;@]" msg
    in
      let pp = Datatype.internal_pretty_code ty in
      if pp == Datatype.undefined then
	pp_error 
	  (Pretty_utils.sfprintf
	     "no printer registered for value of type %s"
             (Type.name ty))
      else if pp == Datatype.pp_fail then
	pp_error
	  (Pretty_utils.sfprintf
             "no code for pretty printer of type %s"
             (Type.name ty))
      else
	pp Type.Call fmt x

let gen_binding =
  let ids = Hashtbl.create 7 in
  let rec gen s =
    try
      let n = succ (Hashtbl.find ids s) in
      Hashtbl.replace ids s n;
      gen (s ^ "_" ^ string_of_int n)
    with Not_found ->
      Hashtbl.add ids s 1;
      s
  in
  gen

let extend_continuation f_acc pp_arg opt_label opt_arg arg fmt =
  f_acc fmt;
  match opt_label, opt_arg with
  | None, None (* no label *) -> Format.fprintf fmt "@;%a" pp_arg arg;
  | None, Some _ -> assert false
  | Some _, Some f when f () == arg ->
    (* [arg] is the default value of the optional label *)
    ()
  | Some l, _ (* other label *) -> Format.fprintf fmt "@;~%s:%a" l pp_arg arg

(* print any comment *)
let print_comment fmt pp = match pp with
  | None -> ()
  | Some pp -> Format.fprintf fmt "(* %t *)@;" pp

let print_sentence f_acc is_dyn comment ?value ty fmt =
  assert Cmdline.use_type;
  print_comment fmt comment;
  (* open a new box for the sentence *)
  Format.fprintf fmt "@[<hv 2>";
  (* add a let binding whenever the return type is not unit *)
  let is_unit = Type.equal ty Datatype.unit in
  if not is_unit then
    Format.fprintf fmt "let %t=@;"
      (fun fmt ->
         let binding =
           let varname = Datatype.varname ty in
           match varname == Datatype.undefined, value with
           | true, _ | _, None ->
               "__" (* no binding nor value: ignore the result *)
           | false, Some value ->
               (* bind to a fresh variable name *)
               let b = gen_binding (varname value) in
               Binding.add ty value b;
               b
         in
         Format.fprintf fmt "%s" binding;
         (* add the return type for dynamic application *)
         if is_dyn then Format.fprintf fmt "@;: %s " (Type.name ty)
         else Format.fprintf fmt " ");
  (* pretty print the sentence itself in a box *)
  Format.fprintf fmt "@[<hv 2>%t@]" f_acc;
  (* close the sentence *)
  if is_unit then Format.fprintf fmt ";@]@;"
  else Format.fprintf fmt "@;<1 -2>in@]@;"

let add_sentence f_acc is_dyn comment ?value ty =
  Sentences.add (print_sentence f_acc is_dyn comment ?value ty) false

let catch_exn f_acc is_dyn comment ret_ty exn =
  let s_exn = Printexc.to_string exn in
  (* [s_exn] is not necessarily a valid OCaml exception.
     So don't use it in OCaml code. *)
  let comment fmt =
    Format.fprintf fmt "@[<hv 2>exception %s@;raised on: @]%t" s_exn
      (fun fmt -> Extlib.may (fun f -> f fmt) comment)
  in
  let print fmt =
    (* open a new box for the sentence *)
    Format.fprintf fmt
      "@[<hv 2>begin try@;@[<hv>%t@[<hv 2>raise Unreachable@]@]@]@;"
      (print_sentence f_acc is_dyn (Some comment) ret_ty);
    (* two opened boxes closed at end *)
    Format.fprintf fmt
      "@[<v>with@;@[<hv 2>| Unreachable as exn -> raise exn@]@;";
    Format.fprintf fmt
      "@[<hv 2>| exn (* %s *) ->@;@[<hv>@[(* continuing: *)@]@;" s_exn
  in
  Sentences.add print true

let rec journalize_function: 't. 
    (Format.formatter -> unit) -> 't Type.t -> bool -> 
  (Format.formatter -> unit) option -> 't -> 't =
  fun (type t) (type a) (type b) f_acc (ty: t Type.t) is_dyn comment (x:t)
  ->
  assert Cmdline.use_type;
  if Type.Function.is_instance_of ty then begin
    (* [ty] is a function type value:
       there exists [a] and [b] such than [t = a -> b] *)
    let ty: (a -> b) Type.t = Obj.magic (ty: t Type.t) in
    let f: a -> b = Obj.magic (x: t) in
    let (a: a Type.t), (b: b Type.t), opt_label =
      Type.Function.get_instance ty
    in
    let opt_arg = Type.Function.get_optional_argument ty in
    let f (y: a) : b =
      if !started then
        (* prevent journalisation if you're journalizing another function *)
        f y
      else begin
        try
          (* [started] prevents journalization of function call
             inside another one *)
          started := true;
          (* apply the closure [x] to its argument [y] *)
          let xy = f y in
          started := false;
          (* extend the continuation and continue *)
          let f_acc = extend_continuation f_acc (pp a) opt_label opt_arg y in
          journalize_function f_acc b is_dyn comment xy
        with
        | Not_writable name ->
          started := false;
          fatal
            "a call to the function %S cannot be written in the journal"
            name
        | exn as e ->
          let f_acc = extend_continuation f_acc (pp a) opt_label opt_arg y in
          catch_exn f_acc is_dyn comment b exn;
          started := false;
          raise e
      end in
   (* cast back the closure of type [a -> b] into [t] *)
   (Obj.magic (f: a -> b): t)
  end else begin
    if not !started then add_sentence f_acc is_dyn comment ~value:x ty;
    x
  end

let register s ty ?comment ?(is_dyn=false) x =
  if Cmdline.journal_enable then begin
    assert Cmdline.use_type;
    if s = "" then
      abort "[Journal.register] the given name should not be \"\"";
    Binding.add_once ty x s;
    if Type.Function.is_instance_of ty then begin
      let f_acc fmt = pp ty fmt x in
      journalize_function f_acc ty is_dyn comment x
    end else
      x
  end else
    x

let prevent f x =
  let old = !started in
  started := true;
  let res = try f x with exn -> started := old; raise exn in
  started := old;
  res

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
