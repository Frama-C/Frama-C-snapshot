(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: journal.ml,v 1.32 2009-01-28 14:34:54 uid568 Exp $ *)

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

include Log.Register
  (struct
     let channel = Log.kernel_channel_name
     let label = Log.kernel_label_name
     let verbose_atleast n = Cmdline.kernel_verbose_level >= n
     let debug_atleast n = Cmdline.kernel_debug_level >= n
   end)

(** Journalization of functions *)

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

exception LoadingError of string

(* [started] prevents journalization of function call 
   inside another one. It is [true] iff a journalized function is being
   applied. *)
let started = ref false

module Sentences = struct

  type t = 
      { sentence: Format.formatter -> unit;
	raise_exn: bool }

  let sentences : t Queue.t = Queue.create ()

  let add print exn = Queue.add { sentence = print; raise_exn = exn } sentences
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
let save = Sentences.save
let restore = Sentences.restore

let now () = Unix.localtime (Unix.time ())

let filename = ref Cmdline.journal_name
let get_name () = !filename
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
    "@[<hv 0>@[<hv 2>Journal.keep_file@;\"%s.ml\";@]@;" 
    !filename;
  Format.fprintf fmt "try run ()@;";
  Format.fprintf fmt "@[<v>with@;@[<hv 2>| Unreachable ->@ ";
  Format.fprintf fmt 
    "@[<hv 2>Kernel.fatal@;\"Journal reachs an assumed dead code\"@;@]@]@;";
  Format.fprintf fmt "@[<hv 2>| Exception s ->@ ";
  Format.fprintf fmt
    "@[<hv 2>Kernel.log@;\"Journal re-raised the exception %%S\"@;s@]@]@;";
  Format.fprintf fmt "@[<hv 2>| exn ->@ ";
  Format.fprintf fmt 
    "@[<hv 2>Kernel.fatal@;\"Journal raised an unexpected exception: %%s\"@;";
  Format.fprintf fmt "(Printexc.to_string exn)@]@]@]@]@]@\n@\n";
  Format.fprintf fmt "@[(* Registering *)@]@\n";
  Format.fprintf fmt
    "@[<hv 2>let main : unit -> unit =@;@[<hv 2>Dynamic.register@;\"%s.main\"@;"
    (String.capitalize (Filename.basename (get_name ())));
  Format.fprintf fmt "@[<hv 2>(Type.func@;Type.unit@;Type.unit)@]@;";
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

let rec get_filename =
  let cpt = ref 0 in
  let rec get_filename first =
    let name = !filename ^ ".ml" in
    if (not first && Sys.file_exists name)
      || List.mem name !preserved_files
    then begin
      incr cpt;
      let suf = "_" ^ string_of_int !cpt in
      (try 
	 let n =
	   Str.search_backward
	     (Str.regexp "_[0-9]+") 
	     !filename 
	     (String.length !filename - 1)
	 in
	 filename := Str.string_before !filename n ^ suf
       with Not_found ->
	 filename := !filename ^ suf);
      get_filename false
    end else
      name
  in
  fun () -> get_filename true

let write () =
  if Cmdline.journal_enable then begin
    let write fmt =
      print_header fmt;
      Sentences.write fmt;
      Format.fprintf fmt "@]@]@;@;";
      print_trailer fmt;
      Format.pp_print_flush fmt ()
    in
    let error msg s = error "cannot %s journal (%s)." msg s in
    let filename = get_filename () in
    feedback ~level:2 "writing journal in file \"%s\"" filename;
    try
      let cout = open_out filename in 
      let fmt = Format.formatter_of_out_channel cout in
      Format.pp_set_margin fmt 78 (* line length *);
      (try write fmt with Sys_error s -> error "write into" s);
      try close_out cout with Sys_error s -> error "close" s
    with Sys_error s -> 
      error "create" s
 end
let () = at_exit write

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

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

let pp ty fmt (x:Obj.t) =
  try Type.pp ty Type.Call fmt (Obj.obj x)
  with Type.NoPrinter _ ->
    fatal 
      "no printer registered for value of type %s.
Journalisation is not possible. Aborting" 
      (Type.name ty)

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

let extend_continuation f_acc pp_arg opt_label arg fmt =
  f_acc fmt;
  match opt_label with
  | None (* no label *) -> Format.fprintf fmt "@;%a" pp_arg arg;
  | Some (_, Some f) when f () == arg ->
      (* [arg] is the default value of the optional label *)
      ()
  | Some (l, _) (* other label *) -> Format.fprintf fmt "@;~%s:%a" l pp_arg arg

(* print any comment *)
let print_comment fmt pp = match pp with
  | None -> () 
  | Some pp -> Format.fprintf fmt "(* %t *)@;" pp

let print_sentence f_acc is_dyn comment ?value ty fmt =
  print_comment fmt comment;
  (* open a new box for the sentence *)
  Format.fprintf fmt "@[<hv 2>";
  (* add a let binding whenever the return type is not unit *)
  if not (Type.equal ty Type.unit) then
    Format.fprintf fmt "let %t=@;"
      (fun fmt ->
	 let binding =
	   match Type.varname ty, value with
	   | None, _ | _, None -> 
	       "__" (* no binding nor value: ignore the result *)
	   | Some f, Some value -> 
	       (* bind to a fresh variable name *)
	       let v = Obj.obj value in
	       let b = gen_binding (f v) in
	       Type.Binding.add ty v b;
	       b
	 in
	 Format.fprintf fmt 
	   "%s%s" 
	   binding
	   (* add the return type for dynamic application *)
	   (if is_dyn then "@;: " ^ Type.name ty else " "));
  (* pretty print the sentence itself in a box *)
  Format.fprintf fmt "@[<hv 2>%t@]" f_acc;
  (* close the sentence *)
  if Type.equal ty Type.unit then Format.fprintf fmt ";@]@;" 
  else Format.fprintf fmt "@;<1 -2>in@]@;"

let add_sentence f_acc is_dyn comment ?value ty = 
  Sentences.add (print_sentence f_acc is_dyn comment ?value ty) false

let catch_exn f_acc is_dyn comment ret_ty exn =
  let s_exn = Printexc.to_string exn in
  (* [s_exn] is not necessarily a valid OCaml exception.
     So don't use it in an ocaml code. *)
  let comment fmt =
    Format.fprintf fmt "@[<hv 2>exception %s@;raised on:@]%t" s_exn 
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

let rec journalize_function f_acc ty is_dyn comment (x:Obj.t) =
  if Type.Function.is_instance_of ty then begin
    (* [ty] is a function type value:
       there exists [a] and [b] such than [ty = a -> b] *)
    let ty : ('a,'b) Type.Function.poly Type.t = Obj.magic (ty:'ty Type.t) in
    let (a:'a Type.t), (b:'b Type.t), opt_label = 
      Type.Function.get_instance ty 
    in
    Obj.repr
      (fun (y:'a) ->
	 if !started (*|| !running*) then
	   (* prevent journalisation if you're journalizing another function *)
	   Obj.repr (Obj.obj x y)
	 else begin
	   let old_started = !started in
	   try
	     (* [started] prevents journalization of function call 
		inside another one *)
	     started := true;
	     (* apply the closure [x] to its argument [y] *)
	     let xy = Obj.obj x y in
	     started := old_started;
	     (* extend the continuation and continue *)
	     let f_acc = extend_continuation f_acc (pp a) opt_label y in
	     journalize_function f_acc b is_dyn comment xy
	   with 
	   | Not_writable name ->
	       started := old_started;
	       fatal
		 "a call to the function %S cannot be written in the journal"
		 name
	   | exn as e->
	       let f_acc = extend_continuation f_acc (pp a) opt_label y in
	       catch_exn f_acc is_dyn comment b exn;
	       started := old_started;
	       raise e
	 end)
  end else begin
    if not !started then add_sentence f_acc is_dyn comment ~value:x ty;
    x
  end
      
let register s ty ?comment ?(is_dyn=false) x =
  if Cmdline.journal_enable then begin
    if s = "" then 
      abort "[Journal.register] the given name should not be \"\"";
    Type.Binding.add_once ty x s;
    if Type.Function.is_instance_of ty then begin
      let x' = Obj.repr x in
      let f_acc fmt = pp ty fmt x' in
      let res : Obj.t = journalize_function f_acc ty is_dyn comment x' in
      Obj.obj res
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
compile-command: "LC_ALL=C make -C ../.."
End:
*)
