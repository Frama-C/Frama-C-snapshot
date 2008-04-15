(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

(** Journalization of functions *)

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

let started = ref true
let enable_exn = ref true
let before_run = ref true
let running = ref false

let journal_buffer = Buffer.create 8192
let journal_fmt = Format.formatter_of_buffer journal_buffer

let something_to_write = ref false

let now () = Unix.localtime (Unix.time ())

exception LoadingError of string

let filename = ref "frama_c_journal"
let get_name () = !filename
let set_name s = filename := s

let is_running () = !running
let finished () = running := false

let print_header fmt =
  let time = now () in
  Format.fprintf fmt
    "(* Frama-C journal generated at %02d:%02d the %02d/%02d/%d *)\n\n"
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_mday
    (time.Unix.tm_mon+1)
    (time.Unix.tm_year + 1900);
  Format.fprintf fmt "(* Running *)@\nlet start () =@\n let () = Journal.run () in@\n"

let print_finished _fmt = ()

let start () = 
  if not !something_to_write then print_header journal_fmt;
  started := true;
  enable_exn := true;
  something_to_write := true

let stop () = 
  started := false;
  enable_exn := false

let clear ?(restart=true) () = 
  Buffer.clear journal_buffer; something_to_write := false; 
  if restart then start ()

let push x = Format.fprintf journal_fmt x

let write () =
  Format.fprintf journal_fmt " (* Finished *)@\n Journal.finished ()@\n
let () =@\n try start ()@\n with e -> Format.eprintf \"Journal raised an exception: %%s\" (Printexc.to_string e)";
  let error msg s =
    Format.eprintf "Error: cannot %s journal (%s).@." msg s
  in
  let filename = !filename ^ ".ml" in
  if !something_to_write then begin
    try
      let cout = open_out filename in begin
      try
	print_finished journal_fmt;
	Format.pp_print_flush journal_fmt ();
	Buffer.output_buffer cout journal_buffer;
      with Sys_error s -> 
	error "write into" s
      end;
      try close_out cout with Sys_error s -> error "close" s
    with Sys_error s -> 
      error "create" s
  end

(* ****************************************************************************)
(** {2 Types registering} *)
(* ****************************************************************************)

let pretty
    : (int, Format.formatter -> Obj.t -> unit) Hashtbl.t 
    = Hashtbl.create 7

let register_printer ty f = 
  Hashtbl.replace pretty
    (Type.id ty) 
    (if Type.Function.is_instance_of ty then (fun _ _ -> assert false)
     else (fun fmt x -> f fmt (Obj.obj x)))

exception PrettyPrinterNotRegistered of string

let is_registered ty = Hashtbl.mem pretty (Type.id ty)

(* Register basic types of module Type *)
let () = 
  register_printer Type.unit (fun fmt () -> Format.fprintf fmt "()");
  register_printer Type.bool (fun fmt b -> Format.fprintf fmt "%B" b);
  register_printer Type.int 
    (fun fmt d -> 
       if d<0 then Format.fprintf fmt "(%d)" d 
       else  Format.fprintf fmt "%d" d);
  register_printer Type.float 
    (fun fmt f -> 
       if f<0. then Format.fprintf fmt "(%f)" f
       else Format.fprintf fmt "%f" f);
  register_printer Type.char (fun fmt c -> Format.fprintf fmt "'%c'" c);
  register_printer Type.string (fun fmt s -> Format.fprintf fmt "%S" s)

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

(* Used for journalization of higher-order function (applied to a
   journalizable function) *)

module FunctionsAsObj = 
  Hashtbl.Make
    (struct 
       type t = Obj.t (* invariant: is an arrow type *)
       let hash f = Obj.magic (Obj.field f 0) 
	 (* assumes that the first word of a closure does not change in
	    anyway (even by Gc.compact invokation). *)
	 (*0*)
       let equal = (==)
     end)

let functions_by_obj : string FunctionsAsObj.t = FunctionsAsObj.create 97

(* These functions are not journalized ! Use only when running a journal *)
let functions_tbl = FunTbl.create 97

(* Table for functions comments *)
let functions_cmt : (string, (Format.formatter -> unit)) Hashtbl.t 
    = Hashtbl.create 97

exception NotJournalizable (* When a higher-order function is applied to an
			      unjournalized one *)

exception NotJournalized of string

let apply = FunTbl.unsafe_apply functions_tbl

let journal_apply_string name =
  Format.sprintf "Journal.apply @[\"%s\"@]" name

let pp ty fmt x =
  if Type.Function.is_instance_of ty then
    try
      let name = FunctionsAsObj.find functions_by_obj x in
      Format.fprintf fmt "%s" (journal_apply_string name)
    with Not_found -> raise NotJournalizable
  else
    try Hashtbl.find pretty (Type.id ty) fmt x with Not_found -> assert false

let check_type ty =
  if not (is_registered ty) then 
    raise (PrettyPrinterNotRegistered (Type.name ty))
      
let rec journalize name acc ty use_apply (x:Obj.t) = 
  let must_write () = !started && not !running in
  if Type.Function.is_instance_of ty then begin
    try
      let (a:'a Type.t), (b:'b Type.t) = Type.split ty in
      if not (Type.Function.is_instance_of a) then check_type a;
      Obj.repr
	(fun (y:'a) ->
	   (* remember the current partial application *)
	   let acc =
	     begin
	       let buf = Buffer.create 17 in
	       let fmt = Format.formatter_of_buffer buf in
	       Format.fprintf fmt "@[%s@;<1 2>%a@]@?" acc (pp a) y;
	       Buffer.contents buf
	     end
	   in
	   try
	     (* do not journalize function call inside a journalised 
		function *)
	     let old_started = !started in
	     started := false;
	     let xy = Obj.obj x y in
	     started := old_started;
	     journalize name acc b use_apply xy
	   with exn ->
	     if !enable_exn then
	       Format.fprintf journal_fmt 
		 " (* exception %s raised on: *)@\n let __ : %s = %s in@\n"
		 (Printexc.to_string exn)
		 (Type.name b)
		 acc;
	     raise exn)
    with Type.Not_functional -> 
      assert false (* not be possible by construction *)
  end else begin
    if must_write () then begin
      (try
	 let pp = Hashtbl.find functions_cmt name in
	 Format.fprintf journal_fmt "%t@\n" pp
       with Not_found -> 
	 (* If not comment pretty printer found, do nothing *)
	 ());
      Format.fprintf journal_fmt " let %t= "
	(fun fmt ->
	   if Type.equal ty Type.unit then
	     Format.fprintf fmt "() " 
	   else
	     Format.fprintf fmt 
	       "_%s " 
	       (if use_apply then "_ : " ^ Type.name ty else ""));
      (* here ty is not an arrow type *)
      if name = "" then begin
	check_type ty;
	(* journalize the basic element *)
	Format.fprintf journal_fmt "%a in@\n" (pp ty) x
      end else
	(* function application is now total: journalize it *)
	Format.fprintf journal_fmt "%s in@\n" acc
    end;
    x
  end
      
let register s ty ?comment ?(use_apply=false) x =
  let x' = Obj.repr x in
  if Type.Function.is_instance_of ty then begin
    let prefix = if use_apply then journal_apply_string s else s in
    let res : Obj.t = journalize s prefix ty use_apply x' in
    FunctionsAsObj.replace functions_by_obj res s;
    begin 
      try FunTbl.register functions_tbl s ty x 
      with FunTbl.AlreadyExists s -> 
	Format.eprintf "The function %s is already registered@." s;
	assert false 
    end;
    (* The previous registration ensures to journalize the function only once.
       Then use of 'Hashtbl.add' here. *)
    begin
      match comment with
      | Some f -> Hashtbl.add functions_cmt s f
      | None -> () 
    end;
    Obj.obj res
  end else
    Obj.obj (journalize "" "" ty use_apply x')

let run () = running := true

module type POLY_INPUT = sig
  
  include Type.POLY_OUTPUT

  val fprintf: (Format.formatter -> 'a -> unit) -> 
    Format.formatter -> 'a poly -> unit
    (** [fprintf pp fmt p] prints the journalization result of the polymorph
	container. [pp] is a pretty printer for elements type. *)
end

module type POLY_OUTPUT = sig

  type 'a poly

  val register_printer: 'a Type.t -> unit
    (** [register ty] register the container with elements of type [ty] and
	returns the new type corresponding to it. 
	
	@raise PrettyPrinterNotRegistered if the given type does not have a
	pretty printer registered for it before. *)
    
end

module Polymorphic (X:POLY_INPUT) = struct

  type 'a poly = 'a X.poly

  let register_printer a =
    check_type a;
    let ty = X.realize a in
    register_printer ty (X.fprintf (pp a))
      
end

module List = Polymorphic
  (struct
     include Type.List
     let fprintf pp fmt l =
       Format.fprintf fmt "@[[%t]@]"
	 (fun fmt -> 
	    List.iter (fun v -> Format.fprintf fmt "%a;@;<1 2>" pp v) l)
   end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
