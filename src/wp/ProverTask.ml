(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Library for Running Provers                                        --- *)
(* -------------------------------------------------------------------------- *)

open Task

(* -------------------------------------------------------------------------- *)
(* --- Export Printer                                                     --- *)
(* -------------------------------------------------------------------------- *)

class printer fmt title =
  let bar = String.make 50 '-' in
  object(self)
    val mutable lastpar = true
    initializer 
      begin
        Format.fprintf fmt "(* ----%s---- *)@\n" bar ;
        Format.fprintf fmt "(* --- %-50s --- *)@\n" title ;
        Format.fprintf fmt "(* ----%s---- *)@\n" bar ;
      end
    method paragraph =
      Format.pp_print_newline fmt () ;
      lastpar <- true
    method lines =
      if lastpar then
        Format.pp_print_newline fmt () ;
      lastpar <- false
    method hline =
      self#paragraph ;
      Format.fprintf fmt "(* %s *)@\n" bar
    method section s =
      self#paragraph ;
      Format.fprintf fmt "(* --- %-20s --- *)@\n" s
    method printf : 'a. ('a,Format.formatter,unit) format -> 'a = 
      fun msg -> Format.fprintf fmt msg
  end

(* -------------------------------------------------------------------------- *)
(* --- Buffer Validation                                                  --- *)
(* -------------------------------------------------------------------------- *)

class type pattern = 
  object
    method get_after : ?offset:int -> int -> string
    method get_string : int -> string
    method get_int : int -> int
    method get_float : int -> float
  end

class group text =
  object
    method search re pos =
      ignore (Str.search_forward re text pos)

    method next = Str.match_end ()

    method get_after ?(offset=0) k = 
      try
        let n = String.length text in
        let p = Str.group_end k + offset + 1 in
        if p >= n then "" else String.sub text p (n-p)
      with Not_found -> ""
    method get_string k = try Str.matched_group k text with Not_found -> ""
    method get_int k = 
      try int_of_string (Str.matched_group k text)
      with Not_found | Failure _ -> 0
    method get_float k = 
      try float_of_string (Str.matched_group k text)
      with Not_found | Failure _ -> 0.0
  end

let rec validate_pattern ((re,all,job) as p) group pos =
  group#search re pos ; 
  job (group :> pattern) ;
  if all then validate_pattern p group group#next

let validate_buffer buffer validers =
  let text = Buffer.contents buffer in
  let group = new group text in
  List.iter
    (fun pattern -> 
       try validate_pattern pattern group 0
       with Not_found -> ()
    ) validers

let dump_buffer buffer = function
  | None -> ()
  | Some log ->
      let n = Buffer.length buffer in
      if n > 0 then 
        Command.write_file log (fun out -> Buffer.output_buffer out buffer)
      else if Wp_parameters.is_out () then
        Extlib.safe_remove log

let echo_buffer buffer =
  let n = Buffer.length buffer in
  if n > 0 then
    Log.print_on_output 
      (fun fmt ->
         Format.pp_print_string fmt (Buffer.contents buffer) ;
         Format.pp_print_flush fmt () ;
      )

let location file line = {
  Lexing.pos_fname = file ;
  Lexing.pos_lnum = line ;
  Lexing.pos_bol = 0 ;
  Lexing.pos_cnum = 0 ;
}

let pp_file ~message ~file =
  if Sys.file_exists file then
    Log.print_on_output
      begin fun fmt ->
        let bar = String.make 60 '-' in
        Format.fprintf fmt "%s@\n" bar ;
        Format.fprintf fmt "--- %s :@\n" message ;
        Format.fprintf fmt "%s@\n" bar ;
        Command.pp_from_file fmt file ;
        Format.fprintf fmt "%s@\n" bar ;
      end

(* -------------------------------------------------------------------------- *)
(* --- Prover Task                                                        --- *)
(* -------------------------------------------------------------------------- *)

let p_group p = Printf.sprintf "\\(%s\\)" p
let p_int = "\\([0-9]+\\)" 
let p_float = "\\([0-9.]+\\)" 
let p_string = "\"\\([^\"]*\\)\"" 
let p_until_space = "\\([^ \t\n]*\\)" 

type logs = [ `OUT | `ERR | `BOTH ]

let is_out = function `OUT | `BOTH -> true | `ERR -> false
let is_err = function `ERR | `BOTH -> true | `OUT -> false

let is_opt a = String.length a > 0 && a.[0] = '-'

let rec pp_args fmt = function
  | [] -> ()
  | a::b::w when is_opt a && not (is_opt a) -> 
      Format.fprintf fmt "@ %s %s" a b ;
      pp_args fmt w
  | a::w ->
      Format.fprintf fmt "@ %s" a ;
      pp_args fmt w

class command name =
  object

    val mutable once = true
    val mutable cmd = name
    val mutable param : string list = []
    val mutable timeout = 0
    val mutable validout = []
    val mutable validerr = []
    val mutable timers = []
    val stdout = Buffer.create 256
    val stderr = Buffer.create 256

    method set_command name = cmd <- name
    method add args = param <- param @ args

    method add_parameter ~name phi =
      if phi () then param <- param @ [name]

    method add_int ~name ~value =
      param <- param @ [ name ; string_of_int value ]

    method add_positive ~name ~value =
      if value > 0 then param <- param @ [ name ; string_of_int value ]

    method add_float ~name ~value =
      param <- param @ [ name ; string_of_float value ]

    method add_list ~name values = 
      List.iter (fun v -> param <- param @ [ name ; v ]) values

    method timeout t = timeout <- t

    method validate_pattern ?(logs=`BOTH) ?(repeat=false) 
        regexp (handler : pattern -> unit) = 
      begin
        let v = [regexp,repeat,handler] in
        if is_out (logs:logs) then validout <- validout @ v ; 
        if is_err (logs:logs) then validerr <- validerr @ v ;
      end

    method validate_time phi = timers <- timers @ [phi]

    method run ?(echo=false) ?logout ?logerr () : int Task.task =
      assert once ; once <- false ;
      let time = ref 0.0 in
      let args = Array.of_list param in
      Buffer.clear stdout ;
      Buffer.clear stderr ;
      Task.command ~timeout ~time ~stdout ~stderr cmd args
      >>?
      begin fun st -> (* finally *)
        if Wp_parameters.has_dkey "prover" then
          Log.print_on_output
            begin fun fmt ->
              Format.fprintf fmt "@[<hov 2>RUN '%s%a'@]@." cmd pp_args param ;
              Format.fprintf fmt "RESULT %a@." (Task.pretty Format.pp_print_int) st ;
              Format.fprintf fmt "OUT:@\n%s" (Buffer.contents stdout) ;
              Format.fprintf fmt "ERR:@\n%sEND@." (Buffer.contents stderr) ;
            end ;
        dump_buffer stdout logout ;
        dump_buffer stderr logerr ;
        if echo then
          begin match st with
            | Task.Result 0 | Task.Canceled | Task.Timeout -> ()
            | Task.Result 127 ->
                begin
                  Wp_parameters.error "%s not installed (exit status 127)@." cmd ;
                  echo_buffer stdout ;
                  echo_buffer stderr ;
                end
            | Task.Result s ->
                begin
                  Wp_parameters.error "%s exit with status [%d]@." cmd s ;
                  echo_buffer stdout ;
                  echo_buffer stderr ;
                end
            | Task.Failed exn ->
                begin
                  Wp_parameters.error "%s fails: %s@." cmd (Task.error exn) ;
                  echo_buffer stdout ;
                  echo_buffer stderr ;
                end
          end ;
        let t = !time in
        List.iter (fun phi -> phi t) timers ;
        validate_buffer stderr validerr ;
        validate_buffer stdout validout ;
        Buffer.clear stdout ;
        Buffer.clear stderr ;
      end

  end

(* -------------------------------------------------------------------------- *)
(* --- Task Server                                                        --- *)
(* -------------------------------------------------------------------------- *)

let server = ref None
let server () =
  match !server with
  | Some s ->
      let procs = Wp_parameters.Procs.get () in
      Task.set_procs s procs ; s
  | None ->
      let procs = Wp_parameters.Procs.get () in
      let s = Task.server ~procs () in
      Task.on_server_stop s Proof.savescripts ;
      server := Some s ; s

(* -------------------------------------------------------------------------- *)
(* --- Task Composition                                                   --- *)
(* -------------------------------------------------------------------------- *)

let spawn jobs =
  let pool = ref [] in
  let canceled = ref false in
  let callback r = if not !canceled && r then
      begin
        canceled := true ;
        List.iter Task.cancel !pool ;
      end in
  let server = server () in
  pool := List.map (fun t -> t >>= Task.call callback) jobs ;
  List.iter (Task.spawn server) !pool
