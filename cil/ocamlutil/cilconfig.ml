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

module H = Hashtbl


(************************************************************************

 Configuration

 ************************************************************************)

let absoluteFilename (fname: string) =
  if Filename.is_relative fname then
    Filename.concat (Sys.getcwd ()) fname
  else
    fname


(** The configuration data can be of several types **)
type configData =
    ConfInt of int
  | ConfBool of bool
  | ConfFloat of float
  | ConfString of string
  | ConfList of configData list


(* Store here window configuration file *)
let configurationData: (string, configData) H.t = H.create 13

let clearConfiguration () = H.clear configurationData

let setConfiguration (key: string) (c: configData) =
  H.replace configurationData key c

let findConfiguration (key: string) : configData =
  H.find configurationData key

let findConfigurationInt (key: string) : int =
  match findConfiguration key with
    ConfInt i -> i
  | _ ->
      Kernel.warning "Configuration %s is not an integer" key;
      raise Not_found

let findConfigurationFloat (key: string) : float =
  match findConfiguration key with
    ConfFloat i -> i
  | _ ->
      Kernel.warning "Configuration %s is not a float" key;
      raise Not_found

let useConfigurationInt (key: string) (f: int -> unit) =
  try f (findConfigurationInt key)
  with Not_found -> ()

let useConfigurationFloat (key: string) (f: float -> unit) =
  try f (findConfigurationFloat key)
  with Not_found -> ()

let findConfigurationString (key: string) : string =
  match findConfiguration key with
    ConfString s -> s
  | _ ->
      Kernel.warning "Configuration %s is not a string" key;
      raise Not_found

let useConfigurationString (key: string) (f: string -> unit) =
  try f (findConfigurationString key)
  with Not_found -> ()


let findConfigurationBool (key: string) : bool =
  match findConfiguration key with
    ConfBool b -> b
  | _ ->
      Kernel.warning "Configuration %s is not a boolean" key;
      raise Not_found

let useConfigurationBool (key: string) (f: bool -> unit) =
  try f (findConfigurationBool key)
  with Not_found -> ()

let findConfigurationList (key: string) : configData list  =
  match findConfiguration key with
    ConfList l -> l
  | _ ->
      Kernel.warning "Configuration %s is not a list" key;
      raise Not_found

let useConfigurationList (key: string) (f: configData list -> unit) =
  try f (findConfigurationList key)
  with Not_found -> ()


let saveConfiguration (fname: string) =
  (** Convert configuration data to a string, for saving externally *)
  let configToString (c: configData) : string =
    let buff = Buffer.create 80 in
    let rec loop (c: configData) : unit =
      match c with
        ConfInt i ->
          Buffer.add_char buff 'i';
          Buffer.add_string buff (string_of_int i);
          Buffer.add_char buff ';'

      | ConfBool b ->
          Buffer.add_char buff 'b';
          Buffer.add_string buff (string_of_bool b);
          Buffer.add_char buff ';'

      | ConfFloat f ->
          Buffer.add_char buff 'f';
          Buffer.add_string buff (string_of_float f);
          Buffer.add_char buff ';'

      | ConfString s ->
          if String.contains s '"' then
            Kernel.fatal "Guilib: configuration string contains quotes";
          Buffer.add_char buff '"';
          Buffer.add_string buff s;
          Buffer.add_char buff '"'; (* '"' *)

      | ConfList l ->
          Buffer.add_char buff '[';
          List.iter loop l;
          Buffer.add_char buff ']'
    in
    loop c;
    Buffer.contents buff
  in
  try
    let oc = open_out fname in
    Kernel.debug "Saving configuration to %s@." (absoluteFilename fname);
    H.iter (fun k c ->
      output_string oc (k ^ "\n");
      output_string oc ((configToString c) ^ "\n"))
      configurationData;
    close_out oc
  with _ ->
    Kernel.warning "Cannot open configuration file %s\n" fname


(** Make some regular expressions early *)
let intRegexp = Str.regexp "i\\([^;]+\\);"
let floatRegexp = Str.regexp "f\\([^;]+\\);"
let boolRegexp = Str.regexp "b\\(\\(true\\)\\|\\(false\\)\\);"
let stringRegexp = Str.regexp "\"\\([^\"]*\\)\""

let loadConfiguration (fname: string) : unit =
  H.clear configurationData;

  let stringToConfig (s: string) : configData =
    let idx = ref 0 in (** the current index *)
    let l = String.length s in

    let rec getOne () : configData =
      if !idx >= l then raise Not_found;
      if Str.string_match intRegexp s !idx then begin
        idx := Str.match_end ();
	let p = Str.matched_group 1 s in
        (try ConfInt (int_of_string p)
	 with Failure "int_of_string" ->
	   Kernel.warning "Invalid integer configuration element %s" p;
	   raise Not_found)
      end else if Str.string_match floatRegexp s !idx then begin
        idx := Str.match_end ();
	let p = Str.matched_group 1 s in
        (try ConfFloat (float_of_string p)
	 with Failure "float_of_string" ->
	   Kernel.warning "Invalid float configuration element %s" p;
	   raise Not_found)
      end else if Str.string_match boolRegexp s !idx then begin
        idx := Str.match_end ();
        ConfBool (bool_of_string (Str.matched_group 1 s))
      end else if  Str.string_match stringRegexp s !idx then begin
        idx := Str.match_end ();
        ConfString (Str.matched_group 1 s)
      end else if String.get s !idx = '[' then begin
        (* We are starting a list *)
        incr idx;
        let rec loop (acc: configData list) : configData list =
          if !idx >= l then begin
            Kernel.warning "Non-terminated list in configuration %s" s;
            raise Not_found
          end;
          if String.get s !idx = ']' then begin
            incr idx;
            List.rev acc
          end else
            loop (getOne () :: acc)
        in
        ConfList (loop [])
      end else begin
        Kernel.warning "Bad configuration element in a list: %s"
                  (String.sub s !idx (l - !idx));
        raise Not_found
      end
    in
    getOne ()
  in
  (try
    let ic = open_in fname in
    Kernel.debug "Loading configuration from %s@." (absoluteFilename fname);
    (try
      while true do
        let k = input_line ic in
        let s = input_line ic in
        try
          let c = stringToConfig s in
          setConfiguration k c
        with Not_found -> ()
      done
    with End_of_file -> ());
    close_in ic;
  with _ -> () (* no file, ignore *));

  ()


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
