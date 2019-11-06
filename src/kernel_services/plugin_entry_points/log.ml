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

type kind = Result | Feedback | Debug | Warning | Error | Failure

[@@@ warning "-32"]

let pretty_kind fmt = function
  | Result -> Format.fprintf fmt "Result"
  | Feedback -> Format.fprintf fmt "Feedback"
  | Debug -> Format.fprintf fmt "Debug"
  | Warning -> Format.fprintf fmt "Warning"
  | Error -> Format.fprintf fmt "Error"
  | Failure -> Format.fprintf fmt "Failure"

[@@@ warning "+32"]

type event = {
  evt_kind : kind ;
  evt_plugin : string ;
  evt_category : string option;
  evt_source : Filepath.position option ;
  evt_message : string ;
}

let kernel_channel_name = "kernel"
let kernel_label_name = "kernel"

(* -------------------------------------------------------------------------- *)
(* --- Exception Management                                               --- *)
(* -------------------------------------------------------------------------- *)

exception FeatureRequest of string * string
exception AbortError of string (* plug-in *)
exception AbortFatal of string (* plug-in *)

(* -------------------------------------------------------------------------- *)
(* --- Terminal Management                                                --- *)
(* -------------------------------------------------------------------------- *)

open Format

type lock =
  | Ready
  | Locked
  | DelayedLock

type terminal = {
  mutable lock : lock ;
  mutable isatty : bool ;
  mutable clean : bool ;
  mutable delayed : (terminal -> unit) list ;
  mutable output : string -> int -> int -> unit ;
  (* Same as Format.make_formatter *)
  mutable flush : unit -> unit ;  
  (* Same as Format.make_formatter *)
}

let delayed_echo t =
  match t.lock with
  | Locked -> true
  | Ready | DelayedLock -> false

let is_locked t =
  match t.lock with
  | Locked | DelayedLock -> true
  | Ready -> false

let is_ready t =
  match t.lock with
  | Locked | DelayedLock -> false
  | Ready -> true

let term_clean t = 
  if t.isatty && not t.clean then
    begin
      let u = "\r\027[K" in
      (* TERM escape commands:
         "\r" is carriage return ;
         "\027[K" is CSI command EL 'Erase in Line' ;
         See https://en.wikipedia.org/wiki/ANSI_escape_code
      *)
      t.output u 0 (String.length u) ;
      t.clean <- true ;
    end

let set_terminal t isatty output flush =
  begin
    (* Ensures previous terminal state is clean *)
    assert (is_ready t) ;
    term_clean t ;
    (* Now reconfigure the terminal *)
    t.isatty <- isatty ;
    t.output <- output ;
    t.flush <- flush ;
    t.clean <- true ;
  end

let stdout = {
  lock = Ready ;
  clean = true ;
  delayed = [] ;
  isatty = Unix.isatty Unix.stdout ;
  output = output_substring stdout ;
  flush =  (fun () -> flush stdout);
}

let clean () = term_clean stdout

let set_output ?(isatty=false) output flush = 
  set_terminal stdout isatty output flush

(* -------------------------------------------------------------------------- *)
(* --- Locked Formatter                                                   --- *)
(* -------------------------------------------------------------------------- *)

type delayed =
  | Delayed of terminal
  | Formatter of (string -> int -> int -> unit) * (unit -> unit)

let lock_terminal t =
  begin
    if is_locked t then
      failwith "Console is already locked" ;
    term_clean t ;
    t.lock <- Locked ;
    Format.make_formatter t.output t.flush ;
  end

let unlock_terminal t fmt =
  if is_ready t then
    failwith "Console can not be unlocked" ;
  begin
    Format.pp_print_flush fmt () ;
    t.lock <- Ready ;
    List.iter
      (fun job -> job t)
      (List.rev t.delayed) ;
    t.delayed <- [] ;
  end

let print_on_output job =
  let fmt = lock_terminal stdout in
  try job fmt ; unlock_terminal stdout fmt
  with error -> unlock_terminal stdout fmt ; raise error

(* -------------------------------------------------------------------------- *)
(* --- Delayed Lock until first write                                     --- *)
(* -------------------------------------------------------------------------- *)

let delayed_terminal terminal =
  if is_locked terminal then
    failwith "Console is already locked" ;
  terminal.lock <- DelayedLock ;
  let d = ref (Delayed terminal) in
  let d_output d text k n =
    match !d with
    | Delayed t ->
      t.lock <- Locked ;
      d := Formatter( t.output , t.flush ) ;
      t.output text k n
    | Formatter(out,_) ->
      out text k n
  in
  let d_flush d () =
    match !d with
    | Delayed _ -> () (* nothing to flush yet ! *)
    | Formatter(_,flush) -> flush ()
  in
  Format.make_formatter (d_output d) (d_flush d)

let print_delayed job =
  let fmt = delayed_terminal stdout in
  try job fmt ; unlock_terminal stdout fmt
  with error -> unlock_terminal stdout fmt ; raise error

(* -------------------------------------------------------------------------- *)
(* --- Echo Line(s)                                                       --- *)
(* -------------------------------------------------------------------------- *)

(* whenever the first line of the event shall be printed along the prefix *)
let is_prefixed_event = function
  | { evt_category = None ; evt_source = None } -> true
  | _ -> false

let is_single_line text =
  try ignore (String.index_from text 0 '\n') ; false
  with Not_found -> true 

let echo_firstline output text p q width =
  let t = try String.index_from text p '\n' with Not_found -> succ q in
  let n = min width (t-p) in
  output text p n

let echo_newline output =
  output "\n" 0 1

(* output indentation unless the first line is along the prefix *)
let echo_line output ~prefix text k n =
  if not prefix then output "  " 0 2 ; output text k n

let rec echo_lines ?(prefix=false) output text p q =
  if p <= q then
    let t = try String.index_from text p '\n' with Not_found -> (-1) in
    if t < 0 || t > q then
      begin
        (* incomplete, last line *)
        echo_line output ~prefix text p (q+1-p) ;
        echo_newline output ;
      end
    else
      begin
        (* complete line *)
        echo_line output ~prefix text p (t+1-p) ;
        echo_lines output text (t+1) q ;
      end

(* -------------------------------------------------------------------------- *)
(* --- Echo Event                                                         --- *)
(* -------------------------------------------------------------------------- *)

let add_source buffer = function
  | None -> ()
  | Some src ->
    begin
      Buffer.add_string buffer
        (Filepath.Normalized.to_pretty_string src.Filepath.pos_path);
      Buffer.add_string buffer ":" ;
      Buffer.add_string buffer (string_of_int src.Filepath.pos_lnum);
      Buffer.add_string buffer ": " ;
    end

let add_category buffer = function
  | None -> ()
  | Some a -> Buffer.add_char buffer ':' ; Buffer.add_string buffer a

let add_kind buffer = function
  | Result | Feedback | Debug -> ()
  | Error -> Buffer.add_string buffer "User Error: "
  | Warning -> Buffer.add_string buffer "Warning: "
  | Failure -> Buffer.add_string buffer "Failure: "

let echo_event evt terminal =
  begin
    term_clean terminal ;
    let buffer = Buffer.create 120 in
    Buffer.add_char buffer '[' ;
    Buffer.add_string buffer evt.evt_plugin ;
    add_category buffer evt.evt_category ;
    Buffer.add_string buffer "] " ;
    add_source buffer evt.evt_source ;
    add_kind buffer evt.evt_kind ;
    let prefix = Buffer.contents buffer in
    let header = String.length prefix in
    let text = evt.evt_message in
    let size = String.length text in
    let output = terminal.output in
    output prefix 0 header ;
    if header + size <= 80 && is_single_line text then
      begin
        output text 0 size ;
        echo_newline output ;
      end
    else
      begin
        let prefix = is_prefixed_event evt in
        if not prefix then echo_newline output ;
        echo_lines output ~prefix text 0 (String.length text - 1) ;
      end ;
    terminal.flush () ;
  end

let do_echo terminal evt =
  if delayed_echo terminal then
    terminal.delayed <- echo_event evt :: terminal.delayed
  else
    echo_event evt terminal

let do_transient terminal text p q =
  if p <= q && not (delayed_echo terminal) then
    begin
      term_clean terminal ;
      echo_firstline terminal.output text p q 80 ;
      if terminal.isatty
      then terminal.clean <- false
      else terminal.output "\n" 0 1 ;
      terminal.flush () ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Source                                                             --- *)
(* -------------------------------------------------------------------------- *)

let source ~file ~line =
  Filepath.{ pos_path = file ; pos_lnum = line ; pos_bol = 0 ; pos_cnum = 0 }

let current_loc = ref (fun () -> raise Not_found)

let set_current_source fpos = current_loc := fpos

let get_current_source () = !current_loc ()

let get_source current = function
  | None -> if current then Some (!current_loc ()) else None
  | Some _ as s -> s

(* -------------------------------------------------------------------------- *)
(* --- Channels                                                           --- *)
(* -------------------------------------------------------------------------- *)

type emitter = {
  mutable listeners : (event -> unit) list ;
  mutable echo : bool ;
}

type ontty = [
  | `Message   (* Normal message (default) *)
  | `Feedback  (* Temporary visible on console, normal message otherwise *)
  | `Transient (* Temporary visible, only on console *)
  | `Silent    (* Not visible on console *)
]

let tty = ref (fun () -> false)

type channel = {
  locked_buffer : Rich_text.buffer ; (* already allocated top-level buffer *)
  mutable stack : int ;   (* number of 'stacked' buffers *)
  plugin : string ;
  emitters : emitter array ;
  terminal : terminal ;
}

type channelstate =
  | NotCreatedYet of emitter array
  | Created of channel

let nth_kind = function
  | Result   -> 0
  | Feedback -> 1
  | Debug    -> 2
  | Error    -> 3
  | Warning  -> 4
  | Failure  -> 5

let all_kinds = [| Result ; Feedback ; Debug ; Error ; Warning ; Failure |]

let () = Array.iteri
    (fun i k -> assert (i == nth_kind k))
    all_kinds

(* -------------------------------------------------------------------------- *)
(* --- Channels                                                           --- *)
(* -------------------------------------------------------------------------- *)

let all_channels : (string,channelstate) Hashtbl.t = Hashtbl.create 31
let default_emitters = Array.map (fun _ -> { listeners=[] ; echo=true }) all_kinds

let new_emitters () =
  Array.map (fun e -> { listeners = e.listeners ; echo = e.echo }) default_emitters

let get_emitters plugin =
  try
    match Hashtbl.find all_channels plugin with
    | NotCreatedYet e -> e
    | Created c -> c.emitters
  with Not_found ->
    let e = new_emitters () in
    Hashtbl.replace all_channels plugin (NotCreatedYet e) ; e


let new_channel plugin =
  let create_with_emitters plugin emitters =
    let c = {
      plugin = plugin ;
      stack = 0 ;
      locked_buffer = Rich_text.create () ;
      emitters = emitters ;
      terminal = stdout ;
    } in
    Hashtbl.replace all_channels plugin (Created c) ; c
  in
  try
    match Hashtbl.find all_channels plugin with
    | Created c -> c
    | NotCreatedYet ems -> create_with_emitters plugin ems
  with Not_found ->
    let ems = new_emitters () in
    create_with_emitters plugin ems

(* -------------------------------------------------------------------------- *)
(* --- Already emitted messages                                           --- *)
(* -------------------------------------------------------------------------- *)

let check_not_yet = ref (fun _evt -> false)

(* -------------------------------------------------------------------------- *)
(* --- Listeners                                                          --- *)
(* -------------------------------------------------------------------------- *)

let do_fire e f = f e

let iter_kind ?kind f ems =
  match kind with
  | None -> Array.iter f ems
  | Some ks -> List.iter (fun k -> f ems.(nth_kind k)) ks

let iter_plugin ?plugin ?kind f =
  match plugin with
  | None ->
    Hashtbl.iter
      (fun _ s ->
         match s with
         | Created c -> iter_kind ?kind f c.emitters
         | NotCreatedYet ems -> iter_kind ?kind f ems)
      all_channels ;
    iter_kind ?kind f default_emitters
  | Some p ->
    iter_kind ?kind f (get_emitters p)

let add_listener ?plugin ?kind demon =
  iter_plugin ?plugin ?kind (fun em -> em.listeners <- em.listeners @ [demon])

let set_echo ?plugin ?kind echo =
  iter_plugin ?plugin ?kind (fun em -> em.echo <- echo)

let notify e =
  let es = get_emitters e.evt_plugin in
  List.iter (fun f -> f e) es.(nth_kind e.evt_kind).listeners

(* -------------------------------------------------------------------------- *)
(* --- Generic Log Routine                                                --- *)
(* -------------------------------------------------------------------------- *)

let open_buffer c =
  if c.stack > 0 then
    ( c.stack <- succ c.stack ; Rich_text.create () )
  else
    ( c.stack <- 1 ; c.locked_buffer )

let close_buffer c =
  if c.stack > 1 then
    c.stack <- pred c.stack
  else
    Rich_text.shrink c.locked_buffer

let logtransient channel text =
  let buffer = open_buffer channel in
  Rich_text.kprintf
    (fun fmt ->
       try
         Format.pp_print_newline fmt () ;
         Format.pp_print_flush fmt () ;
         let p,q = Rich_text.trim buffer in
         do_transient channel.terminal (Rich_text.contents buffer) p q ;
         close_buffer channel
       with e ->
         close_buffer channel ;
         raise e
    ) buffer text

let logwithfinal finally channel
    ?(fire=true)    (* fire channel listeners *)
    ?emitwith       (* additional emitter *)
    ?(once=false)   (* log and emit only once *)
    ?(echo=true)    (* echo on terminal *)
    ?(current=false) (* use current source as default *)
    ?source (* source location *)
    ?(kind=Feedback) (* message kind *)
    ?category (* message category *)
    ?append (* additional text *)
    text =
  let buffer = open_buffer channel in
  Format.pp_open_vbox (Rich_text.formatter buffer) 0 ;
  Rich_text.kprintf
    (fun fmt ->
       try
         (match append with None -> () | Some k -> k fmt) ;
         Format.pp_close_box fmt () ;
         Format.pp_print_newline fmt () ;
         Format.pp_print_flush fmt () ;
         let p,q = Rich_text.trim buffer in
         let output = 
           if p <= q then
             let source = get_source current source in
             let message = Rich_text.range buffer p q in
             let event = {
               evt_kind = kind ;
               evt_plugin = channel.plugin ;
               evt_category = category ;
               evt_message = message ;
               evt_source = source ;
             } in
             if not once || !check_not_yet event then
               begin
                 let e = channel.emitters.(nth_kind kind) in
                 if echo && e.echo then
                   do_echo channel.terminal event ;
                 Extlib.may (do_fire event) emitwith;
                 if fire then List.iter (do_fire event) e.listeners ;
                 Some event
               end
             else None
           else None
         in
         close_buffer channel ;
         finally output
       with e ->
         close_buffer channel ;
         raise e
    ) buffer text

let finally_unit _ = ()
let finally_raise e _ = raise e
let finally_false _ = false

let cmdline_error_occurred = Extlib.mk_fun "Log.cmdline_error_occurred"
let cmdline_at_error_exit = Extlib.mk_fun "Log.at_error_exit"

type deferred_exn =
  | DNo_exn
  | DWarn_as_error of event
  | DError of event
  | DFatal of event

let deferred_exn = ref DNo_exn

let unreported_error = "##unreported-error##"

let unreported_event { evt_category } =
  match evt_category with
  | None -> false
  | Some s -> s = unreported_error

(* we keep track of at most one deferred exception, ordered by seriousness
   (internal error > user error > warning-as-error). the rationale is that
   an internal error might cause subsequent errors or warning, but the reverse
   is not true: an deferred user error must not lead to an internal error.
   Should that ever happen, at the very least the code should be modified to
   directly [abort] instead of merely logging an [error].
*)
let update_deferred_exn exn =
  match !deferred_exn, exn with
  | DNo_exn, _ -> deferred_exn := exn
  | DWarn_as_error _, DWarn_as_error _ -> ()
  | DWarn_as_error _, _ -> deferred_exn := exn
  | DError _, (DNo_exn | DWarn_as_error _ | DError _) -> ()
  | DError _, DFatal _ -> deferred_exn := exn
  | DFatal _, _ -> ()

let warn_event_as_error event = update_deferred_exn (DWarn_as_error event)

let deferred_raise ~fatal ~unreported event msg =
  let channel = new_channel event.evt_plugin in
  let append =
    if unreported then None else
      Some
        (fun fmt ->
           Format.fprintf fmt " See above messages for more information.@\n")
  in
  let exn =
    if fatal then AbortFatal event.evt_plugin
    else AbortError event.evt_plugin
  in
  let finally = finally_raise exn in
  logwithfinal finally channel ?append ~kind:event.evt_kind msg

let treat_deferred_error () =
    match !deferred_exn with
    | DNo_exn -> ()
    | DWarn_as_error event ->
      let unreported = unreported_event event in
      let wkey =
        match event.evt_category with
        | None -> ""
        | Some s when s = unreported_error -> ""
        | Some s -> s
      in
      deferred_raise ~fatal:false ~unreported event
        "warning %s treated as deferred error." wkey
    | DError event ->
      let unreported = unreported_event event in
      deferred_raise ~fatal:false ~unreported event
        "Deferred error message was emitted during execution."
    | DFatal event ->
      let unreported = unreported_event event in
      deferred_raise ~fatal:true ~unreported event
        "Deferred internal error message was emitted during execution."

(* -------------------------------------------------------------------------- *)
(* --- Messages Interface                                                 --- *)
(* -------------------------------------------------------------------------- *)

type 'a pretty_printer =
  ?current:bool -> ?source:Filepath.position ->
  ?emitwith:(event -> unit) -> ?echo:bool -> ?once:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,formatter,unit) format -> 'a

type ('a,'b) pretty_aborter =
  ?current:bool -> ?source:Filepath.position -> ?echo:bool ->
  ?append:(Format.formatter -> unit) ->
  ('a,formatter,unit,'b) format4 -> 'a

let log_channel channel
    ?(kind=Result)
    ?current ?source
    ?emitwith ?echo ?once
    ?append
    text =
  logwithfinal finally_unit channel ?once ?echo ?emitwith ?current ?source
    ~kind ?append text

let echo e =
  try
    match Hashtbl.find all_channels e.evt_plugin with
    | NotCreatedYet _ -> raise Not_found
    | Created c -> do_echo c.terminal e
  with Not_found ->
    let msg =
      Format.sprintf "[unknown channel %s]:%s"
        e.evt_plugin e.evt_message
    in failwith msg

(* ------------------------------------------------------------------------- *)
(* --- Plug-in Interface                                                 --- *)
(* ------------------------------------------------------------------------- *)

module Category_trie =
struct
  (* No Datatype at this level for dependencies reasons *)
  module String_map = Map.Make(String)

  type 'a t =
    | Node of 'a option * 'a t String_map.t

  let empty = Node (None, String_map.empty)

  let rec add_structure l t =
    match l with
    | [] -> t
    | x :: l ->
      let Node (info, map) = t in
      let binding =
        try String_map.find x map
        with Not_found -> Node (info, String_map.empty)
      in
      let res = add_structure l binding in
      Node (info, String_map.add x res map)

  let rec add_info l ?merge info (Node (old_info, map)) =
    match l with
    | [] ->
      let rec aux map =
        String_map.map
          (function Node(old_info, map) ->
             let new_info =
               match old_info, merge with
               | None, _ | _, None -> Some info
               | Some old_info, Some merge -> Some (merge old_info info)
             in
             Node (new_info, aux map)) map
      in
      Node (Some info, aux map)
    | x :: l ->
      let binding = String_map.find x map in
      let res = add_info l info binding in
      Node (old_info, String_map.add x res map)

  let rec get l (Node(info, map)) =
    match l with
    | [] -> info
    | x :: l ->
      let binding = String_map.find x map in
      get l binding

  let fold f map acc =
    let rec aux suf (Node(info, map)) acc =
      let acc = f (List.rev suf) info acc in
      String_map.fold (fun s t acc -> aux (s::suf) t acc) map acc
    in aux [] map acc

  let suffixes l trie =
    let rec aux res suf l (Node(_,map)) =
      match l with
      | [] ->
        let res = (List.rev suf) :: res in
        String_map.fold (fun s t res -> aux res (s::suf) [] t) map res
      | x::l ->
        let t = String_map.find x map in
        aux res (x::suf) l t
    in
    (* Provide results in lexicographic order. *)
    List.rev (aux [] [] l trie)
end

let rec split_joker = function
  | [] -> []
  | ["*"] -> []
  | ""::w -> split_joker w
  | a::w -> a::split_joker w

let split_category s = split_joker (String.split_on_char ':' s)

let evt_category = function
  | { evt_category = None } -> []
  | { evt_category = Some s } -> split_category s

(* a is a sub-category of b *)
let rec is_subcategory a b = match a,b with
  | _,[] -> true
  | [],_ -> false
  | a1::aw , b1::bw -> a1 = b1 && is_subcategory aw bw

let merge_category l =
  match l with
  | [] -> "*"
  | [ s ] -> s
  | hd :: tl ->
    let b = Buffer.create 15 in
    Buffer.add_string b hd;
    List.iter (fun s -> Buffer.add_char b ':'; Buffer.add_string b s) tl;
    Buffer.contents b

type warn_status =
  | Winactive
  | Wfeedback_once
  | Wfeedback
  | Wonce
  | Wactive
  | Werror_once
  | Werror
  | Wabort

let pp_warn_status fmt s =
  let s =
    match s with
    | Winactive -> "inactive"
    | Wfeedback_once -> "feedback,once"
    | Wfeedback -> "feedback"
    | Wonce -> "once"
    | Wactive ->   "active"
    | Werror_once -> "error,once"
    | Werror -> "error"
    | Wabort -> "abort"
  in
  Format.pp_print_string fmt s

let merge_status old_status new_status =
  match old_status, new_status with
  | Winactive, Wactive -> Wactive
  | Winactive, Wonce -> Wonce
  | Winactive, _ -> Winactive
  | _ -> new_status

module type Messages =
sig

  type category

  type warn_category

  val verbose_atleast: int -> bool
  val debug_atleast: int -> bool

  val printf : ?level:int -> ?dkey:category ->
    ?current:bool -> ?source:Filepath.position ->
    ?append:(Format.formatter -> unit) ->
    ?header:(Format.formatter -> unit) ->
    ('a,formatter,unit) format -> 'a

  val result  : ?level:int -> ?dkey:category -> 'a pretty_printer
  val feedback: ?ontty:ontty -> ?level:int -> ?dkey:category -> 'a pretty_printer
  val debug   : ?level:int -> ?dkey:category -> 'a pretty_printer
  val warning : ?wkey: warn_category -> 'a pretty_printer
  val error   : 'a pretty_printer
  val abort   : ('a,'b) pretty_aborter
  val failure : 'a pretty_printer
  val fatal   : ('a,'b) pretty_aborter
  val verify  : bool -> ('a,bool) pretty_aborter

  val not_yet_implemented : ('a,formatter,unit,'b) format4 -> 'a
  val deprecated : string -> now:string -> ('a -> 'b) -> 'a -> 'b

  val with_result  : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_warning : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_error   : (event option -> 'b) -> ('a,'b) pretty_aborter
  val with_failure : (event option -> 'b) -> ('a,'b) pretty_aborter

  val log : ?kind:kind -> ?verbose:int -> ?debug:int -> 'a pretty_printer
  val logwith : (event option -> 'b) ->
    ?wkey: warn_category -> ?emitwith:(event -> unit) -> ?once:bool -> ('a,'b) pretty_aborter

  val register : kind -> (event -> unit) -> unit (** Very local listener. *)
  val register_tag_handlers : (string -> string) * (string -> string) -> unit

  val register_category: string -> category

  val pp_category: Format.formatter -> category -> unit

  val dkey_name: category -> string

  val is_registered_category: string -> bool

  val get_category: string -> category option
  val get_all_categories: unit -> category list

  val add_debug_keys: category -> unit
  val del_debug_keys: category -> unit
  val get_debug_keys: unit -> category list

  val is_debug_key_enabled: category -> bool

  val get_debug_keyset : unit -> category list

  val register_warn_category: string -> warn_category

  val is_warn_category: string -> bool

  val pp_warn_category: Format.formatter -> warn_category -> unit

  val pp_all_warn_categories_status: unit -> unit

  val wkey_name: warn_category -> string

  val get_warn_category: string -> warn_category option

  val get_all_warn_categories: unit -> warn_category list

  val get_all_warn_categories_status: unit -> (warn_category * warn_status) list

  val set_warn_status: warn_category -> warn_status -> unit

  val get_warn_status: warn_category -> warn_status

end

module Register
    (P : sig
       val channel : string
       val label : string
       val verbose_atleast : int -> bool
       val debug_atleast : int -> bool
     end) =
struct

  include P

  type category = string

  type warn_category = string

  let categories = ref Category_trie.empty

  let register_category (s:string) =
    let res: category = s in
    let l = split_category s in
    categories := Category_trie.add_structure l !categories;
    res

  let pp_category fmt (cat: category) = Format.pp_print_string fmt cat

  let get_all_categories () =
    List.map merge_category (Category_trie.suffixes [] !categories)

  let is_registered_category s =
    List.mem (split_category s) (Category_trie.suffixes [] !categories)

  let get_category s =
    if is_registered_category s then Some s else None

  let not_registered s =
    failwith (s ^ " is not a registered category for " ^ label)

  let dkey_name s = s

  let wkey_name s = s

  let add_debug_keys s =
    try
      categories := Category_trie.add_info (split_category s) true !categories
    with Not_found -> not_registered s

  let del_debug_keys s =
    try
      categories := Category_trie.add_info (split_category s) false !categories
    with Not_found -> not_registered s

  let get_debug_keys () =
    let f cat info acc =
      match info with
      | None | Some false -> acc
      | Some true -> (merge_category cat) :: acc
    in
    Category_trie.fold f !categories []

  let is_debug_key_enabled (c:category) =
    let s = (c:>string) in
    match Category_trie.get (split_category s) !categories with
    | None -> false
    | Some flag -> flag
    | exception Not_found -> not_registered s

  let has_debug_key = function
    | None -> true (* No key means to be displayed each time *)
    | Some c -> is_debug_key_enabled c

  let warn_categories = ref Category_trie.empty

  let register_warn_category s =
    warn_categories :=
      Category_trie.add_structure (split_category s) !warn_categories;
    s

  let get_all_warn_categories () =
    List.map merge_category (Category_trie.suffixes [] !warn_categories)

  let get_all_warn_categories_status () =
    List.rev
      (Category_trie.fold
         (fun cat status l  ->
            (merge_category cat, Extlib.opt_conv Wactive status) :: l)
         !warn_categories [])

  let is_warn_category s =
    List.mem (split_category s) (Category_trie.suffixes [] !warn_categories)

  let pp_warn_category fmt s = Format.pp_print_string fmt s

  let get_warn_category s = if is_warn_category s then Some s else None

  let wnot_registered s =
    failwith (s ^ " is not a registered warning category for " ^ label)

  let set_warn_status s status =
    try
      warn_categories :=
        Category_trie.add_info
          (split_category s) ~merge:merge_status status !warn_categories
    with Not_found -> wnot_registered s

  let get_warn_status s =
    match Category_trie.get (split_category s) !warn_categories with
    | Some s -> s
    | None -> Wactive
    | exception Not_found -> wnot_registered s

  let channel = new_channel P.channel

  let internal_register_tag_handlers _c (_ope,_close) = ()
  (* BM->LOIC: I need to keep this code around to be able to handle
     marks and tags correctly.
     Do you think we can emulate all other features of Log but without
     using c.buffer at all?
     Everything but ensure_unique_newline seems feasible.
     See Design.make_slash to see a useful example.

     let start_of_line= Printf.sprintf "\n[%s] " P.label in
     let length= pred (String.length start_of_line) in
     Format.pp_set_all_formatter_output_functions c.formatter
     ~out:c.term.output
     ~flush:c.term.flush
     ~newline:(fun () -> c.term.output start_of_line 0 length)
     ~spaces:(fun _ ->  ()(*TODO:correct margin*))
     ;
     Format.pp_set_tags c.formatter true;
     Format.pp_set_mark_tags c.formatter true;
     Format.pp_set_print_tags c.formatter false;
     Format.pp_set_formatter_tag_functions c.formatter
     {(Format.pp_get_formatter_tag_functions c.formatter ())
     with
     Format.mark_open_tag = ope;
     mark_close_tag = close}
  *)

  let register_tag_handlers h =
    internal_register_tag_handlers channel h

  let to_be_log verbose debug =
    match verbose , debug with
    | 0 , 0 -> verbose_atleast 1
    | v , 0 -> verbose_atleast v
    | 0 , d -> debug_atleast d
    | v , d -> verbose_atleast v || debug_atleast d

  let log ?(kind=Result)
      ?(verbose=0) ?(debug=0)
      ?current ?source
      ?emitwith ?echo ?once
      ?append text =
    if to_be_log verbose debug then
      logwithfinal finally_unit channel ?once ?echo ?emitwith ?current ?source
        ~kind
        ?append text
    else Pretty_utils.nullprintf text

  let result
      ?(level=1) ?dkey ?current ?source
      ?emitwith ?echo ?once ?append text =
    if verbose_atleast level && has_debug_key dkey then
      logwithfinal finally_unit channel ?once ?echo ?emitwith ?current ?source
        ~kind:Result ?category:dkey ?append text
    else Pretty_utils.nullprintf text

  let transient channel = channel.terminal.isatty && !tty ()

  let feedback
      ?(ontty=`Message)
      ?(level=1) ?dkey ?current ?source
      ?emitwith ?echo ?once ?append text =
    let mode =
      if verbose_atleast level && has_debug_key dkey
      then
        match ontty with
        | `Feedback -> if transient channel then `Transient else `Message
        | `Transient -> if transient channel then `Transient else `Silent
        | `Silent -> if transient channel then `Silent else `Message
        | `Message -> `Message
      else `Silent
    in match mode with
    | `Message ->
      logwithfinal finally_unit channel ?once ?echo ?emitwith ?current ?source
        ~kind:Feedback ?category:dkey ?append text
    | `Transient -> logtransient channel text
    | `Silent -> Pretty_utils.nullprintf text

  let should_output_debug level dkey =
    match level, dkey with
    | None, None -> debug_atleast 1
    | Some l, None -> debug_atleast l
    | None, Some _ -> has_debug_key dkey
    | Some l, Some _ -> debug_atleast l && has_debug_key dkey

  let debug
      ?level ?dkey ?current ?source
      ?emitwith ?echo ?once ?append text =
    if should_output_debug level dkey then
      logwithfinal finally_unit channel ?once ?echo ?emitwith ?current ?source
        ~kind:Debug ?category:dkey ?append text
    else
      Pretty_utils.nullprintf text

  let force_error = function
    | None ->
      { evt_kind = Failure;
        evt_plugin = channel.plugin;
        evt_category = Some unreported_error;
        evt_message = "Silent error";
        evt_source = None
      }
    | Some evt -> evt

  let finally_user_error evt =
    let evt = force_error evt in update_deferred_exn (DError evt)

  let finally_internal_error evt =
    let evt = force_error evt in update_deferred_exn (DFatal evt)

  let error
      ?current ?source
      ?emitwith ?echo ?once ?append text =
    logwithfinal
      finally_user_error channel ?once ?echo ?emitwith ?current ?source
      ~kind:Error ?append text

  let abort ?current ?source ?echo ?append text =
    logwithfinal (finally_raise (AbortError P.channel))
      channel ?echo ?current ?source
      ~kind:Error ?append text

  let failure
      ?current ?source
      ?emitwith ?echo ?once ?append text =
    logwithfinal finally_internal_error channel
      ?once ?echo ?emitwith ?current ?source ~kind:Failure ?append text

  let fatal
      ?current ?source
      ?echo ?append text =
    logwithfinal (finally_raise (AbortFatal P.channel)) channel
      ?echo ?current ?source
      ~kind:Failure ?append text

  let verify assertion
      ?current ?source
      ?echo ?append text =
    if assertion then
      Format.kfprintf (fun _ -> true) Pretty_utils.null text
    else
      logwithfinal finally_false channel ?echo ?current ?source
        ~kind:Failure ?append text

  let logwith
      finally
      ?(wkey="") ?emitwith ?once
      ?current ?source
      ?echo ?append text =
    let status = get_warn_status wkey in
    if status <> Winactive then
      begin
        let action, once_suffix =
          match status with
          | Wabort ->
            Some (fun _ -> abort "warning %s treated as fatal error." wkey), ""
          | Werror -> Some warn_event_as_error, ""
          | Werror_once ->
            Some
              (fun evt ->
                 warn_event_as_error evt; set_warn_status wkey Winactive),
            "warn-error-once"
          | Wfeedback_once ->
            Some (fun _ -> set_warn_status wkey Winactive), "warn-feedback-once"
          | Wonce ->
            Some (fun _ -> set_warn_status wkey Winactive), "warn-once"
          | Wactive | Winactive | Wfeedback -> None, ""
        in
        let emitwith =
          match emitwith, action with
          | None, None -> None
          | Some e, None | None, Some e -> Some e
          | Some e1, Some e2 -> Some (fun evt -> e1 evt; e2 evt)
        in
        let kind =
          match status with
          | Wfeedback | Wfeedback_once -> Feedback
          | (Wactive | Werror | Wabort | Wonce | Werror_once | Winactive) ->
            Warning
        in
        let category = if wkey = "" then None else Some wkey in
        let append_once_suffix = (fun fmt ->
            Format.fprintf fmt
              "@.(%s: no further messages from category '%s' will be emitted)"
              once_suffix wkey)
        in
        let append = if once_suffix = "" then append
          else match append with
            | None -> Some append_once_suffix
            | Some app ->
              Some (fun fmt -> app fmt; append_once_suffix fmt)
        in
        logwithfinal finally channel ?once ?echo ?emitwith ?current ?source
          ~kind ?category ?append text
      end
    else Pretty_utils.with_null (fun () -> finally None) text

  let warning
      ?wkey ?current ?source
      ?emitwith ?echo ?once ?append text =
    logwith finally_unit
      ?wkey ?current ?source ?emitwith ?echo ?once ?append text

  let with_result finally ?current ?source ?echo ?append text =
    logwithfinal finally channel
      ~kind:Result ?current ?source ?echo ?append text

  let with_warning finally ?current ?source ?echo ?append text =
    logwithfinal finally channel
      ~kind:Warning ?current ?source ?echo ?append text

  let with_error finally ?current ?source ?echo ?append text =
    logwithfinal finally channel
      ~kind:Error ?current ?source ?echo ?append text

  let with_failure finally ?current ?source ?echo ?append text =
    logwithfinal finally channel
      ~kind:Failure ?current ?source ?echo ?append text

  let register kd f =
    let em = channel.emitters.(nth_kind kd) in
    em.listeners <- em.listeners @ [f]

  let not_yet_implemented text =
    let buffer = Buffer.create 80 in
    let finally fmt =
      Format.pp_print_flush fmt ();
      let msg = Buffer.contents buffer in
      raise (FeatureRequest(channel.plugin,msg)) in
    let fmt = Format.formatter_of_buffer buffer in
    Format.kfprintf finally fmt text

  let deprecated name ~now f x =
    warning ~once:true
      "call to deprecated function '%s'.\nShould use '%s' instead."
      name now ;
    f x

  let get_debug_keyset =
    deprecated "Log.get_debug_key_set"
      ~now:"Log.get_all_categories"
      (fun () -> get_all_categories ())

  let noprint _fmt = ()

  let spynewline bol output buffer start length =
    begin
      let ofs = start+length-1 in
      if 0 <= ofs && ofs < String.length buffer then
        bol := buffer.[ofs] = '\n' ;
      output buffer start length
    end

  let printf ?(level=1) ?dkey ?current ?source ?(append=noprint)
      ?header text =
    if verbose_atleast level && has_debug_key dkey then
      begin
        (* Header is a regular message *)
        let header = match header with None -> noprint | Some h -> h in
        logwithfinal finally_unit channel ~kind:Result ~fire:false ?current ?source
          ?category:dkey "%t" header ;
        let bol = ref true in
        let stdout = { stdout with output = spynewline bol stdout.output } in
        let fmt = delayed_terminal stdout in
        try
          Format.kfprintf
            begin fun fmt ->
              append fmt ;
              Format.pp_print_flush fmt () ;
              unlock_terminal stdout fmt ;
              if not !bol then Format.pp_print_newline fmt () ;
            end
            fmt text
        with error ->
          unlock_terminal stdout fmt ; raise error
      end
    else 
      Pretty_utils.nullprintf text

  let pp_all_warn_categories_status () =
    let l = get_all_warn_categories_status () in
    let max =
      List.fold_left (fun m (s,_) -> max m (String.length s)) 0 l
    in
    let print_one_elt fmt (cat, status) =
      Format.fprintf fmt "%-*s : %a" max cat pp_warn_status status
    in
    feedback "@[<v 2>Warning categories for %s are@;%a@]"
      label Format.(pp_print_list ~pp_sep:pp_print_cut print_one_elt) l

end

(* Deprecated -- backward compatibity only. *)
let null = Pretty_utils.null
let with_null = Pretty_utils.with_null
let nullprintf = Pretty_utils.nullprintf

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
