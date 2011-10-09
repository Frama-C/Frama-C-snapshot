(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

open Cil_types

(** Utility functions for Coolaid *)
module H = Hashtbl
module IH = Inthash

open Cil_datatype

let ($) f g x = f (g x)

let swap f x y = f y x

let hash_copy_into (hfrom: ('a, 'b) H.t) (hto: ('a, 'b) H.t) : unit =
  H.clear hto;
  H.iter (H.add hto) hfrom


let rec list_drop (n : int) (xs : 'a list) : 'a list =
  if n < 0 then invalid_arg "Util.list_drop";
  if n = 0 then
    xs
  else begin
    match xs with
    | [] -> invalid_arg "Util.list_drop"
    | _y::ys -> list_drop (n-1) ys
  end

let list_droptail (n : int) (xs : 'a list) : 'a list =
  if n < 0 then invalid_arg "Util.list_droptail";
  let (ndrop,r) =
    List.fold_right
      (fun x (ndrop,acc) ->
	if ndrop = 0 then (ndrop, x :: acc)
	else (ndrop-1, acc))
      xs
      (n,[])
  in
  if ndrop > 0 then invalid_arg "Util.listdroptail"
  else r

let rec list_span (p : 'a -> bool) (xs : 'a list) : 'a list * 'a list =
  begin match xs with
  | [] -> ([],[])
  | x::xs' ->
      if p x then
        let (ys,zs) = list_span p xs' in (x::ys,zs)
      else ([],xs)
  end

let rec list_rev_append revxs ys =
  begin match revxs with
  | [] -> ys
  | x::xs -> list_rev_append xs (x::ys)
  end

let list_insert_by (cmp : 'a -> 'a -> int)
    (x : 'a) (xs : 'a list) : 'a list =
  let rec helper revhs ts =
    begin match ts with
    | [] -> List.rev (x::revhs)
    | t::ts' ->
        if cmp x t >= 0 then helper (t::revhs) ts'
        else list_rev_append (x::revhs) ts
    end
  in
  helper [] xs

let list_head_default (d : 'a) (xs : 'a list) : 'a =
  begin match xs with
  | [] -> d
  | x::_ -> x
  end

let rec list_iter3 f xs ys zs =
  begin match xs, ys, zs with
  | [], [], [] -> ()
  | x::xs, y::ys, z::zs -> f x y z; list_iter3 f xs ys zs
  | _ -> invalid_arg "Util.list_iter3"
  end

let rec list_last = function
  | [] -> invalid_arg "Cilutil.list_last"
  | [ a ] -> a
  | _ :: tl -> list_last tl

let rec get_some_option_list (xs : 'a option list) : 'a list =
  begin match xs with
  | [] -> []
  | None::xs  -> get_some_option_list xs
  | Some x::xs -> x :: get_some_option_list xs
  end

(* tail-recursive append: reverses xs twice *)
let list_append (xs: 'a list) (ys: 'a list): 'a list =
  match xs with (* optimize some common cases *)
      [] -> ys
    | [x] -> x::ys
    | _ -> list_rev_append (List.rev xs) ys

let list_iteri (f: int -> 'a -> unit) (l: 'a list) : unit =
  let rec loop (i: int) (l: 'a list) : unit =
    match l with
      [] -> ()
    | h :: t -> f i h; loop (i + 1) t
  in
  loop 0 l

let list_mapi (f: int -> 'a -> 'b) (l: 'a list) : 'b list =
  let rec loop (i: int) (l: 'a list) : 'b list =
    match l with
      [] -> []
    | h :: t ->
	let headres = f i h in
	headres :: loop (i + 1) t
  in
  loop 0 l

let list_fold_lefti (f: 'acc -> int -> 'a -> 'acc) (start: 'acc)
                   (l: 'a list) : 'acc =
  let rec loop (i, acc) l =
    match l with
      [] -> acc
    | h :: t -> loop (i + 1, f acc i h) t
  in
  loop (0, start) l

let list_init (len : int) (init_fun : int -> 'a) : 'a list =
  let rec loop n acc =
    if n < 0 then acc
    else loop (n-1) ((init_fun n)::acc)
  in
  loop (len - 1) []

let rec list_find_first (l: 'a list) (f: 'a -> 'b option) : 'b option =
  match l with
    [] -> None
  | h :: t -> begin
      match f h with
        None -> list_find_first t f
      | r -> r
  end

(** Generates the range of integers starting with a and ending with b *)
let int_range_list (a: int) (b: int) =
  list_init (b - a + 1) (fun i -> a + i)


(** Some handling of registers *)
type 'a growArrayFill =
    Elem of 'a
  | Susp of (int -> 'a)

type 'a growArray = {
            gaFill: 'a growArrayFill;
            (** Stuff to use to fill in the array as it grows *)

    mutable gaMaxInitIndex: int;
            (** Maximum index that was written to. -1 if no writes have
             * been made.  *)

    mutable gaData: 'a array;
  }

let growTheArray (ga: 'a growArray) (len: int)
                 (toidx: int) (_why: string) : unit =
  if toidx >= len then begin
    (* Grow the array by 50% *)
    let newlen = toidx + 1 + len  / 2 in
(*
    ignore (E.log "growing an array to idx=%d (%s)\n" toidx why);
*)
    let data' = begin match ga.gaFill with
      Elem x ->

	let data'' = Array.create newlen x in
	Array.blit ga.gaData 0 data'' 0 len;
	data''
    | Susp f -> Array.init newlen
	  (fun i -> if i < len then ga.gaData.(i) else f i)
    end
    in
    ga.gaData <- data'
  end

let getReg (ga: 'a growArray) (r: int) : 'a =
  let len = Array.length ga.gaData in
  if r >= len then
    growTheArray ga len r "get";

  ga.gaData.(r)

let setReg (ga: 'a growArray) (r: int) (what: 'a) : unit =
  let len = Array.length ga.gaData in
  if r >= len then
    growTheArray ga len r "set";
  if r > ga.gaMaxInitIndex then ga.gaMaxInitIndex <- r;
  ga.gaData.(r) <- what

let newGrowArray (initsz: int) (fill: 'a growArrayFill) : 'a growArray =
  { gaFill = fill;
    gaMaxInitIndex = -1;
    gaData = begin match fill with
      Elem x -> Array.create initsz x
    | Susp f -> Array.init initsz f
    end; }

let copyGrowArray (ga: 'a growArray) : 'a growArray =
  { ga with gaData = Array.copy ga.gaData }

let deepCopyGrowArray (ga: 'a growArray) (copy: 'a -> 'a): 'a growArray =
  { ga with gaData = Array.map copy ga.gaData }



(** Iterate over the initialized elements of the array *)
let growArray_iteri  (f: int -> 'a -> unit) (ga: 'a growArray) =
  for i = 0 to ga.gaMaxInitIndex do
    f i ga.gaData.(i)
  done


(** Fold left over the initialized elements of the array *)
let growArray_foldl (f: 'acc -> 'a -> 'acc)
                    (acc: 'acc) (ga: 'a growArray) : 'acc =
  let rec loop (acc: 'acc) (idx: int) : 'acc =
    if idx > ga.gaMaxInitIndex then
      acc
    else
      loop (f acc ga.gaData.(idx)) (idx + 1)
  in
  loop acc 0




let hasPrefix (prefix: string) (what: string) : bool =
  let pl = String.length prefix in
  try String.sub what 0 pl = prefix
  with Invalid_argument _ -> false



let restoreRef ?(deepCopy=(fun x -> x)) (r: 'a ref) : (unit -> unit) =
  let old = deepCopy !r in
  (fun () -> r := old)

let restoreHash ?deepCopy (h: ('a, 'b) H.t) : (unit -> unit) =
  let old =
    match deepCopy with
      None -> H.copy h
    | Some f ->
        let old = H.create (H.length h) in
        H.iter (fun k d -> H.add old k (f d)) h;
        old
  in
  (fun () -> hash_copy_into old h)

let restoreIntHash ?deepCopy (h: 'a IH.t) : (unit -> unit) =
  let old =
    match deepCopy with
      None -> IH.copy h
    | Some f ->
        let old = IH.create 13 in
        IH.iter (fun k d -> IH.add old k (f d)) h;
        old
  in
  (fun () ->
    IH.clear old;
    IH.iter (fun i k -> IH.add old i k) h)

let restoreArray ?deepCopy (a: 'a array) : (unit -> unit) =
  let old = Array.copy a in
  (match deepCopy with
    None -> ()
  | Some f -> Array.iteri (fun i v -> old.(i) <- f v) old);
  (fun () -> Array.blit old 0 a 0 (Array.length a))

let runThunks (l: (unit -> unit) list) : (unit -> unit) =
  fun () -> List.iter (fun f -> f ()) l



(* Memoize *)
let memoize (h: ('a, 'b) Hashtbl.t)
            (arg: 'a)
            (f: 'a -> 'b) : 'b =
  try
    Hashtbl.find h arg
  with Not_found -> begin
    let res = f arg in
    Hashtbl.add h arg res;
    res
  end

(* Just another name for memoize *)
let findOrAdd h arg f = memoize h arg f

(* A tryFinally function *)
let tryFinally
    (main: 'a -> 'b) (* The function to run *)
    (final: 'b option -> unit)  (* Something to run at the end *)
    (arg: 'a) : 'b =
  try
    let res: 'b = main arg in
    final (Some res);
    res
  with e -> begin
    final None;
    raise e
  end




let valOf : 'a option -> 'a = function
    None -> raise (Failure "Util.valOf")
  | Some x -> x

let opt_bind f = function None -> None | Some x -> f x

let opt_app f default = function None -> default | Some x -> f x

let opt_iter f = function None -> () | Some x -> f x

(**
 * An accumulating for loop.
 *
 * Initialize the accumulator with init.  The current index and accumulator
 * from the previous iteration is passed to f.
 *)
let fold_for ~(init: 'a) ~(lo: int) ~(hi: int) (f: int -> 'a -> 'a) =
  let rec forloop i acc =
    if i > hi then acc
    else forloop (i+1) (f i acc)
  in
  forloop lo init

(************************************************************************)

let absoluteFilename (fname: string) =
  if Filename.is_relative fname then
    Filename.concat (Sys.getcwd ()) fname
  else
    fname


(* mapNoCopy is like map but avoid copying the list if the function does not
 * change the elements. *)
let rec mapNoCopy (f: 'a -> 'a) = function
    [] -> []
  | (i :: resti) as li ->
      let i' = f i in
      let resti' = mapNoCopy f resti in
      if i' != i || resti' != resti then i' :: resti' else li

let rec mapNoCopyList (f: 'a -> 'a list) = function
    [] -> []
  | (i :: resti) as li ->
      let il' = f i in
      let resti' = mapNoCopyList f resti in
      match il' with
        [i'] when i' == i && resti' == resti -> li
      | _ -> il' @ resti'


(* Use a filter function that does not rewrite the list unless necessary *)
let rec filterNoCopy (f: 'a -> bool) (l: 'a list) : 'a list =
  match l with
    [] -> []
  | h :: rest when not (f h) -> filterNoCopy f rest
  | h :: rest ->
      let rest' = filterNoCopy f rest in
      if rest == rest' then l else h :: rest'

(** Join a list of strings *)
let rec joinStrings (sep: string) (sl: string list) =
  match sl with
    [] -> ""
  | [s1] -> s1
  | s1 :: ((_ :: _) as rest) -> s1 ^ sep ^ joinStrings sep rest


(************************************************************************

 Configuration

 ************************************************************************)
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



(*********************************************************************)
type symbol = int

(**{ Registering symbol names} *)
let registeredSymbolNames: (string, symbol) H.t = H.create 113
let symbolNames: string IH.t = IH.create 113
let nextSymbolId = ref 0

(* When we register symbol ranges, we store a naming function for use later
 * when we print the symbol *)
let symbolRangeNaming: (int * int * (int -> string)) list ref = ref []

(* Reset the symbols. We want to allow the registration of symbols at the
 * top-level. This means that we cannot simply clear the hash tables. The
 * first time we call "reset" we actually remember the state. *)
let resetThunk: (unit -> unit) option ref = ref None

let snapshotSymbols () : unit -> unit =
  runThunks [ restoreIntHash symbolNames;
              restoreRef nextSymbolId;
              restoreHash registeredSymbolNames;
              restoreRef symbolRangeNaming ]

let resetSymbols () =
  match !resetThunk with
    None -> resetThunk := Some (snapshotSymbols ())
  | Some t -> t ()


let dumpSymbols () =
  begin
    let pp_map fmt map =
      IH.iter (fun i k -> Format.fprintf fmt " %s -> %d\n" k i) map
    in
    Kernel.result "Current symbols\n%a" pp_map symbolNames ;
  end

let newSymbol (n: string) : symbol =
  assert(not (H.mem registeredSymbolNames n));
  let id = !nextSymbolId in
  incr nextSymbolId;
  H.add registeredSymbolNames n id;
  IH.add symbolNames id n;
  id

let registerSymbolName (n: string) : symbol =
  try H.find registeredSymbolNames n
  with Not_found -> begin
    newSymbol n
  end

(** Register a range of symbols. The mkname function will be invoked for
 * indices starting at 0 *)
let registerSymbolRange (count: int) (mkname: int -> string) : symbol =
  if count < 0 then Kernel.fatal "registerSymbolRange: invalid counter";
  let first = !nextSymbolId in
  nextSymbolId := !nextSymbolId + count;
  symbolRangeNaming :=
    (first, !nextSymbolId - 1, mkname) :: !symbolRangeNaming;
  first

let symbolName (id: symbol) : string =
  try IH.find symbolNames id
  with Not_found ->
    (* Perhaps it is one of the lazily named symbols *)
    try
      let (fst, _, mkname) =
        List.find
          (fun (fst,lst,_) -> fst <= id && id <= lst)
          !symbolRangeNaming in
      let n = mkname (id - fst) in
      IH.add symbolNames id n;
      n
    with Not_found ->
      Kernel.warning "Cannot find the name of symbol %d" id;
      "symbol" ^ string_of_int id

(*********************************************************************)

let equals x1 x2 : bool = compare x1 x2 = 0

module GenericMapl (FX: Map.OrderedType) =
struct
  include Map.Make(FX)
  type 'a map = 'a list t
  let map f m =
    fold (fun x l m -> add x (f l) m) m empty
  let add x v m =
    try
      let l = find x m in add x (v::l) m
    with Not_found ->
      add x [v] m
  let find x m =
    try find x m with Not_found -> []
end

module type Mapl=
sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    type 'a map = 'a list t

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val add: key -> 'a -> 'a list t -> 'a list t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val find: key -> 'a list t -> 'a list
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys.
       Only current bindings are presented to [f]:
       bindings hidden by more recent bindings are not passed to [f]. *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

end

module Mapl_Make (X:Map.OrderedType) = GenericMapl(X)

module IntMapl = Mapl_Make(struct type t = int let compare = Datatype.Int.compare end)

let printStages = ref false

(* pretty-printing *)

open Format

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let print_if test fmt print = if test then print fmt ()

let comma fmt () = fprintf fmt ",@ "
let underscore fmt () = fprintf fmt "_"
let semi fmt () = fprintf fmt ";@ "
let space fmt () = fprintf fmt "@ "
let alt fmt () = fprintf fmt "|@ "
let newline fmt () = fprintf fmt "@\n"
let arrow fmt () = fprintf fmt "@ -> "
let nothing _fmt () = ()
let string fmt s = fprintf fmt "%s" s

let rec pretty_list sep p fmt l =
  match l with
    | [] -> ()
    | [x] -> p fmt x
    | x::r -> fprintf fmt "%a%t%a" p x sep (pretty_list sep p) r

let pretty_list_del before after sep p fmt l =
  match l with
      [] -> ()
    | l -> fprintf fmt "%t%a%t" before (pretty_list sep p) l after

let pretty_opt print fmt o =
  match o with
      None -> ()
    | Some s -> print fmt s


let pretty_opt_nl print fmt o =
  match o with
      None -> ()
    | Some s ->
        fprintf fmt "@[%a@]@\n" print s

let nl_sep fmt = fprintf fmt "@\n"

let space_sep s fmt = fprintf fmt "%s@ " s

open Cil_types

module InstrMapl = Mapl_Make(Kinstr)

let out_some v = match v with
| None -> assert false
| Some v -> v

type opaque_term_env = {
  term_lhosts: term_lhost Varinfo.Map.t;
  terms: term Varinfo.Map.t;
  vars: logic_var Varinfo.Map.t;
}

type opaque_exp_env = { exps: exp Varinfo.Map.t }

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
