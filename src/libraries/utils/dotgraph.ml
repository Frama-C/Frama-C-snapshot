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

(* -------------------------------------------------------------------------- *)
(* --- Attributes and Such                                                --- *)
(* -------------------------------------------------------------------------- *)

type attr = [
  | `LR
  | `TB
  | `Label of string
  | `Color of string
  | `Fillcolor of string
  | `Shape of string
  | `Style of string
  | `Circle
  | `Box
  | `Oval
  | `Point
  | `Dotted
  | `Filled
  | `ArrowBoth
  | `ArrowBack
  | `ArrowForward
  | `ArrowHead of string
  | `ArrowTail of string
  | `Attr of string * string
]

let attr = function
  | `LR -> "rankdir" , "LR"
  | `TB -> "rankdir" , "TB"
  | `Label txt -> "label" , txt
  | `Color c -> "color" , c
  | `Fillcolor c -> "fillcolor" , c
  | `Shape sh -> "shape" , sh
  | `Style sty -> "style" , sty
  | `Box -> "shape" , "box"
  | `Oval -> "shape" , "oval"
  | `Point -> "shape" , "point"
  | `Circle -> "shape" , "circle"
  | `Filled -> "style" , "filled"
  | `Dotted -> "style" , "dotted"
  | `ArrowBoth -> "dir" , "both"
  | `ArrowBack -> "dir" , "back"
  | `ArrowForward -> "dir" , "forward"
  | `ArrowHead sh -> "arrowhead" , sh
  | `ArrowTail sh -> "arrowtail" , sh
  | `Attr(name,value) -> name , value

let pp_attr fmt (a : attr) =
  let name,value = attr a in Format.fprintf fmt "%s=%S" name value

let pp_attributes fmt = function
  | [] -> ()
  | p::ps ->
    begin
      Format.fprintf fmt "@ %a" pp_attr p ;
      List.iter (fun p -> Format.fprintf fmt ",@ %a" pp_attr p) ps ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Dot Output                                                         --- *)
(* -------------------------------------------------------------------------- *)

type dot = {
  file : string ;
  queue : (unit -> unit) Queue.t ;
  mutable indent : string ;
  mutable id : int ;
  mutable fmt : Format.formatter ;
  mutable out : out_channel option ;
}

let open_dot ?(name="G") ?(attr=[]) ?(file) () =
  let file = match file with None -> name ^ ".dot" | Some f -> f in
  let out = open_out file in
  let fmt = Format.formatter_of_out_channel out in
  begin
    Format.fprintf fmt "digraph %S {@\n" name ;
    List.iter
      (fun p ->
         Format.fprintf fmt "  %a ;@\n" pp_attr p
      ) attr ;
    Format.pp_print_flush fmt () ;
    { file ; fmt ;
      queue = Queue.create () ;
      indent = "  " ; out = Some out ; id = 0 }
  end

let flush dot =
  match dot.out with
  | Some out -> Format.pp_print_flush dot.fmt () ; flush out
  | None -> ()

let basename f =
  if Filename.check_suffix f ".dot" then Filename.chop_suffix f ".dot" else f

let installed = ref None
let is_dot_installed () =
  match !installed with
  | Some s -> s
  | None -> let s = (Sys.command "dot -V" = 0) in installed := Some s ; s

let close dot =
  match dot.out with
  | None -> ()
  | Some out ->
    begin
      Format.fprintf dot.fmt "}@." ;
      dot.fmt <- Format.err_formatter ;
      Pervasives.flush out ; close_out out ;
      dot.out <- None ;
    end

let layout ?(force=false) ?(target="pdf") ?(engine="dot") ?output dot =
  begin
    if dot.out <> None then raise (Invalid_argument "DotGraph: not closed") ;
    let input = dot.file in
    let output =
      match output with Some f -> f | None ->
        Printf.sprintf "%s.%s" (basename dot.file) target in
    let cmd = Printf.sprintf "dot -K%s -T%s %s -o %s"
        engine target input output in
    let status = Sys.command cmd in
    if status=0 then output else
    if force then
      let msg = Printf.sprintf "dot failed with status %d" status in
      raise (Invalid_argument msg)
    else dot.file
  end

let printf dot msg = Format.fprintf dot.fmt msg
let println dot msg =
  Format.kfprintf (fun fmt -> Format.pp_print_newline fmt ()) dot.fmt msg

let push dot f = Queue.push f dot.queue
let pop_all dot =
  while not (Queue.is_empty dot.queue) do
    (Queue.pop dot.queue) ()
  done

(* -------------------------------------------------------------------------- *)
(* --- Nodes and Edges                                                    --- *)
(* -------------------------------------------------------------------------- *)

type node = string

let fresh ?(prefix="_") dot =
  let k = dot.id in dot.id <- succ k ; Printf.sprintf "%s%03d" prefix k

let pp_node = Format.pp_print_string
let pp_edge fmt (a,b) = Format.fprintf fmt "%s -> %s" a b

let pp_stmt dot pp stmt attr =
  Format.fprintf dot.fmt "%s@[<hv 0>@[<hov 2>%a [%a@]@ ]@];@."
    dot.indent pp stmt pp_attributes attr

let inode dot ?prefix ?id attr =
  let a = match id with Some a -> a | None -> fresh ?prefix dot in
  pp_stmt dot pp_node a attr ; a

let node_default dot attr =
  pp_stmt dot pp_node "node" attr

let edge_default dot attr =
  pp_stmt dot pp_node "edge" attr

let node dot id attr = ignore (inode dot ~id attr)

let edge dot a b attr =
  if attr = []
  then Format.fprintf dot.fmt "%s%a ;@." dot.indent pp_edge (a,b)
  else pp_stmt dot pp_edge (a,b) attr

let link dot ps attr =
  let rec walk dot attr p = function
    | q :: ps -> edge dot p q attr ; walk dot attr q ps
    | [] -> ()
  in match ps with
  | p :: ps -> walk dot attr p ps
  | [] -> ()

(* -------------------------------------------------------------------------- *)
(* --- Clustering                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rank dot nodes =
  begin
    Format.fprintf dot.fmt "%s@[<hov 2>{ rank=same;" dot.indent ;
    List.iter (Format.fprintf dot.fmt "@ %s;") nodes ;
    Format.fprintf dot.fmt "@ }@]@\n" ;
  end

let subgraph dot ?(cluster=true) attr content =
  begin
    let k = dot.id in dot.id <- succ k ;
    let indent = dot.indent in
    Format.fprintf dot.fmt "%ssubgraph %s%d {@\n" indent
      (if cluster then "cluster" else "g") k ;
    List.iter
      (fun a -> Format.fprintf dot.fmt "%s  %a;@\n" indent pp_attr a) attr ;
    dot.indent <- indent ^ "  " ;
    content () ;
    dot.indent <- indent ;
    Format.fprintf dot.fmt "%s}@\n" indent ;
  end

type record = [
  | `Empty
  | `Hbox of record list
  | `Vbox of record list
  | `Label of string
  | `Port of string * link list * string
] and link = string * attr list * node

let port a b = if b <> "" then Printf.sprintf "%s:%s" a b else a

module Record =
struct
  let hbox = function `Empty -> [] | `Hbox hs -> hs | h -> [h]
  let vbox = function `Empty -> [] | `Vbox vs -> vs | v -> [v]
  let (<->) a b = `Vbox (vbox a @ vbox b)
  let (<|>) a b = `Hbox (hbox a @ hbox b)
  let link ?(anchor="") ?(attr=[]) node : link = anchor,attr,node
  let label ?(port="") ?(link=[]) txt : record =
    if port="" && link=[] then `Label txt else `Port(port,link,txt)
end

type env = {
  buffer : Buffer.t ;
  mutable links : (string * (string * attr list * node) list) list ;
  mutable port : int ;
}

let rec mk_record env hv = function
  | `Empty -> ()
  | `Label txt -> Buffer.add_string env.buffer txt
  | `Port(port,links,txt) ->
    if links <> [] then
      let port =
        if port = "" then
          let p = env.port in env.port <- succ p ; Printf.sprintf "_p%d" p
        else port in
      env.links <- (port,links) :: env.links ;
      Printf.bprintf env.buffer "<%s> %s" port txt
  | `Hbox rs -> mk_box env hv true rs
  | `Vbox rs -> mk_box env hv false rs

and mk_box env hv hb = function
  | [] -> ()
  | r::rs ->
    begin
      let buffer = env.buffer in
      let boxed = hv <> hb in
      let hv = not hv in
      if boxed then Buffer.add_char buffer '{' ;
      mk_record env hv r ;
      List.iter (fun r ->
          Buffer.add_char buffer '|' ;
          mk_record env hv r
        ) rs ;
      if boxed then Buffer.add_char buffer '}' ;
    end

let irecord dot ?prefix ?id ?(rounded=false) ?(attr=[]) (box : record) =
  let shape = if rounded then "Mrecord" else "record" in
  let env = { buffer = Buffer.create 80 ; links = [] ; port = 1 } in
  mk_record env true box ;
  let label = Buffer.contents env.buffer in
  let node = inode dot ?prefix ?id (`Shape shape :: `Label label :: attr) in
  List.iter
    (fun (name,links) ->
       List.iter
         (fun (anchor,attr,target) ->
            edge dot (port (port node name) anchor) target attr
         ) links
    ) env.links ;
  node

let record dot node ?rounded ?attr box =
  ignore (irecord dot ~id:node ?rounded ?attr box)

(* -------------------------------------------------------------------------- *)
(* --- Indexing                                                           --- *)
(* -------------------------------------------------------------------------- *)

module type Map =
sig
  type key
  type 'a t
  val empty : 'a t
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
end

let kp = ref 0

module Node(M : Map) =
struct
  type t = M.key

  let kid = ref 0
  let prefix = ref None
  let skip _ _ = ()
  let once = ref skip
  let index : node M.t ref = ref M.empty

  let get_prefix () =
    match !prefix with Some p -> p | None ->
      let k = !kp in
      incr kp ;
      let p =
        if k < 26 then String.make 1 (char_of_int (int_of_char 'A' + k))
        else Printf.sprintf "A%d_" k in
      prefix := Some p ; p

  let get a =
    try M.find a !index
    with Not_found ->
      let k = !kid in incr kid ;
      let prefix = get_prefix () in
      let node = Printf.sprintf "%s%03d" prefix k in
      index := M.add a node !index ; !once a node ; node

  let node dot a attr = node dot (get a) attr
  let inode dot a attr = inode dot ~id:(get a) attr

  let record dot a ?rounded ?attr box =
    record dot (get a) ?rounded ?attr box

  let irecord dot a ?rounded ?attr box =
    irecord dot ~id:(get a) ?rounded ?attr box

  let prefix p = prefix := Some p

  let once f = once := f
  let push dot f = once (fun a n -> push dot (fun () -> f a n))
  let clear () = index := M.empty ; kid := 0 ; once skip
end

(* -------------------------------------------------------------------------- *)
(* --- Decorator                                                          --- *)
(* -------------------------------------------------------------------------- *)

type buffer = {
  label : Buffer.t ;
  mutable attributes : attr list ;
}

let apply_label buffer = function
  | `Label txt ->
    let buf = buffer.label in
    Buffer.add_string buf txt ; true
  | _ -> false

let apply buffer a =
  if not (apply_label buffer a) then
    let name = fst (attr a) in
    let filter name a = fst (attr a) <> name in
    buffer.attributes <- a :: List.filter (filter name) buffer.attributes

let add_attr buffer a = List.iter (apply buffer) a

let buffer attr =
  let buffer = { label = Buffer.create 20 ; attributes = [] } in
  add_attr buffer attr ; buffer

let add_char buffer = Buffer.add_char buffer.label
let add_label buffer = Buffer.add_string buffer.label
let bprintf buffer msg =
  let fmt = Format.formatter_of_buffer buffer.label in
  Format.kfprintf (fun fmt -> Format.pp_print_flush fmt ()) fmt msg

let attributes buffer =
  let label = Buffer.contents buffer.label in
  `Label label :: List.rev buffer.attributes

let add_options buffer options =
  List.iter (fun (b,a) -> if b then add_attr buffer a) options

let decorate attr options =
  let buffer = buffer attr in
  add_options buffer options ; attributes buffer

(* -------------------------------------------------------------------------- *)
