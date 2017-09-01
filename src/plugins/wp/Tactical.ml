(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Conditions
open Lang.F

module Tmap = Map.Make(String)
let composers = ref Tmap.empty
let groups = ref []

(* -------------------------------------------------------------------------- *)
(* --- Composer Factory                                                   --- *)
(* -------------------------------------------------------------------------- *)

class type composer =
  object
    method id : string
    method group : string
    method title : string
    method descr : string
    method arity : int
    method filter : term list -> bool
    method compute : term list -> term
  end

let rec insert_group cc = function
  | [] -> [cc#group , [cc]]
  | (( gid , ccs ) as group ):: others ->
      if cc#group = gid then
        ( gid , ccs @ [cc] ) :: others
      else
        group :: insert_group cc others

let add_composer (c : #composer) =
  let id = c#id in
  if Tmap.mem id !composers then
    Wp_parameters.error "Composer #%s already registered (skipped)" id
  else
    begin
      composers := Tmap.add id (c :> composer) !composers ;
      groups := insert_group (c :> composer) !groups ;
    end

let iter_composer f =
  List.iter (fun (_,ccs) -> List.iter f ccs) !groups

(* -------------------------------------------------------------------------- *)
(* --- Tactical Selection                                                 --- *)
(* -------------------------------------------------------------------------- *)

type clause = Goal of pred | Step of step
type process = sequent -> (string * sequent) list
type status =
  | Not_applicable
  | Not_configured
  | Applicable of process

type selection =
  | Empty
  | Clause of clause
  | Inside of clause * term
  | Compose of compose

and compose =
  | Cint of Integer.t
  | Range of int * int
  | Code of term * string * selection list

(* -------------------------------------------------------------------------- *)
(* --- Selection API                                                      --- *)
(* -------------------------------------------------------------------------- *)

let head = function
  | Goal p -> p
  | Step s -> Conditions.head s

let is_empty = function Empty -> true | _ -> false

let composed = function
  | Cint a -> e_zint a
  | Range(a,_) -> e_int a
  | Code(v,_id,_es) -> v

let selected = function
  | Empty -> e_true
  | Inside(_,t) -> t
  | Clause c -> e_prop (head c)
  | Compose code -> composed code

let subclause clause p =
  match clause with
  | Step s ->
      let hs = Conditions.have s in
      hs == p ||
      ( match Lang.F.p_expr hs with
        | Qed.Logic.And ps -> List.memq p ps
        | _ -> false )
  | Goal hs ->
      hs == p ||
      ( match Lang.F.p_expr hs with
        | Qed.Logic.Or ps -> List.memq p ps
        | _ -> false )

let pp_clause fmt = function
  | Goal _ -> Format.pp_print_string fmt "Goal"
  | Step s -> Format.fprintf fmt "Hyp %d" s.id

let rec pp_selection fmt = function
  | Empty -> Format.pp_print_string fmt "Empty"
  | Inside(c,t) ->
      Format.fprintf fmt "Term %d in %a" (Lang.F.QED.id t) pp_clause c
  | Clause c -> pp_clause fmt c
  | Compose(Cint k) ->
      Format.fprintf fmt "Constant '%a'" (Integer.pretty ~hexa:false) k
  | Compose(Range(a,b)) ->
      Format.fprintf fmt "Range '%d..%d'" a b
  | Compose(Code(_,id,es)) ->
      Format.fprintf fmt "@[<hov 2>Compose '%s'" id ;
      List.iter (fun e -> Format.fprintf fmt "(%a)" pp_selection e) es ;
      Format.fprintf fmt "@]"

let int a = Compose(Cint (Integer.of_int a))
let cint a = Compose(Cint a)
let range a b = Compose(Range(a,b))
let compose id es =
  try
    let cc = Tmap.find id !composers in
    let e = cc#compute (List.map selected es) in
    match Lang.F.repr e with
    | Qed.Logic.Kint n -> cint n
    | _ -> Compose(Code(e,id,es))
  with Not_found -> Empty

let findhead (s:selection) e =
  match s with
  | Empty -> None
  | Compose(Range _ | Cint _) -> None
  | Inside(clause,_) | Clause clause ->
      let p = Lang.F.e_prop (head clause) in
      if Lang.F.is_subterm e p
      then Some(Inside(clause,e))
      else None
  | Compose(Code(v,_,_)) as s ->
      if v == e then Some s else None

let rec lookup (s:selection) e q =
    match findhead s e with
    | Some _ as result -> result
    | None -> lookup_inner s e q

and lookup_inner (s:selection) e q =
  begin match s with
    | Compose(Code(_,_,ps)) ->
        List.iter (fun p -> Queue.add p q) ps
    | _ -> ()
  end ;
  if Queue.is_empty q then None else lookup (Queue.pop q) e q

and subterm (s:selection) e =
  match Lang.F.repr e with
  | Qed.Logic.Kint z -> Some (cint z)
  | _ ->
      match findhead s e with
      | Some _ as result -> result
      | None -> lookup_inner s e (Queue.create ())

let rec subterms s = function
  | [] -> []
  | e::es ->
      let ps = subterms s es in
      match subterm s e with
      | None -> ps
      | Some p -> p::ps

let destruct_value s =
  let v = selected s in
  let open Qed.Logic in
  match Lang.F.repr v with
  | Kint _ | Kreal _ | True | False | Bind _ | Fvar _ | Bvar _ | Apply _ -> []
  | Add es | Mul es | And es | Or es | Fun(_,es) -> subterms s es
  | Imply(hs,p) -> subterms s (hs @ [p])
  | If(a,b,c) | Aset(a,b,c) -> subterms s [a;b;c]
  | Not a | Rget(a,_) -> subterms s [a]
  | Rdef fvs -> subterms s (List.map snd fvs)
  | Times(k,v) -> cint k :: subterms s [v]
  | Div(a,b) | Mod(a,b) | Eq(a,b) | Neq(a,b) | Lt(a,b) | Leq(a,b) | Aget(a,b) ->
      subterms s [a;b]

let destruct = function
  | Empty | Compose(Cint _) -> []
  | Compose(Range(a,b)) -> [int a;int b]
  | s ->
      let ps = destruct_value s in
      if ps <> [] then ps else
        match s with
        | Compose(Code(_,_,ps)) -> ps
        | _ -> []

(* -------------------------------------------------------------------------- *)
(* --- Fields                                                             --- *)
(* -------------------------------------------------------------------------- *)

type 'a named = { title : string ; descr : string ; vid : string ; value : 'a }
type 'a range = { vmin : 'a option ; vmax : 'a option ; vstep : 'a }
type 'a field = 'a named (* value is the default *)
type 'a browser = ('a named -> unit) -> selection -> unit

let field ~id ~title ~descr ~default : 'a field =
  if id = "" then raise (Invalid_argument "Tactical.field") ;
  { title ; descr ; vid = id ; value=default }

let ident fd = fd.vid
let signature fd = fd
let default fd = fd.value

module Fmap :
sig
  type t
  val create : unit -> t
  val reset : t -> unit
  val get : t -> 'a field -> 'a
  val set : t -> 'a field -> 'a -> unit
end =
struct
  type t = (string,Obj.t) Hashtbl.t
  let create () = Hashtbl.create 8
  let reset t = Hashtbl.clear t
  let get m (fd : 'a field) : 'a =
    try Obj.obj (Hashtbl.find m fd.vid) with Not_found -> fd.value
  let set m (fd : 'a field) (v : 'a) = Hashtbl.add m fd.vid (Obj.repr v)
end

(* -------------------------------------------------------------------------- *)
(* --- Parameters                                                         --- *)
(* -------------------------------------------------------------------------- *)

type parameter =
  | Checkbox of bool field
  | Spinner  of int field * int range
  | Composer of selection field * (Lang.F.term -> bool)
  | Selector : 'a field * 'a named list * ('a -> 'a -> bool) -> parameter
  | Search : 'a named option field * 'a browser * (string -> 'a) -> parameter

let checkbox ~id ~title ~descr ?(default=false) () =
  let fd = field ~id ~title ~descr ~default in
  fd , Checkbox fd

let spinner ~id ~title ~descr ?default ?vmin ?vmax ?(vstep=1) () =
  let () = match vmin , vmax with
    | Some a , Some b ->
        if a >= b then raise (Invalid_argument "Tactical.spinner")
    | _ -> () in
  let default = match default, vmin, vmax with
    | Some v , _ , _ -> v
    | None , None , None -> 0
    | None , Some v , _ -> v
    | None , None , Some v -> v in
  let fd = field ~id ~title ~descr ~default in
  fd , Spinner(fd,{vmin;vmax;vstep})

let selector ~id ~title ~descr ?default ~options ?(equal=(=)) () =
  let default = match default,options with
    | _ , [] -> raise (Invalid_argument "Tactical.selector(empty)")
    | Some value , vs ->
        if List.for_all (fun v -> equal v.value value) vs
        then raise (Invalid_argument "Tactical.selector(default)") ;
        value
    | None , {value}::_ -> value in
  let fd = field ~id ~title ~descr ~default in
  fd , Selector(fd,options,equal)

let accept _ = true
let composer ~id ~title ~descr ?(default=Empty) ?(filter=accept) () =
  let fd = field ~id ~title ~descr ~default in
  fd , Composer(fd,filter)

let search ~id ~title ~descr ~browse ~find () =
  let fd = field ~id ~title ~descr ~default:None in
  fd , Search(fd,browse,find)

(* -------------------------------------------------------------------------- *)
(* --- Feedback                                                           --- *)
(* -------------------------------------------------------------------------- *)

type 'a formatter = ('a,Format.formatter,unit) format -> 'a

class type feedback =
  object
    method interactive : bool
    method get_title : string
    method has_error : bool
    method set_title : 'a. 'a formatter
    method set_descr : 'a. 'a formatter
    method set_error : 'a. 'a formatter
    method update_field :
      'a. ?enabled:bool -> ?title:string -> ?tooltip:string ->
      ?range:bool -> ?vmin:int -> ?vmax:int ->
      ?filter:(Lang.F.term -> bool) -> 'a field -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Process Utilities                                         --- *)
(* -------------------------------------------------------------------------- *)

let at = function
  | Empty | Clause (Goal _) | Inside(Goal _,_) | Compose _ -> None
  | Clause (Step s) | Inside(Step s,_) -> Some s.id

let mapi f cases =
  let rec iter f i n = function
    | [] -> []
    | p::ps -> (f i n p) :: iter f (succ i) n ps
  in iter f 1 (List.length cases) cases

let insert ?at cases sequent =
  List.map
    (fun (descr,p) ->
       let step = Conditions.(step ~descr (When p)) in
       descr , Conditions.insert ?at step sequent)
    cases

let replace ~at cases sequent =
  List.map
    (fun (descr,cond) ->
       let step = Conditions.(step ~descr cond) in
       descr , Conditions.replace ~at step sequent)
    cases

let split cases sequent =
  let hyps = fst sequent in
  List.map (fun (descr,p) -> descr,(hyps,p)) cases

let rewrite ?at patterns sequent =
  List.map
    (fun (descr,guard,src,tgt) ->
       let sequent =
         Conditions.subst
           (fun e -> if e == src then tgt else e)
           sequent in
       let step = Conditions.(step ~descr (When guard)) in
       descr , Conditions.insert ?at step sequent
    ) patterns

(* -------------------------------------------------------------------------- *)
(* --- Tactical Engines                                                   --- *)
(* -------------------------------------------------------------------------- *)

class type tactical =
  object
    method id : string
    method title : string
    method descr : string
    method params : parameter list
    method reset : unit
    method get_field : 'a. 'a field -> 'a
    method set_field : 'a. 'a field -> 'a -> unit
    method select : feedback -> selection -> status
  end

type t = tactical

(* -------------------------------------------------------------------------- *)
(* --- Tactical Builder                                                   --- *)
(* -------------------------------------------------------------------------- *)

class virtual make ~id ~title ~descr ~params =
  object
    val hmap = Fmap.create ()
    method id : string = id
    method title : string = title
    method descr : string = descr
    method params : parameter list = params
    method reset = Fmap.reset hmap
    method get_field : 'a. 'a field -> 'a = Fmap.get hmap
    method set_field : 'a. 'a field -> 'a -> unit = Fmap.set hmap
    method virtual select : feedback -> selection -> status
  end

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let tacticals = ref Tmap.empty

let register t =
  let id = t#id in
    if Tmap.mem id !tacticals then
      Wp_parameters.error "Tactical #%s already registered (skipped)" id
    else
      tacticals := Tmap.add id (t :> t) !tacticals

let export t = register t ; (t :> t)
let iter f = Tmap.iter (fun _id t -> f t) !tacticals
let lookup ~id = Tmap.find id !tacticals

(* -------------------------------------------------------------------------- *)
(* --- Default Composers                                                  --- *)
(* -------------------------------------------------------------------------- *)

open Lang

let () =
  for i = 0 to 9 do
    add_composer
      (object
        method id = Printf.sprintf "wp:%d" i
        method group = "const:unit"
        method title = string_of_int i
        method descr = ""
        method arity = 0
        method filter = function _ -> true
        method compute = function _ -> F.e_int i
      end)
  done

let () = add_composer
    (object
      method id = "wp:eq"
      method group = "logic"
      method title = "A == B"
      method descr = ""
      method arity = 2
      method filter = function
        | [a;b] ->
            (try
               let ta = F.typeof a in
               let tb = F.typeof b in
               F.Tau.equal ta tb
             with Not_found -> false)
        | _ -> false
      method compute = function [a;b] -> F.e_eq a b | _ -> F.e_true
    end)

let () = add_composer
    (object
      method id = "wp:leq"
      method group = "logic"
      method title = "A <= B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = function [a;b] -> F.e_leq a b | _ -> F.e_true
    end)

let () = add_composer
    (object
      method id = "wp:lt"
      method group = "logic"
      method title = "A < B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = function [a;b] -> F.e_lt a b | _ -> F.e_true
    end)

let () = add_composer
    (object
      method id = "wp:range"
      method group = "logic"
      method title = "A <= B <= C"
      method descr = ""
      method arity = 3
      method filter = List.for_all F.is_arith
      method compute = function [a;b;c] -> F.e_and [F.e_leq a b;F.e_leq b c]
                              | _ -> F.e_true
    end)

let () = add_composer
    (object
      method id = "wp:not"
      method group = "logic"
      method title = "not A"
      method descr = ""
      method arity = 1
      method filter = List.for_all F.is_prop
      method compute = function a::_ -> F.e_not a | _ -> F.e_false
    end)

let () = add_composer
    (object
      method id = "wp:and"
      method group = "logic"
      method title = "A && B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_prop
      method compute = F.e_and
    end)

let () = add_composer
    (object
      method id = "wp:or"
      method group = "logic"
      method title = "A || B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_prop
      method compute = F.e_or
    end)

let () = add_composer
    (object
      method id = "wp:incr"
      method group = "additive"
      method title = "A+1"
      method descr = ""
      method arity = 1
      method filter = List.for_all F.is_arith
      method compute es = F.e_sum (F.e_int 1 :: es)
    end)

let () = add_composer
    (object
      method id = "wp:decr"
      method group = "additive"
      method title = "A-1"
      method descr = ""
      method arity = 1
      method filter = List.for_all F.is_arith
      method compute es = F.e_sum (F.e_int (-1) :: es)
    end)

let () = add_composer
    (object
      method id = "wp:add"
      method group = "additive"
      method title = "Add A+B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = F.e_sum
    end)

let () = add_composer
    (object
      method id = "wp:sub"
      method group = "additive"
      method title = "Sub A-B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = function [a;b] -> F.e_sub a b | _ -> F.e_int 0
    end)

let () =
  add_composer
    (object
      method id = Printf.sprintf "wp:ten"
      method group = "product"
      method title = "A*10"
      method descr = ""
      method arity = 1
      method filter = List.for_all F.is_arith
      method compute = function [e] -> F.e_times (Integer.of_int 10) e
                              | _ -> F.e_int 0
    end)

let () = add_composer
    (object
      method id = "wp:mul"
      method group = "product"
      method title = "Mul A*B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = F.e_prod
    end)

let () = add_composer
    (object
      method id = "wp:div"
      method group = "product"
      method title = "Div A/B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_arith
      method compute = function [a;b] -> F.e_div a b | _ -> F.e_int 1
    end)

let () = add_composer
    (object
      method id = "wp:mod"
      method group = "product"
      method title = "Mod A%B"
      method descr = ""
      method arity = 2
      method filter = List.for_all F.is_int
      method compute = function [a;b] -> F.e_mod a b | _ -> F.e_int 1
    end)

let () = add_composer
    (object
      method id = "wp:get"
      method group = "structure"
      method title = "Get A[B]"
      method descr = ""
      method arity = 2
      method filter = function
        | [a;b] ->
            begin
              try
                let ta = F.typeof a in
                let tb = F.typeof b in
                match ta with
                | Qed.Logic.Array(tm,_) -> F.Tau.equal tm tb
                | _ -> false
              with Not_found -> false
            end
        | _ -> false
      method compute = function [a;b] -> F.e_get a b | _ -> F.e_int 0
    end)

let () = add_composer
    (object
      method id = "wp:set"
      method group = "structure"
      method title = "Set A[B <-C]"
      method descr = ""
      method arity = 3
      method filter = function
        | [a;b;c] ->
            begin
              try
                let ta = F.typeof a in
                let tb = F.typeof b in
                let tc = F.typeof c in
                match ta with
                | Qed.Logic.Array(tm,tv) ->
                    F.Tau.equal tm tb &&
                    F.Tau.equal tv tc
                | _ -> false
              with Not_found -> false
            end
        | _ -> false
      method compute = function [a;b] -> F.e_get a b | _ -> F.e_int 0
    end)

(* -------------------------------------------------------------------------- *)
