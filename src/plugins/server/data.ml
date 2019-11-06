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
(* --- Data Encoding                                                      --- *)
(* -------------------------------------------------------------------------- *)

module Js = Yojson.Basic
module Ju = Yojson.Basic.Util

type json = Js.t
let pretty = Js.pretty_print ~std:false

module type S =
sig
  type t
  val syntax : Syntax.t
  val of_json : json -> t
  val to_json : t -> json
end

module type Info =
sig
  val page : Doc.page
  val name : string
  val descr : Markdown.text
end

type 'a data = (module S with type t = 'a)

exception InputError of string

let failure ?json msg =
  let add_json msg =
    let msg = match json with
      | None -> msg
      | Some json ->
        Format.asprintf "@[%s:@ %s@]" msg (Js.pretty_to_string json)
    in
    raise(InputError(msg))
  in
  Pretty_utils.ksfprintf add_json msg

let failure_from_type_error msg json =
  failure ~json "%s" msg

(* -------------------------------------------------------------------------- *)
(* --- Option                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Joption(A : S) : S with type t = A.t option =
struct
  type t = A.t option

  let nullable = try ignore (A.of_json `Null) ; true with _ -> false
  let syntax =
    Syntax.option (if nullable then A.syntax else Syntax.tuple [A.syntax])

  let to_json = function
    | None -> `Null
    | Some v -> if nullable then `List [A.to_json v] else A.to_json v

  let of_json = function
    | `Null -> None
    | `List [js] when nullable -> Some (A.of_json js)
    | js -> Some (A.of_json js)

end

(* -------------------------------------------------------------------------- *)
(* --- Tuples                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Jpair(A : S)(B : S) : S with type t = A.t * B.t =
struct
  type t = A.t * B.t
  let syntax = Syntax.tuple [A.syntax;B.syntax]
  let to_json (x,y) = `List [ A.to_json x ; B.to_json y ]
  let of_json = function
    | `List [ ja ; jb ] -> A.of_json ja , B.of_json jb
    | js -> failure ~json:js "Expected list with 2 elements"
end

module Jtriple(A : S)(B : S)(C : S) : S with type t = A.t * B.t * C.t =
struct
  type t = A.t * B.t * C.t
  let syntax = Syntax.tuple [A.syntax;B.syntax;C.syntax]
  let to_json (x,y,z) = `List [ A.to_json x ; B.to_json y ; C.to_json z ]
  let of_json = function
    | `List [ ja ; jb ; jc ] -> A.of_json ja , B.of_json jb , C.of_json jc
    | js -> failure ~json:js "Expected list with 3 elements"
end

(* -------------------------------------------------------------------------- *)
(* --- Lists                                                              --- *)
(* -------------------------------------------------------------------------- *)

module Jlist(A : S) : S with type t = A.t list =
struct
  type t = A.t list
  let syntax = Syntax.array A.syntax
  let to_json xs = `List (List.map A.to_json xs)
  let of_json js = List.map A.of_json (Ju.to_list js)
end

(* -------------------------------------------------------------------------- *)
(* --- Arrays                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Jarray(A : S) : S with type t = A.t array =
struct
  type t = A.t array
  let syntax = Syntax.array A.syntax
  let to_json xs = `List (List.map A.to_json (Array.to_list xs))
  let of_json js = Array.of_list @@ List.map A.of_json (Ju.to_list js)
end

(* -------------------------------------------------------------------------- *)
(* --- Collections                                                        --- *)
(* -------------------------------------------------------------------------- *)

module type S_collection =
sig
  include S
  module Joption : S with type t = t option
  module Jlist : S with type t = t list
  module Jarray : S with type t = t array
end

module Collection(A : S) : S_collection with type t = A.t =
struct
  include A
  module Joption = Joption(A)
  module Jlist = Jlist(A)
  module Jarray = Jarray(A)
end

(* -------------------------------------------------------------------------- *)
(* --- Atomic Types                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Junit : S with type t = unit =
struct
  type t = unit
  let syntax = Syntax.unit
  let of_json _js = ()
  let to_json () = `Null
end

module Jany : S with type t = json =
struct
  type t = json
  let syntax = Syntax.any
  let of_json js = js
  let to_json js = js
end

module Jbool : S_collection with type t = bool =
  Collection
    (struct
      type t = bool
      let syntax = Syntax.boolean
      let of_json = Ju.to_bool
      let to_json b = `Bool b
    end)

module Jint : S_collection with type t = int =
  Collection
    (struct
      type t = int
      let syntax = Syntax.int
      let of_json = Ju.to_int
      let to_json n = `Int n
    end)

module Jfloat : S_collection with type t = float =
  Collection
    (struct
      type t = float
      let syntax = Syntax.number
      let of_json = Ju.to_number
      let to_json v = `Float v
    end)

module Jstring : S_collection with type t = string =
  Collection
    (struct
      type t = string
      let syntax = Syntax.string
      let of_json = Ju.to_string
      let to_json s = `String s
    end)

module Jident : S_collection with type t = string =
  Collection
    (struct
      type t = string
      let syntax = Syntax.ident
      let of_json = Ju.to_string
      let to_json s = `String s
    end)

let text_page = Doc.page `Kernel ~title:"Rich Text Format" ~filename:"text.md"

module Jtext =
struct
  include Jany
  let syntax = Syntax.publish ~page:text_page ~name:"text"
      ~synopsis:Syntax.any ~descr:(Markdown.plain "Formatted text.") ()
end

(* -------------------------------------------------------------------------- *)
(* --- Records                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Fmap = Map.Make(String)

type 'a record = json Fmap.t

type ('r,'a) field = {
  member : 'r record -> bool ;
  getter : 'r record -> 'a ;
  setter : 'r record -> 'a -> 'r record ;
}

type 'a signature = {
  page : Doc.page ;
  name : string ;
  descr : Markdown.text ;
  mutable fields : Syntax.field list ;
  mutable default : 'a record ;
  mutable published : bool ;
}

module Record =
struct

  module type S =
  sig
    type r
    include S with type t = r record
    val default : t
    val has : (r,'a) field -> t -> bool
    val get : (r,'a) field -> t -> 'a
    val set : (r,'a) field -> 'a -> t -> t
  end

  let signature ~page ~name ~descr () = {
    page ; name ; descr ;
    published = false ;
    fields = [] ;
    default = Fmap.empty ;
  }

  let field (type a r) (s : r signature)
      ~name ~descr ?default (d : a data) : (r,a) field =
    if s.published then
      raise (Invalid_argument "Server.Data.Record.field") ;
    let module D = (val d) in
    begin match default with
      | None -> ()
      | Some v -> s.default <- Fmap.add name (D.to_json v) s.default
    end ;
    let field = Syntax.{ name ; syntax = D.syntax ; descr } in
    s.fields <- field :: s.fields ;
    let member r = Fmap.mem name r in
    let getter r = D.of_json (Fmap.find name r) in
    let setter r v = Fmap.add name (D.to_json v) r in
    { member ; getter ; setter }

  let option (type a r) (s : r signature)
      ~name ~descr (d : a data) : (r,a option) field =
    if s.published then
      raise (Invalid_argument "Server.Data.Record.option") ;
    let module D = (val d) in
    let field = Syntax.{ name ; syntax = option D.syntax ; descr } in
    s.fields <- field :: s.fields ;
    let member r = Fmap.mem name r in
    let getter r =
      try Some (D.of_json (Fmap.find name r)) with Not_found -> None in
    let setter r = function
      | None -> Fmap.remove name r
      | Some v -> Fmap.add name (D.to_json v) r in
    { member ; getter ; setter }

  let publish (type r) (s : r signature) =
    if s.published then
      raise (Invalid_argument "Server.Data.Record.publish") ;
    let module M =
    struct
      type nonrec r = r
      type t = r record
      let descr = s.descr
      let syntax =
        let fields = Syntax.fields ~title:"Field" (List.rev s.fields) in
        Syntax.publish ~page:s.page ~name:s.name ~descr
          ~synopsis:(Syntax.record [])
          ~details:[fields] ()
      let default = s.default
      let has fd r = fd.member r
      let get fd r = fd.getter r
      let set fd v r = fd.setter r v
      let of_json js =
        List.fold_left
          (fun r (fd,js) -> Fmap.add fd js r)
          default (Ju.to_assoc js)
      let to_json r : json =
        `Assoc (Fmap.fold (fun fd js fds -> (fd,js) :: fds) r [])
    end in
    begin
      s.default <- Fmap.empty ;
      s.fields <- [] ;
      s.published <- true ;
      (module M : S with type r = r)
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Index                                                              --- *)
(* -------------------------------------------------------------------------- *)

(** Simplified [Map.S] *)
module type Map =
sig
  type 'a t
  type key
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
end

module type Index =
sig
  include S_collection
  val get : t -> int
  val find : int -> t
  val clear : unit -> unit
end

let publish_id (module A : Info) =
  Syntax.publish
    ~page:A.page ~name:A.name ~synopsis:Syntax.int ~descr:A.descr ()

module INDEXER(M : Map)(I : Info) :
sig
  type index
  val create : unit -> index
  val clear : index -> unit
  val get : index -> M.key -> int
  val find : index -> int -> M.key
  val to_json : index -> M.key -> json
  val of_json : index -> json -> M.key
end =
struct

  type index = {
    mutable kid : int ;
    mutable index : int M.t ;
    lookup : (int,M.key) Hashtbl.t ;
  }

  let create () = {
    kid = 0 ;
    index = M.empty ;
    lookup = Hashtbl.create 0 ;
  }

  let clear m =
    begin
      m.kid <- 0 ;
      m.index <- M.empty ;
      Hashtbl.clear m.lookup ;
    end

  let get m a =
    try M.find a m.index
    with Not_found ->
      let id = m.kid in
      m.kid <- succ id ;
      m.index <- M.add a id m.index ;
      Hashtbl.add m.lookup id a ; id

  let find m id = Hashtbl.find m.lookup id

  let to_json m a = `Int (get m a)
  let of_json m js =
    let id = Ju.to_int js in
    try find m id
    with Not_found ->
      failure "[%s] No registered id #%d" I.name id

end

module Static(M : Map)(I : Info) : Index with type t = M.key =
struct
  module INDEX = INDEXER(M)(I)
  let index = INDEX.create ()
  let clear () = INDEX.clear index
  let get = INDEX.get index
  let find = INDEX.find index
  include Collection
      (struct
        type t = M.key
        let syntax = publish_id (module I)
        let of_json = INDEX.of_json index
        let to_json = INDEX.to_json index
      end)
end

module Index(M : Map)(I : Info) : Index with type t = M.key =
struct

  module INDEX = INDEXER(M)(I)
  module TYPE : Datatype.S with type t = INDEX.index =
    Datatype.Make
      (struct
        type t = INDEX.index
        include Datatype.Undefined
        let reprs = [INDEX.create()]
        let name = "Server.Data.Index.Type." ^ I.name
        let mem_project = Datatype.never_any_project
      end)
  module STATE = State_builder.Ref(TYPE)
      (struct
        let name = "Server.Data.Index.State." ^ I.name
        let dependencies = []
        let default = INDEX.create
      end)

  let index () = STATE.get ()
  let clear () = INDEX.clear (index())

  let get a = INDEX.get (index()) a
  let find id = INDEX.find (index()) id

  include Collection
      (struct
        type t = M.key
        let syntax = publish_id (module I)
        let of_json js = INDEX.of_json (index()) js
        let to_json v = INDEX.to_json (index()) v
      end)

end

module type IdentifiedType =
sig
  type t
  val id : t -> int
  include Info
end

module Identified(A : IdentifiedType) : Index with type t = A.t =
struct

  type index = (int,A.t) Hashtbl.t

  module TYPE : Datatype.S with type t = index =
    Datatype.Make
      (struct
        type t = index
        include Datatype.Undefined
        let reprs = [Hashtbl.create 0]
        let name = "Server.Data.Identified.Type." ^ A.name
        let mem_project = Datatype.never_any_project
      end)

  module STATE = State_builder.Ref(TYPE)
      (struct
        let name = "Server.Data.Identified.State." ^ A.name
        let dependencies = []
        let default () = Hashtbl.create 0
      end)

  let lookup () = STATE.get ()
  let clear () = Hashtbl.clear (lookup())

  let get = A.id
  let find id = Hashtbl.find (lookup()) id

  include Collection
      (struct
        type t = A.t
        let syntax = publish_id (module A)
        let to_json a = `Int (get a)
        let of_json js =
          let k = Ju.to_int js in
          try find k
          with Not_found -> failure "[%s] No registered id #%d" A.name k
      end)

end

(* -------------------------------------------------------------------------- *)
(* --- Dictionnary                                                        --- *)
(* -------------------------------------------------------------------------- *)

module type Enum =
sig
  type t
  val values : (t * string * Markdown.text) list
  include Info
end

module Dictionary(E : Enum) =
struct

  let registered = ref false
  let index = Hashtbl.create 0
  let lookup = Hashtbl.create 0

  let register () =
    if not !registered then
      begin
        registered := true ;
        let invalid msg tag =
          let msg = Printf.sprintf "Server.Data.Enum.%s: duplicate %s (%S)"
              E.name msg tag in
          raise (Invalid_argument msg)
        in
        List.iter
          (fun (value,tag,_) ->
             if Hashtbl.mem index value then invalid "value" tag ;
             Hashtbl.add index value tag ;
             if Hashtbl.mem lookup tag then invalid "tag" tag ;
             Hashtbl.add lookup tag value ;
          ) E.values
      end

  let values =
    let open Markdown in
    let caption = Some (plain "Values description") in
    let header = [ plain E.name, Left; plain "Description", Left ] in
    let content =
      List.map
        (fun (_,tag,descr) -> [ format "`%S`" tag ; descr ])
        E.values
    in
    Table { caption; header; content }

  include Collection
      (struct
        type t = E.t

        let syntax = Syntax.publish
            ~page:E.page ~name:E.name
            ~synopsis:Syntax.ident
            ~descr:E.descr ~details:[values] ()

        let to_json value =
          register () ;
          try `String (Hashtbl.find index value)
          with Not_found ->
            raise (Invalid_argument
                     (Printf.sprintf "[%s] Unregistered value" E.name))

        let of_json js =
          register () ;
          let tag = Ju.to_string js in
          try Hashtbl.find lookup tag
          with Not_found ->
            let msg = Printf.sprintf "[%s] Unregistered tag %S" E.name tag in
            raise (Ju.Type_error(msg,js))

      end)

end

(* -------------------------------------------------------------------------- *)
