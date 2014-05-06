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

type ('a, 'b) gen_accessor = 
    { get: unit -> 'a; 
      set: 'a -> unit; 
      add_set_hook: ('b -> 'b -> unit) -> unit;
      add_update_hook: ('b -> 'b -> unit) -> unit }

type 'a accessor = ('a, 'a) gen_accessor

type typed_accessor = 
  | Bool of bool accessor * string option (** the negative option, if any *)
  | Int of int accessor * (unit -> int * int) (** getting range *)
  | String of string accessor * (unit -> string list) (** possible values *)
  | String_set of (string, Datatype.String.Set.t) gen_accessor
  | String_list of (string, string list) gen_accessor

type parameter = 
    { name: string; 
      help: string; 
      accessor: typed_accessor; 
      is_set: unit -> bool }

include
  Datatype.Make_with_collections
    (struct
      type t = parameter
      let name = "Parameter.t"
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_unknown
      let reprs = 
        [ { name = "bool_opt";
            help = "dummy bool option";
            accessor = 
            Bool 
              ({ get = (fun () -> false); 
                 set = (fun _ -> ()); 
                 add_set_hook = (fun _ -> ());
		 add_update_hook = (fun _ -> ()) },
               None);
            is_set = fun () -> false }
        ]
      let equal = (==)
      let compare x y = if x == y then 0 else String.compare x.name y.name
      let hash x = Datatype.String.hash x.name
      let copy x = x (* The representation of the parameter is immutable *)
      let pretty fmt x = Format.pp_print_string fmt x.name
      let internal_pretty_code = Datatype.undefined
      let varname _ = assert false 
          (* unused if internal_pretty_code undefined *)
      let mem_project = Datatype.never_any_project
     end)

let parameters = Datatype.String.Hashtbl.create 97
  
let create ~name ~help ~accessor ~is_set = 
  let p = { name = name; help = help; accessor = accessor; is_set = is_set } in
  (* parameter name unicity already checks in [Plugin]. *)
  assert (not (Datatype.String.Hashtbl.mem parameters name));
  Datatype.String.Hashtbl.add parameters name p;
  p

let get = Datatype.String.Hashtbl.find parameters

let pretty_value fmt p = match p.accessor with
  | Bool(a, _) -> Format.fprintf fmt "%b" (a.get ())
  | Int(a, _) -> Format.fprintf fmt "%d" (a.get ())
  (* factorisation requires GADT (OCaml 4.01) *)
  | String(a, _) -> Format.fprintf fmt "%s" (a.get ())
  | String_set a -> Format.fprintf fmt "%s" (a.get ())
  | String_list a -> Format.fprintf fmt "%s" (a.get ())

let get_value p = Pretty_utils.sfprintf "%a" pretty_value p
 
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
