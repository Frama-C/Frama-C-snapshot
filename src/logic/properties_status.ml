(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Db_types

module type S = sig 
  type t 
  val get_all: 
    ?who:Project.Computation.t list -> t -> 
    (annotation_status * Project.Computation.t) list
  val get_all_status: 
    ?who:Project.Computation.t list -> t -> annotation_status list
  val get_all_states: 
    ?who:Project.Computation.t list -> t -> Project.Computation.t list
  val strongest: 
    ?who:Project.Computation.t list -> t -> 
    annotation_status * Project.Computation.t
  val pretty_all: Format.formatter -> t -> unit

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
  val add_dependency: Project.Computation.t -> Project.Computation.t -> unit
  val self: Project.Computation.t
end    

let emitters = Hashtbl.create 7

let max_state s1 s2 = 
  if s2 = Project.Computation.dummy || Hashtbl.mem emitters s1 then s1
  else s2

let strongest_status l = 
  List.fold_left 
    (fun (s, st1 as acc) (x, st2 as v) -> match s, x with
     | Unknown, _ -> v
     | Checked _, Unknown -> acc
     | Checked {valid=False}, Checked {valid=True}
     | Checked {valid=True}, Checked {valid=False} -> 
         Kernel.error "Inconsistent status: %a/%a" 
           Cil.d_annotation_status s
           Cil.d_annotation_status x;
         acc
     | Checked {valid=True}, Checked {valid=True} -> s, max_state st1 st2
     | Checked {valid=True}, _ -> acc
     | Checked _ ,Checked {valid=True} -> v 
     | Checked {valid=Maybe}, Checked {valid=Maybe} -> s, max_state st1 st2
     | Checked {valid=Maybe}, Checked {valid=False} -> v
     | Checked {valid=False}, Checked {valid=False} -> s, max_state st1 st2
     | Checked {valid=False}, Checked {valid=Maybe} -> acc)
    (Unknown, Project.Computation.dummy)
    l

let weakest_status l = 
  List.fold_left 
    (fun acc x -> match acc, x with
     | Unknown, _ | _,Unknown -> Unknown
     | Checked a, Checked b -> 
         Checked( 
           match a,b with 
           |{valid=True},_ -> b
           |_,{valid=True} -> a
           |{valid=Maybe}, _ -> a
           |_, {valid=Maybe}-> b
           |({valid=False}, {valid=False}) -> a))
    (Checked {valid=True; emitter="nothing to prove"})
    l

(*  Generic local functor to make getters and setters
    for all kind of uniquely identified objects *)
module Make(A: sig type t type id val id: t -> id val name: string end) = 
struct 

  include
    Computation.Dashtbl
    (struct 
       type t = A.t
       let id x = A.id x
       let hash x = Hashtbl.hash (id x)
       let equal x y = id x = id y
     end)
    (Datatype.Ref(Cil_datatype.Annotation_Status))
    (struct 
       let name = A.name
       let size = 7
       let dependencies = [ Ast.self ]
     end)
  
  let add_dependency p1 p2 = 
    Kernel.debug "Adding dependency from %S to %S" 
      (Project.Computation.name p1)
      (Project.Computation.name p2);
    add_dependency p1 p2
  
  let get_all ?who key = List.map (fun (a,b) -> !a,b) (find_all ?who key)
  let get_all_status ?who key = List.map (!) (find_all_data ?who key)
  let get_all_states ?who key = find_all_states ?who key

  let strongest ?who:_ annot = 
    let l = fold_key (fun s a acc -> (!a,s) :: acc) annot [] in
    strongest_status (*(get_all ?who annot)*)l

  let pretty_all fmt annot =
    Pretty_utils.pp_list ~sep:";" 
      Cil.d_annotation_status 
      fmt
      (List.filter ((<>) Unknown) (get_all_status annot))

  include A

  let compare x y = Pervasives.compare (A.id x) (A.id y)
  let equal x y = A.id x = A.id y
  let hash x = Hashtbl.hash (A.id x)
      
end

module CodeAnnotation =
  Make(struct 
         type t = code_annotation
         type id = int
         let id c = c.annot_id
         let name = "Code annotation" 
       end)
 
module Predicate =
  Make(struct 
         type t = identified_predicate
         type id = int
         let id p = p.ip_id
         let name = "Predicate"
       end)

module Assigns =
  Make(struct 
         type t = 
	     kernel_function 
	     * kinstr
	     * funbehavior option
	     * identified_term assigns list
         type id = int*int option*string
         let id ((f,ki,b,_):t) : id= 
           Kernel_function.get_id f,
           (match ki with Kglobal -> None | _ -> Some (Ast_info.get_sid ki)),
           (Extlib.may_map ~dft:"" (fun b -> b.b_name) b)
         let name = "Assigns"
       end)

module Behavior = struct
  include Make(struct 
         type t = Kernel_function.t * kinstr * funbehavior
         type id = int * string
         let id (f, _, b) = Kernel_function.get_id f, b.b_name
         let name = "Behavior"
       end)

  let get_all_ref = ref []
  let strongest ?who annot = 
    assert (who = None); 
    strongest_status (List.map (fun f -> f annot) !get_all_ref)
end

module Complete =
  Make(struct 
         type t = kernel_function * kinstr * string list
         type id = int * int option * string list
         let id (f, ki, x) = Kernel_function.get_id f, 
           (match ki with Kglobal -> None 
            | _ -> Some (Ast_info.get_sid ki)), 
           x
         let name = "Complete"
       end)

module Disjoint =
  Make(struct 
         type t =  kernel_function * kinstr * string list
         type id = int * int option * string list
         let id (f,ki, x) = Kernel_function.get_id f, 
           (match ki with Kglobal -> None | _ -> Some (Ast_info.get_sid ki)), x
         let name = "Disjoint"
       end)
    
module Make_updater
  (P: sig 
     val name: string 
     val dependencies: Project.Computation.t list
   end) = 
struct

  let plugin_self = 
    match P.dependencies with 
    | [] -> Ast.self
    | [ s ] -> s
    | _ :: _ -> assert false

  let () = Hashtbl.add emitters plugin_self ()

  module Make
    (A: sig 
       include S 
       include Computation.DASHTBL_OUTPUT 
         with type key = t 
         and type data = annotation_status ref
       type id
       val id: t -> id
       val name: string 
     end) = 
  struct

    include A

    let get annot = 
      try 
        !(find_data annot plugin_self) 
      with Not_found -> 
        add annot [ plugin_self ] (ref Unknown);
        Unknown

    let set annot status =
      try 
        let s = find_data annot plugin_self in
        s := status
      with Not_found -> 
        add annot [ plugin_self ] (ref status)

    let update annot f = 
      try 
        let s = find_data annot plugin_self in
        s := f !s;
        !s
      with Not_found -> 
        let new_s = f Unknown in
        add annot [ plugin_self ] (ref new_s);
        new_s

  end

  module Make_Dependent
    (A: sig 
       include S 
       include Computation.DASHTBL_OUTPUT with type key = t 
					  and type data = annotation_status ref
       val get_all_ref: 
	 (t -> annotation_status * Project.Computation.t) list ref
       type id
       val id: t -> id
       val name: string
       val from_compute: t -> Project.Computation.t list
       val compute: t -> annotation_status
     end) = 
  struct

    include A

    let get annot = 
      let new_s = A.compute annot in
      (try 
         let s = find_data annot plugin_self in
         s := new_s
       with Not_found ->
         add annot (plugin_self :: A.from_compute annot) (ref new_s));
      new_s

    let () = get_all_ref := (fun a -> get a, plugin_self) :: !get_all_ref

  end

  module CodeAnnotation = Make(CodeAnnotation)
  module Predicate = Make(Predicate)
  module Assigns = Make(Assigns)
  module Complete = Make(Complete)
  module Disjoint = Make(Disjoint)

  module Behavior = 
    Make_Dependent
      (struct 
	 include Behavior

	 let from_compute (kf, st, b) = 
	   let post_states = 
	     List.fold_left
	       (fun acc (_, p) -> 
		  try Predicate.find_state p plugin_self :: acc
		  with Not_found -> acc)
	       []
	       b.b_post_cond
	   in
	   match b.b_assigns with
	   | [] -> post_states
	   | _ :: _ -> 
	       try
		 Assigns.find_state (kf, st, Some b, b.b_assigns) plugin_self
		 :: post_states
		with Not_found ->
		  post_states

         let compute (kf, st, b) = 
	   let post_status =
	     List.map (fun (_, p) -> Predicate.get p) b.b_post_cond
	   in
	   let all_status = match b.b_assigns with
	     | [] -> post_status
	     | _ :: _ -> 
		 Assigns.get (kf, st, Some b, b.b_assigns) :: post_status
	   in
	   weakest_status all_status
       end)

  module type S_ReadOnly = sig
    include S
    val get: t -> annotation_status
  end

  module type S = sig
    include S_ReadOnly
    val set: t -> annotation_status -> unit
    val update: 
      t -> (annotation_status -> annotation_status) -> annotation_status
  end

end

module type Generated = sig
  val get: kernel_function -> bool
  val set: kernel_function -> bool -> unit
  val get_state : kernel_function -> Project.Computation.t
  val self: Project.Computation.t
end


(* Proxy for RTE generation status *)
(* clear the state for a given function => all rte/precond status
   are cleared *)
module RTE_Status_Proxy = struct

  include
    Computation.Dashtbl
      (Globals.Functions.KF_Datatype)
      (Datatype.Unit)
      (struct
	 let size = 97
	 let name = "rte_status_proxy"
	 let dependencies = [ Ast.self ]
       end)

  let get_state kf = 
    match find_all_states kf with
      | [] -> 
	  add kf [] ();
	  (match find_all_states kf with [ s ] -> s | _ -> assert false)
      | [ s ] -> 
	  s
      | _ -> 
	  assert false

end

(* [JS 2010/02/24] should handle dependencies in order to be able to 
   have two different plug-ins which generate RTE *)
(* Table for generation status *)
module GENERATED
  (M:sig 
     val name:string 
     val default: kernel_function -> bool 
   end) = 
struct

  include Computation.Dashtbl
    (Globals.Functions.KF_Datatype)
    (Datatype.Ref(Datatype.Bool))
    (struct 
       let size = 17
       let name = M.name
       let dependencies = [ RTE_Status_Proxy.self ]
     end)

  let get kf =
    let state = RTE_Status_Proxy.get_state kf in
    try !(find_data kf state) 
    with Not_found -> 
      let def = M.default kf in
      add kf [ state ] (ref def);
      def

  let get_state kf = 
    let state = RTE_Status_Proxy.get_state kf in
    try
      find_state kf state 
    with Not_found -> 
      add kf [ state ] (ref (M.default kf));
      try find_state kf state with Not_found -> assert false

  let set kf b = 
    let state = RTE_Status_Proxy.get_state kf in
    try
      let v = find_data kf state in
      v := b
    with Not_found ->
      add kf [ state ] (ref b)

end

(* Tables of RTE generation status. *)
module RTE_Signed_Generated =
  GENERATED
    (struct
       let name = "Signed overflow" 
       let default kf = not (Kernel_function.is_definition kf)
     end)

module RTE_MemAccess_Generated =
  GENERATED
    (struct
       let name = "Mem access" 
       let default kf = not (Kernel_function.is_definition kf)
     end)

module RTE_DivMod_Generated =
  GENERATED
    (struct
       let name = "Div/mod" 
       let default kf = not (Kernel_function.is_definition kf)
     end)

module RTE_DownCast_Generated =
  GENERATED
    (struct
       let name = "Downcast" 
       let default kf = not (Kernel_function.is_definition kf)
     end)

module Called_Precond_Generated =
  GENERATED
    (struct
       let name = "Precondition" 
       let default kf = not (Kernel_function.is_definition kf)
     end)

module M1 = RTE_Signed_Generated 
module M2 = RTE_MemAccess_Generated 
module M3 = RTE_DivMod_Generated 
module M4 = RTE_DownCast_Generated 
module M5 = Called_Precond_Generated
  
let get_all_status () = 
  [ (M1.self, M1.get_state, M1.get);
    (M2.self, M2.get_state, M2.get);
    (M3.self, M3.get_state, M3.get);
    (M4.self, M4.get_state, M4.get);
    (M5.self, M5.get_state, M5.get) ]

type 'a context = { property: 'a;
		    hypothesis: identified_property list}
and identified_property = 
  | IPBlob of Project.Computation.t
  | IPPredicate of Predicate.t context
  | IPAxiom of string
  | IPCodeAnnot of string * Cil_types.code_annotation
  | IPComplete of Complete.t context
  | IPDisjoint of Disjoint.t context
  | IPAssigns of Assigns.t context
  | IPDecrease of (Db_types.kernel_function*kinstr*term variant) context

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
