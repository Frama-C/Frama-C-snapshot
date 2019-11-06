(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Sigs
open Cil_types
open Lang

let dkey = Wp_parameters.register_category "cfg_compiler"
let dumpkey = Wp_parameters.register_category "cfg_compiler_dump"

type mode = [
  | `Tree
  | `Bool_Backward
  | `Bool_Forward
]

module type Cfg =
sig

  module S : Sigma

  module Node : sig
    type t
    module Map : Qed.Idxmap.S with type key = t
    module Set : Qed.Idxset.S with type elt = t
    module Hashtbl : Hashtbl.S with type key = t
    val pp: Format.formatter -> t -> unit
    val create: unit -> t
    val equal: t -> t -> bool
  end

  type node = Node.t

  val node : unit -> node

  module C :
  sig
    type t
    val equal : t -> t -> bool
    val create : S.t -> F.pred -> t
    val get : t -> F.pred
    val reads : t -> S.domain
    val relocate : S.t -> t -> t
  end

  module P :
  sig
    type t
    val pretty : Format.formatter -> t -> unit
    val create : S.t Node.Map.t -> F.pred -> t
    val get: t -> F.pred
    val reads : t -> S.domain Node.Map.t
    val nodes : t -> Node.Set.t
    val relocate : S.t Node.Map.t -> t -> t

    val to_condition: t -> (C.t * Node.t option) option
  end

  module T :
  sig
    type t
    val pretty : Format.formatter -> t -> unit

    (** Bundle an equation with the sigma sequence that created it. *)
    val create : S.t Node.Map.t -> F.term -> t
    val get: t -> F.term
    val reads : t -> S.domain Node.Map.t
    val relocate : S.t Node.Map.t -> t -> t
    val init  : Node.Set.t ->  (S.t Node.Map.t -> F.term) -> t
    val init' : Node.t -> (S.t -> F.term) -> t
  end

  module E : sig
    type t
    val pretty: Format.formatter -> t -> unit
    val create : S.t sequence -> F.pred -> t
    val get : t -> F.pred
    val reads : t -> S.domain
    val writes : t -> S.domain
    val relocate : S.t sequence -> t -> t
  end

  type cfg
  val dump_env: name:string -> cfg -> unit
  val output_dot: out_channel -> ?checks:P.t Bag.t -> cfg -> unit

  val nop : cfg
  val add_tmpnode: node -> cfg
  val concat : cfg -> cfg -> cfg
  val meta : ?stmt:stmt -> ?descr:string -> node -> cfg
  val goto : node -> node -> cfg
  val branch : node -> C.t -> node -> node -> cfg
  val guard : node -> C.t -> node -> cfg
  val guard' : node -> C.t -> node -> cfg
  val either : node -> node list -> cfg
  val implies : node -> (C.t * node) list -> cfg
  val effect : node -> E.t -> node -> cfg
  val assume : P.t -> cfg
  val havoc : node -> effects:node sequence -> node -> cfg

  val compile : ?name:string -> ?mode:mode -> node -> Node.Set.t -> S.domain Node.Map.t ->
    cfg -> F.pred Node.Map.t * S.t Node.Map.t * Conditions.sequence

end

module Cfg (S:Sigma) : Cfg with module S = S =
struct

  module S = S

  module Node : sig
    type t
    module Map : Qed.Idxmap.S with type key = t
    module Set : Qed.Idxset.S with type elt = t
    module Hashtbl : FCHashtbl.S with type key = t
    val tag: t -> int
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val pp: Format.formatter -> t -> unit
    val create: unit -> t
    val node_internal: unit -> t
  end
  = struct
    type t = int
    module I = struct type t = int let id x = x end
    module Map = Qed.Idxmap.Make(I)
    module Set = Qed.Idxset.Make(I)
    module Hashtbl = Datatype.Int.Hashtbl
    let tag = I.id
    let compare = Datatype.Int.compare
    let equal = Datatype.Int.equal
    let pp fmt n =
      if n>=0 then Format.pp_print_int fmt n
      else Format.fprintf fmt "int%i" (-n)

    let node_compter = ref (-1)

    let create () =
      incr node_compter;
      !node_compter

    let node_internal_compter = ref 0

    let node_internal () =
      decr node_internal_compter;
      !node_internal_compter

  end

  let node = Node.create

  let identify sigma ~src ~tgt =
    S.iter2
      (fun _chunk u v ->
         match u,v with
         | Some x , Some y -> F.Subst.add sigma (F.e_var x) (F.e_var y)
         | _ -> ())
      src tgt

  module E = struct
    type t = S.t sequence * F.pred
    let pretty fmt (_seq,p) = Format.fprintf fmt "effect: @[%a@]" F.pp_pred p
    let get : t -> F.pred = snd
    let create seq p = seq,p

    let relocate tgt (src,p) =
      let sigma = Lang.sigma () in
      identify sigma ~src:src.pre ~tgt:tgt.pre ;
      identify sigma ~src:src.post ~tgt:tgt.post ;
      tgt , F.p_subst sigma p

    let reads (seq,_) = S.domain seq.pre
    let writes (seq,_) = S.writes seq
  end

  module C = struct
    type t = S.t * F.pred
    let get = snd
    let create seq p = seq,p
    let relocate tgt (src,p) =
      let sigma = Lang.sigma () in
      identify sigma ~src ~tgt ;
      tgt , F.p_subst sigma p
    let reads (src,_) = S.domain src
    let equal (s1,p1) (s2,p2) =
      let sigma = Lang.sigma () in
      identify sigma ~src:s1 ~tgt:s2 ;
      F.eqp (F.p_subst sigma p1) p2
  end

  module P = struct
    type t = S.t Node.Map.t * F.pred
    let pretty fmt (m,f) =
      Format.fprintf fmt "%a(%a)"
        F.pp_pred f (Pretty_utils.pp_iter2 Node.Map.iter ~between:",@ " Node.pp (fun _ _ -> ())) m
    let get = snd
    let create smap p = smap,p

    let relocate tgt (src,p) =
      let sigma = Lang.sigma () in
      Node.Map.iter2
        (fun n src tgt ->
           match src,tgt with
           | Some src , Some tgt -> identify sigma ~src ~tgt
           | Some _, None ->
               invalid_arg (Format.asprintf "P.relocate: tgt is smaller than src at %a" Node.pp n)
           | _ -> ())
        src tgt ;
      let tgt = Node.Map.inter (fun _ _ tgt -> tgt) src tgt in
      tgt , F.p_subst sigma p

    let reads (smap,_) = Node.Map.map (fun _ s -> S.domain s) smap
    let nodes (smap,_) = Node.Map.fold (fun k _ acc -> Node.Set.add k acc) smap Node.Set.empty
    let nodes_list (smap,_) = Node.Map.fold (fun k _ acc -> k::acc) smap []

    let to_condition (m,p) =
      let l = Node.Map.fold (fun k e acc -> (k,e)::acc) m [] in
      match l with
      | [] -> Some ((S.create (),p), None)
      | [n,s] -> Some ((s,p), Some n)
      | _ -> None
  end

  module T = struct
    type t = S.t Node.Map.t * F.term

    let pretty fmt (m,f) =
      Format.fprintf fmt "%a(%a)"
        F.pp_term f (Pretty_utils.pp_iter2 Node.Map.iter ~between:",@ " Node.pp (fun _ _ -> ())) m

    let get = snd

    let create smap t = smap,t

    let reads (smap,_) = Node.Map.map (fun _ s -> S.domain s) smap

    let relocate tgt (src,p) =
      let sigma = Lang.sigma () in
      Node.Map.iter2
        (fun _ src tgt ->
           match src,tgt with
           | Some src , Some tgt -> identify sigma ~src ~tgt
           | Some _, None -> invalid_arg "T.relocate: tgt is smaller than src"
           | _ -> ())
        src tgt ;
      let tgt = Node.Map.inter (fun _ _ tgt -> tgt) src tgt in
      tgt , F.e_subst sigma p

    let init node_set f =
      let node_map = Node.Set.fold (fun x m ->
          Node.Map.add x (S.create ()) m
        ) node_set Node.Map.empty
      in
      let t = f node_map in
      (node_map,t)

    let init' node f =
      let src = S.create () in
      let t = f src in
      let node_map =
        Node.(Map.add node src Map.empty)
      in
      (node_map,t)
  end

  type node = Node.t

  type without_bindings = Without_Bindings
  type with_bindings = With_Bindings
  let _ = Without_Bindings
  let _ = With_Bindings

  type ('havoc,_) edge =
    | Goto of node
    | Branch of C.t * node option * node option
    | Either of node list
    | Implies of (C.t * node) list
    | Effect of E.t * node
    | Havoc of 'havoc * node
    | Binding : Passive.t * node -> ('havoc,with_bindings) edge
    (** Binding used for sigma merging *)

  type data =
    | Meta of stmt option * string option

  type ('havoc, 'bindings) env = {
    succs : ('havoc, 'bindings) edge Node.Map.t;
    datas : data Bag.t Node.Map.t;
    (* datas is always included in succs *)
    assumes : P.t Bag.t;
    tmpnodes : Node.Set.t; (* node that could be removed *)
  }


  type pre_env = (node sequence, without_bindings) env
  type restricted_env = (S.domain, without_bindings) env
  type localised_env = (S.domain, with_bindings) env

  type cfg = pre_env

  let iter_succs : type a b. (Node.t -> unit) -> (a,b) edge -> unit = fun f -> function
    | Goto n2 | Effect(_,n2) | Havoc(_,n2) -> f n2
    | Branch(_,n2a,n2b) ->
        let f' = function None -> () | Some x -> f x in
        f' n2a; f' n2b
    | Either l -> List.iter f l
    | Implies l -> List.iter (fun (_,a) -> f a) l
    | Binding (_,n2) -> f n2

  let iter_succs_e f cfg n =
    match Node.Map.find n cfg.succs with
    | exception Not_found -> ()
    | e -> iter_succs f e

  let succs : type a b. (a,b) env -> Node.t -> Node.t list =
    fun cfg n ->
    match Node.Map.find n cfg.succs with
    | exception Not_found -> []
    | Goto n2 | Effect(_,n2) | Havoc(_,n2)
    | Branch(_,Some n2,None)
    | Branch(_,None,Some n2) -> [n2]
    | Binding (_,n2) -> [n2]
    | Branch(_,Some n1,Some n2) -> [n1;n2]
    | Branch(_,None,None) -> []
    | Either l -> l
    | Implies l -> List.map snd l

  let pretty_edge : type a. Format.formatter -> (_,a) edge -> unit = fun fmt edge ->
    match edge with
    | Goto(n) -> Format.fprintf fmt "goto(%a)" Node.pp n
    | Branch(c,n1,n2) -> Format.fprintf fmt "branch(%a,%a,%a)"
                           Lang.F.pp_pred (C.get c)
                           (Pretty_utils.pp_opt Node.pp) n1 (Pretty_utils.pp_opt Node.pp) n2
    | Either l -> Format.fprintf fmt "either(%a)" (Pretty_utils.pp_list ~sep:",@ " Node.pp) l
    | Implies l -> Format.fprintf fmt "implies(%a)"
                     (Pretty_utils.pp_list ~sep:",@ " (fun fmt (c,a) ->
                          Format.fprintf fmt "%a=>%a" Lang.F.pp_pred (C.get c) Node.pp a)) l
    | Effect(_,n) -> Format.fprintf fmt "effect(%a)" Node.pp n
    | Havoc(_,n) -> Format.fprintf fmt "havoc(%a)" Node.pp n
    | Binding(_,n) -> Format.fprintf fmt "binding(%a)" Node.pp n

  let pretty_data fmt = function
    | Meta(s_opt,str_opt) ->
        Format.fprintf fmt "Meta(%a,%a)"
          (Pretty_utils.pp_opt ~none:"None" Cil_datatype.Stmt.pretty_sid) s_opt
          (Pretty_utils.pp_opt ~none:"None" Format.pp_print_string) str_opt

  let pretty_env : type a. Format.formatter -> (_,a) env -> unit =
    fun fmt env ->
    Context.bind Lang.F.context_pp (Lang.F.env Lang.F.Vars.empty) (fun () ->
        Format.fprintf fmt
          "@[<v>@[<3>@[succs:@]@ %a@]@,@[<3>@[datas:@]@ %a@]@,@[<3>@[assumes:@]@ %a@]@]@."
          (Pretty_utils.pp_iter2 ~between:"->@," ~sep:",@ " Node.Map.iter Node.pp pretty_edge) env.succs
          (Pretty_utils.pp_iter2 ~between:"->@," ~sep:",@ " Node.Map.iter Node.pp
             (Pretty_utils.pp_iter Bag.iter pretty_data)) env.datas
          (Pretty_utils.pp_iter ~sep:",@ " Bag.iter P.pretty) env.assumes
      ) ()

  let dump_edge : type a. node -> Format.formatter -> (_, a) edge -> unit =
    fun n fmt edge ->
    let pp_edge ?(label="") n' =
      Format.fprintf fmt " %a -> %a [ label=\"%s\" ] ;@." Node.pp n Node.pp n' label
    in
    begin match edge with
      | Goto n1 -> pp_edge n1
      | Branch (_, n1, n2)->
          Extlib.may pp_edge n1;
          Extlib.may pp_edge n2
      | Either ns -> List.iter pp_edge ns
      | Implies ns -> List.iter (fun (_,a) -> pp_edge a) ns
      | Effect (e, n') ->
          pp_edge ~label:(Format.asprintf "%a" E.pretty e) n'
      | Havoc (_, n') -> pp_edge ~label:"havoc" n'
      | Binding (_,n') -> pp_edge ~label:"binding" n'
    end

  let dump_node : data Bag.t -> Format.formatter -> node -> unit =
    fun datas fmt n ->
    Format.fprintf fmt "  %a [ label=\"%a\n%a\" ] ;@."
      Node.pp n Node.pp n (Pretty_utils.pp_iter ~sep:"\n" Bag.iter pretty_data) datas

  let dump_succ : type a. (_, a) env -> Format.formatter -> node -> (_, a) edge -> unit =
    fun env fmt n e ->
    let datas = try Node.Map.find n env.datas with Not_found -> Bag.empty in
    Format.fprintf fmt "%a\n%a@\n" (dump_node datas) n (dump_edge n) e

  let dump_assume : Format.formatter -> P.t -> unit =
    let count = ref 0 in
    fun fmt p ->
      incr count;
      Format.fprintf fmt "  subgraph cluster_%d {@\n" !count;
      Format.fprintf fmt "    color=\"palegreen\";@\n";
      Node.Map.iter
        (fun n _ -> Format.fprintf fmt "    %a;\n" Node.pp n)
        (P.reads p);
      Format.fprintf fmt "    label=\"%a\";" Lang.F.pp_pred (P.get p);
      Format.fprintf fmt "  }@."


  let escape fmt = Pretty_utils.ksfprintf (fun s -> String.escaped s) fmt

  let output_dot : type a b. out_channel -> ?checks:_ -> (a,b) env -> unit =
    fun cout ?(checks=Bag.empty) env ->
    let count = let c = ref max_int in fun () -> decr c; !c in
    let module E = struct
      type t = Graph.Graphviz.DotAttributes.edge list
      let default = []
      let compare x y = assert (x == y); 0
    end
    in
    let module V = struct
      type t =
        | Node of Node.t
        | Assume of int * Lang.F.pred
        | Check of int * Lang.F.pred
        (* todo better saner comparison *)
      let tag = function | Node i -> Node.tag i | Assume (i,_) -> i | Check (i,_) -> i
      let pp fmt = function | Node i -> Node.pp fmt i | Assume (i,_) -> Format.fprintf fmt "ass%i" i
                            | Check (i,_) -> Format.fprintf fmt "chk%i" i
      let equal x y = (tag x) = (tag y)
      let compare x y = Transitioning.Stdlib.compare (tag x) (tag y)
      let hash x = tag x
    end in
    let module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V)(E) in
    let module Dot = Graph.Graphviz.Dot(struct
        let graph_attributes _g = [`Fontname "fixed"]
        let default_vertex_attributes _g = (* [`Shape `Point] *) [`Shape `Circle]
        let vertex_name v = Format.asprintf "cp%a" V.pp  v
        let vertex_attributes  = function
          | V.Node n -> [`Label (escape "%a" Node.pp n)]
          | V.Assume (_,p) -> [`Style `Dashed; `Label (escape "%a" Lang.F.pp_pred p)]
          | V.Check (_,p) -> [`Style `Dotted; `Label (escape "%a" Lang.F.pp_pred p)]
        let get_subgraph _ = None
        let default_edge_attributes _g = []
        let edge_attributes ((_,e,_):G.E.t) : Graph.Graphviz.DotAttributes.edge list = e
        include G
      end) in
    let g = G.create () in
    let add_edge n1 l n2 =  G.add_edge_e g (V.Node n1,l,V.Node n2) in
    let add_edges : type a b. Node.t -> (a,b) edge -> unit = fun n1 -> function
      | Goto n2 -> add_edge n1 [] n2
      | Branch((_,c),n2,n2') ->
          let aux s = function
            | None -> ()
            | Some n -> add_edge n1 [`Label (escape "%s%a" s Lang.F.pp_pred c)] n
          in
          aux "" n2; aux "!" n2'
      | Either l -> List.iter (add_edge n1 []) l
      | Implies l ->
          List.iter (fun (c,n) -> add_edge n1 [`Label (escape "%a" Lang.F.pp_pred (C.get c))] n) l
      | Effect ((_,e),n2) ->
          add_edge n1 [`Label (escape "%a" Lang.F.pp_pred e)] n2
      | Havoc (_,n2) -> add_edge n1 [`Label (escape "havoc")] n2
      | Binding (_,n2) -> add_edge n1 [`Label (escape "binding")] n2
    in
    Node.Map.iter add_edges env.succs;
    (** assumes *)
    Bag.iter (fun (m,p) ->
        let n1 = V.Assume(count (), p) in
        let assume_label = [`Style `Dashed ] in
        Node.Map.iter (fun n2 _ -> G.add_edge_e g (n1,assume_label,V.Node n2)) m
      ) env.assumes;
    (** checks *)
    Bag.iter (fun (m,p) ->
        let n1 = V.Check(count (), p) in
        let label = [`Style `Dotted ] in
        Node.Map.iter (fun n2 _ -> G.add_edge_e g (V.Node n2,label,n1)) m
      ) checks;
    Dot.output_graph cout g

  let dump_env : type a. name:string -> (_, a) env -> unit = fun ~name env ->
    let file = (Filename.get_temp_dir_name ()) ^ "/cfg_" ^ name in
    let fout = open_out (file ^ ".dot") in
    if false then begin
      let out = Format.formatter_of_out_channel fout in
      Format.fprintf out "digraph %s {@\n" name;
      Format.fprintf out "  rankdir = TB ;@\n";
      Format.fprintf out "  node [ style = filled, shape = circle ] ;@\n";
      Node.Map.iter (dump_succ env out) env.succs;
      Bag.iter (dump_assume out) env.assumes;
      Format.fprintf out "}@.";
    end
    else begin
      output_dot fout env;
    end;
    close_out fout;
    ignore (Sys.command
              (Printf.sprintf "dot -Tpdf %s.dot > %s.pdf" file file));
    Wp_parameters.debug ~dkey:dumpkey "Saving dump %s into %s.pdf" name file

  let env_union env1 env2 =
    {
      succs = Node.Map.union
          (fun _ _v1 _v2 -> invalid_arg "A node has more than one successor")
          env1.succs env2.succs;
      datas = Node.Map.union (fun _ -> Bag.concat) env1.datas env2.datas;
      assumes = Bag.concat env1.assumes env2.assumes;
      tmpnodes = Node.Set.union env1.tmpnodes env2.tmpnodes;
    }

  let new_env ?(succs=Node.Map.empty) ?(datas=Node.Map.empty) ?(assumes=Bag.empty)
      ?(tmpnodes=Node.Set.empty) () =
    {succs; datas; assumes; tmpnodes}

  let nop = new_env ()

  let add_tmpnode node = new_env ~tmpnodes:(Node.Set.singleton node) ()

  let concat a b = env_union a b

  let meta ?stmt ?descr n =
    let data = Meta(stmt,descr) in
    new_env ~datas:(Node.Map.add n (Bag.elt data) (Node.Map.empty)) ()

  let edge n e =
    new_env ~succs:(Node.Map.add n e (Node.Map.empty)) ()

  let goto node_orig node_target =
    edge node_orig (Goto(node_target))

  let branch node_orig predicate node_target_then node_target_else =
    edge node_orig (Branch(predicate,
                           Some node_target_then,
                           Some node_target_else))

  let guard node_orig predicate node_target_then =
    edge node_orig (Branch(predicate,
                           Some node_target_then,
                           None))

  let guard' node_orig predicate node_target_else =
    edge node_orig (Branch(predicate,
                           None,
                           Some node_target_else
                          ))

  let either node = function
    | [] -> nop
    | [dest] -> goto node dest
    | node_list -> edge node (Either(node_list))

  let implies node = function
    | [] -> nop
    | [g,dest] -> guard node g dest
    | node_list -> edge node (Implies(node_list))

  let effect node1 e node2 =
    edge node1 (Effect(e, node2))

  let assume (predicate:P.t) =
    if F.is_ptrue (P.get predicate) = Qed.Logic.Yes
    then nop
    else new_env ~assumes:(Bag.elt predicate) ()

  let havoc node1 ~effects:node_seq node2 =
    edge node1 (Havoc(node_seq,node2))

  let option_bind ~f = function
    | None -> None
    | Some x -> f x

  let union_opt_or union d1 d2 =
    match d1, d2 with
    | Some d1, Some d2 -> Some (union d1 d2)
    | (Some  _ as d), None | None, (Some _ as d) -> d
    | None, None -> None

  let union_opt_and union d1 d2 =
    match d1, d2 with
    | Some d1, Some d2 -> Some (union d1 d2)
    | _ -> None

  let add_only_if_alive union d1 = function
    | None -> None
    | Some d2 -> Some (union d1 d2)

  (** return None when post is not accessible from this node *)
  let rec effects : type a.  (_,a) env -> node -> node -> S.domain option =
    fun env post node ->
    if node = post
    then Some S.empty
    else
      match Node.Map.find node env.succs with
      | exception Not_found -> None
      | Goto (node2) ->
          effects env post node2
      | Branch (_, node2, node3) ->
          union_opt_or S.union
            (option_bind ~f:(effects env post) node2)
            (option_bind ~f:(effects env post) node3)
      | Either (l) ->
          (List.fold_left
             (fun acc node2 -> union_opt_or S.union
                 acc (effects env post node2))
             None l)
      | Implies (l) ->
          (List.fold_left
             (fun acc (_,node2) -> union_opt_or S.union
                 acc (effects env post node2))
             None l)
      | Effect (effect , node2) ->
          add_only_if_alive S.union
            (E.writes effect)
            (effects env post node2)
      | Havoc (m, node2) ->
          union_opt_and S.union
            (effects env m.post m.pre)
            (effects env post node2)
      | Binding (_,node2) ->
          effects env post node2

  (** restrict a cfg to the nodes accessible from the pre post given,
      and compute havoc effect *)
  let restrict (cfg:pre_env) pre posts : restricted_env =
    let rec walk acc node : restricted_env option =
      if Node.Map.mem node acc.succs then Some acc
      else
        let new_env edge = new_env ~succs:(Node.Map.add node edge (Node.Map.empty)) () in
        let r = match Node.Map.find node cfg.succs with
          | exception Not_found -> None
          | (Goto (node2) | Effect (_ , node2)) as edge ->
              union_opt_and env_union
                (Some (new_env edge))
                (walk acc node2)
          | Branch (pred, node2, node3) ->
              (** it is important to visit all the childrens *)
              let f acc node =
                match option_bind ~f:(walk acc) node with
                | None -> None, acc
                | Some acc -> node, acc in
              let node2, acc = f acc node2 in
              let node3, acc = f acc node3 in
              if node2 = None && node3 = None then None
              else Some (env_union acc (new_env (Branch(pred, node2, node3))))
          | Either (l) ->
              let acc,l = List.fold_left
                  (fun ((acc,l) as old) node2 ->
                     match walk acc node2 with
                     | None -> old
                     | Some acc -> (acc,node2::l))
                  (acc,[]) l in
              if l = [] then None
              else Some (env_union acc (new_env (Either (List.rev l))))
          | Implies (l) ->
              let acc,l = List.fold_left
                  (fun ((acc,l) as old) ((_,node2) as e) ->
                     match walk acc node2 with
                     | None -> old
                     | Some acc -> (acc,e::l))
                  (acc,[]) l in
              if l = [] then None
              else Some (env_union acc (new_env (Implies (List.rev l))))
          | Havoc (m, node2) ->
              match effects cfg m.post m.pre with
              | None -> None
              | Some eff ->
                  union_opt_and env_union
                    (Some (new_env (Havoc(eff,node2))))
                    (walk acc node2)
        in
        if Node.Set.mem node posts && r = None
        then Some acc
        else r
    in
    match walk (new_env ()) pre with
    | None -> (new_env ())
    | Some acc ->
        { succs = acc.succs;
          datas = Node.Map.inter (fun _ _ v -> v) acc.succs cfg.datas;
          assumes = Bag.filter (fun (seq,_) ->
              Node.Map.subset (fun _ _ _ -> true)
                seq acc.succs) cfg.assumes;
          tmpnodes = cfg.tmpnodes;
        }

  (** succ is decreasing for this order *)
  let topological (type a) (type b) (cfg:(a,b) env) =
    let module G = struct
      type t = (a,b) env
      module V = struct let hash = Hashtbl.hash include Node end
      let iter_vertex f cfg =
        let h = Node.Hashtbl.create 10 in
        let replace n = Node.Hashtbl.replace h n () in
        Node.Map.iter (fun k _ -> replace k; iter_succs_e replace cfg k) cfg.succs;
        Node.Hashtbl.iter (fun k () -> f k) h
      let iter_succ = iter_succs_e
    end in
    let module T = Graph.Topological.Make(G) in
    let h  = Node.Hashtbl.create 10 in
    let h' = Datatype.Int.Hashtbl.create 10 in
    let c = ref (-1) in
    let l = ref [] in
    T.iter (fun n -> l := n::!l; incr c; Node.Hashtbl.add h n !c; Datatype.Int.Hashtbl.add h' !c n) cfg;
    h,h',List.rev !l

  (** topo_list: elements in topological order
      topo_order: post-order mapping
      nb: number of elements *)
  let idoms topo_list topo_order nb ~pred ~is_after =
    let a = Array.make nb (-1) in
    let iter n =
      let first,preds = match pred n with
        | [] -> topo_order n, []
        | f::p -> topo_order f, List.map topo_order p
      in
      let rec find_common n1 n2 =
        if n1 = n2 then n1
        else if is_after n1 n2 then find_common a.(n1) n2
        else find_common n1 a.(n2) in
      let idom = List.fold_left find_common first preds in
      a.(topo_order n) <- idom
    in
    List.iter iter topo_list;
    a

  let find_def ~def x t = try Node.Map.find x t with Not_found -> def

  let rec remove_dumb_gotos (env:restricted_env) : Node.t Node.Map.t * restricted_env =
    let add_map m acc = Node.Map.fold (fun n _ acc -> Node.Set.add n acc) m acc in
    let used_nodes =
      Bag.fold_left (fun acc p -> add_map (P.reads p) acc) Node.Set.empty env.assumes
    in
    let used_nodes = add_map env.datas used_nodes in
    let how_many_preds = Node.Hashtbl.create 10 in
    let incr_how_many_preds n =
      Node.Hashtbl.replace how_many_preds n (succ (Node.Hashtbl.find_def how_many_preds n 0))
    in
    let subst =
      Node.Map.fold (fun n e acc ->
          iter_succs incr_how_many_preds e;
          match (e:(_,without_bindings) edge) with
          | Goto n' when not (Node.Set.mem n used_nodes) ->
              Node.Map.add n n' acc
          | Goto _
          | Branch (_,_,_)
          | Either _
          | Implies _
          | Effect (_,_)
          | Havoc (_,_) -> acc)
        env.succs Node.Map.empty
    in
    let subst =
      let rec compress n =
        match Node.Map.find n subst with
        | exception Not_found -> n
        | n -> compress n
      in
      Node.Map.map (fun _ n' -> compress n') subst
    in
    let find n = find_def ~def:n n subst in
    (** detect either that could be transformed in branch *)
    let to_remove = Node.Hashtbl.create 10 in
    Node.Map.iter (fun _ e ->
        match (e:(_,without_bindings) edge) with
        | Either [a;b] when Node.Hashtbl.find how_many_preds a = 1 &&
                            Node.Hashtbl.find how_many_preds b = 1 &&
                            not (Node.Set.mem a used_nodes) &&
                            not (Node.Set.mem b used_nodes) &&
                            Node.Set.mem a env.tmpnodes &&
                            Node.Set.mem b env.tmpnodes &&
                            true
          ->
            begin
              let find_opt k m =
                match Node.Map.find k m with
                | exception Not_found -> None
                | v -> Some v
              in
              match find_opt a env.succs, find_opt b env.succs with
              | Some Branch(c,Some n1, None), Some Branch(c',None, Some n2)
              | Some Branch(c,None, Some n2), Some Branch(c',Some n1,None) when C.equal c c' ->
                  let n1 = find n1 in
                  let n2 = find n2 in
                  let br = Branch(c,Some n1, Some n2) in
                  Node.Hashtbl.add to_remove a br;
                  Node.Hashtbl.add to_remove b br
              | _ -> ()
            end
        | Goto _
        | Branch (_,_,_)
        | Effect (_,_)
        | Either _
        | Implies _
        | Havoc (_,_) -> ()
      ) env.succs;
    (** substitute and remove *)
    let succs = Node.Map.mapq (fun n e ->
        match (e:(_,without_bindings) edge) with
        | _ when Node.Hashtbl.mem to_remove n -> None
        | Goto _ when not (Node.Set.mem n used_nodes) -> None
        | Goto n' ->
            let n'' = find n' in
            if Node.equal n' n'' then Some e
            else Some (Goto n'')
        | Branch (c,n1,n2) ->
            let n1' = Extlib.opt_map find n1 in
            let n2' = Extlib.opt_map find n2 in
            if Extlib.opt_equal Node.equal n1 n1' && Extlib.opt_equal Node.equal n2 n2'
            then Some e
            else Some (Branch(c,n1',n2'))
        | Either l ->
            let l' = List.map find l in
            let l' = List.sort_uniq Node.compare l' in
            begin match l' with
              | [] -> assert false (* absurd: Either after restricted has at least one successor *)
              | [a] -> Some (Goto a)
              | [a;_] when Node.Hashtbl.mem to_remove a ->
                  let br = Node.Hashtbl.find to_remove a in
                  Some br
              | l' -> Some (Either l')
            end
        | Implies l ->
            let l' = List.map (fun (g,n) -> (g,find n)) l in
            Some (Implies l')
        | Effect (ef,n') ->
            let n'' = find n' in
            if Node.equal n' n'' then Some e
            else Some (Effect(ef,n''))
        | Havoc (h,n') ->
            let n'' = find n' in
            if Node.equal n' n'' then Some e
            else Some (Havoc(h,n''))
      )
        env.succs
    in
    let env = {env with succs} in
    if Node.Map.is_empty subst
    then subst, env
    else
      let subst', env = remove_dumb_gotos env in
      let subst = Node.Map.map (fun _ n' -> find_def ~def:n' n' subst') subst in
      Node.Map.merge (fun _ a b ->
          match a, b with
          | Some _, Some _ -> assert false (** the elements are remove in the new env *)
          | Some x, None | None, Some x -> Some x
          | None, None -> assert false
        ) subst subst', env

  let allocate domain sigma =
    S.Chunk.Set.iter (fun chunk -> ignore (S.get sigma chunk)) domain

  let domains (env : restricted_env) reads pre : localised_env * S.t Node.Map.t =
    let visited = ref Node.Map.empty in
    let new_succs = ref Node.Map.empty in
    let add_edge node edge = new_succs := Node.Map.add node edge !new_succs in
    let add_binding_edge n (p: Passive.t) =
      if Passive.is_empty p then n
      else
        let n' = Node.node_internal () in
        add_edge n' (Binding(p,n));
        n'
    in
    let rec aux node : S.t =
      try Node.Map.find node !visited
      with Not_found ->
        let dom = find_def ~def:S.empty node reads in
        let ret =
          match Node.Map.find node env.succs with
          | exception Not_found ->
              (** posts node *)
              let s1 = S.create () in
              allocate dom s1;
              s1
          | Goto (node2) ->
              let s1 = S.copy (aux node2) in
              allocate dom s1;
              add_edge node (Goto node2);
              s1
          | Branch (pred, node2, node3) ->
              let dom = (S.union (C.reads pred) dom) in
              begin match node2, node3 with
                | (None, Some next) | (Some next, None) ->
                    let s1 = S.copy (aux next) in
                    allocate dom s1;
                    let pred = C.relocate s1 pred in
                    add_edge node (Branch(pred,node2,node3));
                    s1
                | Some node2, Some node3 ->
                    let s2 = aux node2 in
                    let s3 = aux node3 in
                    let s1,p2,p3 = S.merge s2 s3 in
                    allocate dom s1;
                    let node2' = add_binding_edge node2 p2 in
                    let node3' = add_binding_edge node3 p3 in
                    let pred = C.relocate s1 pred in
                    add_edge node (Branch(pred,Some node2',Some node3'));
                    s1
                | _ -> assert false
              end
          | Either (l) ->
              let s1, pl = S.merge_list (List.map aux l) in
              allocate dom s1;
              let l = List.map2 add_binding_edge l pl in
              add_edge node (Either l);
              s1
          | Implies (l) ->
              let dom =
                List.fold_left (fun acc (c,_) -> S.union (C.reads c) acc) dom l
              in
              let s1, pl = S.merge_list (List.map (fun (_,n) -> aux n) l) in
              allocate dom s1;
              let l = List.map2 (fun (c,a) b ->
                  let a = add_binding_edge a b in
                  let c = C.relocate s1 c in
                  (c,a)) l pl
              in
              add_edge node (Implies l);
              s1
          | Effect (effect , node2) ->
              let s2 = aux node2 in
              let s1 = S.remove_chunks s2 (E.writes effect) in
              allocate dom s1;
              allocate (E.reads effect) s1;
              let effect = E.relocate {pre=s1;post=s2} effect in
              add_edge node (Effect(effect,node2));
              s1
          | Havoc (eff, node2) ->
              let s2 = aux node2 in
              let s1 = S.havoc s2 eff in
              allocate dom s1;
              add_edge node (Havoc(eff,node2));
              s1
        in
        visited := Node.Map.add node ret !visited;
        ret
    in
    ignore (aux pre);
    let sigmas = !visited in
    let new_env =
      {succs = !new_succs;
       datas = env.datas;
       assumes = Bag.map (fun e -> P.relocate sigmas e) env.assumes;
       tmpnodes = env.tmpnodes;
      } in
    new_env, sigmas

  let compute_preds env =
    let h = Node.Hashtbl.create 10 in
    let add = Node.Hashtbl.add h in
    Node.Map.iter (fun n s ->
        match s with
        | Goto n1 | Havoc (_, n1) | Effect (_,n1) | Binding (_,n1) -> add n1 n
        | Branch (_,Some n1,Some n2) ->
            add n1 n;
            add n2 n
        | Branch(_,Some n1,None) -> add n1 n
        | Branch(_,None,Some n1) -> add n1 n
        | Branch(_,None,None) -> ()
        | Either l -> List.iter (fun n1 -> add n1 n) l
        | Implies l -> List.iter (fun (_,n1) -> add n1 n) l
      ) env.succs;
    h

  let to_sequence_bool ~mode pre posts env : Conditions.sequence * F.pred Node.Map.t =
    let preds = Node.Hashtbl.create 10 in
    let access n = Node.Hashtbl.memo preds n
        (fun _ ->
           let v = F.fresh ~basename:"node"
               (get_pool ()) Qed.Logic.Bool in
           F.p_bool (F.e_var v))
    in
    let (!.) c = (Conditions.sequence [Conditions.step c]) in
    let have_access n = !. (Conditions.Have (access n)) in
    let add_cond ?descr ?stmt f cond =
      Conditions.append (Conditions.sequence [Conditions.step ?descr ?stmt cond]) f
    in
    let either = function
      | [] -> !. (Conditions.Have F.p_false)
      | [a] -> a
      | l -> !. (Conditions.Either l)
    in
    let f = Conditions.empty in
    (** The start state is accessible *)
    let pre = Conditions.Have (access pre) in
    let f = add_cond f pre in
    (** The posts state are accessible *)
    let f = Node.Set.fold
        (fun n f -> add_cond f (Conditions.Have (access n)))
        posts f in
    (** The assumes are true if all their nodes are accessible *)
    let f =
      Bag.fold_left (fun f p ->
          let nodes_are_accessible =
            Node.Map.fold (fun n _ acc -> F.p_and (access n) acc)
              (P.reads p) F.p_true in
          let f' = F.p_imply nodes_are_accessible (P.get p) in
          add_cond f (Conditions.Have f')
        ) f env.assumes in

    (** compute predecessors *)
    let to_sequence_basic_backward f =
      let predecessors = Node.Map.fold (fun n s acc ->
          let add acc n' p =
            Node.Map.change (fun _ (n,p) -> function
                | None -> Some (Node.Map.add n p Node.Map.empty)
                | Some s -> Some (Node.Map.add n p s)) n' (n,p) acc
          in
          match s with
          | Goto n' | Havoc (_, n') -> add acc n' F.p_true
          | Branch (c,Some n1,Some n2) ->
              let c = P.get c in
              add (add acc n1 c) n2 (F.p_not c)
          | Branch(c,Some n1,None) -> add acc n1 (P.get c)
          | Branch(c,None,Some n1) -> add acc n1 (F.p_not (P.get c))
          | Branch(_,None,None) -> acc
          | Either l -> List.fold_left (fun acc e -> add acc e F.p_true) acc l
          | Implies l -> List.fold_left (fun acc (c,e) -> add acc e (P.get c)) acc l
          | Effect (e,n') -> add acc n' (E.get e)
          | Binding (b,n') ->
              let b = F.p_conj (Passive.conditions b (fun _ -> true)) in
              add acc n' b
        ) env.succs Node.Map.empty
      in
      Node.Map.fold (fun n' preds f ->
          let l = Node.Map.fold (fun n p acc ->
              (Conditions.append (have_access n) (!. (Conditions.Have p)))::acc
            ) preds [] in
          let f' =
            Conditions.Branch(access n', either l, Conditions.empty)
          in
          let stmt,descr =
            let bag = match Node.Map.find n' env.datas with
              | exception Not_found -> Bag.empty
              | bag -> bag
            in
            Bag.fold_left (fun (os,od) b ->
                match b with
                | Meta(os',od') ->
                    (if os = None then os' else os),
                    (if od = None then od' else od)
              ) (None,None) bag in
          add_cond ?stmt ?descr f f'
        ) predecessors f
    in

    (** The transitions *)
    let to_sequence_basic_forward f =
      Node.Map.fold (fun n s f ->
          let node_is_accessible = access n in
          let f' = match s with
            | Goto n' | Havoc (_, n') ->
                (* The havoc is already taken into account during {!domains} *)
                Conditions.Branch(node_is_accessible,
                                  have_access n',
                                  Conditions.empty)
            | Branch (c,Some n1,Some n2) ->
                Conditions.Branch(node_is_accessible,
                                  !. (Conditions.Branch((C.get c),
                                                        have_access n1,
                                                        have_access n2)),
                                  Conditions.empty)
            | Branch(c,Some n1,None) ->
                Conditions.Branch(node_is_accessible,
                                  Conditions.append
                                    (!. (Conditions.Have (C.get c)))
                                    (have_access n1),
                                  Conditions.empty)
            | Branch(c,_,Some n1) ->
                Conditions.Branch(node_is_accessible,
                                  Conditions.append
                                    (!. (Conditions.Have (F.p_not (C.get c))))
                                    (have_access n1),
                                  Conditions.empty)
            | Branch(_,None,None) -> assert false
            | Either l ->
                let l = List.map have_access l in
                Conditions.Branch(node_is_accessible, either l, Conditions.empty)
            | Implies l ->
                let l = List.map
                    (fun (c,n) -> !. (Conditions.Branch (C.get c, have_access n, Conditions.empty)))
                    l in
                Conditions.Branch(node_is_accessible, Conditions.concat l, Conditions.empty)
            | Effect (e,n) ->
                Conditions.Branch(node_is_accessible,
                                  Conditions.append
                                    (!. (Conditions.Have (E.get e)))
                                    (have_access n) ,
                                  Conditions.empty)
            | Binding (b,n') ->
                (** For basic: all the variables are important *)
                let b = !. (Conditions.Have(F.p_conj (Passive.conditions b (fun _ -> true)))) in
                Conditions.Branch(node_is_accessible,
                                  Conditions.append b (have_access n'),
                                  Conditions.empty) in
          let stmt,descr =
            let bag = match Node.Map.find n env.datas with
              | exception Not_found -> Bag.empty
              | bag -> bag
            in
            Bag.fold_left (fun (os,od) b ->
                match b with
                | Meta(os',od') ->
                    (if os = None then os' else os),
                    (if od = None then od' else od)
              ) (None,None) bag in
          add_cond ?stmt ?descr f f'
        ) env.succs f
    in

    let f = match mode with
      | `Bool_Backward -> to_sequence_basic_backward f
      | `Bool_Forward -> to_sequence_basic_forward f
    in
    f,Node.Hashtbl.fold Node.Map.add preds Node.Map.empty

  module To_tree = struct
    (** Use a simplified version of "A New Elimination-Based Data Flow Analysis
        Framework Using Annotated Decomposition Trees" where there is no loop *)


    type tree = {
      c : F.pred (** condition for this tree *) ;
      q : tree Queue.t (** childrens *) ;
      mutable fact : F.pred list (** facts at this level *) ;
    }

    [@@@ warning "-32"]

    let rec pp_tree
        ?(pad : (string * string)= ("", ""))
        (tree : tree) : unit =
      let pd, pc = pad in
      Format.printf "%sNode condition: %a @." pd Lang.F.pp_pred tree.c;
      Format.printf "%sNode fact:%a@."
        pd
        (Pretty_utils.pp_list ~sep:"," ~pre:"[" ~suf:"]" Lang.F.pp_pred) tree.fact;
      let n = Queue.length tree.q - 1 in
      let _ =
        Queue.fold (
          fun i c ->
            let pad =
              (pc ^ (if i = n then "`-- " else "|-- "),
               pc ^ (if i = n then "    " else "|   "))
            in
            pp_tree ~pad c;
            i+1
        ) 0 tree.q
      in ()

    let pp_idoms fmt a =
      Pretty_utils.pp_array ~sep:";@ " (fun fmt i j -> Format.fprintf fmt "%i -> %i" i j)
        fmt a

    [@@@ warning "+32"]

    type env_to_sequence_tree = {
      env: localised_env;
      (** predecessors *)
      pred: Node.t -> Node.t list;
      (** topological order *)
      topo_order : Node.t -> int;
      (** Immediate dominator forward *)
      get_idom_forward: Node.t -> Node.t;
      (** Immediate dominator backward *)
      get_idom_backward: int -> int;

      (** For each node we are going to compute different formulas *)
      (** Necessary conditions of the node from start *)
      full_conds: Lang.F.pred Node.Hashtbl.t;
      (** Necessary conditions from its forward idiom *)
      conds: Lang.F.pred Node.Hashtbl.t;
      (** To which subtree corresponds this node *)
      subtrees: tree Node.Hashtbl.t;
      (** Root the full tree *)
      root: tree;
      (** Variable used for the non-deterministic choice of either *)
      eithers: Lang.F.pred Node.Hashtbl.t Node.Hashtbl.t;
    }

    let is_after n1 n2 = n1 > n2
    let is_before n1 n2 = n1 < n2

    let create_env_to_sequence_tree env =
      (** Compute topological order for immediate dominator computation
          and the main iteration on nodes
      *)
      let node_int,int_node,ordered = topological env in
      let nb = Node.Hashtbl.length node_int in

      (** We compute the forward immediate dominators (path that use succ)
          and the backward immediate dominators (path that use pred)
      *)
      let predecessors = compute_preds env in
      let pred n = Node.Hashtbl.find_all predecessors n in
      let succ n = succs env n in
      let topo_order = Node.Hashtbl.find node_int in
      let idoms_forward =
        idoms ordered topo_order nb ~pred ~is_after in
      let idoms_backward =
        idoms (List.rev ordered) topo_order nb ~pred:succ ~is_after:is_before in
      let get_idom_forward n =
        Datatype.Int.Hashtbl.find int_node idoms_forward.(topo_order n) in
      (* Format.printf "@[ordered: %a@]@." (Pretty_utils.pp_list ~sep:"@ " (fun fmt n -> Format.fprintf fmt "%a (%i)" Node.pp n (Node.Hashtbl.find node_int n))) ordered;
       * Format.printf "@[pred: %a@]@."
       *   (Pretty_utils.pp_iter2 ~sep:"@ " ~between:":" Node.Hashtbl.iter Node.pp Node.pp) predecessors;
       * Format.printf "@[idoms forward: @[@[%a@]@]@." _pp_idoms idoms_forward;
       * Format.printf "@[idoms backward: @[@[%a@]@]@." _pp_idoms idoms_backward; *)
      {
        env;
        pred;
        topo_order;
        get_idom_forward;
        get_idom_backward = (fun i -> idoms_backward.(i));
        full_conds = Node.Hashtbl.create 10;
        conds = Node.Hashtbl.create 10;
        subtrees = Node.Hashtbl.create 10;
        root = {c = Lang.F.p_true; q = Queue.create (); fact = [] };
        eithers = Node.Hashtbl.create 10;
      }, ordered


    let either env n last =
      let h = Node.Hashtbl.memo env.eithers n (fun _ -> Node.Hashtbl.create 10) in
      Node.Hashtbl.memo h last (fun _ ->
          let v = F.fresh ~basename:"node"
              (get_pool ()) Qed.Logic.Bool in
          F.p_bool (F.e_var v)
        )

    (** For a node n *)
    let iter env n =
      let idom = env.get_idom_forward n in
      let rec get_cond acc n' =
        if n' = idom then acc
        else
          let acc = F.p_and acc (Node.Hashtbl.find env.conds n') in
          get_cond acc (env.get_idom_forward n')
      in
      (** find all the conditions that keep the path toward n, i.e.
          the condition of the nodes that are not dominated backwardly
          (for which not all the nodes goes to n)
      *)
      let rec find_frontiere last acc n' =
        if (env.topo_order n) <=  env.get_idom_backward (env.topo_order n')
        then
          let cond = get_cond F.p_true n' in
          let branch = match Node.Map.find n' env.env.succs with
            | exception Not_found -> F.p_true
            | Goto _ | Havoc (_, _) -> F.p_true
            | Branch (c,Some n'',Some _) when Node.equal n'' last -> C.get c
            | Branch (c,Some _,Some n'') when Node.equal n'' last -> F.p_not (C.get c)
            | Branch (_,_,_) -> F.p_true
            | Either _ -> either env n' last
            | Implies l -> List.fold_left (fun acc (c,n) ->
                if n = last then C.get c
                else acc) F.p_true l
            | Effect (_,_) -> F.p_true
            | Binding (_,_) -> F.p_true
          in
          F.p_or acc (F.p_and branch cond)
        else
          List.fold_left (find_frontiere n') acc (env.pred n')
      in
      let c, q =
        if Node.equal idom n
        then
          (** it is the root *)
          begin
            Node.Hashtbl.add env.full_conds n F.p_true;
            Node.Hashtbl.add env.conds n F.p_true;
            F.p_true, env.root.q
          end
        else
          let c = List.fold_left (find_frontiere n) F.p_false (env.pred n) in
          (* Format.printf "for %a c=%a@." Node.pp n Lang.F.pp_pred c; *)
          Node.Hashtbl.add env.conds n c;
          Node.Hashtbl.add env.full_conds n (F.p_and c (Node.Hashtbl.find env.full_conds idom));
          let p = Node.Hashtbl.find env.subtrees idom in
          c,p.q
      in
      let fact = match Node.Map.find n env.env.succs with
        | exception Not_found -> F.p_true
        | Goto _ | Havoc (_, _) -> F.p_true
        | Branch (c,Some _,None) -> C.get c
        | Branch (c,None,Some _) -> F.p_not (C.get c)
        | Branch (_,_,_) -> F.p_true
        | Either _ -> F.p_true
        | Implies _ -> F.p_true
        | Effect (e,_) -> E.get e
        | Binding (b,_) -> F.p_conj (Passive.conditions b (fun _ -> true))
      in
      (* Format.printf "Here: For %a idoms=%a c=%a fact=%a@." Node.pp n Node.pp idom F.pp_pred c F.pp_pred fact; *)
      let t = {c = c; q = Queue.create (); fact = [fact]} in
      Queue.push t q;
      Node.Hashtbl.add env.subtrees n t

    let add_cond ?descr ?stmt f cond =
      match cond with
      | Conditions.Have c when F.is_ptrue c = Qed.Logic.Yes -> f
      | _ ->
          Conditions.append (Conditions.sequence [Conditions.step ?descr ?stmt cond]) f

    let access env n = Node.Hashtbl.find env.full_conds n

    let get_latest_node env  = function
      | [] -> env.root
      | a::l ->
          let n = List.fold_left (fun a e ->
              if env.topo_order a < env.topo_order e then e else a
            ) a l in
          Node.Hashtbl.find env.subtrees n

    (** Add each assume to the sub-tree corresponding to the latest
        node it uses. The assumes are true if all their nodes are
        accessible *)
    let add_assumes_fact env = Bag.iter (fun p ->
        let nodes = P.nodes_list p in
        let nodes_are_accessible =
          (** TODO: don't add the condition of access of the node that are dominators of latest *)
          List.fold_left (fun acc n -> F.p_and (access env n) acc) F.p_true nodes in
        let f' = F.p_imply nodes_are_accessible (P.get p) in
        let t = get_latest_node env nodes in
        t.fact <- f' :: t.fact
      ) env.env.assumes

    (** convert the tree to formula *)
    let rec convert t f =
      let f' =
        if t.fact = []
        then Conditions.empty
        else List.fold_left (fun acc e -> add_cond acc (Conditions.Have e)) Conditions.empty t.fact in
      let f' = Queue.fold (fun f' t -> convert t f') f' t.q in
      match F.is_ptrue t.c with
      | Qed.Logic.Yes -> Conditions.concat [f;f']
      | Qed.Logic.No -> f
      | Qed.Logic.Maybe ->
          add_cond f (Conditions.Branch(t.c,f',Conditions.empty))

    let to_sequence_tree _ posts env =
      let env,ordered = create_env_to_sequence_tree env in
      (** Iterate in topo order the vertex.
          Except for root, the tree of the vertex is the one of its immediate dominator forward.
      *)
      List.iter (iter env) ordered;
      let f = Conditions.empty in
      (** The posts state are accessible *)
      let f = Node.Set.fold
          (fun n f -> add_cond f (Conditions.Have (access env n)))
          posts f in
      (** For all either one of the condition is true *)
      let f = Node.Hashtbl.fold (fun _ h f ->
          let p = Node.Hashtbl.fold (fun _ t p -> F.p_or p t) h F.p_false in
          add_cond f (Conditions.Have p)
        ) env.eithers f in
      add_assumes_fact env;
      let f = convert env.root f in
      f, Node.Hashtbl.fold Node.Map.add env.full_conds Node.Map.empty
  end

  let compile : ?name:string -> ?mode:mode -> node -> Node.Set.t -> S.domain Node.Map.t ->
    cfg -> F.pred Node.Map.t * S.t Node.Map.t * Conditions.sequence =
    fun ?(name="cfg") ?(mode=`Bool_Forward) pre posts user_reads env ->
    if Wp_parameters.has_dkey dkey then
      Format.printf "@[0) pre:%a post:%a@]@."
        Node.pp pre (Pretty_utils.pp_iter ~sep:"@ " Node.Set.iter Node.pp) posts;
    if Wp_parameters.has_dkey dkey then
      Format.printf "@[1) %a@]@." pretty_env env;
    (** restrict environment to useful node and compute havoc effects *)
    let env = restrict env pre posts in
    if Wp_parameters.has_dkey dkey then
      Format.printf "@[2) %a@]@." pretty_env env;
    if Node.Map.is_empty env.succs then
      Node.Map.empty,Node.Map.empty,
      Conditions.sequence [Conditions.step (Conditions.Have(F.p_false))]
    else
      (** Simplify *)
      let subst,env =
        if true
        then remove_dumb_gotos env
        else Node.Map.empty, env
      in
      let pre = find_def ~def:pre pre subst in
      (** Substitute in user_reads *)
      let user_reads =
        Node.Map.fold
          (fun n n' acc ->
             match Node.Map.find n user_reads with
             | exception Not_found -> acc
             | domain ->
                 let domain' =
                   try
                     S.union (Node.Map.find n' acc) domain
                   with Not_found -> domain
                 in
                 Node.Map.add n' domain' acc)
          subst user_reads
      in
      (** For each node what must be read for assumes *)
      let reads =
        Bag.fold_left (fun acc e ->
            Node.Map.union
              (fun _ -> S.union) acc
              (P.reads e))
          user_reads env.assumes in
      (** compute sigmas and relocate them *)
      let env, sigmas = domains env reads pre in
      if Wp_parameters.has_dkey dkey then
        Format.printf "@[3) %a@]@." pretty_env env;
      if Wp_parameters.has_dkey dumpkey then
        dump_env ~name env;
      let f, preds =
        match mode with
        | `Tree ->
            (** Add a unique post node *)
            let final_node = node () in
            let env =
              Node.Set.fold (fun p cfg ->
                  let s = {pre=S.create();post=S.create()} in
                  let e =  s,Lang.F.p_true in
                  let goto = effect p e final_node in
                  concat goto cfg
                ) posts env
            in
            To_tree.to_sequence_tree pre posts env
        | (`Bool_Backward | `Bool_Forward) as mode ->
            to_sequence_bool ~mode pre posts env in
      let predssigmas =
        Node.Map.merge
          (fun _ p s -> Some (Extlib.opt_conv F.p_false p, Extlib.opt_conv (S.create ()) s))
          preds sigmas in
      (** readd simplified nodes *)
      let predssigmas =
        Node.Map.fold (fun n n' acc -> Node.Map.add n (Node.Map.find n' predssigmas) acc )
          subst predssigmas
      in
      let preds =
        Node.Map.map(fun _ (x,_) -> x) predssigmas
      in
      let sigmas =
        Node.Map.map(fun _ (_,x) -> x) predssigmas
      in
      preds,sigmas,f

end
