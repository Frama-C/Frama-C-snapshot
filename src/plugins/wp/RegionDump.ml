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

module Wp = Wp_parameters
module Kf = Kernel_function
module G = Dotgraph
module R = G.Node(Region.Map)

let node_default = [`Attr("fontname","monospace")]
let edge_default = [`Attr("fontname","monospace")]

let attr_offset = [ `Filled ; `Color "grey" ; `Box ]
let attr_write = [ `Label "W" ; `Fillcolor "green" ; `Filled ]
let attr_read  = [ `Label "R" ; `Fillcolor "green" ; `Filled ]
let attr_alias = [ `Label "&" ; `Fillcolor "orange" ; `Filled ]
let attr_merge = [ `Color "red" ; `Fillcolor "red" ; `Filled ]
let attr_shift = [ `Label "[]" ]
let attr_delta = [ `Filled ; `Color "lightblue" ; `Box ]
let attr_deref = [ `ArrowHead "tee" ]
let attr_cil = [ `Filled ; `Fillcolor "yellow" ]
let attr_region = `Shape "tab" :: attr_cil
let attr_var = `Shape "cds" :: attr_cil
let attr_garbled = [`Fillcolor "red";`Filled]
let attr_froms = [ `Color "blue" ; `Attr("dir","back") ]

let attr_pointed = [
  `Color "red"
]

let attr_pointed_deref = [
  `Attr("taillabel","*");
  `Attr("labelangle","+30");
  `Color "red";
]

let attr_pointed_shift = [
  `Attr("taillabel","[..]");
  `Attr("labeldistance","1.7");
  `Attr("labelangle","+40");
  `Color "red";
]

let rid_key = Wp.register_category "rid"
let dot_key = Wp.register_category "dot"
let pdf_key = Wp.register_category "pdf"
let deref_key = Wp.register_category "deref"
let roots_key = Wp.register_category "roots"
let froms_key = Wp.register_category "froms"
let cluster_key = Wp.register_category "cluster"
let chunk_key = Wp.register_category "chunk"
let offset_key = Wp.register_category "offset"

let sfprintf = Pretty_utils.sfprintf

let dotpointed ~label r =
  let attr =
    if Region.is_shifted r
    then attr_pointed_shift else attr_pointed_deref in
  let target = G.port (R.get r) "w" in
  `Port ("",["",attr,target],label)

let dotvalue ?(prefix="") value : Dotgraph.record =
  let open Layout in
  match value with
  | Int i -> `Label (sfprintf "%s%a" prefix Ctypes.pp_int i)
  | Float f -> `Label (sfprintf "%s%a" prefix Ctypes.pp_float f)
  | Pointer r -> dotpointed ~label:(prefix ^ "ptr") r

let dotrange ?(prefix="") rg : Dotgraph.record =
  let open Layout in
  let pp_dim fmt = function
    | Raw _ -> Format.pp_print_string fmt "raw"
    | Dim(s,ds) -> Format.fprintf fmt "%d%a" s Matrix.pretty ds
  in
  let label = sfprintf "%d..%d: %s%a"
      rg.ofs (rg.ofs + rg.len - 1)
      prefix pp_dim rg.dim in
  `Port("",["",[`Dotted],R.get rg.reg],label)

let dotcluster cluster : Dotgraph.record =
  let open Layout in
  match cluster with
  | Empty -> `Label "-"
  | Garbled -> `Label "Garbled"
  | Chunk v -> dotvalue v
  | Layout { sizeof ; layout } ->
      let label = Printf.sprintf "sizeof:%d" sizeof in
      `Hbox (`Label label :: List.map dotrange layout)

let dotchunk mem : Dotgraph.record =
  let open Layout in
  match mem with
  | Mraw(_,None) -> `Label "Raw"
  | Mraw(_,Some r) -> dotpointed ~label:"Raw" r
  | Mref r -> dotpointed ~label:"Ref" r
  | Mmem(rt,v) ->
      let prefix = if Layout.Root.indexed rt then "Mem " else "Var " in
      dotvalue ~prefix v
  | Mcomp(_,ovl) ->
      let range rg = dotrange
          ~prefix:(if Overlay.once rg.reg ovl then "D" else "C") rg in
      `Hbox (List.map range ovl)

let dotregion dot map region node =
  begin
    let is_read = Region.is_read region in
    let is_written = Region.is_written region in
    let is_aliased = Region.is_aliased region in
    let is_accessed = is_read || is_written || is_aliased in
    let has_deref = Wp.has_dkey deref_key && Region.has_deref region in
    let has_roots = Wp.has_dkey roots_key && Region.has_roots map region in
    let has_index_infos = has_deref || has_roots in
    let has_side_cluster =
      is_accessed ||
      has_index_infos ||
      Region.has_names region ||
      Wp.has_dkey offset_key ||
      Wp.has_dkey rid_key ||
      not (Wp.has_dkey cluster_key || Wp.has_dkey chunk_key) ||
      not (Wp.Region_fixpoint.get ())
    in
    if has_side_cluster then
      begin
        let attr = G.decorate [ `Oval ] [
            is_read , attr_read ;
            Region.has_pointed region , [ `Label "D" ] ;
            is_written , attr_write ;
            Region.is_shifted region , attr_shift ;
            is_aliased , attr_alias ;
            Region.get_alias map region != region , attr_merge ;
            Region.is_garbled region , attr_merge ;
          ] in
        G.node dot node attr ;
      end ;
    if Wp.has_dkey offset_key then
      Region.iter_offset map
        (fun offset target ->
           let label = Pretty_utils.to_string Layout.Offset.pretty offset in
           let delta = G.inode dot (`Label label :: attr_offset) in
           G.link dot [node;delta;R.get target] [`Dotted]
        ) region ;
    if Wp.has_dkey offset_key then
      Extlib.may
        (fun target ->
           let label = if Region.is_shifted target then "[..]" else "*" in
           let deref = G.inode dot (`Label label :: attr_offset) in
           G.link dot [node;deref;R.get target] attr_pointed
        ) (Region.get_pointed map region) ;
    if has_index_infos then
      begin
        let derefs = ref [] in
        let label s = derefs := s :: !derefs in
        if has_roots then
          label @@ sfprintf "roots:%a"
            Layout.Root.pretty (Region.get_roots map region) ;
        if has_deref then
          Region.iter_deref
            (fun deref ->
               label @@ Pretty_utils.to_string Layout.Deref.pretty deref
            ) region ;
        if !derefs <> [] then
          begin
            let label = String.concat "\n" (List.rev !derefs) in
            let delta = G.inode dot (`Label label :: attr_delta) in
            G.rank dot [node;delta] ;
            G.edge dot delta node attr_deref
          end
      end ;
    if Wp.has_dkey cluster_key then
      begin
        let cluster = Region.cluster map region in
        if not (has_side_cluster && Layout.Cluster.is_empty cluster) then
          let record = dotcluster cluster in
          let attr = if Region.is_garbled region then attr_garbled else [] in
          if has_side_cluster then
            let delta = G.irecord dot ~attr record in
            G.edge dot node (G.port delta "w") attr_deref
          else
            G.record dot node ~attr record
      end ;
    if Wp.has_dkey chunk_key then
      begin
        let chunk = Region.chunk map region in
        let record = dotchunk chunk in
        let attr = if Region.is_garbled region then attr_garbled else [] in
        if has_side_cluster then
          let delta = G.irecord dot ~attr record in
          G.edge dot node (G.port delta "w") attr_deref
        else
          G.record dot node ~attr record
      end ;
    if Wp.has_dkey froms_key then
      begin
        let open Layout in
        List.iter
          (function
            | Fvar _ -> ()
            | Farray r ->
                G.edge dot (R.get r) node (`Label "[]"::attr_froms)
            | Fderef r ->
                G.edge dot (R.get r) node (`Label "*"::attr_froms)
            | Findex r ->
                G.edge dot (R.get r) node (`Label "+(..)"::attr_froms)
            | Ffield(r,ofs) ->
                let label = Printf.sprintf "+%d" ofs in
                G.edge dot (R.get r) node (`Label label::attr_froms)
          ) (Region.get_froms map region)
      end ;
    Region.iter_copies map
      (fun target ->
         G.edge dot node (R.get target) [`Color "green"]
      ) region ;
    Extlib.may
      (fun target ->
         G.edge dot node (R.get target) [`Color "red"]
      ) (Region.get_merged map region) ;
  end

let dotvar dot x r =
  begin
    let open Cil_types in
    let xnode = G.inode dot ~prefix:"V" (`Label x.vname :: attr_var) in
    G.edge dot (G.port xnode "e") (R.get r) [] ;
  end

let dotlabel dot a r =
  begin
    let anode = G.inode dot ~prefix:"R" (`Label a :: attr_region) in
    let rnode = R.get r in
    G.rank dot [ anode ; rnode ] ;
    G.edge dot anode rnode []
  end

let dotrid dot r =
  dotlabel dot (Pretty_utils.to_string Region.R.pretty r) r

let dotstr dot r cst =
  dotlabel dot (String.escaped cst) r

let dotgraph dot map =
  begin
    G.node_default dot node_default ;
    G.edge_default dot edge_default ;
    R.clear () ;
    R.push dot (dotregion dot map) ;
    Region.iter_vars map (dotvar dot) ;
    Region.iter_strings map (dotstr dot) ;
    G.pop_all dot ;
    if Wp.has_dkey rid_key then Region.iter map (dotrid dot) ;
    Region.iter_names map (dotlabel dot) ;
    if Region.has_return map then
      dotlabel dot "\\result" (Region.of_return map) ;
    Region.iter_fusion map (fun i r ->
        let rid = Region.id r in
        if i <> rid then
          dotlabel dot (Printf.sprintf "Fusion R%03d" i) r
        else
          dotlabel dot "Fusion (Self)" r
      ) ;
    G.pop_all dot ;
  end

let dump ~dir kf map =
  if Wp.has_dkey dot_key || Wp.has_dkey pdf_key then
    begin
      let name = Kf.get_name kf in
      let file = Printf.sprintf "%s/%s.dot" dir name in
      let dot = Dotgraph.open_dot ~attr:[`LR] ~name ~file () in
      dotgraph dot map ;
      Dotgraph.close dot ;
      let outcome =
        if Wp.has_dkey pdf_key
        then Dotgraph.layout dot
        else file in
      Wp.result "Region Graph: %s" outcome
    end
