(* $Id: register.ml,v 1.11 2008/04/14 13:28:09 uid528 Exp $ *)

open Cil_types
open Cilutil
open Cil

module Occurrences: sig
  val add: varinfo -> kinstr -> lval -> unit
  val get: varinfo -> (kinstr * lval) list
  val self: Project.Computation.t
  val iter: (varinfo -> (kinstr * lval) list -> unit) -> unit
end = struct

  module M =
    Kernel_computation.VarinfoHashtbl
      (Datatype.Couple(Kernel_datatype.Kinstr)(Kernel_datatype.Lval))
      (struct
	 let size = 17
	 let name = Project.Computation.Name.make "occurrences"
	 let dependencies = [ Db.Value.self ]
       end)

  let add vi ki lv = M.add vi (ki, lv)
  let get vi = try M.find_all vi with Not_found -> []

  let iter f =
    let old, l =
      M.fold
	(fun v elt (old, l) -> match v, old with
	 | v, None ->
	     assert (l = []);
	     Some v, [ elt ]
	 | v, (Some old as some) when VarinfoComparable.equal v old ->
	     some, elt :: l
	 | v, Some old ->
	     f old l;
	     Some v, [ elt ])
	(None, [])
    in
    Extlib.may (fun v -> f v l) old

  let self = M.self

end

class occurrence = object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (inplace_visit ()) as super

  val mutable decls = []

  method private current_ki =
    match self#current_stmt with None -> Kglobal | Some s -> Kstmt s

  method vvdec vi =
    let ki = self#current_ki in
    if Db.Value.is_accessible ki then begin
      let z =
	!Db.Value.lval_to_zone
	  ki ~with_alarms:CilE.warn_none_mode (Var vi, NoOffset)
      in
      decls <-  (vi, z) :: decls
    end;
    DoChildren

  method vlval lv =
    let ki = self#current_ki in
    if Db.Value.is_accessible ki then begin
      let z = !Db.Value.lval_to_zone ki ~with_alarms:CilE.warn_none_mode lv in
      if not (Locations.Zone.equal Locations.Zone.bottom z) then
        List.iter
          (fun (vi, zvi) ->
	     if Locations.Zone.intersects z zvi then Occurrences.add vi ki lv)
          decls
    end;
    DoChildren


  method vterm_lval tlv =
    (try
       let lv = !Db.Properties.Interp.term_lval_to_lval tlv in
       ignore (self#vlval lv)
     with Invalid_argument msg ->
       if Cmdline.Occurrence.Debug.get () > 0 then
	 Format.printf "[occurrence:] %s@." msg);
    DoChildren
      
  method vstmt_aux s = 
    !Db.progress ();
    super#vstmt_aux s

  initializer !Db.Value.compute ()

end

let compute, _self =
  let run () =
    if Cmdline.Occurrence.Debug.get () > 0 then
      Format.printf "[occurrence] Beginning analysis...@.";
    ignore (visitCilFile (new occurrence :> cilVisitor) (Cil_state.file ()));
    if Cmdline.Occurrence.Debug.get () > 0 then
      Format.printf "[occurrence] Done.@.";
  in
  Computation.apply_once
    (Project.Computation.Name.make "Occurrence.compute")
    [ Occurrences.self ]
    run

let get vi =
  compute ();
  Occurrences.get vi

let print_one v l =
  Format.printf "variable %s (%d):\n" v.vname v.vid;
  List.iter
    (fun (ki, lv) ->
       Format.printf "  sid %a: %a\n"
	 (fun fmt ki -> match ki with
	  | Kglobal -> Format.fprintf fmt "Global"
	  | Kstmt s -> Format.fprintf fmt "%d" s.sid)
	 ki
	 d_lval lv)
    l;
  Format.print_flush ()

let print_all () =
  compute ();
  Occurrences.iter print_one

let debug =
  [ "-debug",
    Arg.Int Cmdline.Occurrence.Debug.set,
    ": level of debug" ]

let options =
  [ "-occurrence",
    Arg.Unit Cmdline.Occurrence.Print.on,
    ": print results of occurrence analysis" ]

let () =
  Db.Occurrence.get := get;
  Db.Occurrence.print_all := print_all;
  Options.add_plugin
    ~name:"occurrence"
    ~descr:"Compute occurrences of variable declarations"
    ~debug
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
