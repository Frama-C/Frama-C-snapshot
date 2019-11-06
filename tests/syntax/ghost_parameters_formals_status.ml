open Cil_types

let run () =
  let print_info kf =
    let pretty_formal fmt vi =
      assert(vi.vformal) ;
      Format.fprintf fmt "@\n- %a which is %s"
        Cil_datatype.Varinfo.pretty vi
        (if vi.vghost then "ghost" else "non-ghost")
    in
    let pretty_formal_list fmt l =
      match l with
      | [] ->
        Format.fprintf fmt "No Formals"
      | _ ->
        Format.fprintf fmt "Formals are %a"
          (Pretty_utils.pp_flowlist ~left:"" ~sep:"" ~right:"" pretty_formal) l
    in
    let vi = Kernel_function.get_vi kf in
    let formals = Cil.getFormalsDecl vi in    
    Kernel.feedback "Type of %s is %a.@ %a"
      vi.vname
      Cil_datatype.Typ.pretty vi.vtype
      pretty_formal_list formals
  in
  Globals.Functions.iter print_info
  
let () = Db.Main.extend run
