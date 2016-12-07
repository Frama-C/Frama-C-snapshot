open Cabsvisit
open Cabshelper
open Logic_ptree
open Cil_types
open Cil
open Cabs
open Lexing

class visit = object
  inherit nopCabsVisitor
  method vstmt s =
    let loc = get_statementloc s in
    ChangeTo
      [{ stmt_ghost = false;
         stmt_node =
           CODE_ANNOT(
             AAssert([],
                     { lexpr_node =
                         PLat ({ lexpr_node = PLtrue; lexpr_loc = loc},"Pre");
                       lexpr_loc = loc}), loc)};
       s]
end

let visitor = new visit;;

Frontc.add_syntactic_transformation (Cabsvisit.visitCabsFile visitor);;

let warn_pure_exp f e =
  let loc = e.eloc in
  Kernel.warning ~source:(fst loc)
    "[SH]: function %s, pure expression %a is dropped"
    f (Printer.pp_exp) e
;;

Cabs2cil.register_ignore_pure_exp_hook warn_pure_exp;;

let warn_proto vi =
  Kernel.warning ~source:(fst vi.vdecl) "[SH]: implicit declaration for prototype %a"
    (Format.pp_print_string) vi.vname
;;

Cabs2cil.register_implicit_prototype_hook warn_proto
;;

let warn_conflict oldvi vi reason =
  Kernel.warning
    ~source:(fst vi.vdecl)
    "[SH]: conflict with declaration of %a at line %d: %s"
    Format.pp_print_string vi.vname
    (fst oldvi.vdecl).pos_lnum
    reason
;;

Cabs2cil.register_incompatible_decl_hook warn_conflict;;

let warn_distinct oldvi vi =
  Kernel.warning
    ~source:(fst vi.vdecl)
    "[SH]: definition of %a does not use exactly the same prototype as \
     declared on line %d"
    Format.pp_print_string vi.vname
    (fst oldvi.vdecl).pos_lnum
;;

Cabs2cil.register_different_decl_hook warn_distinct;;

let warn_local_func vi =
  Kernel.warning ~source:(fst vi.vdecl)
    "[SH]: definition of local function %a" Format.pp_print_string vi.vname
;;

Cabs2cil.register_local_func_hook warn_local_func;;

let warn_drop_effect olde e =
  Kernel.warning ~source:(fst e.eloc)
    "[SH]: dropping side effect in sizeof: %a is converted to %a"
    Cprint.print_expression olde
    Printer.pp_exp e
;;

Cabs2cil.register_ignore_side_effect_hook warn_drop_effect

let warn_cond_effect orig e =
  let source = fst e.expr_loc in
  Kernel.warning ~source
    "[SH]: side effect of expression %a occurs in \
     conditional part of expression %a. It is not always executed"
    Cprint.print_expression e Cprint.print_expression orig
;;

Cabs2cil.register_conditional_side_effect_hook warn_cond_effect

let process_new_global =
  let seen_vi = Cil_datatype.Varinfo.Hashtbl.create 10 in
  fun vi exists ->
    let source = fst vi.vdecl in
    Kernel.feedback ~source
      "New global node introducing identifier %s(%d)" vi.vname vi.vid;
    if exists then begin
      Kernel.feedback "New occurrence of existing identifier %s" vi.vname;
      if not (Cil_datatype.Varinfo.Hashtbl.mem seen_vi vi) then
        Kernel.fatal
          "identifier %s is supposed to have been already seen, but it has \
           not been processed through this hook." vi.vname;
      let vi' = Cil_datatype.Varinfo.Hashtbl.find seen_vi vi in
      if vi != vi' then
        Kernel.fatal
          "identifier %s(%d) is not shared with its previous occurrence %s(%d)"
          vi.vname vi.vid vi'.vname vi'.vid
    end else begin
      Kernel.feedback "First occurrence of %s" vi.vname;
      if Cil_datatype.Varinfo.Hashtbl.mem seen_vi vi then
        Kernel.fatal
          "This is supposed to be the first occurrence of %s, \
           but it is already present in the table." vi.vname;
      Cil_datatype.Varinfo.Hashtbl.add seen_vi vi vi
    end
;;

Cabs2cil.register_new_global_hook process_new_global
