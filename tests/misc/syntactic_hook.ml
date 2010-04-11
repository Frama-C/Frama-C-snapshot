open Cabsvisit
open Cabshelper
open Logic_ptree
open Cil_types
open Cil
open Cabs

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
                       lexpr_loc = loc},
                     {status=Unknown}), loc)};
       s]
end

let visitor = new visit;;

Frontc.add_syntactic_transformation (Cabsvisit.visitCabsFile visitor);;
