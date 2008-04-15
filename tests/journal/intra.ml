let _ = !Db.Sparecode.get true true;;

let nb_projects () =
  let n = ref 0 in
  Project.iter_on_projects (fun _ -> incr n);
  !n

(* Calling [!Db.Sparecode.get] twice (even from a journal) does not create
   twice a project. Hence we must end up with 2 projects, or 3 if there is an
   initial copy.
*)
let nb_projects_expected = if Cmdline.Files.Copy.get() then 3 else 2

let () = assert (nb_projects () = nb_projects_expected);;
