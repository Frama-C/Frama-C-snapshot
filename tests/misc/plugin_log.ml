open Kernel

let dkey = register_category "foo-category"

let main () =
  (* oracle stability is not great with backtrace on. *)
  Printexc.record_backtrace false;
  result ~dkey "result with dkey";
  result "result";
  feedback ~dkey "feedback with dkey";
  feedback "feedback";
  debug ~level:0 ~dkey "debug (level 0) with dkey";
  debug ~level:0 "debug (level 0)";
  warning "warning";
  error "error";
  failure "failure"

let () = Db.Main.extend main
