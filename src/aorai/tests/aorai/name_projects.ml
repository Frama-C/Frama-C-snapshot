let run () =
  let print_one p = Kernel.feedback "Found %a" Project.pretty p in
  Project.iter_on_projects print_one

let () = Cmdline.at_normal_exit run
