let () =
  Kernel.feedback "normalize(/): %s" (Filepath.normalize "/");
  Kernel.feedback "normalize(/..): %s" (Filepath.normalize "/..");
  Kernel.feedback "normalize(/../../.): %s" (Filepath.normalize "/../../.");
  (* when there are several '/', only the last one is removed *)
  Kernel.feedback "normalize(///): %s" (Filepath.normalize "///");
  Kernel.feedback "normalize(//tmp//): %s" (Filepath.normalize "//tmp//");
  Kernel.feedback "normalize(/../tmp/../..): %s"
    (Filepath.normalize "/../tmp/../..");
  Kernel.feedback "normalize(/tmp/inexistent_directory/..): %s"
    (Filepath.normalize "/tmp/inexistent_directory/..");
  try
    Kernel.feedback "normalize(): %s" (Filepath.normalize "");
    Kernel.fatal "this code should be unreachable"
  with Invalid_argument _ ->
    Kernel.feedback "caught the expected exception"
