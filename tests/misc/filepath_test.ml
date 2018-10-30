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
  Kernel.feedback "normalize(): %s"
    (Filepath.normalize "");
  Kernel.feedback "relativize(.): %s"
    (Filepath.relativize ".");
  Kernel.feedback "relativize(./tests/..): %s"
    (Filepath.relativize "./tests/..");
  Kernel.feedback "relativize(/a/bc/d,base_name:/a/b/): %s"
    (Filepath.relativize ~base_name:"/a/b/" "/a/bc/d")
