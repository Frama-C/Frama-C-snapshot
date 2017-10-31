let norm1 = Filepath.normalize ~base_name:"/dir1/" "dir/file" in
let norm2 = Filepath.normalize ~base_name:"/dir2/" "dir/file" in
(* norm2 should be different than norm1 *)
Printf.printf "norm1: %s\nnorm2: %s\n" norm1 norm2