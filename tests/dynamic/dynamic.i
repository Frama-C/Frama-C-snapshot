/*run.config
  EXECNOW: make tests/dynamic/empty.cmo tests/dynamic/abstract.cmo tests/dynamic/abstract2.cmo
  CMD: FRAMAC_PLUGIN=tests/dynamic bin/toplevel.byte
  OPT: -add-path tests/dynamic/file_path -add-path tests/dynamic/directory_path -add-path tests/dynamic/none
  OPT: -load-module tests/dynamic/empty.cmo,tests/dynamic/abstract.cmo,tests/dynamic/abstract2.cmo
 */
