/* run.config
EXECNOW: make -s tests/misc/Debug_category.cmxs
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key help
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key-unset a:b
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key-unset a:b -test-msg-key a:b:c
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "a:b:c,d"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "*"
*/
