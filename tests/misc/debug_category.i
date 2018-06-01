/* run.config
EXECNOW: make -s tests/misc/Debug_category.cmxs
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key help -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key="-a:b" -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key="-a:b" -test-msg-key a:b:c -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "a:b:c,d" -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "*" -test-warn-key="-a"
OPT: -load-module tests/misc/Debug_category.cmxs
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-error a
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-abort a
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-feedback a
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-abort="*"
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-once a
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-feedback-once a
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-err-once a
*/
