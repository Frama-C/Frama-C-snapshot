/* run.config
EXECNOW: make -s tests/misc/Debug_category.cmxs
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key help -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key="-a:b" -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key a -test-msg-key="-a:b" -test-msg-key a:b:c -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "a:b:c,d" -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs -test-msg-key "*" -test-warn-key="a=inactive"
OPT: -load-module tests/misc/Debug_category.cmxs
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key a=error
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key a=abort
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key a=feedback
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key="*=abort"
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key=a=once
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key a=feedback-once
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key a=err-once
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key test-vis-err
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key test-inv-err
OPT: -load-module tests/misc/Debug_category.cmxs -test-warn-key test-failure
*/
