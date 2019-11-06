/* run.config
MODULE: tests/misc/Debug_category.cmxs
OPT: -test-msg-key help -test-warn-key="a=inactive"
OPT: -test-msg-key a -test-warn-key="a=inactive"
OPT: -test-msg-key a -test-msg-key="-a:b" -test-warn-key="a=inactive"
OPT: -test-msg-key a -test-msg-key="-a:b" -test-msg-key a:b:c -test-warn-key="a=inactive"
OPT: -test-msg-key "a:b:c,d" -test-warn-key="a=inactive"
OPT: -test-msg-key "*" -test-warn-key="a=inactive"
OPT:
OPT: -test-warn-key a=error
OPT: -test-warn-key a=abort
OPT: -test-warn-key a=feedback
OPT: -test-warn-key="*=abort"
OPT: -test-warn-key=a=once
OPT: -test-warn-key a=feedback-once
OPT: -test-warn-key a=err-once
OPT: -test-warn-key test-vis-err
OPT: -test-warn-key test-inv-err
OPT: -test-warn-key test-failure
FILTER: sed 's|Your Frama-C version is.*|Your Frama-C version is VERSION|'
*/
