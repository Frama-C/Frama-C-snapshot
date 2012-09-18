/* run.config
DONTRUN: main test is in tests/spec/model1.c
*/

#include "tests/spec/model1.h"

struct S { int bar; };

/*@ type invariant foobar(struct S s) = s.bar == s.foo; */

void reset (struct S* s) { s->bar == 0; }

void inc(struct S* s) { s->bar += 5; }

void dec(struct S* s) { s->bar--; }

int is_pos (struct S* s) { return (s->bar > 0) ? 1 : 0; }
