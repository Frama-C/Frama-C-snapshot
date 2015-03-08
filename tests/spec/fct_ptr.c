typedef void(*proc)(void);

int x;

void inc() { x++; }

void dec() { x--; }

proc f = dec;
proc g = inc;

void call(proc x) { x(); }

/*@ predicate foo = f == dec || f == &inc; */
