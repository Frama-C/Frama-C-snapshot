int f() { return 0; }

/*@ predicate is_f( void (*g)()) = g == (void(*)())f; */
