//@ lemma cos_pi: \cos(\pi) == -1.0;

//@ lemma truncate: \truncate(1.0) == (integer) 1.0 == 1;

//@ lemma coerce_and_truncate: \forall double x; (integer) x == \truncate(x);

//@ lemma nop: (integer) 1 == 1;
