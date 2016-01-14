/* variation around issue #2091 */

struct S;

struct S* x[4]; // OK

struct S a[4]; // KO: base type of the array must be complete
