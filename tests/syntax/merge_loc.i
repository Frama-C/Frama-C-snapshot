/* run.config
   EXECNOW: make -s tests/syntax/pp_lines.cmxs
   STDOPT: #"-load-module tests/syntax/pp_lines.cmxs"
*/

// Test locations when cabs2cil merges declarations and tentative definitions
// together. We should always favor the tentative definition.

extern int foo;

int foo;  // Better


int bar; // Better

extern int bar;


extern int baz;

extern int baz;

int z = &baz;
