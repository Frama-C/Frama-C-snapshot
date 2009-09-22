/* run.config
*    GCC:
*    OPT: -slice-return f3 -slice-print -no-slice-callers -journal-disable
*    OPT: -slice-return f3 -slice-print -journal-disable
*    OPT: -slice-return main -slice-print -journal-disable
*    OPT: -slice-return main -slice-print -slicing-level 3  -journal-disable
*/

#include "../pdg/variadic.c"
