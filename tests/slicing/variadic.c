/* run.config
    OPT: -slice-return f3 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
    OPT: -slice-return f3 -journal-disable -then-on 'Slicing export' -print
    OPT: -slice-return main -journal-disable -then-on 'Slicing export' -print
    OPT: -slice-return main -slicing-level 3  -journal-disable -then-on 'Slicing export' -print
*/

#include "../pdg/variadic.c"
