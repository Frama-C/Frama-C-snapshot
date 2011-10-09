/* run.config
    OPT: -check -slice-return f3 -no-slice-callers -journal-disable -then-on 'Slicing export' -print
    OPT: -check -slice-return f3 -journal-disable -then-on 'Slicing export' -print
    OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
    OPT: -check -slice-return main -slicing-level 3  -journal-disable -then-on 'Slicing export' -print
*/

#include "../pdg/variadic.c"
