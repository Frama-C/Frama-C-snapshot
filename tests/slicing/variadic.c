/* run.config
    STDOPT: +"-slice-return f3 -no-slice-callers -journal-disable -then-on 'Slicing export' -print"
    STDOPT: +"-slice-return f3 -no-slice-callers -journal-disable -variadic-no-translation -then-last -print"
    STDOPT: +"-slice-return f3 -journal-disable -then-on 'Slicing export' -print"
    STDOPT: +"-slice-return main -journal-disable -then-on 'Slicing export' -print"
    STDOPT: +"-slice-return main -slicing-level 3  -journal-disable -then-on 'Slicing export' -print"
*/

#include "../pdg/variadic.c"
