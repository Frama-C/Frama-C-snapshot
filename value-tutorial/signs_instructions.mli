(**

   The Sign domain computes information about the sign of integer
   variables. The implementation is given in {!Sign_values} and
   {!Sign_domain}.

   - The first module defines the "values" for signs, e.g. can the
   variable be negative, zero or positive. The implementation of
   {!Sign_values} is self-contained, and should be read in its
   entirety. It implements the signature {!Abstract_value}.

   - The second module is the memory domain itself. Currently, it only keeps
   track of basic variables (no support for aggregates). The implementation
   is mostly done by {!BaseMapLattice}. These two modules need not be
   read for this tutorial.

*)

(**

   Task 1:

   Read in detail {!Sign_values.mul} and {!Sign_values.forward_binop}, that
   are the forward transfer functions for the sign domain. Write [div]
   by adapting [mul].

   Test your code using

   ./fc.sh signs.c

   and check that q is not negative.

*)

(**

   Task 2:

    A division by 0 alarm is still emitted in the first 'if', even though the
    domain "knows" that v is not null. (This is one of the rare cases where
    the signs domain is more expressive than the interval domain, which
    is active by default in EVA.)

    Modify the code for [div] so that it no longer returns {!Alarmset.all},
    but instead the set of alarms corresponding to "all alarms except
    a division by zero". The APIs to use are in {!Alarms} and {!Alarmset}.

    Check that the alarm is no longer emitted on the previous example.
*)

(**

   Task 3:

   The second 'if' is still analyzed imprecisely. Indeed, it uses the
   '<=" and '>=" operators, but the only abstract transformers currently
   implemented are those for '==' and '!='.
   Adapt the code for [Eq] and [Ne] in {!Sign_values.backward_comp_right} for
   the operators [Le], [Lt], [Ge], [Gt]. Test your code on the second if.

*)
