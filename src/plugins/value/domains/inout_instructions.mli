(**

   The Inout domain computes information about the memory locations
   that are read and written during the analysis of a function. Its
   implementation can be found in the Inout_domain module. The OCaml
   type of the abstract domain is {!Inout_domain.inout}. A set of
   read or written bits is modeled by the Frama-C type {!Locations.Zone.t}.

   Currently, the domain computes two things:
   - an over-approximation of the memory locations read by the
     function (field {!over_inputs})
   - an over-approximation of the memory locations written by the
     function (field {!over_outputs})

   You should familiarize yourself with the code in the modules
   LatticeInout and Transfer, on which we will work. The functions
   [inputs_*] can be skipped: they compute the inputs of various
   fragments of Frama-C's Ast (see {!Cil_types}), but we won't modify
   them.

*)

(**

   Task 1:

   Read functions [assign] and [assume] in the functor [Internal.Transfer].
   The effects of assignments and conditionals are simply accumulated using
   function {!Locations.Zone.join}. Thus, the analysis is not really
   path-sensitive. We are going to add two new types of inputs and outputs
   that must be computed in a path-sensitive way.

   - *Sure outputs* are memory locations that the function is guaranteed
     to overwrite during its execution. We are going to compute an
     under-approximation of them
   - *Operational inputs* are the "real" inputs of the functions, e.g.
     the memory locations it reads, without having defined them previously.
     in the code below, x is an operational input, but not y.

   int x, y, z;
   void f() {
     y = 1;
     z = x + y;
   }

   We are going to over-approximate the operational inputs, by subtracting
   sure outputs from inputs.

   First, add two new fields to the type inout:

   type inout = {
     (* over-approximation of the memory locations written by the function *)
     over_outputs: Zone.t;
     (* over-approximation of the memory locations read by the function *)
     over_inputs: Zone.t;
     (* under-approximation of the memory locations written by the function *)
     under_outputs: Zone.t;
     (* over-approximation of the memory locations parts read by the function
        that are parts of its inputs (i.e. that the function has not written
        previously) *)
     operational_inputs: Zone.t;
   }

   then update all functions of LatticeInout to compute those fields. You
   will probably need the functions {!Locations.Zone.diff},
   {!Locations.Zone.meet} and {!Precise_locs.cardinal_zero_or_one}.
   Remember that locations are themselves over-approximations. Thus
   an assignment is "certain" only if they abstract at most one concrete
   location.

   Test your code on the file inout.c, with the command-line
   ./bin/frama-c -val -eva-inout-domain value-tutorial/inout.c -value-msg-key d-inout,-final-states

   Are under-inputs properly computed for function f? If not, how should
   the code of the function Transfer.sequence be modified?

   Add option -slevel 10 to the command-line, which will unroll 10 iterations
   of simple loops. If everything goes well, sure outputs and operational
   inputs should be computed in a more precise way in g.

*)

(** Task 2:

    Observe the abstract state in the second call to [g], when running Frama-C
    on inout.c. The inputs and outputs of the first call still appear in
    the second call. Why?

    We would like the analysis to be more modular: it should compute
    the various kinds of inputs and outputs starting from the
    beginning of _the function being currently analyzed_. In
    particular, the analyis should start with an empty state. Read the
    documentation of functions {!Abstract_domain.start_call} and
    {!Abstract_domain.finalize_call}, and modify them accordingly.

*)
