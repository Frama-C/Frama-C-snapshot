# Wp Tutorial

## Presentation

## Task 1

**Objective:** understand how to use WP & the TIP.

Enter directory `wp-tutorial`.
A shell script `./fc.sh` is provided to run `frama-c` with useful
options for the tutorial. Use `frama-c -wp-h` for more details.

Open file `reverse.c`. This is an implementation of reversing all bits
of a single byte, here represented as an int. The function uses a pre-computed
table which is stored in module `endian.c`. The `ACSL` contract specifies
the expected result in terms of macro `BIT(x,k)`
which tests the `k`-th bit of `x`, for `k` in `1..8`.

    $ ./fc.sh endian.c reverse.c

Alt-Ergo fails to prove the `InRange` behavior of the function. This is because
it has no way to compute bitwise operations. However `Qed` is able to compute
and simplify them.

    $ ./fc.sh endian.c reverse.c -g

This opens the GUI. Select `Wp Goals`, you see all the
results (use the popup menu to filter goals, e.g. select "All Results" to ensure
that all goals are always displayed).

Double-click the non-proved goal. Select the value `m@pre` of the input parameter
of the function. The `Tactics` pane shows a list of applicable tactics.
Apply the `Range (0-255)` tactic to brute-force prove the property.
Click on the tactical buttons to open a small pane and enter the parameters.

Finally, don't forget to **Save** your proof using the `Save` button on the toolbar.

There is actually an error in the table, for `m = 145`.
File `endian-fix.c` contains the fixed table.
You shall update your script using (using either command-line or GUI):

    $ ./fc.sh endian-fix.c reverse.c [-g]

**Remark:** By default, the `./fc.sh` script always update your session scripts according to
the last execution results. Use `-s` to re-run your scripts without updating them:

    $ ./fc.sh endian-fix.c reverse.c -s

## Task 2 (optional)

Apply the same method to function `reverse2.c`. The specification
is written differently, and you need to range over the bit-number. This leads
to 8 sub-goals, all of which requiring a brute-force `Range (0-255)` range
tactic to be applied.

However, instead of manually select the range tactic, you can use the `Strategies`
tactic and try an `auto-range`. You get each of the 8 sub-proofs in one-click,
and the overall proof of the function in 9 clicks. Although it is acceptable,
we clearly need a more powerfull way to interact with the prover.

## Task 3

**Objective:** learn how to extend the TIP with user-defined tactics.

Presentation:

    $ make wp-doc-api
    $ open src/plugins/wp/doc/html/index.html

A tactical uses a mixture of several components:
- user's _selection_, which can be a clause (goal or hypothesis) or a term inside of a clause
- tactical parameters (spinners, checkbox, menu items, ...)

Boilerplate code to write a custom tactic is shown in `wp-tutorial/TacDummy.ml`
file. The tutorial source file contains a predefined `.merlin` configuration.
A custom tactic can be dynamically imported in frama-c using:

    $ ./fc.sh -load-script TacDummy.ml ...

The main method of a tactical class has the following signature:

    $ method select : feedback -> selection -> status

The feedback parameter is used to refine the tactical description with respect
to user's selection and parameters, or inform the user's or selection errors.

The returned status can be either `Not_applicable` when the tactical does not
apply to user's selection, `Not_configured` if the parameters are not consistent
(e.g. range `10..5` instead of `5..10`), of `Applicable p` where `p` is a
_process_, aka, a proof transformer.

Proof transformers take one sequent and produce sub-sequents.
Typically, the module `Wp.Auto` provides the following basic process combinator:

    val t_case : pred -> process -> process -> process

Given two process (proof trees) `A`, `B` and a predicate `p`,
function `t_cut` simply builds the following proof tree:

      A              B
    --------     ---------
    H p |- G     H !p |- G
    ----------------------  t_case p A B
           H |- G

Another combinator is `t_finally : string -> process` which returns an identity
proof tree, with just an informative label, to be proved later on by another
tactic or an automated prover.

For example, using such a combinator, it is easy to build a tactic performing
case analysis (like the Split built-in tactic):

    (* See TacSplit.ml for all details *)

    class dummy_split =
    object
       ...
       method select feedback selection =
         let e = Tactical.selected selection in
           if F.is_prop e then
           let p = F.p_bool e in
           Applicable(t_case p
                       (t_finally "Positive")
                       (t_finally "Negative"))
         else
           Not_applicable
       ...
     end

The tactical method reads as follows: first, compute the selected expression `e` from
user's selection. When user's selection is a boolean property, apply the `t_case p`
transformer on current goal. The two remaining branches of the proof tree are left to the user
using with informative labels instead of `A` and `B` above, using `t_finally`.

The complete solution is in `TacSplit.ml`. You can play with this tactic with:

    $ ./f.sh -load-script TacSplit.ml decoder.c -g

## Task 4

**Objective:** build and play more complex tactics.

We consider now file `decoder.c`. It shows a C function apply various operations
on two integers `a` and `b` depending on a op-code `m`. Its specification is as follows:
- if bit-0 of `m` is set, `a` is transformed into `-a`,
- if bit-1 of `m` is set, `b` is transformed into `-b`,
- finally, transformed `a` and `b` are added and the sum is returned.

The implementation proceed on a different way, and the goal is to prove that the function
is correct with respect to its specification. The basic strategy of WP decompose the proof
into 5 sub-goals (see the `Wp Goals` window):
- one for each `m & 3 = 0`, `1`, `2` and `3`
- one for exluding any other value of `m & 3`

The basic idea is to perform a brute-force analysis over `m & 3` over range
`0..3`.  However, `Qed` is not able to further simplify hypothesis like
`m & 3 = 2` in terms of `bit-test(m,1)` and `but-test(m,0)`.

Moreover, `Qed` is not able to prove that `0 <= m & 3 <= 3`.
This where tactical comes to rescue.

We start by writing in `TacBits1.ml` a tactical `BitRange` to add such an hypothesis when the
user selects an expression with a logical-and (`&`) with positive constants.
This is sufficient to prove automatically the last goal excluding any other
value from `0..3`.

To cope with expression like `m & 3 = 2`, the solution is to decompose any
equality `a = b` over the bitwise equality of bits of `a` and `b`.  Since
integers can have an arbitrary number of bits, we can here restrict ourselves to
`8` bits, since `a` and `b` are small enough. However, to remain sound, the tactical
still checks for `a` and `b` to have 8 significative bits:

    0 <= a,b < 255          a & (1 lsl k) == b & (1 lsl k)   for k = 0..7
    ---------------------------------------------------------------------
                    a == b

This is implemented in file `TacBits2.ml`.

# Task 5

**Objective:** add parameters to tactics.

The previous tactic only applys to 8-bits values. It is possible to add a
parameter to extend the tactic to N-bits values for any N. The original 8-bits
tactical has the following code:

    method select feedback selection =
      let e = T.selected selection in
      match R.term e with
      | R.Eq(a,b) when F.is_int a && F.is_int b ->
          let inrange = F.p_and (range a 8) (range b 8) in
          let bitwise = bitwise_eq a b 8 in
          T.Applicable
            (A.t_cut ~by:"range" inrange
               (T.rewrite [ "bitwise" , F.p_true , e , bitwise ]))
      | _ -> T.Not_applicable

To add a spinner-selected parameter, we first need to declare it:

    let vrange,prange = T.spinner ~id:"POPL.TacBit.range"
       ~vmin:0 ~vmax:64 ~default:8
       ~title:"Bits" ~descr:"Number of bits for bitwise equality" ()

Finally, we add the `prange` parameter to the tactical class, and use the associated
`vrange` field value instead of the fixed `8` value:

    method select feedback selection =
      let e = T.selected selection in
      match R.term e with
      | R.Eq(a,b) when F.is_int a && F.is_int b ->
          let n = self#get_field vfield in
          let inrange = F.p_and (range a n) (range b n) in
          let bitwise = bitwise_eq a b n in
          T.Applicable
            (A.t_cut ~by:"range" inrange
               (T.rewrite [ "bitwise" , F.p_true , e , bitwise ]))
      | _ -> T.Not_applicable

# Thanks !


