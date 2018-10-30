/* run.config
   OPT: -wp-warn-memory-model
   OPT: -wp-model "+ref"
 */

/* run.config_qualif
   OPT: -wp-warn-memory-model
   OPT: -wp-model "+ref"
 */

// --------------------------------------------------------------------------
// --- Local Frame
// --------------------------------------------------------------------------

/*
  The function has _no_ separation hypotheses.
  The assertion shall be proven in all models.
*/

//@ requires \valid(one);
void local_frame(int *one){
  int two;
  //@ assert ok: one != &two;
}

// --------------------------------------------------------------------------
// --- Global Frame
// --------------------------------------------------------------------------

/*
  The function _has_ separation hypotheses in Typed model: the pointer 'one'
  might be aliased to any global variable. Since the global 'zero' is not
  aliased, it is assumed not be pointed-to. The default separation hypothesis is
  then: \separated( one , &zero );

  This hypothesis _could_ be released if we assume that pointers to scalar of
  different types are not permitted.

  Like function above, if `zero` and `one` are valid pointers, 
  they point to allocated blocks in the heap.
  Hence, they are separated from all locals and formals.

  However, using the `REF` model, we are also able to prove that `one` and
  `zero` are separated, since `*one` and `*zero` are placed on the ByRef model.
  With this model, the generated separation hypothesis shall be: \separated(one,zero).
 */

int *zero;

/*@ 
  requires \valid(zero) && \valid(one);
  ensures sep_iff_ref: \separated(zero,one);
  ensures one_iff_ref: *one == \old(*one + *zero + 1);
  ensures zero_always: *zero == \old(*one + *zero - 1);
*/
void global_frame(int *one, int arg){
  int two = *one + *zero ;
  *one = two + 1 ;
  *zero = two - 1 ; // might havoc *one
  //@ assert ok: \separated(one,&arg,&two);
  //@ assert ok: \separated(zero,&arg,&two);
}
