/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU General Public                   */
/*  License version 2, as published by the Free Software Foundation.      */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/*  See the GNU General Public License version 2 for more details         */
/*  (enclosed in the file GPL).                                           */
/*                                                                        */
/**************************************************************************/

// TODO: does Caduceus permit verification where N and t are
// parameters?

// TODO: assert that the initial queue state models Qnew() -- not
// clear that Caduceus supports this

// TODO: create separate interface functions that implement the spec,
// that call internal functions to do the work, put the algebraic
// queue into a separate file

// TODO: head and dequeue require q_size>0 but also handle the case
// where this is not true -- can and should the spec say something
// about this case?

// TODO: is the set of axioms sound, complete, and minimal?  would be
// nice to check this.

typedef unsigned char uint8_t;
typedef uint8_t bool;
typedef uint8_t error_t;
typedef int t;

#define N 10



// Simplify has no model for mod and verifying this queue requires
// only these special cases

/*@ axiom int_mod_1: \forall int x; \forall int y; 
  @   ((0 <= x < y) => ((x%y) == x))
  @*/

/*@ axiom int_mod_2: \forall int x; \forall int y; 
  @   ((y > 0) && (y <= x < (y+y))) => ((x%y) == (x-y))
  @*/





/*@ type Q */

/*@ logic Q Qnew() */
/*@ logic Q Qerror() */
/*@ logic t Ierror() */

/*@ logic int QmaxSize(Q q) */
/*@ logic int Qsize(Q q) */
/*@ logic Q Qenqueue(Q q, t x) */
/*@ logic int Qempty(Q q) */
/*@ logic t Qelement(Q q, int x) */
/*@ logic t Qhead(Q q) */
/*@ logic t Qdequeue_t(Q q) */
/*@ logic Q Qdequeue_Q(Q q) */

/*@ axiom Q01 : \forall Q q; QmaxSize(q) == N */

/*@ axiom Q02 : \forall Q q; \forall t x; (N <= Qsize(q)) => (Qenqueue(q,x) == Qerror()) */
/*@ axiom Q02a : \forall Q q; \forall t x; (Qsize(q) < N) => (Qenqueue(q,x) != Qerror()) */
/*@ axiom Q02b : \forall Q q; (Qsize(q) == 0) => (Qdequeue_t(q) == Ierror()) */
/*@ axiom Q02c : \forall Q q; (0 < Qsize(q)) => !(Qdequeue_t(q) == Ierror()) */
/*@ axiom Q02d : \forall Q q; (Qsize(q) == 0) => (Qdequeue_Q(q) == Qerror()) */
/*@ axiom Q02e : \forall Q q; (0 < Qsize(q)) => (Qdequeue_Q(q) != Qerror()) */
/*@ axiom Q09 : \forall Q q; \forall int x; (x >= Qsize(q)) => (Qelement(q,x) == Ierror()) */

/*@ axiom Q03 : \forall Q q; Qsize(Qnew()) == 0 */

/*@ axiom Q04 : \forall Q q; \forall t x; (Qsize(q) < N) => Qsize(Qenqueue(q,x)) == 1+Qsize(q) */
/*@ axiom Q04a : \forall Q q; ((0 < Qsize(q))) => Qsize(Qdequeue_Q(q)) == Qsize(q) - 1 */

/*@ axiom Q05 : \forall Q q; (Qsize(q) == 0) => (Qempty(q) == 1) */
/*@ axiom Q06 : \forall Q q; (0 < Qsize(q)) => (Qempty(q) == 0) */


/*@ axiom Q08 : \forall t y; \forall Q q; \forall int x; (0 <= x < Qsize(q))  => 
  @                            ( Qelement(Qenqueue(q,y),x) == Qelement(q,x)) 
  @*/
/*@ axiom Q08a : \forall Q q; \forall int x; ((0 <= x < Qsize(q) - 1) )  => 
  @                            (Qelement(Qdequeue_Q(q),x) == Qelement(q,x + 1)) 
  @*/
/*@ axiom Q08b : \forall Q q; \forall int x; ((x == Qsize(q)) && (Qsize(q) < N))  => 
  @                            (\forall t y; Qelement(Qenqueue(q,y),x) == y) 
  @*/


/*@ axiom Q10 : \forall Q q; Qhead(q) == Qelement(q,0) */
/*@ axiom Q11 : \forall Q q; Qdequeue_t(q) == Qelement(q,0) */
/*@ axiom Q12 : \forall Q q; (Qempty(q) == 1) => 
  @                            (\forall t x; Qdequeue_Q(Qenqueue(q,x)) == Qnew()) 
  @*/
/*@ axiom Q13 : \forall Q q; (Qempty(q) == 0) => 
  @                            (\forall t x; Qdequeue_Q(Qenqueue(q,x)) == 
  @                                          Qenqueue(Qdequeue_Q(q),x)) 
  @*/


static t queue[N];
static unsigned char q_head = 0;
static unsigned char q_tail = 0;
static unsigned char q_size = 0;

/*@ predicate models(Q q) {
  @   (q != Qerror()) &&
  @   (q_size == Qsize(q)) &&
  @   (\forall int i; (0 <= i < q_size) => (Qelement(q,i) == queue[(i+q_head)%N]))
  @ }
  @*/

/*@ predicate valid_slot (uint8_t i) {
  @   (i >= 0) && (i < N) &&
  @   \valid_index(queue,i)
  @ }
  @*/

/*@ predicate occupied_slot (uint8_t i) {   
  @   ((q_tail > q_head) && (q_head <= i < q_tail)) ||
  @   ((q_tail < q_head) && ((q_head <= i) || (i < q_tail))) ||
  @   ((q_head == q_tail) && (q_size == N))
  @ }
  @*/

/* should say 2**(8*sizeof(uint8_t)) instead of 256 */
/*@ invariant q0 : 0 <= N < 256 */ 
/*@ invariant q1 : 0 <= q_size <= N */
/*@ invariant q2 : valid_slot(q_head) */
/*@ invariant q3 : valid_slot(q_tail) */

/*@ invariant size_1 : (q_tail > q_head) => (q_size == (q_tail - q_head)) */
/*@ invariant size_2 : (q_tail < q_head) => (q_size == (N + q_tail - q_head)) */
/*@ invariant size_3 : (q_tail == q_head) <=> ((q_size == 0) || (q_size == N)) */


/*@ assigns \nothing
  @ ensures (\result == N) && 
  @         (\forall Q q; \old(models(q)) => (\result == QmaxSize(q)))
  @*/


uint8_t maxSize (void)
{
  return N;
}

/*@ assigns \nothing
  @ ensures (\result == q_size) && 
  @         (\forall Q q; \old(models(q)) => (\result == Qsize(q)))
  @*/
uint8_t size (void)
{
  return q_size;
}

/*@ assigns \nothing
  @ ensures ((q_size == 0) => (\result == 1)) && 
  @         ((q_size != 0) => (\result == 0)) &&
  @         (\forall Q q; \old(models(q)) => (\result == Qempty(q)))
  @*/
bool empty (void)
{
  return (q_size == 0);
}

/*@ requires q_size > 0
  @ assigns \nothing
  @ ensures (\forall Q q; \old(models(q)) => ((Qhead(q) == \result) &&
  @                                           (Qhead(q) != Ierror())))
  @*/
t head (void)
{ 
  /*@ assert occupied_slot(q_head) */
  return queue[q_head];
}

/*@ requires 0 <= e < q_size 
  @ assigns \nothing
  @ ensures (\forall Q q; \old(models(q)) => ((Qelement(q,e) == \result))) 
  @*/
t element (uint8_t e)
{
  uint8_t tmp = e + q_head;
  tmp = tmp % N;
  /*@ assert valid_slot(tmp) */
  /*@ assert occupied_slot(tmp) */
  return queue[tmp];
}


/*@ assigns queue[\old(q_tail)], q_tail, q_size 
  @ ensures (((\old(q_size) <N) => 
  @           (
  @            (\result == 1) && 
  @            (q_size == (\old(q_size)+1)) &&
  @	       (occupied_slot(\old(q_tail))) &&
  @            (\forall Q q; \old(models(q)) =>
  @              ( 
  @               (Qelement(Qenqueue(q,e),Qsize(q)) ==queue[(Qsize(q)+q_head)%N]) &&
  @               (models(Qenqueue(q,e)))
  @              )
  @            )
  @           )
  @          ) &&
  @          ((\old(q_size) == N) => 
  @           ((\result == 0) && (q_size == \old(q_size)) &&   (\forall Q q; \old(models(q)) => (Qenqueue(q,e) == Qerror())))
  @          )
  @         )
  @*/
error_t enqueue (t e)
{
  if (q_size < N) {
    /*@ assert !occupied_slot(q_tail) */
    queue[q_tail] = e;
    q_tail = q_tail + 1;
    q_tail = q_tail % N;
    q_size = q_size + 1;
    return 1;
  } else {
    return 0;
  }
}

/*@ requires q_size > 0
  @ assigns q_head, q_size
  @ ensures (\result == queue[\old(q_head)]) &&
  @         (q_size == (\old(q_size)-1)) &&
  @         (!occupied_slot(\old(q_head))) &&
  @         (\forall Q q; \old(models(q)) => models(Qdequeue_Q(q)))  &&
  @         (\forall Q q; \old(models(q)) => ((Qdequeue_t(q) == \result) &&
  @                                           (Qdequeue_t(q) != Ierror())))
  @*/
t dequeue (void)
{
  int tmp;
  /*@ assert occupied_slot(q_head) */
  tmp = queue[q_head];
  if (!(q_size == 0)) {
    q_head = q_head + 1;
    q_head = q_head % N;
    q_size = q_size - 1;
  }
  return tmp;
}

