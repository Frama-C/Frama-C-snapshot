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

//@ type larray

/* [create(x)] returns an array  
 * where each cell contains [x] 
 **/
//@ logic larray create(double x);

//@ logic double select(larray t, integer i);

/*@ logic larray 
  @  store(larray t, integer i, double x);
  @*/

/*@ axiom select_store_eq:
  @  \forall larray t, integer i, double x;
  @   select(store(t,i,x),i) == x;
  @*/

/*@ axiom select_store_neq:
  @  \forall larray t, integer i j;
  @   \forall double x;
  @    i != j ==> 
  @     select(store(t,i,x),j) == select(x,j);
  @*/


// public Interface for PArray
interface PArrayInterface {

    //@ model larray model_array;
    //@ model integer model_length;
  
    /* @ requires n >= 0;
      @ ensures 
      @   this.model_array == create(0.0)
      @   && this.model_length == n;
      @*/
    // PArrayInterface(int n);
    
    /*@ requires 0 <= i < this.model_length;
      @ assigns \nothing;
      @ ensures \result == select(this.model_array,i);
      @*/
    double get(int i);

    /*@ requires 0 <= i < this.model_length;
      @ assigns \nothing;
      @ // ensures \fresh(\result); 
      @ ensures \result.model_array == store(this.model_array,i,x)
      @   && \result.model_length == this.model_length;
      @*/
    PArrayInterface set(int i, double x);

}



abstract class Data {

    abstract double get(int i);
    
    abstract PArray set(int i, double x, PArray parent);

}

class Arr extends Data {

    double table[];

    //@ invariant table_non_null: table != null;

    Arr(int n) {
	table = new double[n];
    }

    double get(int i) {
	return table[i];
    }

    PArray set(int i, double x, PArray parent) {
	double old = table[i];
	table[i] = x;
	PArray tmp1 = new PArray(this);
	Data tmp2 = new Diff(i,old,tmp1);
	parent.contents = tmp2;
	return tmp1;
    }

    public String toString() {
	String s = "Arr [";
	for (int i = 0; i < table.length; i++)
	    s += table[i] + "; ";
	return (s + "]");
    }
}

class Diff extends Data {

    private int index;
    private double value;
    private PArray remaining;

    //@ invariant remaining_non_null: remaining != null;
    /*@ invariant diff_repr: 
      @   data_repr(this.remaining.model_length,store(index,value,this.remaining.model_array),this);
      @*/

    Diff(int i, double x, PArray rem) {
	index = i;
	value = x;
	remaining = rem;
    }

    double get(int i) {
	if (i == index) return value;
	return remaining.get(i);
    }

    PArray set(int i, double x, PArray t) {
	Data tmp = new Diff(i,x,t);
	return new PArray(tmp);
    }

    public String toString() {
	return "Diff " + index + ", " + value + ", " + remaining;
    }
}

public class PArray implements PArrayInterface 
{
    
    protected Data contents;
    
    //@ invariant contents_non_null: contents != null;
    /*@ invariant data_repr:
      @    data_repr(model_length,model_array,contents);
      @*/
    
    protected PArray(Data d) {
	contents = d;
    }
    
    public PArray(int n) {
	this(new Arr(n));
    }
    
    public double get(int i) {
	return contents.get(i);
    }

    public PArray set(int i, double x) {
	return contents.set(i,x,this);
    }

    public String toString() {
	return ("-> " + contents);
    }

    public static void main(String argv[]) {

	PArray t1 = new PArray(4);
	PArray t2 = t1.set(0,2.2);
	PArray t3 = t2.set(1,3.3);
	System.out.println("t1 = " + t1);
	System.out.println("t2 = " + t2);
	System.out.println("t3 = " + t3);
    }

}


/*@ predicate data_repr(integer model_length, 
  @                     larray model_array, Data contents);
  @*/

/*@ predicate repr(integer model_length, 
  @                larray model_array, PArray p);
  @*/
 
/*@ axiom arr_repr :
  @   \forall integer model_length, larray model_array, Arr a ;
  @     data_repr(model_length, model_array,a) 
  @     <==>
  @     model_length == a.table.length &&
  @     \forall integer i; 
  @       0 <= i < a.table.length ==> a.table[i] == select(model_array,i) ;
  @*/

/*@ axiom diff_repr_1 :
  @   \forall integer model_length, larray model_array, Diff d; 
  @   \forall integer i, double x; 
  @     repr(model_length, model_array, d.remaining) &&
  @     i == d.index && x == d.value  
  @     ==> 
  @     data_repr(model_length, store(i,x,model_array),d) ;
  @*/

/*@ axiom diff_repr_2 :
  @   \forall integer model_length, larray model_array, Diff d ;
  @     data_repr(model_length, model_array, d) &&
  @     \exists larray a; 
  @     model_array == store(d.index,d.value,a)
  @     &&
  @     repr(model_length, a, d.remaining) ;
  @*/




	
	
	
