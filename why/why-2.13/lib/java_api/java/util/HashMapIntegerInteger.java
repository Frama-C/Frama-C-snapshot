
package java.util;

class HashMapIntegerInteger {

    //@ model mappings hashmap_model;


    /*@ ensures this.hashmap_model == empty_mappings();
      @*/
    HashMapIntegerInteger();

    /*@ requires x != null;
      @ assigns hashmap_model;
      @ ensures 
      @   this.hashmap_model == update(\old(this.hashmap_model),x,y);
      @*/
    void put(Integer x, Integer y);

    /*@ requires x != null;
      @ assigns \nothing;
      @ behavior key_found:
      @   ensures \result != null ==>
      @    \result == access(this.hashmap_model,x) ;
      @ behavior null_found:
      @   assumes containsKey(this.hashmap_model,x);
      @   ensures access(this.hashmap_model,x) == null ;
      @       
      @*/
    Integer get(Integer x);

}
