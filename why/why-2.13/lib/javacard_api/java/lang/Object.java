/*
* $Workfile: Object.java $	$Revision: 1.2 $, $Date: 2007/09/27 14:28:59 $
*
* Copyright (c) 1999 Sun Microsystems, Inc. All Rights Reserved.
*
* This software is the confidential and proprietary information of Sun
* Microsystems, Inc. ("Confidential Information").  You shall not
* disclose such Confidential Information and shall use it only in
* accordance with the terms of the license agreement you entered into
* with Sun.
*
* SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
* SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
* IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
* PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
* SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
* THIS SOFTWARE OR ITS DERIVATIVES.
*/

// /*
// $Workfile: Object.java $
// $Revision: 1.2 $
// $Date: 2007/09/27 14:28:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/java/lang/Object.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package java.lang;

  /**
   * Class <code>Object</code> is the root of the Java Card class hierarchy.
   * Every class has <code>Object</code> as a superclass.
   * All objects, including arrays, implement the methods of this
   * class.
   * <p>This Java Card class's functionality is a strict subset of the definition in the
   * <em>Java Platform Core API Specification</em>.<p>
   */
public class Object {

  /* @ public normal_behavior 
         requires true;
       assignable \nothing;
          ensures true;
    @*/
  public Object() {}

 /**
  * Compares two Objects for equality. <p>
  * The <code>equals</code> method implements an equivalence relation:
  * <ul>
  * <li>It is <i>reflexive</i>: for any reference value <code>x</code>,
  * <code>x.equals(x)</code> should return <code>true</code>.
  * <li>It is <i>symmetric</i>: for any reference values <code>x</code> and
  * <code>y</code>, <code>x.equals(y)</code> should return
  * <code>true</code> if and only if <code>y.equals(x)</code> returns
  * <code>true</code>.
  * <li>It is <i>transitive</i>: for any reference values <code>x</code>,
  * <code>y</code>, and <code>z</code>, if <code>x.equals(y)</code>
  * returns  <code>true</code> and <code>y.equals(z)</code> returns
  * <code>true</code>, then <code>x.equals(z)</code> should return
  * <code>true</code>.
  * <li>It is <i>consistent</i>: for any reference values <code>x</code>
  * and <code>y</code>, multiple invocations of <code>x.equals(y)</code>
  * consistently return <code>true</code> or consistently return
  * <code>false</code>.
  * <li>For any reference value <code>x</code>, <code>x.equals(null)</code>
  * should return <code>false</code>.
  * </ul>
  * <p>
  * The equals method for class <code>Object</code> implements the most discriminating possible equivalence
  * relation on objects; that is, for any reference values <code>x</code> and <code>y</code>,
  * this method returns <code>true</code> if and only if <code>x</code> and <code>y</code>
  * refer to the same object (<code>x==y</code> has the value <code>true</code>).
  * @param obj the reference object with which to compare.
  * @return <code>true</code> if this object is the same as the obj argument; <code>false</code> otherwise.
  */

  /* @ public normal_behavior 
    @  requires true;
    @  assignable \nothing;
    @  ensures \result <==> this==obj;
    @*/
    public /* @ pure @*/ boolean equals(Object obj){ return (this==obj); }

}
