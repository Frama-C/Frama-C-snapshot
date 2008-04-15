/*
* $Workfile: Throwable.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:37:49 $
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
// $Workfile: Throwable.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:37:49 $
// $Author: marche $
// $Archive: /Products/Europa/api21/java/lang/Throwable.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package java.lang;

  /**
   * The Throwable class is the superclass of all errors and exceptions in the Java Card subset
   * of the Java language.
   * Only objects that are instances of this class (or of one of its
   * subclasses) are thrown by the Java Card Virtual Machine
   * or can be thrown by the Java <code>throw</code> statement.
   * Similarly, only this class or one of its subclasses
   * can be the argument type in a <code>catch</code> clause.
   * <p>This Java Card class's functionality is a strict subset of the definition in the 
   * <em>Java Platform Core API Specification</em>.<p>
   */
public class Throwable {

  /**
   * Constructs a new <code>Throwable</code>. 
   */
  public Throwable() {}
  
}
