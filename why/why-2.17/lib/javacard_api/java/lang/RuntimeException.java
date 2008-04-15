/*
* $Workfile: RuntimeException.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:37:49 $
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
// $Workfile: RuntimeException.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:37:49 $
// $Author: marche $
// $Archive: /Products/Europa/api21/java/lang/RuntimeException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package java.lang;

/**
 * <code>RuntimeException</code> is the superclass of those exceptions that can be thrown
 * during the normal operation of the Java Card Virtual Machine.<p> 
 * A method is not required to declare in its throws clause any subclasses of
 * <code>RuntimeException</code> that might be thrown during the execution of the
 * method but not caught.
 * <p>This Java Card class's functionality is a strict subset of the definition in the 
 * <em>Java Platform Core API Specification</em>.<p> 
 */

public class RuntimeException extends Exception {

  /**
   * Constructs a <code>RuntimeException</code> instance.
   */
  public RuntimeException() {}

}
