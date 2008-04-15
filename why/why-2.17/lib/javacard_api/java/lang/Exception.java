/*
* $Workfile: Exception.java $	$Revision: 1.2 $, $Date: 2007/09/28 13:41:14 $
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
// $Workfile: Exception.java $
// $Revision: 1.2 $
// $Date: 2007/09/28 13:41:14 $
// $Author: marche $
// $Archive: /Products/Europa/api21/java/lang/Exception.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package java.lang;

/**
  * The class <code>Exception</code> and its subclasses are a form of <code>Throwable</code> that indicates
  * conditions that a reasonable applet might want to catch.
  * <p>This Java Card class's functionality is a strict subset of the definition in the 
  * <em>Java Platform Core API Specification</em>.<p>
  */

public class Exception extends Throwable{

  /**
   * Constructs an <code>Exception</code> instance. 
   */
  /* @ public normal_behavior 
         requires true;
       assignable \nothing;
          ensures true;
    @*/
  public Exception() {}
  
}
