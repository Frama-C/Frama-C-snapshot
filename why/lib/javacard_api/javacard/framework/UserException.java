/*
* $Workfile: UserException.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: UserException.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/UserException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * <code>UserException</code> represents a User exception.
 * This class also provides a resource-saving mechanism (the <code>throwIt()</code> method) for user
 * exceptions by using a JCRE owned instance.
 * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these temporary objects
 * cannot be stored in class variables or instance variables or array components.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
 */

public class UserException extends CardException{

  // initialized when created by Dispatcher
  private static UserException systemInstance;

  /**
   * Constructs a <code>UserException</code> with reason = 0.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   */
  public UserException() {
    this((short)0);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
  }

  /**
   * Constructs a <code>UserException</code> with the specified reason.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   * @param reason the reason for the exception.
   */
  public UserException(short reason) {
    super(reason);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
    }

  /**
   * Throws the JCRE owned instance of <code>UserException</code> with the specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param reason the reason for the exception.
   * @exception UserException always.
   */
  public static void throwIt(short reason) throws UserException{
    systemInstance.setReason(reason);
    throw systemInstance;
  }
}
