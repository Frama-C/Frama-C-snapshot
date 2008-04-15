/*
* $Workfile: CardException.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: CardException.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/CardException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Zhiqun
// */

package javacard.framework;

/**
 * The <code>CardException</code> class
 * defines a field <code>reason </code>and two accessor methods <code>
 * getReason()</code> and <code>setReason()</code>. The <code>reason</code>
 * field encapsulates exception cause identifier in Java Card.
 * All Java Card checked Exception classes should extend
 * <code>CardException</code>. This class also provides a resource-saving mechanism
 * (<code>throwIt()</code> method) for using a JCRE owned instance of this class.
 */

public class CardException extends Exception {

  // initialized when created by Dispatcher
  private static CardException systemInstance;

  // CardException reason code
  private short reason;

  /**
   * Construct a CardException instance with the specified reason.
   * To conserve on resources, use the <code>throwIt()</code> method
   * to use the JCRE owned instance of this class.
   * @param reason the reason for the exception
   */
  public CardException(short reason){
    this.reason = reason;
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
  }

  /** Get reason code
   * @return the reason for the exception
   */
  public short getReason() {
    return reason;
  }

  /** Set reason code
   * @param reason the reason for the exception
   */
  public void setReason(short reason) {
    this.reason = reason;
  }


  /**
   * Throw the JCRE owned instance of <code>CardException</code> class with the
   * specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param reason the reason for the exception
   * @exception CardException always.
   */
  public static void throwIt(short reason) throws CardException{   
    systemInstance.setReason(reason);
    throw systemInstance;
  }
}
