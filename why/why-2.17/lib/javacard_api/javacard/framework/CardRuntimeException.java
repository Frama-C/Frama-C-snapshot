/*
* $Workfile: CardRuntimeException.java $	$Revision: 1.2 $, $Date: 2007/09/26 15:15:36 $
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
// $Workfile: CardRuntimeException.java $
// $Revision: 1.2 $
// $Date: 2007/09/26 15:15:36 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/CardRuntimeException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Zhiqun
// */

package javacard.framework;

/**
 * The <code>CardRuntimeException</code> class
 * defines a field <code>reason </code>and two accessor methods <code>
 * getReason()</code> and <code>setReason()</code>. The <code>reason</code>
 * field encapsulates exception cause identifier in Java Card.
 * All Java Card unchecked Exception classes should extend
 * <code>CardRuntimeException</code>. This class also provides a resource-saving mechanism
 * (<code>throwIt()</code> method) for using a JCRE owned instance of this class.
 */

public class CardRuntimeException extends RuntimeException {

  // initialized when created by Dispatcher
  private static CardRuntimeException systemInstance;

  // CardRuntimeException reason code
  private /* @ spec_public */ short reason;

  /**
   * Construct a CardRuntimeException instance with the specified reason.
   * To conserve on resources, use <code>throwIt()</code> method
   * to use the JCRE owned instance of this class.
   * @param reason the reason for the exception
   */
  public CardRuntimeException(short reason){
    this.reason = reason;
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
  }

  /** Get reason code
   *  @return the reason for the exception
   */

    
    public /* @ pure @*/ short getReason() {
	return reason;
    }

  /** Set reason code
   * @param reason the reason for the exception
   */
  public void setReason(short reason) {
    this.reason = reason;
  }


  /**
   * Throw the JCRE owned instance of the <code>CardRuntimeException</code> class with the
   * specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param reason the reason for the exception
   * @exception CardRuntimeException always.
   */
  public static void throwIt(short reason) throws CardRuntimeException{  
    systemInstance.setReason(reason);
    throw systemInstance;
  }
}
