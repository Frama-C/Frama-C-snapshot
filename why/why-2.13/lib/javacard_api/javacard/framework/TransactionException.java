/*
* $Workfile: TransactionException.java $	$Revision: 1.2 $, $Date: 2007/10/22 08:58:43 $
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
// $Workfile: TransactionException.java $
// $Revision: 1.2 $
// $Date: 2007/10/22 08:58:43 $
// $Author: nrousset $
// $Archive: /Products/Europa/api21/javacard/framework/TransactionException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * <code>TransactionException</code> represents an exception in the transaction subsystem.
 * The methods referred to in this class are in the <code>JCSystem</code> class.
 * <p>The <code>JCSystem</code> class and the transaction facility throw JCRE owned instances
 * of <code>TransactionException</code>.
 * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these temporary objects
 * cannot be stored in class variables or instance variables or array components.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
 * @see JCSystem
 */

public class TransactionException extends CardRuntimeException {

  // constants
 /**
  * This reason code is used by the <code>beginTransaction</code> method to indicate
  * a transaction is already in progress.
  */
  public final static short IN_PROGRESS       = 1;    // beginTransaction called when already in progress
  
 /**
  * This reason code is used by the <code>abortTransaction</code> and <code>commitTransaction</code> methods
  * when a transaction is not in progress.
  */
  public final static short NOT_IN_PROGRESS   = 2;    // commit/abortTransaction called when not in progress
  
 /**
  * This reason code is used during a transaction to indicate that the commit buffer is full.
  */
  public final static short BUFFER_FULL       = 3;    // commit buffer is full
  
 /**
  * This reason code is used during a transaction to indicate 
  * an internal JCRE problem (fatal error).
  */
  public final static short INTERNAL_FAILURE  = 4;    // internal JCRE problem (fatal error)

  // initialized when created by Dispatcher
  private static TransactionException systemInstance;

  /**
   * Constructs a TransactionException with the specified reason.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   */
  public TransactionException(short reason) {
    super(reason);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
    }

  /**
   * Throws the JCRE owned instance of <code>TransactionException</code> with the specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @exception TransactionException always.
   */

    /* @ public exceptional_behavior
      @   requires true;
      @ 
      @   assignable systemInstance.reason;
      @
      @   signals (TransactionException te) te.getReason() == reason; 
      @   signals (TransactionException te) te == systemInstance;
      @*/
    
    public static void throwIt(short reason) {
	systemInstance.setReason(reason);
	throw systemInstance;
    }
}
