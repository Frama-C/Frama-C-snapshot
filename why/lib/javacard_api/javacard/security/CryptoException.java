/*
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

/*
// $Workfile: CryptoException.java $
// $Revision: 1.2 $
// $Date: 2007/10/22 07:38:21 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/CryptoException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;
import javacard.framework.CardRuntimeException;

/**
 * <code>CryptoException</code> represents a cryptography-related exception.
 * <p>The API classes throw JCRE owned instances of <code>SystemException</code>.
 * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these temporary objects
 * cannot be stored in class variables or instance variables or array components.
 * @see javacard.security.KeyBuilder
 * @see javacard.security.MessageDigest
 * @see javacard.security.Signature
 * @see javacard.security.RandomData
 * @see javacardx.crypto.Cipher
 */
public class CryptoException extends CardRuntimeException{

  // initialized when created by Dispatcher
  private static CryptoException systemInstance;

  // CryptoException reason codes
 /**
  * This reason code is used to indicate that one or more input parameters
  * is out of allowed bounds.
  */
  public static final short ILLEGAL_VALUE = 1;

 /**
  * This reason code is used to indicate that the key is uninitialized.
  */
  public static final short UNINITIALIZED_KEY = 2;

 /**
  * This reason code is used to indicate that the requested algorithm or key type
  * is not supported.
  */
  public static final short NO_SUCH_ALGORITHM = 3;

 /**
  * This reason code is used to indicate that the signature or cipher object has not been
  * correctly initialized for the requested operation.
  */
  public static final short INVALID_INIT = 4;

 /**
  * This reason code is used to indicate that the signature or cipher algorithm does
  * not pad the incoming message and the input message is not block aligned.
  */
  public static final short ILLEGAL_USE = 5;


  /**
   * Constructs a <code>CryptoException</code> with the specified reason.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   * @param reason the reason for the exception.
   */
  public CryptoException(short reason) {
    super(reason);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this;
  }

  /**
   * Throws the JCRE owned instance of <code>CryptoException</code> with the specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param reason the reason for the exception.
   * @exception CryptoException always.
   */
  public static void throwIt (short reason){
    systemInstance.setReason(reason);
    throw systemInstance;
  }

}
