/*
* $Workfile: SystemException.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: SystemException.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/SystemException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * <code>SystemException</code> represents a <code>JCSystem</code> class related exception.
 * It is also thrown by the <code>javacard.framework.Applet.register()</code> methods and by
 * the <code>AID</code> class constructor.
 * <p>These API classes throw JCRE owned instances of <code>SystemException</code>. 
 * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these temporary objects
 * cannot be stored in class variables or instance variables or array components.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
 * @see JCSystem
 * @see Applet
 * @see AID
 */

public class SystemException extends CardRuntimeException{

  // initialized when created by Dispatcher
  private static SystemException systemInstance;

   // SystemException reason code
 /**
  * This reason code is used to indicate that one or more input parameters
  * is out of allowed bounds.
  */
  public static final short ILLEGAL_VALUE = 1;
  
 /**
  * This reason code is used by the <code>makeTransient..()</code> methods 
  * to indicate that no room is available in volatile memory for the requested object.
  */
  public static final short NO_TRANSIENT_SPACE = 2;
  
 /**
  * This reason code is used to indicate that the request to create
  * a transient object is not allowed in the current applet context.
  * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details. 
  */
  public static final short ILLEGAL_TRANSIENT = 3;
  
 /**
  * This reason code is used by the <code>javacard.framework.Applet.register()</code> method
  * to indicate that the input AID parameter is not a legal AID value.
  */
  public static final short ILLEGAL_AID = 4;
 
 /**
  * This reason code is used to indicate that there is insufficient resource
  * in the Card for the request. 
  * <p>For example, the Java Card Virtual Machine may <code>throw</code>
  * this exception reason when there is insufficient heap space to create a new instance.
  */
  public static final short NO_RESOURCE = 5;

  /**
   * Constructs a SystemException.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   * @param reason the reason for the exception.
   */

  public SystemException(short reason) {
    super(reason);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this; 
    }

  /**
   * Throws the JCRE owned instance of <code>SystemException</code> with the specified reason.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param reason the reason for the exception.
   * @exception SystemException always.
   */
  public static void throwIt(short reason){
    systemInstance.setReason(reason);
    throw systemInstance;
  }
}
