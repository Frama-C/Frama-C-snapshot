/*
* $Workfile: JCSystem.java $	$Revision: 1.7 $, $Date: 2008/02/07 22:08:32 $
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
// $Workfile: JCSystem.java $
// $Revision: 1.7 $
// $Date: 2008/02/07 22:08:32 $
// $Author: nrousset $
// $Archive: /Products/Europa/api21/javacard/framework/JCSystem.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

import com.sun.javacard.impl.PrivAccess;
import com.sun.javacard.impl.NativeMethods;

/**
 * The <code>JCSystem</code> class includes a collection of methods to control
 * applet execution, resource management, atomic transaction management
 * and inter-applet object sharing in Java Card.
 * All methods in <code>JCSystem</code> class are static methods.<p>
 *
 * The JCSystem class also includes methods to control the persistence
 * and transience of objects. The term <em>persistent</em> means that objects and their values
 * persist from one CAD session to the next, indefinitely.
 * Persistent object values are updated atomically using transactions.<p>
 * The <code>makeTransient...Array()</code> methods can be used to create <em>transient</em> arrays
 * with primitive data components.
 * Transient array data is lost (in an undefined state, but the real data is unavailable)
 * immediately upon power loss, and is reset to the default value at the occurrence of certain events
 * such as card reset or deselect.
 * Updates to the values of transient arrays are not atomic and are not affected by transactions.
 * <p>
 * The JCRE maintains an atomic transaction commit buffer which is initialized on card reset
 * (or power on).
 * When a transaction is in progress, the JCRE journals all updates to persistent data space into
 * this buffer so that it can always guarantee, at commit time, that everything in the buffer
 * is written or nothing at all is written.
 * The <code>JCSystem</code> includes methods to  control an atomic transaction.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em> for details.
 * <p>
 * @see SystemException
 * @see TransactionException
 * @see Applet
 */

public final class JCSystem
{
    private static final short API_VERSION = 0x0201;
    static PrivAccess thePrivAccess; // initialized by Dispatcher

    /**
     * Only JCRE can use constructor.
     */
    JCSystem(){}

    /**
     * This event code indicates that the object is not transient.
     */
    public static final byte NOT_A_TRANSIENT_OBJECT = 0;

    /**
     * This event code indicates that the contents of the transient object are cleared
     * to the default value on card reset ( or power on ) event.
     */
    public static final byte CLEAR_ON_RESET = 1;

    /**
     * This event code indicates that the contents of the transient object are cleared
     * to the default value on applet deselection event or in <code>CLEAR_ON_RESET</code> cases.
     * <p>Notes:<ul>
     * <li><code>CLEAR_ON_DESELECT</code><em> transient objects can be accessed only when the applet
     * which created the object is the currently the selected applet.</em>
     * <li><em>The JCRE will throw a </em><code>SecurityException</code><em> if a
     * </em><code>CLEAR_ON_DESELECT</code><em> transient
     * object is accessed when the currently selected applet is not the applet which created the object.</em>
     * </ul>
     */
    public static final byte CLEAR_ON_DESELECT = 2;

    /**
     * Used to check if the specified object is transient.
     * <p>Notes:<ul>
     * <em>This method returns </em><code>NOT_A_TRANSIENT_OBJECT</code><em> if the specified object is 
     * <code>null</code> or is not an array type.</em>
     * </ul>
     * @param theObj the object being queried.
     * @return <code>NOT_A_TRANSIENT_OBJECT</code>, <code>CLEAR_ON_RESET</code>, or <code>CLEAR_ON_DESELECT</code>.
     * @see #makeTransientBooleanArray(short, byte)
     * @see #makeTransientByteArray(short, byte)
     * @see #makeTransientShortArray(short, byte)
     * @see #makeTransientObjectArray(short, byte)
     */

    /* @ public normal_behavior
      @   requires true;
      @   assignable \nothing;
      @   ensures true;
      @*/
    public static native /* @ pure @*/ byte isTransient(Object theObj);

    /**
     * Create a transient boolean array with the specified array length.
     * @param length the length of the boolean array.
     * @param event the <code>CLEAR_ON...</code> event which causes the array elements to be cleared.
     * @exception SystemException with the following reason codes:<ul>
     * <li><code>SystemException.ILLEGAL_VALUE</code> if event is not a valid event code.
     * <li><code>SystemException.NO_TRANSIENT_SPACE</code> if sufficient transient space is not available.
     * <li><code>SystemException.ILLEGAL_TRANSIENT</code> if the current applet context
     * is not the currently selected applet context and <code>CLEAR_ON_DESELECT</code> is specified.
     * </ul>
     */

    /* @ public normal_behavior
      @   ensures true;
      @*/

    public static native boolean[] makeTransientBooleanArray(short length, byte event) throws SystemException;

    /**
     * Create a transient byte array with the specified array length.
     * @param length the length of the byte array.
     * @param event the <code>CLEAR_ON...</code> event which causes the array elements to be cleared.
     * @exception SystemException with the following reason codes:<ul>
     * <li><code>SystemException.ILLEGAL_VALUE</code> if event is not a valid event code.
     * <li><code>SystemException.NO_TRANSIENT_SPACE</code> if sufficient transient space is not available.
     * <li><code>SystemException.ILLEGAL_TRANSIENT</code> if the current applet context
     * is not the currently selected applet context and <code>CLEAR_ON_DESELECT</code> is specified.
     * </ul>
     */

    /*@ behavior normal:
      @   ensures \result.length == length;
      @*/
    public static native byte[] makeTransientByteArray(short length, byte event) 
	throws SystemException;
    
     /**
     * Create a transient short array with the specified array length.
     * @param length the length of the short array.
     * @param event the <code>CLEAR_ON...</code> event which causes the array elements to be cleared.
     * @exception SystemException with the following reason codes:<ul>
     * <li><code>SystemException.ILLEGAL_VALUE</code> if event is not a valid event code.
     * <li><code>SystemException.NO_TRANSIENT_SPACE</code> if sufficient transient space is not available.
     * <li><code>SystemException.ILLEGAL_TRANSIENT</code> if the current applet context
     * is not the currently selected applet context and <code>CLEAR_ON_DESELECT</code> is specified.
     * </ul>
     */

    /* @ public normal_behavior
      @   ensures true;
      @*/
    public static native short[] makeTransientShortArray(short length, byte event) throws SystemException;
    
    /**
     * Create a transient array of <code>Object</code> with the specified array length.
     * @param length the length of the <code>Object</code> array.
     * @param event the <code>CLEAR_ON...</code> event which causes the array elements to be cleared.
     * @exception SystemException with the following reason codes:<ul>
     * <li><code>SystemException.ILLEGAL_VALUE</code> if event is not a valid event code.
     * <li><code>SystemException.NO_TRANSIENT_SPACE</code> if sufficient transient space is not available.
     * <li><code>SystemException.ILLEGAL_TRANSIENT</code> if the current applet context
     * is not the currently selected applet context and <code>CLEAR_ON_DESELECT</code> is specified.
     * </ul>
     */

    /* @ public normal_behavior
      @   ensures true;
      @*/

    public static native Object[] makeTransientObjectArray(short length, byte event) throws SystemException;

    /**
     * Returns the current major and minor version of the Java Card API.
     * @return version number as byte.byte (major.minor)
     */
    public static short getVersion()
    {
        return API_VERSION;
    }

    /**
     * Returns the JCRE owned instance of the <code>AID</code> object associated with
     * the current applet context.
     * Returns <code>null</code> if the <code>Applet.register()</code> method
     * has not yet been invoked.
     * <p>JCRE owned instances of <code>AID</code> are permanent JCRE
     * Entry Point Objects and can be accessed from any applet context.
     * References to these permanent objects can be stored and re-used.
     * <p>See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
     * @return the <code>AID</code> object.
     */
    
    public static AID getAID()
    {
	    return thePrivAccess.getAID( PrivAccess.getCurrentAppID() );
    }

    /**
     * Returns the JCRE owned instance of the <code>AID</code> object, if any, 
     * encapsulating the specified AID bytes in the <code>buffer</code> parameter
     * if there exists a successfully installed applet on the card whose instance AID
     * exactly matches that of the specified AID bytes.
     * <p>JCRE owned instances of <code>AID</code> are permanent JCRE
     * Entry Point Objects and can be accessed from any applet context.
     * References to these permanent objects can be stored and re-used.
     * <p>See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
     * @param buffer byte array containing the AID bytes.
     * @param offset offset within buffer where AID bytes begin.
     * @param length length of AID bytes in buffer.
     * @return the <code>AID</code> object, if any; <code>null</code> otherwise. A VM exception
     * is thrown if <code>buffer</code> is <code>null</code>,
     * or if <code>offset</code> or <code>length</code> are out of range.
     */

    /* @ // Claude Marche', borrowed from LOOP project specification
      @ public behavior 
      @    requires true;
      @  assignable \nothing; 
      @     ensures 
      @        buffer != null && offset >= 0 && length >= 0 && 
      @           offset+length <= buffer.length 
      @        //&& 
      @        //(\result != null ==> \result.equals(buffer,offset,length) )
      @          ;
      @     signals (NullPointerException) 
      @              buffer == null;
      @     signals (ArrayIndexOutOfBoundsException)
      @              buffer != null &&
      @              (offset < 0 || length < 0 || offset+length > buffer.length);
      @*/
    public static /* @ pure @*/ AID lookupAID( byte[] buffer, short offset, byte length )
    throws NullPointerException,          // not in original source
           ArrayIndexOutOfBoundsException // not in original source
    {
        byte test = buffer[0]; // throw NullPointerException if buffer==null
        return thePrivAccess.getAID(buffer, offset, length);
    }

    /**
     * Begins an atomic transaction. If a transaction is already in
     * progress (transactionDepth != 0), a TransactionException is
     * thrown.
     * @exception TransactionException with the following reason codes:<ul>
     * <li><code>TransactionException.IN_PROGRESS</code> if a transaction is already in progress.</ul>
     * @see #commitTransaction()
     * @see #abortTransaction()
     */

    // specs to avoid Why error msg 'Exception TransactionException_exc cannot be raised' (Nicolas)

    /* @ behavior normal:
      @   ensures true;
      @ behavior exc:
      @   signals (TransactionException) true;
      @*/
    public static native void beginTransaction() throws TransactionException;
 
    /**
     * Aborts the atomic transaction. The contents of the commit
     * buffer is discarded.
     * <p>Notes:<ul>
     * <li><em>Do not call this method from within a transaction which creates new objects because
     * the JCRE may not recover the heap space used by the new object instances.</em>
     * <li><em>Do not call this method from within a transaction which creates new objects because
     * the JCRE may, to ensure the security of the card and to avoid heap space loss,
     * lock up the card session to force tear/reset processing.</em> 
     * <li><em>The JCRE ensures that any variable of reference type which references an object
     * instantiated from within this aborted transaction is equivalent to 
     * a </em><code>null</code><em> reference.</em>  
     * </ul>
     * @exception TransactionException with the following reason codes:<ul>
     * <li><code>TransactionException.NOT_IN_PROGRESS</code> if a transaction is not in progress.</ul>
     * @see #beginTransaction()
     * @see #commitTransaction()
     */

    /* @ behavior normal:
      @   ensures true;
      @ behavior exc:
      @   signals (TransactionException) true;
      @*/

    public static native void abortTransaction() throws TransactionException;

    /**
     * Commits an atomic transaction. The contents of commit
     * buffer is atomically committed. If a transaction is not in
     * progress (transactionDepth == 0) then a TransactionException is
     * thrown.
     * @exception TransactionException with the following reason codes:<ul>
     * <li><code>TransactionException.NOT_IN_PROGRESS</code> if a transaction is not in progress.</ul>
     * @see #beginTransaction()
     * @see #abortTransaction()
     */

    /* @ behavior normal:
      @   ensures true;
      @ behavior exc:
      @   signals (TransactionException) true;
      @*/

    public static native void commitTransaction() throws TransactionException;

    /**
     * Returns the current transaction nesting depth level. At present,
     * only 1 transaction can be in progress at a time.
     * @return 1 if transaction in progress, 0 if not.
     */

    /* @ public normal_behavior
      @   ensures true;
      @*/

    public static native /* @ pure @*/ byte getTransactionDepth();

    /**
     * Returns the number of bytes left in the commit buffer.
     * @return the number of bytes left in the commit buffer
     * @see #getMaxCommitCapacity()
     */
    public static native short getUnusedCommitCapacity();

    /**
     * Returns the total number of bytes in the commit buffer.
     * This is approximately the maximum number of bytes of
     * persistent data which can be modified during a transaction.
     * However, the transaction subsystem requires additional bytes
     * of overhead data to be included in the commit buffer, and this
     * depends on the number of fields modified and the implementation
     * of the transaction subsystem. The application cannot determine
     * the actual maximum amount of data which can be modified during
     * a transaction without taking these overhead bytes into consideration.
     * @return the total number of bytes in the commit buffer
     * @see #getUnusedCommitCapacity()
     */
    public static native short getMaxCommitCapacity();

    /**
     * This method is called to obtain the JCRE owned instance of the <code>AID</code> object associated
     * with the previously active applet context. This method is typically used by a server applet,
     * while executing a shareable interface method to determine the identity of its client and
     * thereby control access privileges.
     * <p>JCRE owned instances of <code>AID</code> are permanent JCRE
     * Entry Point Objects and can be accessed from any applet context.
     * References to these permanent objects can be stored and re-used.
     * <p>See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
     * @return the <code>AID</code> object of the previous context, or <code>null</code> if JCRE.
     */
    public static AID getPreviousContextAID()
    {
        return thePrivAccess.getAID( (byte)
                (NativeMethods.getPreviousContext() & PrivAccess.APPID_BITMASK)
                );
    }
    
    /**
     * This method is called by a client applet to get a server applet's
     * shareable interface object. <p>This method returns <code>null</code>
     * if the <code>Applet.register()</code> has not yet been invoked or
     * if the server does not exist or if the server returns <code>null</code>.
     * @param serverAID the AID of the server applet.
     * @param parameter optional parameter data.
     * @return the shareable interface object or <code>null</code>.
     * @see javacard.framework.Applet#getShareableInterfaceObject(AID, byte)
     */
    public static Shareable getAppletShareableInterfaceObject(AID serverAID, byte param /* CM parameter */)
    {  
	    return thePrivAccess.getSharedObject(serverAID, param /* CM parameter */);
    }

}
