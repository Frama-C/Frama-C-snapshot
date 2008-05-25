/*
* $Workfile: OwnerPIN.java $	$Revision: 1.2 $, $Date: 2007/09/26 15:15:36 $
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
// $Workfile: OwnerPIN.java $
// $Revision: 1.2 $
// $Date: 2007/09/26 15:15:36 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/OwnerPIN.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * This class represents an Owner PIN. It implements Personal Identification Number 
 * functionality as defined in the <code>PIN</code> interface. It
 * provides the ability to update the PIN and thus owner functionality.<p>
 * 
 * The implementation of this class must protect against attacks based on program
 * flow prediction.Even if a transaction is in progress, internal state
 * such as the try counter, the validated flag and the blocking state must not
 * be conditionally updated during PIN presentation.<p>
 * 
 * If an implementation of this class creates transient arrays, it must ensure that they are
 * <code>CLEAR_ON_RESET</code> transient objects.<p>
 * 
 * The protected methods <code>getValidatedFlag</code> and
 * <code>setValidatedFlag</code> allow a subclass
 * of this class to optimize the storage for the validated boolean state.<p>
 *
 * Some methods of instances of this class are only suitable for sharing when there exists
 * a trust relationship among the applets. A typical shared usage would use
 * a proxy PIN interface which implements both the <code>PIN</code> interface and
 * the <code>Shareable</code> interface.
 * <p>Any of the methods of the <code>OwnerPIN</code> may be called with a transaction in  
 * progress. None of the methods of <code>OwnerPIN</code> class initiate or alter the state
 * of the transaction if one is in progress.
 * @see PINException
 * @see PIN
 * @see Shareable
 * @see JCSystem
 */
public class OwnerPIN implements PIN{


  /**
   * try limit, the maximum number of times an incorrect PIN can be
   * presented before the PIN is blocked. When the PIN is blocked, it
   * cannot be validated even on valid PIN presentation.
   */
    private /* @ spec_public */ byte tryLimit;

  /**
   * max PIN size, the maximum length of PIN allowed
   */
  private byte maxPINSize;

  /**
   * PIN value
   */
    private byte[] pinValue;

  /**
   * the current size of PIN array.
   */
  private byte pinSize;

  /**
   * validated flag, true if a valid PIN has been presented. This flag is
   * reset on every card reset.
   */
    // @ invariant flags instanceof boolean[];
    private boolean[] flags; //default null

  private static final byte VALIDATED = (byte)0;
  private static final byte NUMFLAGS = (byte)(VALIDATED+1);
  
  /**
   * Create and initialize Flags array.
   */
  private void createFlags() {
    if (flags!=null) return;
    flags = JCSystem.makeTransientBooleanArray(NUMFLAGS, JCSystem.CLEAR_ON_RESET);
    setValidatedFlag(false);
    }

  /**
   * This protected method returns the validated flag.
   * This method is intended for subclass of this <code>OwnerPIN</code> to access or
   * override the internal PIN state of the <code>OwnerPIN</code>.
   * @return the boolean state of the PIN validated flag.
   */
    /* @public normal_behavior
      @ensures \result == flags[VALIDATED];
      @*/
    protected /* @ pure */ boolean getValidatedFlag() {
    createFlags();
    return flags[VALIDATED];
    }

  /**
   * This protected method sets the value of the validated flag.
   * This method is intended for subclass of this <code>OwnerPIN</code> to control or
   * override the internal PIN state of the <code>OwnerPIN</code>.
   * @param value the new value for the validated flag.
   */
    // Modified by Xavier (boolean value) -> (boolean v) to avoid name clashes.
  protected void setValidatedFlag(boolean v) {
    createFlags();
    flags[VALIDATED] = v;
    }
    
  /**
   * tries left, the remaining number of times an incorrect PIN
   * presentation is permitted. It is declared as an array so
   * that <code>arrayCopyNonAtomic</code> can be used during decrement.
   */
    private byte[] triesLeft;

  /**
   * tries : temporary byte. This byte is used while decrementing triesLeft field
   * in persistent space, to ensure unconditional update.
   */
    // @ invariant temps instanceof byte[];
    private byte[] temps; // default 0
    
  private static final byte TRIES = (byte)0;
  private static final byte NUMTEMPS = (byte)(TRIES+1);
  
  /**
   * Reset triesLeft field to tryLimit.
   */
  private void resetTriesRemaining() {
    triesLeft[0] = tryLimit;
  }
  
 /**
   * Decrement triesLeft field in persistent space by 1. Ensure that the update is
   * unconditional even if a transaction is in progress.
   */
  private void decrementTriesRemaining() {
    if (temps==null) temps = JCSystem.makeTransientByteArray(NUMTEMPS, JCSystem.CLEAR_ON_RESET);
    temps[TRIES] = (byte)(triesLeft[0]-1);
    Util.arrayCopyNonAtomic( temps, TRIES, triesLeft, (short)0, (short)1 );
  }
    
  /**
   * Constructor. Allocates a new <code>PIN</code> instance with validated flag
   * set to <code>false</code>.
   * @param tryLimit the maximum number of times an incorrect PIN can be presented. <code>tryLimit</code> must be >=1.
   * @param maxPINSize the maximum allowed PIN size. <code>maxPINSize</code> must be >=1.
   * @exception PINException with the following reason codes:<ul>
   * <li><code>PINException.ILLEGAL_VALUE</code> if <code>tryLimit</code> parameter is less than 1.
   * <li><code>PINException.ILLEGAL_VALUE</code> if <code>maxPINSize</code> parameter is less than 1.</ul>
   */
   
    /* @invariant
      @triesLeft instanceof byte[] &&
      @pinValue instanceof byte[] ;
      @*/
  public OwnerPIN(byte tryLimit, byte maxPINSize) throws PINException{
        if ((tryLimit<1) || (maxPINSize<1)) PINException.throwIt( PINException.ILLEGAL_VALUE );
    pinValue = new byte[maxPINSize]; //default value 0
    this.pinSize = maxPINSize; //default
    this.maxPINSize = maxPINSize;
    this.tryLimit = tryLimit;
    triesLeft = new byte[1];
    resetTriesRemaining();
    }

  /**
   * Returns the number of times remaining that an incorrect PIN can
   * be presented before the <code>PIN</code> is blocked.
   *
   * @return the number of times remaining
   */
    /* @public normal_behavior
      @modifiable \nothing;
      @ensures \result==triesLeft[0];
      @*/
    public /* @ pure */ byte getTriesRemaining(){
    return triesLeft[0];
    }

  /**
   * Compares <code>pin</code> against the PIN value. If they match and the
   * <code>PIN</code> is not blocked, it sets the validated flag
   * and resets the try counter to its maximum. If it does not match,
   * it decrements the try counter and, if the counter has reached
   * zero, blocks the <code>PIN</code>. Even if a transaction is in progress, internal state
   * such as the try counter, the validated flag and the blocking state
   * must not be conditionally updated.
   * <p>
   * Notes:<ul>
   * <li><em>If </em><code>NullPointerException</code><em> or </em><code>ArrayIndexOutOfBoundsException</code><em> is
   * thrown, the validated flag must be set to false, the try counter must be decremented
   * and, the <code>PIN</code> blocked if the counter reaches zero.</em>
   * <li><em>If </em><code>offset</code><em> or </em><code>length</code><em> parameter
   * is negative an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
   * <li><em>If </em><code>offset+length</code><em> is greater than </em><code>pin.length</code><em>, the length
   * of the </em><code>pin</code><em> array, an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
   * <li><em>If </em><code>pin</code><em> parameter is </em><code>null</code><em>
   * a </em><code>NullPointerException</code><em> exception is thrown.</em></ul>
   * @param pin the byte array containing the PIN value being checked
   * @param offset the starting offset in the <code>pin</code> array
   * @param length the length of <code>pin</code>.
   * @return <code>true</code> if the PIN value matches; <code>false</code> otherwise
   * @exception java.lang.ArrayIndexOutOfBoundsException
   * - if the check operation would cause access of data outside array bounds.
   * @exception java.lang.NullPointerException - if <code>pin</code> is <code>null</code> 
   */
  public boolean check(byte[] pin, short offset, byte length)
  throws ArrayIndexOutOfBoundsException, NullPointerException{
    setValidatedFlag(false);
    if ( getTriesRemaining() == 0 ) return false;
    decrementTriesRemaining();
    if (length!=pinSize) return false;
    if ( Util.arrayCompare( pin, offset, pinValue, (short)0, (short)length )==(byte)0 ) {
        setValidatedFlag(true);
        resetTriesRemaining();
        return true;
        }
    return false;
    }

  /**
   * Returns <code>true</code> if a valid PIN has been presented since the last
   * card reset or last call to <code>reset()</code>.
   *
   * @return <code>true</code> if validated; <code>false</code> otherwise
   */
    /* @public normal_behavior
      @ensures \result<==>getValidatedFlag();
      @*/
    public /* @ pure */ boolean isValidated(){
    return getValidatedFlag();
    }

  /**
   * If the validated flag is set, this method resets it.
   * If the validated flag is not set, this method does nothing.
   */
  public void reset(){
    if (isValidated()) resetAndUnblock();
    }

  /**
   * This method sets a new value for the PIN and resets the <code>PIN</code> try
   * counter to the value of the <code>PIN</code> try limit. It also resets the validated flag.<p>
   * This method copies the input pin parameter into an internal representation. If a transaction is
   * in progress, the new pin and try counter update must be conditional i.e
   * the copy operation must use the transaction facility. 
   * @param pin the byte array containing the new PIN value
   * @param offset the starting offset in the pin array
   * @param length the length of the new PIN.
   * @exception PINException with the following reason codes:<ul>
   * <li><code>PINException.ILLEGAL_VALUE</code> if length is greater than configured maximum PIN size.</ul>
   * @see javacard.framework.JCSystem#beginTransaction()
   */
  public void update(byte[] pin, short offset, byte length)
  throws PINException{
    if ( length>maxPINSize ) PINException.throwIt( PINException.ILLEGAL_VALUE );
    Util.arrayCopy( pin, offset, pinValue, (short)0, length );
    pinSize = length;
    resetTriesRemaining();
    setValidatedFlag(false);
	}

  /**
   * This method resets the validated flag and
   * resets the <code>PIN</code> try counter to the value of the <code>PIN</code> try limit.
   * This method is used by the owner to re-enable the blocked <code>PIN</code>.
   */
  public void resetAndUnblock()
    {
    resetTriesRemaining();
    setValidatedFlag(false);
	}
}
