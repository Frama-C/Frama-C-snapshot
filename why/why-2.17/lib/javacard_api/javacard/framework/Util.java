/*
* $Workfile: Util.java $	$Revision: 1.2 $, $Date: 2007/09/26 15:15:36 $
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
// $Workfile: Util.java $
// $Revision: 1.2 $
// $Date: 2007/09/26 15:15:36 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/Util.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * The <code>Util</code> class contains common utility functions.
 * Some of the methods may be implemented as native functions for
 * performance reasons.
 * All methods in <code>Util</code>, class are static methods.<p>
 * Some methods of <code>Util</code> namely <code>arrayCopy()</code>, <code>arrayCopyNonAtomic()</code>,
 * <code>arrayFillNonAtomic()</code> and <code>setShort()</code>, refer to the persistence of
 * array objects. The term <em>persistent</em> means that arrays and their values persist from
 * one CAD session to the next, indefinitely.
 * The <code>JCSystem</code> class is used to control the persistence and transience of objects.
 * @see javacard.framework.JCSystem
 */

public class Util
{

    /**
     * Only JCRE can use constructor.
     * No need to construct this class anyway, since it is all statics.
     */
    Util(){}

    /**
     * Copies an array from the specified source array,
     * beginning at the specified position,
     * to the specified position of the destination array.
     * <p>
     * Notes:<ul>
     * <li><em>If </em><code>srcOff</code><em> or </em><code>destOff</code><em> or </em><code>length</code><em> parameter
     * is negative an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>srcOff+length</code><em> is greater than </em><code>src.length</code><em>, the length
     * of the </em><code>src</code><em> array a </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown
     * and no copy is performed.</em>
     * <li><em>If </em><code>destOff+length</code><em> is greater than </em><code>dest.length</code><em>, the length
     * of the </em><code>dest</code><em> array an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown
     * and no copy is performed.</em>
     * <li><em>If </em><code>src</code><em> or </em><code>dest</code><em> parameter is </em><code>null</code><em>
     * a </em><code>NullPointerException</code><em> exception is thrown.</em>
     * <li><em>If the <code>src</code> and <code>dest</code> arguments refer to the same array object,
     * then the copying is performed as if the components at positions </em><code>srcOff</code><em>
     * through </em><code>srcOff+length-1</code><em> were first copied to a temporary array with length components
     * and then the contents of the temporary array were copied into
     * positions </em><code>destOff</code><em> through </em><code>destOff+length-1</code><em> of the argument array.</em>
     * <li><em>If the destination array is persistent, the entire copy is performed atomically.</em>
     * <li><em>The copy operation is subject to atomic commit capacity limitations.
     * If the commit capacity is exceeded, no copy is performed and a </em><code>TransactionException</code><em>
     * exception is thrown.</em>
     * </ul>
     * @param src source byte array.
     * @param srcOff offset within source byte array to start copy from.
     * @param dest destination byte array.
     * @param destOff offset within destination byte array to start copy into.
     * @param length byte length to be copied.
     * @return <code>destOff+length</code>
     * @exception java.lang.ArrayIndexOutOfBoundsException
     * - if copying would cause access of data outside array bounds.
     * @exception java.lang.NullPointerException - if either <code>src</code> or <code>dest</code> is <code>null</code>.
     * @exception javacard.framework.TransactionException
     * - if copying would cause the commit capacity to be exceeded.
     * @see javacard.framework.JCSystem#getUnusedCommitCapacity()
     */
    /* @ normal_behavior
      @  requires src != null &&  srcOff >= 0 &&
      @              srcOff+length <= src.length &&
      @            dest != null && destOff >= 0 &&
      @              destOff+length <= dest.length &&
      @            length >= 0;
      @  modifiable dest[destOff..destOff+length-1];
      @     ensures (\forall short i ; 0 <= i && i < length ;
      @                    dest[destOff+i] == \old(src[srcOff+i]));
      @*/
    public static final native short arrayCopy(byte[] src, short srcOff, byte[] dest, short destOff, short length)
    throws ArrayIndexOutOfBoundsException, NullPointerException, TransactionException;

    /**
     * Copies an array from the specified source array,
     * beginning at the specified position,
     * to the specified position of the destination array (non-atomically).
     * <p>This method does not use the transaction facility during the copy operation even if
     * a transaction is in progress. Thus, this
     * method is suitable for use only when the contents of the destination array can be left in
     * a partially modified state in the event of a power loss in the middle of the copy operation.
     * <p>
     * Notes:<ul>
     * <li><em>If </em><code>srcOff</code><em> or </em><code>destOff</code><em> or </em><code>length</code><em> parameter
     * is negative an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>srcOff+length</code><em> is greater than </em><code>src.length</code><em>, the length
     * of the </em><code>src</code><em> array a </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown
     * and no copy is performed.</em>
     * <li><em>If </em><code>destOff+length</code><em> is greater than </em><code>dest.length</code><em>, the length
     * of the </em><code>dest</code><em> array an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown
     * and no copy is performed.</em>
     * <li><em>If </em><code>src</code><em> or </em><code>dest</code><em> parameter is </em><code>null</code><em>
     * a </em><code>NullPointerException</code><em> exception is thrown.</em>
     * <li><em>If the <code>src</code> and <code>dest</code> arguments refer to the same array object,
     * then the copying is performed as if the components at positions </em><code>srcOff</code><em>
     * through </em><code>srcOff+length-1</code><em> were first copied to a temporary array with length components
     * and then the contents of the temporary array were copied into
     * positions </em><code>destOff</code><em> through </em><code>destOff+length-1</code><em> of the argument array.</em>
     * <li><em>If power is lost during the copy operation and the destination array is persistent,
     * a partially changed destination array could result.</em>
     * <li><em>The copy </em><code>length</code><em> parameter is not constrained by the atomic commit capacity limitations.</em></ul>
     * @param src source byte array.
     * @param srcOff offset within source byte array to start copy from.
     * @param dest destination byte array.
     * @param destOff offset within destination byte array to start copy into.
     * @param length byte length to be copied.
     * @return <code>destOff+length</code>
     * @exception java.lang.ArrayIndexOutOfBoundsException
     * - if copying would cause access of data outside array bounds.
     * @exception java.lang.NullPointerException - if either <code>src</code> or <code>dest</code> is <code>null</code>.
     * @see javacard.framework.JCSystem#getUnusedCommitCapacity()
     */

    /* @ public normal_behavior
      @   requires src != null;
      @   requires srcOff >= 0;
      @   requires srcOff + length <= src.length;
      @   requires dest != null;
      @   requires destOff >= 0;
      @   requires destOff + length <= dest.length;
      @   requires length >= 0;
      @
      @   assignable dest[destOff..destOff + length - 1];
      @
      @   ensures (\forall short i; 0 <= i && i < length; dest[destOff + i] == \old(src[srcOff + i]));
      @*/

    public static final native short arrayCopyNonAtomic(byte[] src, short srcOff, byte[] dest, short destOff, short length)
	throws ArrayIndexOutOfBoundsException, NullPointerException;


    /**
     * Fills the byte array (non-atomically) beginning at the specified position,
     * for the specified length with the specified byte value.
     * <p>This method does not use the transaction facility during the fill operation even if
     * a transaction is in progress. Thus, this
     * method is suitable for use only when the contents of the byte array can be left in
     * a partially filled state in the event of a power loss in the middle of the fill operation.
     * <p>
     * Notes:<ul>
     * <li><em>If </em><code>bOff</code><em> or </em><code>bLen</code><em> parameter
     * is negative an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>bOff+bLen</code><em> is greater than </em><code>bArray.length</code><em>, the length
     * of the </em><code>bArray</code><em> array an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>bArray</code><em> parameter is </em><code>null</code><em>
     * a </em><code>NullPointerException</code><em> exception is thrown.</em>
     * <li><em>If power is lost during the copy operation and the byte array is persistent,
     * a partially changed byte array could result.</em>
     * <li><em>The </em><code>bLen</code><em> parameter is not constrained by the atomic commit capacity limitations.</em></ul>
     * @param bArray the byte array.
     * @param bOff offset within byte array to start filling bValue into.
     * @param bLen byte length to be filled.
     * @param bValue the value to fill the byte array with.
     * @return <code>bOff+bLen</code>
     * @exception java.lang.ArrayIndexOutOfBoundsException
     * - if the fill operation would cause access of data outside array bounds.
     * @exception java.lang.NullPointerException - if bArray is <code>null</code>
     * @see javacard.framework.JCSystem#getUnusedCommitCapacity()
     */
    public static final native short arrayFillNonAtomic(byte[] bArray, short bOff, short bLen, byte bValue)
    throws ArrayIndexOutOfBoundsException, NullPointerException;
   
    /**
     * Compares an array from the specified source array,
     * beginning at the specified position,
     * with the specified position of the destination array from left to right.
     * Returns the ternary result of the comparison : less than(-1), equal(0) or greater than(1).
     * <p>Notes:<ul>
     * <li><em>If </em><code>srcOff</code><em> or </em><code>destOff</code><em> or </em><code>length</code><em> parameter
     * is negative an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>srcOff+length</code><em> is greater than </em><code>src.length</code><em>, the length
     * of the </em><code>src</code><em> array a </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>destOff+length</code><em> is greater than </em><code>dest.length</code><em>, the length
     * of the </em><code>dest</code><em> array an </em><code>ArrayIndexOutOfBoundsException</code><em> exception is thrown.</em>
     * <li><em>If </em><code>src</code><em> or </em><code>dest</code><em> parameter is </em><code>null</code><em>
     * a </em><code>NullPointerException</code><em> exception is thrown.</em>
     * </ul>
     * @param src source byte array.
     * @param srcOff offset within source byte array to start compare.
     * @param dest destination byte array.
     * @param destOff offset within destination byte array to start compare.
     * @param length byte length to be compared.
     * @return the result of the comparison as follows:<ul>
     * <li> <code>0</code> if identical
     * <li> <code>-1</code> if the first miscomparing byte in source array is less than that in destination array,
     * <li> <code>1</code> if the first miscomparing byte in source array is greater that that in destination array.</ul>
     * @exception java.lang.ArrayIndexOutOfBoundsException
     * - if comparing all bytes would cause access of data outside array bounds.
     * @exception java.lang.NullPointerException - if either <code>src</code> or <code>dest</code> is <code>null</code>.
     */
    // TEMP // This method might eventually be a native method

    /* @ public normal_behavior
      @   requires src != null;
      @   requires srcOff >= 0;
      @   requires srcOff + length <= src.length;
      @   requires dest != null;
      @   requires destOff >= 0;
      @   requires destOff + length <= dest.length;
      @   requires length >= 0;
      @
      @   assignable \nothing;
      @
      @   ensures -1 <= \result && \result <= 1;
      @   ensures \result == -1
      @           <==>
      @           (\exists short j; 0 <= j && j < length;
      @                    src[srcOff + j] < dest[destOff + j] &&
      @                    (\forall short i; 0 <= i && i < j; src[srcOff + i] == dest[destOff + i]));
      @   ensures \result == 0 
      @           <==>
      @           (\forall short i; 0 <= i && i < length; src[srcOff + i] == dest[destOff + i]);
      @   ensures \result == 1
      @           <==>
      @           (\exists short j; 0 <= j && j < length;
      @                    src[srcOff + j] > dest[destOff + j] &&
      @                    (\forall short i; 0 <= i && i < j; src[srcOff + i] == dest[destOff + i]));
      @*/

    public static final /* @ pure @*/ native byte arrayCompare(byte[] src, short srcOff, byte[] dest, short destOff, short length)
    throws ArrayIndexOutOfBoundsException, NullPointerException;

    /**
     * Concatenates the two parameter bytes to form a short value.
     * @param b1 the first byte ( high order byte ).
     * @param b2 the second byte ( low order byte ).
     * @return the short value - the concatenated result
     */
    public static final short makeShort( byte b1, byte b2 )
    {
        return (short)(((short)b1 << 8) + ((short)b2 & 0xFF));
    }

    /**
     * Concatenates two bytes in a byte array to form a short value.
     * @param bArray byte array.
     * @param bOff offset within byte array containing first byte (the high order byte).
     * @return the short value - the concatenated result
     */

    // Added by Xavier to avoid & in specs
    /* @ normal_behavior
      @// requires -128 <= b  && b <= 127 ;
      @ ensures (b >= 0 ==> \result==b) && (b < 0 ==> \result==b+256) ;
      @*/
    public static /* @ pure @*/ short k_unsigned(byte b){}

    // Is this ensures clause necessary ?
    //    ensures     (\result >> 8) == (int)bArray[bOff] &&
    //    (\result & 0x00ff) == (k_unsigned(bArray[bOff+1]));
      

    /* @ normal_behavior
      @  requires bArray != null && 
      @           bOff >= 0 &&
      @           bArray.length - 1 > bOff;
      @  modifiable \nothing;
      @     ensures \result == 256 * bArray[bOff]
      @                          + (k_unsigned(bArray[bOff+1]));
      @*/
    public static final /* @ pure @*/ short getShort( byte[] bArray, short bOff )
    {
        return (short)(( (short)(bArray[bOff]) << 8 ) +
                       ( (short)(bArray[(short)(bOff+1)]) & 0xFF));
    }

    /**
     * Deposits the short value as two successive bytes at the specified offset in the byte array.
     * @param bArray byte array.
     * @param bOff offset within byte array to deposit the first byte (the high order byte).
     * @param sValue the short value to set into array.<p>
     * @return <code>bOff+2</code>
     * <p>Note:<ul>
     * <li><em>If the byte array is persistent, this operation is performed atomically.
     * If the commit capacity is exceeded, no operation is performed and a </em><code>TransactionException</code><em>
     * exception is thrown.</em></ul>
     * @exception javacard.framework.TransactionException
     * - if the operation would cause the commit capacity to be exceeded.
     * @see javacard.framework.JCSystem#getUnusedCommitCapacity()
     */
    // TEMP // This method should eventually be a native method
    public static final native short setShort( byte[] bArray, short bOff, short sValue )
    throws TransactionException;
}
