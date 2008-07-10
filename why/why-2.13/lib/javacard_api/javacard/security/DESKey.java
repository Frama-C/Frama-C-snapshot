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
// $Workfile: DESKey.java $
// $Revision: 1.3 $
// $Date: 2007/10/22 07:38:21 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/DESKey.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

/**
 * <code>DESKey</code> contains an 8/16/24 byte key for single/2 key triple DES/3 key triple DES
 * operations.
 * <p>When the key data is set, the key is initialized and ready for use.
 *<p>
 * @see KeyBuilder
 * @see Signature
 * @see javacardx.crypto.Cipher
 * @see javacardx.crypto.KeyEncryption
 */
public interface DESKey extends SecretKey{
    
    /**
     * Sets the <code>Key</code> data. The plaintext length of input key data is 8 bytes for DES,
     * 16 bytes for 2 key triple DES and 24 bytes for 3 key triple DES.
     * The data format is big-endian and right-aligned (the least significant bit is the least significant
     * bit of last byte). Input key data is copied into the internal representation.
     * @param keyData byte array containing key initialization data
     * @param kOff offset within keyData to start
     * @exception CryptoException with the following reason code:<ul>
     * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input key data length is inconsistent
     * with the implementation or if input data decryption is required and fails.
     * </ul>
     * <p>Note:<ul>
     * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
     * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
     * is not </em><code>null</code><em>, </em><code>keyData</code><em> is decrypted using the </em><code>Cipher</code><em> object.</em>
     * </ul>
     */
    void setKey( byte[] keyData, short kOff ) throws CryptoException;

    /**
     * Returns the <code>Key</code> data in plain text. The length of output key data is 8 bytes for DES,
     * 16 bytes for 2 key triple DES and 24 bytes for 3 key triple DES.
     * The data format is big-endian and right-aligned (the least significant bit is the least significant
     * bit of last byte).
     * @param keyData byte array to return key data
     * @param kOff offset within <code>keyData</code> to start.
     * @return the byte length of the key data returned.
     */
    byte getKey( byte[] keyData, short kOff );

    }
