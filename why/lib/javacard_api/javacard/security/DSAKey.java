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
// $Workfile: DSAKey.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/DSAKey.java $
// $Modtime: 5/02/00 8:48p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The </code>DSAKey</code> interface is the base interface for the DSA algorithms private and
 * public key implementations. A DSA private key implementation must also implement
 * the <code>DSAPrivateKey</code> interface methods. A DSA public key implementation must also implement
 * the <code>DSAPublicKey</code> interface methods.
 * <p>When all four components of the key (X or Y,P,Q,G) are set, the key is
 * initialized and ready for use.
 * <p>
 *
 * @see DSAPublicKey
 * @see DSAPrivateKey
 * @see KeyBuilder
 * @see Signature
 * @see javacardx.crypto.KeyEncryption
 *
 */

public interface DSAKey {

  /**
   * Sets the prime parameter value of the key.
   * The plaintext data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte). Input prime parameter data is copied into the internal representation.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer at which the prime parameter value begins
   * @param length the length of the prime parameter value
   * @exception CryptoException with the following reason code:<ul>
   * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input parameter data length is inconsistent
   * with the implementation or if input data decryption is required and fails.
   * </ul>
   * <p>Note:<ul>
   * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
   * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
   * is not </em><code>null</code><em>, the prime parameter value is decrypted using the </em><code>Cipher</code><em> object.</em>
   * </ul>
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    void setP( byte[] buffer, short offset, short length) throws CryptoException;

  /**
   * Sets the subprime parameter value of the key.
   * The plaintext data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte). Input subprime parameter data is copied into the internal representation.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer at which the subprime parameter value begins
   * @param length the length of the subprime parameter value
   * @exception CryptoException with the following reason code:<ul>
   * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input parameter data length is inconsistent
   * with the implementation or if input data decryption is required and fails.
   * </ul>
   * <p>Note:<ul>
   * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
   * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
   * is not </em><code>null</code><em>, the subprime parameter value is decrypted using the </em><code>Cipher</code><em> object.</em>
   * </ul>
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    void setQ( byte[] buffer, short offset, short length) throws CryptoException;

  /**
   * Sets the base parameter value of the key.
   * The plaintext data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte). Input base parameter data is copied into the internal representation.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer at which the base parameter value begins
   * @param length the length of the base parameter value
   * @exception CryptoException with the following reason code:<ul>
   * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input parameter data length is inconsistent
   * with the implementation or if input data decryption is required and fails.
   * </ul>
   * <p>Note:<ul>
   * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
   * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
   * is not </em><code>null</code><em>, the base parameter value is decrypted using the </em><code>Cipher</code><em> object.</em>
   * </ul>
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    void setG( byte[] buffer, short offset, short length) throws CryptoException;

  /**
   * Returns the prime parameter value of the key in plain text.
   * The data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte).
   * @param buffer the output buffer
   * @param offset the offset into the output buffer at which the prime parameter value starts
   * @return the byte length of the prime parameter value returned
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    short getP( byte[] buffer, short offset );

  /**
   * Returns the subprime parameter value of the key in plain text.
   * The data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte).
   * @param buffer the output buffer
   * @param offset the offset into the output buffer at which the subprime parameter value begins
   * @return the byte length of the subprime parameter value returned
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    short getQ( byte[] buffer, short offset );

  /**
   * Returns the base parameter value of the key in plain text.
   * The data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte).
   * @param buffer the output buffer
   * @param offset the offset into the output buffer at which the base parameter value begins
   * @return the byte length of the base parameter value returned
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    short getG( byte[] buffer, short offset );

 }
