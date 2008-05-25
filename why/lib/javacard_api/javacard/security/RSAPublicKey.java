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
// $Workfile: RSAPublicKey.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/RSAPublicKey.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The <code>RSAPublicKey</code> is used to verify signatures on signed data using the RSA algorithm.
 * It may also used by the <code>javacardx.crypto.Cipher</code> class to encrypt/decrypt messages.
 * <p>When both the modulus and exponent of the key are set, the key is
 * initialized and ready for use.
 * @see RSAPrivateKey
 * @see RSAPrivateCrtKey
 * @see KeyBuilder
 * @see Signature
 * @see javacardx.crypto.Cipher
 * @see javacardx.crypto.KeyEncryption
 */

public interface RSAPublicKey extends PublicKey{

   /**
   * Sets the modulus value of the key.
   * The plaintext data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte). Input modulus data is copied into the internal representation.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer at which the modulus value begins
   * @param length the byte length of the modulus
   * @exception CryptoException with the following reason code:<ul>
   * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input modulus data length is inconsistent
   * with the implementation or if input data decryption is required and fails.
   * </ul>
   * <p>Note:<ul>
   * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
   * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
   * is not </em><code>null</code><em>, the modulus value is decrypted using the </em><code>Cipher</code><em> object.</em>
   * </ul>
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    void setModulus( byte[] buffer, short offset, short length) throws CryptoException;

  /**
   * Sets the public exponent value of the key.
   * The plaintext data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte). Input exponent data is copied into the internal representation.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer at which the exponent value begins
   * @param length the byte length of the exponent
   * @exception CryptoException with the following reason code:<ul>
   * <li><code>CryptoException.ILLEGAL_VALUE</code> if the input exponent data length is inconsistent
   * with the implementation or if input data decryption is required and fails.
   * </ul>
   * <p>Note:<ul>
   * <li><em>If the key object implements the </em><code>javacardx.crypto.KeyEncryption</code><em>
   * interface and the </em><code>Cipher</code><em> object specified via </em><code>setKeyCipher()</code><em>
   * is not </em><code>null</code><em>, the exponent value is decrypted using the </em><code>Cipher</code><em> object.</em>
   * </ul>
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    void setExponent( byte[] buffer, short offset, short length) throws CryptoException;

  /**
   * Returns the modulus value of the key in plain text.
   * The data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte).
   * @param buffer the output buffer
   * @param offset the offset into the input buffer at which the modulus value starts
   * @return the byte length of the modulus value returned
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    short getModulus( byte[] buffer, short offset );

  /**
   * Returns the private exponent value of the key in plain text.
   * The data format is big-endian and right-aligned (the least significant bit is the least significant
   * bit of last byte).
   * @param buffer the output buffer
   * @param offset the offset into the output buffer at which the exponent value begins
   * @return the byte length of the public exponent returned
   */

    /*@ public normal_behavior
      @   ensures true;
      @*/

    short getExponent( byte[] buffer, short offset );

 }
