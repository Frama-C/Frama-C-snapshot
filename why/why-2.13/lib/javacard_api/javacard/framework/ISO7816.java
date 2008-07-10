/*
* $Workfile: ISO7816.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: ISO7816.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/ISO7816.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * <code>ISO7816</code> encapsulates constants related to ISO 7816-3 and ISO 7816-4.
 * <code>ISO7816</code> interface contains only static fields.<p>
 * The static fields with <code>SW_</code> prefixes define constants for the ISO 7816-4 defined response
 * status word. The fields which use the <code>_00</code> suffix require the low order byte to be
 * customized appropriately e.g (ISO7816.SW_CORRECT_LENGTH_00 + (0x0025 & 0xFF)).<p>
 * The static fields with <code>OFFSET_</code> prefixes define constants to be used to index into
 * the APDU buffer byte array to access ISO 7816-4 defined header information.
 */
public interface ISO7816 {

  // Mnemonics for the SW1,SW2 error codes

  /**
   * Response status : No Error = (short)0x9000
   */
  short SW_NO_ERROR		      = (short)0x9000;

  /**
   * Response status : Response bytes remaining = 0x6100
   */
  short SW_BYTES_REMAINING_00 = 0x6100;

  /**
   * Response status : Wrong length = 0x6700
   */
  short SW_WRONG_LENGTH	      = 0x6700;

  /**
   * Response status : Security condition not satisfied = 0x6982
   */
  short SW_SECURITY_STATUS_NOT_SATISFIED = 0x6982;

  /**
   * Response status : File invalid = 0x6983
   */
  short SW_FILE_INVALID       = 0x6983;

  /**
   * Response status : Data invalid = 0x6984
   */
  short SW_DATA_INVALID	      = 0x6984;

  /**
   * Response status : Conditions of use not satisfied = 0x6985
   */
  short SW_CONDITIONS_NOT_SATISFIED	      = 0x6985;

  /**
   * Response status : Command not allowed (no current EF) = 0x6986
   */
  short SW_COMMAND_NOT_ALLOWED	      = 0x6986;
  
  /**
   * Response status : Applet selection failed = 0x6999;
   */
  short SW_APPLET_SELECT_FAILED	      = 0x6999;
  
  /**
   * Response status : Wrong data = 0x6A80
   */
  short SW_WRONG_DATA	      = 0x6A80;

  /**
   * Response status : Function not supported = 0x6A81
   */
  short SW_FUNC_NOT_SUPPORTED = 0x6A81;

  /**
   * Response status : File not found = 0x6A82
   */
  short SW_FILE_NOT_FOUND     = 0x6A82;

  /**
   * Response status : Record not found = 0x6A83
   */
  short SW_RECORD_NOT_FOUND   = 0x6A83;

  /**
   * Response status : Incorrect parameters (P1,P2) = 0x6A86
   */
  short SW_INCORRECT_P1P2 	  = 0x6A86;

  /**
   * Response status : Incorrect parameters (P1,P2) = 0x6B00
   */
  short SW_WRONG_P1P2 	      = 0x6B00;

  /**
   * Response status : Correct Expected Length (Le) = 0x6C00
   */
  short SW_CORRECT_LENGTH_00  = 0x6C00;

  /**
   * Response status : INS value not supported = 0x6D00
   */
  short SW_INS_NOT_SUPPORTED  = 0x6D00;

  /**
   * Response status : CLA value not supported = 0x6E00
   */
  short SW_CLA_NOT_SUPPORTED  = 0x6E00;

  /**
   * Response status : No precise diagnosis = 0x6F00
   */
  short SW_UNKNOWN            = 0x6F00;

  /**
   * Response status : Not enough memory space in the file  = 0x6A84
   */
  short SW_FILE_FULL = 0x6A84;

  // Offsets into APDU header information
  
  /**
   * APDU header offset : CLA = 0
   */
  byte OFFSET_CLA  = 0;

  /**
   * APDU header offset : INS = 1
   */
  byte OFFSET_INS  = 1;

  /**
   * APDU header offset : P1 = 2
   */
  byte OFFSET_P1   = 2;

 /**
   * APDU header offset : P2 = 3
   */
  byte OFFSET_P2   = 3;

 /**
   * APDU header offset : LC = 4
   */
  byte OFFSET_LC   = 4;

 /**
   * APDU command data offset : CDATA = 5
   */
  byte OFFSET_CDATA= 5;
  
  
  //commands
  
  /**
   * APDU command CLA : ISO 7816 = 0x00
   */
  byte CLA_ISO7816 = (byte) 0x00;
  
  /** 
   * APDU command INS : SELECT = 0xA4
   */
  byte INS_SELECT = (byte) 0xA4;
  
  /** 
   * APDU command INS : EXTERNAL AUTHENTICATE = 0x82
   */
  byte INS_EXTERNAL_AUTHENTICATE = (byte) 0x82;

}
