/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id: SCID_nonnull.java,v 1.8 2008/11/05 14:03:13 filliatr Exp $ */

//@+ CheckArithOverflow = no
//@+ InvariantPolicy = Arguments
//@+ NonNullByDefault = alllocal
// @+ AnnotationPolicy = Invariants
// @+ AbstractDomain = Box
// @+ AbstractDomain = Oct
// @+ AbstractDomain = Pol

// Smart Card ID - Example for a Smart Card based Identification and Authentication Application
// File: SCID.java
//
// This applet realizes a identification and authentication application which is typically used
// in online environment. The advantage of this solution is that nearly no adinistrative overhead
// on the smart card side is neccessary because the card is only used for identification of the
// user by PIN verification and authentication of the smart card by a challenge response algorithm.
//
// Smart Card ID is a multi purpose applet. Nearly all applications can be realised with it 
// (e.g. physical access for room, logical access computers or information, ....). 
// The application functionality is in the background system. Offline Terminals can use precomputed 
// triplets for secure authentication of the smart card. For a no security authentication only the 
// smart card ID can be used.
//
// In the administrative phase it is possible to write after a successful verification of the
// administrative PIN all data objects into the memory.
// In the operative phase it is possible to read the unique smart card ID, to authenticate the
// smart card with a callenge response algorithm based on DES and to identifiay the user with 
// his user PIN. A successful PIN verification can be checked in a secured way.
//
// Memory consumption: EEPROM: 1344 byte, RAM 44 byte
// 
// Package AID: 'D2 76 00 00 60 50 05'
// Applet AID:  'D2 76 00 00 60 41 05'
//
// Source code based on java card specification version 2.1.
// Compiled and tested with JDK 1.3.1 Java Card Application Studio (JCAST) V 2.3,
// JLoad 2.0, Java Card Class File Converter 2.1.2, IFDSIM 4.0 and UniverSIM Java Card 64 kB from G&D.
//
//---------------------------------------------------------------------------------------
// Specification of ISO/IEC 7816-4 case 3 command SELECT FILE
//   command APDU               CLA = '00' || INS = 'A4' ||
//                              P1 (= '04' = select by DF Name) || P2 (='00') ||
//                              Lc (= length of DF Name) || DATA (= DF Name)
//   response APDU (all case)   SW1 || SW2
//
// Specification of ISO/IEC 7816-4 case 3 command VERIFY
//   command APDU               CLA = '00' || INS = '20' || P1 (= '00') ||
//                              P2 ('01' = user PIN, '02' = admin PIN)
//                              Lc (= length of PIN) || DATA (= PIN)
//   response APDU (all case)   SW1 || SW2
//
// Specification of ISO/IEC 7816-4 case 3 command PUT DATA
//   command APDU               CLA = '00' || INS = 'DA' ||
//                              P1 (= '00') || P2 (= TAG ['40' ... 'FE']) ||
//                              Lc (= length of DATA) ||
//                              DATA (value field of the DO, with length Lc)
//   response APDU (all case)   SW1 || SW2
//
// Specification of ISO/IEC 7816-4 case 2 command GET DATA
//   command APDU               CLA = '00' || INS = 'CA' ||
//                              P1 (= '00') || P2 (= TAG ['40' ... 'FE'])
//                              Le (= length of expected data)
//   response APDU (good case)  DATA (value field of the DO, with length Le) ||
//                              SW1 || SW2
//   response APDU (bad case)   SW1 || SW2
//
// Specification of the proprietary case 4 command AUTH SC (authenticate smart card)
//   command APDU               CLA = '80' || INS = '08' || P1 (= '00') || P2 (= '00')
//                              Lc (= 8 byte) || C-DATA (with length Lc) || Le (= 21)
//   response APDU (good case)  R-DATA (with length Le) || SW1 || SW2
//   response APDU (bad case)   SW1 || SW2
//
//                C-DATA: RND [8 byte]
//                R-DATA: authentication data || MAC
//                authentication data = RND [8 byte] || smart card ID [4 byte] ||
//                                      authentication counter [2 byte] ||
//                                      admin PIN error counter [1 byte] ||
//                                      user PIN error counter [1 byte] || PIN status [1 byte] || 
//                                      padding (with '00', length must be a multiple of 8)
//                PIN status = Bit 8 (RFU) || ... Bit 3 (RFU) ||
//                             Bit 2 (1 = admin PIN succesful verified) ||
//                             Bit 1 (1 = user PIN succesful verified)
//                MAC = last 4 byte of enc (AuthKey) (authentication data)
//                authentication counter: no of further possible authentications 
//                                        '7FFF' = 32767 max. value, 
//                                        '0000' = no further authentication possible
//                padding according to ISO/IEC 9797 Method 1 (= 00 00 ...)
//
//---------------------------------------------------------------------------------------
// Implementation Notes
//    * no support of different DFs
//    * no secure messaging support (= class is alway '00')
//    * the authentication counter is used to limit the number of possible attacks to 
//      the used cryptoalgorithm, it can be set to any value with the PUT DATA command
//    * instead of the encryption of the whole response data in the AUTH SC command only 
//      a 4 byte MAC is used for authentication, because this gives more transperency about
//      the exchanged data (data protection laws)
//    * the adminstration phase (e.g. the personalisation of the smart card) is done with the
//      PUT DATA command. All external seeable data elements of the smart card can be written
//      with this ISO/IEC 7816-4 compatible command.
//    * The access condition for the administration phase is a VERIFY command with the 
//      administration PIN.
//    * the authentication key could be smart card individual or common for all smart cards
//    * if neccessary it is possible to use smart card individual authentication keys. This 
//      keys should be written into the smart card during the personalisation. As a reference 
//      to smart card individual keys the smart card ID can be used.
//    * abbreviations:  RND   - random number
//                      RFU   - reserved for future use (content of such a data element is not defined!)
//                      admin - administration
//
//---------------------------------------------------------------------------------------
// Use Cases
//   note:   all shown use cases are good cases
//   format: --command to smart card-->
//           <--response from smart card--
//
// -----Administrative Phase
// Personalisation
//   --VERIFY (with default Admin PIN)-->
//   <--ok--
//   --PUT DATA (Smart Card ID, admin PIN, user PIN, authentication key, authentication counter)-->
//   <--ok--
//  the following reset is important because of the reset of the validation status of the admin PIN
//   --Reset-->            
//   <--ATR--
//
// change user PIN (e.g. new user PIN, forgotten user PIN, error counter of the user PIN reached maximum value)
//   the following command is only neccessary if a card individual admin PIN is used
//   --GET DATA (smart card ID)-->
//   <--Smart Card ID--
//   --VERIFY (with admin PIN)-->
//   <--ok--
//   --PUT DATA (new user PIN)-->
//   <--ok--
//  the following reset is important because of the reset of the validation status of the admin PIN
//   --Reset-->            
//   <--ATR--
//
// forgotten admin PIN, error counter of the admin PIN reached maximum value
//   new smart card neccessary
//
// -----Operative Phase
// get smart card ID
//   --GET DATA (smart card ID)-->
//   <--smart card ID--
//
// authenticate smart card without security (advantage: no key is neccessary)
//   --GET DATA (Smart Card ID)-->
//   <--smart card ID--
//
// authenticate smart card with security
//   the following command is only neccessary if a card individual authentication key is used
//   --GET DATA (smart card ID)-->
//   <--smart card ID--
//   --AUTH SC (RND)-->
//   <--auth data || MAC--
//
// check user identity
//   the following command is only neccessary if a card individual authentication key is used
//   --GET DATA (smart card ID)-->
//   <--smart card ID--
//   --VERIFY (with user PIN)-->
//   <--ok--
//   --AUTH SC (RND)-->
//   <--auth data || MAC--
//
//---------------------------------------------------------------------------------------
// Test Strategy
//          good case tests:
//               1.1 test all use cases in good case
//               2.1 write all data objects and verify if they are stored correct
//               3.1 Test all marked "Test!" testpoints
//
//          bad case tests:
//              1.1 test correct and wrong admin PIN incl. error counter behaviour
//              1.2 is PUT DATA only possible after successful admin PIN verification?
//              1.3 does AUTH SC give the correct admin PIN validation state back?
//              1.4 does AUTH SC give the correct admin PIN error counter back?
//              2.1 test case 1.1 with user PIN
//              2.2 test case 1.3 with user PIN
//              2.2 test case 1.4 with user PIN
//              4.1 correct authetication behaviour if error counter = 0?
//
//---------------------------------------------------------------------------------------
// This source code is under GNU general public license (see www.opensource.org for details).
// Please send corrections and ideas for extensions to Wolfgang Rankl (www.wrankl.de)
// Copyright 2004 by Wolfgang Rankl, Munich
//---------------------------------------------------------------------------------------
// 29. April 2004 - V 1: initial runnable version
// 10. May   2004 - V 2: improved documentation, 1st published version
//---------------------------------------------------------------------------------------

import  javacard.framework.*;    // import all neccessary packages for the java card framework
import  javacard.security.*;     // import all neccessary packages for the java card security

// Bug with lemma ? : gwhy blocked ?
// @ lemma a1: \forall int b; 0 <= b <= 127 ==> (b & 0x00FF) == b;
// @ axiom a1: \forall int b; 0 <= b <= 127 ==> (b & 0x00FF) == b;

public class SCID extends Applet {
    // definitions for the classes and commands
    final static byte    PROP_CLASS  = (byte) 0x80;     // Class of the proprietary APDU commands
    final static byte    INS_SELECT  = (byte) 0xA4;     // instruction for the ISO/IEC 7816-4 SELECT FILE command
    final static byte    INS_VERIFY  = (byte) 0x20;     // instruction for the ISO/IEC 7816-4 VERIFY command
    final static byte    INS_PUTDATA = (byte) 0xDA;     // instruction for the ISO/IEC 7816-4 PUT DATA command
    final static byte    INS_GETDATA = (byte) 0xCA;     // instruction for the ISO/IEC 7816-4 GET DATA command
    final static byte    INS_AUTHSC  = (byte) 0x08;     // instruction for the proprietary AUTH SC command
    
    // definitions for specific returncodes
    final static short   SW_PIN_FAILED =     (short) 0x63C0;  // returncode for PIN verification failed
    // the last nibble shows the number of remaining tries
    final static short   SW_DATA_NOT_FOUND = (short) 0x6A88;  // referenced data not found
    
    // definitions for the AUTH SC command
    final static byte    LEN_CAPDU_AUTHSC = (byte)   8;   // length of the data in the command APDU of the AUTH SC command
    final static byte    LEN_RAPDU_AUTHSC = (byte)  21;   // length of the data in the response APDU of the AUTH SC command
    final static short   INDEX_RND        = (short)  0;   // start position of the RND in the AUTH SC response
    final static short   INDEX_SCID       = (short)  8;   // start position of the authentication counter in the AUTH SC response
    final static short   INDEX_AuthCntr   = (short) 12;   // start position of the authentication counter in the AUTH SC response
    final static short   INDEX_APINTries  = (short) 14;   // position of the admin PIN remaining tries in the AUTH SC response
    final static short   INDEX_UPINTries  = (short) 15;   // position of the user PIN remaining tries in the AUTH SC response
    final static short   INDEX_APINValid  = (short) 16;   // position of the admin PIN validation status in the AUTH SC response
    final static short   INDEX_UPINValid  = (short) 16;   // position of the user PIN validation status in the AUTH SC response
    final static short   INDEX_MAC        = (short) 17;   // start position of the MAC in the AUTH SC response
    
    // definitions for some data elements
    static short         authcntr;                        // authentication counter, it is a 2 byte signed short value
    final static short   SIZE_AUTHCNTR  = (short)  2;     // size of the authentication counter in byte
    static byte[]        scid;                            // the smart card ID
    final static short   SIZE_SCID      = (short)  4;     // size of the unique smart card identifier in byte
    //@ static invariant scid_inv: scid.length == SIZE_SCID;

    static byte[]        workarray;                       // workarray
    final static short   SIZE_WORKARRAY = (short) 30;     // size of the workarray 
    //@ static invariant workarray_inv: workarray.length == SIZE_WORKARRAY;
    
    static DESKey        authkey;                         // authentication key
    final static short   SIZE_AUTHKEY   = (short)  8;     // authentication key is a 8 byte long DES key
    static Signature     mac;                             // MAC (message authentication code)
    
    // definitions for the storage of the data elements
    final static short TAG_USERPIN  = (short) 0x51;   // tag for the user PIN
    final static short TAG_ADMINPIN = (short) 0x52;   // tag for the administrative PIN
    final static short TAG_AUTHKEY  = (short) 0x53;   // tag for the authentication key
    final static short TAG_AUTHCNTR = (short) 0x54;   // tag for the athentication counter
    final static short TAG_SCID     = (short) 0x55;   // tag for the smart card ID
    
    // constants and variables for the admin PIN management
    final static byte[]  DEFAULT_ADMINPIN                     // default admin PIN value
	= {(byte) 0x22, (byte) 0x22, (byte) 0x22};
    final static byte    ADMINPIN_SIZE          = (byte) 3;   // size of the PIN in byte
    final static byte    DEFAULT_ADMINPIN_MAXEC = (byte) 2;   // default value of the PIN error counter
    final static byte    ADMINPIN_ID            = (byte) 2;   // PIN identifier, PIN 2 = admin PIN
    static OwnerPIN      adminpin;                            // the PIN object
    
    // constants and variables for the user PIN management
    final static byte[]  DEFAULT_USERPIN                      // default user PIN value
	= {(byte) 0x00, (byte) 0x00};
    final static byte    USERPIN_SIZE          = (byte) 2;    // size of the PIN in byte
    final static byte    DEFAULT_USERPIN_MAXEC = (byte) 3;    // default value of the PIN error counter
    final static byte    USERPIN_ID            = (byte) 1;    // PIN identifier, PIN 1 = user PIN
    static OwnerPIN      userpin;                             // the PIN object
    
    // constants and variables for the key management
    final static byte[]  DEFAULT_AUTHKEY            // default authentication key
	= {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
	   (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
    

    //---------------------------------------------------------------------------------------
    //----- installation and registration of the applet
    public static void install(byte[] buffer, short offset, byte length) {
	// create the object for the smart card ID
	scid = new byte[SIZE_SCID];
	
	// create a multiple useable object in RAM with an array of bytes
	workarray = JCSystem.makeTransientByteArray(SIZE_WORKARRAY, JCSystem.CLEAR_ON_DESELECT);  // Test!
	
	// create the object for admin PIN and set it to the default value
	adminpin = new OwnerPIN(DEFAULT_ADMINPIN_MAXEC, ADMINPIN_SIZE);
	adminpin.update(DEFAULT_ADMINPIN, (short)(0), ADMINPIN_SIZE);
	
	// create the object for user PIN and set it to the default value
	userpin = new OwnerPIN(DEFAULT_USERPIN_MAXEC, USERPIN_SIZE);
	userpin.update(DEFAULT_USERPIN, (short)(0), USERPIN_SIZE);
	
	// create the key object for authentication and set the key to the default value
	authkey = (DESKey)KeyBuilder.buildKey(KeyBuilder.TYPE_DES, KeyBuilder.LENGTH_DES, false);
	authkey.setKey(DEFAULT_AUTHKEY, (short)0);
	
	// create a signature object for MAC calculating
	// padding in line with ISO/IEC 9797 Method 1 (= 000 ...)
	mac = Signature.getInstance(Signature.ALG_DES_MAC4_ISO9797_M1, false);
	
	// register the application within the JCRE
	new SCID().register();
    }  // install
    
    //---------------------------------------------------------------------------------------
    //----- this is the command dispatcher
    public void process(APDU apdu) {
      byte[] cmd_apdu = apdu.getBuffer();
      
    // delete global used variables, for better robustness
    Util.arrayFillNonAtomic(workarray, (short) 0, SIZE_WORKARRAY, (byte) 0);

    if (cmd_apdu[ISO7816.OFFSET_CLA] == ISO7816.CLA_ISO7816) {
      //----- it is the ISO/IEC 7816 class
      switch(cmd_apdu[ISO7816.OFFSET_INS]) {
        case INS_SELECT:
          // it is a SELECT FILE instruction
          cmdSELECT(apdu);
          break;
        case INS_VERIFY:
          // it is a VERIFY instruction
          cmdVERIFY(apdu);
          break;
        case INS_PUTDATA:
          // it is a PUT DATA instruction
          cmdPUTDATA(apdu);
          break;
        case INS_GETDATA:
          // it is a GET DATA instruction
          cmdGETDATA(apdu);
          break;
        default :
          // the instruction in the command apdu is not supported
          ISOException.throwIt(ISO7816.SW_INS_NOT_SUPPORTED);
      }  // switch
    }  // if
    else if (cmd_apdu[ISO7816.OFFSET_CLA] == PROP_CLASS) {
      //----- it is the proprietary class
      switch(cmd_apdu[ISO7816.OFFSET_INS]) {
        case INS_AUTHSC:
          // it is a AUTH SC instruction
          cmdAUTHSC(apdu);
          break;
        default :
          // the instruction in the command apdu is not supported
          ISOException.throwIt(ISO7816.SW_INS_NOT_SUPPORTED);
      }  // switch
    }  // else if
    else {
      // the class in the command apdu is not supported
      ISOException.throwIt(ISO7816.SW_CLA_NOT_SUPPORTED);
    }  // else
  }  // process

    //---------------------------------------------------------------------------------------
    //----- program code for the APDU command SELECT FILE
    private void cmdSELECT(APDU apdu) {
	byte[] cmd_apdu = apdu.getBuffer();
	
	//----- check preconditions in the APDU header
	// check if P1='04'
	if (cmd_apdu[ISO7816.OFFSET_P1] != 0x04) {
	    ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
	} // if
	// check if P2='00'
	if (cmd_apdu[ISO7816.OFFSET_P2] != 0x00) {
	    ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
	} // if
	short lc = (short)(cmd_apdu[ISO7816.OFFSET_LC] & 0x00FF);  // get Lc (command length)
	receiveAPDUBody(apdu);                                     // get the command body
	
	//----- command functionality
	if (JCSystem.getAID().equals(cmd_apdu, ISO7816.OFFSET_CDATA, (byte) lc) == false) {
	    ISOException.throwIt(ISO7816.SW_APPLET_SELECT_FAILED);
	} // if
	
	//----- prepare response APDU
	ISOException.throwIt(ISO7816.SW_NO_ERROR);   // command proper executed
    }  // cmdSELECT
    
  //---------------------------------------------------------------------------------------
  //----- program code for the APDU command VERIFY
  private void cmdVERIFY(APDU apdu) {
    byte[] cmd_apdu = apdu.getBuffer();

    //----- check preconditions in the APDU header
    // check if P1='00'
    if (cmd_apdu[ISO7816.OFFSET_P1] != 0) {
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // if
    short lc = (short)(cmd_apdu[ISO7816.OFFSET_LC] & 0x00FF);  // get Lc (command length)
    receiveAPDUBody(apdu);                                     // get the command body

    if (cmd_apdu[ISO7816.OFFSET_P2] == USERPIN_ID) {  // it is the user PIN
      if (lc != USERPIN_SIZE) {                       // Lc = length of PIN ?
        ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
      } //if
      if (userpin.check(cmd_apdu, ISO7816.OFFSET_CDATA, USERPIN_SIZE) == false) {
        // PIN verification not successful
        short tries = userpin.getTriesRemaining();
        ISOException.throwIt( (short) (SW_PIN_FAILED + tries));  // send error counter in APDU back
      } // if
    } // if
    else if (cmd_apdu[ISO7816.OFFSET_P2] == ADMINPIN_ID) {    // it is the administrative PIN
      if (lc != ADMINPIN_SIZE) {                              // Lc = length of PIN ?
        ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
      } //if
      if (adminpin.check(cmd_apdu, ISO7816.OFFSET_CDATA, ADMINPIN_SIZE) == false) {
        // PIN verification not successful
        short tries = adminpin.getTriesRemaining();
        ISOException.throwIt( (short) (SW_PIN_FAILED + tries));  // send error counter in APDU back
      } // if
    } // if
    else {          // it is not a valid PIN identifier
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // else
    // PIN verification successful
    ISOException.throwIt(ISO7816.SW_NO_ERROR);               // command proper executed
  }  // cmdVERIFY

  //---------------------------------------------------------------------------------------
  //----- program code for the APDU command PUT DATA
  private void cmdPUTDATA(APDU apdu) {
    byte[] cmd_apdu = apdu.getBuffer();

    //----- check preconditions in the APDU header
    // check if P1=0
    if (cmd_apdu[ISO7816.OFFSET_P1] != 0) {
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // if
    // check if P2 contents an allowed tag value
    short tag = (short) (cmd_apdu[ISO7816.OFFSET_P2] & (short) 0x00FF);
    if ((tag < (short) 0x0040) || (tag > (short) 0x00FE)) {
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // if
    short lc = (short)(cmd_apdu[ISO7816.OFFSET_LC] & (short) 0x00FF);  // calculate Lc (command length)
    receiveAPDUBody(apdu);     // get the command body

    //----- check precoditions of security status
    if (adminpin.isValidated() == false) {
      ISOException.throwIt(ISO7816.SW_SECURITY_STATUS_NOT_SATISFIED);
    } // if

    //----- command functionality
    switch(tag) {
      case TAG_USERPIN:
        // the user PIN must be changed
        if (lc != USERPIN_SIZE) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(cmd_apdu, (short)((ISO7816.OFFSET_CDATA) & 0x00FF), workarray, (short) 0, lc);
        userpin.update(workarray, (short) 0, USERPIN_SIZE);
        break;
      case TAG_ADMINPIN:
        // the admin PIN must be changed
        if (lc != ADMINPIN_SIZE) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(cmd_apdu, (short)((ISO7816.OFFSET_CDATA) & 0x00FF), workarray, (short) 0, lc);
        adminpin.update(workarray, (short) 0, ADMINPIN_SIZE);
        break;
      case TAG_AUTHKEY:
        // the authentication key must be changed
        if (lc != SIZE_AUTHKEY) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(cmd_apdu, (short)((ISO7816.OFFSET_CDATA) & 0x00FF), workarray, (short) 0, lc);
        authkey.setKey(workarray, (short)0);
        break;
      case TAG_AUTHCNTR:
        // the authentication counter must be changed
        if (lc != SIZE_AUTHCNTR) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(cmd_apdu, (short)((ISO7816.OFFSET_CDATA) & 0x00FF), workarray, (short) 0, lc);
        if (Util.getShort(workarray, (short)0) > 0x7FFF) ISOException.throwIt(ISO7816.SW_WRONG_DATA);
        authcntr = Util.getShort(workarray, (short)0);
        break;
      case TAG_SCID:
        // the smart card ID must be changed
        if (lc != SIZE_SCID) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(cmd_apdu, (short)((ISO7816.OFFSET_CDATA) & 0x00FF), scid, (short) 0, lc);
        break;
    default :
      // the tag in the command apdu is not supported
      ISOException.throwIt(SW_DATA_NOT_FOUND);
    }  // switch

    //----- prepare response APDU
    ISOException.throwIt(ISO7816.SW_NO_ERROR);   // command proper executed
  }  // cmdPUTDATA

  //---------------------------------------------------------------------------------------
  //----- program code for the APDU command GET DATA
  private void cmdGETDATA(APDU apdu) {
    byte[] cmd_apdu = apdu.getBuffer();

    //----- check preconditions in the APDU header
    // check if P1=0
    if (cmd_apdu[ISO7816.OFFSET_P1] != 0) {
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // if
    // check if P2 contents an allowed tag value
    short tag = (short) (cmd_apdu[ISO7816.OFFSET_P2] & (short) 0x00FF);
    if ((tag < 0x40) || (tag > 0xFE)) {
      ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    } // if
    short le = (short)(cmd_apdu[ISO7816.OFFSET_LC] & 0x00FF);  // calculate Le (expected length)

    //----- command functionality
    switch(tag) {       // a switch construct is used because of the easy extensibility for additional tags
      case TAG_SCID:
        // get the smart card ID
        if (le != SIZE_SCID) ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
        Util.arrayCopy(scid, (short) (0), workarray, (short) 0, le);
        break;
    default :
      // the tag in the command apdu could not found
      ISOException.throwIt(SW_DATA_NOT_FOUND);
    }  // switch

    //----- now all preparations are done, the data object can be send to the IFD -----
    apdu.setOutgoing();                           // set transmission to outgoing data
    apdu.setOutgoingLength((short)le);            // set the number of bytes to send to the IFD
    apdu.sendBytesLong(workarray, (short) 0, (short)le); // send the requested the number of bytes to the IFD, data field of the DO
  }  // cmdGETDATA

  //---------------------------------------------------------------------------------------
  //----- program code for the APDU command AUTH SC
  private void cmdAUTHSC(APDU apdu) {
    byte[] cmd_apdu = apdu.getBuffer();
    //----- check preconditions in the APDU header
    // check if P1=0
    if (cmd_apdu[ISO7816.OFFSET_P1] != 0) 
	ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    // check if P2=0
    if (cmd_apdu[ISO7816.OFFSET_P2] != 0) 
	ISOException.throwIt(ISO7816.SW_WRONG_P1P2);
    short lc = (short)(cmd_apdu[ISO7816.OFFSET_LC] & 0x00FF); // get Lc (command length)
    if (lc != LEN_CAPDU_AUTHSC) 
	ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
    receiveAPDUBody(apdu); // get the command body
    //----- check security preconditions
    if (authcntr < 0) 
	ISOException.throwIt(SW_PIN_FAILED);  // Test!
    //----- collect all data for the signature
    Util.arrayCopy(cmd_apdu, (short) ISO7816.OFFSET_CDATA, workarray, INDEX_RND, lc); // RND from IFD
    Util.arrayCopy(scid, (short) (0), workarray, INDEX_SCID, SIZE_SCID);     // smart card ID
    Util.setShort(workarray, INDEX_AuthCntr, authcntr);                      // authentication counter
    workarray[INDEX_APINTries] = adminpin.getTriesRemaining();
    workarray[INDEX_UPINTries] = userpin.getTriesRemaining();
    if (adminpin.isValidated() == true) {
      workarray[INDEX_APINValid] = (byte) (workarray[INDEX_APINValid] | (byte) 0x01);  // Test!
    } // if
    if (userpin.isValidated() == true) {
      workarray[INDEX_UPINValid] = (byte) (workarray[INDEX_UPINValid] | (byte) 0x02);  // Test!
    } // if
    mac.init(authkey, Signature.MODE_SIGN);
    short len = mac.sign(workarray, (short) 0, LEN_RAPDU_AUTHSC, workarray, INDEX_MAC);  // calculate the MAC
    authcntr = (short) (authcntr - (short) 1);                    // update number of allowed authentication

    //----- now all calculations are done, the data can be send to the IFD
    apdu.setOutgoing();                           // set transmission to outgoing data
    apdu.setOutgoingLength(LEN_RAPDU_AUTHSC);            // set the number of bytes to send to the IFD
    apdu.sendBytesLong(workarray, (short) 0, LEN_RAPDU_AUTHSC);  // send the requested the number of bytes to the IFD, data field of the DO
  }  // cmdAUTHSC

    //---------------------------------------------------------------------------------------
    //----- receive the body of the command APDU
    public void receiveAPDUBody(APDU apdu) {
	byte[] buffer = apdu.getBuffer();
	short lc = (short)(buffer[ISO7816.OFFSET_LC] & 0x00FF);  // calculate Lc (command length)
	// check if Lc != number of received bytes of the command APDU body
	if (lc != apdu.setIncomingAndReceive()) {
	    ISOException.throwIt(ISO7816.SW_WRONG_LENGTH);
	} // if
    }  // receiveAPDUBody
    
}  // class

