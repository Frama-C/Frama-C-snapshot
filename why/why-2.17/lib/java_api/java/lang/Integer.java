/*
 * @(#)Integer.java	1.76 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.lang;

/**
 * The <code>Integer</code> class wraps a value of the primitive type
 * <code>int</code> in an object. An object of type
 * <code>Integer</code> contains a single field whose type is
 * <code>int</code>.
 *
 *  <p>
 * 
 * In addition, this class provides several methods for converting an
 * <code>int</code> to a <code>String</code> and a <code>String</code>
 * to an <code>int</code>, as well as other constants and methods
 * useful when dealing with an <code>int</code>.
 *
 * @author  Lee Boynton
 * @author  Arthur van Hoff
 * @version 1.76, 01/23/03
 * @since   JDK1.0
 */
public final class Integer extends Number implements Comparable {
    /**
     * A constant holding the minimum value an <code>int</code> can
     * have, -2<sup>31</sup>.
     */
    public static final int   MIN_VALUE = 0x80000000;

    /**
     * A constant holding the maximum value an <code>int</code> can
     * have, 2<sup>31</sup>-1.
     */
    public static final int   MAX_VALUE = 0x7fffffff;

    /**
     * The <code>Class</code> instance representing the primitive type
     * <code>int</code>.
     *
     * @since   JDK1.1
     */
    //KML public static final Class	TYPE = Class.getPrimitiveClass("int");

    /**
     * All possible chars for representing a number as a String
     */
    /* KML
    final static char[] digits = {
	'0' , '1' , '2' , '3' , '4' , '5' ,
	'6' , '7' , '8' , '9' , 'a' , 'b' ,
	'c' , 'd' , 'e' , 'f' , 'g' , 'h' ,
	'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
	'o' , 'p' , 'q' , 'r' , 's' , 't' ,
	'u' , 'v' , 'w' , 'x' , 'y' , 'z'
    };
    */

    /**
     * Returns a string representation of the first argument in the
     * radix specified by the second argument.
     * <p>
     * If the radix is smaller than <code>Character.MIN_RADIX</code>
     * or larger than <code>Character.MAX_RADIX</code>, then the radix
     * <code>10</code> is used instead.
     * <p>
     * If the first argument is negative, the first element of the
     * result is the ASCII minus character <code>'-'</code>
     * (<code>'&#92;u002D'</code>). If the first argument is not
     * negative, no sign character appears in the result.
     * <p>
     * The remaining characters of the result represent the magnitude
     * of the first argument. If the magnitude is zero, it is
     * represented by a single zero character <code>'0'</code>
     * (<code>'&#92;u0030'</code>); otherwise, the first character of
     * the representation of the magnitude will not be the zero
     * character.  The following ASCII characters are used as digits: 
     * <blockquote><pre>
     *   0123456789abcdefghijklmnopqrstuvwxyz
     * </pre></blockquote>
     * These are <code>'&#92;u0030'</code> through
     * <code>'&#92;u0039'</code> and <code>'&#92;u0061'</code> through
     * <code>'&#92;u007A'</code>. If <code>radix</code> is
     * <var>N</var>, then the first <var>N</var> of these characters
     * are used as radix-<var>N</var> digits in the order shown. Thus,
     * the digits for hexadecimal (radix 16) are
     * <code>0123456789abcdef</code>. If uppercase letters are
     * desired, the {@link java.lang.String#toUpperCase()} method may
     * be called on the result:
     * <blockquote><pre>
     * Integer.toString(n, 16).toUpperCase()
     * </pre></blockquote>
     *
     * @param   i       an integer to be converted to a string.
     * @param   radix   the radix to use in the string representation.
     * @return  a string representation of the argument in the specified radix.
     * @see     java.lang.Character#MAX_RADIX
     * @see     java.lang.Character#MIN_RADIX
     */
    public static String toString(int i, int radix) {

        if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX)
	    radix = 10;

	/* Use the faster version */
	if (radix == 10) {
	    return toString(i);
	}

	char buf[] = new char[33];
	boolean negative = (i < 0);
	int charPos = 32;

	if (!negative) {
	    i = -i;
	}

	while (i <= -radix) {
	    buf[charPos--] = digits[-(i % radix)];
	    i = i / radix;
	}
	buf[charPos] = digits[-i];

	if (negative) {
	    buf[--charPos] = '-';
	}

	return new String(buf, charPos, (33 - charPos));
    }

    /**
     * Returns a string representation of the integer argument as an
     * unsigned integer in base&nbsp;16.
     * <p>
     * The unsigned integer value is the argument plus 2<sup>32</sup>
     * if the argument is negative; otherwise, it is equal to the
     * argument.  This value is converted to a string of ASCII digits
     * in hexadecimal (base&nbsp;16) with no extra leading
     * <code>0</code>s. If the unsigned magnitude is zero, it is
     * represented by a single zero character <code>'0'</code>
     * (<code>'&#92;u0030'</code>); otherwise, the first character of
     * the representation of the unsigned magnitude will not be the
     * zero character. The following characters are used as
     * hexadecimal digits:
     * <blockquote><pre>
     * 0123456789abcdef
     * </pre></blockquote>
     * These are the characters <code>'&#92;u0030'</code> through
     * <code>'&#92;u0039'</code> and <code>'&#92;u0061'</code> through
     * <code>'&#92;u0066'</code>. If uppercase letters are
     * desired, the {@link java.lang.String#toUpperCase()} method may
     * be called on the result:
     * <blockquote><pre>
     * Integer.toHexString(n).toUpperCase()
     * </pre></blockquote>
     *
     * @param   i   an integer to be converted to a string.
     * @return  the string representation of the unsigned integer value
     *          represented by the argument in hexadecimal (base&nbsp;16).
     * @since   JDK1.0.2
     */
    public static String toHexString(int i) {
	return toUnsignedString(i, 4);
    }

    /**
     * Returns a string representation of the integer argument as an
     * unsigned integer in base&nbsp;8.
     * <p>
     * The unsigned integer value is the argument plus 2<sup>32</sup>
     * if the argument is negative; otherwise, it is equal to the
     * argument.  This value is converted to a string of ASCII digits
     * in octal (base&nbsp;8) with no extra leading <code>0</code>s.
     * <p>
     * If the unsigned magnitude is zero, it is represented by a
     * single zero character <code>'0'</code>
     * (<code>'&#92;u0030'</code>); otherwise, the first character of
     * the representation of the unsigned magnitude will not be the
     * zero character. The following characters are used as octal
     * digits:
     * <blockquote><pre>
     * 01234567
     * </pre></blockquote>
     * These are the characters <code>'&#92;u0030'</code> through
     * <code>'&#92;u0037'</code>.
     *
     * @param   i   an integer to be converted to a string.
     * @return  the string representation of the unsigned integer value
     *          represented by the argument in octal (base&nbsp;8).
     * @since   JDK1.0.2
     */
    public static String toOctalString(int i) {
	return toUnsignedString(i, 3);
    }

    /**
     * Returns a string representation of the integer argument as an
     * unsigned integer in base&nbsp;2.
     * <p>
     * The unsigned integer value is the argument plus 2<sup>32</sup>
     * if the argument is negative; otherwise it is equal to the
     * argument.  This value is converted to a string of ASCII digits
     * in binary (base&nbsp;2) with no extra leading <code>0</code>s.
     * If the unsigned magnitude is zero, it is represented by a
     * single zero character <code>'0'</code>
     * (<code>'&#92;u0030'</code>); otherwise, the first character of
     * the representation of the unsigned magnitude will not be the
     * zero character. The characters <code>'0'</code>
     * (<code>'&#92;u0030'</code>) and <code>'1'</code>
     * (<code>'&#92;u0031'</code>) are used as binary digits.
     *
     * @param   i   an integer to be converted to a string.
     * @return  the string representation of the unsigned integer value
     *          represented by the argument in binary (base&nbsp;2).
     * @since   JDK1.0.2
     */
    public static String toBinaryString(int i) {
	return toUnsignedString(i, 1);
    }

    /**
     * Convert the integer to an unsigned number.
     */
    private static String toUnsignedString(int i, int shift) {
	char[] buf = new char[32];
	int charPos = 32;
	int radix = 1 << shift;
	int mask = radix - 1;
	do {
	    buf[--charPos] = digits[i & mask];
	    i >>>= shift;
	} while (i != 0);

	return new String(buf, charPos, (32 - charPos));
    }

    /* KML
    final static char [] DigitTens = {
	'0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
	'1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
	'2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
	'3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
	'4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
	'5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
	'6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
	'7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
	'8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
	'9', '9', '9', '9', '9', '9', '9', '9', '9', '9',
	} ; 

    final static char [] DigitOnes = { 
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	} ;
    */
	// I use the "invariant division by multiplication" trick to
	// accelerate Integer.toString.  In particular we want to
	// avoid division by 10.
	//
	// The "trick" has roughly the same performance characterists
	// as the "classic" Integer.toString code on a non-JIT VM.
	// The trick avoids .rem and .div calls but has a longer code
	// path and is thus dominated by dispatch overhead.  In the
	// JIT case the dispatch overhead doesn't exist and the
	// "trick" is considerably faster than the classic code.
	//
	// TODO-FIXME: convert (x * 52429) into the equiv shift-add
	// sequence.
	//
	// RE:  Division by Invariant Integers using Multiplication
	//      T Gralund, P Montgomery
	//      ACM PLDI 1994
	//

    /**
     * Returns a <code>String</code> object representing the
     * specified integer. The argument is converted to signed decimal
     * representation and returned as a string, exactly as if the
     * argument and radix 10 were given as arguments to the {@link
     * #toString(int, int)} method.
     *
     * @param   i   an integer to be converted.
     * @return  a string representation of the argument in base&nbsp;10.
     */
    public static String toString(int i) {
        switch(i) {
            case Integer.MIN_VALUE: return "-2147483648";
            case -3: return "-3";
            case -2: return "-2";
            case -1: return "-1";
            case 0: return "0";
            case 1: return "1";
            case 2: return "2";
            case 3: return "3";
            case 4: return "4";
            case 5: return "5";
            case 6: return "6";
            case 7: return "7";
            case 8: return "8";
            case 9: return "9";
            case 10: return "10";
        }
        char[] buf = (char[])(perThreadBuffer.get());
        int charPos = getChars(i, buf);
        return new String(buf, charPos, 12 - charPos);
    }

    // Per-thread buffer for string/stringbuffer conversion
    // private static ThreadLocal perThreadBuffer = new ThreadLocal() {
    //    protected synchronized Object initialValue() {
    //        return new char[12];
    //    }
    //};

    private static int getChars(int i, char[] buf) {
        int q, r;
        int charPos = 12;
        char sign = 0;

        if (i < 0) { 
            sign = '-';
            i = -i;
        }

        // Generate two digits per iteration
        while (i >= 65536) {
            q = i / 100;
        // really: r = i - (q * 100);
            r = i - ((q << 6) + (q << 5) + (q << 2));
            i = q;
            buf [--charPos] = DigitOnes[r];
            buf [--charPos] = DigitTens[r];
        }

        // Fall thru to fast mode for smaller numbers
        // assert(i <= 65536, i);
        for (;;) { 
            q = (i * 52429) >>> (16+3);
            r = i - ((q << 3) + (q << 1));  // r = i-(q*10) ...
            buf [--charPos] = digits [r];
            i = q;
            if (i == 0) break;
        }
        if (sign != 0) {
            buf [--charPos] = sign;
        }
        return charPos;
    }
 
    static void appendTo(int i, StringBuffer sb) {
        switch(i) {
            case Integer.MIN_VALUE:
                sb.append("-2147483648");
                return;
            case -3: sb.append("-3"); return;
            case -2: sb.append("-2"); return;
            case -1: sb.append("-1"); return;
            case 0: sb.append("0"); return;
            case 1: sb.append("1"); return;
            case 2: sb.append("2"); return;
            case 3: sb.append("3"); return;
            case 4: sb.append("4"); return;
            case 5: sb.append("5"); return;
            case 6: sb.append("6"); return;
            case 7: sb.append("7"); return;
            case 8: sb.append("8"); return;
            case 9: sb.append("9"); return;
            case 10: sb.append("10"); return;
        }
        char[] buf = (char[])(perThreadBuffer.get());
        int charPos = getChars(i, buf);
        sb.append(buf, charPos, 12 - charPos);
    }
    
    /**
     * Parses the string argument as a signed integer in the radix 
     * specified by the second argument. The characters in the string 
     * must all be digits of the specified radix (as determined by 
     * whether {@link java.lang.Character#digit(char, int)} returns a 
     * nonnegative value), except that the first character may be an 
     * ASCII minus sign <code>'-'</code> (<code>'&#92;u002D'</code>) to 
     * indicate a negative value. The resulting integer value is returned. 
     * <p>
     * An exception of type <code>NumberFormatException</code> is
     * thrown if any of the following situations occurs:
     * <ul>
     * <li>The first argument is <code>null</code> or is a string of
     * length zero.
     * <li>The radix is either smaller than 
     * {@link java.lang.Character#MIN_RADIX} or
     * larger than {@link java.lang.Character#MAX_RADIX}. 
     * <li>Any character of the string is not a digit of the specified
     * radix, except that the first character may be a minus sign
     * <code>'-'</code> (<code>'&#92;u002D'</code>) provided that the
     * string is longer than length 1.
     * <li>The value represented by the string is not a value of type
     * <code>int</code>. 
     * </ul><p>
     * Examples:
     * <blockquote><pre>
     * parseInt("0", 10) returns 0
     * parseInt("473", 10) returns 473
     * parseInt("-0", 10) returns 0
     * parseInt("-FF", 16) returns -255
     * parseInt("1100110", 2) returns 102
     * parseInt("2147483647", 10) returns 2147483647
     * parseInt("-2147483648", 10) returns -2147483648
     * parseInt("2147483648", 10) throws a NumberFormatException
     * parseInt("99", 8) throws a NumberFormatException
     * parseInt("Kona", 10) throws a NumberFormatException
     * parseInt("Kona", 27) returns 411787
     * </pre></blockquote>
     *
     * @param      s   the <code>String</code> containing the integer 
     * 			representation to be parsed
     * @param      radix   the radix to be used while parsing <code>s</code>.
     * @return     the integer represented by the string argument in the
     *             specified radix.
     * @exception  NumberFormatException if the <code>String</code>
     * 		   does not contain a parsable <code>int</code>.
     */
    public static int parseInt(String s, int radix)
		throws NumberFormatException
    {
        if (s == null) {
            throw new NumberFormatException("null");
        }

	/*KML
	if (radix < Character.MIN_RADIX) {
	    throw new NumberFormatException("radix " + radix +
					    " less than Character.MIN_RADIX");
	}

	if (radix > Character.MAX_RADIX) {
	    throw new NumberFormatException("radix " + radix +
					    " greater than Character.MAX_RADIX");
	}
	*/

	int result = 0;
	boolean negative = false;
	int i = 0, max = s.length();
	int limit;
	int multmin;
	int digit;

	if (max > 0) {
	    if (s.charAt(0) == '-') {
		negative = true;
		limit = Integer.MIN_VALUE;
		i++;
	    } else {
		limit = -Integer.MAX_VALUE;
	    }
	    multmin = limit / radix;
	    if (i < max) {
		digit = Character.digit(s.charAt(i++),radix);
		if (digit < 0) {
		    throw NumberFormatException.forInputString(s);
		} else {
		    result = -digit;
		}
	    }
	    while (i < max) {
		// Accumulating negatively avoids surprises near MAX_VALUE
		digit = Character.digit(s.charAt(i++),radix);
		if (digit < 0) {
		    throw NumberFormatException.forInputString(s);
		}
		if (result < multmin) {
		    throw NumberFormatException.forInputString(s);
		}
		result *= radix;
		if (result < limit + digit) {
		    throw NumberFormatException.forInputString(s);
		}
		result -= digit;
	    }
	} else {
	    throw NumberFormatException.forInputString(s);
	}
	if (negative) {
	    if (i > 1) {
		return result;
	    } else {	/* Only got "-" */
		throw NumberFormatException.forInputString(s);
	    }
	} else {
	    return -result;
	}
    }

    /**
     * Parses the string argument as a signed decimal integer. The 
     * characters in the string must all be decimal digits, except that 
     * the first character may be an ASCII minus sign <code>'-'</code> 
     * (<code>'&#92;u002D'</code>) to indicate a negative value. The resulting 
     * integer value is returned, exactly as if the argument and the radix 
     * 10 were given as arguments to the 
     * {@link #parseInt(java.lang.String, int)} method.
     *
     * @param s	   a <code>String</code> containing the <code>int</code>
     *             representation to be parsed
     * @return     the integer value represented by the argument in decimal.
     * @exception  NumberFormatException  if the string does not contain a
     *               parsable integer.
     */
    public static int parseInt(String s) throws NumberFormatException {
	return parseInt(s,10);
    }

    /**
     * Returns an <code>Integer</code> object holding the value
     * extracted from the specified <code>String</code> when parsed
     * with the radix given by the second argument. The first argument
     * is interpreted as representing a signed integer in the radix
     * specified by the second argument, exactly as if the arguments
     * were given to the {@link #parseInt(java.lang.String, int)}
     * method. The result is an <code>Integer</code> object that
     * represents the integer value specified by the string.
     * <p>
     * In other words, this method returns an <code>Integer</code>
     * object equal to the value of:
     *
     * <blockquote><code>
     * new Integer(Integer.parseInt(s, radix))  
     * </code></blockquote>
     *
     * @param      s   the string to be parsed.
     * @param      radix the radix to be used in interpreting <code>s</code>
     * @return     an <code>Integer</code> object holding the value
     *             represented by the string argument in the specified
     *             radix.
     * @exception NumberFormatException if the <code>String</code>
     * 		  does not contain a parsable <code>int</code>.
     */
    public static Integer valueOf(String s, int radix) throws NumberFormatException {
	return new Integer(parseInt(s,radix));
    }

    /**
     * Returns an <code>Integer</code> object holding the
     * value of the specified <code>String</code>. The argument is
     * interpreted as representing a signed decimal integer, exactly
     * as if the argument were given to the {@link
     * #parseInt(java.lang.String)} method. The result is an
     * <code>Integer</code> object that represents the integer value
     * specified by the string.
     * <p>
     * In other words, this method returns an <code>Integer</code>
     * object equal to the value of:
     *
     * <blockquote><code>
     * new Integer(Integer.parseInt(s))
     * </code></blockquote>
     *
     * @param      s   the string to be parsed.
     * @return     an <code>Integer</code> object holding the value
     *             represented by the string argument.
     * @exception  NumberFormatException  if the string cannot be parsed 
     *             as an integer.
     */
    public static Integer valueOf(String s) throws NumberFormatException
    {
	return new Integer(parseInt(s, 10));
    }

    /**
     * The value of the <code>Integer</code>.
     *
     * @serial
     */
    private int value;

    /**
     * Constructs a newly allocated <code>Integer</code> object that
     * represents the specified <code>int</code> value.
     *
     * @param   value   the value to be represented by the 
     *			<code>Integer</code> object.
     */
    /*@ ensures this.value == value;
      @*/
    public Integer(int value) {
	this.value = value;
    }

    /**
     * Constructs a newly allocated <code>Integer</code> object that
     * represents the <code>int</code> value indicated by the
     * <code>String</code> parameter. The string is converted to an
     * <code>int</code> value in exactly the manner used by the
     * <code>parseInt</code> method for radix 10.
     *
     * @param      s   the <code>String</code> to be converted to an
     *                 <code>Integer</code>.
     * @exception  NumberFormatException  if the <code>String</code> does not
     *               contain a parsable integer.
     * @see        java.lang.Integer#parseInt(java.lang.String, int)
     */
    public Integer(String s) throws NumberFormatException {
	this.value = parseInt(s, 10);
    }

    /**
     * Returns the value of this <code>Integer</code> as a
     * <code>byte</code>.
     */
    public byte byteValue() {
	return (byte)value;
    }

    /**
     * Returns the value of this <code>Integer</code> as a
     * <code>short</code>.
     */
    public short shortValue() {
	return (short)value;
    }

    /**
     * Returns the value of this <code>Integer</code> as an
     * <code>int</code>.
     */
    /*@ ensures \result == this.value;
      @*/
    public int intValue() {
	return value;
    }

    /**
     * Returns the value of this <code>Integer</code> as a
     * <code>long</code>.
     */
    public long longValue() {
	return (long)value;
    }

    /**
     * Returns the value of this <code>Integer</code> as a
     * <code>float</code>.
     */
    public float floatValue() {
	return (float)value;
    }

    /**
     * Returns the value of this <code>Integer</code> as a
     * <code>double</code>.
     */
    public double doubleValue() {
	return (double)value;
    }

    /**
     * Returns a <code>String</code> object representing this
     * <code>Integer</code>'s value. The value is converted to signed
     * decimal representation and returned as a string, exactly as if
     * the integer value were given as an argument to the {@link
     * java.lang.Integer#toString(int)} method.
     *
     * @return  a string representation of the value of this object in
     *          base&nbsp;10.
     */
    public String toString() {
	return String.valueOf(value);
    }

    /**
     * Returns a hash code for this <code>Integer</code>.
     *
     * @return  a hash code value for this object, equal to the 
     *          primitive <code>int</code> value represented by this 
     *          <code>Integer</code> object. 
     */
    public int hashCode() {
	return value;
    }

    /**
     * Compares this object to the specified object.  The result is
     * <code>true</code> if and only if the argument is not
     * <code>null</code> and is an <code>Integer</code> object that
     * contains the same <code>int</code> value as this object.
     *
     * @param   obj   the object to compare with.
     * @return  <code>true</code> if the objects are the same;
     *          <code>false</code> otherwise.
     */
    public boolean equals(Object obj) {
	if (obj instanceof Integer) {
	    return value == ((Integer)obj).intValue();
	}
	return false;
    }

    /**
     * Determines the integer value of the system property with the
     * specified name.
     * <p>
     * The first argument is treated as the name of a system property. 
     * System properties are accessible through the 
     * {@link java.lang.System#getProperty(java.lang.String)} method. The 
     * string value of this property is then interpreted as an integer 
     * value and an <code>Integer</code> object representing this value is 
     * returned. Details of possible numeric formats can be found with 
     * the definition of <code>getProperty</code>. 
     * <p>
     * If there is no property with the specified name, if the specified name
     * is empty or <code>null</code>, or if the property does not have 
     * the correct numeric format, then <code>null</code> is returned.
     * <p>
     * In other words, this method returns an <code>Integer</code>
     * object equal to the value of:
     *
     * <blockquote><code>
     * getInteger(nm, null)
     * </code></blockquote>
     *
     * @param   nm   property name.
     * @return  the <code>Integer</code> value of the property.
     * @see     java.lang.System#getProperty(java.lang.String)
     * @see     java.lang.System#getProperty(java.lang.String, java.lang.String)
     */
    public static Integer getInteger(String nm) {
	return getInteger(nm, null);
    }

    /**
     * Determines the integer value of the system property with the
     * specified name.
     * <p>
     * The first argument is treated as the name of a system property.
     * System properties are accessible through the {@link
     * java.lang.System#getProperty(java.lang.String)} method. The 
     * string value of this property is then interpreted as an integer 
     * value and an <code>Integer</code> object representing this value is 
     * returned. Details of possible numeric formats can be found with 
     * the definition of <code>getProperty</code>. 
     * <p>
     * The second argument is the default value. An <code>Integer</code> object
     * that represents the value of the second argument is returned if there
     * is no property of the specified name, if the property does not have
     * the correct numeric format, or if the specified name is empty or
     *  <code>null</code>.
     * <p>
     * In other words, this method returns an <code>Integer</code> object 
     * equal to the value of:
     * <blockquote><code>
     * getInteger(nm, new Integer(val))
     * </code></blockquote>
     * but in practice it may be implemented in a manner such as: 
     * <blockquote><pre>
     * Integer result = getInteger(nm, null);
     * return (result == null) ? new Integer(val) : result;
     * </pre></blockquote>
     * to avoid the unnecessary allocation of an <code>Integer</code> 
     * object when the default value is not needed. 
     *
     * @param   nm   property name.
     * @param   val   default value.
     * @return  the <code>Integer</code> value of the property.
     * @see     java.lang.System#getProperty(java.lang.String)
     * @see     java.lang.System#getProperty(java.lang.String, java.lang.String)
     */
    public static Integer getInteger(String nm, int val) {
        Integer result = getInteger(nm, null);
        return (result == null) ? new Integer(val) : result;
    }

    /**
     * Returns the integer value of the system property with the
     * specified name.  The first argument is treated as the name of a
     * system property.  System properties are accessible through the
     * {@link java.lang.System#getProperty(java.lang.String)} method.
     * The string value of this property is then interpreted as an
     * integer value, as per the <code>Integer.decode</code> method,
     * and an <code>Integer</code> object representing this value is
     * returned.
     * <p>
     * <ul><li>If the property value begins with the two ASCII characters 
     *         <code>0x</code> or the ASCII character <code>#</code>, not 
     *      followed by a minus sign, then the rest of it is parsed as a 
     *      hexadecimal integer exactly as by the method 
     *      {@link #valueOf(java.lang.String, int)} with radix 16. 
     * <li>If the property value begins with the ASCII character 
     *     <code>0</code> followed by another character, it is parsed as an 
     *     octal integer exactly as by the method 
     *     {@link #valueOf(java.lang.String, int)} with radix 8. 
     * <li>Otherwise, the property value is parsed as a decimal integer 
     * exactly as by the method {@link #valueOf(java.lang.String, int)} 
     * with radix 10. 
     * </ul><p>
     * The second argument is the default value. The default value is
     * returned if there is no property of the specified name, if the
     * property does not have the correct numeric format, or if the
     * specified name is empty or <code>null</code>.
     *
     * @param   nm   property name.
     * @param   val   default value.
     * @return  the <code>Integer</code> value of the property.
     * @see     java.lang.System#getProperty(java.lang.String)
     * @see java.lang.System#getProperty(java.lang.String, java.lang.String)
     * @see java.lang.Integer#decode
     */
    public static Integer getInteger(String nm, Integer val) {
	/*KML
	  String v = null;
        try {
            v = System.getProperty(nm);
        } catch (IllegalArgumentException e) {
        } catch (NullPointerException e) {
        }
	if (v != null) {
	    try {
		return Integer.decode(v);
	    } catch (NumberFormatException e) {
	    }
	}
	*/
	return val;
    }

    /**
     * Decodes a <code>String</code> into an <code>Integer</code>.
     * Accepts decimal, hexadecimal, and octal numbers numbers given
     * by the following grammar:
     *
     * <blockquote>
     * <dl>
     * <dt><i>DecodableString:</i>
     * <dd><i>Sign<sub>opt</sub> DecimalNumeral</i>
     * <dd><i>Sign<sub>opt</sub></i> <code>0x</code> <i>HexDigits</i>
     * <dd><i>Sign<sub>opt</sub></i> <code>0X</code> <i>HexDigits</i>
     * <dd><i>Sign<sub>opt</sub></i> <code>#</code> <i>HexDigits</i>
     * <dd><i>Sign<sub>opt</sub></i> <code>0</code> <i>OctalDigits</i>
     * <p>
     * <dt><i>Sign:</i>
     * <dd><code>-</code>
     * </dl>
     * </blockquote>
     *
     * <i>DecimalNumeral</i>, <i>HexDigits</i>, and <i>OctalDigits</i>
     * are defined in <a href="http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#48282">&sect;3.10.1</a> 
     * of the <a href="http://java.sun.com/docs/books/jls/html/">Java 
     * Language Specification</a>.
     * <p>
     * The sequence of characters following an (optional) negative
     * sign and/or radix specifier (&quot;<code>0x</code>&quot;,
     * &quot;<code>0X</code>&quot;, &quot;<code>#</code>&quot;, or
     * leading zero) is parsed as by the <code>Integer.parseInt</code>
     * method with the indicated radix (10, 16, or 8).  This sequence
     * of characters must represent a positive value or a {@link
     * NumberFormatException} will be thrown.  The result is negated
     * if first character of the specified <code>String</code> is the
     * minus sign.  No whitespace characters are permitted in the
     * <code>String</code>.
     *
     * @param     nm the <code>String</code> to decode.
     * @return    a <code>Integer</code> object holding the <code>int</code>
     *		   value represented by <code>nm</code>
     * @exception NumberFormatException  if the <code>String</code> does not
     *            contain a parsable integer.
     * @see java.lang.Integer#parseInt(java.lang.String, int)
     * @since 1.2
     */
    public static Integer decode(String nm) throws NumberFormatException {
        int radix = 10;
        int index = 0;
        boolean negative = false;
        Integer result;

        // Handle minus sign, if present
        if (nm.startsWith("-")) {
            negative = true;
            index++;
        }

        // Handle radix specifier, if present
	if (nm.startsWith("0x", index) || nm.startsWith("0X", index)) {
	    index += 2;
            radix = 16;
	}
	else if (nm.startsWith("#", index)) {
	    index ++;
            radix = 16;
	}
	else if (nm.startsWith("0", index) && nm.length() > 1 + index) {
	    index ++;
            radix = 8;
	}

        if (nm.startsWith("-", index))
            throw new NumberFormatException("Negative sign in wrong position");

        try {
            result = Integer.valueOf(nm.substring(index), radix);
            result = negative ? new Integer(-result.intValue()) : result;
        } catch (NumberFormatException e) {
            // If number is Integer.MIN_VALUE, we'll end up here. The next line
            // handles this case, and causes any genuine format error to be
            // rethrown.
            String constant = negative ? new String("-" + nm.substring(index))
                                       : nm.substring(index);
            result = Integer.valueOf(constant, radix);
        }
        return result;
    }

    /**
     * Compares two <code>Integer</code> objects numerically.
     *
     * @param   anotherInteger   the <code>Integer</code> to be compared.
     * @return	the value <code>0</code> if this <code>Integer</code> is
     * 		equal to the argument <code>Integer</code>; a value less than
     * 		<code>0</code> if this <code>Integer</code> is numerically less
     * 		than the argument <code>Integer</code>; and a value greater 
     * 		than <code>0</code> if this <code>Integer</code> is numerically
     * 		 greater than the argument <code>Integer</code> (signed
     * 		 comparison).
     * @since   1.2
     */
    public int compareTo(Integer anotherInteger) {
	int thisVal = this.value;
	int anotherVal = anotherInteger.value;
	return (thisVal<anotherVal ? -1 : (thisVal==anotherVal ? 0 : 1));
    }

    /**
     * Compares this <code>Integer</code> object to another object.
     * If the object is an <code>Integer</code>, this function behaves
     * like <code>compareTo(Integer)</code>.  Otherwise, it throws a
     * <code>ClassCastException</code> (as <code>Integer</code>
     * objects are only comparable to other <code>Integer</code>
     * objects).
     *
     * @param   o the <code>Object</code> to be compared.
     * @return  the value <code>0</code> if the argument is a 
     *		<code>Integer</code> numerically equal to this 
     *		<code>Integer</code>; a value less than <code>0</code> 
     *		if the argument is a <code>Integer</code> numerically 
     *		greater than this <code>Integer</code>; and a value 
     *		greater than <code>0</code> if the argument is a 
     *		<code>Integer</code> numerically less than this 
     *		<code>Integer</code>.
     * @exception <code>ClassCastException</code> if the argument is not an
     *		  <code>Integer</code>.
     * @see     java.lang.Comparable
     * @since   1.2
     */
    public int compareTo(Object o) {
	return compareTo((Integer)o);
    }

    /** use serialVersionUID from JDK 1.0.2 for interoperability */
    private static final long serialVersionUID = 1360826667806852920L;
}
