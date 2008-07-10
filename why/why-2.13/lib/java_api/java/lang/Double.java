/*
 * @(#)Double.java	1.82 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.lang;

/**
 * The <code>Double</code> class wraps a value of the primitive type
 * <code>double</code> in an object. An object of type
 * <code>Double</code> contains a single field whose type is
 * <code>double</code>.
 * <p>
 * In addition, this class provides several methods for converting a
 * <code>double</code> to a <code>String</code> and a
 * <code>String</code> to a <code>double</code>, as well as other
 * constants and methods useful when dealing with a
 * <code>double</code>.
 *
 * @author  Lee Boynton
 * @author  Arthur van Hoff
 * @version 1.82, 01/23/03
 * @since JDK1.0
 */
public final class Double extends Number implements Comparable {
    /**
     * A constant holding the positive infinity of type
     * <code>double</code>. It is equal to the value returned by
     * <code>Double.longBitsToDouble(0x7ff0000000000000L)</code>.
     */
    public static final double POSITIVE_INFINITY = 1.0 / 0.0;

    /**
     * A constant holding the negative infinity of type
     * <code>double</code>. It is equal to the value returned by
     * <code>Double.longBitsToDouble(0xfff0000000000000L)</code>.
     */
    public static final double NEGATIVE_INFINITY = -1.0 / 0.0;

    /** 
     * A constant holding a Not-a-Number (NaN) value of type
     * <code>double</code>. It is equivalent to the value returned by
     * <code>Double.longBitsToDouble(0x7ff8000000000000L)</code>.
     */
    public static final double NaN = 0.0d / 0.0;

    /**
     * A constant holding the largest positive finite value of type
     * <code>double</code>, (2-2<sup>-52</sup>)&middot;2<sup>1023</sup>.
     * It is equal to the value returned by:
     * <code>Double.longBitsToDouble(0x7fefffffffffffffL)</code>.
     */
    public static final double MAX_VALUE = 1.7976931348623157e+308;

    /**
     * A constant holding the smallest positive nonzero value of type
     * <code>double</code>, 2<sup>-1074</sup>. It is equal to the
     * value returned by <code>Double.longBitsToDouble(0x1L)</code>.
     */
    public static final double MIN_VALUE = 4.9e-324;

    /**
     * The <code>Class</code> instance representing the primitive type
     * <code>double</code>.
     *
     * @since JDK1.1 
     */
    public static final Class	TYPE = Class.getPrimitiveClass("double");

    /**
     * Returns a string representation of the <code>double</code> 
     * argument. All characters mentioned below are ASCII characters.
     * <ul>
     * <li>If the argument is NaN, the result is the string
     *     &quot;<code>NaN</code>&quot;.
     * <li>Otherwise, the result is a string that represents the sign and 
     * magnitude (absolute value) of the argument. If the sign is negative, 
     * the first character of the result is '<code>-</code>' 
     * (<code>'&#92;u002D'</code>); if the sign is positive, no sign character 
     * appears in the result. As for the magnitude <i>m</i>:
     * <ul>
     * <li>If <i>m</i> is infinity, it is represented by the characters 
     * <code>"Infinity"</code>; thus, positive infinity produces the result 
     * <code>"Infinity"</code> and negative infinity produces the result 
     * <code>"-Infinity"</code>.
     *
     * <li>If <i>m</i> is zero, it is represented by the characters 
     * <code>"0.0"</code>; thus, negative zero produces the result 
     * <code>"-0.0"</code> and positive zero produces the result 
     * <code>"0.0"</code>. 
     *
     * <li>If <i>m</i> is greater than or equal to 10<sup>-3</sup> but less 
     * than 10<sup>7</sup>, then it is represented as the integer part of 
     * <i>m</i>, in decimal form with no leading zeroes, followed by 
     * '<code>.</code>' (<code>'&#92;u002E'</code>), followed by one or 
     * more decimal digits representing the fractional part of <i>m</i>. 
     *
     * <li>If <i>m</i> is less than 10<sup>-3</sup> or greater than or
     * equal to 10<sup>7</sup>, then it is represented in so-called
     * "computerized scientific notation." Let <i>n</i> be the unique
     * integer such that 10<sup><i>n</i></sup> &lt;= <i>m</i> &lt;
     * 10<sup><i>n</i>+1</sup>; then let <i>a</i> be the
     * mathematically exact quotient of <i>m</i> and
     * 10<sup><i>n</i></sup> so that 1 &lt;= <i>a</i> &lt; 10. The
     * magnitude is then represented as the integer part of <i>a</i>,
     * as a single decimal digit, followed by '<code>.</code>'
     * (<code>'&#92;u002E'</code>), followed by decimal digits
     * representing the fractional part of <i>a</i>, followed by the
     * letter '<code>E</code>' (<code>'&#92;u0045'</code>), followed
     * by a representation of <i>n</i> as a decimal integer, as
     * produced by the method {@link Integer#toString(int)}.
     * </ul>
     * </ul>
     * How many digits must be printed for the fractional part of 
     * <i>m</i> or <i>a</i>? There must be at least one digit to represent 
     * the fractional part, and beyond that as many, but only as many, more 
     * digits as are needed to uniquely distinguish the argument value from
     * adjacent values of type <code>double</code>. That is, suppose that 
     * <i>x</i> is the exact mathematical value represented by the decimal 
     * representation produced by this method for a finite nonzero argument 
     * <i>d</i>. Then <i>d</i> must be the <code>double</code> value nearest 
     * to <i>x</i>; or if two <code>double</code> values are equally close 
     * to <i>x</i>, then <i>d</i> must be one of them and the least
     * significant bit of the significand of <i>d</i> must be <code>0</code>.
     * <p>
     * To create localized string representations of a floating-point
     * value, use subclasses of {@link java.text.NumberFormat}.
     *
     * @param   d   the <code>double</code> to be converted.
     * @return a string representation of the argument.
     */
    public static String toString(double d) {
	return new FloatingDecimal(d).toJavaFormatString();
    }

    /**
     * Returns a <code>Double</code> object holding the
     * <code>double</code> value represented by the argument string
     * <code>s</code>.
     * <p>
     * If <code>s</code> is <code>null</code>, then a 
     * <code>NullPointerException</code> is thrown.
     * <p>
     * Leading and trailing whitespace characters in <code>s</code>
     * are ignored. The rest of <code>s</code> should constitute a
     * <i>FloatValue</i> as described by the lexical rule:
     * <blockquote><i>
     * <dl>
     * <dt>FloatValue:
     * <dd><i>Sign<sub>opt</sub></i> <code>NaN</code>
     * <dd><i>Sign<sub>opt</sub></i> <code>Infinity</code>
     * <dd>Sign<sub>opt</sub> FloatingPointLiteral
     * </dl>
     * </i></blockquote>
     * where <i>Sign</i> and <i>FloatingPointLiteral</i> are as
     * defined in 
     * <a href="http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798">&sect;3.10.2</a>
     * of the <a href="http://java.sun.com/docs/books/jls/html/">Java 
     * Language Specification</a>. If <code>s</code> does not have the 
     * form of a <i>FloatValue</i>, then a <code>NumberFormatException</code>
     * is thrown. Otherwise, <code>s</code> is regarded as
     * representing an exact decimal value in the usual "computerized
     * scientific notation"; this exact decimal value is then
     * conceptually converted to an "infinitely precise" binary value
     * that is then rounded to type <code>double</code> by the usual
     * round-to-nearest rule of IEEE 754 floating-point arithmetic,
     * which includes preserving the sign of a zero value. Finally, a
     * <code>Double</code> object representing this
     * <code>double</code> value is returned.
     * <p>
     * To interpret localized string representations of a
     * floating-point value, use subclasses of {@link
     * java.text.NumberFormat}.
     *
     * <p>Note that trailing format specifiers, specifiers that
     * determine the type of a floating-point literal
     * (<code>1.0f</code> is a <code>float</code> value;
     * <code>1.0d</code> is a <code>double</code> value), do
     * <em>not</em> influence the results of this method.  In other
     * words, the numerical value of the input string is converted
     * directly to the target floating-point type.  The two-step
     * sequence of conversions, string to <code>float</code> followed
     * by <code>float</code> to <code>double</code>, is <em>not</em>
     * equivalent to converting a string directly to
     * <code>double</code>. For example, the <code>float</code>
     * literal <code>0.1f</code> is equal to the <code>double</code>
     * value <code>0.10000000149011612</code>; the <code>float</code>
     * literal <code>0.1f</code> represents a different numerical
     * value than the <code>double</code> literal
     * <code>0.1</code>. (The numerical value 0.1 cannot be exactly
     * represented in a binary floating-point number.)
     *
     * @param      s   the string to be parsed.
     * @return     a <code>Double</code> object holding the value
     *             represented by the <code>String</code> argument.
     * @exception  NumberFormatException  if the string does not contain a
     *               parsable number.
     */
    public static Double valueOf(String s) throws NumberFormatException {
	return new Double(FloatingDecimal.readJavaFormatString(s).doubleValue());
    }

    /**
     * Returns a new <code>double</code> initialized to the value
     * represented by the specified <code>String</code>, as performed
     * by the <code>valueOf</code> method of class
     * <code>Double</code>.
     *
     * @param      s   the string to be parsed.
     * @return the <code>double</code> value represented by the string
     *         argument.
     * @exception NumberFormatException if the string does not contain
     *            a parsable <code>double</code>.
     * @see        java.lang.Double#valueOf(String)
     * @since 1.2
     */
    public static double parseDouble(String s) throws NumberFormatException {
	return FloatingDecimal.readJavaFormatString(s).doubleValue();
    }

    /**
     * Returns <code>true</code> if the specified number is a
     * Not-a-Number (NaN) value, <code>false</code> otherwise.
     *
     * @param   v   the value to be tested.
     * @return  <code>true</code> if the value of the argument is NaN;
     *          <code>false</code> otherwise.
     */
    static public boolean isNaN(double v) {
	return (v != v);
    }

    /**
     * Returns <code>true</code> if the specified number is infinitely
     * large in magnitude, <code>false</code> otherwise.
     *
     * @param   v   the value to be tested.
     * @return  <code>true</code> if the value of the argument is positive
     *          infinity or negative infinity; <code>false</code> otherwise.
     */
    static public boolean isInfinite(double v) {
	return (v == POSITIVE_INFINITY) || (v == NEGATIVE_INFINITY);
    }

    /**
     * The value of the Double.
     *
     * @serial
     */
    private double value;

    /**
     * Constructs a newly allocated <code>Double</code> object that
     * represents the primitive <code>double</code> argument.
     *
     * @param   value   the value to be represented by the <code>Double</code>.
     */
    public Double(double value) {
	this.value = value;
    }

    /**
     * Constructs a newly allocated <code>Double</code> object that
     * represents the floating-point value of type <code>double</code>
     * represented by the string. The string is converted to a
     * <code>double</code> value as if by the <code>valueOf</code> method.
     *
     * @param      s   a string to be converted to a <code>Double</code>.
     * @exception  NumberFormatException  if the string does not contain a
     *               parsable number.
     * @see        java.lang.Double#valueOf(java.lang.String)
     */
    public Double(String s) throws NumberFormatException {
	// REMIND: this is inefficient
	this(valueOf(s).doubleValue());
    }

    /**
     * Returns <code>true</code> if this <code>Double</code> value is
     * a Not-a-Number (NaN), <code>false</code> otherwise.
     *
     * @return  <code>true</code> if the value represented by this object is
     *          NaN; <code>false</code> otherwise.
     */
    public boolean isNaN() {
	return isNaN(value);
    }

    /**
     * Returns <code>true</code> if this <code>Double</code> value is
     * infinitely large in magnitude, <code>false</code> otherwise.
     *
     * @return  <code>true</code> if the value represented by this object is
     *          positive infinity or negative infinity;
     *          <code>false</code> otherwise.
     */
    public boolean isInfinite() {
	return isInfinite(value);
    }

    /**
     * Returns a string representation of this <code>Double</code> object.
     * The primitive <code>double</code> value represented by this
     * object is converted to a string exactly as if by the method
     * <code>toString</code> of one argument.
     *
     * @return  a <code>String</code> representation of this object.
     * @see java.lang.Double#toString(double)
     */
    public String toString() {
	return String.valueOf(value);
    }

    /**
     * Returns the value of this <code>Double</code> as a <code>byte</code> (by
     * casting to a <code>byte</code>).
     *
     * @return  the <code>double</code> value represented by this object
     *          converted to type <code>byte</code>
     * @since JDK1.1 
     */
    public byte byteValue() {
	return (byte)value;
    }

    /**
     * Returns the value of this <code>Double</code> as a
     * <code>short</code> (by casting to a <code>short</code>).
     *
     * @return  the <code>double</code> value represented by this object
     *          converted to type <code>short</code>
     * @since JDK1.1 
     */
    public short shortValue() {
	return (short)value;
    }

    /**
     * Returns the value of this <code>Double</code> as an
     * <code>int</code> (by casting to type <code>int</code>).
     *
     * @return  the <code>double</code> value represented by this object
     *          converted to type <code>int</code>
     */
    public int intValue() {
	return (int)value;
    }

    /**
     * Returns the value of this <code>Double</code> as a
     * <code>long</code> (by casting to type <code>long</code>).
     *
     * @return  the <code>double</code> value represented by this object
     *          converted to type <code>long</code>
     */
    public long longValue() {
	return (long)value;
    }

    /**
     * Returns the <code>float</code> value of this
     * <code>Double</code> object.
     *
     * @return  the <code>double</code> value represented by this object
     *          converted to type <code>float</code>
     * @since JDK1.0 
     */
    public float floatValue() {
	return (float)value;
    }

    /**
     * Returns the <code>double</code> value of this
     * <code>Double</code> object.
     *
     * @return the <code>double</code> value represented by this object
     */
    public double doubleValue() {
	return (double)value;
    }

    /**
     * Returns a hash code for this <code>Double</code> object. The
     * result is the exclusive OR of the two halves of the
     * <code>long</code> integer bit representation, exactly as
     * produced by the method {@link #doubleToLongBits(double)}, of
     * the primitive <code>double</code> value represented by this
     * <code>Double</code> object. That is, the hash code is the value
     * of the expression:
     * <blockquote><pre>
     * (int)(v^(v&gt;&gt;&gt;32))
     * </pre></blockquote>
     * where <code>v</code> is defined by: 
     * <blockquote><pre>
     * long v = Double.doubleToLongBits(this.doubleValue());
     * </pre></blockquote>
     *
     * @return  a <code>hash code</code> value for this object.
     */
    public int hashCode() {
	long bits = doubleToLongBits(value);
	return (int)(bits ^ (bits >>> 32));
    }

    /**
     * Compares this object against the specified object.  The result
     * is <code>true</code> if and only if the argument is not
     * <code>null</code> and is a <code>Double</code> object that
     * represents a <code>double</code> that has the same value as the
     * <code>double</code> represented by this object. For this
     * purpose, two <code>double</code> values are considered to be
     * the same if and only if the method {@link
     * #doubleToLongBits(double)} returns the identical
     * <code>long</code> value when applied to each.
     * <p>
     * Note that in most cases, for two instances of class
     * <code>Double</code>, <code>d1</code> and <code>d2</code>, the
     * value of <code>d1.equals(d2)</code> is <code>true</code> if and
     * only if
     * <blockquote><pre>
     *   d1.doubleValue()&nbsp;== d2.doubleValue()
     * </pre></blockquote>
     * <p>
     * also has the value <code>true</code>. However, there are two
     * exceptions:
     * <ul>
     * <li>If <code>d1</code> and <code>d2</code> both represent
     *     <code>Double.NaN</code>, then the <code>equals</code> method
     *     returns <code>true</code>, even though
     *     <code>Double.NaN==Double.NaN</code> has the value
     *     <code>false</code>.
     * <li>If <code>d1</code> represents <code>+0.0</code> while
     *     <code>d2</code> represents <code>-0.0</code>, or vice versa,
     *     the <code>equal</code> test has the value <code>false</code>,
     *     even though <code>+0.0==-0.0</code> has the value <code>true</code>.
     * </ul>
     * This definition allows hash tables to operate properly.
     * @param   obj   the object to compare with.
     * @return  <code>true</code> if the objects are the same;
     *          <code>false</code> otherwise.
     * @see java.lang.Double#doubleToLongBits(double)
     */
    public boolean equals(Object obj) {
	return (obj instanceof Double)
	       && (doubleToLongBits(((Double)obj).value) ==
		      doubleToLongBits(value));
    }

    /**
     * Returns a representation of the specified floating-point value
     * according to the IEEE 754 floating-point "double
     * format" bit layout.
     * <p>
     * Bit 63 (the bit that is selected by the mask 
     * <code>0x8000000000000000L</code>) represents the sign of the 
     * floating-point number. Bits 
     * 62-52 (the bits that are selected by the mask 
     * <code>0x7ff0000000000000L</code>) represent the exponent. Bits 51-0 
     * (the bits that are selected by the mask 
     * <code>0x000fffffffffffffL</code>) represent the significand 
     * (sometimes called the mantissa) of the floating-point number. 
     * <p>
     * If the argument is positive infinity, the result is
     * <code>0x7ff0000000000000L</code>.
     * <p>
     * If the argument is negative infinity, the result is
     * <code>0xfff0000000000000L</code>.
     * <p>
     * If the argument is NaN, the result is 
     * <code>0x7ff8000000000000L</code>. 
     * <p>
     * In all cases, the result is a <code>long</code> integer that, when 
     * given to the {@link #longBitsToDouble(long)} method, will produce a 
     * floating-point value the same as the argument to 
     * <code>doubleToLongBits</code> (except all NaN values are
     * collapsed to a single &quot;canonical&quot; NaN value).
     *
     * @param   value   a <code>double</code> precision floating-point number.
     * @return the bits that represent the floating-point number.  
     */
    public static native long doubleToLongBits(double value);

    /**
     * Returns a representation of the specified floating-point value
     * according to the IEEE 754 floating-point "double
     * format" bit layout, preserving Not-a-Number (NaN) values.
     * <p>
     * Bit 63 (the bit that is selected by the mask 
     * <code>0x8000000000000000L</code>) represents the sign of the 
     * floating-point number. Bits 
     * 62-52 (the bits that are selected by the mask 
     * <code>0x7ff0000000000000L</code>) represent the exponent. Bits 51-0 
     * (the bits that are selected by the mask 
     * <code>0x000fffffffffffffL</code>) represent the significand 
     * (sometimes called the mantissa) of the floating-point number. 
     * <p>
     * If the argument is positive infinity, the result is
     * <code>0x7ff0000000000000L</code>.
     * <p>
     * If the argument is negative infinity, the result is
     * <code>0xfff0000000000000L</code>.
     * <p>
     * If the argument is NaN, the result is the <code>long</code>
     * integer representing the actual NaN value.  Unlike the
     * <code>doubleToLongBits</code> method,
     * <code>doubleToRawLongBits</code> does not collapse all the bit
     * patterns encoding a NaN to a single &quot;canonical&quot; NaN
     * value.
     * <p>
     * In all cases, the result is a <code>long</code> integer that,
     * when given to the {@link #longBitsToDouble(long)} method, will
     * produce a floating-point value the same as the argument to
     * <code>doubleToRawLongBits</code>.
     *
     * @param   value   a <code>double</code> precision floating-point number.
     * @return the bits that represent the floating-point number.
     */
    public static native long doubleToRawLongBits(double value);

    /**
     * Returns the <code>double</code> value corresponding to a given
     * bit representation.
     * The argument is considered to be a representation of a
     * floating-point value according to the IEEE 754 floating-point
     * "double format" bit layout.
     * <p>
     * If the argument is <code>0x7ff0000000000000L</code>, the result 
     * is positive infinity. 
     * <p>
     * If the argument is <code>0xfff0000000000000L</code>, the result 
     * is negative infinity. 
     * <p>
     * If the argument is any value in the range
     * <code>0x7ff0000000000001L</code> through
     * <code>0x7fffffffffffffffL</code> or in the range
     * <code>0xfff0000000000001L</code> through
     * <code>0xffffffffffffffffL</code>, the result is a NaN.  No IEEE
     * 754 floating-point operation provided by Java can distinguish
     * between two NaN values of the same type with different bit
     * patterns.  Distinct values of NaN are only distinguishable by
     * use of the <code>Double.doubleToRawLongBits</code> method.
     * <p>
     * In all other cases, let <i>s</i>, <i>e</i>, and <i>m</i> be three 
     * values that can be computed from the argument: 
     * <blockquote><pre>
     * int s = ((bits &gt;&gt; 63) == 0) ? 1 : -1;
     * int e = (int)((bits &gt;&gt; 52) & 0x7ffL);
     * long m = (e == 0) ?
     *                 (bits & 0xfffffffffffffL) &lt;&lt; 1 :
     *                 (bits & 0xfffffffffffffL) | 0x10000000000000L;
     * </pre></blockquote>
     * Then the floating-point result equals the value of the mathematical 
     * expression <i>s</i>&middot;<i>m</i>&middot;2<sup><i>e</i>-1075</sup>.
     *<p>
     * Note that this method may not be able to return a
     * <code>double</code> NaN with exactly same bit pattern as the
     * <code>long</code> argument.  IEEE 754 distinguishes between two
     * kinds of NaNs, quiet NaNs and <i>signaling NaNs</i>.  The
     * differences between the two kinds of NaN are generally not
     * visible in Java.  Arithmetic operations on signaling NaNs turn
     * them into quiet NaNs with a different, but often similar, bit
     * pattern.  However, on some processors merely copying a
     * signaling NaN also performs that conversion.  In particular,
     * copying a signaling NaN to return it to the calling method
     * may perform this conversion.  So <code>longBitsToDouble</code>
     * may not be able to return a <code>double</code> with a
     * signaling NaN bit pattern.  Consequently, for some
     * <code>long</code> values,
     * <code>doubleToRawLongBits(longBitsToDouble(start))</code> may
     * <i>not</i> equal <code>start</code>.  Moreover, which
     * particular bit patterns represent signaling NaNs is platform
     * dependent; although all NaN bit patterns, quiet or signaling,
     * must be in the NaN range identified above.
     *
     * @param   bits   any <code>long</code> integer.
     * @return  the <code>double</code> floating-point value with the same
     *          bit pattern.
     */
    public static native double longBitsToDouble(long bits);

    /**
     * Compares two <code>Double</code> objects numerically.  There
     * are two ways in which comparisons performed by this method
     * differ from those performed by the Java language numerical
     * comparison operators (<code>&lt;, &lt;=, ==, &gt;= &gt;</code>)
     * when applied to primitive <code>double</code> values:
     * <ul><li>
     *		<code>Double.NaN</code> is considered by this method
     *		to be equal to itself and greater than all other
     *		<code>double</code> values (including
     *		<code>Double.POSITIVE_INFINITY</code>).
     * <li>
     *		<code>0.0d</code> is considered by this method to be greater
     *		than <code>-0.0d</code>.
     * </ul>
     * This ensures that <code>Double.compareTo(Object)</code> (which
     * forwards its behavior to this method) obeys the general
     * contract for <code>Comparable.compareTo</code>, and that the
     * <i>natural order</i> on <code>Double</code>s is <i>consistent
     * with equals</i>.
     *
     * @param   anotherDouble   the <code>Double</code> to be compared.
     * @return  the value <code>0</code> if <code>anotherDouble</code> is
     *		numerically equal to this <code>Double</code>; a value
     *		less than <code>0</code> if this <code>Double</code>
     *		is numerically less than <code>anotherDouble</code>;
     *		and a value greater than <code>0</code> if this
     *		<code>Double</code> is numerically greater than
     *		<code>anotherDouble</code>.
     *		
     * @since   1.2
     * @see Comparable#compareTo(Object)
     */
    public int compareTo(Double anotherDouble) {
        return Double.compare(value, anotherDouble.value);
    }

    /**
     * Compares this <code>Double</code> object to another object.  If
     * the object is a <code>Double</code>, this function behaves like
     * <code>compareTo(Double)</code>.  Otherwise, it throws a
     * <code>ClassCastException</code> (as <code>Double</code> objects
     * are comparable only to other <code>Double</code> objects).
     *
     * @param   o the <code>Object</code> to be compared.
     * @return the value <code>0</code> if the argument is a
     *		<code>Double</code> numerically equal to this
     *		<code>Double</code>; a value less than <code>0</code>
     *		if the argument is a <code>Double</code> numerically
     *		greater than this <code>Double</code>; and a value
     *		greater than <code>0</code> if the argument is a
     *		<code>Double</code> numerically less than this
     *		<code>Double</code>.
     * @exception <code>ClassCastException</code> if the argument is not a
     *		  <code>Double</code>.
     * @see     java.lang.Comparable
     * @since 1.2
     */
    public int compareTo(Object o) {
	return compareTo((Double)o);
    }

    /**
     * Compares the two specified <code>double</code> values. The sign
     * of the integer value returned is the same as that of the
     * integer that would be returned by the call:
     * <pre>
     *    new Double(d1).compareTo(new Double(d2))
     * </pre>
     *
     * @param   d1        the first <code>double</code> to compare
     * @param   d2        the second <code>double</code> to compare
     * @return  the value <code>0</code> if <code>d1</code> is
     *		numerically equal to <code>d2</code>; a value less than
     *          <code>0</code> if <code>d1</code> is numerically less than
     *		<code>d2</code>; and a value greater than <code>0</code>
     *		if <code>d1</code> is numerically greater than
     *		<code>d2</code>.
     * @since 1.4
     */
    public static int compare(double d1, double d2) {
        if (d1 < d2)
            return -1;		 // Neither val is NaN, thisVal is smaller
        if (d1 > d2)
            return 1;		 // Neither val is NaN, thisVal is larger

        long thisBits = Double.doubleToLongBits(d1);
        long anotherBits = Double.doubleToLongBits(d2);

        return (thisBits == anotherBits ?  0 : // Values are equal
                (thisBits < anotherBits ? -1 : // (-0.0, 0.0) or (!NaN, NaN)
                 1));                          // (0.0, -0.0) or (NaN, !NaN)
    }

    /** use serialVersionUID from JDK 1.0.2 for interoperability */
    private static final long serialVersionUID = -9172774392245257468L;
}
