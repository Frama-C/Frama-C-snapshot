/*
 * @(#)OutputStreamWriter.java	1.45 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.io;

//KML import java.nio.charset.Charset;
//KML import java.nio.charset.CharsetEncoder;
//KML import sun.nio.cs.StreamEncoder;


/**
 * An OutputStreamWriter is a bridge from character streams to byte streams:
 * Characters written to it are encoded into bytes using a specified {@link
 * java.nio.charset.Charset <code>charset</code>}.  The charset that it uses
 * may be specified by name or may be given explicitly, or the platform's
 * default charset may be accepted.
 *
 * <p> Each invocation of a write() method causes the encoding converter to be
 * invoked on the given character(s).  The resulting bytes are accumulated in a
 * buffer before being written to the underlying output stream.  The size of
 * this buffer may be specified, but by default it is large enough for most
 * purposes.  Note that the characters passed to the write() methods are not
 * buffered.
 *
 * <p> For top efficiency, consider wrapping an OutputStreamWriter within a
 * BufferedWriter so as to avoid frequent converter invocations.  For example:
 *
 * <pre>
 * Writer out
 *   = new BufferedWriter(new OutputStreamWriter(System.out));
 * </pre>
 *
 * <p> A <i>surrogate pair</i> is a character represented by a sequence of two
 * <tt>char</tt> values: A <i>high</i> surrogate in the range '&#92;uD800' to
 * '&#92;uDBFF' followed by a <i>low</i> surrogate in the range '&#92;uDC00' to
 * '&#92;uDFFF'.  If the character represented by a surrogate pair cannot be
 * encoded by a given charset then a charset-dependent <i>substitution
 * sequence</i> is written to the output stream.
 *
 * <p> A <i>malformed surrogate element</i> is a high surrogate that is not
 * followed by a low surrogate or a low surrogate that is not preceeded by a
 * high surrogate.  It is illegal to attempt to write a character stream
 * containing malformed surrogate elements.  The behavior of an instance of
 * this class when a malformed surrogate element is written is not specified.
 *
 * @see BufferedWriter
 * @see OutputStream
 * @see java.nio.charset.Charset
 *
 * @version 	1.45, 03/01/23
 * @author	Mark Reinhold
 * @since	JDK1.1
 */

public class OutputStreamWriter extends Writer {

    private final StreamEncoder se;

    /**
     * Create an OutputStreamWriter that uses the named charset.
     *
     * @param  out
     *         An OutputStream
     *
     * @param  charsetName
     *         The name of a supported
     *         {@link java.nio.charset.Charset </code>charset<code>}
     *
     * @exception  UnsupportedEncodingException
     *             If the named encoding is not supported
     */
    public OutputStreamWriter(OutputStream out, String charsetName)
	throws UnsupportedEncodingException
    {
	super(out);
	if (charsetName == null)
	    throw new NullPointerException("charsetName");
	se = StreamEncoder.forOutputStreamWriter(out, this, charsetName);
    }

    /**
     * Create an OutputStreamWriter that uses the default character encoding.
     *
     * @param  out  An OutputStream
     */
    public OutputStreamWriter(OutputStream out) {
	super(out);
	try {
	    se = StreamEncoder.forOutputStreamWriter(out, this, (String)null);
	} catch (UnsupportedEncodingException e) {
	    throw new Error(e);
        }
    }

    /**
     * Create an OutputStreamWriter that uses the given charset. </p>
     *
     * @param  out
     *         An OutputStream
     *
     * @param  cs
     *         A charset
     *
     * @since 1.4
     * @spec JSR-51
     */
    public OutputStreamWriter(OutputStream out, Charset cs) {
	super(out);
	if (cs == null)
	    throw new NullPointerException("charset");
	se = StreamEncoder.forOutputStreamWriter(out, this, cs);
    }

    /**
     * Create an OutputStreamWriter that uses the given charset encoder.  </p>
     *
     * @param  out
     *         An OutputStream
     *
     * @param  enc
     *         A charset encoder
     *
     * @since 1.4
     * @spec JSR-51
     */
    public OutputStreamWriter(OutputStream out, CharsetEncoder enc) {
	super(out);
	if (enc == null)
	    throw new NullPointerException("charset encoder");
	se = StreamEncoder.forOutputStreamWriter(out, this, enc);
    }

    /**
     * Return the name of the character encoding being used by this stream.
     *
     * <p> If the encoding has an historical name then that name is returned;
     * otherwise the encoding's canonical name is returned.
     *
     * <p> If this instance was created with the {@link
     * #OutputStreamWriter(OutputStream, String)} constructor then the returned
     * name, being unique for the encoding, may differ from the name passed to
     * the constructor.  This method may return <tt>null</tt> if the stream has
     * been closed. </p>
     *
     * @return The historical name of this encoding, or possibly
     *         <code>null</code> if the stream has been closed
     *
     * @see java.nio.charset.Charset
     *
     * @revised 1.4
     * @spec JSR-51
     */
    public String getEncoding() {
	return se.getEncoding();
    }



    /**
     * Flush the output buffer to the underlying byte stream, without flushing
     * the byte stream itself.  This method is non-private only so that it may
     * be invoked by PrintStream.
     */
    void flushBuffer() throws IOException {
	se.flushBuffer();
    }

    /**
     * Write a single character.
     *
     * @exception  IOException  If an I/O error occurs
     */
    public void write(int c) throws IOException {
	se.write(c);
    }

    /**
     * Write a portion of an array of characters.
     *
     * @param  cbuf  Buffer of characters
     * @param  off   Offset from which to start writing characters
     * @param  len   Number of characters to write
     *
     * @exception  IOException  If an I/O error occurs
     */
    public void write(char cbuf[], int off, int len) throws IOException {
	se.write(cbuf, off, len);
    }

    /**
     * Write a portion of a string.
     *
     * @param  str  A String
     * @param  off  Offset from which to start writing characters
     * @param  len  Number of characters to write
     *
     * @exception  IOException  If an I/O error occurs
     */
    public void write(String str, int off, int len) throws IOException {
	se.write(str, off, len);
    }

    /**
     * Flush the stream.
     *
     * @exception  IOException  If an I/O error occurs
     */
    public void flush() throws IOException {
	se.flush();
    }

    /**
     * Close the stream.
     *
     * @exception  IOException  If an I/O error occurs
     */
    public void close() throws IOException {
	se.close();
    }

}
