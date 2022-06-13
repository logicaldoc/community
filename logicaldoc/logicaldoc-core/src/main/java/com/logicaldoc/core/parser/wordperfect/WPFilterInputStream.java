package com.logicaldoc.core.parser.wordperfect;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;

import org.apache.commons.lang.ArrayUtils;

/**
 * A FilterInputStream that processes bytes that have a special meaning in
 * WordPerfect documents. Processed characters are: <code>0x80</code> (space
 * character in WP 6), <code>0xA9</code> (hyphen in WP 6) and <code>0xAC</code>
 * (word break indicator).
 * 
 * <p>
 * This class is made available as a utility class as other file formats may
 * also use WordPerfect's file structure, e.g. Corel Presentations 3.0 files.
 */
public class WPFilterInputStream extends FilterInputStream {

	protected final static int[] intarry = new int[] { 0x05, 0x06, 0x1a, 0x1b, 0x1c, 0x1d };

	// The contents of this class was reverse engineered from example
	// WordPerfect documents.
	// If someone wishes to improve on it, maybe this documentation (which we
	// encountered later) may be
	// helpful: http://www.wotsit.org/download.asp?f=wp51ref

	public WPFilterInputStream(InputStream in) {
		super(new PushbackInputStream(in, 4));
	}

	/**
	 * Converts all characters to 0 except for the readable ASCII-characters and
	 * characters from special byte sequences.
	 */
	public int read() throws IOException {
		int result = in.read();

		if (result >= 0x20 && result <= 0x7E) {
			// readable ASCII character, leave it intact
		}
		else if (result == -1) {
			// end of file, leave it intact
		}
		else if (result == 0x80) {
			// replace WP 6 space with normal space
			result = ' ';
		}
		else if (result == 0xA9) {
			// replace special word separator with normal '-'
			result = '-';
		}
		else if (result == 0xAC) {
			// remove word break indicators from stream
			result = read();
		} else if (ArrayUtils.contains(intarry, result)) {
			result = substituteSpecialChars(result);
		} else if (result == 0xC0) {
				// this could mark the start of a 4-byte sequence representing a
				// non-ASCII character (e.g. an accented character)
				byte[] buf = new byte[3];
				int bytesRead = fillByteArray(in, buf);

				if (bytesRead == 3 && buf[2] == (byte) 0xC0) {
					// likely, a pattern was found
					if (buf[1] == (byte) 0x01) {
						switch (buf[0]) {
						case (byte) 0x17:
							return 0xDF; // szlig
						case (byte) 0x1A:
							return 0xC1; // Aacute
						case (byte) 0x1B:
							return 0xE1; // aacute
						case (byte) 0x1C:
							return 0xC2; // Acirc
						case (byte) 0x1D:
							return 0xE2; // acirc
						case (byte) 0x1E:
							return 0xC4; // Auml
						case (byte) 0x1F:
							return 0xE4; // auml
						case (byte) 0x20:
							return 0xC0; // Agrave
						case (byte) 0x21:
							return 0xE0; // agrave
						case (byte) 0x22:
							return 0xC5; // A with small circle
						case (byte) 0x23:
							return 0xE5; // a with small circle
						case (byte) 0x24:
							return 0xC6; // AE
						case (byte) 0x25:
							return 0xE6; // ae
						case (byte) 0x26:
							return 0xC7; // C-cedille
						case (byte) 0x27:
							return 0xE7; // c-cedille
						case (byte) 0x28:
							return 0xC9; // Eacute
						case (byte) 0x29:
							return 0xE9; // eacute
						case (byte) 0x2A:
							return 0xCA; // Ecirc
						case (byte) 0x2B:
							return 0xEA; // ecirc
						case (byte) 0x2C:
							return 0xCB; // Euml
						case (byte) 0x2D:
							return 0xEB; // euml
						case (byte) 0x2E:
							return 0xC8; // Egrave
						case (byte) 0x2F:
							return 0xE8; // egrave
						case (byte) 0x30:
							return 0xCD; // Iacute
						case (byte) 0x31:
							return 0xED; // iacute
						case (byte) 0x32:
							return 0xCE; // Icirc
						case (byte) 0x33:
							return 0xEE; // icirc
						case (byte) 0x34:
							return 0xCF; // Iuml
						case (byte) 0x35:
							return 0xEF; // iuml
						case (byte) 0x36:
							return 0xCC; // Igrave
						case (byte) 0x37:
							return 0xEC; // igrave
						case (byte) 0x38:
							return 0xD1; // Ntitle
						case (byte) 0x39:
							return 0xF1; // ntitle
						case (byte) 0x3A:
							return 0xD3; // Oacute
						case (byte) 0x3B:
							return 0xF3; // oacute
						case (byte) 0x3C:
							return 0xD4; // Ocirc
						case (byte) 0x3D:
							return 0xF4; // ocirc
						case (byte) 0x3E:
							return 0xD6; // Ouml
						case (byte) 0x3F:
							return 0xF6; // ouml
						case (byte) 0x40:
							return 0xD2; // Ograve
						case (byte) 0x41:
							return 0xF2; // ograve
						case (byte) 0x42:
							return 0xDA; // Uacute
						case (byte) 0x43:
							return 0xFA; // uacute
						case (byte) 0x44:
							return 0xDB; // Ucirc
						case (byte) 0x45:
							return 0xFB; // ucirc
						case (byte) 0x46:
							return 0xDC; // Uuml
						case (byte) 0x47:
							return 0xFC; // uuml
						case (byte) 0x48:
							return 0xD9; // Ugrave
						case (byte) 0x49:
							return 0xF9; // ugrave
						case (byte) 0x4A:
							return (int) 'Y'; // Yuml (not supported in ANSI)
						case (byte) 0x4B:
							return 0xFF; // yuml
						case (byte) 0x4C:
							return 0xC3; // Atilde
						case (byte) 0x4D:
							return 0xE3; // atilde
						case (byte) 0x4E:
							return 0xD0; // ETH (again, see 0x56)
						case (byte) 0x50:
							return 0xD8; // Oslash
						case (byte) 0x51:
							return 0xF8; // oslash
						case (byte) 0x52:
							return 0xD5; // Otilde
						case (byte) 0x53:
							return 0xF5; // otilde
						case (byte) 0x54:
							return 0xDD; // Yacute
						case (byte) 0x55:
							return 0xFD; // yacute
						case (byte) 0x56:
							return 0xD0; // ETH
						case (byte) 0x57:
							return 0xF0; // eth
						case (byte) 0x58:
							return 0xDE; // THORN
						case (byte) 0x59:
							return 0xFE; // thorn
						}
					}
					else if (buf[1] == (byte) 0x04) {
						switch (buf[0]) {
						case (byte) 0x1C:
							return 0x92; // quote
						case (byte) 0x1D:
							return 0x91; // backquote
						}
					}
				}

				// no special sequence was recognized, unread the buffer
				((PushbackInputStream) in).unread(buf, 0, bytesRead);

				result = 0;
			}
			else if (result == 0xC3 || result == 0xC4) {
				// this could mark the start of a 3-byte sequence representing a
				// change in type face (bold, italic, underlined).
				byte[] buf = new byte[2];
				int bytesRead = fillByteArray(in, buf);

				if (bytesRead == 2
						&& (result == 0xC3 && buf[1] == (byte) 0xC3 || result == 0xC4 && buf[1] == (byte) 0xC4)
						&& (buf[0] == (byte) 0x08 || buf[0] == (byte) 0x0C || buf[0] == (byte) 0x0E)) {
					// ignore control sequence
					result = read();
				}
				else {
					// no special sequence was recognized, unread the buffer
					((PushbackInputStream) in).unread(buf, 0, bytesRead);

					result = 0;
				}
			}
			else {
				// convert all other character to 0 to prevent StringExtractor to accept accented
				// characters that were not encoded and thus had another meaning
				result = 0;
			}

		return result;
	}

	private int substituteSpecialChars(int result) {

//		if (result == 0x80)
//			return 0x20; // word space
//		if (result == 0xD0)
//			return 0x0D; // Line break

		if (result == 0x05)
			return 0xe4; // auml
		if (result == 0x06)
			return 0xC4; // Auml
		if (result == 0x1a)
			return 0xf6; // ouml
		if (result == 0x1b)
			return 0xd6; // Ouml
			// if (result == 0x20)
			// return 0xdf; // ß 223 0xdf &szlig;
		if (result == 0x1c)
			return 0xfc; // uuml
		if (result == 0x1d)
			return 0xdc; // Uuml

		return 0;
	}

	public int read(byte[] byteArray, int off, int len) throws IOException {
		int i = off;
		for (; i < off + len; i++) {
			int b = read();

			if (b == -1) {
				if (i == off) {
					// no bytes were available
					return -1;
				}
				break;
			}

			byteArray[i] = (byte) b;
		}

		// return the number of bytes that were read
		return i - off;
	}

	/**
	 * Fills the supplied byte array with bytes read from the specified
	 * InputStream. This method will only stop reading when the byte array has
	 * been filled completely, or when the end of the stream has been reached
	 * 
	 * @param in
	 *            The InputStream to read the bytes from.
	 * @param byteArray
	 *            The byte array to fill with bytes
	 *            
	 * @return The number of bytes written to the byte array
	 * 
	 * @throws IOException if an error happens retrieving the stream
	 */
	public int fillByteArray(InputStream in, byte[] byteArray) throws IOException {
		int result = 0;

		int bytesRead = in.read(byteArray);

		while (bytesRead >= 0) {
			result += bytesRead;

			if (result == byteArray.length) {
				break;
			}
			bytesRead = in.read(byteArray, result, byteArray.length - result);
		}
		return result;
	}

}
