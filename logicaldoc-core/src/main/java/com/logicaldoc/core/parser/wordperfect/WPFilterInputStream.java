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

		if (result >= 0x20 && result <= 0x7E)
			return result;

		switch (result) {
		case 0x80:
			return ' ';
		case 0xA9:
			return '-';
		default:
			// do noting;
		}

		if (ArrayUtils.contains(intarry, result)) {
			result = substituteSpecialChars(result);
		} else if (result == 0xC0) {
			// this could mark the start of a 4-byte sequence representing a
			// non-ASCII character (e.g. an accented character)
			byte[] buf = new byte[3];
			int bytesRead = fillByteArray(in, buf);

			result = substituteNonASCII(buf, bytesRead);

			if (result == 0) {
				// no special sequence was recognized, unread the buffer
				((PushbackInputStream) in).unread(buf, 0, bytesRead);
			}

			return result;
		} else if (result == 0xC3 || result == 0xC4) {
			result = skipControlSequences(result);
		} else {
			// convert all other character to 0 to prevent StringExtractor to
			// accept accented
			// characters that were not encoded and thus had another meaning
			result = 0;
		}

		return result;
	}

	private int substituteNonASCII(byte[] buf, int bytesRead) {
		int result = 0;
		if (bytesRead == 3 && buf[2] == (byte) 0xC0) {
			// likely, a pattern was found
			if (buf[1] == (byte) 0x01) {
				switch (buf[0]) {
				case (byte) 0x17:
					result = 0xDF; // szlig
					break;
				case (byte) 0x1A:
					result = 0xC1; // Aacute
					break;
				case (byte) 0x1B:
					result = 0xE1; // aacute
					break;
				case (byte) 0x1C:
					result = 0xC2; // Acirc
					break;
				case (byte) 0x1D:
					result = 0xE2; // acirc
					break;
				case (byte) 0x1E:
					result = 0xC4; // Auml
					break;
				case (byte) 0x1F:
					result = 0xE4; // auml
					break;
				case (byte) 0x20:
					result = 0xC0; // Agrave
					break;
				case (byte) 0x21:
					result = 0xE0; // agrave
					break;
				case (byte) 0x22:
					result = 0xC5; // A with small circle
					break;
				case (byte) 0x23:
					result = 0xE5; // a with small circle
					break;
				case (byte) 0x24:
					result = 0xC6; // AE
					break;
				case (byte) 0x25:
					result = 0xE6; // ae
					break;
				case (byte) 0x26:
					result = 0xC7; // C-cedille
					break;
				case (byte) 0x27:
					result = 0xE7; // c-cedille
					break;
				case (byte) 0x28:
					result = 0xC9; // Eacute
					break;
				case (byte) 0x29:
					result = 0xE9; // eacute
					break;
				case (byte) 0x2A:
					result = 0xCA; // Ecirc
					break;
				case (byte) 0x2B:
					result = 0xEA; // ecirc
					break;
				case (byte) 0x2C:
					result = 0xCB; // Euml
					break;
				case (byte) 0x2D:
					result = 0xEB; // euml
					break;
				case (byte) 0x2E:
					result = 0xC8; // Egrave
					break;
				case (byte) 0x2F:
					result = 0xE8; // egrave
					break;
				case (byte) 0x30:
					result = 0xCD; // Iacute
					break;
				case (byte) 0x31:
					result = 0xED; // iacute
					break;
				case (byte) 0x32:
					result = 0xCE; // Icirc
					break;
				case (byte) 0x33:
					result = 0xEE; // icirc
					break;
				case (byte) 0x34:
					result = 0xCF; // Iuml
					break;
				case (byte) 0x35:
					result = 0xEF; // iuml
					break;
				case (byte) 0x36:
					result = 0xCC; // Igrave
					break;
				case (byte) 0x37:
					result = 0xEC; // igrave
					break;
				case (byte) 0x38:
					result = 0xD1; // Ntitle
					break;
				case (byte) 0x39:
					result = 0xF1; // ntitle
					break;
				case (byte) 0x3A:
					result = 0xD3; // Oacute
					break;
				case (byte) 0x3B:
					result = 0xF3; // oacute
					break;
				case (byte) 0x3C:
					result = 0xD4; // Ocirc
					break;
				case (byte) 0x3D:
					result = 0xF4; // ocirc
					break;
				case (byte) 0x3E:
					result = 0xD6; // Ouml
					break;
				case (byte) 0x3F:
					result = 0xF6; // ouml
					break;
				case (byte) 0x40:
					result = 0xD2; // Ograve
					break;
				case (byte) 0x41:
					result = 0xF2; // ograve
					break;
				case (byte) 0x42:
					result = 0xDA; // Uacute
					break;
				case (byte) 0x43:
					result = 0xFA; // uacute
					break;
				case (byte) 0x44:
					result = 0xDB; // Ucirc
					break;
				case (byte) 0x45:
					result = 0xFB; // ucirc
					break;
				case (byte) 0x46:
					result = 0xDC; // Uuml
					break;
				case (byte) 0x47:
					result = 0xFC; // uuml
					break;
				case (byte) 0x48:
					result = 0xD9; // Ugrave
					break;
				case (byte) 0x49:
					result = 0xF9; // ugrave
					break;
				case (byte) 0x4A:
					result = (int) 'Y'; // Yuml (not supported in ANSI)
					break;
				case (byte) 0x4B:
					result = 0xFF; // yuml
					break;
				case (byte) 0x4C:
					result = 0xC3; // Atilde
					break;
				case (byte) 0x4D:
					result = 0xE3; // atilde
					break;
				case (byte) 0x4E:
					result = 0xD0; // ETH (again, see 0x56)
					break;
				case (byte) 0x50:
					result = 0xD8; // Oslash
					break;
				case (byte) 0x51:
					result = 0xF8; // oslash
					break;
				case (byte) 0x52:
					result = 0xD5; // Otilde
					break;
				case (byte) 0x53:
					result = 0xF5; // otilde
					break;
				case (byte) 0x54:
					result = 0xDD; // Yacute
					break;
				case (byte) 0x55:
					result = 0xFD; // yacute
					break;
				case (byte) 0x56:
					result = 0xD0; // ETH
					break;
				case (byte) 0x57:
					result = 0xF0; // eth
					break;
				case (byte) 0x58:
					result = 0xDE; // THORN
					break;
				case (byte) 0x59:
					result = 0xFE; // thorn
					break;
				default:
					result = 0;
				}
			} else if (buf[1] == (byte) 0x04) {
				switch (buf[0]) {
				case (byte) 0x1C:
					result = 0x92; // quote
					break;
				case (byte) 0x1D:
					result = 0x91; // backquote
					break;
				default:
					result = 0;
				}
			}
		}
		return result;
	}

	private int skipControlSequences(int result) throws IOException {
		// this could mark the start of a 3-byte sequence representing a
		// change in type face (bold, italic, underlined).
		byte[] buf = new byte[2];
		int bytesRead = fillByteArray(in, buf);

		if (bytesRead == 2 && (result == 0xC3 && buf[1] == (byte) 0xC3 || result == 0xC4 && buf[1] == (byte) 0xC4)
				&& (buf[0] == (byte) 0x08 || buf[0] == (byte) 0x0C || buf[0] == (byte) 0x0E)) {
			// ignore control sequence
			result = read();
		} else {
			// no special sequence was recognized, unread the buffer
			((PushbackInputStream) in).unread(buf, 0, bytesRead);

			result = 0;
		}
		return result;
	}

	private int substituteSpecialChars(int result) {

		if (result == 0x05)
			return 0xe4; // auml
		if (result == 0x06)
			return 0xC4; // Auml
		if (result == 0x1a)
			return 0xf6; // ouml
		if (result == 0x1b)
			return 0xd6; // Ouml
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
	 * @param in The InputStream to read the bytes from.
	 * @param byteArray The byte array to fill with bytes
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
