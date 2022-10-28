package com.logicaldoc.core.parser.wordperfect;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.Map;

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

	protected static final int[] intarry = new int[] { 0x05, 0x06, 0x1a, 0x1b, 0x1c, 0x1d };

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
	@Override
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
			// do noting
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
				Map<Byte, Integer> map = new HashMap<>();
				map.put((byte) 0x17, 0xDF); // szlig
				map.put((byte) 0x1A, 0xC1); // Aacute
				map.put((byte) 0x1B, 0xE1); // aacute
				map.put((byte) 0x1C, 0xC2); // Acirc
				map.put((byte) 0x1D, 0xE2); // acirc
				map.put((byte) 0x1E, 0xC4); // Auml
				map.put((byte) 0x1F, 0xE4); // auml
				map.put((byte) 0x20, 0xC0); // Agrave
				map.put((byte) 0x21, 0xE0); // agrave
				map.put((byte) 0x22, 0xC5); // A with small circle
				map.put((byte) 0x23, 0xE5); // a with small circle
				map.put((byte) 0x24, 0xC6); // AE
				map.put((byte) 0x25, 0xE6); // ae
				map.put((byte) 0x26, 0xC7); // C-cedille
				map.put((byte) 0x27, 0xE7); // c-cedille
				map.put((byte) 0x28, 0xC9); // Eacute
				map.put((byte) 0x29, 0xE9); // eacute
				map.put((byte) 0x2A, 0xCA); // Ecirc
				map.put((byte) 0x2B, 0xEA); // ecirc
				map.put((byte) 0x2C, 0xCB); // Euml
				map.put((byte) 0x2D, 0xEB); // euml
				map.put((byte) 0x2E, 0xC8); // Egrave
				map.put((byte) 0x2F, 0xE8); // egrave
				map.put((byte) 0x30, 0xCD); // Iacute
				map.put((byte) 0x31, 0xED); // iacute
				map.put((byte) 0x32, 0xCE); // Icirc
				map.put((byte) 0x33, 0xEE); // icirc
				map.put((byte) 0x34, 0xCF); // Iuml
				map.put((byte) 0x35, 0xEF); // iuml
				map.put((byte) 0x36, 0xCC); // Igrave
				map.put((byte) 0x37, 0xEC); // igrave
				map.put((byte) 0x38, 0xD1); // Ntitle
				map.put((byte) 0x39, 0xF1); // ntitle
				map.put((byte) 0x3A, 0xD3); // Oacute
				map.put((byte) 0x3B, 0xF3); // oacute
				map.put((byte) 0x3C, 0xD4); // Ocirc
				map.put((byte) 0x3D, 0xF4); // ocirc
				map.put((byte) 0x3E, 0xD6); // Ouml
				map.put((byte) 0x3F, 0xF6); // ouml
				map.put((byte) 0x40, 0xD2); // Ograve
				map.put((byte) 0x41, 0xF2); // ograve
				map.put((byte) 0x42, 0xDA); // Uacute
				map.put((byte) 0x43, 0xFA); // uacute
				map.put((byte) 0x44, 0xDB); // Ucirc
				map.put((byte) 0x45, 0xFB); // ucirc
				map.put((byte) 0x46, 0xDC); // Uuml
				map.put((byte) 0x47, 0xFC); // uuml
				map.put((byte) 0x48, 0xD9); // Ugrave
				map.put((byte) 0x49, 0xF9); // ugrave
				map.put((byte) 0x4A, (int)'Y'); // Yuml (not supported in
											// ANSI)
				map.put((byte) 0x4B, 0xFF); // yuml
				map.put((byte) 0x4C, 0xC3); // Atilde
				map.put((byte) 0x4D, 0xE3); // atilde
				map.put((byte) 0x4E, 0xD0); // ETH (again, see 0x56)
				map.put((byte) 0x50, 0xD8); // Oslash
				map.put((byte) 0x51, 0xF8); // oslash
				map.put((byte) 0x52, 0xD5); // Otilde
				map.put((byte) 0x53, 0xF5); // otilde
				map.put((byte) 0x54, 0xDD); // Yacute
				map.put((byte) 0x55, 0xFD); // yacute
				map.put((byte) 0x56, 0xD0); // ETH
				map.put((byte) 0x57, 0xF0); // eth
				map.put((byte) 0x58, 0xDE); // THORN
				map.put((byte) 0x59, 0xFE); // thorn

				if (map.containsKey(buf[0]))
					result = map.get(buf[0]);
				else
					result = 0;
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

	@Override
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
