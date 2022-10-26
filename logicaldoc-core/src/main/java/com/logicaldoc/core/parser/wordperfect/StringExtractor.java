package com.logicaldoc.core.parser.wordperfect;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * StringExtractor uses a set of heuristics to extract as much human-readable text as possible from a binary
 * stream. This is useful for binary document formats that often or always contain the document text as ascii
 * characters (e.g. MS Office files), intermixed with binary parts. When such a document could not be parsed
 * using the appropriate library (e.g., Apache POI), a StringExtractor might still be able to produce some
 * meaningful content and can thus serve as a fallback.
 * 
 * <p>
 * The output of StringExtractor is suited for text indexing but less for human consumption, as any formatting
 * will most likely be lost and some amount of unwanted characters slipping through can also not be prevented.
 */
public class StringExtractor {

	private static final String END_OF_LINE = System.getProperty("line.separator", "\n");

	// the names of some commonly appearing fonts. note: they are all lowercased
	public static final String[] COMMON_FONT_NAMES = new String[] { "albertus medium", "albertus extra bold",
			"algerian", "antique olive", "arial", "book antiqua", "bookman old style", "braggadocio",
			"britannic bold", "brush script mt", "century gothic", "century schoolbook", "cg omega",
			"cg times", "clarendon condensed", "colonna mt", "coronet", "courier", "courier new",
			"desdemona", "footlight mt light", "garamond", "helvetica", "impact", "kino mt", "line printer",
			"marigold", "matura mt script capitals", "modern", "monotype corsiva", "letter gothic",
			"playbill", "roman", "script", "symbol", "tahoma", "times new roman", "times roman", "univers",
			"univers condensed", "verdana", "wide latin", "wingdings" };

	/**
	 * Extract all human-readable text from an InputStream.
	 * 
	 * @param stream The InputStream to read the bytes from. The stream will be fully consumed but not closed.
	 * @return The resulting, heuristically determined text. A String is always returned, although it can be
	 *         empty.
	 * @throws IOException When reading characters from the InputStream caused an IOException.
	 */
	public String extract(InputStream stream) throws IOException {
		// read all bytes and extract readable characters
		StringBuilder lineBuffer = new StringBuilder(512);
		StringBuilder textBuffer = new StringBuilder(64 * 1024);

		int b = -1;
		while ((b = stream.read()) != -1) {
			// test whether it is a reasonable readable character
			if (isTextCharacter(b)) {
				// append to the current line we're processing
				lineBuffer.append((char) b);
			}
			else if (lineBuffer.length() > 0) {
				// it's not and therefore marks an end of line
				// process the line we have gathered
				String line = lineBuffer.toString();
				lineBuffer.setLength(0);

				// perform some post processing, possibly invalidating the found line of text
				line = postProcessLine(line);

				if (line != null) {
					String lineLowerCase = line.toLowerCase();

					if (isStartLine(lineLowerCase)) {
						// scrap everything until this start line and continue with the next
						textBuffer.setLength(0);
					}
					else if (isValidLine(lineLowerCase)) {
						// append the original, non-lowercased line to the end result and continue processing
						// the stream
						textBuffer.append(line);
						textBuffer.append(END_OF_LINE);
					}
				}
			}
		}

		return textBuffer.toString();
	}

	/**
	 * Determines whether the supplied line indicates the start of the textual contents. If 'true', all text
	 * extracted up to this point will be ignored, i.e. text extraction will start again from scratch but at
	 * the current location in the stream. The specified line is expected to be fully lowercased. This default
	 * implementation returns 'false'.
	 */
	protected boolean isStartLine(String lineLowerCase) {
		return false;
	}

	/**
	 * Determines whether the supplied line should be included in the end result. The specified line is
	 * expected to be fully lowercased.
	 */
	protected boolean isValidLine(String lineLowerCase) {
		// Check if line starts with a font name
		for (int i = 0; i < COMMON_FONT_NAMES.length; i++) {
			if (lineLowerCase.startsWith(COMMON_FONT_NAMES[i])) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Checks whether the supplied character is a text character. By default, this method returns true for
	 * letters and single quotes.
	 */
	protected boolean isTextCharacter(int charNumber) {
		return charNumber >= 32 && charNumber <= 126 || // readable ASCII characters
				//charNumber >= 128 && charNumber <= 168 || // letters with accents, currency symbols, etc.
				charNumber == 9; // tab
	}

	protected String postProcessLine(String line) {
		line = line.trim();

		if (line.length() <= 2) {
			// line too short
			line = null;
		}
		else {
			// line should contain at least one 'normal' word
			boolean containsWord = false;

			StringTokenizer st = new StringTokenizer(line, " ");
			while (st.hasMoreTokens() && containsWord == false) {
				containsWord = isNormalWord(st.nextToken());
			}

			if (!containsWord) {
				line = null;
			}
		}

		return line;
	}

	protected boolean isNormalWord(String word) {
		boolean result = false;

		int wordLength = word.length();

		if (wordLength > 0) {
			char lastChar = word.charAt(wordLength - 1);
			if (lastChar == '.' || lastChar == ',') {
				wordLength--;
			}
		}

		if (wordLength >= 3) {
			result = true;

			// check if the word contains non-letter characters
			for (int i = 0; i < wordLength && result == true; i++) {
				if (!Character.isLetter(word.charAt(i))) {
					result = false;
				}
			}

			// Check use of upper- and lower case. Note that we can't check for
			// lower case, as some non-Latin characters do not have a case at all.
			// Allowed patterns are:
			// - all upper case
			// - none upper case
			// - first character upper case, rest not upper case
			result = checkUpperAndLowerCase(word, wordLength, result);

			// check character frequency
			result = checkCharacterFrequency(word, wordLength, result);
		}

		return result;
	}

	private boolean checkUpperAndLowerCase(String word, int wordLength, boolean result) {
		if (Character.isUpperCase(word.charAt(0))) {
			if (Character.isUpperCase(word.charAt(1))) {
				// all upper case?
				for (int i = 2; i < wordLength && result == true; i++) {
					result = Character.isUpperCase(word.charAt(i));
				}
			}
			else {
				// rest not upper case?
				for (int i = 2; i < wordLength && result == true; i++) {
					result = !Character.isUpperCase(word.charAt(i));
				}
			}
		}
		else {
			// all not upper case?
			for (int i = 0; i < wordLength && result == true; i++) {
				result = !Character.isUpperCase(word.charAt(i));
			}
		}
		return result;
	}

	private boolean checkCharacterFrequency(String word, int wordLength, boolean result) {
		if (result == true) {
			Map charFreq = new HashMap(32);
			for (int i = 0; i < wordLength; i++) {
				Character c = new Character(word.charAt(i));

				Integer freq = (Integer) charFreq.get(c);
				if (freq == null) {
					freq = new Integer(1);
				}
				else {
					freq = new Integer(freq.intValue() + 1);
				}
				charFreq.put(c, freq);
			}

			// no word should consist for 50% or more of a single character
			int freqThreshold = wordLength / 2;

			Iterator valueIter = charFreq.values().iterator();
			while (valueIter.hasNext() && result == true) {
				Integer freq = (Integer) valueIter.next();
				result = (freq.intValue() < freqThreshold);
			}
		}
		return result;
	}
}
