package com.logicaldoc.core.parser.wordperfect;

import java.io.IOException;
import java.io.InputStream;


/**
 * A StringExtractor extension optimized for processing WordPerfect document streams.
 * 
 * <p>
 * This class is made available as a utility class as other file formats may also use WordPerfect's file
 * structure, e.g. Corel Presentations 3.0 files.
 */
public class WPStringExtractor extends StringExtractor {

	private static final String[] EXACT_START_LINES = { "doc init", "tech init" };

	private static final String[] START_EXCLUDES = { "wpc", "monotype sorts", "section", "columns",
			"aligned ", "standard", "default ", "biblio", "footnote", "gfootnote", "endnote", "heading",
			"header for ", "underlined heading", "centered heading", "technical", "object #",
			"microsoft word" };

	private static final String[] END_EXCLUDES = { "aligned paragraph numbers", "heading", "bullet list"
	// " style", " roman", "laserJet", "bullet list", "defaults", "typestyle", "land", "landscape", "portrait"
	};

	private static final String[] EXACT_EXCLUDES = { "nlus.", "usjp", "initialize technical style",
			"document style", "pleading", "times", "and", "where", "left", "right", "over", "(k over",
			"document", "header", "footer", "itemize", "page number", "pages", "body text", "word",
			"sjablone", "d printer" };

	private static final String[] CONTAIN_EXCLUDES = { "left (", "right )", "right ]", "right par",
			"default paragraph", };

	/**
	 * Wraps the specified InputStream in a WPFilterInputStream and passes it to the super class.
	 */
	public String extract(InputStream stream) throws IOException {
		WPFilterInputStream wpfis = new WPFilterInputStream(stream);
		String text = super.extract(wpfis);
		//wpfis.closeExtraOut();
		return text;
	}
	
	// overrides StringExtractor.isTextCharacter
	protected boolean isTextCharacter(int charNumber) {
		
		boolean xxx = super.isTextCharacter(charNumber) || charNumber >= 0xC0 && charNumber <= 0xFF
		// accented ANSI character
		|| charNumber == 0x91 // backquote
		|| charNumber == 0x92; // quote;
		
//		if (xxx == false) {
//			System.out.println(charNumber + " Is NOT valid char");
//		} 
		
		return xxx;
	}

	// overrides StringExtractor.isStartLine
	protected boolean isStartLine(String lineLowerCase) {
		for (int i = 0; i < EXACT_START_LINES.length; i++) {
			if (lineLowerCase.equals(EXACT_START_LINES[i])) {
				return true;
			}
		}
		return false;
	}

	// overrides StringExtractor.isValidLine
	protected boolean isValidLine(String lineLowerCase) {
		for (int i = 0; i < EXACT_EXCLUDES.length; i++) {
			if (lineLowerCase.equals(EXACT_EXCLUDES[i])) {
				return false;
			}
		}

		for (int i = 0; i < START_EXCLUDES.length; i++) {
			if (lineLowerCase.startsWith(START_EXCLUDES[i])) {
				return false;
			}
		}

		for (int i = 0; i < END_EXCLUDES.length; i++) {
			if (lineLowerCase.endsWith(END_EXCLUDES[i])) {
				return false;
			}
		}

		// most expensive operation: make sure this is the last check
		for (int i = 0; i < CONTAIN_EXCLUDES.length; i++) {
			if (lineLowerCase.indexOf(CONTAIN_EXCLUDES[i]) >= 0) {
				return false;
			}
		}

		return super.isValidLine(lineLowerCase);
	}
}
