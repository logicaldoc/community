package com.logicaldoc.core.parser.wordperfect;

import java.io.InputStream;
import java.io.StringReader;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.parser.AbstractParser;
import com.logicaldoc.util.StringUtil;

/**
 * @author Alessandro Gasparini
 * @since 4.5.2
 */
public class WordPerfectParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(WordPerfectParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuilder content) {
		try {
			WPStringExtractor extractor = new WPStringExtractor();
			String text = extractor.extract(input).trim();
			content.append(StringUtil.writeToString(new StringReader(text)));
		} catch (Exception e) {
			log.warn("Failed to extract WordPerfect text content", e);
		}
	}
}