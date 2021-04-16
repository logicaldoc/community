package com.logicaldoc.core.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Locale;

import org.apache.poi.hslf.extractor.PowerPointExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.StringUtil;

/**
 * Parser for Office 2003 presentations
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class PPTParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(PPTParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) {
		PowerPointExtractor extractor = null;
		try {
			extractor = new PowerPointExtractor(input);
			String tmp = extractor.getText(true, true);

			// Replace Control characters
			if (tmp != null)
				tmp = tmp.replaceAll("[\\p{Cntrl}&&[^\\n]]", " ");

			content.append(StringUtil.writeToString(new StringReader(tmp)));
		} catch (Exception e) {
			log.warn("Failed to extract PowerPoint text content", e);
		} finally {
			if (extractor != null)
				try {
					extractor.close();
				} catch (IOException e) {
				}
		}
	}
}