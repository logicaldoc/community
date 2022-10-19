package com.logicaldoc.core.parser;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Locale;

import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.StringUtil;

/**
 * Parses a MS Word (*.doc, *.dot) file to extract the text contained in the
 * file. This class uses the external library HWPF provided by the Apache
 * Jakarta POI project. Even though this library provides features to extract
 * the document author and version, we do not use those features, because the
 * library is known to be buggy. The important part is to get the text content,
 * not extracting the author, date, etc. is not essential.
 * 
 * @author Michael Scholz
 * @author Sebastian Stein
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class DOCParser extends RTFParser {

	protected static Logger log = LoggerFactory.getLogger(DOCParser.class);

	@Override
	public String parse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion) {
		try (BufferedInputStream bis = new BufferedInputStream(input)) {
			bis.mark(Integer.MAX_VALUE);

			String tmp = "";
			try (WordExtractor extractor = new WordExtractor(bis)) {
				tmp = extractor.getText();
			} catch (Throwable e) {
				// Maybe the document to be parsed is not a Word file.
				// Try to evaluate it as a RTF file.
			}

			try {
				bis.reset();
			} catch (Exception e) {
				// Noting to do
			}

			if (tmp.length() == 0) {
				// Try to evaluate it as a RTF file.
				tmp = super.extractText(bis);
			}

			// Replace Control characters
			if (tmp != null)
				tmp = tmp.replaceAll("[\\p{Cntrl}&&[^\\n]]", " ");
			return StringUtil.writeToString(new StringReader(tmp));
		} catch (Throwable e) {
			log.warn("Failed to extract Word text content", e);
		}
		return "";
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try (HWPFDocument wordDoc = new HWPFDocument(input)) {
			return wordDoc.getSummaryInformation().getPageCount();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}