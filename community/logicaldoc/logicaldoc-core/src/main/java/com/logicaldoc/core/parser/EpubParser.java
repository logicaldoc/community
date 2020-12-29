package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.mertakdut.BookSection;
import com.github.mertakdut.Reader;
import com.github.mertakdut.exception.OutOfPagesException;
import com.logicaldoc.util.io.FileUtil;

/**
 * A specialized parser to extract text from .epub(e-books) format
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.3
 */
public class EpubParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(EpubParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			StringBuffer content) {
		File tmpFile = null;
		try {
			tmpFile = File.createTempFile("epubparser", ".epub");
			FileUtil.writeFile(input, tmpFile.getAbsolutePath());

			Reader reader = new Reader();
			reader.setIsIncludingTextContent(true);
			// Max string length for the current page
			// reader.setMaxContentPerSection(10000);
			reader.setFullContent(tmpFile.getAbsolutePath());

			for (int i = 0; i < Integer.MAX_VALUE; i++) {
				try {
					BookSection bookSection = reader.readSection(i);

					// Excludes html tags
					String sectionTextContent = bookSection.getSectionTextContent();
					if (StringUtils.isNotEmpty(sectionTextContent)) {
						if (content.length() > 0)
							content.append("\n");
						content.append(sectionTextContent);
					}
				} catch (OutOfPagesException t) {
					break;
				}
			}
		} catch (Throwable ex) {
			log.error(ex.getMessage(), ex);
		} finally {
			FileUtil.strongDelete(tmpFile);
		}
	}
}