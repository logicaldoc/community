package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hslf.usermodel.HSLFSlideShow;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.mertakdut.BookSection;
import com.github.mertakdut.Reader;
import com.github.mertakdut.exception.OutOfPagesException;
import com.github.mertakdut.exception.ReadingException;
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
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		File tmpFile = null;
		try {
			tmpFile = FileUtil.createTempFile("epubparser", ".epub");
			FileUtil.writeFile(input, tmpFile.getAbsolutePath());

			Reader reader = new Reader();
			reader.setIsIncludingTextContent(true);
			// Max string length for the current page
			reader.setFullContent(tmpFile.getAbsolutePath());

			extractSections(reader, content);
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		} finally {
			FileUtil.delete(tmpFile);
		}
	}

	private void extractSections(Reader reader, StringBuilder content) throws ReadingException {
		for (int sectionIndex = 0; sectionIndex < Integer.MAX_VALUE; sectionIndex++) {
			try {
				BookSection bookSection = reader.readSection(sectionIndex);

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
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try (HSLFSlideShow pptDoc = new HSLFSlideShow(input)) {
			return pptDoc.getSlides().size();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}