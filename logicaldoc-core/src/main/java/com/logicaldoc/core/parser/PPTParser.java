package com.logicaldoc.core.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import org.apache.poi.hslf.usermodel.HSLFShape;
import org.apache.poi.hslf.usermodel.HSLFSlideShow;
import org.apache.poi.hslf.usermodel.HSLFSlideShowImpl;
import org.apache.poi.hslf.usermodel.HSLFTextParagraph;
import org.apache.poi.sl.extractor.SlideShowExtractor;
import org.apache.poi.sl.usermodel.SlideShow;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.StringUtil;

/**
 * Parser for Office 2003 presentations
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class PPTParser extends AbstractParser {

	private static final Logger log = LoggerFactory.getLogger(PPTParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {

		try (SlideShow<HSLFShape, HSLFTextParagraph> slideshow = new HSLFSlideShow(input);
				SlideShowExtractor<HSLFShape, HSLFTextParagraph> slideShowExtractor = new SlideShowExtractor<>(
						slideshow);) {

			slideShowExtractor.setCommentsByDefault(true);
			slideShowExtractor.setMasterByDefault(true);
			slideShowExtractor.setNotesByDefault(true);
			String tmp = slideShowExtractor.getText();

			// Replace Control characters
			if (tmp != null)
				tmp = tmp.replaceAll("[\\p{Cntrl}&&[^\\n]]", " ");

			content.append(StringUtil.writeToString(new StringReader(tmp)));
		} catch (Exception e) {
			log.warn("Failed to extract PowerPoint text content", e);
		}
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try (HSLFSlideShowImpl pptDoc = new HSLFSlideShowImpl(input)) {
			return pptDoc.getSummaryInformation().getPageCount();
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}