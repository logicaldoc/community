package com.logicaldoc.core.parser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.MarkdownConverter;
import com.logicaldoc.util.io.FileUtil;

/**
 * Text extractor for the Markdown language.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class MarkdownParser extends HTMLParser {

	/**
	 * Logger instance.
	 */
	protected static Logger log = LoggerFactory.getLogger(MarkdownParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content)
			throws ParsingException {
		File html = null;
		File md = null;
		try {
			html = FileUtil.createTempFile("mdparse", ".html");
			md = FileUtil.createTempFile("mdparse", ".md");
			FileUtil.writeFile(input, md.getAbsolutePath());

			MarkdownConverter converter = new MarkdownConverter();
			converter.convert(md, html);

			parseHtml(html, parameters, content);
		} catch (IOException e) {
			throw new ParsingException(e);
		} finally {
			FileUtil.delete(html);
			FileUtil.delete(md);
		}

	}

	protected void parseHtml(File html, ParseParameters parameters, StringBuilder content)
			throws ParsingException, IOException, FileNotFoundException {
		try (InputStream htmlInput = new BufferedInputStream(new FileInputStream(html))) {
			super.internalParse(htmlInput, parameters, content);
		}
	}
}