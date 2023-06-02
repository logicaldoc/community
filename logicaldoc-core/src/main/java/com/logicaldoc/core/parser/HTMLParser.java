package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;

import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

/**
 * Text extractor for HyperText Markup Language (HTML).
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class HTMLParser extends AbstractParser {

	/**
	 * Logger instance.
	 */
	protected static Logger log = LoggerFactory.getLogger(HTMLParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		try {
			org.jsoup.nodes.Document doc = Jsoup.parse(FileUtil.readFile(new File("test.html")));
			String title = doc.title();
			String body = doc.body().text();
			content.append(title + "\n" + body);
		} catch (Exception e) {
			if (log.isDebugEnabled())
				log.debug("Failed to extract HTML text content", e);
			else
				log.warn("Failed to extract HTML text content");
		}
	}
}