package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.jsoup.Jsoup;

import com.logicaldoc.util.io.FileUtil;

/**
 * Text extractor for HyperText Markup Language (HTML).
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class HTMLParser extends AbstractParser {

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content)
			throws ParsingException {
		File tempFile = null;
		try {
			tempFile = FileUtil.createTempFile("html", ".html");
			FileUtil.writeFile(input, tempFile.getAbsolutePath());
			org.jsoup.nodes.Document doc = Jsoup.parse(tempFile);
			String title = doc.title();
			String body = doc.body().text();
			content.append(title + "\n" + body);
		} catch (IOException e) {
			throw new ParsingException(e);
		} finally {
			FileUtil.delete(tempFile);
		}
	}
}