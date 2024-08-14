package com.logicaldoc.core.parser;

import java.io.File;
import java.util.Locale;

import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;

public class EpubParserTest extends AbstractCoreTestCase {

	@Test
	public void testParse() throws ParsingException {
		String inputFile = "src/test/resources/aliceDynamic.epub";
		File file = new File(inputFile);
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		EpubParser epubp = (EpubParser) parser;

		String content = epubp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		Assert.assertTrue(content.contains("wonder"));
		Assert.assertTrue(content.contains("Alice"));
	}
}
