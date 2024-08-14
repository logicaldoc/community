package com.logicaldoc.core.parser;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.Locale;

import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;

public class PPTParserTest extends AbstractCoreTestCase {

	@Test
	public void testParse() throws ParsingException {
		String inputFile = "src/test/resources/accessibility.ppt";
		File file = new File(inputFile);
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		PPTParser ppt = (PPTParser) parser;
		String content = ppt.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);

		Assert.assertTrue(content.contains("Jakob Nielsen ne parla nel suo libro"));
	}
}