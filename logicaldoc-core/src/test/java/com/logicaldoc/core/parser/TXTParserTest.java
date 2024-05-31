package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.Locale;

import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;

public class TXTParserTest extends AbstractCoreTestCase {

	@Test
	public void testParse() throws UnsupportedEncodingException, FileNotFoundException, ParsingException {
		String inputFile = "src/test/resources/AnalyzeFileTest_enc.txt";
		File file = new File(inputFile);
		String filename = file.getPath();
		Parser parser = ParserFactory.getParser(filename);
		TXTParser p = (TXTParser) parser;
		String content = p.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		Assert.assertTrue(content.contains("scalpo"));
		content = p.parse(new FileInputStream(inputFile), filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		Assert.assertTrue(content.contains("scalpo"));
	}
}