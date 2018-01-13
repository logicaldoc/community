package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;

import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.parser.TXTParser;
import com.logicaldoc.core.security.Tenant;

public class TXTParserTest extends AbstractCoreTCase{


	@Test
	public void testParse() throws UnsupportedEncodingException, FileNotFoundException {
		String inputFile = "src/test/resources/AnalyzeFileTest_enc.txt";
		File file = new File(inputFile);
		String filename = file.getPath();
		Parser parser = ParserFactory.getParser(filename, Tenant.DEFAULT_NAME);
		TXTParser p = (TXTParser) parser;
		p.parse(file);
		Assert.assertTrue(p.getContent().contains("scalpo"));
		p.parse(new FileInputStream(inputFile));
		Assert.assertTrue(p.getContent().contains("scalpo"));
	}
}