package com.logicaldoc.core.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;

public class OpenOfficeParserTest extends AbstractCoreTestCase {

	@Test
	public void testParse() throws UnsupportedEncodingException, ParseException {
		String inputFile = "target/test-classes/logicaldoc-user_manual-en.odt";
		File file = new File(inputFile);
		String filename = file.getPath();

		for (int i = 0; i < 10; i++) {
			Parser parser = ParserFactory.getParser(filename);
			OpenOfficeParser p = (OpenOfficeParser) parser;

			String content = p.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);

			assertNotNull(content);
			assertTrue(StringUtils.isNotEmpty(content));

			assertEquals(69386, content.length());
		}
	}
}
