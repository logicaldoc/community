package com.logicaldoc.core.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.security.Tenant;

public class OpenOfficeParserTest extends AbstractCoreTCase {

	private long startTime;

	private long mem1;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		super.setUp();
		this.startTime = System.currentTimeMillis();
		this.mem1 = Runtime.getRuntime().totalMemory();
		System.out.println("freeMemory: " + Runtime.getRuntime().freeMemory());
		System.out.println("totalMemory: " + Runtime.getRuntime().totalMemory());
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		super.tearDown();
		long elapsedMillis = System.currentTimeMillis() - this.startTime;
		System.err.println("elapsedMillis: " + elapsedMillis);
		long mem2 = Runtime.getRuntime().totalMemory();

		System.err.println("freeMemory AFTER: " + Runtime.getRuntime().freeMemory());
		System.err.println("totalMemory AFTER: " + Runtime.getRuntime().totalMemory());

		System.err.println("Difference in memory allocation: " + ((mem2 - mem1) / 1024) + " KB");
		Runtime.getRuntime().gc(); // request garbage collection
	}

	@Test
	public void testParse() throws UnsupportedEncodingException {
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
