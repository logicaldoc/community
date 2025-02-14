package com.logicaldoc.core.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.sql.SQLException;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;

public class PDFParserTest extends AbstractCoreTestCase {

	private static final Logger log = LoggerFactory.getLogger(PDFParserTest.class);

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
	}

	@Test
	public void testParse() throws ParsingException {
		File file = new File("src/test/resources/probiotic-1.4.pdf");
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		PDFParser pdfp = (PDFParser) parser;
		String content = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		Assert.assertTrue(content.contains("adequate"));
	}

	@Test
	public void testSmall() throws ParsingException {
		String inputFile = "src/test/resources/small.pdf";
		File file = new File(inputFile);
		String filename = file.getPath();

		for (int i = 0; i < 300; i++) {
			Parser parser = ParserFactory.getParser(filename);
			PDFParser pdfp = (PDFParser) parser;
			String content = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);

			assertTrue(content.startsWith("1: prova"));
		}
	}

	@Test
	public void testStress() throws ParsingException {
		File file1 = new File("src/test/resources/Digital_Day.pdf");
		String filename1 = file1.getPath();
		File file2 = new File("src/test/resources/Arabic/SharePoint.pdf");
		String filename2 = file2.getPath();

		for (int i = 0; i < 10; i++) {
			Parser parser = null;
			if (i % 2 == 0)
				parser = ParserFactory.getParser(filename1);
			else
				parser = ParserFactory.getParser(filename2);

			PDFParser pdfp = (PDFParser) parser;
			if (i % 2 == 0) {
				String content = pdfp.parse(file1, filename1, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
				assertEquals(28388, content.length());
			} else {
				String content = pdfp.parse(file2, filename2, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
				assertEquals(8759, content.length());
			}
		}
	}

	@Test
	public void testParseArabic() throws ParsingException {

		// This is a pdf document with two (2) columns, one english and one
		// Arabic on the right
		// The text in the left column is left aligned, while the text in the
		// right goes from right to left (Arabic)
		// The documentation of PDFBox 1.4.0 states that this requires ICU4J 3.8
		String inputFile = "src/test/resources/Arabic/imaging14.pdf";
		String outputFile = "src/test/resources/Arabic/UTF-8.txt";
		File file = new File(inputFile);
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		PDFParser pdfp = (PDFParser) parser;
		String content = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);

		assertNotNull(content);
		assertTrue(StringUtils.isNotEmpty(content));

		assertEquals(3034, content.length());

		try {
			FileOutputStream out = new FileOutputStream(outputFile);
			BufferedWriter BW = new BufferedWriter(new OutputStreamWriter(out, "UTF-8"));
			BW.write(content);
			BW.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testParseArabic2() throws ParsingException {

		// This is an Arabic pdf document
		// The text goes from right to left (Arabic)
		// The documentation of PDFBox 1.4.0 states that this requires ICU4J 3.8
		String inputFile = "src/test/resources/Arabic/SharePoint.pdf";
		File file = new File(inputFile);
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		PDFParser pdfp = (PDFParser) parser;
		String content = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);

		assertNotNull(content);
		assertTrue(StringUtils.isNotEmpty(content));

		assertEquals(8759, content.length());
	}

	@Test
	public void testForm() throws ParsingException {

		String inputFile = "src/test/resources/pdf_form_fields.pdf";
		File file = new File(inputFile);
		String filename = file.getPath();

		Parser parser = ParserFactory.getParser(filename);
		PDFParser pdfp = (PDFParser) parser;
		String x = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		assertNotNull(x);
		log.debug("Extracted text size: {}", x.length());
		assertTrue(x.length() > 2000);

		inputFile = "src/test/resources/fillablePDF1.pdf";
		file = new File(inputFile);
		filename = file.getPath();

		parser = ParserFactory.getParser(filename);
		pdfp = (PDFParser) parser;
		x = pdfp.parse(file, filename, null, Locale.ENGLISH, Tenant.DEFAULT_NAME);
		assertNotNull(x);
		log.debug("Extracted text size: {}", x.length());
		assertTrue(x.length() > 2500);
	}
}
