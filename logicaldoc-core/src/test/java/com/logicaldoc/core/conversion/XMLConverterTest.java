package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.util.io.FileUtil;

public class XMLConverterTest extends AbstractCoreTestCase {

	private DocumentDAO ddao;

	protected static Logger log = LoggerFactory.getLogger(XMLConverterTest.class);

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context.
		ddao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testInternalConvert() throws PersistenceException, IOException {
		Document doc = ddao.findById(7);

		XMLConverter convert = new XMLConverter();

		String inputFile = "src/test/resources/context.xml";
		File srcXMLFile = new File(inputFile);

		File targetFile = null;
		try {
			// Creating a temp file
			targetFile = FileUtil.createTempFile("converted", ".txt");

			convert.internalConvert("sid1", doc, srcXMLFile, targetFile);
		} catch (IOException e) {
			log.error("Exception during conversion", e);
			Assert.fail();
		} finally {
			FileUtil.strongDelete(targetFile);
		}
	}
}