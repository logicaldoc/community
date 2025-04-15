package com.logicaldoc.core.conversion;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;

public class XMLConverterTest extends AbstractCoreTestCase {

	private DocumentDAO ddao;

	private static final Logger log = LoggerFactory.getLogger(XMLConverterTest.class);

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context.
		ddao = (DocumentDAO) context.getBean("documentDAO");
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
			fail();
		} finally {
			FileUtil.delete(targetFile);
		}
	}
}