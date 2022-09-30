package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;

public class XMLConverterTest extends AbstractCoreTCase {

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

		System.err.println(doc.getFileName());
		System.err.println(doc.getFileExtension());
		log.error(doc.getFileName());
		log.error(doc.getFileExtension());

		XMLConverter convert = new XMLConverter();

		String inputFile = "src/test/resources/context.xml";
		File srcXMLFile = new File(inputFile);

		File targetFile = null;
		try {
			// Creating a temp file
			targetFile = File.createTempFile("converted", ".txt");
			System.err.println(targetFile);

			convert.internalConvert("sid1", doc, srcXMLFile, targetFile);
			
			// read file and print length the outputstream
			System.err.println("targetFile.length(): " +targetFile.length());

		} catch (IOException e) {
			log.error("Exception during conversion", e);
			e.printStackTrace();
			Assert.fail();
		} finally {
			if (targetFile != null && targetFile.exists())
				targetFile.delete();
		}
		
		
	}

}
