package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.dao.SessionDAO;

public class XMLConverterTest extends AbstractCoreTCase {
	
	// Instance under test
	private SessionDAO sdao;	
	
	private DocumentDAO ddao;
	
	protected static Logger log = LoggerFactory.getLogger(XMLConverterTest.class);

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateSessionDAO
		sdao = (SessionDAO) context.getBean("SessionDAO");
		ddao = (DocumentDAO) context.getBean("DocumentDAO");		
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testInternalConvert() throws PersistenceException, IOException {

		List<Document> xxx = ddao.findAll();
		for (Document document : xxx) {
			System.out.println("id: " + document.getId());
		}
		
		Document doc = ddao.findById(3);
		log.error(doc.getFileName());
		log.error(doc.getFileExtension());
		
		XMLConverter convert = new XMLConverter();
		
		String inputFile = "src/test/resources/context.xml";
		File srcXMLFile = new File(inputFile);

	    try {
			//Creating a temp file
			File targetFile = File.createTempFile("converted", ".html");
			convert.internalConvert("sid1", doc, srcXMLFile, targetFile);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			log.error("ghghgj", e);
		}
	}

}
