package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;

import junit.framework.Assert;

public class SoapSearchServiceTest extends AbstractWebserviceTestCase {

	private DocumentDAO docDao;

	// Instance under test
	private SoapSearchService searchServiceImpl;

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();
		docDao = (DocumentDAO) context.getBean("DocumentDAO");

		// Make sure that this is a SoapSearchService instance
		searchServiceImpl = new SoapSearchService();
		searchServiceImpl.setValidateSession(false);
	}

	@Test
	public void testFindByFilename() throws Exception {
		List<WSDocument> documents = searchServiceImpl.findByFilename("", "pluto");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());

		Assert.assertEquals(2, documents.get(0).getId());

		documents = searchServiceImpl.findByFilename("", "PLUTO");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());

		Assert.assertEquals(2, documents.get(0).getId());

		documents = searchServiceImpl.findByFilename("", "paperino");
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.size());

		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());
		docDao.initialize(doc);
		doc.setFileName("paperina");
		docDao.store(doc);
		Assert.assertEquals("paperina", doc.getFileName());

		documents = searchServiceImpl.findByFilename("", "pluto");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());
	}

	@Test
	public void testFindFolders() throws Exception {
		List<WSFolder> folders = searchServiceImpl.findFolders("", "menu.admin");
		Assert.assertNotNull(folders);
		Assert.assertEquals(4, folders.size());
		Assert.assertEquals(80, folders.get(0).getId());
		Assert.assertEquals(99, folders.get(1).getId());

		folders = searchServiceImpl.findFolders("", "menu.adminxx");
		Assert.assertEquals(1, folders.size());
		Assert.assertEquals(80, folders.get(0).getId());

		folders = searchServiceImpl.findFolders("", "qqqxxx");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.size());
	}
}