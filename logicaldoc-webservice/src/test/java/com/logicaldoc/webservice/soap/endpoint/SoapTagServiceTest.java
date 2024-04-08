package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;

import junit.framework.Assert;

public class SoapTagServiceTest extends AbstractWebserviceTestCase {

	private FolderDAO folderDao;

	// Instance under test
	private SoapTagService tagService;

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		folderDao = (FolderDAO) context.getBean("FolderDAO");

		tagService = new SoapTagService();
		tagService.setValidateSession(false);
	}

	@Test
	public void testAddDocumentTags() throws Exception {
		List<WSDocument> documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());

		tagService.addDocumentTags("", documents.get(0).getId(), Arrays.asList("pippo", "pluto"));
		documents = tagService.findDocumentsByTag("", "pippo");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());
	}

	@Test
	public void testAddFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<WSFolder> folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.size());
		folders = tagService.findFoldersByTag("", "unexisting");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.size());
	}

	@Test
	public void testSetDocumentTags() throws Exception {
		List<WSDocument> documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());

		tagService.setDocumentTags("", documents.get(0).getId(), List.of("pippo", "pluto"));
		documents = tagService.findDocumentsByTag("", "pippo");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());
		documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.size());
	}

	@Test
	public void testSetFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<WSFolder> folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.size());

		tagService.setFolderTags("", 4L, Arrays.asList("paperino"));
		folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.size());
		folders = tagService.findFoldersByTag("", "paperino");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.size());
	}

	@Test
	public void testGetDocumentTags() throws Exception {
		List<WSDocument> documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());

		tagService.addDocumentTags("", documents.get(0).getId(), Arrays.asList("pippo", "pluto"));
		List<String> tags = tagService.getDocumentTags("", documents.get(0).getId());
		Assert.assertTrue(tags.contains("abc"));
		Assert.assertTrue(tags.contains("pippo"));
		Assert.assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testGetFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<String> tags = tagService.getFolderTags("", 4L);
		Assert.assertTrue(tags.contains("pippo"));
		Assert.assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testFindDocumentsByTag() throws Exception {
		List<WSDocument> documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());
		Assert.assertEquals(1, documents.get(0).getId());

		documents = tagService.findDocumentsByTag("", "ask");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.size());
		Assert.assertEquals(2, documents.get(0).getId());

		documents = tagService.findDocumentsByTag("", "xxx");
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.size());
	}

	@Test
	public void testFindFoldersByTag() throws Exception {
		Folder folder = folderDao.findById(4L);
		folderDao.initialize(folder);
		folder.addTag("xyz");
		folderDao.store(folder);

		List<WSFolder> folders = tagService.findFoldersByTag("", "xyz");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.size());

		folders = tagService.findFoldersByTag("", "unexisting");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.size());
	}
}