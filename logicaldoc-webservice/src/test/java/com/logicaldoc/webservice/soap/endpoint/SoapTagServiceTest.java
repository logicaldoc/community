package com.logicaldoc.webservice.soap.endpoint;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.webservice.AbstractWebserviceTCase;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;

import junit.framework.Assert;

public class SoapTagServiceTest extends AbstractWebserviceTCase {

	private FolderDAO folderDao;

	// Instance under test
	private SoapTagService tagService;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		folderDao = (FolderDAO) context.getBean("FolderDAO");

		tagService = new SoapTagService();
		tagService.setValidateSession(false);
	}

	@Test
	public void testAddDocumentTags() throws Exception {
		WSDocument[] documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);

		tagService.addDocumentTags("", documents[0].getId(), new String[] { "pippo", "pluto" });
		documents = tagService.findDocumentsByTag("", "pippo");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);
	}

	@Test
	public void testAddFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, new String[] { "pippo", "pluto" });
		WSFolder[] folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.length);
		folders = tagService.findFoldersByTag("", "unexisting");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.length);
	}

	@Test
	public void testSetDocumentTags() throws Exception {
		WSDocument[] documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);

		tagService.setDocumentTags("", documents[0].getId(), new String[] { "pippo", "pluto" });
		documents = tagService.findDocumentsByTag("", "pippo");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);
		documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.length);
	}

	@Test
	public void testSetFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, new String[] { "pippo", "pluto" });
		WSFolder[] folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.length);

		tagService.setFolderTags("", 4L, new String[] { "paperino" });
		folders = tagService.findFoldersByTag("", "pippo");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.length);
		folders = tagService.findFoldersByTag("", "paperino");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.length);
	}

	@Test
	public void testGetDocumentTags() throws Exception {
		WSDocument[] documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);

		tagService.addDocumentTags("", documents[0].getId(), new String[] { "pippo", "pluto" });
		List<String> tags = Arrays.asList(tagService.getDocumentTags("", documents[0].getId()));
		Assert.assertTrue(tags.contains("abc"));
		Assert.assertTrue(tags.contains("pippo"));
		Assert.assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testGetFolderTags() throws Exception {
		tagService.addFolderTags("", 4L, new String[] { "pippo", "pluto" });
		List<String> tags = Arrays.asList(tagService.getFolderTags("", 4L));
		Assert.assertTrue(tags.contains("pippo"));
		Assert.assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testFindDocumentsByTag() throws Exception {
		WSDocument[] documents = tagService.findDocumentsByTag("", "abc");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);
		List<WSDocument> docsList = Arrays.asList(documents);
		Assert.assertEquals(1, docsList.get(0).getId());

		documents = tagService.findDocumentsByTag("", "ask");
		Assert.assertNotNull(documents);
		Assert.assertEquals(1, documents.length);
		docsList = Arrays.asList(documents);
		Assert.assertEquals(2, docsList.get(0).getId());

		documents = tagService.findDocumentsByTag("", "xxx");
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.length);
	}

	@Test
	public void testFindFoldersByTag() throws Exception {
		Folder folder = folderDao.findById(4L);
		folderDao.initialize(folder);
		folder.addTag("xyz");
		folderDao.store(folder);

		WSFolder[] folders = tagService.findFoldersByTag("", "xyz");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.length);

		folders = tagService.findFoldersByTag("", "unexisting");
		Assert.assertNotNull(folders);
		Assert.assertEquals(0, folders.length);
	}
}